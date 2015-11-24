{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-| This module provides a `Hint` monad which (under the hood) supports per-user
    interpreter sessions.  Unfortunately, GHC's API is not thread-safe and does
    not support multiple concurrent interpreters so for now the API only exposes
    support for a single-user session.
-}

module Network.Xmpp.Bot.Hint (
    -- * Commands
      command
    , runHint
    , liftXMPP

    -- * Types
    , UserName(..)
    , Message(..)
    , HintCommand(..)
    , Hint

    -- * Re-exports
    , MonadState(..)
    , Text
    ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (evaluate)
import Control.DeepSeq (($!!))
import Control.Monad (forever)
import Control.Monad.Catch (catch)
import Control.Monad.Managed (Managed)
import Control.Monad.State (MonadState(..), StateT)
import Control.Monad.Trans (lift, MonadIO(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Language.Haskell.Interpreter
    (Extension(..), InterpreterError(..), OptionVal((:=)))

import qualified Control.Concurrent.Async     as Async
import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Monad.Managed        as Managed
import qualified Control.Monad.State          as State
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.Text                    as Text
import qualified Language.Haskell.Interpreter as Hint
import qualified Network.Protocol.XMPP        as XMPP
import qualified System.Timeout               as Timeout
import qualified Turtle

extensions :: [Extension]
extensions =
    [ NoMonomorphismRestriction
    ]

-- | The XMPP user that sent the message
newtype UserName = UserName { getUserName :: Text }
    deriving (Eq, Hashable, IsString)

instance Show UserName where
    show (UserName txt) = show txt

-- | A message that the XMPP bot sends or receives
newtype Message = Message { getMessage :: Text }

-- | Commands that the interpreter accepts
data HintCommand
    = TypeOf Text
    -- ^ Request the type of the given expression
    | Eval   Text
    -- ^ Evaluate the given expression
    | Reset
    -- ^ Restart the interpreter session

-- | The internal state of the `Hint` `Monad`
data HintState s = HintState
    { _sessions    :: HashMap UserName UserState
    -- ^ All user sessions
    , _xmppSession :: XMPP.Session
    -- ^ The current XMPP sessions
    , _custom      :: s
    -- ^ Custom user-defined state
    }

-- | The state of the interpreter associated with a given `UserName`
data UserState = UserState
    { _async    :: Async InterpreterError
    -- ^ Reference to the thread running the user's interpreter session
    , _inTMVar  :: TMVar HintCommand
    -- ^ Send commands to the interpreter via this `TMVar`
    , _outTMVar :: TMVar [Text]
    -- ^ Receive responses from the interpreter via this `TMVar`
    }

{-| Build primitive `Hint` commands using:

    * `command` - execute an interpreter command
    * `liftIO`  - execute an arbitrary `IO` action
    * `get` / `put` - read and write user-defined state

    Combine these `Hint` commands using @do@ notation

    Consume `Hint` commands using `runHint`
-}
newtype Hint s a = Hint { unHint :: StateT (HintState s) Managed a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState s (Hint s) where
    get = Hint (do
        HintState _ _ s <- get
        return s )

    put s = Hint (do
        HintState m x _ <- get
        put (HintState m x s) )

-- | Pretty print an error message
handler :: Monad m => InterpreterError -> m Text
handler (UnknownError msg ) = return ("Unexpected error: "  <> Text.pack msg)
handler (NotAllowed   msg ) = return ("Permission denied: " <> Text.pack msg)
handler (GhcException msg ) = return ("GHC failure: " <> Text.pack msg)
handler (WontCompile  msgs) = return
    (   "Compile error:\n\n"
    <>  Text.pack (unlines (map Hint.errMsg msgs))
    )

-- | > unpacked :: Lens' Text String
unpacked :: Functor f => (String -> f String) -> (Text -> f Text)
unpacked k txt = fmap Text.pack (k (Text.unpack txt))

{-| Initialize a new interpreter for a given user

    This does not take care of disposing any prior sessions for that user
-}
initUser :: UserName -> Hint s UserState
initUser userName = Hint (do
    cmdTMVar <- liftIO (STM.atomically TMVar.newEmptyTMVar)
    resTMVar <- liftIO (STM.atomically TMVar.newEmptyTMVar)

    let reset = do
            Hint.reset
            Hint.set
                [ Hint.languageExtensions      := extensions
                , Hint.installedModulesInScope := False
                ]
            Hint.setImports ["Prelude"]

    let io :: IO InterpreterError
        io = do
            x <- Hint.runInterpreter (do
                reset
                forever (do
                    cmd <- liftIO (STM.atomically (TMVar.takeTMVar cmdTMVar))
                    txts <- case cmd of
                        TypeOf txt -> do
                            txt' <- unpacked Hint.typeOf txt `catch` handler
                            return [txt']
                        Eval   txt -> do
                            txt' <- unpacked Hint.eval   txt `catch` handler
                            return [txt']
                        Reset             -> do
                            reset
                            return []
                    liftIO (STM.atomically (TMVar.putTMVar resTMVar txts)) ))
            case x of
                Left  e -> return e
                Right v -> v

    HintState m x s <- get
    a               <- lift (Managed.managed (Async.withAsync io))
    let userState = UserState a cmdTMVar resTMVar
    put (HintState (HashMap.insert userName userState m) x s)
    return userState )

-- | Run an interpreter command for the given user
command :: HintCommand -> Hint s [Message]
command hintCommand = Hint (do
    HintState m _ _               <- get
    UserState a cmdTMVar resTMVar <- case HashMap.lookup userName m of
        Nothing -> unHint (initUser userName)
        Just us -> return us
    let writeTransaction = do
            x <- Async.pollSTM a
            case x of
                -- Interpreter thread is still alive
                Nothing        -> do
                    TMVar.putTMVar cmdTMVar hintCommand
                    return (Right ())
                -- Interpreter thread died with some `IOException`
                Just (Left  e) -> do
                    return (Left (errMsg e))
                -- Interpreter thread died with an `InterpreterError`
                Just (Right e) -> do
                    return (Left (errMsg e))
    x <- liftIO (STM.atomically writeTransaction)
    case x of
        Left  e -> do
            let msg = "Interpreter thread failed: " <> Turtle.repr e
            Turtle.err ("[!] " <> msg)
            _ <- unHint (initUser userName)
            return [Message msg]
        Right () -> do
            let readTransaction = do
                    y <- Async.pollSTM a
                    case y of
                        -- Interpreter thread is still alive
                        Nothing        -> do
                            txts <- TMVar.takeTMVar resTMVar
                            return (Right txts)
                        -- Interpreter thread died with some `IOException`
                        Just (Left  e) -> do
                            return (Left (errMsg e))
                        -- Interpreter thread died with an `InterpreterError`
                        Just (Right e) -> do
                            return (Left (errMsg e))
            y <- liftIO (Timeout.timeout 1000000 (do
                y <- STM.atomically readTransaction
                evaluate $!! y))
            case y of
                -- Interpreter thread took more than 1 second to compute result
                Nothing           -> do
                    let msg = "Resource error: 1 second time limit"
                    liftIO (Async.cancel a)
                    _ <- unHint (initUser userName)
                    return [Message msg]
                -- Interpreter thread died with an exception
                Just (Left  e   ) -> do
                    let msg = "Internal error: " <> Turtle.repr e
                    Turtle.err ("[!] " <> msg)
                    _ <- unHint (initUser userName)
                    return [Message msg]
                Just (Right txts) -> do
                    return (map Message txts) )
  where
    -- Replace with this a new parameter to the `command` function once GHC's
    -- interpreter API supports multiple concurrent sessions
    userName :: UserName
    userName = "ghci"

    errMsg :: Show e => e -> Text
    errMsg e =
        "Warning: A user's interpreter thread threw an exception\n\
        \\n\
        \User     : " <> Turtle.repr userName <> "\n\
        \Exception: " <> Turtle.repr e


{-| Execute `Hint` instructions by providing a starting value for the
    user-defined state

    If you don't use user-defined state, just supply @()@ as the starting value
-}
runHint :: XMPP.Session -> s -> Hint s () -> IO ()
runHint session s b =
    Managed.runManaged
        (State.evalStateT (unHint b) (HintState HashMap.empty session s))

-- | Transform an `XMPP.XMPP` command into a `Hint` command
liftXMPP :: XMPP.XMPP a -> Hint s a
liftXMPP xmpp = Hint (do
    HintState m x s <- get
    e <- liftIO (XMPP.runXMPP x (do
        a  <- xmpp
        x' <- XMPP.getSession
        return (a, x') ))
    case e of
        Left   exc    -> Turtle.die (Turtle.repr exc)
        Right (a, x') -> do
            put (HintState m x' s)
            return a )
