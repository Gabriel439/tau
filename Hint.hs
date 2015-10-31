{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hint (
      UserName(..)
    , HintCommand(..)
    , Bot
    , Code(..)
    , command
    , runBot
    ) where

import Language.Haskell.Interpreter (Extension(..))

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Monad (forever)
import Control.Monad.Catch (catch)
import Control.Monad.Managed (Managed)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans (lift, MonadIO(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Language.Haskell.Interpreter (InterpreterError(..), OptionVal((:=)))
import Turtle (err, repr)

import qualified Control.Concurrent.Async     as Async
import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import qualified Control.Monad.Managed        as Managed
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.Text                    as Text
import qualified Language.Haskell.Interpreter as Hint
import qualified System.Timeout               as Timeout

extensions :: [Extension]
extensions =
    [ NoMonomorphismRestriction
    , OverloadedStrings
    , Generics
    , GeneralizedNewtypeDeriving
    , DeriveDataTypeable
    , DeriveFunctor
    , DeriveTraversable
    , DeriveFoldable
    , EmptyDataDecls
    ]

newtype UserName = UserName { getUserName :: Text }
    deriving (Eq, IsString, Hashable, Show)

newtype Code = Code { getCode :: Text } deriving (IsString)

data HintCommand
    = TypeOf Code
    | Eval   Code
    | Reset

data BotState s = BotState
    { _sessions :: HashMap UserName UserState
    , _custom   :: s
    }

data UserState = UserState
    { _async    :: Async InterpreterError
    , _inTMVar  :: TMVar HintCommand
    , _outTMVar :: TMVar [Text]
    }

newtype Bot s a = Bot { unBot :: StateT (BotState s) Managed a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState s (Bot s) where
    get = Bot (do
        BotState _ s <- get
        return s )

    put s = Bot (do
        BotState m _ <- get
        put (BotState m s) )

handler :: Monad m => InterpreterError -> m Text
handler (UnknownError msg ) = return ("Unexpected error: "  <> Text.pack msg)
handler (NotAllowed   msg ) = return ("Permission denied: " <> Text.pack msg)
handler (GhcException msg ) = return ("Internal error: "    <> Text.pack msg)
handler (WontCompile  msgs) = return
    (   "I could not compile your program:\n"
    <>  Text.pack (unlines (map Hint.errMsg msgs))
    )

unpacked :: Functor f => (String -> f String) -> (Text -> f Text)
unpacked k txt = fmap Text.pack (k (Text.unpack txt))

initUser :: UserName -> Bot s UserState
initUser userName = Bot (do
    cmdTMVar <- liftIO (STM.atomically TMVar.newEmptyTMVar)
    resTMVar <- liftIO (STM.atomically TMVar.newEmptyTMVar)

    let reset = do
            Hint.reset
            Hint.set [Hint.languageExtensions := extensions]
            Hint.setImports ["Prelude"]

    let io :: IO InterpreterError
        io = do
            x <- Hint.runInterpreter (do
                reset
                forever (do
                    cmd <- liftIO (STM.atomically (TMVar.takeTMVar cmdTMVar))
                    txts <- case cmd of
                        TypeOf (Code txt) -> do
                            txt' <- unpacked Hint.typeOf txt `catch` handler
                            return [txt']
                        Eval   (Code txt) -> do
                            txt' <- unpacked Hint.eval   txt `catch` handler
                            return [txt']
                        Reset             -> do
                            reset
                            return []
                    liftIO (STM.atomically (TMVar.putTMVar resTMVar txts)) ))
            case x of
                Left  e -> return e
                Right v -> v

    BotState m s <- get
    a            <- lift (Managed.managed (Async.withAsync io))
    let userState = UserState a cmdTMVar resTMVar
    put (BotState (HashMap.insert userName userState m) s)
    return userState )

command :: UserName -> HintCommand -> Bot s [Text]
command userName hintCommand = Bot (do
    BotState m _                  <- get
    UserState a cmdTMVar resTMVar <- case HashMap.lookup userName m of
        Nothing -> unBot (initUser userName)
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
            err e
            _ <- unBot (initUser userName)
            return []
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
            y <- liftIO (Timeout.timeout 1000000 (STM.atomically readTransaction))
            case y of
                -- Interpreter thread took more than 1 second to compute the result
                Nothing           -> do
                    liftIO (Async.cancel a)
                    _ <- unBot (initUser userName)
                    return []
                -- Interpreter thread died with an exception
                Just (Left  e   ) -> do
                    err e
                    _ <- unBot (initUser userName)
                    return []
                Just (Right txts) -> do
                    return txts )
  where
    errMsg :: Show e => e -> Text
    errMsg e =
        "Warning: A user's interpreter thread threw an exception\n\
        \n\
        \User     : " <> repr userName <> "\n\
        \Exception: " <> repr e

runBot :: s -> Bot s () -> IO ()
runBot s b =
    Managed.runManaged (evalStateT (unBot b) (BotState HashMap.empty s))
