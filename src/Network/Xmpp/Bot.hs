{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Example usage:
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Control.Applicative ((<|>))
-- > import Network.Xmpp.Bot
-- > import Turtle (Pattern, chars, match)
-- > 
-- > parseMessage :: Pattern HintCommand
-- > parseMessage
-- >     =   fmap Eval   ("> "       *> chars)
-- >     <|> fmap TypeOf ("> :type " *> chars)
-- >     
-- > handleMessage :: UserName -> Message -> Hint s [Text]
-- > handleMessage _ (Message msg) = case match parseMessage msg of
-- >     cmd:_ -> command cmd
-- >     _     -> return []
-- >     
-- > main :: IO ()
-- > main = do
-- >     o <- options "Haskell-driven XMPP bot" opts
-- >     runXmpp o () handleMessage

module Network.Xmpp.Bot (
    -- * Commands
      runXmpp
    , command
    , opts
    , options

    -- * Types
    , Options(..)
    , UserName(..)
    , Message(..)
    , HintCommand(..)
    , Hint

    -- * Re-exports
    , MonadState(..)
    , MonadIO(..)
    , Text
    ) where

import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad (when, forever)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid ((<>))
import Data.XML.Types
    ( elementText
    , nameLocalName
    , Element(..)
    , Content(ContentText)
    , Node(NodeContent)
    )
import Network.Xmpp
    ( Presence(..)
    , Session
    , answerMessage
    , def
    , getJid
    , jidFromTexts
    , reconnectNow
    , resourcepart
    , sendMessage
    , sendPresence
    , session
    , setConnectionClosedHandler
    , simpleAuth
    )
import Network.Xmpp.Bot.Hint
import Turtle (Parser, argText, die, err, options, repr)

import qualified Data.Text    as Text
import qualified Network.Xmpp as Xmpp

elems :: Text -> Xmpp.Message -> [Element]
elems tagname msg =
    filter ((== tagname) . nameLocalName . elementName) (Xmpp.messagePayload msg)

mainLoop :: Session -> (UserName -> Message -> Hint s [Message]) -> Hint s ()
mainLoop sess handleMessage = do
    msg <- liftIO (Xmpp.getMessage sess)
    let from = maybe "(anybody)" id (resourcepart =<< Xmpp.messageFrom msg)
    let bodyElems  = elems "body"      msg
    let delayElems = elems "delay"     msg -- hipchat delayed messages
    let responder  = elems "responder" msg -- so you can't respond to yourself
    case bodyElems of
        [bodyElem] -> do
            when (null delayElems && null responder) $ do
                let body = head (elementText bodyElem)
                replies <- handleMessage (UserName from) (Message body)
                mapM_ (liftIO . sendReply sess msg . getMessage) replies
        _          -> liftIO (err
            ("Warning: Unexpected number of message body elements\n\
             \\n\
             \Message body elements: " <> repr bodyElems <> "\n\
             \Expected # of elements: 1\n\
             \Actual   # of elements: " <> repr (length bodyElems) ) )

-- | Hipchat connection options
data Options = Options
    { host     :: Text
    -- ^ Connect host
    , xmppid   :: Text
    -- ^ Jabber ID
    , xmpppass :: Text
    -- ^ Password
    , xmppnick :: Text
    -- ^ Room nickname
    , xmpproom :: Text
    -- ^ Room (XMPP/Jabber name)
    }

-- | Parser for command-line options
opts :: Parser Options
opts =  Options
    <$> argText "host"     "Connect host"
    <*> argText "id"       "Jabber ID"
    <*> argText "password" "Password"
    <*> argText "nickname" "Room nickname"
    <*> argText "room"     "Room (XMPP/Jabber name)"

-- | Run an Xmpp bot
runXmpp :: Options -> s -> (UserName -> Message -> Hint s [Message]) -> IO ()
runXmpp (Options {..}) initialState handleMessage = do
    sess <- throws (session (Text.unpack host) (simpleAuth xmppid xmpppass) def)

    sendMUCPresence xmpproom xmppnick sess
  
    setConnectionClosedHandler (\_ _ -> do
        throws_ (reconnectNow sess)
        sendMUCPresence xmpproom xmppnick sess) sess
    forever (catch (runHint initialState (mainLoop sess handleMessage)) handler)

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    x <- io
    case x of
        Left  e -> throwIO e
        Right a -> return a

throws_ :: Exception e => IO (Maybe e) -> IO ()
throws_ io = throws (fmap f io)
  where
    f (Just e ) = Left  e
    f  Nothing  = Right ()

sendReply :: Session -> Xmpp.Message -> Text -> IO ()
sendReply sess msg content = do
    let element = Element
            { elementName = "body"
            , elementAttributes = []
            , elementNodes = [NodeContent (ContentText content)]
            }
    case answerMessage msg [element] of
        Just answer -> throws (sendMessage answer sess)
        Nothing     -> err
            ("Warning: Can not reply to a message missing a `from:` field\n\
             \\n\
             \Message: " <> repr msg )

handler :: SomeException -> IO ()
handler = print

sendMUCPresence :: Text -> Text -> Session -> IO ()
sendMUCPresence xmpproom xmppnick sess = do
    jabberID <- getJid sess
    let element = Element
            { elementName       = "x"
            , elementAttributes =
                [("xmlns" , [ContentText "http://jabber.org/protocol/muc"])]
            , elementNodes      = []
            }
    case jidFromTexts Nothing xmpproom (Just xmppnick) of
        Just roomId -> do
            let presence = def
                    { presenceFrom    = jabberID
                    , presenceTo      = Just roomId
                    , presencePayload = [element]
                    }
            throws (sendPresence presence sess)
        Nothing     -> die
            ("Error: Invalid room or nickname\n\
             \\n\
             \Room    : " <> repr xmpproom <> "\n\
             \Nickname: " <> repr xmppnick )
