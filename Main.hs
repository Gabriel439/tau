{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Bot

import Control.Monad (when, forever)
import Control.Exception (Exception, SomeException, catch, throwIO)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.IORef
import Data.XML.Types
    ( elementText
    , nameLocalName
    , Element(..)
    , Content(ContentText)
    , Node(NodeContent)
    )
import Network.Xmpp
    ( Message(..)
    , Presence(..)
    , Session
    , answerMessage
    , def
    , getJid
    , getMessage
    , jidFromTexts
    , reconnectNow
    , resourcepart
    , sendMessage
    , sendPresence
    , session
    , setConnectionClosedHandler
    , simpleAuth
    )
import Turtle (Parser, argText, die, err, options, repr)

elems :: Text -> Message -> [Element]
elems tagname msg =
    filter ((== tagname) . nameLocalName . elementName) (messagePayload msg)

-- mainLoop :: IORef HintCmds -> Text -> Session -> IO ()
mainLoop conf xmpproom sess = do
    msg <- getMessage sess
    let from' = maybe "(anybody)" id (resourcepart =<< messageFrom msg)
    let to'   = maybe "(anybody)" id (resourcepart =<< messageTo   msg)
    let bodyElems  = elems "body"      msg
    let delayElems = elems "delay"     msg -- hipchat delayed messages
    let responder  = elems "responder" msg -- so you can't respond to yourself
    case bodyElems of
        [bodyElem] -> do
            when (null delayElems && null responder) $ do
                let body = head (elementText bodyElem)
                conf' <- readIORef conf
                (replies, newConf) <- receiveMessage
                    conf'
                    xmpproom
                    from'
                    to'
                    body
                mapM_ (sendReply sess msg) (map pack replies)
                writeIORef conf newConf
        _      -> err
            ("Warning: Unexpected number of message body elements\n\
             \n\
             \Message body elements: " <> repr bodyElems <> "\n\
             \Expected # of elements: 1\n\
             \Actual   # of elements: " <> repr (length bodyElems) )

data Options = Options
    { host     :: Text
    , xmppid   :: Text
    , xmpppass :: Text
    , xmppnick :: Text
    , xmpproom :: Text
    }

opts :: Parser Options
opts =  Options
    <$> argText "host"     "Connect host"
    <*> argText "id"       "Jabber ID"
    <*> argText "password" "Password"
    <*> argText "nickname" "Nickname"
    <*> argText "room"     "XMPP/Jabber room name"

main :: IO ()
main = do
    Options {..} <- options "Haskell-driven HipChat bot" opts
  
    c'   <- mkConf
    conf <- newIORef c'
  
    sess <- throws (session (unpack host) (simpleAuth xmppid xmpppass) def)

    sendMUCPresence xmpproom xmppnick sess
  
    setConnectionClosedHandler (\_ _ -> do
        throws_ (reconnectNow sess)
        sendMUCPresence xmpproom xmppnick sess) sess
    forever (catch (mainLoop conf xmpproom sess) handler)

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

sendReply :: Session -> Message -> Text -> IO ()
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
             \n\
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
             \n\
             \Room    : " <> repr xmpproom <> "\n\
             \Nickname: " <> repr xmppnick )
