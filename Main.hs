{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Bot

import Control.Monad (when, forever, void)
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
    , parseJid
    , plain
    , reconnectNow
    , resourcepart
    , sendMessage
    , sendPresence
    , session
    , setConnectionClosedHandler
    )
import Turtle (Parser, argText, err, options, repr)

mainLoop :: IORef Conf -> Text -> Session -> IO ()
mainLoop conf xmpproom sess = do
    msg <- getMessage sess
    let from' = maybe "(anybody)" unpack (resourcepart =<< messageFrom msg)
    let to'   = maybe "(anybody)" unpack (resourcepart =<< messageTo   msg)
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
                    (unpack xmpproom)
                    from'
                    to'
                    (unpack body)
                mapM_ (sendReply sess msg) (map pack replies)
                writeIORef conf newConf
        _      ->
            err ("Error: Unexpected number of message body elements\n\
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
    <*> argText "nickname" "Room Nickname"
    <*> argText "room"     "XMPP/Jabber room name"

main :: IO ()
main = do
    Options {..} <- options "Haskell-driven HipChat bot" opts
  
    c'   <- mkConf
    conf <- newIORef c'
  
    sess <- throws
        (session
            (unpack host)
            (Just (\_ -> ([plain xmppid Nothing xmpppass]), Nothing))
            def )

    sendMUCPresence (unpack xmpproom) (unpack xmppnick) sess
  
    setConnectionClosedHandler (\_ _ -> do
        _ <- reconnectNow sess
        sendMUCPresence (unpack xmpproom) (unpack xmppnick) sess) sess
    forever (catch (mainLoop conf xmpproom sess) handler)

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    x <- io
    case x of
        Left e  -> throwIO e
        Right a -> return a

sendReply :: Session -> Message -> Text -> IO ()
sendReply sess msg content = do
    let element = Element
            { elementName = "body"
            , elementAttributes = []
            , elementNodes = [NodeContent (ContentText content)]
            }
    case answerMessage msg [element] of
        Just answer -> throws (sendMessage answer sess)
        Nothing     -> return ()

handler :: SomeException -> IO ()
handler = print

elems :: Text -> Message -> [Element]
elems tagname msg =
    filter ((== tagname) . nameLocalName . elementName) (messagePayload msg)

sendMUCPresence :: String -> String -> Session -> IO ()
sendMUCPresence xmpproom xmppnick sess = do
    jid' <- getJid sess
    let element = Element
            { elementName       = "x"
            , elementAttributes =
                [("xmlns" , [ContentText "http://jabber.org/protocol/muc"])]
            , elementNodes      = []
            }
    let presence = def
            { presenceFrom    = jid'
            , presenceTo      = Just (parseJid (xmpproom ++ '/' : xmppnick))
            , presencePayload = [element]
            }
    void (sendPresence presence sess)
