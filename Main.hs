{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Bot

import Network.Xmpp

import Control.Monad
import Control.Exception (SomeException, catch)
import Data.Text (Text, pack, unpack)
import Data.String
import Data.IORef
import System.Exit
import Data.XML.Types (
    nameLocalName, elementName, elementText
  , Element(Element), Name(Name), Content(ContentText), Node(NodeContent)
  )
import Turtle (Parser, argText, options)

mainLoop :: IORef Conf -> Text -> Session -> IO ()
mainLoop conf xmpproom sess = do
  msg <- getMessage sess
  let from' = maybe "(anybody)" unpack (resourcepart =<< messageFrom msg)
  let to' = maybe "(anybody)" unpack (resourcepart =<< messageTo msg)
  let bodyElems = elems "body" msg
  let delayElems = elems "delay" msg -- hipchat delayed messages
  let responder = elems "responder" msg -- so you can't respond to yourself
  when (null delayElems && (not . null) bodyElems && null responder) $ do
    let body = head $ elementText (head bodyElems)
    conf' <- readIORef conf
    (replies, newConf) <- receiveMessage conf' (unpack xmpproom) from' to' (unpack body)
    mapM_ (sendReply sess msg) replies
    writeIORef conf newConf
  return ()

data Options = Options
    { hostname :: Text
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
  
  c' <- mkConf
  conf <- newIORef c'
  
  esess <- session (fromString (unpack hostname)) (Just (\_ -> ([plain xmppid Nothing xmpppass]), Nothing)) def
  sess <- case esess of
    Right s -> return s
    Left e -> putStrLn ("XmppFailure: " ++ show e) >> exitWith (ExitFailure 1)
  sendMUCPresence (unpack xmpproom) (unpack xmppnick) sess
  
  setConnectionClosedHandler (\_ _ -> do
    _ <- reconnectNow sess
    sendMUCPresence (unpack xmpproom) (unpack xmppnick) sess) sess
  forever (catch (mainLoop conf xmpproom sess) handler)

sendReply :: Session -> Message -> String -> IO ()
sendReply sess msg content = do
  case answerMessage msg [
    Element "body" [] [
      NodeContent (ContentText (pack content))]
    ] of
      Just answer -> sendMessage answer sess >> return ()
      Nothing -> return ()

handler :: SomeException -> IO ()
handler = print

elems :: Text -> Message -> [Element]
elems tagname mes = filter ((== tagname) . nameLocalName . elementName) $
                           (messagePayload mes)

sendMUCPresence :: String -> String -> Session -> IO ()
sendMUCPresence xmpproom xmppnick sess = do
  jid' <- getJid sess
  void $ sendPresence (def {
      presenceFrom = jid'
    , presenceTo = Just (parseJid (xmpproom ++ '/' : xmppnick))
    , presencePayload = [Element "x" [(Name "xmlns" Nothing Nothing, [ContentText "http://jabber.org/protocol/muc"])] []]
    }) sess

getOrElse :: [String] -> Int -> IO Text
getOrElse xs i =
  if length xs >= i
    then return $ pack $ xs !! i
    else printUsage >> exitWith (ExitFailure 1)

printUsage :: IO ()
printUsage =
  putStrLn "USAGE: tau HOSTNAME XMPPID XMPPPASS XMPPNICK XMPPROOM"
