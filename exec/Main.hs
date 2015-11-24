{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Network.Xmpp.Bot
import Turtle (Pattern, chars, match)

parseMessage :: Pattern HintCommand
parseMessage
    =   fmap Eval   ("> "       *> chars)
    <|> fmap TypeOf ("> :type " *> chars)

handleMessage :: UserName -> Message -> Hint s [Message]
handleMessage _ (Message msg) = case match parseMessage msg of
    cmd:_ -> do
        result <- command cmd
        return (map prefix result)
    _     -> return []
  where
    prefix (Message txt) = Message ("⤷ " <> txt)

main :: IO ()
main = do
    o <- options "Haskell-driven XMPP bot" opts
    runXmpp o () handleMessage
