{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import HipChat.Bot
import Turtle (Pattern, chars, match)

parseMessage :: Pattern HintCommand
parseMessage
    =   fmap Eval   ("> "       *> chars)
    <|> fmap TypeOf ("> :type " *> chars)

handleMessage :: UserName -> Text -> Hint s [Text]
handleMessage userName msg = case match parseMessage msg of
    cmd:_ -> command userName cmd
    _     -> return []

main :: IO ()
main = do
    o <- options "Haskell-driven HipChat bot" opts
    runHipChat o () handleMessage
