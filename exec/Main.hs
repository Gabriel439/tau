{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import HipChat.Bot
import Turtle (Pattern, chars, match)

parseMessage :: Pattern HintCommand
parseMessage
    =   fmap (Eval   . Code) ("> "       *> chars)
    <|> fmap (TypeOf . Code) ("> :type " *> chars)

handleMessage :: UserName -> Text -> Hint s [Text]
handleMessage userName msg = case match parseMessage msg of
    cmd:_ -> command userName cmd
    _     -> return []

main :: IO ()
main = runHipChat () handleMessage
