{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Hint
import Turtle (Pattern, chars, match)

parseMessage :: Pattern HintCommand
parseMessage =
        fmap (Eval   . Code) ("> "       *> chars)
    <|> fmap (TypeOf . Code) ("> :type " *> chars)

receiveMessage :: Text -> Text -> Bot s [Text]
receiveMessage from msg = case match parseMessage msg of
    cmd:_ -> command (UserName from) cmd
    _     -> return []
