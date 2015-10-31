{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Hint
import Turtle (Pattern, chars, match)

import qualified Data.Text as Text

mkConf :: IO HintCmds
mkConf = startHint

data Command = Interpret Cmd Text

parseMessage :: Pattern Command
parseMessage =
        Interpret Eval   <$> ("> "       *> chars)
    <|> Interpret TypeOf <$> ("> :type " *> chars)

receiveMessage
    :: HintCmds -> Text -> Text -> Text -> Text -> IO ([String], HintCmds)
receiveMessage conf _xmpproom _from _to msg = do
    case match parseMessage msg of
        Interpret code cmd:_ -> do
            res <- evalHint cmd code conf
            let res' = case res of
                    Left  e -> "error: " ++ e
                    Right v -> v
            return ([res'], conf)
        _                    -> do
            return ([], conf)
