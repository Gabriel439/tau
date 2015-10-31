# tau

*The simple XMPP bot.*

This is both a library and an example executable.

To install and run the executable, run these commands:

```bash
$ stack install --install-ghc
$ ~/.local/bin/tau --help
Haskell-driven XMPP bot

Usage: tau HOST ID PASSWORD NICKNAME ROOM

Available options:
  -h,--help                Show this help text
  HOST                     Connect host
  ID                       Jabber ID
  PASSWORD                 Password
  NICKNAME                 Room nickname
  ROOM                     Room (XMPP/Jabber name)
```

You can also use this as a library to build your own custom XMPP bot.  For
example, the above executable was written using the library, like this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Network.Xmpp.Bot
import Turtle (Pattern, chars, match) 

parseMessage :: Pattern HintCommand
parseMessage
    =   fmap Eval   ("> "       *> chars)
    <|> fmap TypeOf ("> :type " *> chars)

handleMessage :: UserName -> Message -> Hint s [Message]
handleMessage _ (Message msg) = case match parseMessage msg of
    cmd:_ -> command cmd
    _     -> return []

main :: IO ()              
main = do                  
    o <- options "Haskell-driven XMPP bot" opts
    runXmpp o () handleMessage
```

## HipChat Instructions

```
HOST    : chat.hipchat.com
ID      : 1234_1234@chat.hipchat.com/bot
PASSWORD: password
NICKNAME: My Name
ROOM    : 1234_words@conf.hipchat.com
```
