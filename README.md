# tau

*The simple XMPP bot.*

This is both a library and an example executable.

To install and run the executable, run these commands depending on your
operating system:

## OSX Installation

```bash
$ brew install icu4c
$ brew install gsasl
$ brew install gnutls
$ git clone https://github.com/mxswd/tau.git
$ stack install --install-ghc
```

## Debian (Jessie) Installation

```bash
$ sudo apt-get install libedit-dev
$ sudo apt-get install libgsasl7-dev
$ sudo apt-get install libxml2-dev
$ sudo apt-get install libgnutls28-dev
$ git clone https://github.com/mxswd/tau.git
$ stack install --install-ghc
```

## Other operating systems

If you would like to contribute build instructions for any other operating
systems, please submit a pull request.

## Usage

```bash
$ stack exec -- tau --help
Haskell-driven XMPP bot

Usage: tau USERHOST USER NICKNAME PASSWORD ROOMHOST ROOM

Available options:
  -h,--help                Show this help text
  USERHOST                 User host Jabber ID
  USER                     User Jabber ID
  NICKNAME                 User nickname
  PASSWORD                 Password
  ROOMHOST                 Room host Jabber ID
  ROOM                     Room Jabber ID
```

You can also use this as a library to build your own custom XMPP bot.  For
example, the above executable was written using the library, like this:

```haskell
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
```

## HipChat Instructions

You can find the relevant information by

* Logging into hipchat.com
* Clicking in the top-right corner on your profile picture and selecting
  "Account Settings"
* Selecting "XMPP/Jabber Info" on the left pane

The following fields will come from that "XMPP/Jabber Account Information" page:

```bash
$ USER=1234_5678               # Hipchat calls this "Username"
$ NICKNAME='Gabriel Gonzalez'  # Hipchat calls this "Room nickname"
$ PASSWORD=topsecret           # The same password that you use to log into HipChat
$ ROOM=1234_room_name          # Hipchat calls this the room's "XMPP/Jabber name"
$ stack exec -- tau chat.hipchat.com "$USER" "$NICKNAME" "$PASSWORD" conf.hipchat.com "$ROOM"
```
