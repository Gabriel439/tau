{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Example usage:
-- 
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Control.Applicative ((<|>))
-- > import Data.Monoid ((<>))
-- > import Network.Xmpp.Bot
-- > import Turtle (Pattern, chars, match)
-- > 
-- > parseMessage :: Pattern HintCommand
-- > parseMessage
-- >     =   fmap Eval   ("> "       *> chars)
-- >     <|> fmap TypeOf ("> :type " *> chars)
-- > 
-- > handleMessage :: UserName -> Message -> Hint s [Message]
-- > handleMessage _ (Message msg) = case match parseMessage msg of
-- >     cmd:_ -> do
-- >         result <- command cmd
-- >         return (map prefix result)
-- >     _     -> return []
-- >   where
-- >     prefix (Message txt) = Message ("⤷ " <> txt)
-- > 
-- > main :: IO ()
-- > main = do
-- >     o <- options "Haskell-driven XMPP bot" opts
-- >     runXmpp o () handleMessage

module Network.Xmpp.Bot (
    -- * Commands
      runXmpp
    , command
    , opts
    , Turtle.options

    -- * Types
    , Options(..)
    , UserName(..)
    , Message(..)
    , HintCommand(..)
    , Hint

    -- * Re-exports
    , MonadState(..)
    , MonadIO(..)
    , Text
    ) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Data.Monoid ((<>))
import Lens.Family (to, toListOf, view)
import Network.Xmpp.Bot.Hint

import qualified Data.Text             as Text
import qualified Data.XML.Types        as XML
import qualified Network
import qualified Network.Protocol.XMPP as XMPP
import qualified Turtle

-- | Hipchat connection options
data Options = Options
    { userHost :: Text
    -- ^ User host Jabber ID
    , user     :: Text
    -- ^ User Jabber ID
    , nickname :: Text
    -- ^ User nickname
    , password :: Text
    -- ^ Password
    , roomHost :: Text
    -- ^ Room nickname
    , room     :: Text
    -- ^ Room host Jabber ID
    }

-- | Parser for command-line options
opts :: Turtle.Parser Options
opts =  Options
    <$> Turtle.argText "userhost" "User host Jabber ID"
    <*> Turtle.argText "user"     "User Jabber ID"
    <*> Turtle.argText "nickname" "User nickname"
    <*> Turtle.argText "password" "Password"
    <*> Turtle.argText "roomhost" "Room host Jabber ID"
    <*> Turtle.argText "room"     "Room Jabber ID"

toJabberID :: Text -> IO XMPP.JID
toJabberID jidText = case XMPP.parseJID jidText of
    Nothing  -> Turtle.die ("Invalid Jabber ID: " <> jidText)
    Just jid -> return jid

-- | Run an Xmpp bot
runXmpp :: Options -> s -> (UserName -> Message -> Hint s [Message]) -> IO ()
runXmpp (Options {..}) initialState handleMessage = do
    outerLoop `catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = do
        Turtle.err ("[!] Exception caught: " <> Turtle.repr e)
        outerLoop

    outerLoop = do
        userJID   <- toJabberID (user <> "@" <> userHost)
        serverJID <- toJabberID  userHost
        roomJID   <- toJabberID (room <> "@" <> roomHost <> "/" <> nickname)
        let server = XMPP.Server
                { XMPP.serverJID      = serverJID
                , XMPP.serverHostname = Text.unpack userHost
                , XMPP.serverPort     = Network.PortNumber 5222
                }

        Turtle.echo ("[+] Connecting to: " <> Turtle.repr serverJID)
        x <- XMPP.runClient server userJID user password XMPP.getSession
        case x of
            Left  e       -> Turtle.die (Turtle.repr e)
            Right session -> do
                runHint
                    session
                    initialState
                    (xmpp userJID roomJID handleMessage)

joinRoom :: XMPP.JID -> XMPP.JID -> XMPP.XMPP ()
joinRoom roomJID userJID' = XMPP.putStanza (XMPP.Presence
    { XMPP.presenceType     = XMPP.PresenceAvailable
    , XMPP.presenceTo       = Just roomJID
    , XMPP.presenceFrom     = Just userJID'
    , XMPP.presenceID       = Nothing
    , XMPP.presenceLang     = Nothing
    , XMPP.presencePayloads =
        [ XML.Element
            { XML.elementName       = XML.Name
                { XML.nameLocalName = "x"
                , XML.nameNamespace = Just "http://jabber.org/protocol/muc"
                , XML.namePrefix    = Nothing
                }
            , XML.elementAttributes = []
            , XML.elementNodes      = []
            }
        ]
    })

sayRoom :: XMPP.JID -> XMPP.JID -> Text -> XMPP.XMPP ()
sayRoom roomJID userJID' txt =
    XMPP.putStanza (XMPP.Message
        { XMPP.messageType     = XMPP.MessageGroupChat
        , XMPP.messageTo       = Just roomJID
        , XMPP.messageFrom     = Just userJID'
        , XMPP.messageID       = Nothing
        , XMPP.messageLang     = Nothing
        , XMPP.messagePayloads =
            [ XML.Element
                { XML.elementName       = "body"
                , XML.elementAttributes = []
                , XML.elementNodes      =
                    [XML.NodeContent (XML.ContentText txt)]
                }
            ]
        } )

xmpp
    :: XMPP.JID
    -> XMPP.JID
    -> (UserName -> Message -> Hint s [Message])
    -> Hint s ()
xmpp userJID roomJID handleMessage = do
    liftIO (Turtle.echo  "[+] Connected successfully")
    liftIO (Turtle.echo ("[+] Binding resource to user: " <> Turtle.repr userJID))
    userJID' <- liftXMPP (XMPP.bindJID userJID)
    liftIO (Turtle.echo ("[+] Resource bound successfully: " <> Turtle.repr userJID'))

    liftIO (Turtle.echo ("[+] Joining room: " <> Turtle.repr roomJID))
    liftXMPP (joinRoom roomJID userJID')
    liftIO (Turtle.echo  "[+] Room joined successfully")

    let innerLoop = do
            stanza <- liftXMPP XMPP.getStanza
            let delayLens = stanzaPayload . named "delay"
            let keep =  null (toListOf delayLens          stanza)
                     && null (toListOf stanzaPresenceType stanza)
            when keep (do
                let bodyLens
                        = stanzaPayload
                        . named "body"
                        . nodes
                        . traverse
                        . content
                        . text
                let body = view bodyLens stanza
                let userNameLens
                        = stanzaFrom
                        . traverse
                        . resource
                        . traverse
                        . to XMPP.strResource
                let userName = view userNameLens stanza
                responses <- handleMessage (UserName userName) (Message body)
                liftXMPP (mapM_ (sayRoom roomJID userJID' . getMessage) responses) )
            innerLoop
    innerLoop

stanzaFrom
    :: Functor f
    => (Maybe XMPP.JID -> f (Maybe XMPP.JID))
    -> (XMPP.ReceivedStanza -> f XMPP.ReceivedStanza)
stanzaFrom k (XMPP.ReceivedMessage msg) =
    fmap
        (\x -> XMPP.ReceivedMessage (msg { XMPP.messageFrom = x }))
        (k (XMPP.messageFrom msg))
stanzaFrom k (XMPP.ReceivedPresence msg) =
    fmap
        (\x -> XMPP.ReceivedPresence (msg { XMPP.presenceFrom = x }))
        (k (XMPP.presenceFrom msg))
stanzaFrom k (XMPP.ReceivedIQ msg) =
    fmap
        (\x -> XMPP.ReceivedIQ (msg { XMPP.iqFrom = x }))
        (k (XMPP.iqFrom msg))

stanzaPayload
    :: Applicative f
    => (XML.Element -> f XML.Element)
    -> (XMPP.ReceivedStanza -> f XMPP.ReceivedStanza)
stanzaPayload k (XMPP.ReceivedMessage msg) =
    fmap
        (\x -> XMPP.ReceivedMessage (msg { XMPP.messagePayloads = x }))
        (traverse k (XMPP.messagePayloads msg))
stanzaPayload k (XMPP.ReceivedPresence msg) =
    fmap
        (\x -> XMPP.ReceivedPresence (msg { XMPP.presencePayloads = x }))
        (traverse k (XMPP.presencePayloads msg))
stanzaPayload k (XMPP.ReceivedIQ msg) =
    fmap
        (\x -> XMPP.ReceivedIQ (msg { XMPP.iqPayload = x }))
        (traverse k (XMPP.iqPayload msg))

stanzaPresenceType
    :: Applicative f
    => (XMPP.PresenceType -> f XMPP.PresenceType)
    -> (XMPP.ReceivedStanza -> f XMPP.ReceivedStanza)
stanzaPresenceType k (XMPP.ReceivedPresence p) =
    fmap
        (\x -> XMPP.ReceivedPresence (p { XMPP.presenceType = x }))
        (k (XMPP.presenceType p))
stanzaPresenceType _  s                        =
    pure s

resource
    :: Functor f
    => (Maybe XMPP.Resource -> f (Maybe XMPP.Resource))
    -> (XMPP.JID -> f XMPP.JID)
resource k jid =
    fmap
        (\x -> jid { XMPP.jidResource = x })
        (k (XMPP.jidResource jid))

named :: Applicative f
    => Text
    -> (XML.Element -> f XML.Element )
    -> (XML.Element -> f XML.Element)
named n k element
    | XML.nameLocalName (XML.elementName element) == n = k    element
    | otherwise                                        = pure element

nodes
    :: Functor f
    => ([XML.Node] -> f [XML.Node])
    -> (XML.Element -> f XML.Element)
nodes k element =
    fmap
        (\x -> element { XML.elementNodes = x })
        (k (XML.elementNodes element))

content
    :: Applicative f
    => (XML.Content -> f XML.Content)
    -> (XML.Node -> f XML.Node)
content k (XML.NodeContent x) = fmap XML.NodeContent (k x)
content _  n                  = pure n

text
    :: Applicative f
    => (Text -> f Text)
    -> (XML.Content -> f XML.Content)
text k (XML.ContentText x) = fmap XML.ContentText (k x)
text _  c                  = pure c
