module Bot where

import Hint

-- The conf type. Feel free to change it.
data Conf = Conf {
    messageQueue :: (String, String)
  , hint :: HintCmds
  }

-- the initial conf value. can use IO to construct
mkConf :: IO Conf
mkConf = do
  cmdQueue <- startHint
  return Conf {
      messageQueue = ("Nobody", "hello")
    , hint = cmdQueue
    }

-- Takes a conf, does some IO and returns a list of messages to reply with and a conf.
-- Use the conf to pass state around.
receiveMessage :: Conf -> String -> String -> String -> String -> IO ([String], Conf)
receiveMessage conf xmpproom from to ('!':' ':msg) = do
  let lastmsg = messageQueue conf
  return ([fst lastmsg ++ " said: " ++ snd lastmsg], conf { messageQueue = (from, msg) })

-- hint eval
receiveMessage conf xmpproom from to ('>':' ':msg) = do
  res <- evalHint msg Eval (hint conf)
  let res' = case res of
              (Left e) -> "error: " ++ e
              (Right v) -> v
  return ([res'], conf)

-- hint type
receiveMessage conf xmpproom from to (':':'t':' ':msg) = do
  res <- evalHint msg TypeOf (hint conf)
  let res' = case res of
              (Left e) -> "error: " ++ e
              (Right v) -> v
  return ([res'], conf)

-- default case, all messages
receiveMessage conf xmpproom from to _msg = do
  -- don't reply with something if the message doesn't start with !. you could get in a loop!
  return ([], conf)
