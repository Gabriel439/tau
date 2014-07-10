{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, RankNTypes #-}
module Hint (HintCmds, Result, Cmd(..), startHint, evalHint) where

-- Hint from https://github.com/mchakravarty/language-c-inline/blob/master/tests/objc/app/Interpreter.hs
-- Copyright (c) [2013..2014] Manuel M T Chakravarty.  All rights reserved. BSD3 licensed.

import Control.Monad
import Control.Monad.Catch
import Control.Concurrent
import Control.Exception.Extensible (ErrorCall(..))
import qualified Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter (Extension(..))

type Result = Either String String
type HintCmds = (ThreadId, MVar (Maybe (Hint.InterpreterT IO ())))
data Cmd = Eval | TypeOf

hintCmd :: forall (m :: * -> *).
                 Hint.MonadInterpreter m =>
                 Cmd -> String -> m String
hintCmd Eval = Hint.eval
hintCmd TypeOf = Hint.typeOf

funExtensions :: [Extension]
funExtensions = [NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables, FlexibleInstances, EmptyDataDecls, KindSignatures, BangPatterns, TypeSynonymInstances, TemplateHaskell, Generics, NamedFieldPuns, PatternGuards, MagicHash, TypeFamilies, StandaloneDeriving, TypeOperators, OverloadedStrings, GADTs, DeriveDataTypeable, QuasiQuotes, ViewPatterns, DeriveFunctor, DeriveTraversable, DeriveFoldable, ExtendedDefaultRules]

startHint :: forall a. IO (ThreadId, MVar (Maybe (Hint.InterpreterT IO a)))
startHint = do
  cmdQueue <- newEmptyMVar
  let hintLoop = Hint.lift (takeMVar cmdQueue) >>= \command -> case command of
        Just cmd -> cmd >> hintLoop
        Nothing -> return ()
  tid <- forkIO $ void $ Hint.runInterpreter (Hint.setImports ["Prelude"]
    >> Hint.set [Hint.languageExtensions Hint.:= (funExtensions ++ Hint.glasgowExtensions)]
    >> hintLoop)
  return (tid, cmdQueue)

evalHint :: forall (t :: (* -> *) -> * -> *).
            (Hint.MonadInterpreter (t IO), Hint.MonadTrans t) =>
            String
            -> Cmd
            -> (ThreadId, MVar (Maybe (t IO ())))
            -> IO (Either String String)
evalHint msg cmd (tid, cmdQueue) = do
  result <- newEmptyMVar
  void $ forkIO $ do
    putMVar cmdQueue $ Just $ do
      res' <- (do
        !res <- (hintCmd cmd) msg
        return (Right res))
                  `catch` (return . Left . pprError)
                  `catch` (return . Left . (show :: SomeException -> String))
      Hint.lift $ putMVar result res'
  void $ forkIO $ do
    threadDelay 100000
    notOk <- isEmptyMVar result
    if notOk
      then throwTo tid (ErrorCall "time out")
      else return ()
  readMVar result

pprError :: Hint.InterpreterError -> String
pprError (Hint.UnknownError msg) = msg
pprError (Hint.WontCompile errs) = "Compile time error: \n" ++ unlines (map Hint.errMsg errs)
pprError (Hint.NotAllowed msg)   = "Permission denied: " ++ msg
pprError (Hint.GhcException msg) = "Internal error: " ++ msg
