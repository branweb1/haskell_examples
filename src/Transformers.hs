module Transformers where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

-- runIdentity $ evalStateT floop 0
-- State(Idenity)
floop :: StateT Int Identity Int
floop = do
  put 7
  modify (\n -> n * 2)
  get

-- :i StateT
-- newtype StateT s (m :: * -> *) a
-- evalState (evalStateT doop 0) "hello"
-- State(State)
doop :: StateT Int (State String) (Int, String)
doop = do
  modify (+1)
  lift $ modify (++ " world")
  num <- get
  wrd <- lift get
  return (num, wrd)

-- :i ReaderT
-- newtype ReaderT r (m :: * -> *) a
-- runReaderT woop [1..10]
-- runIO done by haskell, but you could sup Identity for IO and do runIdentity $ runReaderT woop [1..10]
-- Reader(IO)
woop :: ReaderT [Int] IO Int
woop = do
  xs <- ask
  liftIO $ print $ "orig list" ++ (show xs)
  return $ sum xs


boop :: StateT Int IO Int
boop = do
  s <- get
  liftIO $ print ("orig state is " ++ (show s))
  modify (\s -> s * 2)
  s1 <- get
  liftIO $ print ("doubled it's " ++ (show s1))
  modify (\s -> s + 10)
  s2 <- get
  liftIO $ (print ("and final result is " ++ (show s2)))
  get
