module ReaderAlgebra where

import Data.List (sortBy)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (<*>) (Reader f) (Reader ra) = Reader $ \r -> f r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) f = Reader $ \r ->
    let
      a = ra r
    in  
      runReader (f a) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks ra = Reader ra

data AppState = AppState
  { players :: [String]
  , ages :: [Int]
  } deriving (Show, Eq)

appState :: AppState
appState = AppState ["bob", "joe", "sally"] [44, 29, 22]

addUp :: Num a => [a] -> a
addUp = sum

multUp :: Num a => [a] -> a
multUp = product

-- Here we use Reader to pull out parts of the global state
-- and hand it to more generic functions. This lets us keep
-- the functions generic
computation :: Reader AppState (Int, Int)
computation = do
 a <- asks ages
 s <- return $ addUp a
 p <- return $ multUp a
 return $ (s, p)

-- Here we use Reader to avoid manually passing around AppState
longestName :: Reader AppState String
longestName = do
  ps <- asks players
  return $ getLongest ps
  where
    withLengths xs = zip xs (map length xs)
    sorted = sortBy (\p1 p2 -> (snd p2) `compare` (snd p1))
    longest = fst . head
    getLongest = longest . sorted . withLengths
  

main :: IO ()
main = do
  n <- return $ runReader computation appState
  putStrLn $ show n
