module StateAlgebra where

-- the structure here is \s -> (_ , s)
-- wherease with reader it was \s -> _
newtype State s a = State { runState :: s -> (a, s) }

-- this is a way to load a state into the structure
put :: s -> State s ()
put s = State $ \_ -> ((),s)

-- seems to be used to extract out the state after running
-- computations on it
get :: State s s
get = State $ \s -> (s, s)

-- a way to run a function over the state
modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

-- runs and extracts the state
execState :: State s a -> s -> s
execState sa s =
  let
    (_, y) = runState sa $ s
  in
    y

-- runs and extracts the other thing
evalState :: State s a -> s -> a
evalState sa =
  fst . runState sa

instance Functor (State s) where
  fmap f s = State $ \x ->
    let
      (n, p) = (runState s) x
    in
      (f n, p)

instance Applicative (State s) where
  pure a = State $ \x -> (a, x)
  (<*>) (State f) (State n) = State $ \x ->
    let
      (p, _) = n x
      (g, y) = f x
    in
      (g p, y)

instance Monad (State s) where
  return = pure
  (>>) (State f) (State g) = State $ \x ->
    let
      (_, q) = f x      
    in
      g q
  (>>=) (State n) f = State $ \x ->
    let
      (p, x') = n x
    in
      runState (f p) x'

test :: State Int String
test = do
  modify (+1)
  modify (+1)
  modify (+1)
  n <- get
  return $ show n
    
main :: IO ()
main = do
  print $ execState test 0
  print $ evalState test 0

-- for instances ,think of it is as State -> (_, State)
-- for interface fns, focus just on State. Operate on it using put, get, and modify. In the end, get it and fill in _
