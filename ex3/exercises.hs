class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry = fmap

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  -- furry :: (a -> b) -> EitherLeft t a -> EitherLeft t b
  -- I needed a bit of help from Learn You A Haskell for this one.
  --    See: http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
  --
  -- Still not too sure whether it's cheating to match against the arguments
  -- though!
  --
  -- TODO - Try to do this point-free. (Pretty sure i need to fmap on the inner
  -- Either but can't work out how to!)
  furry f (EitherLeft e) = case e of
    Left x -> EitherLeft (Left $ f x)
    Right x -> EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight e) = case e of
    Left x -> EitherRight (Left x)
    Right x -> EitherRight (Right $ f x)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' = banana . ((.) unicorn)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn = (: [])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana = (=<<)
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> (t -> b)) -> (t -> a) -> (t -> b)
  --
  -- Alternatively:
  -- banana :: (a -> t -> b) -> (t -> a) -> t -> b
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = error "todo"

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
