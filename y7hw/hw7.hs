module JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Turns out (+++) is just Append
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append jl1 jl2

-- Get the tag from a JoinList. Since tag is a monoid we this isn't hard.
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append jl1 jl2) = tag jl1 <> tag jl2
