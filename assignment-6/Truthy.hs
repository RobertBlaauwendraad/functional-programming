module Truthy where

class Truthy a where
    truthy :: a -> Bool

instance Truthy Bool where
    truthy = id

instance Truthy Integer where
    truthy = (/= 0)

data Nope = Nope

instance Truthy Nope where
    truthy _ = False

instance (Truthy a, Truthy b) => Truthy (a, b) where
    truthy (a, b) = truthy a || truthy b

infixr 3 &&&
infixr 2 |||

(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) a b = truthy a && truthy b

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) a b = truthy a || truthy b

ifThenElse :: Truthy p => p -> a -> a -> a
ifThenElse p a b = if truthy p then a else b