-- Pure Functional Data Structures
-- Chapter 1

module StackData where

import           Data.Maybe


-- | Playing with Data

a :: Stack Int
a = 1 |: 2 |: 3 |: 4 |: 5 |: empty

doubleList :: Funktor f => f Int -> f Int
doubleList = mapp (* 2)

stack = repeet 42 42

someOps :: Stack Int
someOps = mapp (*3) $ updateAtIndex (*99) 10 $ mapp (*5) $ replaceAtIndex 818 20 stack

-- | Data

data Stack a = Nil | Cons a (Stack a)
    deriving (Eq, Show)

empty :: Stack a
empty = Nil


isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty _   = False


hed :: Stack a -> Maybe a
hed Nil        = Nothing
hed (Cons a _) = Just a


tale :: Stack a -> Stack a
tale Nil        = Nil
tale (Cons _ t) = t


atIndex :: Int -> Stack a -> Maybe a
atIndex 0 xs = hed xs
atIndex n xs = atIndex (n-1) (tale xs)


updateAtIndex :: (a -> a) -> Int -> Stack a -> Stack a
updateAtIndex update 0 Nil         = Nil
updateAtIndex update 0 (Cons x xs) = Cons (update x) xs
updateAtIndex update n (Cons x xs) = Cons x (updateAtIndex update (n-1) xs)
updateAtIndex update n Nil         = Nil


replaceAtIndex :: a -> Int -> Stack a -> Stack a
replaceAtIndex val = updateAtIndex (const val)


repeet :: Int -> a -> Stack a
repeet 0 a = Cons a Nil
repeet n a = Cons a (repeet (n-1) a)


lengthh :: Stack a -> Int
lengthh = lengthh' 0
    where
        lengthh' 0 Nil         = 0
        lengthh' n Nil         = n-1
        lengthh' n (Cons x xs) = lengthh' (n+1) xs



class Funktor f where
    mapp :: (a -> b) -> f a -> f b


instance Funktor Stack where
    mapp f Nil         = Nil
    mapp f (Cons x xs) = Cons (f x) (mapp f xs)


infixr 1 |:

(|:) :: a -> Stack a -> Stack a
a |: b = Cons a b
