{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding ((++), head, tail, length, null, (!!))
import qualified Prelude ((++), head, tail, length, null, (!!))

class List l where
  nil :: l a
  cons :: a -> l a -> l a
  head :: l a -> a
  tail :: l a -> l a
  (++) :: l a -> l a -> l a
  (!!) :: l a -> Int -> a
  toList :: [a] -> l a
  fromList :: l a -> [a]

instance List [] where
  nil = []
  cons el l = el:l
  head (x:xs) = x
  tail (x:xs) = xs
  (++) (x:xs) l = x:(xs ++ l)
  (++) [] l = l
  (!!) (x:xs) 1 = x
  (!!) (x:xs) n = ((!!) xs (n-1))
  toList l = l
  fromList l = l

class List l => SizedList l where
  length :: l a -> Int
  null :: l a -> Bool
  null l = length l == 0

instance SizedList [] where
  length (x:xs) = 1 + (length xs)
  length [] = 0
  null [] = True
  null (x:xs) = False

data SL l a = SL {len :: Int, list :: l a}

instance List l => List (SL l) where
  nil = SL {len = 0, list = nil}
  cons el l = SL {len = (len l) + 1, list = cons el (list l)}
  head = head . list
  tail l = SL {len = (len l) - 1, list = (tail . list) l}
  (++) a b = SL {len = len a + len b, list = (list a) ++ (list b)}
  (!!) l n = (list l) !! n
  toList l = SL {len = length l, list = toList l}
  fromList l = (fromList . list) l

instance List l => SizedList (SL l) where
  length = len

infixr 6 :+
data AppList a = Nil | Sngl a | AppList a :+ AppList a

lengthAL (a :+ b) = 
  lengthAL a + lengthAL b
lengthAL (Sngl _) = 1
lengthAL Nil = 0

instance List AppList where
  nil = Nil
  cons el l = (Sngl el) :+ l
  head (a :+ b) = head a
  head (Sngl a) = a
  tail ((Sngl a) :+ b) = b
  tail (a :+ b) = (tail a) :+ b
  (++) a b = a :+ b
  (!!) (a :+ b) n =
    let llen = lengthAL a in
    if llen >= n then
      a !! n
    else
      b !! (n - llen)
  (!!) (Sngl a) 1 = a
  toList (x:xs) = (Sngl x) :+ (toList xs)
  toList [] = Nil
  fromList (a :+ b) = fromList a ++ fromList b
  fromList (Sngl a) = [a]
  fromList Nil = []

instance SizedList AppList where
  length = lengthAL

instance Show a => Show (AppList a) where
  show l = (show . fromList) l


newtype DiffList a = DL ([a] -> [a])

instance List DiffList where
  nil = DL (\xs -> xs)
  cons el (DL f) = DL (\xs -> el:(f xs))
  head (DL f) = (head . f) []
  tail (DL f) = DL (tail . f)
  (++) (DL a) (DL b) = DL (a . b)
  (!!) (DL f) = (!!) (f [])
  toList l = DL ((++) l)
  fromList (DL f) = f []

instance SizedList DiffList where
  length = length . fromList
  null = null . fromList

instance Show a => Show (DiffList a) where
  show l = (show . fromList) l

