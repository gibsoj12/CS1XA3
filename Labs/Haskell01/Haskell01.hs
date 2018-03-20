module Haskell01 where

macid = "gibsoj12"

data MyEither a b = MyLeft a | MyRight b
    deriving Show

instance Functor (MyEither a) where
    fmap f (MyRight x) = MyRight $ f x
    fmap _ (MyLeft x) = MyLeft x

instance Applicative (MyEither a) where
	pure val = MyRight val
	MyRight f <*> x = fmap f x
	MyLeft y <*> _ = MyLeft y

instance Monad (MyEither a) where
	return                  = MyRight
	(MyLeft a) >>= _        = MyLeft a
	(MyRight b) >>= f       = f b

data List a = Cons a (List a) | Empty
	deriving Show

instance Functor List where
	fmap f (Cons x xs) 	= Cons (f x) (fmap f xs)
	fmap _ Empty		= Empty

instance Monoid (List a) where
	mempty				= Empty
	(Cons x xs) `mappend` ys	= Cons x (xs `mappend` ys)
	Empty `mappend` ys		= ys

instance Applicative List where
	pure x			= Cons x Empty
	(Cons f fs) <*> xs	= (fmap f xs) `mappend` (fs <*> xs)
	Empty <*> _		= Empty

instance Monad List where
	return x		= Cons x Empty
	(Cons x xs) >>= f	= join $ fmap f xs

join :: List (List a) -> List a
join Empty		= Empty
join (Cons xs xss)	= cat xs (join xss)

cat :: List a -> List a -> List a
cat Empty ys		= ys
cat (Cons x xs) ys	= Cons x (cat xs ys)
