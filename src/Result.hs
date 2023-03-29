module Result where

data Result err ok = Error err | Success ok deriving (Show, Eq)

instance Functor (Result err) where
  fmap _ (Error e) = Error e
  fmap f (Success ok) = Success (f ok)

instance Applicative (Result err) where
  pure x = Success x
  (Success f) <*> (Success x) = Success (f x)
  (Error e) <*> _ = Error e
  _ <*> (Error e) = Error e

instance Monad (Result err) where
  (Success ok) >>= f = f ok
  (Error e) >>= _ = Error e

mapError :: (err -> err') -> Result err ok -> Result err' ok
mapError _ (Success s) = Success s
mapError f (Error e) = Error $ f e

isSuccess :: Result err ok -> Bool
isSuccess (Error _) = False
isSuccess (Success _) = True

isError :: Result err ok -> Bool
isError (Error _) = True
isError (Success _) = False

getSuccess :: Result err ok -> ok -> ok
getSuccess (Error _) d = d
getSuccess (Success ok) _ = ok

getError :: Result err ok -> err -> err
getError (Error e) _ = e
getError (Success o) d = d

-- >>> Success 2 >>= (\x -> Success (x + 3))
-- Success 5

--- >>> (+) <$> (Success 2) <*> (Success 3)
-- Success 5
