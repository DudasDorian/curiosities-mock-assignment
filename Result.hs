module Result where
import Test.SimpleTest.Diff ( Diff(..), Showing(..) )

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

instance (Show err, Eq err, Diff err, Show ok, Eq ok, Diff ok) => Diff (Result err ok) where
  showDifference :: Result err ok -> Result err ok -> [Showing String]
  showDifference (Error err1) (Error err2) = Normal "Error (" : showDifference err1 err2 ++ [Normal ")"]
  showDifference (Success ok1) (Success ok2) = Normal "Success (" : showDifference ok1 ok2 ++ [Normal ")"]
  showDifference (Error err) _ = [Highlight $ "Error (" ++ show err ++ ")"]
  showDifference (Success ok) _ = [Highlight $ "Success (" ++ show ok ++ ")"]

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
