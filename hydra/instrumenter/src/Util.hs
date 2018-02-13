module Util
( showError
, consError
, fromRight
, failNothing
, justOrError
) where

showError :: (Show a) => Either a b -> Either String b
showError (Left l)  = Left . show $ l
showError (Right r) = Right r

consError :: c -> Either a b -> Either (c, a) b
consError e' = either (\e -> Left (e',e)) Right

fromRight :: (Show a) => Either a b -> b
fromRight = either (\x -> error $ "unexpected Left" ++ show x) id

failNothing :: a -> Maybe b -> Either a b
failNothing x (Just y) = Right y
failNothing x Nothing  = Left x

justOrError :: String -> Maybe a -> a
justOrError msg (Just y) = y
justOrError msg Nothing  = error msg