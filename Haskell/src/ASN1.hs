-- Baby's first Haskell module... trying to figure out unit testing in Eclipse.

module ASN1 where

myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]