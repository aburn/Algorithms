module Main where

import           Lib

main :: IO ()
main = do
    someFunc
    print (countdown 831 [1,3,7,10,25,50])
