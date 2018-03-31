module Lib
    ( someFunc
    , module Bird
    , module RealWorld
    ) where

import           Bird
import           RealWorld

someFunc :: IO ()
someFunc = putStrLn "someFunc"
