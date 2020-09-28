module Main where

import           Config (privateConfig)
import           XMonad (xmonad)

-- >>> map (++ ".") ["1", "2"]

main :: IO ()
main = privateConfig >>= xmonad