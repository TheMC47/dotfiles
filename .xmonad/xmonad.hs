module Main where

import           Config (privateConfig)
import           XMonad (xmonad)

main :: IO ()
main = privateConfig >>= xmonad
