{-# LANGUAGE BangPatterns #-}

module Main where

import Lib

import Data.Tuple
import Data.List
import Criterion.Main
import Control.Monad.Fix
import Data.Char

original :: Integer -> [Integer]
original x = reverse $ digits x where
    digits x | x <= 0 = []
             | otherwise = (mod x 10) : (digits $ div x 10)

cheating :: Integer -> [Integer]
cheating x = map (fromIntegral . digitToInt) $ show x


fixDig :: Integer -> [Integer]
fixDig x = (fix digits) x (:[]) where
    digits g !n !f
      | div n 10 > 0 = g (div n 10) (: f (mod n 10))
      | otherwise = f n 

unfoldDig :: Integer -> [Integer]
unfoldDig x = reverse $ unfoldr f x
  where f 0 = Nothing
        f n = Just (swap $ divMod n 10) 

input = 2^9999

main :: IO ()
main = defaultMain [
  bgroup "test" [ bench "orig" $ nf original input 
                , bench "cheat" $ nf cheating input 
                , bench "fix" $ nf fixDig input
                , bench "unfoldr" $ nf unfoldDig input
                ]
  ]
