module Main where

import Test.Hspec

import Audio.Chewie.Tests(spec)

main :: IO ()
main = hspec spec

