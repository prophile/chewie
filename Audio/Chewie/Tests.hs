module Audio.Chewie.Tests(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Applicative(pure)

import Audio.Chewie
import Audio.Chewie.Evaluator

import Test.QuickCheck

de :: Evaluator
de = defaultEvaluator 4

spec :: Spec
spec = do
  describe "Audio.Chewie.basic" $ do
    prop "evaluates pure correctly" $ \k t ->
      evaluate de (pure (k :: Double)) t `shouldBe` k
    prop "evaluates a ramp correctly" $ \(Small t') -> do
      let t = fromInteger t'
      evaluate de (integrate $ pure 1) t `shouldBe` t

