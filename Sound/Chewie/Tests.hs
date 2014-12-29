module Sound.Chewie.Tests(spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Applicative(pure)

import Sound.Chewie
import Sound.Chewie.Evaluator

import Test.QuickCheck
import Test.QuickCheck.Function

de :: Evaluator
de = defaultEvaluator 4

spec :: Spec
spec = do
  describe "Audio.Chewie.basic" $ do
    prop "evaluates pure correctly" $ \k t ->
      evaluate de (pure (k :: Double)) t `shouldBe` k
    prop "evaluates a trivial ramp correctly" $ \(Small t') -> do
      let t = fromInteger t'
      evaluate de (integrate $ pure 1) t `shouldBe` t
    prop "evaluates fmap correctly" $ \f' k t -> do
      let f = apply f'
      evaluate de (fmap f $ pure (k :: Integer)) t `shouldBe` (f k :: Double)

