module Audio.Chewie.Tests(spec) where

import Test.Hspec
import Distribution.TestSuite

spec :: Spec
spec = do
  describe "Audio.Chewie.bees" $ do
    it "passes" $ do
      True `shouldBe` True

