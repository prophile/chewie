{-# LANGUAGE RankNTypes #-}

module Audio.Chewie.Evaluator(Evaluator, evalIntegrate, evalConvolve) where

import Audio.Chewie.Time

data Evaluator = Evaluator {
  evalIntegrate :: forall n. Fractional n => Time -> Time -> (Time -> n) -> n,
  evalConvolve :: forall n. Fractional n => (Time -> n) -> (Time -> n) -> Time -> n
}

