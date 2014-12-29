{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Sound.Chewie.Evaluator(Evaluator(Evaluator),
                              evalIntegrate, evalConvolutionBounds,
                              trapezeIntegrate,
                              defaultEvaluator) where

import Sound.Chewie.Time
import Data.Ratio

data Evaluator = Evaluator {
  evalIntegrate :: forall n. Fractional n => Time -> Time -> (Time -> n) -> n,
  evalConvolutionBounds :: (Time, Time)
}

data TrapezePoint n = TrapezeEnd n
                    | Trapeze n Time (TrapezePoint n)
  deriving (Functor, Eq, Ord, Show)

getSample :: TrapezePoint n -> n
getSample (TrapezeEnd x) = x
getSample (Trapeze x _ _) = x

trapezeIntegrate :: Fractional n => Time -> Time -> Time -> (Time -> n) -> n
trapezeIntegrate dt t0 t1 f = total samples
  where samples = collectTrapezeSamples t0
        collectTrapezeSamples t | t > t1 = error "sampling beyond end"
        collectTrapezeSamples t | t == t1 = TrapezeEnd (f t1)
        collectTrapezeSamples t | t < t0 = error "sampling before beginning"
        collectTrapezeSamples t | otherwise = let nextPoint = min (t + dt) t1
                                                  delta = nextPoint - t
                                                  sample = f t
                                                  rest = collectTrapezeSamples nextPoint
                                                    in if delta <= 0 then error "dt <= 0"
                                                                     else Trapeze sample delta rest
        total (Trapeze x d y) = total y + (fromRational (d / 2))*(x + getSample y)
        total (TrapezeEnd _) = 0

defaultEvaluator :: Integer -> Evaluator
defaultEvaluator hz = Evaluator { evalIntegrate = trapezeIntegrate (1 % hz),
                                  evalConvolutionBounds = (-7, 7) }

