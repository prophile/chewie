{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Sound.Chewie.FromSignal(Interpolation,
                               noInterpolation,
                               linearInterpolation,
                               fromSignal) where

import Sound.Chewie

import qualified Data.Array as Arr
import Control.Monad.Reader.Class(reader)

import Data.Ratio
import Data.List(genericLength)

type Interpolation = forall n. Fractional n => n -> n -> n -> n -> Time -> n

noInterpolation :: Interpolation
noInterpolation a b c d delta = b

linearInterpolation :: Interpolation
linearInterpolation a b c d delta = b*(1 - del) + c*del
  where del = fromRational delta

fromSignal :: forall n. (Fractional n) => Time -> Interpolation -> [n] -> Chewie n
fromSignal dt interp samples = reader sample
  where sample t = readArrayAt (t / dt)
        readArrayAt ix = interp a b c d offset
          where (position, remainder) = divMod num denom
                offset = remainder % denom
                num = numerator ix
                denom = denominator ix
                a = arrayVal (position - 1)
                b = arrayVal position
                c = arrayVal (position + 1)
                d = arrayVal (position + 2)
        arrayVal n | n < arrStart = 0
        arrayVal n | n >= arrEnd = 0
        arrayVal n | otherwise = arr Arr.! n
        (arrStart, arrEnd) = Arr.bounds arr
        arr :: Arr.Array Integer n
        arr = Arr.listArray (0, genericLength samples) samples

