{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Sound.Chewie(Chewie, Time,
                    integrate, integrateFrom,
                    evaluate) where

import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Monoid

import Control.Monad.Reader.Class

import Sound.Chewie.Time
import Sound.Chewie.Evaluator

data Chewie a where
  CTime :: (Time -> a) -> Chewie a
  CIntegrate :: (Fractional b) => Time -> Chewie b -> (b -> a) -> Chewie a
  CConvolve :: (Fractional b) => Chewie b -> Chewie b -> (b -> a) -> Chewie a
  CAp :: Chewie (a -> b) -> Chewie a -> Chewie b
  CJoin :: Chewie (Chewie a) -> Chewie a
  CConst :: a -> Chewie a

instance Functor Chewie where
  fmap f (CTime g) = CTime (f . g)
  fmap f (CIntegrate t0 s g) = CIntegrate t0 s (f . g)
  fmap f (CConvolve l r g) = CConvolve l r (f . g)
  fmap f (CAp g x) = CAp (fmap (f .) g) x
  fmap f (CJoin x) = CJoin (fmap (fmap f) x)
  fmap f (CConst k) = CConst (f k)

instance Applicative Chewie where
  pure = CConst
  {-# INLINE pure #-}
  CConst f <*> x = fmap f x
  f <*> CConst x = fmap ($ x) f
  CTime f <*> CTime x = CTime (f <*> x)
  f <*> x = CAp f x

instance Monad Chewie where
  return = CConst
  {-# INLINE return #-}
  x >>= f = joinChewie (fmap f x)
    where joinChewie (CConst k) = k
          joinChewie x = CJoin x

instance MonadReader Time Chewie where
  reader = CTime
  {-# INLINE reader #-}
  local f = descend
    where
      descend :: Chewie a -> Chewie a
      descend x@(CConst _) = x
      descend (CTime g) = CTime (g . f)
      descend (CJoin x) = CJoin (descend x)
      descend (CAp g x) = CAp (descend g) (descend x)
      descend (CConvolve l r g) = CConvolve (descend l) (descend r) g
      descend (CIntegrate t0 s g) = CIntegrate (f t0) (descend s) g
  {-# INLINE local #-}

delta :: (Num a) => Time -> a
delta 0 = 1
delta _ = 0

instance (Fractional a) => Monoid (Chewie a) where
  mempty = CTime delta
  {-# INLINE mempty #-}
  x `mappend` y = CConvolve x y id
  {-# INLINE mappend #-}

instance (Num a) => Num (Chewie a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}

  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}

  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

integrateFrom :: (Fractional a) => Time -> Chewie a -> Chewie a
integrateFrom t0 s = CIntegrate t0 s id
{-# INLINE integrateFrom #-}

integrate :: (Fractional a) => Chewie a -> Chewie a
integrate = integrateFrom 0
{-# INLINE integrate #-}

evaluate :: Evaluator -> Chewie a -> Time -> a
evaluate ev = sample
  where sample :: Chewie b -> Time -> b
        sample (CTime f) = f
        sample (CConst k) = const k
        sample (CJoin s) = \t -> sample (sample s t) t
        sample (CAp f x) = \t -> (sample f t) (sample x t)
        sample (CIntegrate t0 s f) = \t -> f (eInt t0 t (sample s))
        sample (CConvolve l r f) = f . eConv (sample l) (sample r)
        eInt :: Fractional n => Time -> Time -> (Time -> n) -> n
        eInt t0 t1 | t0 <= t1  = evalIntegrate ev t0 t1
        eInt t0 t1 | otherwise = negate . evalIntegrate ev t1 t0
        (convBoundL, convBoundR) = evalConvolutionBounds ev
        eConv :: (Fractional n) => (Time -> n) -> (Time -> n) -> Time -> n
        eConv l r t = eInt (t - convBoundL) (t + convBoundR) query
          where query tau = (l tau) * (r (t - tau))

