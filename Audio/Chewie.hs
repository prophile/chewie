{-# LANGUAGE GADTs #-}

module Audio.Chewie(Chewie, Time,
                    getTime,
                    integrate, integrateFrom) where

import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Monoid

type Time = Rational

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

getTime :: Chewie Time
getTime = CTime id
{-# INLINE getTime #-}

integrateFrom :: (Fractional a) => Time -> Chewie a -> Chewie a
integrateFrom t0 s = CIntegrate t0 s id
{-# INLINE integrateFrom #-}

integrate :: (Fractional a) => Chewie a -> Chewie a
integrate = integrateFrom 0
{-# INLINE integrate #-}

