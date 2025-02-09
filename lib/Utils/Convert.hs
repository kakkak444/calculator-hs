{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

module Utils.Convert
    ( TryFrom(..), From(..)
    , TryInto(..), Into(..)

    , tryIntoDefault
    , intoDefault
    ) where

import Data.Void

class TryInto a into where
    type IntoError a into
    tryInto :: a -> Either (IntoError a into) into

    default tryInto :: (TryFrom into a, FromError into a ~ IntoError a into) => a -> Either (IntoError a into) into
    tryInto = tryIntoDefault
    {-# INLINE tryInto #-}

class (TryInto a into, IntoError a into ~ Void) => Into a into where
    into :: a -> into

    default into :: (From into a) => a -> into
    into = intoDefault
    {-# INLINE into #-}

class TryFrom s from where
    type FromError s from
    tryFrom :: from -> Either (FromError s from) s

class (TryFrom s from, FromError s from ~ Void) => From s from where
    from :: from -> s
    {-# INLINE from #-}
    from = either absurd id . tryFrom


tryIntoDefault :: (TryFrom into from, FromError into from ~ IntoError from into) => from -> Either (FromError into from) into
{-# INLINE tryIntoDefault #-}
tryIntoDefault = tryFrom

intoDefault :: (From into from) => from -> into
{-# INLINE intoDefault #-}
intoDefault = from


instance TryFrom a a where
    type FromError a a = Void
    tryFrom = Right
    {-# INLINE tryFrom #-}
instance From a a

instance TryInto a a where
    type IntoError a a = Void
instance Into a a


instance (Integral a) => TryFrom Integer a where
    type FromError Integer a = Void
    tryFrom = Right . toInteger
    {-# INLINE tryFrom #-}
instance (Integral a) => From Integer a

instance (Integral a) => TryInto a Integer where
    type IntoError a Integer = Void
instance (Integral a) => Into a Integer


instance (Integral a) => TryFrom Rational a where
    type FromError Rational a = Void
    tryFrom = Right . fromIntegral
    {-# INLINE tryFrom #-}
instance (Integral a) => From Rational a

instance (Integral a) => TryInto a Rational where
    type IntoError a Rational = Void
instance (Integral a) => Into a Rational
