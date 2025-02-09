{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module AST.Types.Number
    ( NumType(..)
    , Number(..)
    , SomeNumber(..)

    , toInt, toRatio, toReal
    ) where

import Data.Ratio (denominator, numerator, (%))
import Data.Bifunctor (bimap)

data NumType
    = IntType
    | RatioType
    | RealType
    deriving (Show, Eq)

data Number (ty :: NumType) where
    MkInt   :: Integer  -> Number 'IntType
    MkRatio :: Rational -> Number 'RatioType
    MkReal  :: Double   -> Number 'RealType
deriving instance Show (Number ty)

data SomeNumber = forall (ty :: NumType). MkSomeNumber (Number ty)
deriving instance Show SomeNumber

toInt :: Number ty -> Number 'IntType
{-# INLINE toInt #-}
toInt = \case
    MkInt   a -> MkInt a
    MkRatio a ->
        let denom = denominator a
            numer = numerator a
        in
            MkInt $ numer `quot` denom
    MkReal a -> MkInt $ floor a

toRatio :: Number ty -> Number 'RatioType
{-# INLINE toRatio #-}
toRatio = \case
    MkInt   a -> MkRatio $ fromInteger a
    MkRatio a -> MkRatio a
    MkReal  a -> MkRatio $ toRational a

toReal :: Number ty -> Number 'RealType
{-# INLINE toReal #-}
toReal = \case
    MkInt   a -> MkReal $ fromInteger a
    MkRatio a -> MkReal $ fromRational a
    MkReal  a -> MkReal a


instance Num SomeNumber where
    (+) = applyNumFunc2' (+)
    (-) = applyNumFunc2' (-)
    (*) = applyNumFunc2' (*)
    abs = applyNumFunc' abs
    signum = applyNumFunc' signum
    fromInteger = MkSomeNumber . MkInt
instance Fractional SomeNumber where
    (MkSomeNumber a) / (MkSomeNumber b) =
        case a of
            MkInt a' ->
                case b of
                    MkInt   b' -> MkSomeNumber $ MkRatio $ a' % b'
                    MkRatio _  -> MkSomeNumber $ toRatio a / b
                    MkReal  _  -> MkSomeNumber $ toReal  a / b
            MkRatio _ ->
                case b of
                    MkInt   _ -> MkSomeNumber $ toRatio a / toRatio b
                    MkRatio _ -> MkSomeNumber $ toRatio a / toRatio b
                    MkReal  _ -> MkSomeNumber $ toReal  a / toReal  b
            MkReal _ ->
                case b of
                    MkInt _ -> MkSomeNumber   $ toReal a / toReal b
                    MkRatio _ -> MkSomeNumber $ toReal a / toReal b
                    MkReal _ -> MkSomeNumber  $ toReal a / toReal b
    fromRational = MkSomeNumber . MkRatio
    recip (MkSomeNumber a) =
        case a of
            MkInt   _ -> MkSomeNumber $ recip $ toRatio a
            MkRatio _ -> MkSomeNumber $ recip $ toRatio a
            MkReal  _ -> MkSomeNumber $ recip $ toReal  a
instance Eq SomeNumber where
    (MkSomeNumber a) == (MkSomeNumber b) =
        case a of
            MkInt a' ->
                case b of
                    MkInt   b' -> a' == b'
                    MkRatio _  -> toRatio a == toRatio b
                    MkReal  _  -> toReal  a == toReal  b
            MkRatio a' ->
                case b of
                    MkInt   _  -> a  == toRatio b
                    MkRatio b' -> a' == b'
                    MkReal  _  -> toReal a == toReal b
            MkReal a' ->
                case b of
                    MkInt   _  -> toReal a == toReal b
                    MkRatio _  -> toReal a == toReal b
                    MkReal  b' -> a' == b'
instance Ord SomeNumber where
    compare (MkSomeNumber a) (MkSomeNumber b) =
        case a of
            MkInt a' ->
                case b of
                    MkInt   b' -> compare a' b'
                    MkRatio _  -> toRatio a `compare` toRatio b
                    MkReal  _  -> toReal  a `compare` toReal  b
            MkRatio a' ->
                case b of
                    MkInt   _  -> a  `compare` toRatio b
                    MkRatio b' -> a' `compare` b'
                    MkReal  _  -> toReal a `compare` toReal b
            MkReal a' ->
                case b of
                    MkInt   _  -> toReal a `compare` toReal b
                    MkRatio _  -> toReal a `compare` toReal b
                    MkReal  b' -> a' `compare` b'
instance Real SomeNumber where
    toRational (MkSomeNumber a) = toRational $ toRatio a


instance Num (Number 'IntType) where
    (+) = applyNumFunc2 (+)
    (-) = applyNumFunc2 (-)
    (*) = applyNumFunc2 (*)
    abs = applyNumFunc abs
    signum = applyNumFunc signum
    fromInteger = MkInt
instance Num (Number 'RatioType) where
    (+) = applyNumFunc2 (+)
    (-) = applyNumFunc2 (-)
    (*) = applyNumFunc2 (*)
    abs = applyNumFunc abs
    signum = applyNumFunc signum
    fromInteger = MkRatio . fromInteger
instance Num (Number 'RealType) where
    (+) = applyNumFunc2 (+)
    (-) = applyNumFunc2 (-)
    (*) = applyNumFunc2 (*)
    abs = applyNumFunc abs
    signum = applyNumFunc signum
    fromInteger = MkReal . fromInteger

instance Enum (Number 'IntType) where
    succ = (+ 1)
    pred = (`subtract` 1)
    fromEnum (MkInt a) = fromEnum a
    toEnum a = MkInt $ toEnum a

    enumFrom (MkInt a) = map MkInt $ enumFrom a
    {-# INLINE enumFrom #-}
    enumFromThen (MkInt a) (MkInt b) = map MkInt $ enumFromThen a b
    {-# INLINE enumFromThen #-}

    enumFromTo (MkInt a) (MkInt b) = map MkInt $ enumFromTo a b
    {-# INLINE enumFromTo #-}

    enumFromThenTo (MkInt a) (MkInt b) (MkInt c) = map MkInt $ enumFromThenTo a b c
    {-# INLINE enumFromThenTo #-}


instance Integral (Number 'IntType) where
    toInteger (MkInt a) = a
    quotRem (MkInt a) (MkInt b) = bimap MkInt MkInt $ quotRem a b
    divMod (MkInt a) (MkInt b) = bimap MkInt MkInt $ divMod a b

instance Fractional (Number 'RatioType) where
    (MkRatio a) / (MkRatio b) = MkRatio $ a / b
    fromRational = MkRatio
    recip (MkRatio a) = MkRatio $ recip a
instance Fractional (Number 'RealType) where
    (MkReal a) / (MkReal b) = MkReal $ a / b
    fromRational = MkReal . fromRational
    recip (MkReal a) = MkReal $ recip a

instance Eq (Number ty) where
    (MkInt a) == (MkInt b) = a == b
    (MkRatio a) == (MkRatio b) = a == b
    (MkReal a) == (MkReal b) = a == b

instance Ord (Number ty) where
    compare (MkInt a) (MkInt b) = compare a b
    compare (MkRatio a) (MkRatio b) = compare a b
    compare (MkReal a) (MkReal b) = compare a b

instance Real (Number 'IntType) where
    toRational a = case toRatio a of MkRatio a' -> a'
instance Real (Number 'RatioType) where
    toRational a = case toRatio a of MkRatio a' -> a'
instance Real (Number 'RealType) where
    toRational a = case toRatio a of MkRatio a' -> a'


applyNumFunc :: (forall a. (Real a) => a -> a) -> Number ty -> Number ty
{-# INLINE applyNumFunc #-}
applyNumFunc f = \case
    MkInt   a -> MkInt   $ f a
    MkRatio a -> MkRatio $ f a
    MkReal  a -> MkReal  $ f a

applyNumFunc' :: (forall a. (Real a) => a -> a) -> SomeNumber -> SomeNumber
{-# INLINE applyNumFunc' #-}
applyNumFunc' f = \(MkSomeNumber a) -> MkSomeNumber $ applyNumFunc f a

applyNumFunc2 :: (forall a. (Real a) => a -> a -> a) -> Number ty -> Number ty -> Number ty
{-# INLINE applyNumFunc2 #-}
applyNumFunc2 f = \a b ->
    case a of
        MkInt   a' -> case b of MkInt   b' -> MkInt   $ f a' b'
        MkRatio a' -> case b of MkRatio b' -> MkRatio $ f a' b'
        MkReal  a' -> case b of MkReal  b' -> MkReal  $ f a' b'

applyNumFunc2' :: (forall a. (Real a) => a -> a -> a) -> SomeNumber -> SomeNumber -> SomeNumber
{-# INLINE applyNumFunc2' #-}
applyNumFunc2' f = \(MkSomeNumber a) (MkSomeNumber b) ->
    case a of
        MkInt a' ->
            case b of
                MkInt   b' -> MkSomeNumber $ MkInt   $ f a' b'
                MkRatio b' -> MkSomeNumber $ MkRatio $ f (fromInteger a') b'
                MkReal  b' -> MkSomeNumber $ MkReal  $ f (fromInteger a') b'
        MkRatio a' ->
            case b of
                MkInt   b' -> MkSomeNumber $ MkRatio $ f a' (fromInteger b')
                MkRatio b' -> MkSomeNumber $ MkRatio $ f a' b'
                MkReal  b' -> MkSomeNumber $ MkReal  $ f (fromRational a') b'
        MkReal a' ->
            case b of
                MkInt   b' -> MkSomeNumber $ MkReal $ f a' (fromInteger  b')
                MkRatio b' -> MkSomeNumber $ MkReal $ f a' (fromRational b')
                MkReal  b' -> MkSomeNumber $ MkReal $ f a' b'
