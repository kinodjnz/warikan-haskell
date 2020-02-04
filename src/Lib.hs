{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lib
    ( someFunc
    ) where

import Prelude hiding (sum)

class Zero a where
    zero :: a

class Zero s => Adder m s | m -> s where
    add :: s -> m -> s
    sum :: [m] -> s

    sum = foldl add zero

newtype PaymentWeight = PaymentWeight Int deriving Show

newtype PaymentWeightSum = PaymentWeightSum Int deriving Show

instance Zero PaymentWeightSum where
    zero = PaymentWeightSum 0

instance Adder PaymentWeight PaymentWeightSum where
    add (PaymentWeightSum s) (PaymentWeight m) = PaymentWeightSum (s + m)

class AdditiveRatio ratio source morphed | ratio source -> morphed where
    mapRatio :: ratio -> source -> morphed

class (Adder source sourceSum,
       Adder morphed morphedSum,
       AdditiveRatio ratio source morphed) =>
        ToRatio source sourceSum morphed morphedSum ratio | sourceSum morphedSum -> ratio source morphed where
    toRatio :: sourceSum -> morphedSum -> ratio

newtype PaymentAmount = PaymentAmount Int deriving Show

newtype ChargeAmount = ChargeAmount Int deriving Show

instance Zero ChargeAmount where
    zero = ChargeAmount 0

instance Adder PaymentAmount ChargeAmount where
    add (ChargeAmount c) (PaymentAmount p) = ChargeAmount (c + p)

newtype PaymentRatio = PaymentRatio Double deriving Show

instance AdditiveRatio PaymentRatio PaymentWeight PaymentAmount where
    mapRatio (PaymentRatio r) (PaymentWeight w) = PaymentAmount (truncate r * fromIntegral w)

instance ToRatio PaymentWeight PaymentWeightSum PaymentAmount ChargeAmount PaymentRatio where
    toRatio (PaymentWeightSum s) (ChargeAmount c) = PaymentRatio (fromIntegral c / fromIntegral s)

class WarikanMethod a where
    doWarikan :: a -> [PaymentWeight] -> ChargeAmount -> [PaymentAmount]

data LinearWarikan = LinearWarikan

instance WarikanMethod LinearWarikan where
    doWarikan _ weights charge = payments
        where
            s = sum weights
            ratio = toRatio s charge
            payments = map (mapRatio ratio) weights

someFunc :: IO ()
someFunc = print
    $ doWarikan
        LinearWarikan
        [PaymentWeight 3, PaymentWeight 5, PaymentWeight 1]
        $ ChargeAmount 100
