{-# LANGUAGE TypeOperators, ViewPatterns, GeneralizedNewtypeDeriving #-}
module Util where

import Data.Lens.Lazy
import Graphics.Win32
import Data.Bits
import Data.Function
import Control.Arrow

type RECTLens = Lens RECT Int

leftL,topL,rightL,bottomL :: RECTLens
leftL = lens (fromIntegral . (\ (x,_,_,_) -> x)) $ \ (fromIntegral -> x) (_,y,z,w) -> (x,y,z,w)
topL = lens (fromIntegral . (\ (_,y,_,_) -> y)) $ \ (fromIntegral -> y) (x,_,z,w) -> (x,y,z,w)
rightL = lens (fromIntegral . (\ (_,_,z,_) -> z)) $ \ (fromIntegral -> z) (x,y,_,w) -> (x,y,z,w)
bottomL = lens (fromIntegral . (\ (_,_,_,w) -> w)) $ \ (fromIntegral -> w) (x,y,z,_) -> (x,y,z,w)

newtype EnumBits a = EnumBits { unEnumBits :: a }
    deriving (Enum)

unOp :: (a -> a) -> (a -> b) -> (b -> a) -> b -> b
unOp h f g = f . h . g

binOp :: (a -> a -> a) -> (a -> b) -> (b -> a) -> b -> b -> b
binOp op f g = curry $ g *** g >>> uncurry op >>> f

unOp' :: Enum b => (Int -> Int) -> EnumBits b -> EnumBits b
unOp' f = EnumBits . unOp f toEnum fromEnum . unEnumBits

binOp' :: Enum b => (Int -> Int -> Int) -> EnumBits b -> EnumBits b -> EnumBits b
binOp' f = curry $ EnumBits . uncurry (binOp f toEnum fromEnum) . uncurry ((,) `on` unEnumBits)

instance Enum a => Eq (EnumBits a) where
    (==) = (==) `on` (fromEnum . unEnumBits)

instance (Enum a) => Num (EnumBits a) where
    (+) = binOp' (+)
    (*) = binOp' (*)
    abs = unOp' abs
    signum = unOp' signum
    fromInteger = EnumBits . toEnum . fromIntegral

instance Enum a => Bits (EnumBits a) where
    (.&.) = binOp' (.&.)
    (.|.) = binOp' (.|.)
    xor = binOp' xor
    complement = unOp' complement
    bitSize = bitSize . fromEnum . unEnumBits
    isSigned = isSigned . fromEnum . unEnumBits
