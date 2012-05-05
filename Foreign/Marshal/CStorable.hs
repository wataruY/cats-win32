{-# LANGUAGE FlexibleInstances, UndecidableInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Foreign.Marshal.CStorable (withCS,fromStorable,toStorable,withPtr) where

import Foreign.CStorable
import Foreign


newtype ToStorable a = ToStorable { fromStorable :: a }
toStorable :: a -> ToStorable a
toStorable = ToStorable
deriving instance CStorable a => CStorable (ToStorable a)

instance CStorable a => Storable (ToStorable a) where
    peek ptr = cPeek ptr
    poke = cPoke
    alignment = cAlignment
    sizeOf = cSizeOf

withCS :: CStorable a => a -> (Ptr a -> IO b) -> IO b
withCS val f = with (ToStorable val) (f . castPtr)

withPtr :: Storable a => a -> (Ptr a -> IO b) -> IO b
withPtr = with
