{-# LANGUAGE TypeOperators, ViewPatterns #-}
module Util where

import Data.Lens.Lazy
import Graphics.Win32

type RECTLens = Lens RECT Int

leftL,topL,rightL,bottomL :: RECTLens
leftL = lens (fromIntegral . (\ (x,_,_,_) -> x)) $ \ (fromIntegral -> x) (_,y,z,w) -> (x,y,z,w)
topL = lens (fromIntegral . (\ (_,y,_,_) -> y)) $ \ (fromIntegral -> y) (x,_,z,w) -> (x,y,z,w)
rightL = lens (fromIntegral . (\ (_,_,z,_) -> z)) $ \ (fromIntegral -> z) (x,y,_,w) -> (x,y,z,w)
bottomL = lens (fromIntegral . (\ (_,_,_,w) -> w)) $ \ (fromIntegral -> w) (x,y,z,_) -> (x,y,z,w)