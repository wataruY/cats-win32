-- GENERATED by C->Haskell Compiler, version 0.16.3 Crystal Seed, 24 Jan 2009 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "d:/haskell/Graphics/Win32/GDI/Text.chs" #-}{-# LANGUAGE ForeignFunctionInterface, DeriveGeneric #-}
module Graphics.Win32.GDI.Text (drawTextEx,DTFormat (..), DrawTextParams (..)) where

import System.IO
import Graphics.Win32 hiding (c_MessageBox, messageBox)
import System.Win32
import Foreign.Marshal
import Foreign.Ptr
import Foreign.C
import GHC.Generics
import Foreign.CStorable
import Foreign.Marshal.CStorable
import Foreign.Storable
import Control.Applicative
import Foreign.ForeignPtr


data DTFormat = DT_TOP
              | DT_LEFT
              | DT_CENTER
              | DT_RIGHT
              | DT_VCENTER
              | DT_BOTTOM
              | DT_WORDBREAK
              | DT_SINGLELINE
              | DT_EXPANDTABS
              | DT_TABSTOP
              | DT_NOCLIP
              | DT_EXTERNALLEADING
              | DT_CALCRECT
              | DT_NOPREFIX
              | DT_INTERNAL
              | DT_EDITCONTROL
              | DT_PATH_ELLIPSIS
              | DT_END_ELLIPSIS
              | DT_MODIFYSTRING
              | DT_RTLREADING
              | DT_WORD_ELLIPSIS
              deriving (Eq,Generic)
instance Enum DTFormat where
  fromEnum DT_TOP = 0
  fromEnum DT_LEFT = 0
  fromEnum DT_CENTER = 1
  fromEnum DT_RIGHT = 2
  fromEnum DT_VCENTER = 4
  fromEnum DT_BOTTOM = 8
  fromEnum DT_WORDBREAK = 16
  fromEnum DT_SINGLELINE = 32
  fromEnum DT_EXPANDTABS = 64
  fromEnum DT_TABSTOP = 128
  fromEnum DT_NOCLIP = 256
  fromEnum DT_EXTERNALLEADING = 512
  fromEnum DT_CALCRECT = 1024
  fromEnum DT_NOPREFIX = 2048
  fromEnum DT_INTERNAL = 4096
  fromEnum DT_EDITCONTROL = 8192
  fromEnum DT_PATH_ELLIPSIS = 16384
  fromEnum DT_END_ELLIPSIS = 32768
  fromEnum DT_MODIFYSTRING = 65536
  fromEnum DT_RTLREADING = 131072
  fromEnum DT_WORD_ELLIPSIS = 262144

  toEnum 0 = DT_TOP
  toEnum 0 = DT_LEFT
  toEnum 1 = DT_CENTER
  toEnum 2 = DT_RIGHT
  toEnum 4 = DT_VCENTER
  toEnum 8 = DT_BOTTOM
  toEnum 16 = DT_WORDBREAK
  toEnum 32 = DT_SINGLELINE
  toEnum 64 = DT_EXPANDTABS
  toEnum 128 = DT_TABSTOP
  toEnum 256 = DT_NOCLIP
  toEnum 512 = DT_EXTERNALLEADING
  toEnum 1024 = DT_CALCRECT
  toEnum 2048 = DT_NOPREFIX
  toEnum 4096 = DT_INTERNAL
  toEnum 8192 = DT_EDITCONTROL
  toEnum 16384 = DT_PATH_ELLIPSIS
  toEnum 32768 = DT_END_ELLIPSIS
  toEnum 65536 = DT_MODIFYSTRING
  toEnum 131072 = DT_RTLREADING
  toEnum 262144 = DT_WORD_ELLIPSIS
  toEnum unmatched = error ("DTFormat.toEnum: Cannot match " ++ show unmatched)

{-# LINE 42 "d:/haskell/Graphics/Win32/GDI/Text.chs" #-}
instance CStorable DTFormat



data DrawTextParams = DrawTextParams { dtTabLength, dtLeftMargin, dtRightMargin :: Int, dtLengthDrawn :: UINT } deriving (Show,Eq,Read)

instance Storable DrawTextParams where
    sizeOf _ =  20
{-# LINE 50 "d:/haskell/Graphics/Win32/GDI/Text.chs" #-}
    alignment = sizeOf
    peek p = do
      DrawTextParams <$> (fromIntegral <$> (\ptr -> do {peekByteOff ptr 4 ::IO CInt}) p)
                     <*> (fromIntegral <$> (\ptr -> do {peekByteOff ptr 8 ::IO CInt}) p)
		     <*> (fromIntegral <$> (\ptr -> do {peekByteOff ptr 12 ::IO CInt}) p )
		     <*> (fromIntegral <$> (\ptr -> do {peekByteOff ptr 16 ::IO CUInt}) p)
    poke p (DrawTextParams tl lm rm ld) = do
      (\ptr val -> do {pokeByteOff ptr 4 (val::CInt)}) p $ fromIntegral tl
      (\ptr val -> do {pokeByteOff ptr 8 (val::CInt)}) p $ fromIntegral lm
      (\ptr val -> do {pokeByteOff ptr 12 (val::CInt)}) p $ fromIntegral rm
      (\ptr val -> do {pokeByteOff ptr 16 (val::CUInt)}) p $ fromIntegral ld


drawTextEx :: HDC -> String -> RECT -> DTFormat -> DrawTextParams -> IO Int
drawTextEx hdc s rect format params =
    withTStringLen s $ \ (txt,n) ->
    withRECT rect $ \ rc ->
    let dwFormat = fromIntegral (fromEnum format) in
    with params $ \ pparam ->
    fmap fromIntegral $ drawTextExW hdc (castPtr txt) (fromIntegral n) (castPtr rc) dwFormat (castPtr pparam)
foreign import stdcall safe "d:/haskell/Graphics/Win32/GDI/Text.chs.h DrawTextExW"
  drawTextExW :: ((Ptr ()) -> ((Ptr CUShort) -> (CInt -> ((Ptr ()) -> (CUInt -> ((Ptr ()) -> (IO CInt)))))))