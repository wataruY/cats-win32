{-# LANGUAGE ForeignFunctionInterface, DeriveGeneric #-}
module Graphics.Win32.GDI.Text (drawTextEx,DTFormat (..), DrawTextParams) where

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
#include <windows.h>


{#enum define DTFormat {
DT_TOP as DT_TOP,
DT_LEFT as DT_LEFT,
DT_CENTER as DT_CENTER,
DT_RIGHT as DT_RIGHT,
DT_VCENTER as DT_VCENTER,
DT_BOTTOM as DT_BOTTOM,
DT_WORDBREAK as DT_WORDBREAK,
DT_SINGLELINE as DT_SINGLELINE,
DT_EXPANDTABS as DT_EXPANDTABS,
DT_TABSTOP as DT_TABSTOP,
DT_NOCLIP as DT_NOCLIP,
DT_EXTERNALLEADING as DT_EXTERNALLEADING,
DT_CALCRECT as DT_CALCRECT,
DT_NOPREFIX as DT_NOPREFIX,
DT_INTERNAL as DT_INTERNAL,
DT_EDITCONTROL as DT_EDITCONTROL,
DT_PATH_ELLIPSIS as DT_PATH_ELLIPSIS,
DT_END_ELLIPSIS as DT_END_ELLIPSIS,
DT_MODIFYSTRING as DT_MODIFYSTRING,
DT_RTLREADING as DT_RTLREADING,
DT_WORD_ELLIPSIS as DT_WORD_ELLIPSIS
       }
   deriving (Eq, Generic)#}
instance CStorable DTFormat



data DrawTextParams = DrawTextParams { dtTabLength, dtLeftMargin, dtRightMargin :: Int, dtLengthDrawn :: UINT } deriving (Show,Eq,Read)

instance Storable DrawTextParams where
    sizeOf _ =  {#sizeof DRAWTEXTPARAMS#}
    alignment = sizeOf
    peek p = do
      DrawTextParams <$> (fromIntegral <$> {#get DRAWTEXTPARAMS->iTabLength #} p)
                     <*> (fromIntegral <$> {#get DRAWTEXTPARAMS->iLeftMargin#} p)
		     <*> (fromIntegral <$> {#get DRAWTEXTPARAMS->iRightMargin#} p )
		     <*> (fromIntegral <$> {#get DRAWTEXTPARAMS->uiLengthDrawn#} p)
    poke p (DrawTextParams tl lm rm ld) = do
      {#set DRAWTEXTPARAMS->iTabLength #} p $ fromIntegral tl
      {#set DRAWTEXTPARAMS->iLeftMargin #} p $ fromIntegral lm
      {#set DRAWTEXTPARAMS->iRightMargin #} p $ fromIntegral rm
      {#set DRAWTEXTPARAMS->uiLengthDrawn #} p $ fromIntegral ld


drawTextEx :: HDC -> String -> RECT -> DTFormat -> DrawTextParams -> IO Int
drawTextEx hdc s rect format params =
    withTStringLen s $ \ (txt,n) ->
    withRECT rect $ \ rc ->
    let dwFormat = fromIntegral (fromEnum format) in
    with params $ \ pparam ->
    fmap fromIntegral $ {#call DrawTextExW as ^ #} hdc (castPtr txt) (fromIntegral n) (castPtr rc) dwFormat (castPtr pparam)