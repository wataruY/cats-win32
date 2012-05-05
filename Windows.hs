{-# LANGUAGE ForeignFunctionInterface #-}
module Windows where
import System.Win32 
import Graphics.Win32 hiding (c_MessageBox,messageBox)
import Graphics.Win32.GDI.Text
import Control.Exception (bracket)

messageBox :: HWND -> String -> String -> MBStyle -> IO MBStatus
messageBox wnd text caption style =
  withTString text $ \ c_text ->
  withTString caption $ \ c_caption ->
  failIfZero "MessageBox" $ c_MessageBox wnd c_text c_caption style

foreign import stdcall safe "windows.h MessageBoxW"
  c_MessageBox :: HWND -> LPCTSTR -> LPCTSTR -> MBStyle -> IO MBStatus

foreign import stdcall unsafe "PostQuitMessage" postQuitMessage :: Int -> IO ()


drawText :: HDC -> String -> RECT -> DTFormat -> IO Int
drawText hdc str rt f = 
  withTStringLen str $ \ (pstr,n) ->
  withRECT rt $ \ lprect ->
  fmap fromIntegral . failIfZero "drawText" $ c_DrawText hdc pstr (fromIntegral n) lprect (fromIntegral $ fromEnum f)


foreign import stdcall safe "windows.h DrawTextW"
  c_DrawText :: HDC -> LPCTSTR -> INT -> LPRECT -> UINT -> IO Int



withPaint :: HWND -> (HDC -> LPPAINTSTRUCT -> IO a) -> IO a
withPaint hwnd f = allocaPAINTSTRUCT $ \ ps -> withHDC hwnd ps (flip f ps)

withHDC :: HWND -> LPPAINTSTRUCT -> (HDC -> IO a) -> IO a
withHDC hwnd ps = bracket (failIfNull "withHDC : DC unavailable" $ beginPaint hwnd ps) (const $ endPaint hwnd ps)

defWindowClosure :: WindowClosure
defWindowClosure w = defWindowProc $ Just w
