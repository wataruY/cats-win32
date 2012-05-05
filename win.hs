module Main where
import Prelude hiding (id,(.))
import Control.Category
import Graphics.Win32 hiding (c_MessageBox, messageBox)
import System.Win32

import Data.Bits
import Control.Monad
import Data.Maybe
import Control.Monad.Fix
import Control.Monad.Trans.Resource
import Data.Global
import Data.IORef
import Graphics.Win32.GDI.Text
import Windows
import Text.Printf
import Data.Lens.Lazy
import Util


hInstance :: IORef HINSTANCE
hInstance = declareIORef "module instance" undefined

main :: IO ()
main = do
  inst <- withUpdateIORef hInstance $ getModuleHandle Nothing
  runResourceT $  do
        
       let classInfo = (cS_HREDRAW .|. cS_VREDRAW, inst
                       , Nothing, Nothing, Nothing, Nothing
                       , mainWindow)
       allocate (failIf isNothing "registerClass" $ registerClass classInfo) $
                const $ unregisterClass mainWindow inst
       unsafeLiftIO main' 

mainWindow :: ClassName
mainWindow = mkClassName "myWindowclass"

main' :: IO ()
main' = do
  inst <- readIORef hInstance
  menu <- setupMenu

  let (title, n) = ("猫でもわかるHaskell", numToMaybe cW_USEDEFAULT)
  hwnd <- failIfNull "during create main window" $ createWindowEx
          wS_EX_CLIENTEDGE mainWindow title wS_OVERLAPPEDWINDOW
          n n n n
         Nothing (Just menu)
         inst wndProc

  showWindow hwnd sW_SHOWNORMAL
  updateWindow hwnd

  messageLoop

iDM_END,iDM_TEST,iDM_ABOUT :: MenuID
(iDM_END,iDM_TEST,iDM_ABOUT) = (100,200,300)

setupMenu :: IO HMENU
setupMenu = do
  menu <- createMenu
  fileMenu <- do fileMenu <- createMenu
                 appendMenu fileMenu 0 iDM_END "終了"
                 appendMenu fileMenu 0 iDM_TEST "テスト"
                 return fileMenu
  helpMenu <- do helpMenu <- createMenu
                 appendMenu helpMenu 0 iDM_ABOUT "About"
                 return helpMenu
  appendMenu menu 0x10 (handleToWord fileMenu) "ファイル"
  appendMenu menu 0x10 (handleToWord helpMenu) "ヘルプ"
  return menu



wndProc :: WindowClosure
wndProc hwnd msg wp lp
    | msg == wM_PAINT = showMyText hwnd >> return 0
    | msg == wM_COMMAND = handleMenu $ fromIntegral $ lOWORD wp
    | msg == wM_CLOSE = do
  res <- messageBox hwnd "終了しますか？" "終了確認" (mB_OKCANCEL .|. mB_ICONQUESTION)
  when (res == iDOK) $ destroyWindow hwnd
  return 0
    | msg == wM_DESTROY = postQuitMessage 0 >> return 0
    | otherwise = defProc
  where defProc = defWindowProc (Just hwnd) msg wp lp
        handleMenu :: MenuID -> IO LRESULT
        handleMenu x | x == iDM_END = sendMessage hwnd wM_CLOSE 0 0 >> return 0
                     | x == iDM_TEST = messageBox hwnd "テストが押された" "test" mB_OK >> return 0
                     | x == iDM_ABOUT = messageBox hwnd "Aboutが押された" "About" mB_OK >> return 0
                     | otherwise = return 0

showMyText :: HWND -> IO ()
showMyText hwnd = do
  rect <- getClientRect hwnd
  brush <- getStockBrush wHITE_BRUSH
  withPaint hwnd $ \ hdc _ -> do
      fillRect hdc rect brush

      setTextColor hdc $ rgb 255 0 0
      textOut hdc 10 10 $ printf org (rect ^. leftL) ( rect ^. topL) (rect ^. rightL) (rect ^. bottomL)

      setTextColor hdc $ rgb 0 0 255
      let rect' = (leftL^%=(+40)) . (topL^%= (+40)) . (rightL^%= subtract 40) . (bottomL ^%= subtract 40) $ rect
      drawText hdc str rect' DT_WORDBREAK

      return ()
      where str,org ::String
            str = "猫でもわかるHaskell\n山崎渉 制作\n" ++
                  "わかりやすくてためになる!"
            org = "left=%d top=%d right=%d bottom=%d"

messageLoop :: IO ()
messageLoop = allocaMessage $ fix $ \ doLoop msg -> 
                whenM (getMessage msg Nothing) $
                      translateMessage msg >> dispatchMessage msg >> doLoop msg

whenM :: (Monad m, Functor m) => m Bool -> m () -> m ()
whenM m f = m >>= \ p -> when p f

withUpdateIORef :: IORef a -> IO a -> IO a
withUpdateIORef ref m = m >>= \ a -> writeIORef ref a >> m
