module Main where

import Graphics.Win32 hiding (c_MessageBox, messageBox)
import System.Win32
import Control.Concurrent
import Data.Bits
import Control.Monad
import Data.Maybe
import Control.Monad.Fix
import Control.Monad.Trans.Resource
import Data.Global
import Data.IORef
import Windows

hInstance :: IORef HINSTANCE
hInstance = declareIORef "module instance" undefined

main :: IO ()
main = do
  inst <- withUpdateIORef hInstance $ getModuleHandle Nothing
  runResourceT $ do
        
       let classInfo = (cS_HREDRAW .|. cS_VREDRAW, inst
                       , Nothing, Nothing, Nothing, Nothing
                       , mainWindow)
       allocate (failIf isNothing "registerClass" $ registerClass classInfo) $
                const $ unregisterClass mainWindow inst
       unsafeLiftIO main'

mainWindow :: ClassName
mainWindow = mkClassName "timer"

main' :: IO ()
main' = do
  inst <- readIORef hInstance
  (menu, _) <- setupMenu

  let (title, n) = ("タイマー", numToMaybe cW_USEDEFAULT)
  hwnd <- failIfNull "during create main window" $ createWindowEx
          wS_EX_OVERLAPPEDWINDOW mainWindow title wS_OVERLAPPEDWINDOW
          n n n n
         Nothing (Just menu)
         inst wndProc

  showWindow hwnd sW_SHOWNORMAL
  oneShot hwnd
  updateWindow hwnd

  messageLoop

iDM_END :: MenuID
iDM_END = 100

setupMenu :: IO (HMENU, IO ())
setupMenu = do
  menu <- createMenu
  appendMenu menu 0 iDM_END "終了"
  return (menu, destroyMenu menu)

wndProc :: WindowClosure
wndProc hwnd msg wp lp
    | msg == wM_PAINT = eraceClient hwnd >> return 0
    | msg == wM_COMMAND = handleMenu $ fromIntegral $ lOWORD wp
    | msg == wM_TIMER = do
  res <- messageBox hwnd "タイマーを再発行しますか？" "タイマー終了の確認" mB_YESNO
  when (res  == iDYES) $ oneShot hwnd
  return 0
    | msg == wM_DESTROY = postQuitMessage 0 >> return 0
    | otherwise = defProc
  where defProc = defWindowProc (Just hwnd) msg wp lp
        handleMenu :: MenuID -> IO LRESULT
        handleMenu x | x == iDM_END = sendMessage hwnd wM_CLOSE 0 0 >> return 0
                     | otherwise = return 0

oneShot :: HWND -> IO ()
oneShot hwnd = do
  forkIO $ threadDelay (3000 * 1000) >> sendMessage hwnd wM_TIMER 0 0 >> return ()
  return ()

eraceClient :: HWND -> IO ()
eraceClient hwnd = do
  rect <- getClientRect hwnd
  brush <- getStockBrush wHITE_BRUSH
  withPaint hwnd $ \ hdc _ -> do
      fillRect hdc rect brush
      return ()

messageLoop :: IO ()
messageLoop = allocaMessage $ fix $ \ doLoop msg ->
                whenM (getMessage msg Nothing) $
                      translateMessage msg >> dispatchMessage msg >> doLoop msg

whenM :: (Monad m, Functor m) => m Bool -> m () -> m ()
whenM m f = m >>= \ p -> when p f

withUpdateIORef :: IORef a -> IO a -> IO a
withUpdateIORef ref m = m >>= \ a -> writeIORef ref a >> m