module Time where
import Data.Time
import Data.Text hiding (map) 
import Data.Text.ICU.Convert
import qualified Data.ByteString.Char8 as B
import Data.String
import Control.Monad
import System.Locale

toU8 :: Text -> IO B.ByteString
toU8 t = do
  c <- open "utf8" Nothing
  return $ fromUnicode c t

fromCp932 :: String -> IO Text
fromCp932 s = do
  c<- open "cp932" Nothing
  return $ toUnicode c $ fromString s

cp932 :: String -> IO String
cp932 = fromCp932 >=> toU8 >=> return . B.unpack



formattedTime :: IO String
formattedTime = getZonedTime >>= return . formatTime defaultTimeLocale　"%Y年%_m月%_d日 %_H時%M分%S秒"