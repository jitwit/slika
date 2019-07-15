{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

import Options.Applicative
import Codec.Picture
import Control.Comonad.Store
import qualified Data.ByteString.Char8 as B
import Linear
import Data.Colour.SRGB.Linear
import Data.Colour.Names

import Control.Slika
import Control.Slika.Juicy

egTransform center = extend boxblur
  
--  reflectAboutBy (V2 240 175) (V2 0 1)

egFop = unmelt $
        scaleXY (V2 (1/100) (1/40)) $
        rotate2 (pi/6) $
        slika g zero where
  g :: V2 Double -> Colour Double
  g (V2 x y)
    | even (floor x+floor y) = blue
    | otherwise = snow

fop =
  do let out = "output/slika"
     writePng out (toRGB8 400 400 egFop)

slika' :: Settings -> IO ()
slika' settings = do
  bs <- B.getContents
  let Right img = convertRGB8 <$> decodeImage bs
      w = imageWidth img; h = imageHeight img
      w' = fromIntegral w; h' = fromIntegral h      
      out = "output/" <> destination settings
      trans = unmelt . egTransform (V2 30 30) . melt -- V2 w' h' ^/ 2
  writePng out (toRGB8 w h $ trans $ fromRGB8 img)
--  fop
  
main :: IO ()
main = execParser getSettings >>= slika'

data Settings = Settings
  { customKernel :: [Double]
  , destination  :: FilePath
  }

getSettings :: ParserInfo Settings
getSettings = info (helper <*> parser) parserinfo where
  parserinfo  = fullDesc <> progDesc "Transform images"
  parser = Settings <$> getKernel <*> getDestination
  getKernel = option auto $ mconcat kernelArgs
  getDestination = strOption $ mconcat destinationArgs
  kernelArgs = [long "kernel",short 'k',value [],help "list of doubles"]
  destinationArgs = [long "out",short 'o',value "slika", help "out file"]
