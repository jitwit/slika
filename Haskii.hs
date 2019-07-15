{-# language ViewPatterns #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}

module Main where

import Options.Applicative
import Control.Comonad.Store
import Control.Lens
import qualified Data.ByteString.Char8 as B
import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Unboxed as V
-- based on running many of the different ones on images, this seems to be fastest
-- for this use case
import Data.Vector.Algorithms.Intro
import Linear hiding (trace)
import Data.Bool
import Debug.Trace

import Slika
import Slika.Juicy

type Haskii   = B.ByteString

-- | add up pixels and sort, assign palette to quantiles
haskii :: Int -> Int -> Settings -> Slika' (V2 Double) Int -> Haskii
haskii w h settings image =
  let t' = fromIntegral t; t = haskiiSize settings
      w' = fromIntegral w; h' = fromIntegral h
      sh = verticalStretch settings; xs = palette settings
      l = V.length pixels; p = length xs
      
      pixels = V.fromList
        [ peek (V2 x y) image | y <- [0..(sh*h'*t'/w')-1], x <- [0..(t'-1)] ]
      qs = pixels & V.modify sort
      quantiles = [ (qs V.! ((i*l-1)`div`p),x) | (i,x) <- zip [1..p] xs ]
      pix n = snd $ head $ dropWhile ((n>).fst) quantiles
   in B.unlines [ B.pack chunk | chunk <- chunks t (pix <$> V.toList pixels) ]

fuzz :: Settings -> Image Pixel8 -> Slika' (V2 Double) Int
fuzz settings img@(Image w h _) = melt (fromPixel8 img)
  =>> fromIntegral . extract
  =>> crop_fuzz (fromIntegral w/fromIntegral (haskiiSize settings))
    & scaleXY (V2 1 (verticalStretch settings))

crop_fuzz :: Double -> Slika' (V2 Double) Int -> Int
crop_fuzz b = sum . experiment
  (\(V2 x y) -> V2 <$> [b*x..b*(x+1)-1] <*> [b*y..b*(y+1)-1])

chunks :: Int -> [a] -> [[a]]
chunks n = go where
  go (splitAt n -> (ys,zs)) = ys : bool (go zs) [] (null zs)

run :: Settings -> IO ()
run settings@(Settings n _ xs b) =
  do Right dynimg <- decodeImage <$> B.getContents
     let img = extractLumaPlane $ convertRGBA8 dynimg
         w = imageWidth img; h = imageHeight img
         plt = if b then reverse xs else xs
     B.putStr $ haskii w h (settings{palette = plt}) (fuzz settings img)

main :: IO ()
main = execParser settings >>= run

data Settings = Settings
  { haskiiSize        :: Int
  , verticalStretch   :: Double
  , palette           :: String
  , invert            :: Bool }
  deriving (Show)

settings :: ParserInfo Settings
settings = info (helper <*> parser) parserinfo where
  parserinfo  = fullDesc <> progDesc "Convert images to ascii-art" 
  parser      = Settings <$> getSize <*> getVer <*> getPalette <*> getInvert
  getSize     = option auto $ mconcat sizeArgs
  getVer      = option auto $ mconcat verArgs  
  getPalette  = strOption $ mconcat paletteArgs
  getInvert   = switch $ mconcat invertArgs
  sizeArgs    = [short 'n',value 80,showDefault,help "width of output"]
  paletteArgs = [short 'p',value  " .,-~+%#",showDefault,help "chars to use"]
  invertArgs  = [short 'i',help "invert the palette" ]
  verArgs     = [short 'v',value 1,showDefault,help "scale vertically"]  
-- #%+~-,. 
-- first default: @?+*,. 
  

