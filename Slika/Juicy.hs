{-# OPTIONS_GHC -Wall #-}
{-# language LambdaCase, MultiWayIf #-}

module Slika.Juicy
  ( fromPixel8
  , fromRGB8'
  , toRGB8'
  , fromRGB8
  , toRGB8
  ) where

import Control.Lens.Internal.Context
import Slika
import Codec.Picture
import Data.Colour
import Data.Colour.SRGB.Linear
import Linear
import Data.Ix

fromPixel8 :: Image Pixel8 -> Slika (V2 Int) (V2 Int) Pixel8
fromPixel8 img@(Image w h _) = Context g zero where
    g (V2 x y)
      | inRange (0,w-1) x && inRange (0,h-1) y = pixelAt img x y
      | otherwise = 0
{-# inline fromPixel8 #-}

fromRGB8' :: Image PixelRGB8 -> Slika (V2 Int) (V2 Int) PixelRGB8
fromRGB8' img@(Image w h _) = Context g zero where
    g (V2 x y)
      | inRange (0,w-1) x && inRange (0,h-1) y = pixelAt img x y
      | otherwise = PixelRGB8 0 0 0
{-# inline fromRGB8' #-}

toRGB8' :: Int -> Int ->  Slika (V2 Int) (V2 Int) PixelRGB8 -> Image PixelRGB8
toRGB8' w h img = generateImage g w h where
  g i j = ipeek (V2 i j) img
{-# inline toRGB8' #-}

fromRGB8 :: Fractional c => Image PixelRGB8 -> Slika (V2 Int) (V2 Int) (Colour c)
fromRGB8 = toColourPixels . fromRGB8'
{-# inline fromRGB8 #-}

toRGB8 :: RealFrac c => Int -> Int ->  Slika (V2 Int) (V2 Int) (Colour c) -> Image PixelRGB8
toRGB8 w h = toRGB8' w h . fromColourPixels
{-# inline toRGB8 #-}

toColourPixels :: Fractional c => Slika u v PixelRGB8 -> Slika u v (Colour c)
toColourPixels = ifmap convPix
{-# inline toColourPixels #-}

fromColourPixels  :: RealFrac c => Slika u v (Colour c) -> Slika u v PixelRGB8
fromColourPixels = ifmap (convPix' . toRGB)
{-# inline fromColourPixels #-}

convPix :: Fractional a => PixelRGB8 -> Colour a
convPix = \case
  PixelRGB8 r g b -> rgb (r'/255) (g'/255) (b'/255) where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b
{-# inline convPix #-}

convPix' :: RealFrac a => RGB a -> PixelRGB8
convPix' (RGB r g b) = PixelRGB8 r' g' b' where
  r' = floor $ 255 * r
  g' = floor $ 255 * g
  b' = floor $ 255 * b
{-# inline convPix' #-}
