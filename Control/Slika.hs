--{-# OPTIONS_GHC -Wall #-}
{-# language LambdaCase, MultiWayIf #-}

module Control.Slika
  ( todo
  , π  
  , Slika
  , Slika'
  , filterK
  , gaussian5
  , boxblur
  , sharpen5
  , blur0
  , melt
  , unmelt
  , scaleXY
  , rotate2
  , transform
  , rotateAboutBy
  , translate
  , reflect2
  , reflectAboutBy
  , shearX
  , shearY
  , polar
  , unpolar
  , slika
  ) where

import Control.Comonad
import Control.Comonad.Store
import Control.Lens hiding (transform)
import Linear
import Data.Colour

type Slika = Context
type Slika' v = Context' v

π :: Floating x => x
π = pi

todo :: todo
todo = error "todo"

-- | sample in a 2d box of size 2n x 2n around a point 
sample2 :: (Enum a, Num a) => a -> V2 a -> [V2 a]
sample2 n (V2 x y) = V2 <$> [x-n..x+n] <*> [y-n..y+n]
{-# inlinable sample2 #-}

sampleK5 :: (Enum a, Num a) => V2 a -> [V2 a]
sampleK5 z@(V2 x y) = [z,V2 (x+1) y, V2 (x-1) y, V2 x (y-1), V2 x (y+1)]

-- | take affine combination of colours with weights given by a kernel
kernel :: Fractional a => [a] -> [Colour a] -> Colour a
kernel ker clrs = affineCombo (zip ker clrs) black
{-# inlinable kernel #-}

-- | calculate convolution at focus
withKernel :: (Enum x, Num x, Fractional p) => [p] -> Slika' (V2 x) (Colour p) -> Colour p
withKernel k = kernel k . experiment (sample2 l) where
  r = floor $ sqrt $ fromIntegral $ length k; l = fromIntegral $ (r+1)`div`2
{-# inlinable withKernel #-}

-- | apply kernel to image
filterK
  :: (Enum x, Num x, Fractional p)
  => [p] -> Slika' (V2 x) (Colour p) -> Slika' (V2 x) (Colour p)
filterK ker image = withKernel ker <<= image
{-# inlinable filterK #-}

-- | some kernels
gaussian5',blur0',sharpen5',boxblur' :: Fractional p => [p]
gaussian5' = [1,4,6,4,1,4,16,24,16,4,6,24,36,24,6,4,16,24,16,4,1,4,6,4,1] <&> (/256)
blur0' = [1/16,2/16,1/16,2/16,4/16,2/16,1/16,2/16,1/16]
sharpen5' = [0,-1,0,-1,5,-1,0,-1,0] <&> (/5)
boxblur'  = [1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9]

blur0 :: (Fractional p, Num a, Enum a) => Slika' (V2 a) (Colour p) -> Colour p
blur0 = withKernel blur0'
{-# inlinable blur0 #-}

gaussian5 :: (Fractional p, Num a, Enum a) => Slika' (V2 a) (Colour p) -> Colour p
gaussian5 = withKernel gaussian5'
{-# inlinable gaussian5 #-}

sharpen5 :: (Fractional p, Num a, Enum a) => Slika' (V2 a) (Colour p) -> Colour p
sharpen5 = withKernel sharpen5'
{-# inlinable sharpen5 #-}

boxblur :: (Fractional p, Num a, Enum a) => Slika' (V2 a) (Colour p) -> Colour p
boxblur = withKernel boxblur'
{-# inlinable boxblur #-}

-- | Transform an image on discrete coordinates to one on continuous coordinates
melt :: (Functor v, RealFrac x) => Slika' (v Int) p -> Slika' (v x) p
melt (Context g v) = Context (g . fmap round) (fromIntegral <$> v)
{-# inlinable melt #-}

-- | Transform an image on continuous coordinates to one on discrete coordinates
unmelt :: (Functor v, RealFrac x) => Slika' (v x) p -> Slika' (v Int) p
unmelt (Context g v) = Context (g . fmap fromIntegral) (round <$> v)
{-# inlinable unmelt #-}

-- | precompose a function on the underlying vector space of an image
transform :: (v -> v) -> Slika v' v p -> Slika v' v p
transform t (Context g v) = (Context (g . t) v)
{-# inlinable transform #-}

-- | shifts the image by a vector
translate :: (Num x, Additive v) => v x -> Slika' (v x) p -> Slika' (v x) p
translate z = transform (^-^z)
{-# inlinable translate #-}

-- | rotates with theta=0 as the x axis, origin at top left corner for a parsed image
-- 0 -> +x
-- |
-- v
-- +y
rotate2 :: (Floating x) => x -> Slika' (V2 x) p -> Slika' (V2 x) p
rotate2 θ' = transform rote where
  rote (V2 x y) = V2 (x*cos θ - y*sin θ) (x*sin θ + y*cos θ); θ = negate θ'
{-# inlinable rotate2 #-}

reflect2 :: (Floating x) => x -> Slika' (V2 x) p -> Slika' (V2 x) p
reflect2 θ' = transform refl where
  refl (V2 x y) = V2 (x*cos (2*θ) + y*sin (2*θ)) (x*sin (2*θ) - y*cos (2*θ)); θ = θ'
{-# inlinable reflect2 #-}

rotateAboutBy :: (Floating x) => V2 x -> x -> Slika' (V2 x) p -> Slika' (V2 x) p
rotateAboutBy z θ = translate z . rotate2 θ . translate (negate z)
{-# inlinable rotateAboutBy #-}

reflectAboutBy :: (RealFloat x, Ord x)
               => V2 x -> V2 x -> Slika' (V2 x) p -> Slika' (V2 x) p
reflectAboutBy z (V2 xl yl) = translate z . reflect2 θ . translate (negate z) where
  θ = atan2 yl xl
{-# inlinable reflectAboutBy #-}

scaleXY :: Floating x => V2 x -> Slika' (V2 x) p -> Slika' (V2 x) p
scaleXY z = transform (\s -> liftI2 (/) s z)
{-# inlinable scaleXY #-}

shearX :: Floating x => x -> Slika' (V2 x) p -> Slika' (V2 x) p
shearX θ = transform (\(V2 x y) -> V2 (x + y*tan (-θ)) y)
{-# inlinable shearX #-}

shearY :: Floating x => x -> Slika' (V2 x) p -> Slika' (V2 x) p
shearY θ = transform (\(V2 x y) -> V2 x (y + x*tan (-θ)))
{-# inlinable shearY #-}

polar :: RealFloat x => Slika' (V2 x) p -> Slika' (V2 x) p
polar = transform (\z@(V2 x y) -> V2 (norm z) (atan2 y x))
{-# inlinable polar #-}

unpolar :: RealFloat x => Slika' (V2 x) p -> Slika' (V2 x) p
unpolar = transform (\z@(V2 ρ θ) -> V2 (ρ*cos θ) (ρ*sin θ))
{-# inlinable unpolar #-}

slika :: (v -> p) -> v -> Slika' v p
slika = Context

--latticeAbout :: RealFrac x => V2 x -> [V2 x]
--latticeAbout z@(V2 x y) = [V2 x' y', V2 (x'+1) y', V2 x' (y'+1), V2 (x'+1) (y'+1)]
--  where V2 x' y' = fromIntegral . floor <$> z

