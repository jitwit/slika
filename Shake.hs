{-# language ViewPatterns #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Bool

main :: IO ()
main =
  do let build_dir   = "_shake"
         args        = output <> par -- <> prof
         output      = ["-odir", build_dir,"-hidir", build_dir]
         par         = ["-rtsopts","-threaded","-O2"]
         prof        = ["-prof","-fprof-auto"]
         execs       = ["slika","haskii","psync"]
         
     shakeArgs shakeOptions{shakeFiles = build_dir} $ do

       want $ [ build_dir </> e | e <- execs ]

       forM_ execs $ \e -> build_dir </> e %> \out ->
         do let src = e </> "Main" -<.> "hs"
                modules = ["Control" </> "Slika" -<.> "hs"
                          ,"Control" </> "Slika" </> "Juicy" -<.> "hs"]
            need $ src : modules
            cmd ("ghc" :: String) (args) [src,"-o",out]

       "clean" ~>
         do putNormal $ unwords ["cleaning..."]
            removeFilesAfter "" $ "report.html" : "*.prof" : []
            removeFilesAfter build_dir ["//*"]
            removeFilesAfter "dist-newstyle" ["//*"]
            
