{ mkDerivation, array, async, base, bytestring, colour, comonad
, deepseq, diagrams-core, diagrams-lib, diagrams-rasterific
, diagrams-svg, dual-tree, FontyFruity, fsnotify, ghc, ghc-paths
, ghci, gl, JuicyPixels, lens, linear, mtl, mwc-probability
, mwc-random, optparse-applicative, process, Rasterific, repa, sdl2
, shake, stdenv, vector
}:
mkDerivation {
  pname = "slika";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array async base bytestring colour comonad deepseq diagrams-core
    diagrams-lib diagrams-rasterific diagrams-svg dual-tree FontyFruity
    fsnotify ghc ghc-paths ghci gl JuicyPixels lens linear mtl
    mwc-probability mwc-random optparse-applicative process Rasterific
    repa sdl2 shake vector
  ];
  license = stdenv.lib.licenses.asl20;
}
