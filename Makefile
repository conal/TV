# See README for Cabal-based building.  Other fancy stuff (like haddock) here.

user = conal
package = TV

haddock_args=\
  --no-use-packages \
  --haddock-arg=--read-interface=http://haskell.org/ghc/docs/latest/html/libraries/base,c:/ghc/ghc-6.6/doc/html/libraries/base/base.haddock \
  --haddock-arg=--read-interface=http://haskell.org/ghc/docs/latest/html/libraries/mtl,c:/ghc/ghc-6.6/doc/html/libraries/mtl/mtl.haddock \
  --haddock-arg=--read-interface=http://wxhaskell.sourceforge.net/doc,c:/Haskell/wxhaskell/out/doc/wxhaskell.haddock \
  --haddock-arg=--read-interface=http://darcs.haskell.org/packages/phooey/doc/html,c:/Haskell/packages/phooey-0.1/doc/html/phooey.haddock \
  --haddock-arg=--read-interface=http://darcs.haskell.org/packages/DeepArrow/doc/html,c:/Haskell/packages/DeepArrow-0.0/doc/html/DeepArrow.haddock \
  # enough, already!

#   --haddock-arg=--read-interface=http://darcs.haskell.org/packages/monadLib/doc,c:/Haskell/packages/monadLib-3.1.0/doc/html/monadLib.haddock \


include ../my-cabal-make.inc
