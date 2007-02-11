# See README for Cabal-based building.  Other fancy stuff (like haddock) here.

user = conal
package = TV

configure_args=--disable-use-packages --haddock-args="\
  --read-interface=http://haskell.org/ghc/docs/latest/html/libraries/base,c:/ghc/ghc-6.6/doc/html/libraries/base/base.haddock \
  --read-interface=http://haskell.org/ghc/docs/latest/html/libraries/mtl,c:/ghc/ghc-6.6/doc/html/libraries/mtl/mtl.haddock \
  --read-interface=http://darcs.haskell.org/packages/phooey/doc/html,c:/Haskell/packages/phooey-0.1/doc/html/phooey.haddock \
  --read-interface=http://darcs.haskell.org/packages/DeepArrow/doc/html,c:/Haskell/packages/DeepArrow-0.0/doc/html/DeepArrow.haddock \
  $(comments_args)\
  "

# I don't think I need wxHaskell, since it's used only invisibly via phooey.
#   --read-interface=http://wxhaskell.sourceforge.net/doc,c:/Haskell/wxhaskell/out/doc/wxhaskell.haddock \

include ../my-cabal-make.inc
