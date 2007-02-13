# See README for Cabal-based building.  Other fancy stuff (like haddock) here.

user = conal

configure_args=--disable-use-packages --haddock-args="\
  --read-interface=http://haskell.org/ghc/docs/latest/html/libraries/base,c:/ghc/ghc-6.6/doc/html/libraries/base/base.haddock \
  --read-interface=http://haskell.org/ghc/docs/latest/html/libraries/mtl,c:/ghc/ghc-6.6/doc/html/libraries/mtl/mtl.haddock \
  --read-interface=http://darcs.haskell.org/packages/DeepArrow/doc/html,c:/Haskell/packages/DeepArrow-0.0/doc/html/DeepArrow.haddock \
  $(source_args)\
  $(comments_args)\
  "
include ../my-cabal-make.inc
