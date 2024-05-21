module Main (
  main,
) where

import Test.Cardano.Prelude

import qualified Test.Cardano.Prelude.GHC.Heap.NormalFormSpec
import qualified Test.Cardano.Prelude.GHC.Heap.SizeSpec
import qualified Test.Cardano.Prelude.GHC.Heap.TreeSpec

main :: IO ()
main =
  runTests
    [ Test.Cardano.Prelude.GHC.Heap.NormalFormSpec.tests
    , Test.Cardano.Prelude.GHC.Heap.SizeSpec.tests
    , Test.Cardano.Prelude.GHC.Heap.TreeSpec.tests
    ]
