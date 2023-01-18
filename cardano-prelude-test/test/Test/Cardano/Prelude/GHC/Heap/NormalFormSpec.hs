{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Prelude.GHC.Heap.NormalFormSpec (
  tests,
) where

import Cardano.Prelude

import GHC.Exts.Heap
import Hedgehog (
  Property,
  annotateShow,
  assert,
  checkParallel,
  discover,
  forAll,
  property,
  withTests,
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Property: Fully evaluated '[Int]'s are in normal form.
prop_isNormalForm_correct :: Property
prop_isNormalForm_correct = withTests 1000 $ property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  isNF <- liftIO $ isNormalForm $!! xs
  closureData <- liftIO $ getClosureData $!! xs
  annotateShow closureData
  assert isNF

-- | Property: Lazy '[Int]'s of length > 0 are not in normal form.
prop_isNormalForm_incorrect :: Property
prop_isNormalForm_incorrect = withTests 1000 $ property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  isNF <- liftIO $ isNormalForm xs
  closureData <- liftIO $ getClosureData xs
  annotateShow closureData
  assert $ not isNF

tests :: IO Bool
tests = and <$> sequence [checkParallel $$(discover)]
