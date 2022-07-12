module Test.Cardano.Tasty.Hedgehog
  ( testProperty
  ) where

import Data.String (IsString(..))
import Test.Tasty (TestName, TestTree)
import Hedgehog (Property)

import qualified Test.Tasty.Hedgehog as TH

testProperty :: TestName -> Property -> TestTree
testProperty testName = TH.testPropertyNamed testName (fromString testName)
