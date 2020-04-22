{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Prelude.CanonicalExamples (tests) where

import Cardano.Prelude
import Cardano.Prelude.CanonicalExamples.Orphans ()

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Hedgehog hiding (Var)

data Sum1 = X | Y
    deriving (Generic, Eq, Show)
data Sum2 = A | B | C
    deriving (Generic, Eq, Show)
data Rec = End | Next Rec
    deriving (Generic, Eq, Show)
data Rec1 = C1 Rec2 | Rec1
    deriving (Generic, Eq, Show)
newtype Rec2 = C2 Rec1
    deriving (Generic, Eq, Show)
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Generic, Eq, Show)
data Rose a = Branch a (Forest a)
    deriving (Generic, Eq, Show)
data Forest a = NilF | ConsF (Rose a) (Forest a)
    deriving (Generic, Eq, Show)
data Fork a = Fork a a
    deriving (Generic, Eq, Show)
data Fork' a = Fork' a a
    deriving (Generic, Eq, Show)
newtype WithLoop = Wrap WithLoop
    deriving (Generic, Eq, Show)
data Perfect a = ZeroP a | SuccP (Perfect (Fork a))
    deriving (Generic, Eq, Show)
data Bush a = NilB | ConsB (a, Bush (Bush a))
    deriving (Generic, Eq, Show)
data Term a = Var a | App (Term a, Term a) | Abs (Term (Bind a))
    deriving (Generic, Eq, Show)
data Bind a = Zero | Succ a
    deriving (Generic, Eq, Show)

instance CanonicalExamples Sum1
instance CanonicalExamples Sum2
instance CanonicalExamples Rec
instance CanonicalExamples Rec1
instance CanonicalExamples Rec2
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Tree a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Forest a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Rose a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Fork a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Perfect a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Bush a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Term a)
instance (CanonicalExamples a, Typeable a) => CanonicalExamples (Bind a)

instance CanonicalExamples a => CanonicalExamples (Fork' a) where
    canonicalExamples = do
      as <- canonicalExamples
      bs <- canonicalExamples
      return $ [Fork' a b| a <- as, b <- bs]

verifyExamples :: (Eq a, Show a, CanonicalExamples a)
               => [a]-> Property
verifyExamples examples =
    verifyExamplesSized 2 examples Set.empty

verifyExamplesSized :: (Eq a, Show a, CanonicalExamples a)
               => Int -> [a] -> Warnings -> Property
verifyExamplesSized = verifyExamplesArgs . stdArgs

verifyExamplesArgs :: (Eq a, Show a, CanonicalExamples a)
                   => Args
                   -> [a]
                   -> Warnings
                   -> Property
verifyExamplesArgs args examples warnings = withTests 1 $ property $
        getCanonicalExamplesWithWarnings args === (examples, warnings)

prop_Sum1    = verifyExamples [X, Y]
prop_Proxy   = verifyExamples ([Proxy] :: [Proxy Int])
prop_Id      = verifyExamples [Identity False]
prop_IdId    = verifyExamples [Identity (Identity False)]
prop_par     = verifyExamples [()]
prop_Bool    = verifyExamples [False]
prop_Int     = verifyExamples [0 :: Int]
prop_Maybe   = verifyExamples [Nothing, Just X, Just Y]
prop_Either  = verifyExamples [Left X, Left Y, Right X, Right Y]
prop_Tuple   = verifyExamples [(X, A), (X, B), (X, C), (Y, A), (Y, B), (Y, C)]
prop_Fork    = verifyExamples ([Fork 0 0] :: [Fork Int])

prop_ListSum = verifyExamplesSized 2
    [[], [X], [Y]]
    (Set.fromList [ LoopDetected "[Sum1]"])
prop_NE      = verifyExamplesSized 2
    [ False :| [] , False :| [ False] ]
    (Set.fromList [ LoopDetected "[Bool]" ])
prop_NEVoid  = verifyExamplesSized 2
    ([] :: [NonEmpty Void])
    (Set.fromList [ LoopDetected "[Void]" ])
prop_SList0  = verifyExamplesSized 0
    ([] :: [[Bool]])
    Set.empty
prop_SList1  = verifyExamplesSized 1
    [[] :: [Bool]]
    (Set.singleton $ LoopDetected "[Bool]")
prop_SList2  = verifyExamplesSized 2
    [[], [False]]
    (Set.singleton $ LoopDetected "[Bool]")
prop_SList3  = verifyExamplesSized 3
    [[], [False], [False, False]]
    (Set.singleton $ LoopDetected "[Bool]")
prop_SRec1   = verifyExamplesSized 1
    [Rec1]
    (Set.singleton $ LoopDetected "Rec1")
prop_SRec2   = verifyExamplesSized 2
    [C1 (C2 Rec1) , Rec1]
    (Set.fromList [LoopDetected "Rec1", LoopDetected "Rec2"])
prop_STree2  = verifyExamplesSized 2
    [Leaf False, Node (Leaf False) (Leaf False)]
    (Set.singleton $ LoopDetected "Tree Bool")
prop_Rose    = verifyExamplesSized 2
    [ NilF , ConsF (Branch False NilF) NilF ]
    (Set.fromList [ LoopDetected "Forest Bool" , LoopDetected "Rose Bool" ])
prop_ForkId' = verifyExamplesSized 2
    [Fork' (Identity False) (Identity False)]
    Set.empty

prop_Bush    = verifyExamplesArgs args examples warnings
  where
    args = Args {
        maxOccurences = 2
      , typesLimit = 4
      , productLimit = 10
      }

    examples =
      [ NilB
      , ConsB ( False , NilB )
      , ConsB ( False , ConsB ( NilB , NilB ) )
      , ConsB ( False , ConsB ( ConsB ( False , NilB ) , NilB ) )
      ]

    warnings = Set.fromList
      [ LoopDetected "((Bush Bool),(Bush (Bush (Bush Bool))))"
      , LoopDetected "(Bool,(Bush (Bush Bool)))"
      , LoopDetected "Bush (Bush Bool)"
      , LoopDetected "Bush Bool"
      , TypeLimitReached 4
      ]

prop_Term  = verifyExamplesArgs args examples warnings
  where
    args = Args {
        maxOccurences = 1
      , typesLimit = 4
      , productLimit = 10
      }

    examples =
      [ Var False
      , Abs (Var Zero)
      , Abs (Var (Succ False))
      , Abs (Abs (Var Zero))
      , Abs (Abs (Var (Succ Zero)))
      , Abs (Abs (Var (Succ (Succ False))))
      , Abs (Abs (Abs (Var Zero)))
      ]

    warnings = Set.fromList
      [ LoopDetected "Term (Bind (Bind Bool))"
      , LoopDetected "Term (Bind Bool)"
      , LoopDetected "Term Bool"
      , TypeLimitReached 4
      ]

prop_Perfect = withTests 1 $ property $
    getCanonicalExamplesWithWarnings args === (examples, Set.fromList [ TypeLimitReached 4 ])
  where
    examples =
      [ ZeroP False
      , SuccP (ZeroP (Fork False False))
      , SuccP (SuccP (ZeroP (Fork (Fork False False) (Fork False False))))
      ]
    args = Args {
        maxOccurences = 2
      , typesLimit = 4
      , productLimit = 10
      }

tests :: IO Bool
tests = and <$> sequence [checkParallel $$(discover)]
