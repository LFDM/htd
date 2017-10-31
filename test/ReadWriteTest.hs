module ReadWriteTest
  ( suite
  ) where

import           Data.Map
import           Test.Tasty
import           Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "Lib"
    [ testGroup
        "addition"
        [testCase "can add stuff" addition1
        ]
    ]

addition1 :: Assertion
addition1  = expected @=? actual
  where
    actual = 1 + 1
    expected = 2

