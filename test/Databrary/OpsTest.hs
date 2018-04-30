module Databrary.OpsTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Ops

unit_thenUse :: Assertion
unit_thenUse = do
    -- example
    True `thenUse` ("abc" :: String) @?= Just "abc"
    False `thenUse` ("abc" :: String) @?= Nothing

unit_useWhen :: Assertion
unit_useWhen = do
    -- example
    ("abc" :: String) `useWhen` True @?= Just "abc"

test_all :: [TestTree]
test_all =
    [ testCase "rightJust-1" (rightJust (Right 10) @?= (Just 10 :: Maybe Int))
    , testCase "rightJust-2" (rightJust (Left ()) @?= (Nothing :: Maybe Int))
    , testCase
        "mergeBy-1"
        (mergeBy compare [1, 3] [2, 4] @?= ([1, 2, 3, 4] :: [Int]))
    , testCase
        "groupTuplesBy-1"
        (groupTuplesBy (==) [(True, 1), (True, 2), (False, 3)]
            @?= ([(True, [1, 2]), (False, [3])] :: [(Bool, [Int])])
        )
    ]
