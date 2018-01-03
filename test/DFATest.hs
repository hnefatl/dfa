module DFATest
(
    dfaTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import DFA

testTemplate :: String -> Integer -> TestTree
testTemplate s r = testCase s $ sum (evaluate s) @=? r

dfaTests :: TestTree
dfaTests = testGroup "DFA Tests"
    [
        testTemplate "+-n--n" 2,
        testTemplate "+e----e++" 3,
        testTemplate "-n++e++e--+-n++" 1
    ]