module DFATest
(
    dfaTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
--import Test.Tasty.QuickCheck.Gen
import qualified DFA as D

shouldBe :: String -> D.Output -> TestTree
shouldBe input expected = testCase input $
            case D.toInputs input of
                Nothing -> assertFailure "Input could not be converted to DFA input"
                Just i  -> expected @=? D.evaluate i

dfaTests :: TestTree
dfaTests = testGroup "DFA Tests"
    [
        testGroup "toInput"
        [
            testCase "toInput '+'" $ do D.toInput '+' @?= Just D.Plus,
            testCase "toInput '-'" $ do D.toInput '-' @?= Just D.Minus,
            testCase "toInput 'n'" $ do D.toInput 'n' @?= Just D.ToggleNegate,
            testCase "toInput 'e'" $ do D.toInput 'e' @?= Just D.ToggleEnabled,

            let others = arbitrary `suchThat` (\c -> c `notElem` ['+', '-', 'n', 'e'])
                conversionFails c = D.toInput c == Nothing
            in
                testProperty "toInput '*'" (forAll others conversionFails)
        ],
        testGroup "evaluate"
        [
            "+-n--n" `shouldBe` [1,-1,0,1,1,0],
            "+e----e++" `shouldBe` [1,0,0,0,0,0,0,1,1],
            "-n++e++e--+-n++" `shouldBe` [-1,0,-1,-1,0,0,0,0,1,1,-1,1,0,1,1]
        ]
    ]