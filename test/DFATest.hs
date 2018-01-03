module DFATest
(
    dfaTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
--import Test.Tasty.QuickCheck.Gen
import qualified DFA.Internal as D

shouldBe :: String -> D.Output -> TestTree
shouldBe input expected = testCase input $
            case D.toInputs input of
                Nothing -> assertFailure "Input could not be converted to DFA input"
                Just i  -> expected @=? D.evaluate i

pureValidSymbols :: [Char]
pureValidSymbols = ['+', '-', 'n', 'e']

validSymbols :: Gen Char
validSymbols = elements pureValidSymbols

invalidSymbols :: Gen Char
invalidSymbols = arbitrary `suchThat` (\c -> c `notElem` pureValidSymbols)

invalidInputs :: Gen [Char]
invalidInputs = do
                    invalids <- listOf1 invalidSymbols
                    valids   <- listOf  validSymbols
                    shuffle (invalids ++ valids)

dfaTests :: TestTree
dfaTests = testGroup "DFA Tests"
    [
        testGroup "toInput"
        [
            testCase "toInput '+'" $ do D.toInput '+' @?= Just D.Plus,
            testCase "toInput '-'" $ do D.toInput '-' @?= Just D.Minus,
            testCase "toInput 'n'" $ do D.toInput 'n' @?= Just D.ToggleNegate,
            testCase "toInput 'e'" $ do D.toInput 'e' @?= Just D.ToggleEnabled,

            testProperty "toInput '*'" (forAll invalidSymbols (\c -> D.toInput c == Nothing))
        ],
        testGroup "toInputs"
        [
            testCase "toInputs \"+-n--n\"" $
                do D.toInputs "+-n--n" @?= Just [D.Plus, D.Minus, D.ToggleNegate, D.Minus, D.Minus, D.ToggleNegate],
            testCase "toInputs \"+e----e++\"" $
                do D.toInputs "+e--e+" @?= Just [D.Plus, D.ToggleEnabled, D.Minus, D.Minus, D.ToggleEnabled, D.Plus],
            
            testProperty "toInputs \"*\"" (forAll invalidInputs (\s -> D.toInputs s == Nothing))
        ],
        testGroup "fromInput"
        [
            testCase "fromInput Plus" $ do D.fromInput D.Plus @?= '+',
            testCase "fromInput Minus" $ do D.fromInput D.Minus @?= '+',
            testCase "fromInput ToggleNegate" $ do D.fromInput D.ToggleNegate @?= 'n',
            testCase "fromInput ToggleEnabled" $ do D.fromInput D.ToggleEnabled @?= 'e'
        ],
        testGroup "evaluate"
        [
            "+-n--n" `shouldBe` [1,-1,0,1,1,0],
            "+e----e++" `shouldBe` [1,0,0,0,0,0,0,1,1],
            "-n++e++e--+-n++" `shouldBe` [-1,0,-1,-1,0,0,0,0,1,1,-1,1,0,1,1]
        ]
    ]