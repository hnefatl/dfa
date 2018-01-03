import Test.Tasty

import DFATest (dfaTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Global" [dfaTests]