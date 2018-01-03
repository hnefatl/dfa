import Test.Tasty

import DFATest (dfaTests)

main :: IO ()
main = putStrLn "Test suite not yet implemented"

tests :: TestTree
tests = testGroup "Global" [dfaTests]