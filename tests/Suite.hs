module Main where
import           AocPrelude
import           Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual,
                             runTestTT, runTestTTAndExit)

testMakeFileName = TestCase $ do
    assertEqual "" "inputs/foo/21.txt" (makeFileName "foo" "21")

tests = TestList [TestLabel "testMakeFileName" testMakeFileName]

main = runTestTTAndExit tests
