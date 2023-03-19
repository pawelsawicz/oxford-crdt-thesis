module Main

import Control.App
import Control.App.Console

-- data Test = MkTest String Bool

-- mergeIdempotentTest : Test
-- mergeIdempotentTest = MkTest "Test 1" True

-- tests : List Test
-- tests = [mergeIdempotentTest]

-- runTests : List Test -> IO ()
-- runTests xs = printLn "Enter a number: "

listofNumbers : List Nat
listofNumbers = [1,2,3,4,5,6]

hello : Console es => App es ()
hello = putStrLn (show Z)

main : IO ()
main = run hello