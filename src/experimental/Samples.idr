module Samples

import Data.Vect

data ProgramInput = Dummy
data Schools = Dummy
data BestGrade = Dummy

typeDrivenDevelopmentExample : ProgramInput -> Schools -> BestGrade
typeDrivenDevelopmentExample = ?hole

composeExample : List Nat -> List Nat
composeExample = (map (\x => x+2)) . sort

evalueFunction : List Nat -> List Nat
evalueFunction xs = map (\x => x+2) (sort xs)