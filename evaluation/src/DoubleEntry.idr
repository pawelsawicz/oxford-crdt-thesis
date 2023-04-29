module DoubleEntry

import Data.List
import Data.Vect
import Data.SortedSet

import Core.Crdt
import Core.LogCrdt
import Core.Log.LogEvent

import Core.Semilattice

%default total
%hide Prelude.toList

data Direction = Debit | Credit

data Account = Bob | Alice

Eq Account where
  Bob == Bob     = True
  Alice == Alice = True
  _ == _         = False

data Amount : Direction -> Type where
  MkAmount : Double -> (a : Direction) -> Amount a

record Entry direction where
  constructor MkEntry
  amount : Amount direction
  account : Account

record DoubleEntry where
  constructor MkDoubleEntry
  debitEntry : Entry Debit
  creditEntry : Entry Credit

createCreditEntry : Double -> Account -> Entry Credit
createCreditEntry a acc = MkEntry (MkAmount a Credit) acc

createDebitEntry : Double -> Account -> Entry Debit
createDebitEntry a acc = MkEntry (MkAmount a Debit) acc

createDoubleEntry : (amount : Double) -> DoubleEntry
createDoubleEntry a = MkDoubleEntry (createDebitEntry a Alice) (createCreditEntry a Bob)

createDoubleEntry' : Double -> (debit : Account) -> (credit: Account) -> DoubleEntry
createDoubleEntry' amount d c = 
  MkDoubleEntry (createDebitEntry amount d) (createCreditEntry amount c)

doubleEntryMap : DoubleEntry -> (Entry Credit, Entry Debit)
doubleEntryMap de = (de.creditEntry, de.debitEntry)

unwrapDoubleEntry : List (LogEvent k DoubleEntry) -> List (Entry Credit, Entry Debit)
unwrapDoubleEntry xs =
  let entries = map (\(MkLogEvent b _) => doubleEntryMap b) xs in
    entries

filterAccounts : Account -> (List (Entry c), List (Entry d)) -> (List (Entry c), List (Entry d))
filterAccounts acc xs =
  bimap filterByAccountType filterByAccountType xs where
    filterByAccountType : List (Entry a) -> List (Entry a)
    filterByAccountType entries = filter (\entry => isAccountType acc entry) entries where
      isAccountType : Account -> Entry e -> Bool
      isAccountType a e = e.account == a

calculateBalance : (List (Entry c), List (Entry d)) -> Double
calculateBalance (xs,ys) =
  let creditAmounts = map (\(MkEntry (MkAmount a dir) acc) => a) xs in
  let debitAmounts = map (\(MkEntry (MkAmount a dir) acc) => a) ys in
  let credits = (-1*) $ foldl (+) 0 creditAmounts in
  let debits = foldl (+) 0 debitAmounts in
    credits + debits

accountBalance : (List (Entry c), List (Entry d)) -> Account -> Double
accountBalance xs acc = calculateBalance $ filterAccounts acc xs

queryForAccount : (SortedSet (LogEvent k DoubleEntry), (Vect k Nat)) 
  -> (Account -> Double)
queryForAccount (xs,_) =
  (accountBalance. unzip . unwrapDoubleEntry . toList) xs

initial : (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat))
initial = (empty, [0,0])

doubleEntryLog : Crdt (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat))
doubleEntryLog =
  MkCrdt
    (Account -> Double)
    ((Fin 2) -> (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat)) ->
      (ele : DoubleEntry) -> (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat)))
    queryForAccount
    update
    lub

testingState : (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat))
testingState = 
  let up1 = doubleEntryLog.update 0 initial (createDoubleEntry 100) in
  let up2 = doubleEntryLog.update 1 up1 (createDoubleEntry 50) in
    up2

balanceTest : Account -> Double
balanceTest = doubleEntryLog.query testingState