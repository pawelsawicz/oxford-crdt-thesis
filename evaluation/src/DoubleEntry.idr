module DoubleEntry

import Data.List
import Data.Vect
import Data.SortedSet

import Core.Crdt
import Core.LogCrdt
import Core.Log.LogEvent

import Core.Semilattice

data Direction = Debit | Credit

data Account = Bob | Alice

Eq Account where
  Bob == Bob     = True
  Alice == Alice = True
  _ == _         = False

data Amount : Direction -> Type where
  MkAmount : Nat -> (a : Direction) -> Amount a

%default total

record Entry direction where
  constructor MkEntry
  amount : Amount direction
  account : Account

record DoubleEntry where
  constructor MkDoubleEntry
  creditEntry : Entry Credit
  debitEntry : Entry Debit

createCreditEntry : Nat -> Account -> Entry Credit
createCreditEntry a acc = MkEntry (MkAmount a Credit) acc

createDebitEntry : Nat -> Account -> Entry Debit
createDebitEntry a acc = MkEntry (MkAmount a Debit) acc

createDoubleEntry : (amount : Nat) -> DoubleEntry
createDoubleEntry a = MkDoubleEntry (createCreditEntry a Bob) (createDebitEntry a Alice)

isAccountType : Entry d -> Account -> Bool
isAccountType e a = e.account == a

-- filterAccount : List (LogEvent k DoubleEntry) -> List (LogEvent k DoubleEntry)
-- filterAccount xs = filter (\(MkLogEvent (MkDoubleEntry c d) _) => (isAccountType c Bob) || (isAccountType d Bob)) xs

doubleEntryMap : DoubleEntry -> (Entry Credit, Entry Debit)
doubleEntryMap de = (de.creditEntry, de.debitEntry)

ff : List (LogEvent k DoubleEntry) -> List (Entry Credit, Entry Debit)
ff xs =
  let entries = map (\(MkLogEvent b _) => doubleEntryMap b) xs in
    entries

unzipEntries : List (Entry c, Entry d) -> (List (Entry c), List (Entry d))
unzipEntries = unzip

filterAccounts : Account -> (List (Entry c), List (Entry d)) -> (List (Entry c), List (Entry d))
filterAccounts acc (xs,ys) = (filter (\x => isAccountType x acc) xs, filter (\y => isAccountType y acc) ys)

calculateBalance : (List (Entry c), List (Entry d)) -> Integer
calculateBalance (xs,ys) =
  let creditAmounts = map (\(MkEntry (MkAmount a dir) acc) => a) xs in
  let debitAmounts = map (\(MkEntry (MkAmount a dir) acc) => a) ys in
  let credits = natToInteger (foldl (+) 0 creditAmounts) in
  let debits = natToInteger (foldl (+) 0 debitAmounts) in
    credits - debits

queryForAccount : (SortedSet (LogEvent k DoubleEntry), (Vect k Nat)) -> 
  SortedSet (LogEvent k DoubleEntry)

queryForAccount (xs,_) = 
  let list = Data.SortedSet.toList xs in
  let accountEntries = list in ?hole2

initial : (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat))
initial = (empty, [0,0])

doubleEntryLog : Crdt (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat))
doubleEntryLog =
  MkCrdt
    (SortedSet (LogEvent 2 DoubleEntry))
    ((Fin 2) -> (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat)) ->
      (ele : DoubleEntry) -> (SortedSet (LogEvent 2 DoubleEntry), (Vect 2 Nat)))
    query
    update
    lub

deState : List (LogEvent 2 DoubleEntry)
deState = 
  let up1 = doubleEntryLog.update 0 initial (createDoubleEntry 100) in
  let up2 = doubleEntryLog.update 1 up1 (createDoubleEntry 50) in
    Data.SortedSet.toList (doubleEntryLog.query up2)

balanceTest : Account -> Integer
balanceTest acc =
  let result = calculateBalance $ filterAccounts acc $ unzipEntries $ ff deState in result