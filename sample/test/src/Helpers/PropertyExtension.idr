module Helpers.PropertyExtension

import Hedgehog

%default total

public export
comm_property : (Eq a, Show a) =>
  (x, y : (Gen a)) -> (f : (a -> a -> a)) -> Property

comm_property g1 g2 f = property $
  do
    x <- forAll g1
    y <- forAll g2
    (f x y) === (f y x)

public export
assoc_property : (Eq a, Show a) => 
  (x, y, z : (Gen a)) -> (f : (a -> a -> a)) -> Property

assoc_property g1 g2 g3 f = property $
  do
    x <- forAll g1
    y <- forAll g2
    z <- forAll g3
    (f (f x y) z) === (f x (f y z))

public export
idempotent_property : (Eq a, Show a) =>
  (x : (Gen a)) -> (f : (a -> a -> a)) -> Property

idempotent_property g1 f = property $
  do
    x <- forAll g1
    (f x x) === x

public export
identity_property : (Eq a, Show a) =>
  (x, neutral : (Gen a)) -> (f : (a -> a -> a)) -> Property

identity_property g1 n f = property $
  do
    x <- forAll g1
    neutral <- forAll n
    (f x neutral) === x