module TestExtensions

%default total

public export
comms : (Eq a) => (x, y : a) -> (f : (a -> a -> a)) -> Bool
comms x y f = (f x y) == (f y x)

public export
assoc : (Eq a) => (x, y, z : a) -> (f : (a -> a -> a)) -> Bool
assoc x y z f = (f (f x y) z) == (f x (f y z))

public export
idempotent : (Eq a) => (x : a) -> (f : (a -> a -> a)) -> Bool
idempotent x f = (f x x) == x

public export
identity : (Eq a) => (x, neutral : a) -> (f : (a -> a -> a)) -> Bool
identity x neutral f = (f x neutral) == x