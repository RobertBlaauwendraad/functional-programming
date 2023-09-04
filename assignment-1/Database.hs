module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
robert = ("Robert", 20,  "Functional Programming")
frits = ("Frits", 25, "Functional Programming")

students :: [Person]
students = [elena, peter, pol, robert, frits]

age :: Person -> Age
age (_, n, _)  =  n

name             :: Person -> Name
name (n, _, _)   =  n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_, _, c)  =  c

showPerson       :: Person -> String
showPerson p = name p ++ " is " ++ show (age p) ++ " years old and likes " ++ favouriteCourse p

twins            :: Person -> Person -> Bool
twins p1 p2 = age p1 == age p2

increaseAge      :: Person -> Person
increaseAge p = (name p, age p + 1, favouriteCourse p)

-- first develop the expressions in GHCi, then replace the TODO's below with them
query1 :: [Person]
query1 = map increaseAge students

query2 :: [Person]
query2 = let promote p = ("dr. " ++ name p, age p, favouriteCourse p) in map promote students

query3 :: [Person]
query3 = filter (\p -> name p == "Frits") students

query4 :: [Person]
query4 = filter (\p -> age p >= 20 && age p < 30) students

query5 :: Age
query5 = sum (map age students) `div` toInteger (length students)

query6 :: [Person]
query6 = let promote p = ("dr. " ++ name p, age p, favouriteCourse p) in filter (\p -> favouriteCourse p == "Functional Programming") (map promote students)
