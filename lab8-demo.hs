{-

Polimorfism

- mecanism al limbajelor prin care se poate defini un set de operații (interfață
comună) pentru mai multe tipuri

- categorii: parametric și ad-hoc.

-}

{-

Poliformism parametric

- functiile pot opera pe structuri de date generice

- ex: functia length (nu depinde de tipul elementelor listei)

- daca nu am fi avut genericitate, ar fi trebuit noi (sau compilatorul) sa
  implementam functia pentru fiecare tip folosit in program.

- pentru semnatura, se foloseste o abstractiune numita variabila de tip, de ex:
length :: [a] -> Int
Semnătura de tip spune: “Pentru orice tip a, funcția length ia o listă cu
elemente din a și întoarce un întreg”.

- functiile pot contine mai multe variabile de tip:
map :: (a -> b) -> [a] -> [b]

- ca rezultat: algoritmi generici, aplicabili într-un număr mare de cazuri,
  încurajând reutilizarea de cod

-}

{-
Poliformism ad-hoc

Implementare elem:
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem x (y:ys)   = x == y || elem x ys

Eq a -> constrangere de tip, generata de existenta == in corpul lui elem

- functia este generica, dar intr-un sens mai restrans
- poate fi aplicata pe Int, String, etc
- nu poate fi aplicata pe functii

- in plus, se poate comporta diferit in functie de cum este implementata
  egalitatea fiecarui tip

-}

{-
Clase (Type class)

- diferit fata de conceptul din POO
- se aseamana mai mult cu o interfata din Java
- o clasă reprezintă un set de funcții care definesc o interfață sau un
  comportament unitar pentru un tip de date

- exemplu Haskell:
class  Eq a  where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

echivalent Java:
interface Eq<A implements Eq<A>> {
    boolean eq(another: A);
    boolean notEq(another: A);
}
-}

data Person = Person {name :: String, cnp :: Integer}
-- includem Person în clasa Eq astfel:
instance Eq Person where
    Person name1 cnp1 == Person name2 cnp2 = name1 == name2 && cnp1 == cnp2
    p1 /= p2 = not (p1 == p2)

{-
- spre deosebire de Java, se decupleaza implementarea clasei de definirea tipului
=> modularitate mai bună
- putem înrola tipuri create anterior în alte biblioteci în clase proaspăt create
  de noi
- putem defini multiple implementări ale unei clase pentru un acelasi tip de date
  în module diferite și să importăm doar implementarea care ne interesează într-un
  anumit caz (însă acest lucru nu este recomandat)
-}

-- pentru o clasa cu tipuri generice de date
data BST a = Empty | Node a (BST a) (BST a)
-- instance         Eq Person where
instance Eq a => Eq (BST a) where
    Empty == Empty = True
    Node info1 l1 r1 == Node info2 l2 r2 = info1 == info2 && l1 == l2 && r1 == r2
    _ == _ = False

    t1 /= t2 = not (t1 == t2)

{-
Extindere de clase

- necesar când dorim ca un tip inclus într-o clasă să fie inclus doar dacă face deja
  parte dintr-o altă clasă
- Ord contine doar <, > si >=

“Dacă a este în Eq, atunci a poate fi în Ord dacă definește funcțiile de mai jos.”

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
-}

{-
Membri impliciți

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    a /= b = not (a == b) -- definit implicit, dar poate fi suprascris
-}

data Person2 = Person2 {name2 :: String, cnp2 :: Integer}
instance Eq Person2 where
    Person2 name1 cnp1 == Person2 name2 cnp2 = name1 == name2 && cnp1 == cnp2

{-
in Prelude

class  Eq a  where
    (==), (/=)           :: a -> a -> Bool
    x /= y               = not (x == y)
    x == y               = not (x /= y)
-}

{-
Clase predefinite

Ord, Show, Read, Enum, Num, Integral, Fractional, Floating, Monad

- a monad is an abstraction that allows structuring programs generically
- they abstract away boilerplate code needed by the program logic
- simplify a wide range of problems, like handling potential undefined values
  (with the Maybe monad), or keeping values within a flexible, well-formed list
  (using the List monad)
https://en.wikipedia.org/wiki/Monad_(functional_programming)
-}

-- Deriving
-- data BST a = Empty | Node a (BST a) (BST a) deriving (Eq)

-- Num
-- :t 5
-- 5 :: Num p => p
-- 5 este o reprezentare pentru toate tipurile numerice

-- Tipuri de ordin superior

--  map :: (a -> b) -> [a] -> [b]
treeMap :: (a -> b) -> BST a -> BST b

treeMap _ Empty = Empty
treeMap f (Node info l r) = Node (f info) (treeMap f l) (treeMap f r)

-- data Maybe a = Nothing | Just a
--   map :: (a -> b) -> [a] -> [b]
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

{-
Observăm un șablon comun între definițiile lui map, treeMap, maybeMap.
Semnăturile lor sunt foarte similare – definesc structuri de date, sau
așa-zise “containere”, care pot fi “mapate” – adică putem aplica o funcție
pe fiecare din elementele conținute și obținem un nou container cu aceeași
forma, dar cu alt conținut.
-}

-- definim o clasa
class MyFunctor container where
    myfmap :: (a -> b) -> container a -> container b

instance MyFunctor BST where
   myfmap - Empty = Empty
   myfmap f (Node a l r) = Node (f a) (myfmap f l) (myfmap f r)

{-
BST se comportă ca o funcție, dar la nivel de tip.
Constructorii de tip BST iau ca argument un tip de date și produce alt tip de date.
De exemplu, BST Int ia tipul Int ca argument și produce un tip de date care reprezintă
un arbore de întregi. Spunem că BST este un tip de ordin superior (higher-order type).
In cazul de față este un constructor de tip unar.
-}

main = do
    print $ elem 2 [1,2,3]
    print $ elem "b" ["ab", "b", "cd"]
    -- print $ elem (+1) [(+1), (*2), (/3)] -- eroare, nu exista functie de egalitate pe functii
    print $ (Person "Ion" 4) == (Person "Ion" 4)
    print $ (Person "Ion" 4) == (Person "Ion" 5)
    print $ Empty /= (Node 5 Empty Empty)
    print $ (Person2 "Ion" 4) /= (Person2 "Vasile" 4)
