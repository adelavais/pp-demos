-- mecanism de tipuri care are la baza tipurile de date algebrice
-- scop: garantarea unor proprietăți de corectitudine în funcție de tipurile folosite

{-
Haskell este un limbaj puternic tipat

cod de C valid:
int x = -1;
double y = x;

cod de Haskell echivalent, nevalid:
(puteti rula exemplul din fișierul lab7-exemplu1.hs)

x :: Int
x = -1

y :: Double
y = x

soluție: folosire de funcții care fac conversie de tip, de exemplu fromIntegral

-}

-- tipare puternică => liste omogene

{-

tipare statică, la compilare => nu rulează nimic dacă există erori de tip

raționament: tipurile de date reprezintă principala metodă de abstractizare
în limbajele de programare, astfel că, dacă semantica programelor este corectă
atunci corectitudinea implementării va decurge din aceasta

nu e 100% fail proof, de exemplu atunci când se folosesc funcții parțiale
exemplu: head []
nu va da eroare la compilare, pentru ca primește tipul corect de date, listă,
dar va da eroare dinamică (la runtime) pentru că nu există o definiție a
funcției pe această valoare

-}

{-

Stabilirea statică a tipurilor este făcută cu ajutorul unui mecanism de
sinteză de tip: la compilare sunt verificate tipurile tuturor expresiilor,
compilarea terminându-se cu succes doar când acestea corespund. Sinteza
este efectuată pe tipuri de date oricât de complexe, astfel că, de exemplu,
o expresie expr având tipul:

nume :: tipul
nume :: p1 -> p3 ... -> pn -> val ret
nume :: constrangeri despre urm param => p1 -> p2 -> .... -> val retur

expr :: [(a,Int)]

va fi verificată în adâncime, de la „rădăcină” (tipul listă) către „frunze”
(variabila de tip *a*, tipul *Int*).

-}

-- Tipuri de date definite de utilizator

-- type - similar cu typedef din C
-- ne permite definirea unui sinonim de tip
type Point = (Int, Int)
p :: Point
p = (2, 3)
-- se face o echivalență internă între contructorul perechii și Point
-- p = (2.0, 3.0) -- eroare, primește Double, nu Int

-- data - permite definirea de noi tipuri de date algebrice
-- ATENȚIE la distincția dintre numele tipului (denumit și constructor de tip)
-- și numele constructorilor (denumiți și constructori de date, sunt văzuți ca funcții)

data PointT = PointC Double Double deriving Show
pointDouble = PointC 3.0 2.0

-- The only classes in the Prelude for which derived instances are allowed are
-- Eq, Ord, Enum, Bounded, Show, and Read
-- https://stackoverflow.com/a/3864746

-- dacă scriem în consolă ":t <expresie>" putem să inspectăm semnături sau tipuri

-- data permite declararea de tipuri enumerate, similare cu construcția enum din C
data Colour = Red | Green | Blue | Black deriving Show

nonColour :: Colour -> Bool
nonColour Black = True
nonColour _ = False

-- Tipuri înregistrare
-- asociere de nume pentru câmpurile structurii
-- pot fi utilizați pentru a face match sau modificare selectivă
-- implicit se creează 2 funcții
-- px (PointC x _) = x
-- py (PointC _ y) = y

data PointT2 = PointC2
    { px :: Double
    , py :: Double
    } deriving Show

p2 = PointC2 3.0 2.0
newP2 = p2 { px = 5 }


-- tipuri parametrizate
-- tipuri care primesc ca parametru un alt tip
-- data Maybe a = Just a | Nothing deriving (Show, Eq, Ord)
-- structură utilă când lucrăm cu funcții care pot eșua în a întoarce o valoare utilă

maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _ = Nothing

-- tipuri recursive
data List a = Void | Cons a (List a) deriving Show
data Natural = Zero | Succ Natural deriving Show

-- newtype
-- similară cu `data`
-- cu diferența că ne permite crearea unui tip de date cu un singur constructor pe un singur argument
-- pe baza altor tipuri de date existente
-- spre deosebire de `type`, creează un nou tip, nu un tip identic

newtype Celsius = MakeCelsius Float deriving Show
c = MakeCelsius 20.0
-- sau
-- newtype Celsius = MakeCelsius { getDegrees :: Float } deriving Show

-- diferența principală între `data` și `newtype` este că `newtype` permite
-- crearea de tipuri izomorfe: atât `Celsius` cât și `Fahrenheit` sunt tipuri
-- identice cu `Float` din punctul de vedere al structurii, însă folosirea lor
-- în cadrul programului diferă, `Float` având o semantică mai generală
-- (orice număr în virgulă mobilă)

newtype Fahrenheit = MakeFahrenheit Float deriving Show
f = MakeFahrenheit 5.0

celsiusToFahrenheit :: Celsius -> Fahrenheit
celsiusToFahrenheit (MakeCelsius c) = MakeFahrenheit $ c * 9/5 + 32

-- sintaxă de pattern matching pentru constructori

data MyData = D
  { d1 :: Double
  , d2 :: Double
  , d3 :: Double
  } deriving (Show, Eq)

sumD (D x y z) = x + y + z
prodD (D x y z) = x * y * z

sum_prodD d@(D x y z) = D (x / prod) (y / prod) (z / prod)
  where prod = prodD d

main = do
    print p
    print pointDouble
    print $ nonColour Black
    print $ nonColour Red
    print $ p2
    print $ newP2
    -- următoarele 2 comenzi merg în consolă, de investigat de ce nu și în contextul acesta
    -- print $ maybeHead []
    -- print $ maybeHead [1]
    print $ Succ (Succ Zero)
    print c
    print f
    -- print $ MakeFahrenheit c - eroare
    print $ celsiusToFahrenheit c
    print $ sumD (D 1.0 2.0 3.0)
    print $ sum_prodD (D 1.0 2.0 3.0)
