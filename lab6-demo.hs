-- Tipuri: Char, Bool, String (== [Char]), Num, Int, Integer, etc.

-- Moduri de a scrie si apela functii

f1 = \x y -> x + y
f2 x y = x + y


-- Limbaj pur (cu mici exceptii, in general in partea de IO)
-- https://stackoverflow.com/questions/15183324/what-is-the-difference-between-pure-and-impure-in-haskell

main = do
    print (f1 2 3) -- 5
    print (f2 2 3) -- 5

-- Moduri de a scrie apelurile de functii

-- 2 + 3 == (+) 2 3
-- elem 2 [1,2,3] == 2 `elem` [1, 2, 3]

-- Sectiuni -> asemanatoare cu functiile curry din Racket
-- In Haskell functiile sunt by default curry

{-

(2 +) == \x -> 2 + x
(+ 2) == \x -> x + 2
(- 2) == -2        -- minus e considerat semn
(2 -) == \x -> 2 - x

-}

-- Listele sunt omogene (toate elementele au acelasi tip)
-- Diverse moduri de a genera liste [1,3,5]
    print $ [1, 3, 5]
    print $ 1 : 3 : 5 : []
    print $ [1, 3 .. 5]
    print $ [1, 3 .. 6]

-- : operatorul cons
-- ++ operatorul de concatenare
    print $ 1 : [2,3]
    print $ [1] ++ [2,3]

-- head si tail sunt echivalantele pentru car si cdr
    print $ head [1, 2, 3] -- 1
    print $ tail [1, 2, 3] -- [2, 3]

-- in plus, exista last si init
    print $ last [1, 2, 3] -- 3
    print $ init [1, 2, 3] -- [1, 2]

-- vreau sa imi creez un predicat care sa imi faca 3 verificari:
-- lista sa nu fie nula, 3 sa fie element al acesteia si 5 sa nu fie
-- mypred L = not null L &&  elem 3 L && notElem 5 L
    print $ mypred [1,2,3,4,5]

-- Tupluri -> sintaxa cu paranteze rotunde (la liste este cu paranteze patrate)
    print $ ("Hello", True, 2)
    print $ fst ("Hello", True)  -- "Hello"
    print $ snd ("Hello", True)  -- True

-- Functionale
    print $ map (+ 2) [1, 2, 3]             -- [3, 4, 5]
    print $ filter odd [1, 2, 3, 4]          -- [1, 3]
    print $ foldl (+) 0 [1, 2, 3, 4]         -- 10
    print $ foldl (-) 0 [1, 2]              -- -3 <==> (0 - 1) - 2
    print $ foldr (-) 0 [1, 2]              -- -1 <==> 1 - (2 - 0)
    print $ zip [1, 2] [3, 4]               -- [(1, 3), (2, 4)]
    print $ zipWith (+) [1, 2] [3, 4]        -- [4, 6]

-- Functia primita de foldl si foldr are o ordine a parametrilor diferita
    print $ foldr (\e acc -> e : acc) [] [1,2,3,4] -- [1,2,3,4]
    print $ foldl (\acc e -> e : acc) [] [1,2,3,4] -- [4,3,2,1]

-- List comprehension
    print $ take 5 [x | x <- [0, 2 ..], x `mod` 3 == 0] -- lista numerelor naturale pare, divizibile cu 3
-- lista completa ar fi [0,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108, ...

-- Garzi
-- Case
-- Pattern matching
    print $ factorial_if 5
    print $ factorial_guards 5
    print $ factorial_case 5
    print $ factorial_pm 5
    print $ factorial_pm_tail 5 1

-- Point free functions
    print $ my_point_free_func 5 -- 26

--------------------------------------------------------------------
mypred l = not (null l) &&  elem 3 l && notElem 5 l

factorial_if x = if x < 1 then 1 else x * factorial_if (x - 1)

factorial_guards x
  | x < 1 = 1
  | otherwise = x * factorial_guards (x - 1)

factorial_case x = case x < 1 of
  True -> 1
  _ -> x * factorial_case (x - 1)

factorial_pm 0 = 1
factorial_pm x = x * factorial_pm (x - 1)

factorial_pm_tail 0 acc = acc
factorial_pm_tail x acc = factorial_pm_tail (x - 1) (acc * x)


square x = x * x
inc x = x + 1

my_point_free_func = inc . square . (2 +) -- inc(square(2+x))
--------------------------------------------------------------------
