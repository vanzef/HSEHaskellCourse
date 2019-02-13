module HW01 where

import Data.List (genericLength)

{-
1. Задача на лямбда-исчисление

1.1. Уберите скобки в следующем лямбда-терме, произвести редукцию. Расписать пошагово:

((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))

1.2. Аналогично:

((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
-}

{-
Правило редукции:
(λ x. M) N -> M [x := N]

Правила избавления от скобок:
1. M N P = (M N) P
2. λ x. λ y. M = λ x. (λ y. M)
3. λ x y. M = λ x. λ y. M
-}

{-
2. Реализовать алгоритм Евклида:
-}

euclid :: Integer -> Integer -> Integer
euclid x 0 = x
euclid x y = euclid y (x `mod` y)

{-
3. Реализуйте функцию Эйлера:
https://en.wikipedia.org/wiki/Euler%27s_totient_function
-}


eulerTotient :: Integer -> Integer
eulerTotient a =
    genericLength . (filter $ coPrime a) $ [1..a - 1]
    where
        coPrime a b = (a `euclid` b) == 1


{-
4. Не пользуясь стандартными функциями, реализуйте возведение в степень:
-}

expon :: Integer -> Integer -> Integer
expon a 0 = 1
expon a b = a * (expon a (b - 1))

{-
5. Функция integrate принимает одноместную функцию f :: Double -> Double, два вещественных числа a, b :: Double
и возвращает определенный интерграл функции f на отрезке [a,b], который вычисляется методом трапеций:
-}

integrate
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integrate f a b =
    height * singleStep ((f a + f b) / 2) (a + height) 1
    where
        height = (b - a) / seg
        seg = 1000
        singleStep acc x i
            | i == seg     = height * acc
            | otherwise    = singleStep (acc + (f x)) (x + height) (i + 1)


{- 6. Заселить следующие типы термами: -}

-- # 6.1:

permute :: (a -> b -> c) -> b -> a -> c
permute f x y = f y x

-- # 6.2:

pairProd :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairProd f g (a, c) = (f a, g c)

-- # 6.3:

fix :: (a -> a) -> a
fix f = f (fix f)
-- подсказка к # 6.3: вспомнить про комбинатор неподвижной точки, о котором говорилось на второй лекции:

-- # 6.4

weirdFunction
    :: (d -> d -> b)
    -> (a -> b -> c)
    -> (d -> b)
    -> d -> b
weirdFunction f _ g x = g x
-- второй вариант: weirdFunction f _ g x = f x x


{-
7. Определим тип ко-списков, где, в отличие от обычных списков, новый элемент добавляется не в голову, а
в хвост.
-}

data CoList a = Nil | Snoc (CoList a) a
    deriving (Show, Eq)

{-7.1 Реализовать функцию, которая по ко-списку возвращает список -}

coListToList :: CoList a -> [a]
coListToList Nil = []
coListToList (Snoc xs x) = coListToList xs ++ [x]

{-7.2 Реализовать конкатенацию ко-списков.
Реализация функции должна удовлетворять следующему равенству:
listToCoList (coListConcat a b) = (listToColist a) ++ (listToColist b),

 -}

coListConcat :: CoList a -> CoList a -> CoList a
coListConcat Nil ys = Nil
coListConcat (Snoc xs x) ys = Snoc (coListConcat xs ys) x

{-
8. Определим тип деревьев с двоичным ветвлением
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

-- # 8.1 Реализовать instance класса типов Functor для деревьев

instance Functor Tree where
 -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node left node right) =
        Node (fmap f left) (f node) (fmap f right)

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node left node right) =
    (treeToList left) ++ [node] ++ (treeToList right)

-- # 8.3 Аналогично для ко-списков

treeToCoList :: Tree a -> CoList a
treeToCoList Leaf = Nil
treeToCoList (Node left node right) =
    treeToCoList left `coListConcat`
    Snoc Nil node `coListConcat`
    treeToCoList right

{- # 8.4 Реализовать проверку на пустоту -}

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

{- # 9. В стандартной библиотеке языка Haskell определен двухпараметрический тип Either,
data Either a b = Left a | Right b, семантика которого похожа на семантику Maybe, который обсуждался на семинаре.
Если нужное нам вычисление закончилось хорошо, то мы кладем результат в Right (правое вложение), а если
вычисление закончилось не очень хорошо, то мы применяем Left, который, в отличие от Nothing, еще требует объекта
некоторого типа a
Пример:

divideEither :: (Fractional a, Eq a) => a -> a -> Either String a
divideEither a b =
    if b == 0 then (Left "cho ti delash, ne deli na nol' ples") else Right (a / b)

> divideEither 5 0
Left "cho ti delash, ne deli na nol' ples"

> divideEither 5 6
Right 0.8333333333333334
 -}

-- # 9.1 Заселить данный тип

eitherCommute :: Either a b -> Either b a
eitherCommute (Left a)  = Right a
eitherCommute (Right b) = Left b

-- # 9.2 Аналогично

eitherAssoc :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc (Left a)          = Left (Left a)
eitherAssoc (Right (Left b))  = Left (Right b)
eitherAssoc (Right (Right c)) = Right c

{- 10. В Haskell определена также конструкция case of, которая позволяет делать паттерн-матчинг
внутри реализации функции.


Примеры:

caseOfListLength :: [a] -> Int
caseOfListLength xs = case xs of
    [] -> 0
    (x:xs) -> 1 + caseOfListLength xs

booleanImplication :: Bool -> Bool -> Bool
booleanImplication x y = case (not x || y) of
    True -> True
    False -> False

Реализовать через case of следующие функции -}

-- # 10.1

listSum :: Num a => [a] -> a
listSum xs = case xs of
    []       -> 0
    (x : xs) -> 1 + listSum xs

-- # 10.2

filterList :: (a -> Bool) -> [a] -> [a]
filterList predicate xs = case xs of
    [] -> []
    (x : xs) -> case predicate x of
        True  -> x : filterList predicate xs
        False -> filterList predicate xs

-- # 10.3

safeHead :: [a] -> Maybe a
safeHead xs = case xs of
    []       -> Nothing
    (x : xs) -> Just x

-- # 10.4

distributivity :: (a, Either b c) -> Either (a, b) (a, c)
distributivity (x, y) = case y of
    Left b  -> Left (x, b)
    Right c -> Right (x, c)
