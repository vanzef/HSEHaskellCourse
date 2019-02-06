module HW01 where

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
euclid = undefined

{-
3. Реализуйте функцию Эйлера:
https://en.wikipedia.org/wiki/Euler%27s_totient_function
-}


eulerTotient :: Integer -> Integer
eulerTotient = undefined


{-
4. Не пользуясь стандартными функциями, реализуйте возведение в степень:
-}

exp :: Integer -> Integer -> Integer
exp = undefined

{-
5. Функция integrate принимает одноместную функцию f :: Double -> Double, два вещественных числа a, b :: Double
и возвращает определенный интерграл функции f на отрезке [a,b], который вычисляется методом трапеций:
-}

integrate
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integrate = undefined


{- 6. Заселить следующие типы термами: -}

-- # 6.1:

permute :: (a -> b -> c) -> b -> a -> c
permute = undefined

-- # 6.2:

pairProd :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairProd = undefined

-- # 6.3:

fix :: (a -> a) -> a
fix = undefined
-- подсказка к # 6.3: вспомнить про комбинатор неподвижной точки, о котором говорилось на второй лекции:

-- # 6.4

weirdFunction
    :: (d -> d -> b)
    -> (a -> b -> c)
    -> (d -> b)
    -> d -> b
weirdFunction = undefined


{-
7. Определим тип ко-списков, где, в отличие от обычных списков, новый элемент добавляется не в голову, а
в хвост.
-}

data CoList a = Nil | Snoc (CoList a) a
    deriving (Show, Eq)

{-7.1 Реализовать функцию, которая по ко-списку возвращает список -}

listToCoList :: CoList a -> [a]
listToCoList = undefined

{-7.2 Реализовать конкатенацию ко-списков.
Реализация функции должна удовлетворять следующему равенству:
listToCoList (coListConcat a b) = (listToColist a) ++ (listToColist b),

 -}

coListConcat :: CoList a -> CoList a -> CoList a
coListConcat = undefined

{-
8. Определим тип деревьев с двоичным ветвлением
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

-- # 8.1 Реализовать instance класса типов Functor для деревьев

instance Functor Tree where
    fmap = undefined

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList = undefined

-- # 8.3 Аналогично для ко-списков

listToTree :: Tree a -> CoList a
listToTree = undefined

{- # 8.4 Реализовать проверку на пустоту -}

isEmpty :: Tree a -> Bool
isEmpty = undefined

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
eitherCommute = undefined

-- # 9.2 Аналогично

eitherAssoc :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc = undefined

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
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- # 10.2

filterList :: (a -> Bool) -> [a] -> [a]
filterList predicate [] = []
filterList predicate (x:xs) =
    if predicate x then (x : filterList predicate xs) else filterList predicate xs

-- # 10.3

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- # 10.4

distributivity :: (a, Either b c) -> Either (a, b) (a, c)
distributivity (x, Left y) = Left (x, y)
distributivity (x, Right y) = Right (x, y)
