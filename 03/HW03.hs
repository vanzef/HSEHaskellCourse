module HW03 where

import Control.Applicative (Alternative (..))
import Control.Monad (forever)
import Data.Char

-- 1. Предположим, у нас есть простой тип парсера.

newtype PrsE a
  = PrsE { runPrsE :: String -> Either String (a, String) }

-- Данный тип - это обертка над функцией, которая принимает строку,
-- а возвращает либо сообщение об ошибке, либо пару с результатом разбора типа a
-- и оставшейся строкой.

-- 1.1. Реализовать функцию, делающую парсер из унарного предиката над Char

satisfy :: (Char -> Bool) -> PrsE Char
satisfy = undefined

-- 1.2. Реализовать парсер, который разбирает первый символ непустой строки

anyE :: PrsE Char
anyE = undefined

charE :: Char -> PrsE Char
charE c = satisfy (== c)

satTest :: Bool
satTest =
  and [ runPrsE (satisfy isDigit) "lorem ipsum" == Left "Parse error"
      , runPrsE (satisfy isPunctuation) "-,+[-[>>++++[>++++++++<-]" == Right ('-', ",+[-[>>++++[>++++++++<-]")
      , runPrsE anyE "" == Left "Parse error"
      , runPrsE anyE "ABCDE" == Right ('A', "BCDE")
      ]

-- 2.1. Реализовать инcтансы классов Functor, Applicative и Alternative

instance Functor PrsE where
  fmap = undefined

instance Applicative PrsE where
  pure  = undefined
  (<*>) = undefined

instanceTest :: Bool
instanceTest =
  and [ runPrsE ((,) <$> anyE <* charE 'B' <*> anyE) "ABCDE" == Right (('A','C'),"DE")
      , runPrsE (digitToInt <$> anyE) "BCD" == Right (5, "CD")
      , runPrsE (anyE *> anyE) "ABCDE" == Right ('B', "CDE")
      ]

-- 3. Определить слабую головную нормальную форму следующих термов
-- (weak head normal form):

-- 3.1
{-
distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

где:

distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left x) =
  (Left x, Left x)
distributivity (Right (y, z)) =
  (Right y, Right z)
-}

{-
import Data.Maybe (mapMaybe)

-- 3.2

null $ mapMaybe foo "pole chudes ochen' chudesno"

где
foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing

-- 3.3

join [[1,2,3], [1,2,3], [4,5,6]]
-}

-- 4. Реализовать следующие функции для монады IO

-- 4.1

seqIO :: [IO ()] -> IO ()
seqIO = undefined

-- Данную функцию реализовать через seqIO

-- 4.2

putStr' :: String -> IO ()
putStr' = undefined

-- 4.3

-- Данная функция получает строку (с использованием getLine),
-- после к полученной строке принимается парсер charE,
-- а на консоль возвращается либо сообщение об ошибке "parse error",
-- либо строка, второй компонент пары (a, String), которая получается в результате удачного разбора строки.

putPrsE :: IO ()
putPrsE = undefined

-- Пример:
{-
> putPrsE
Enter the line:
ABCDE

Output:
BCDE

Enter the line:

Output
Parse error
-}

-- 5. Рассмотрим тип данных, остатки по модулю 2:

data Z2 = Zero | One
  deriving (Show, Read, Eq)

-- На данном типе реализованы следующие функции

mod2Plus
  :: Z2 -> Z2 -> Z2
mod2Plus x y =
  case x == y of
    True  -> Zero
    False -> One

mod2Mult
  :: Z2 -> Z2 -> Z2
mod2Mult One One = One
mod2Mult _ _     = Zero

-- Реализовать функцию, которая в мире IO считывает значение по модулю 2
-- и возвращает его инволюцию, для чего надо реализовать функцию отрицания
-- через mod2Plus и mod2Mult. Данное монадическое действие должно итерироваться постоянно
-- (использовать функцию forever)

ioNeg :: IO ()
ioNeg = undefined

-- Пример работы:
{-
> ioNeg
Enter the value:
One

Result:
Zero

Enter the value:
Zero

Result:
One
-}
