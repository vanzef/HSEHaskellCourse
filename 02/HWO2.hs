module HW02 where

import Control.Applicative (liftA2)
import Data.Char (toLower, isSymbol)

-- # 1 Инстансы полугрупп и моноидов

-- # 1.1  Показать, что тип ValueOpEx v является полугруппой
-- | (достаточно реализовать инстанс)

-- Рассмотрим тип данных ValueOp, параметризованный типом v.
-- | Подобные типы часто используются для алгебраического представления изменения значения по его ключу,
-- | например, в состоянии блокчейн-системы.
-- | Во-первых, значение может быть новым, для нового значения используется конструктор New.
-- | Во-вторых, мы можем изменить значение, соответствующее заданному ключу.
-- | В-третих, искомого значения может и не быть, для этого существует маркер NotExisted.
-- | Для удаления значения используется маркер Rem.

-- Бинарная операция над данным типом должна соотвествовать композиции двух изменений с состоянии по данному ключу.

data ValueOp v
  = New v       -- ^ новое значение
  | Upd v       -- ^ изменить значение
  | Rem         -- ^ удаление значение
  | NotExisted  -- ^ несуществущее значение

-- Проблема в том, что композиция некоторых конструктором немного ошибочка, например,
-- | композиция Upd и NotExisted не очень осмысленно, так как несуществующее значение
-- | не может быть обновлено.

-- Определим тип ValueOpEx v, данный тип изоморфен Maybe (ValueOpEx v).
-- Конструктор Op хранит значение типа (ValueOp v).
-- Err необходимо возвращать в тех случаях, когда композиция двух изменений не осмыслена,
-- как в примере выше.


-- Задача: реализовать инстанс полугруппы для типа (ValueOpEx v), где v - переменная типа.

-- Реализаций данного инстанса может быть несколько,
-- поэтому стоит в комментариях описывать, почему при тех или иных значениях на входе,
-- вы возвращаете тот или иной результат.

data ValueOpEx v
  = Op (ValueOp v)
  | Err

instance Semigroup (ValueOpEx v) where
  a <> b = undefined


-- # 1.2* Показать, что предложенная операция ассоциативна



-- #2. Еще немного моноидов и полугрупп

-- # 2.1.

-- Тип данных VerRes e a, параметризованный типами e и a изоморфен типу Either.
-- VErr хранит значение типа e, семантика -- возвращение ошибки в случае неудачного вычисления
-- VRes хранит значение типа a, успешный результат.

-- Показать, что тип (VerRes e a) является полугруппой,
-- | Доопределить полученную полугруппу до моноида,
-- | проверить, что полученная единица действительно является единицей

data VerRes e a
  = VErr e
  | VRes a
  deriving (Show, Eq)

instance Semigroup a => Semigroup (VerRes e a) where
  a <> b = undefined

testVerRes :: Bool
testVerRes = and
  [ verRes1 == verRes2
  , VErr "some log" <> VRes ['a'..'z'] == VErr "some log"
  ]
  where
    verRes1 :: VerRes () [Char]
    verRes1 = VRes ['a'..'z'] <> VRes ['1'..'0']

    verRes2 :: VerRes () [Char]
    verRes2 = VRes $ ['a'..'z'] ++ ['1'..'0']

instance Monoid a => Monoid (VerRes e a) where
  mempty = undefined

memptyTest :: Bool
memptyTest = mempty <> VRes ['a'..'z'] <> VErr "some log" == VErr "some log"

-- # 2.2*.

-- | доказать ассоциативность предложенной бинарной операции.

-- | проверить, что полученная единица действительно является единицей

-- # 2.3.

-- Тип (BlockIntegrityVerifier block) -- это тип, параметризованный типом block.
-- | Данный тип является оберткой над типом функции из абстрактного типа block в VerRes e (),
-- где e -- это тип ошибки, а () (одноэлементный тип) -- тип успешной проверки,
-- в данном случае, проверки сходимости некоторых абстрактных блоков.

-- Задача: реализовать инстансы классов Semigroup и Monoid для типа (BlockIntegrityVerifier block)

-- Подсказка: пользоваться тем фактом, что VerRes a b - полугруппа (моноид).

newtype BlockIntegrityVerifier block
  = BIV { unBIV :: block -> VerRes String () }

instance Semigroup (BlockIntegrityVerifier block) where
  a <> b = undefined

instance Monoid (BlockIntegrityVerifier block) where
  mempty = undefined

-- # 2.4*. Проверить аксиомы моноида для типа (BlockIntegrityVerifier block)

-- # 2.5. Реализовать инстансы моноида и полугруппы для типа Endo a,
-- | который является оберткой над типом функции из a в a.

newtype Endo a =
  Endo { runEndo :: a -> a }

instance Semigroup (Endo a) where
  a <> b = undefined

instance Monoid (Endo a) where
  mempty = undefined

-- # 2.6*. Проверить аксиомы полугруппы и моноида для типа (Endo a).

-- # 3.

-- Рассмотрим тип MyCont r a, параметризованный типами r и a.
-- Данный тип - обертка над типов функции (a -> r) -> r.
-- Что в функциональном программировании еще часто называется монадой Cont.
-- | предназначенной для так называемых функций с продолжением.

newtype MyCont r a
  = MyCont { runCont :: (a -> r) -> r }

-- # 3.1 Реализовать инстанс функтора для типа MyCont r

instance Functor (MyCont r) where
  fmap = undefined

-- # 3.2 Реализовать инстанс аппликатива для типа MyCont r

instance Applicative (MyCont r) where
  pure  = undefined
  (<*>) = undefined

-- # 3.3 Реализовать инстанс монады для типа MyCont r

instance Monad (MyCont r) where
  (>>=) = undefined


-- # 3.4*. Доказать законы классов Applicative и Monad для MyCont r


-- # 4.

-- Рассмотрим класс типов Monoidal f, который, на самом деле, изоморфен классу Applicative:

class Functor f => Monoidal f where
  munit :: f ()
  (<#>) :: f a -> f b -> f (a, b)

-- Например:

instance Monoidal Maybe where
  munit             = Just ()
  _ <#> Nothing     = Nothing
  Nothing <#> _     = Nothing
  Just a <#> Just b = Just (a, b)

instance Monoidal [] where
  munit     = [()]
  xs <#> ys = do
    x <- xs
    y <- ys
    return (x,y)

-- # 4.1. Выразить методы Applicative через Monoidal

pure'
  :: Monoidal f
  => a
  -> f a
pure' = undefined

(<**>)
  :: Monoidal f
  => f (a -> b)
  -> f a
  -> f b
(<**>) = undefined

-- # 4.2. Выразить методы Monoidal через Applicative

munit'
  :: Applicative f
  => f ()
munit' = undefined

(<##>)
  :: Applicative f
  => f a
  -> f b
  -> f (a, b)
(<##>) = undefined

testMonoidal :: Bool
testMonoidal =
  and [ ([succ, pred] <*> testList) == [succ, pred] <**> testList
      , (Just (4 :: Int) <##> Just (6 :: Int)) == (Just (4 :: Int) `appPair` Just (6 :: Int))
      , (Just (4 :: Int) `appPair` Just (6 :: Int)) == (Just (4 :: Int) <#> Just (6 :: Int))
      ]
  where
    testList :: [Int]
    testList = [1..10]
    appPair = liftA2 (,)

-- Если бы миром правили алгебраисты-теоретики,
-- | то монада в хаскелле вводилась бы следующим образом:

class Applicative m => AnotherMonad m where
  join :: m (m a) -> m a

-- # 4.3. Выразить AnotherMonad через Monad, иными словами,
-- | реализовать join методами класса типов Monad:

join'
  :: Monad m
  => m (m a)
  -> m a
join' = undefined

-- # 4.4. Выразить монадический bind через AnotherMonad

anotherBind ::
  AnotherMonad m
  => m a
  -> (a -> m b)
  -> m b
anotherBind = undefined

-- # 4.5. Реализовать альтернативную монаду списка:

instance AnotherMonad [] where
    join = undefined

-- # 4.6. Реализовать альтернативую монаду Maybe:

instance AnotherMonad Maybe where
    join = undefined


-- # 4.7* Предложить законы класса Monoidal и показать их эквивалентность законам
-- | класа Applicative

-- # 4.8* Предложить законы класса AnotherMonad и показать их эквивалентность законам
-- | класа Monad


-- # 5 Реализовать функции через do-нотацию

-- # 5.1

foldM
  :: Monad m
  => (a -> b -> m a)
  -> a
  -> [b]
  -> m a
foldM = undefined

-- # 5.2

bothM
  :: Monad m
  => (a -> m b)
  -> (a -> m c)
  -> m a
  -> m (b, c)
bothM = undefined

-- Дальше ничего интересного.

newtype Sum
  = Sum { runSum :: Int }
  deriving (Show, Eq, Ord)

plusSum
  :: Sum
  -> Sum
  -> Sum
plusSum (Sum a) (Sum b) = Sum $ a + b

instance Semigroup Sum where
  (<>) = plusSum

instance Monoid Sum where
  mempty = Sum 0

testM :: IO Bool
testM = do
  result1 <- foldM mappendM (Sum 0) (Sum <$> [1..10])
  result2 <- bothM fun1 fun2 (pure 1303)
  return $ result1 == Sum 55 && result2 == (2606, 1697809)
  where
    mappendM :: Sum -> Sum -> IO Sum
    mappendM = \x y -> return $ x <> y

    fun1, fun2 :: Int -> IO Int
    fun1 = \x -> return $ x + x
    fun2 = \x -> return $ x * x

-- # 5.3.

newtype ListT m a
  = ListT { runListT :: m [a] }

monadInMonad
  :: (Monad m, Monad n, Monoid b)
  => (m b -> n b)
  -> (a -> b)
  -> ListT m a
  -> n b
monadInMonad trans mor xsT =
    undefined

-- # 6

-- Рассмотрим класс MonadTrans.
-- MonadTrans позволяет делать новую монаду из существующей монады,
-- вкладывая в новую монаду все вычисления и действия из предыдущей монады.
-- Такой способ формирования новых монад называется трансформером монад,
-- и задается классом MonadTrans:

class MonadTrans n where
  lift :: Monad m => m a -> n m a

-- MonadTrans -- это класс с единственным методом, который берет значение в монаде m
-- и посылает его в новую монаду n.

-- Реализовать инстанс MonadTrans для следующих типов

-- # 6.1. MaybeT

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = undefined

-- # 6.2. ContT

newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
  lift = undefined

-- # 6.3. ListT

instance MonadTrans ListT where
  lift = undefined

-- # 7 Рассахарить do-нотацию

-- # 7.1.

prodM
  :: Monad m
  => (a -> m b)
  -> (c -> m d)
  -> m (a, c)
  -> m (b, d)
prodM f g mac = do
  (a, c) <- mac
  b <- f a
  d <- g c
  return (b, d)

-- # 7.2.

compose
  :: Monad m
  => (b -> m c)
  -> (a -> m b)
  -> m a
  -> m c
compose fm gm xm = do
  x <- xm
  gx <- gm x
  fm gx

-- # 7.3. Рассахарить list comprehension в do-нотацию

listFunction
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
listFunction fxs gxs xs =
  [ f x (g x) | f <- fxs, g <- gxs, x <- xs]

-- # 7.4. Рассахарить do-нотацию через методы классы типа Monad

listFunction'
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
listFunction' = undefined

-- # 7.5. Реализовать ту же функцию, раскрыв использованные методы класса типов Monad
-- | в соотвествии с тем, как реализован представитель класса типов Monad для списков.

listFunction''
  :: [a -> b -> c]
  -> [a -> b]
  -> [a]
  -> [c]
listFunction'' = undefined

listFunctionTest :: Bool
listFunctionTest =
  and [ listFunction fs gs vals  == listFunction' fs gs vals
      , listFunction' fs gs vals == listFunction'' fs gs vals
      ]
  where
    fs :: [Int -> Int -> Int]
    fs = [(+), (*), (-)]

    gs :: [Int -> Int]
    gs = [succ, pred]

    vals :: [Int]
    vals = [1..100]

-- # 8. Рассмотрим класс типов Contravariant, который является двойственным классу типов Functor

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

-- # 8.1
-- Реализовать инстанс класса типов Contravariant для однопараметрического типа Predicate a, который
-- является оберткой над одноместным предикатом, определенным на типе a

newtype Predicate a
  = Predicate { runPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap = undefined

predicateTest :: Bool
predicateTest =
  and [ (runPredicate $ contramap toLower (Predicate isSymbol)) '$' == True
      , (runPredicate $ contramap (`div` (49 :: Int)) (Predicate even)) 95 == False
      ]

-- # 8.2.

newtype Const a b
  = Const { runConst :: a }

instance Contravariant (Const a) where
  contramap = undefined

-- # 8.3

newtype Compare a
  = Compare { runCompare :: a -> a -> Ordering }

instance Contravariant Compare where
  contramap = undefined

compareTest :: Bool
compareTest =
  and
    [ (runCompare $ contramap length (Compare compare)) numbers1 numbers2 == LT
    , (runCompare $ contramap mconcat (Compare compare)) listString1 listString2 == GT
    ]
  where
    numbers1 = [1..10] :: [Int]
    numbers2 = [11..29]
    listString1 = ["harold", " hide "]
    listString2 = [" the ", "pain"]

-- использовать функцию main для прогона тестов для ваших решений.
-- Тест устроен просто: елси тесты пройдены, то main вернет поздравление.
-- В противном случае, main попросит перепроверить решения.

main :: IO ()
main = do
  fourResult <- testM
  let hwTest = and
                [ testVerRes
                , memptyTest
                , fourResult
                , testMonoidal
                , listFunctionTest
                , predicateTest
                , compareTest
                ]
  case hwTest of
    True  -> putStrLn "Success! Good job!"
    False -> putStrLn "Something went wrong! Check your solutions, please."
  putStrLn ""
