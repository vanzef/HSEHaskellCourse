module RWS where

{-|
    __# 9 Монадический стек RWS__
    Все подпункты оцениваются одинаково, то есть за каждую задачу вида @9.a.b@
    даётся @n/6@ балла, где @n@ --- число баллов за всю задачу @9@.
-}

-- | __# 9.1 Reader__

-- | Зафиксируем некоторый тип @r@, заметим, что функции вида @r -> a@ являются
-- функтором, действительно:
--
-- @
-- instance Functor ((->) r) where
--   fmap = (.)
-- @
--
-- Поскольку инстанс @Functor@ для @((->) r@ определен
-- в "GHC.Base", воспользуемся типом-обёрткой:
--
-- @
-- newtype Reader r a = Reader { runReader :: r -> a }
-- @

-- | Семантика этого типа такова: вычисления, которые происходят в некотором
-- общем окружении @r@, которое можно локально изменять.

-- | При работе с монадой @'Reader' r@ удобно использовать следующие функции:
--
-- 1. @ask@ --- возвращает окружение,
--
-- 2. @local@ --- выполняет вычисление в модифицированном окружении.

newtype Reader r a = Reader { runReader :: r -> a }

-- | __Задача #9.1.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'Reader' r@. Использование @deriving@ в каком-либо виде запрещено.

-- | __Задача #9.1.2__: реализуйте функции-помощники @ask@, @local@.

ask :: Reader r r
ask = Reader id

local
  :: (r -> r)
  -> Reader r a
  -> Reader r a
local f (Reader h) = Reader $ h . f

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure x    = Reader $ const x
  Reader f <*> Reader g = Reader $ f <*> g

instance Monad (Reader r) where
  Reader f >>= g =
    Reader $ \x -> runReader (g (f x)) x

-- | __#9.2 Writer__

-- | Семантика этого типа такова: Writer является оберткой над типом упорядоченной пары,
-- первым элементом которой является некоторый результат вычисления,
-- а вторым -- лог для актуального результата вычислений.

-- | __Задача #9.2.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'Writer' w@. Использование @deriving@ в каком-либо виде запрещено.

-- | При работе с монадой @'Writer' w@ удобно использовать следующие функции:
--
-- 1. @tell@ --- записывает значение в @'Writer'@.
--
-- 2. @listen@ --- функция, заменяющая внутреннее состояние.
--
-- 3. @pass@ --- функция, изменяющая лог, но при этом сохраняет значение.

-- | __Задача #9.2.2__: реализуйте функции-помощники @tell@, @listen@ и @pass@.

newtype Writer w a
  = Writer { runWriter :: (a, w) }

tell
  :: Monoid w
  => w
  -> Writer w ()
tell w = Writer ((), w)

listen
  :: Monoid w
  => Writer w a
  -> Writer w (w, a)
listen (Writer (a,w)) =
  Writer ((w, a), w)

pass
  :: Monoid w
  => Writer w (a, w -> w)
  -> Writer w a
pass (Writer ((a, f), w)) =
  Writer (a, f w)

instance Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w1) <*> Writer (x, w2) = Writer (f x, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  Writer (a, w) >>= f = Writer (fst unWriter, w <> snd unWriter)
    where
      unWriter = runWriter (f a)


-- | __#9.3 State__

-- | Часто бывает так, что нужно использовать состояние, которых, как известно,
-- в Haskell нет.

-- Для эмуляции состояния принято использовать монаду @'State' s@.

-- | Монада State является комбинацией монад Reader и Writer.

-- | __Задача #9.3.1__: реализуйте инстансы @Functor@, @Applicative@ и @Monad@
-- для @'State' s@. Использование @deriving@ в каком-либо виде запрещено.

-- | При работе с монадой @'State' s@ удобно использовать следующие функции:
--
-- 1. @get :: 'State' s a@ --- функция, возвращающая внутреннее состояние,
--
-- 2. @put :: s -> 'State' s ()@ --- функция, заменяющая внутреннее состояние.

-- | __Задача #9.3.2__: реализуйте функции-помощники @get@, @put@.

newtype State s a
  = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \x -> (x,x)

put :: s -> State s ()
put s = State $ const ((), s)

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State h) =
    State $ \x -> (f . fst $ h x, snd . h $ x)

instance Applicative (State s) where
 -- pure :: a -> State s a
  pure x = State $ \y -> (x, y)
 -- (<*>) :: State s (a -> b) -> State s a -> State s b
  State fs <*> State gs =
    State $ \s ->
      let (fun, s1) = fs s
          (res, s2) = gs s1
      in (fun res, s2)

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= g = State $
    \x -> runState (g $ fst $ f x) (snd $ f x)
