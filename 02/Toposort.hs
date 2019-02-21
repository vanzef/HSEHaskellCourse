module Toposort where

import           Control.Monad.RWS.Lazy
import           Control.Monad.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


{-|
__#9 Топологическая сортировка__
Рассмотрим DFS алгоритм топологической сортировки:
<https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search>.
Заметим, что нам надо передавать неизменяемый @`Graph` a@ в качестве
аргумента для функции @visit@, вывод функции записывается в конец
списка, а также нужно постоянно поддерживать структуру с маркировкой
вершин. Похоже на работу для @Reader@, @Writer@ и @State@.
Введем следующие типы данных: 'Node', 'Edge', 'Graph' и 'Mark'.
В качестве структуры с метками предлагается использовать @Map@ из
"Data.Map".

__Задача__: реализуйте алгоритм топологической сортировки, используя
монаду @RWS r w s@.
В данной задаче предлагается воспользоваться
пакетом @mtl@, самостоятельно реализовывать стек @RWS@ не надо.
Необходимо реализовать монадический объект @torjan@ и функцию
@toposort@.
Заметим, что нужно реализовать необходимые инстансы для 'Node', 'Edge',
'Graph' и 'Mark'.
В данной задаче можно пользоваться @deriving@.
При необходимости, импортируйте другие модули из @mtl@.
-}


data Node a = Node a

data Edge a
  = Edge { from :: Node a
         , to   :: Node a
         }

newtype Graph a
  = Graph { getGraph :: Set (Edge a) }

data Mark
  = NoMark
  | Temporary
  | Permanent
  deriving Show

tarjan :: RWS (Graph a) [Node a] (Map (Node a) Mark) ()
tarjan = undefined
  where
    visit = undefined

-- ^ Описание алгоритма можно найти
-- [здесь](https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search).

toposort :: Graph a -> [Node a]
toposort = undefined
-- ^ Функция @toposort@ должна исползоваться @tarjan@.
--
-- __Подсказка-критерий__: вся работа должна происходить в 'tarjan', в
-- 'toposort', возможно, необходимо только привести данные к удобному
-- виду.
