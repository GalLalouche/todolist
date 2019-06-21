{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify, runStateT)
import Data.ByteString.Char8 (ByteString, findSubstring, unpack, words)
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Debug.Trace

import Types (Description (..), Index (..), MonadTodoList (..), SearchParams (..), SearchWord (..),
              Tag (..), TodoItem (..))

import Prelude hiding (words)

-- The general idea is that the list only keeps undone items (since undone items can't be viewed anyway), and
-- the next index to use (we can't use the index of the last item since it might have been removed).
type ByteStrings = Seq.Seq ByteString
data InternalTodoItem =
  InternalTodoItem
    { itiWords :: !ByteStrings
    , itiTags  :: !ByteStrings
    }
  deriving (Show, Eq, Ord)

type Words = Seq.Seq Word
type IndexMap = M.HashMap ByteString (S.Set Word)

data TodoList =
  TodoList
    { itemsByIndex      :: !(M.HashMap Word InternalTodoItem)
    , indicesByDescWord :: !IndexMap
    , indicesByTagWord  :: !IndexMap
    , nextIndex         :: !Word
    }
  deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList M.empty M.empty M.empty 0

newtype TodoListM a =
  TodoListM
    { runTodoListM :: StateT TodoList IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

updateMap :: Word -> ByteStrings -> IndexMap -> IndexMap
updateMap w bss im = foldr aux im bss
  where
    aux :: ByteString -> IndexMap -> IndexMap
    aux bs = M.insertWith S.union bs (S.singleton w)

removeFromMap :: Word -> ByteStrings -> IndexMap -> IndexMap
removeFromMap w bss im = foldr aux im bss
  where
    aux :: ByteString -> IndexMap -> IndexMap
    aux = M.adjust (S.delete w)

instance MonadTodoList TodoListM where
  add :: Description -> [Tag] -> TodoListM Index
  add (Description desc) tags =
    let
     in TodoListM $ do
          i <- gets nextIndex
          modify $ addItem $ InternalTodoItem descWords tagBytes
          return $ Index i
    where
      descWords = Seq.fromList $ words desc
      tagBytes = Seq.fromList $ map getTag tags
      addItem newItem (TodoList items descIndex tagIndex lastIndex) =
        let updatedItems = M.insert lastIndex newItem items
            updatedDescs = updateMap lastIndex descWords descIndex
            updatedTags = updateMap lastIndex tagBytes tagIndex
         in TodoList updatedItems updatedDescs updatedTags (lastIndex + 1)
  done :: Index -> TodoListM ()
  done index = TodoListM $ modify $ markDone $ getIndex index
      -- This could have been done using lenses but it's a bit of an overkill here.
    where
      markDone :: Word -> TodoList -> TodoList
      markDone index list@(TodoList items descIndex tagIndex lastIndex) = case M.lookup index items of
          Nothing -> list
          Just (InternalTodoItem words tags) -> let
              updatedItems = M.delete index items
              updatedDescs = removeFromMap index words descIndex
              updatedTags = removeFromMap index tags tagIndex
           in TodoList updatedItems updatedDescs updatedTags (lastIndex + 1)
  search :: SearchParams -> TodoListM [Index]
  search (SearchParams queryWords queryTags) =
    TodoListM $ do
      (TodoList _ descMap tagMap _) <- get
      let wordIntersections = getSets (map getSearchWord queryWords) descMap
      let tagIntersections = getSets (map getTag queryTags) tagMap
      let intersections =
            case (queryWords, queryTags) of
              (qw, []) -> wordIntersections
              ([], qt) -> tagIntersections
              (qt, qw) -> wordIntersections `S.intersection` tagIntersections
      return $ map Index $ reverse $ S.toList intersections
    where
      getSets :: [ByteString] -> IndexMap -> S.Set Word
      getSets bs im =
        case map (getSet im) bs of
          []     -> S.empty
          (x:xs) -> foldr S.intersection x xs
      getSet :: IndexMap -> ByteString -> S.Set Word
      getSet im bs = fromMaybe S.empty (M.lookup bs im)
