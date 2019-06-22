{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This implementation only provides an approximation of the required functionality: instead of looking for
-- subsequences, it looks for word intersection, i.e., if query word matches the description iff it is equal
-- to one of the words in the descriptions, and query tag matches the item tag iff it equal to one of the tags.
module PerformanceTodo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify, runStateT)
import Data.ByteString.Char8 (ByteString, words)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Types
  ( Description(..)
  , Index(..)
  , MonadTodoList(..)
  , SearchParams(..)
  , SearchWord(..)
  , Tag(..)
  , TodoItem(..)
  )

import Prelude hiding (words)

type ByteStrings = Seq.Seq ByteString

data InternalTodoItem =
  InternalTodoItem
    { itiWords :: !ByteStrings
    , itiTags :: !ByteStrings
    }
  deriving (Show, Eq, Ord)

type Words = Seq.Seq Word

type IndexMap = M.HashMap ByteString (Set.Set Word)

-- Like the Correct implementation, we only keep undone items. However, we also keep a map from each word in
-- the description to the item index from each tag to the index, so we could quickly search matching items.
-- We also keep a map from index to all its description words and tags so we could quickly delete it on done.
data TodoList =
  TodoList
    { itemsByIndex :: !(M.HashMap Word InternalTodoItem)
    , indicesByDescWord :: !IndexMap
    , indicesByTagWord :: !IndexMap
    , nextIndex :: !Word
    }
  deriving (Show, Eq)

newtype TodoListM a =
  TodoListM
    { runTodoListM :: StateT TodoList IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = let
      emptyTodoList = TodoList M.empty M.empty M.empty 0
   in void . flip runStateT emptyTodoList . runTodoListM



instance MonadTodoList TodoListM where
  add :: Description -> [Tag] -> TodoListM Index
  add (Description desc) tags =
    TodoListM $ do
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
      updateMap :: Word -> ByteStrings -> IndexMap -> IndexMap
      updateMap w bss im = foldr aux im bss
        where
          aux :: ByteString -> IndexMap -> IndexMap
          aux bs = M.insertWith Set.union bs (Set.singleton w)

  done :: Index -> TodoListM ()
  done index = TodoListM $ modify $ markDone $ getIndex index
    where
      markDone :: Word -> TodoList -> TodoList
      markDone index list@(TodoList items descIndex tagIndex lastIndex) =
        case M.lookup index items of
          Nothing -> list
          Just (InternalTodoItem words tags) ->
            let updatedItems = M.delete index items
                updatedDescs = removeFromMap index words descIndex
                updatedTags = removeFromMap index tags tagIndex
             in TodoList updatedItems updatedDescs updatedTags (lastIndex + 1)
      removeFromMap :: Word -> ByteStrings -> IndexMap -> IndexMap
      removeFromMap w bss im = foldr aux im bss
        where
          aux :: ByteString -> IndexMap -> IndexMap
          aux = M.adjust (Set.delete w)

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
              (qt, qw) -> wordIntersections `Set.intersection` tagIntersections
      return $ map Index $ reverse $ Set.toList intersections
    where
      getSets :: [ByteString] -> IndexMap -> Set.Set Word
      getSets bs im =
        case map (getSet im) bs of
          [] -> Set.empty
          (x:xs) -> foldr Set.intersection x xs
      getSet :: IndexMap -> ByteString -> Set.Set Word
      getSet im bs = fromMaybe Set.empty (M.lookup bs im)