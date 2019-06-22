{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CorrectTodo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT, gets, modify, runStateT)
import Data.ByteString.Char8 (unpack)
import Data.List (isSubsequenceOf)
import qualified Data.Map.Strict as M

import Types (Description (..), Index (..), MonadTodoList (..), SearchParams (..), SearchWord (..),
              Tag (..), TodoItem (..))

-- The general idea is that the list only keeps undone items (since undone items can't be viewed anyway), and
-- the next index to use (we can't use the index of the last item since it might have been removed).
data TodoList =
  TodoList
    { itemsByIndex :: !(M.Map Word TodoItem)
    , nextIndex    :: !Word
    }
  deriving (Show, Eq)

index :: TodoItem -> Word
index = getIndex . tiIndex

newtype TodoListM a =
  TodoListM
    { runTodoListM :: StateT TodoList IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList =
  let emptyTodoList = TodoList M.empty 0
   in void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add :: Description -> [Tag] -> TodoListM Index
  add desc tags =
    TodoListM $ do
      i <- gets nextIndex
      modify $ addItem $ TodoItem (Index i) desc tags
      return $ Index i
    where
      addItem newItem (TodoList items lastIndex) =
        let updatedMap = M.insert (index newItem) newItem items in TodoList updatedMap (lastIndex + 1)

  done :: Index -> TodoListM ()
  done index = TodoListM $ modify $ markDone $ getIndex index
      -- This could have been done using lenses but it's a bit of an overkill here.
    where
      markDone index (TodoList items lastIndex) = TodoList (M.delete index items) lastIndex

  search :: SearchParams -> TodoListM [Index]
  search sp =
    TodoListM $ do
      descElems <- gets $ fmap snd . M.toDescList . itemsByIndex
      return $ map tiIndex $ filter (matches sp) descElems
    where
      matches :: SearchParams -> TodoItem -> Bool
      matches (SearchParams queryWords queryTags) (TodoItem _ (Description d) itemTags) =
        wordsMatch d queryWords && tagsMatch (map getTag itemTags) (map getTag queryTags)
        where
          wordsMatch desc = all (wordMatches desc) . map getSearchWord
          wordMatches = flip unpackIsSubsequenceOf
          tagsMatch existingTags = all (hasMatchingTag existingTags)
          hasMatchingTag existingTags queryTag = any (tagMatches queryTag) existingTags
          tagMatches = unpackIsSubsequenceOf
          unpackIsSubsequenceOf a b = isSubsequenceOf (unpack a) (unpack b)
