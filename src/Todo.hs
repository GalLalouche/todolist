{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}

module Todo
  ( TodoListM
  , runTodoList
  ) where

import Control.Monad (void, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify, runStateT, put)
import Data.ByteString.Char8 (unpack)
import Data.Char (isLetter, toLower)
import Debug.Trace
import Data.Function (on)
import Data.List (intersect, intercalate, isSubsequenceOf)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as M
import Data.Maybe(maybe)

import Types (MonadTodoList (..), TodoItem (..), Description(..), Index(..), Tag(..), SearchParams(..), SearchWord(..))

-- The general idea is that the list only keeps undone items (since undone items can't be viewed anyway), and
-- the next index to use (we can't use the index of the last item since it might have been removed).
data TodoList = TodoList { itemsByIndex :: !(M.Map Word TodoItem), nextIndex :: !Word} deriving (Show, Eq)

emptyTodoList :: TodoList
emptyTodoList = TodoList M.empty 0

index :: TodoItem -> Word
index = getIndex . tiIndex

newtype TodoListM a = TodoListM { runTodoListM :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runTodoList :: TodoListM a -> IO ()
runTodoList = void . flip runStateT emptyTodoList . runTodoListM

instance MonadTodoList TodoListM where
  add :: Description -> [Tag] -> TodoListM Index
  add desc tags = TodoListM $ do
    m <- itemsByIndex <$> get
    i <- gets nextIndex
    let newItem = TodoItem (Index i) desc tags
    modify $ addItem newItem
    return $ Index i where
      addItem newItem (TodoList items lastIndex) = let
          updatedMap = M.insert (index newItem) newItem items
        in TodoList updatedMap (lastIndex + 1)

  done :: Index -> TodoListM ()
  done index = TodoListM $ modify $ markDone $ getIndex index where
      -- This could have been done using lenses but it's a bit of an overkill here.
      markDone index (TodoList items lastIndex) = TodoList (M.delete index items) lastIndex

  search :: SearchParams -> TodoListM [TodoItem]
  search sp = TodoListM $ do
    descElems <- gets $ fmap snd . M.toDescList . itemsByIndex
    let filtered = filter (matches sp) descElems
    return filtered where
      matches :: SearchParams -> TodoItem -> Bool
      matches (SearchParams words tags) (TodoItem _ d t) = wordsMatch (getDescription d) words && tagsMatch t tags where
        wordsMatch d = all (wordMatches d) . map getSearchWord
        wordMatches d w = isSubsequenceOf (unpack w) (unpack d)
        tagsMatch existingTags = all (hasMatchingTag existingTags)
        hasMatchingTag existingTags queryTag = any (tagMatches queryTag) existingTags
        tagMatches queryTag existingTag = isSubsequenceOf (unpack $ getTag queryTag) (unpack $ getTag existingTag)

