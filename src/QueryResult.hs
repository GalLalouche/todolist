module QueryResult
  ( QueryResult (..)
  , toBytestring
  ) where

import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Data.List (intercalate)

import Types (Description (..), Index (..), Tag (..), TodoItem (..))

data QueryResult
  = Added Index
  | Done
  | Found [TodoItem]
  | FoundIndex [Index]
  deriving (Show, Eq)

toBytestring :: QueryResult -> B.ByteString
toBytestring result = case result of
  Added i     -> fromString (show $ getIndex i)
  Done        -> "done"
  Found items ->
    fromString (show (length items) ++ " item(s) found")
    <> foldMap itemToBytestring items
    where
      itemToBytestring (TodoItem (Index index) (Description d) tags) =
        "\n"
        <> fromString (show index)
        <> " \""
        <> d
        <> "\""
        <> foldMap showTag tags
        where
          showTag (Tag tag) = " #" <> tag
  FoundIndex indices ->
    fromString (show (length indices) ++ " item(s) found")
    <> foldMap itemToBytestring indices
    where
      itemToBytestring (Index index) = "\n" <> fromString (show index)

