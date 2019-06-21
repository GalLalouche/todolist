module Runner
  ( performQuery
  , runNQueries
  ) where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import Data.Char (toLower)
import Data.Word (Word8)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parse)

import Query (Query (..), queryParser)
import QueryResult (QueryResult (..), toBytestring)
import Types (MonadTodoList (..))

performQuery :: (MonadTodoList m, MonadIO m) => Query -> m QueryResult
performQuery (Add d tags) = do
  index <- add d tags
  return $ Added index
performQuery (MakeDone index) = do
  done index
  return Done
performQuery (Search params) = do
  items <- search params
  return $ Found items

-- Modified from http://www.brool.com/post/haskell-performance-lowercase/
ctype_lower = listArray (0, 255) (map (BI.c2w . toLower) ['\0' .. '\255']) :: UArray Word8 Word8

lowercase = B.map (\x -> BI.w2c $ ctype_lower ! BI.c2w x)

runNQueries :: (MonadTodoList m, MonadIO m) => Int -> m ()
runNQueries n =
  replicateM_ n $
    -- Since comparison should be case-insensitive, and since we don't care about the output text other than
    -- the numbers, we can just convert it to lowercase on parsing.
   do
    rawQuery <- liftIO $ lowercase <$> B.getLine
    case parse queryParser "-" rawQuery of
      Left err -> error (show err)
      Right parsedQuery -> do
        result <- performQuery parsedQuery
        liftIO $ B.putStrLn (toBytestring result)
