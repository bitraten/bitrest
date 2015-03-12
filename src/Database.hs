module Database(connPool, execute, query) where

import           Control.Applicative        ((<$>))
import           Data.Int                   (Int64)
import           Data.IORef                 (IORef, newIORef, readIORef)
import           Data.Maybe                 (fromJust)
import           Data.Pool                  (Pool, withResource)
import qualified Database.PostgreSQL.Simple as P
import           System.IO.Unsafe           (unsafePerformIO)


connPool :: IORef (Maybe (Pool P.Connection))
{-# NOINLINE connPool #-}
connPool = unsafePerformIO (newIORef Nothing)

withPool :: (P.Connection -> P.Query -> q -> IO r) -> P.Query -> q -> IO r
withPool action qry qs = do pool <- fromJust <$> readIORef connPool
                            withResource pool (\conn -> action conn qry qs)

query :: (P.FromRow r, P.ToRow q) => P.Query -> q -> IO [r]
query = withPool P.query

execute :: (P.ToRow q) => P.Query -> q -> IO Int64
execute = withPool P.execute
