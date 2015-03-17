module Database(connPool, execute, query, runMigrations, setupDB) where

import           Control.Applicative                  ((<$>))
import           Data.ByteString.Char8                (pack)
import           Data.Int                             (Int64)
import           Data.IORef                           (IORef, newIORef,
                                                       readIORef, writeIORef)
import           Data.Maybe                           (fromJust)
import           Data.Pool                            (Pool, createPool,
                                                       withResource)
import qualified Database.PostgreSQL.Simple           as P
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult,
                                                       runMigration)
import           System.Environment                   (getEnv)
import           System.IO.Unsafe                     (unsafePerformIO)

import           Paths_bitrest


connPool :: IORef (Maybe (Pool P.Connection))
{-# NOINLINE connPool #-}
connPool = unsafePerformIO (newIORef Nothing)

withPool :: (P.Connection -> P.Query -> q -> IO r) -> P.Query -> q -> IO r
withPool action qry qs = do pool <- fromJust <$> readIORef connPool
                            withResource pool (\conn -> action conn qry qs)

setupDB :: IO (Pool P.Connection)
setupDB = do db_url <- getEnv "DATABASE_URL"
             pool <- createPool (P.connectPostgreSQL $ pack db_url) P.close 1 10 5
             writeIORef connPool $ Just pool
             return pool

query :: (P.FromRow r, P.ToRow q) => P.Query -> q -> IO [r]
query = withPool P.query

execute :: (P.ToRow q) => P.Query -> q -> IO Int64
execute = withPool P.execute

runMigrations :: Pool P.Connection -> IO (MigrationResult String)
runMigrations pool = do migrations <- getDataFileName "migrations"
                        withResource pool (\conn -> do
                            _ <- P.withTransaction conn $ runMigration $ MigrationContext MigrationInitialization True conn
                            P.withTransaction conn $ runMigration $ MigrationContext (MigrationDirectory migrations) True conn
                            )
