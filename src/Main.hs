module Main where

import           Data.ByteString.Char8      (pack)
import           Data.IORef                 (writeIORef)
import           Data.Pool                  (createPool)
import           Database.PostgreSQL.Simple (close, connectPostgreSQL)
import           Network.Wai.Handler.Warp   (runEnv)
import           System.Environment         (getEnv)

import           Api                        (api)
import           Database                   (connPool, runMigrations)

main :: IO ()
main = do
    db_url <- getEnv "DATABASE_URL"
    pool <- createPool (connectPostgreSQL $ pack db_url) close 1 10 5
    writeIORef connPool $ Just pool
    _ <- runMigrations pool
    runEnv 8000 api
