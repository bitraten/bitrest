module Main where

import           Network.Wai.Handler.Warp   (runEnv)

import           Api                        (api)
import           Database                   (setupDB, runMigrations)

main :: IO ()
main = do
    pool <- setupDB
    _ <- runMigrations pool
    runEnv 8000 api
