{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Data.Semigroup ((<>))
import Data.Text.Lazy (Text, pack)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Transaction (PGTransaction, query_)
import Lib
import Lucid (renderText)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, isNotAbsolute, addBase)
import Page
import Web.Scotty.Trans (get, text, middleware, html)

main :: IO ()
main = runScotty application

application :: Middleware -> App
application sessionMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots <> isNotAbsolute <> addBase "static")
  middleware sessionMiddleware
  get "/" $ do
    [Only value] <- runDB (query_ "select 1 + 1" :: PGTransaction [Only Int])
    insertSessionValue "count" (show value)
    e <- lift $ asks environment
    text $ pack $ show e ++ " " ++ show value
  get "/hello" $ do
    value <- lookupSessionValue "count"
    case value of
      Just v -> text $ pack ("HELLO " ++ v)
      Nothing -> text "NO HELLO"
  get "/hello2" $ do
    html $ renderText page

