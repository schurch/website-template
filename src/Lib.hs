{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runScotty
    , runDB
    , lookupSessionValue
    , insertSessionValue
    , App
    , Config(..)
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Default.Class (def)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text.Lazy (Text, pack)
import Database.PostgreSQL.Simple (Only(..), connect, defaultConnectInfo, ConnectInfo(..), Connection)
import Database.PostgreSQL.Transaction (PGTransaction, query_, runPGTransactionT)
import Network.Wai (vault, Middleware)
import Network.Wai.Handler.Warp (setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, isNotAbsolute, addBase)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL (dbStore, defaultSettings, fromSimpleConnection)
import Web.Scotty.Trans (ScottyT, get, scottyOptsT, text, middleware, ActionT, Options, verbose, settings, request)

import qualified Data.Vault.Lazy as Vault

type VaultSessionKey = Vault.Key (Session IO String String)
type App = ScottyT Text Stack ()
type Action = ActionT Text Stack

data Env = Dev | Test | Prod deriving (Show)

data Config = Config
  { environment :: Env
  , vaultSessionKey :: VaultSessionKey
  }

newtype Stack a = Stack
  { unpackStack :: ReaderT Config PGTransaction a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


runScotty :: (Middleware -> App) -> IO ()
runScotty application = do
  key <- Vault.newKey
  conn <- connection >>= fromSimpleConnection
  store <- dbStore conn defaultSettings
  let sessionMiddleware = withSession store (fromString "SESSION") def key
  let config = createConfig key
  let opts = def { verbose = 1
                 , settings = setPort 4000 $ settings def
                 }
  scottyOptsT opts (runStack config) (application sessionMiddleware)

connection :: IO Connection
connection = connect defaultConnectInfo { connectUser = "website"
                                           , connectPassword = "password"
                                           , connectHost = "db"
                                           , connectPort = 5432
                                           , connectDatabase = "website" }

runDB :: PGTransaction a -> Action a
runDB = lift . Stack . lift

lookupSessionValue :: String -> Action (Maybe String)
lookupSessionValue lookupKey = do
  sessionFunctions' <- sessionFunctions
  case sessionFunctions' of
    Just (sessionLookup, _) -> liftIO $ sessionLookup lookupKey
    Nothing -> return Nothing

insertSessionValue :: String -> String -> Action ()
insertSessionValue key value = do
  sessionFunctions' <- sessionFunctions
  case sessionFunctions' of
    Just (_, sessionInsert) -> liftIO $ sessionInsert key value
    Nothing -> return ()

sessionFunctions :: Action (Maybe (Session IO String String))
sessionFunctions = do
  vaultSessionKey' <- lift $ asks vaultSessionKey
  vault' <- vault <$> request
  return $ Vault.lookup vaultSessionKey' vault'

createConfig :: VaultSessionKey -> Config
createConfig sessionKey' = Config
  { environment = Dev
  , vaultSessionKey = sessionKey'
  }

runStack :: Config -> Stack a -> IO a
runStack config stack = do
  connection' <- connection
  runPGTransactionT (runReaderT (unpackStack stack) config) connection'
