{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( startApp
    , app
    ) where

import           System.Process
import           System.IO 

import           Control.Monad
import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.Except

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence 

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           GHC.Generics
import           Network.HTTP.Client (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           System.Environment
import qualified Paths_texBot as P
import           Data.Version (showVersion)

data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO,
                 MonadReader BotConfig, MonadError ServantErr)

data BotConfig = BotConfig 
  { telegramToken :: Token
  , manager :: Manager
  }

type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook" -- maps to /webhook/<secret_token>
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
          transform config = Nat (flip runReaderT config . runBot)

botApi :: Proxy BotAPI
botApi = Proxy

botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'
          handleWebhook :: Text -> Update -> Bot ()
          handleWebhook secret update = do
              Token token <- asks telegramToken
              if EQ == compare secret token
                 then handleUpdate update
                 else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
--      Update { ... } more cases will go here
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update

startApp :: IO ()
startApp = do
    putStrLn "bot is starting..."
    env <- getEnvironment
    manager' <- newManager tlsManagerSettings
    let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
        config = BotConfig
            { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
            , manager = manager'
            }
    run 8080 $ app config

