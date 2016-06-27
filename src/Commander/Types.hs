{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}

module Commander.Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource

import Control.Lens
import Control.Lens.TH

import Control.Exception

import Katip

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

data ConfigError = NoConfigurationFilesFoundError
                 | ConfigurationCouldNotParseError Text
  deriving (Show)

data ConfigFile = ConfigFile { _awsRegion           :: Region
                             , _numberOfInstances   :: Int
                             , _waitToRunningSec    :: Int
                             , _keyPairName         :: Text
                             , _amiIdentifier       :: Text
                             , _subnetIdentifier    :: Text
                             , _instanceType        :: Text
                             , _iamRole             :: Text
                             } deriving (Show)
makeLenses ''ConfigFile


data AppConfig = AppConfig { _configFile :: ConfigFile }
makeLenses ''AppConfig


data AppState = AppState { _ec2Instances   :: [Instance]
                         , _sessionId      :: Text
                         , _katipContext   :: LogContexts
                         , _katipLogEnv    :: LogEnv
                         , _katipNamespace :: Namespace
                         }
makeLenses ''AppState


newtype Commander m a = Commander { unStack :: ReaderT AppConfig (StateT AppState m) a  }
  deriving ( MonadReader AppConfig, MonadState AppState, Functor
           , Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadAWS )


instance MonadIO m => Katip (Commander m) where
  getLogEnv = use katipLogEnv


instance MonadIO m => KatipContext (Commander m) where
  getKatipContext   = use katipContext
  getKatipNamespace = use katipNamespace

runCommander :: MonadIO m => AppConfig -> AppState -> Commander m a -> m a
runCommander c s = (flip evalStateT) s . (flip runReaderT) c . unStack
