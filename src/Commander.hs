{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
module Commander
    ( someFunc
    ) where

import Commander.Conf

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Control.Monad.Trans.Error
import Control.Monad.Trans.Resource

import Data.UUID    (toText)
import Data.UUID.V4 (nextRandom)

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import System.IO

import Network.AWS
import Network.AWS.EC2

import Katip
import Commander.EC2
import Commander.Types

developmentEnv = Environment "Development"
productionEnv  = Environment "Production"

namespace :: Namespace
namespace =  Namespace ["Commander"]

someFunc :: IO ()
someFunc = void $ do
-- Get default config environment unless specified otherwise by command line args
  confFile <- getConfigOrExit
  scribe   <- mkHandleScribe ColorIfTerminal stdout InfoS V3
  le       <- registerScribe "stdout" scribe <$> initLogEnv namespace developmentEnv
  awsEnv   <- newEnv Oregon Discover
  uuid     <- toText <$> nextRandom

  let state :: AppState
      state = (AppState mempty uuid mempty le namespace) 

      config :: AppConfig
      config = (AppConfig confFile)

  runAWSWithEnv awsEnv . runCommander config state $ do
    $(logTM) InfoS "Starting"
    createInstances 

    $(logTM) InfoS "Instances ready."

    $(logTM) InfoS "Terminating Instances"
    terminateInstancesInState

    $(logTM) InfoS "Instances Terminated"
    $(logTM) InfoS "Stopping"


  
runAWSWithEnv :: Env -> AWS a -> IO a
runAWSWithEnv env = runResourceT . runAWS env

reportErrors :: SomeException -> IO ()
reportErrors = putStrLn . displayException
