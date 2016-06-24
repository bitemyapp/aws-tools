{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
module Commander.EC2 where

import Commander.Conf

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Trans.Error
import Control.Monad.Trans.Resource

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import System.IO

import Network.AWS
import Network.AWS.EC2

import Katip
import Commander.Types
import Commander.EC2.SecurityGroup

import Debug.Trace

installPlebScript :: Text
installPlebScript = "echo Hello World"

-- | Checks to see if the instances stored in local state are running
allInstancesAreReady :: forall m . (MonadAWS m, MonadState AppState m) => m Bool
allInstancesAreReady = areInstancesRunning . getStates <$> (instanceStatuses =<< getInstanceIdsInState)
  where
    areInstancesRunning [] = False
    areInstancesRunning xs = all (ISNRunning ==) xs

    getStates :: DescribeInstanceStatusResponse -> [InstanceStateName] 
    getStates r = r ^.. disrsInstanceStatuses . traverse . isInstanceState . _Just . isName

    instanceStatuses :: [Text] -> m DescribeInstanceStatusResponse
    instanceStatuses xs = send $ describeInstanceStatus & disInstanceIds .~ xs

-- | Gets the InstanceIds from the instances held in AppState
getInstanceIdsInState :: (MonadState AppState m) => m [Text]
getInstanceIdsInState = fmap (view insInstanceId) <$> use ec2Instances
    
getInstanceState :: Instance -> InstanceStateName
getInstanceState i = i ^. (insState . isName)

isInstanceStateEq :: InstanceStateName -> Instance -> Bool
isInstanceStateEq s = (s ==) . getInstanceState

updateInstances :: (MonadAWS m, MonadState AppState m, KatipContext m) => m ()
updateInstances = do
  instanceIds <- getInstanceIdsInState 
  response    <- send $ describeInstances & diiInstanceIds .~ instanceIds
  ec2Instances .= response ^. dirsReservations . traverse . rInstances

waitUntilInstancesAreRunning :: (MonadAWS m, MonadReader AppConfig m, MonadState AppState m, KatipContext m) => m ()
waitUntilInstancesAreRunning = do
  allRunning <- allInstancesAreReady
  secs <- view $ configFile . waitToRunningSec
  liftIO $ putStrLn . show $ allRunning
  if allRunning then return ()
  else do
    $(logTM) InfoS "Instances are not ready... waiting..."
    liftIO $ threadDelay (secs * 1000000)
    waitUntilInstancesAreRunning

-- | Create instances to run jobs on. Install Pleb.
createInstances :: (MonadAWS m, MonadIO m, MonadReader AppConfig m, MonadState AppState m, KatipContext m) => m ()
createInstances = do
  uuid    <- use sessionId
  --sgId    <- view sgGroupId <$> getCommanderSecurityGroup
  ami     <- view $ configFile . amiIdentifier
  snetId  <- view $ configFile . subnetIdentifier
  role    <- view $ configFile . iamRole
  keyName <- view $ configFile . keyPairName
  num     <- view $ configFile . numberOfInstances

  let iamr    = iamInstanceProfileSpecification & iapsName ?~ role
  let request = runInstances ami num num

  reservation <- send $ request -- & rSecurityGroupIds  <>~ [sgId]
                                & rKeyName            ?~ keyName
                                & rSubnetId           ?~ snetId
                                & rIAMInstanceProfile ?~ iamr
                                & rUserData           ?~ installPlebScript

  ec2Instances .= reservation ^. rInstances 
  waitUntilInstancesAreRunning
  updateInstances

  -- Need to give each instance a name tag

terminateInstancesInState :: (MonadAWS m, MonadIO m, MonadState AppState m, KatipContext m) => m ()
terminateInstancesInState = do
  instanceIds <- getInstanceIdsInState
  send $ terminateInstances & tiInstanceIds .~ instanceIds

  -- clear instance state
  ec2Instances .= mempty
