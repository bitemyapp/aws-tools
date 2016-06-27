{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Commander.EC2.SecurityGroup where

import Control.Lens
import Control.Monad.Reader

import Data.Maybe

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

import Commander.Types

commanderSGName = "CommanderSG"

openInboundPort :: Text -> Int -> IPPermission
openInboundPort c p = 
  ipPermission "tcp" & ipFromPort ?~ p
                     & ipToPort   ?~ p
                     & ipIPRanges .~ [ipRange c]

checkToSeeIfSecurityGroupNameExists :: (MonadAWS m) => Text -> m (Maybe SecurityGroup)
checkToSeeIfSecurityGroupNameExists name = do
  response <- send $ describeSecurityGroups
  let sgroups :: [SecurityGroup]
      sgroups = response ^. dsgrsSecurityGroups
  return . maybeGetMatching $ sgroups
  where
    maybeGetMatching = listToMaybe . filter (\sg -> name == sg ^. sgGroupName)

createCommanderSecurityGroup :: (MonadAWS m, MonadReader AppConfig m) => Text -> m Text
createCommanderSecurityGroup name = do
  cidr     <- view $ configFile . awsSGCidr
  port     <- view $ configFile . awsSGPort
  response <- send $ createSecurityGroup name ""

  let groupId       = response ^. csgrsGroupId
      ipPermissions = openInboundPort cidr port : []

  -- Attach permissions
                                       
  send $ authorizeSecurityGroupIngress & asgiGroupId       ?~ groupId
                                       & asgiFromPort      ?~ port
                                       & asgiToPort        ?~ port
                                       & asgiCIdRIP        ?~ cidr
                                       & asgiIPProtocol    ?~ "tcp"
                                       & asgiIPPermissions .~ ipPermissions

  return groupId

getCommanderSecurityGroupId :: (MonadAWS m, MonadReader AppConfig m) => m Text
getCommanderSecurityGroupId = createGroupIfNotExists =<< checkToSeeIfSecurityGroupNameExists commanderSGName 
  where
    createGroupIfNotExists :: (MonadAWS m, MonadReader AppConfig m) => Maybe SecurityGroup -> m Text
    createGroupIfNotExists (Just sg) = return $ sg ^. sgGroupId
    createGroupIfNotExists _         = createCommanderSecurityGroup commanderSGName
