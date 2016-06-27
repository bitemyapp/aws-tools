{-# LANGUAGE OverloadedStrings #-}
module Commander.EC2.SecurityGroup where

import Control.Lens

import Data.Maybe

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

commanderSGName = "CommanderSG"

checkToSeeIfSecurityGroupNameExists :: (MonadAWS m) => Text -> m (Maybe SecurityGroup)
checkToSeeIfSecurityGroupNameExists name = do
  response <- send $ describeSecurityGroups
  let sgroups :: [SecurityGroup]
      sgroups = response ^. dsgrsSecurityGroups
  return . maybeGetMatching $ sgroups
  where
    maybeGetMatching = listToMaybe . filter (\sg -> name == sg ^. sgGroupName)

createCommanderSecurityGroup :: (MonadAWS m) => Text -> m Text
createCommanderSecurityGroup name = do
  response <- send $ createSecurityGroup name ""
  return $ response ^. csgrsGroupId

getCommanderSecurityGroupId :: (MonadAWS m) => m Text
getCommanderSecurityGroupId = createGroupIfNotExists =<< checkToSeeIfSecurityGroupNameExists commanderSGName 
  where
    createGroupIfNotExists :: (MonadAWS m) => Maybe SecurityGroup -> m Text
    createGroupIfNotExists (Just sg) = return $ sg ^. sgGroupId
    createGroupIfNotExists _         = createCommanderSecurityGroup commanderSGName
