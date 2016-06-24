{-# LANGUAGE OverloadedStrings #-}
module Commander.EC2.SecurityGroup where

import Control.Lens

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

checkToSeeIfSecurityGroupNameTagExists :: (MonadAWS m) => Text -> m (Maybe SecurityGroup)
checkToSeeIfSecurityGroupNameTagExists = undefined

createCommanderSecurityGroup :: (MonadAWS m) => m SecurityGroup
createCommanderSecurityGroup = undefined

getCommanderSecurityGroup :: (MonadAWS m) => m SecurityGroup
getCommanderSecurityGroup = createGroupIfNotExists =<< checkToSeeIfSecurityGroupNameTagExists "CommanderSG"
  where
    createGroupIfNotExists :: (MonadAWS m) => Maybe SecurityGroup -> m SecurityGroup
    createGroupIfNotExists (Just sg) = return sg
    createGroupIfNotExists _         = createCommanderSecurityGroup
