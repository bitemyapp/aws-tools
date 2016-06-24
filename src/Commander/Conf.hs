{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commander.Conf where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Except


import Data.ConfigFile
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes)
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import System.Directory
import System.Exit
import System.IO
import System.IO.Error

import Debug.Trace

import Katip

import Commander.Types


configPaths :: [FilePath]
configPaths = ["./commander.conf", "/etc/commander/conf"]

readInConfig :: (MonadIO m, MonadError CPError m) => ConfigParser -> FilePath -> m ConfigParser
readInConfig def h = join . liftIO $ readfile def h

convertCPErrors :: CPError -> ConfigError
convertCPErrors (err, str) = ConfigurationCouldNotParseError $ Text.pack str <> " :: " <> buildErr err
  where
    buildErr :: CPErrorData -> Text
    buildErr (ParseError str)           = "ParseError: "             <> Text.pack str
    buildErr (SectionAlreadyExists str) = "Section already exists: " <> Text.pack str
    buildErr (NoSection str)            = "No Section: "             <> Text.pack str
    buildErr (NoOption str)             = "No Option: "              <> Text.pack str
    buildErr (OtherProblem str)         = "Other Problem: "          <> Text.pack str
    buildErr (InterpolationError str)   = "Interpolation Error: "    <> Text.pack str

buildConfigFromPaths :: (MonadIO m) => [FilePath] -> m (Either ConfigError ConfigFile)
buildConfigFromPaths [] = return $ Left NoConfigurationFilesFoundError
buildConfigFromPaths fp = convertErrors <$> runErrorT tryParsing
  where
    convertErrors :: Either CPError ConfigFile -> Either ConfigError ConfigFile
    convertErrors = either (Left . convertCPErrors) (Right)
    tryParsing = do
      cp  <- foldlM (readInConfig) emptyCP fp
      ConfigFile <$> get cp "DEFAULT" "number_of_instances"
                 <*> get cp "DEFAULT" "wait_to_running_sec"
                 <*> get cp "DEFAULT" "key_pair_name"
                 <*> get cp "DEFAULT" "ami"
                 <*> get cp "DEFAULT" "subnet"
                 <*> get cp "DEFAULT" "instance_type"
                 <*> get cp "DEFAULT" "iam_role"

loadConfig :: (MonadIO m) => m (Either ConfigError ConfigFile)
loadConfig = do
  homeConf <- liftIO $ (++ "/.commander.conf") <$> getUserDocumentsDirectory
  paths    <- liftIO $ filterM doesFileExist $ homeConf : configPaths 
  buildConfigFromPaths paths

getConfigOrExit :: IO ConfigFile
getConfigOrExit = either (exit) return =<< loadConfig 
  where
    exit :: ConfigError -> IO ConfigFile
    exit (ConfigurationCouldNotParseError str) = do
      Text.hPutStrLn stderr str
      exitWith (ExitFailure 1)
