{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ConfLoader (Config(..), loadCfg) where

import Data.Maybe
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import System.IO
import Phys

data Config = Config{
  g::Double,
  dT::Double,
  debug::Bool,
  bhMode::Bool,
  scale::Double,
  bodies::[Phys.Body]
  } deriving (Show,Eq)

instance FromJSON Phys.Vec2D where
  parseJSON (Y.Object v) =
    Phys.Vec2D <$>
    v .:   "x" <*>
    v .:   "y"
  parseJSON _ = fail "Expected Object for Config value"


instance FromJSON Phys.Body where
  parseJSON (Y.Object v) =
    Phys.Body <$>
    v .:   "pos"  <*>
    v .:   "mass" <*>
    v .:   "mom"
  parseJSON _ = fail "Expected Object for Config value"


instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "g"  <*>
    v .:   "dT" <*>
    v .:   "debug" <*>
    v .:   "barnesHut" <*>
    v .:   "scale" <*>
    v .:   "bodies"
  parseJSON _ = fail "Expected Object for Config value"


loadCfg :: String -> IO(Config)
loadCfg fpath = do
  cfg <- Y.decodeFile fpath ::  IO (Maybe Config)
  return $ fromJust cfg
