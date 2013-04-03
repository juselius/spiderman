--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE OverloadedStrings #-}

module Lmodulator where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Vector as V

data Package = Package 
    { name :: T.Text
    , categories  :: T.Text
    , defaultVersion :: T.Text
    , description :: T.Text
    , keywords :: T.Text
    , url :: T.Text
    , displayName :: T.Text
    , versions :: HM.HashMap T.Text Version
    } deriving (Eq, Show)
 
data Version = Version 
    { version :: T.Text
    , fullName :: T.Text
    , helpText :: T.Text 
    } deriving (Eq, Show)

newtype Packages = Packages {getPackages :: [Package]} deriving(Eq, Show)

instance FromJSON Packages where
  parseJSON (Array a) = do
    pack <- liftM V.toList $ V.mapM parseJSON a :: Parser [Package]
    return $ Packages pack
  parseJSON _ = mzero

instance FromJSON Package where
    parseJSON (Object o) = 
        Package <$> o .: "package"
        <*> o .:? "categories" .!= ""
        <*> o .: "defaultVersionName" 
        <*> o .:? "description" .!= "No description" 
        <*> o .:? "keywords" .!= ""
        <*> o .:? "url" .!= ""
        <*> o .: "displayName" 
        <*> do  
            v <- o .: "versions" 
            vl <- liftM V.toList $ V.mapM parseJSON v :: Parser [Version]
            return $ HM.fromList $ map (\x -> 
                (version x, x)) vl :: Parser (HM.HashMap T.Text Version)
    parseJSON _ = mzero 

instance FromJSON Version where
    parseJSON (Object o) = 
        Version <$> o .: "versionName"
        <*> o .: "full"
        <*> o .:? "help" .!= ""

