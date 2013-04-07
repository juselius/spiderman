-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

-- | Parse Lmod JSON into more Haskell types.
--
-- The JSON can be generated from Lmod with:
--
-- @ $ \/path\/to\/spider -o softwarePage \/opt\/modulefiles > lmod.json@
module Lmodulator where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Data
import Data.List
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Lmod package representation
data Package = Package 
    { package :: T.Text
    , displayName :: T.Text
    , defaultVersion :: T.Text
    , description :: T.Text
    , url :: T.Text
    , license :: T.Text
    , category  :: T.Text
    , keywords :: [T.Text]
    , versions :: HM.HashMap T.Text Version
    } deriving (Eq, Show, Data, Typeable)
 
-- | Package version representation
data Version = Version 
    { version :: T.Text
    , fullName :: T.Text
    , helpText :: T.Text 
    } deriving (Eq, Show, Data, Typeable)

-- | Packages are packages.
newtype Packages = Packages {getPackages :: [Package]} deriving(Eq, Show)

instance FromJSON Packages where
  parseJSON (Array a) = do
    pack <- liftM V.toList $ V.mapM parseJSON a :: Parser [Package]
    return $ Packages pack
  parseJSON _ = mzero

instance FromJSON Package where
    parseJSON (Object o) = 
        Package <$> o .: "package"
        <*> o .: "displayName" 
        <*> o .: "defaultVersionName" 
        <*> o .:? "description" .!= "No description" 
        <*> o .:? "url" .!= ""
        <*> o .:? "license" .!= ""
        <*> liftM T.toLower (o .:? "categories" .!= "") 
        <*> liftM (map T.strip . T.splitOn (T.pack ",") . T.toLower) 
            (o .:? "keywords" .!= "")
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

unspace = T.filter (/=' ') 
