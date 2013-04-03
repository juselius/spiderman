{-# LANGUAGE OverloadedStrings #-}

module Lmodulus where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Environment as Env

data Package = Package 
    { name :: T.Text
    , categories  :: Maybe T.Text
    , defaultVersion :: T.Text
    , description :: T.Text
    , keywords :: Maybe T.Text
    , url :: Maybe T.Text
    , displayName :: T.Text
    , versions :: [Version]
    } deriving (Eq, Show)
 
data Version = Version 
    { version :: T.Text
    , fullName :: T.Text
    , helpText :: Maybe T.Text 
    } deriving (Eq, Show)

newtype Packages = Packages [Package] deriving(Eq, Show)

instance FromJSON Packages where
  parseJSON (Array a) = do
    pack <- (liftM V.toList) $ V.mapM parseJSON a :: Parser ([Package])
    return $ Packages pack
  parseJSON _ = mzero

instance FromJSON Package where
    parseJSON (Object o) = 
        Package <$> o .: "package"
        <*> o .:? "categories" 
        <*> o .: "defaultVersionName" 
        <*> o .:? "description" .!= "No description" 
        <*> o .:? "keywords" 
        <*> o .:? "url" 
        <*> o .: "displayName" 
--         <*> ((liftM V.toList) <$> V.mapM parseJSON v :: Parser ([Version]))
        <*> do  
            v <- o .: "versions" 
            (liftM V.toList) $ V.mapM parseJSON v :: Parser ([Version])
    parseJSON _ = mzero 

instance FromJSON Version where
    parseJSON (Object o) = 
        Version <$> o .: "versionName"
        <*> o .: "full"
        <*> o .:? "help"

main = do
    args <- Env.getArgs
    ason <- BS.readFile (head args)
    print $ (decode (ason) :: Maybe Packages)
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)


