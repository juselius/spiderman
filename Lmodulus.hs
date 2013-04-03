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
    , categories  :: T.Text
    , defaultVersion :: T.Text
    , description :: T.Text
    , keywords :: T.Text
    , url :: T.Text
    , displayName :: T.Text
--     , versions :: [Version]
    } deriving (Eq, Show)
 
data Versions = Version 
    { fullName :: T.Text
    , helpText :: T.Text 
    , version :: T.Text
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
        <*> o .:? "categories" .!= "Unknown" 
        <*> o .: "defaultVersionName" 
        <*> o .:? "description" .!= "No description" 
        <*> o .:? "keywords" .!= "" 
        <*> o .:? "url" .!= "" 
        <*> o .: "displayName" 
--     parseJSON (Object o) = do 
--         p <- o .: "package" 
--         Array a <- o .: "Versions"
--         return $ Package (T.pack p)
    parseJSON _ = mzero 

lason ason = (BS.pack "{\"packages\":") `BS.append` ason `BS.append` (BS.pack "}")

main = do
    args <- Env.getArgs
    ason <- BS.readFile (head args)
    print $ (decode (ason) :: Maybe Packages)
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)


