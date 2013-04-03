--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
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
    , versions :: HM.HashMap T.Text Version
    } deriving (Eq, Show)
 
data Version = Version 
    { version :: T.Text
    , fullName :: T.Text
    , helpText :: Maybe T.Text 
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
        <*> o .:? "categories" 
        <*> o .: "defaultVersionName" 
        <*> o .:? "description" .!= "No description" 
        <*> o .:? "keywords" 
        <*> o .:? "url" 
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
        <*> o .:? "help"

nameOrURL :: Package -> String
nameOrURL x = case url x of
    Just u -> "<a href=" ++ T.unpack u ++ ">" 
        ++ T.unpack (name x) ++ "</a>"
    otherwise -> T.unpack (name x)


extractText :: Maybe T.Text -> String
extractText (Just t) = T.unpack t
extractText _ = mzero

getDefaultHelpText :: Package -> String
getDefaultHelpText x = 
    case HM.lookup (defaultVersion x) (versions x) of
        Just v -> 
            "<a href=" ++ extractText (helpText v) ++ ">" 
            ++ T.unpack (name x) ++ "</a>"
        otherwise -> mzero

toListHTML x = "<tr>" 
        ++ "<td> " ++ nameOrURL x ++ " </td>"
        ++ "<td> " ++ extractText (keywords x) ++ " </td>"
        ++ "<td> " ++ T.unpack (defaultVersion x) ++ " </td>"
        ++ "<td> " ++ T.unpack (description x) ++ " </td>"
        ++ "<td> " ++ getDefaultHelpText x ++ " </td>"
        ++ "</tr>"

main = do
    args <- Env.getArgs
    ason <- BS.readFile (head args)
    putStrLn "<table>"
    case (decode ason :: Maybe Packages) of
        Just x -> putStrLn $ unlines (map toListHTML (getPackages x))
        otherwise -> putStrLn "damn. failed."
    putStrLn "</table>"
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)


