-- (c) jonas.juselius@uit.no, 2013
-- | Parse Lmod JSON into more Haskell types.
--
--
-- The JSON can be generated from Lmod with:
--
-- @ $ \/path\/to\/spider -o softwarePage \/opt\/modulefiles > lmod.json@
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module LmodPackage where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Data
import Data.List
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Lmod package representation
data Package = Package
    { packageName :: T.Text
    , displayName :: T.Text
    , defaultVersionName :: T.Text
    , description :: T.Text
    , infoUrl :: T.Text
    , license :: T.Text
    , category  :: T.Text
    , moduleName :: T.Text
    , packageIndexName :: T.Text
    , keywords :: [T.Text]
    , versions :: HM.HashMap T.Text Version
    , defaultVersion :: Version
    } deriving (Eq, Show, Data, Typeable)

-- | Package version representation
data Version = Version
    { versionSpec :: T.Text
    , versionName :: T.Text
    , helpText :: T.Text
    , helpPageHref :: T.Text
    } deriving (Eq, Show, Data, Typeable)

type RecNo = Int

-- | Packages are packages.
data Packages = Packages
    { packages :: HM.HashMap T.Text Package
    , failures :: [(RecNo, String)]
    } deriving(Eq, Show)

instance FromJSON Packages where
  parseJSON (Array a) = do
    pack <- return $ V.toList (V.map fromJSON a) :: Parser [Result Package]
    let good = HM.fromList $ map (\(Success x) ->
            (packageName x, x)) $ filter isSuccess pack
        bad = map (\(n, Error x) -> (n, x)) $ filter isFailed (zip [1..] pack)
    return $ Packages good bad
  parseJSON _ = mzero

isFailed (_, Error _) = True
isFailed _ = False

isSuccess (Success _) = True
isSuccess (Error _) = False

instance FromJSON Package where
    parseJSON (Object o) = Package
        <$> o .: "package"
        <*> o .: "displayName"
        <*> o .: "defaultVersionName"
        <*> o .:? "description" .!= "No description"
        <*> o .:? "url" .!= ""
        <*> o .:? "license" .!= ""
        <*> liftA T.toLower (o .:? "categories" .!= "")
        <*> pure ""
        <*> pure ""
        <*> liftA tok (o .:? "keywords" .!= "")
        <*> liftA tov (o .: "versions")
        <*> pure emptyVersion
        where
            tok = map T.strip . T.splitOn (T.pack ",") . T.toLower
            tov = HM.fromList . V.toList . V.map (\x ->(versionSpec x, x))
    parseJSON _ = mzero

instance FromJSON Version where
    parseJSON (Object o) =
        Version <$> o .: "versionName"
        <*> liftA T.toLower (o .: "full")
        <*> o .:? "help" .!= ""
        <*> return ""

emptyPackage = Package T.empty T.empty T.empty T.empty T.empty
    T.empty T.empty T.empty T.empty [T.empty]
    (HM.fromList [(T.empty, emptyVersion)]) emptyVersion

emptyVersion = Version T.empty T.empty T.empty T.empty

-- | Fetch the default Version object from a package
getDefaultVersion p =
    let x = HM.lookup (defaultVersionName p) (versions p) in
    fromMaybe emptyVersion x

unspace = T.filter (/=' ')
