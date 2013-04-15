--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | Generate HTML from Lmod(ualtor) Packages using HStringTemplates.
module SoftwarePageTemplate (
      renderListingTemplate
    , renderVersionTemplate
    , renderHelpTemplate
    , renderHtmlListingTemplate
    , renderHtmlVersionTemplate
    , renderHtmlHelpTemplate
    , formatPackageList
    , sortPackages
    , toUrl
    , toGitit
    , rstToHtml
    , htmlToRst
    , packageVersionUrl
    , packageHelpUrl
    , packageVersionFile
    , packageHelpFile
    ) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Lmodulator 
import Data.List
import Data.Char
import Data.Function (on)
import Text.Blaze.Html.Renderer.Pretty 
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM

formatPackageList :: [Package] -> [Package]
formatPackageList = map formatPackage 

formatPackage :: Package -> Package
formatPackage p = p 
    { moduleName = trimPackageName . package $ p
--     , description = T.take 80 . description $ p
    , defaultVersion = formatVersion . getDefaultVersion $ p -- ugly
    , versionPageUrl = packageVersionUrl p
    , category  = T.toLower . category $ p
    , keywords = map T.toLower $ keywords p 
    , versions = HM.map formatVersion $ versions p
    } 
    
formatVersion v = v 
    { helpText = T.pack . rstToHtml . T.unpack . helpText $ v
    , helpPageUrl = packageHelpUrl v 
    } 

trimPackageName pkg =
    let p = T.takeWhile (/='/') . T.reverse $ pkg in
        if p == T.empty then "" else T.reverse p

-- | Remove first part of a package path, up until first '/' 
cleanPath x 
    | T.any (=='/') x = T.tail . T.dropWhile (/='/') $ x
    | otherwise = x

toGitit p = "---\ntoc: no\ntitle:\n...\n\n" `T.append` p

-- | Convert a package/version path to a usable url name
toUrl :: T.Text -> T.Text
toUrl = T.toLower . T.replace "/" "."  

rstToHtml =  P.writeHtmlString P.def . P.readRST P.def 

htmlToRst =  T.pack . P.writeRST P.def . P.readHtml P.def . T.unpack

sortPackages p = sortBy (compare `on` T.toLower . displayName) $ p 

runListingTemplate t tit p = 
    setAttribute "pagetitle" tit $ 
    setAttribute "packages" p t
    
runVersionTemplate t p = 
    setAttribute "pagetitle" ("Package " `T.append` package p) $ 
    setAttribute "versions" (versions p) $
    setAttribute "keywords" (keywords p) t 

runHelpTemplate t v = 
    setAttribute "pagetitle" ("Module " `T.append` fullName v) $ 
    setAttribute "helptext" (helpText v) t

renderHtmlListingTemplate templ tit p = 
    let Just t = getStringTemplate "page" templ in
    render $
        setAttribute "ext" (".html" :: String) $ 
        runListingTemplate t tit p

renderHtmlVersionTemplate templ p = 
    let Just t = getStringTemplate "page" templ in
    render $ 
        setAttribute "ext" (".html" :: String) $ 
        runVersionTemplate t p

renderHtmlHelpTemplate templ v = 
    let Just t = getStringTemplate "page" templ in
    render $ 
        setAttribute "ext" (".html" :: String) $ 
        runHelpTemplate t v

renderListingTemplate templ tit p = 
    let Just t = getStringTemplate "package" templ in
    render $ runListingTemplate t tit p

renderVersionTemplate templ p = 
    let Just t = getStringTemplate "package" templ in
    render $ runVersionTemplate t p

renderHelpTemplate templ v = 
    let Just t = getStringTemplate "package" templ in
    render $ runHelpTemplate t v

packageVersionUrl p = toUrl (package p)

packageHelpUrl v = toUrl (fullName v)

packageVersionFile ext p= T.unpack (toUrl (package p)) ++ ext

packageHelpFile ext v = T.unpack (toUrl (fullName v)) ++ ext

htmlReplaceMap :: [(T.Text, T.Text)]
htmlReplaceMap =  
    map packBoth  [   
          ("<", "&lt;")
        , (">", "&gt;")
        , ("\"", "&quot;")
        , ("\'", "&#39;")
        , ("&", "&amp;") ]
    where packBoth = T.pack Control.Arrow.*** T.pack
 
escapeHtmlString :: T.Text -> T.Text
escapeHtmlString =
  foldl1 (.) $ map (uncurry T.replace) htmlReplaceMap

