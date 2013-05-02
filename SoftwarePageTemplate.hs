--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | Generate HTML from Lmod(ualtor) Packages using HStringTemplates.
module SoftwarePageTemplate (
      PageInfo(..) 
    , renderListingTemplate
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
    , packageVersionFileName
    , packageHelpFileName
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
import Text.Regex.Posix
import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM

data PageInfo = PageInfo
    { fname :: String
    , title :: String
    , ext :: String
    , templ :: STGroup T.Text
    }

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

renderHtmlListingTemplate page p = 
    let Just t = getStringTemplate "page" $ templ page in
    render $
        setAttribute "ext" (ext page) $ 
        runListingTemplate t (title page) p

renderHtmlVersionTemplate page p = 
    let Just t = getStringTemplate "page" $ templ page in
    render $ 
        setAttribute "ext" (ext page) $ 
        runVersionTemplate t p

renderHtmlHelpTemplate page v = 
    let Just t = getStringTemplate "page" $ templ page in
    render $ 
        setAttribute "ext" (ext page) $ 
        runHelpTemplate t v

renderListingTemplate page p = 
    let Just t = getStringTemplate "package" (templ page) in
    render $ runListingTemplate t (title page) p

renderVersionTemplate page p = 
    let Just t = getStringTemplate "package" (templ page) in
    render $ runVersionTemplate t p

renderHelpTemplate page v = 
    let Just t = getStringTemplate "package" (templ page) in
    render $ runHelpTemplate t v

packageVersionUrl p = toUrl (package p)

packageHelpUrl v = 
    let (a, b, c, g) = t =~ pat :: (String, String, String, [String]) in
    case g of
        [s, u] -> if s == "Site" then T.pack u else ""  
        [] -> toUrl (fullName v)  -- return url to help page to be generated
    where 
        t = T.unpack . helpText $ v
        pat = "(Site|Off-site) help:  *(http://[^ \t]*) *$" :: String

packageVersionFileName ext p= T.unpack (toUrl (package p)) ++ ext

packageHelpFileName ext v = T.unpack (toUrl (fullName v)) ++ ext

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

