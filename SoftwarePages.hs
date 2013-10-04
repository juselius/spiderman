--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | Generate HTML from Lmod(ualtor) Packages using HStringTemplates.
module SoftwarePages (
      PageInfo(..) 
    , renderListingTemplate
    , renderVersionTemplate
    , renderHelpTemplate
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
import LmodPackage 
import Data.List
import Data.Char
import Data.Function (on)
import Text.Regex.Posix
import Text.Blaze.Html.Renderer.Pretty 
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM

data PageInfo = PageInfo
    { title :: String
    , ext :: String
    , mainTemplate :: StringTemplate T.Text
    , pkg :: Package
    }

formatPackageList :: PageInfo -> [Package] -> [PageInfo]
formatPackageList page = map (\p -> formatPackage page { pkg = p})

formatPackage :: PageInfo -> PageInfo
formatPackage page = page { pkg = p
    { moduleName = trimPackageName . package $ p
--     , description = T.take 80 . description $ p
    , defaultVersion = formatVersion page defaultVersion
    , versionPageUrl = packageVersionUrl p `T.append` T.pack (ext page)
    , category  = T.toLower . category $ p
    , keywords = map T.toLower $ keywords p 
    , versions = HM.map (formatVersion page) $ versions p
    }}
    where 
        p = pkg page
        defaultVersion = getDefaultVersion p 
    
formatVersion page v = v 
    { helpText = T.pack . rstToHtml . T.unpack . helpText $ v
    , helpPageHref = if url == "" 
        then ""
        else "href=\"" `T.append` url `T.append` "\""
    } 
    where 
        url = packageHelpUrl page v 

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

sortPackages = sortBy (compare `on` T.toLower . displayName)  

runListingTemplate pages = 
    setAttribute "pagetitle" tit $ 
    setAttribute "packages" p t
    where 
        t = mainTemplate (head pages)
        tit = title (head pages)
        p = map pkg pages
    
runVersionTemplate page = 
    setAttribute "pagetitle" ("Package " `T.append` package p) $ 
    setAttribute "versions" (versions p) $
    setAttribute "keywords" (keywords p) t 
    where 
        t = mainTemplate page
        p = pkg page

runHelpTemplate page v = 
    setAttribute "pagetitle" ("Module " `T.append` fullName v) $ 
    setAttribute "helptext" (helpText v) t
    where 
        t = mainTemplate page

renderListingTemplate pages = render $ runListingTemplate pages

renderVersionTemplate pages = render $ runVersionTemplate pages 

renderHelpTemplate pages v = render $ runHelpTemplate pages v

packageVersionUrl p = toUrl (package p)

packageHelpUrl page v = 
    let (a, b, c, g) = t =~ pat :: (String, String, String, [String]) in
    case g of
        [s, u] -> if s == "Site" then T.pack u else ""  
        [s, a1, u, a2] -> if s == "Site" then T.pack u else ""  
        -- return url to help page to be generated
        [] -> toUrl (fullName v) `T.append` T.pack (ext page)
    where 
        t = T.unpack . helpText $ v
        pat = "(Site|Off-site) help:" ++ 
            "*(<a .*>)* *(http://[^ \t]*) *(</a>)*$" :: String

-- packageHelpUrl v = 
--     let (a, b, c , d) = helpText v =~ "See  *(http://[^ \t]*)" :: 
--         (String,String,String,[String]) in
--     toUrl (fullName v)

packageVersionFileName page = 
    T.unpack (toUrl (package p)) ++ ext page
    where p = pkg page

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

