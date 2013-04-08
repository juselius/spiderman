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
import Lmodulator 
import Data.List
import Data.Char
import Data.Function (on)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Internal (text)
import Text.Blaze.Html.Renderer.Pretty 
import qualified Text.Pandoc as P
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

formatPackageList :: [Package] -> [Package]
formatPackageList p = map formatPackage p

formatPackage :: Package -> Package
formatPackage p = p 
    { moduleName = trimPackageName . package $ p
    , description = T.take 80 . description $ p
    , defaultVersion = formatVersion . getDefaultVersion $ p -- ugly
    , versionPageUrl = packageVersionUrl p
    , category  = T.toLower . category $ p
    , keywords = map T.toLower $ keywords p 
    , versions = HM.map formatVersion $ versions p
    } 
    
formatVersion v = v 
    { helpText = T.pack . renderHtml . rstToHtml . T.unpack . helpText $ v
    , helpPageUrl = packageHelpUrl v 
    } 

trimPackageName pkg =
    let p = T.takeWhile (/='/') . T.reverse $ pkg in
        if p == T.empty then "" else T.reverse p

-- | Create a version page for a package 
toVersionPage :: Package -> H.Html
toVersionPage p = do
    H.h1 . H.toHtml $ T.append "Package " (displayName p)
    H.table $ -- H.span ! A.class_ "version_table" $ 
        forM_ (HM.elems $ versions p) toVersionRow 

-- | Make a version infomation row for a version page
toVersionRow v = H.tr $ do 
    H.td $ H.toHtml vv
    H.td $ H.a ! A.href (H.toValue $ toUrl fn) $ 
        H.toHtml $ cleanPath fn
    where 
        vv = version v
        fn = fullName v

-- | Generate a help page for a package version
toHelpPage :: Version -> H.Html
toHelpPage v = do
    H.h1 . H.toHtml $ T.append "Module " t
    H.div $ -- ! A.class_ "help_page" $ 
        H.toHtml . rstToHtml . T.unpack . helpText $ v
    where t = cleanPath . fullName $ v
            
-- | Remove first part of a package path, up until first '/' 
cleanPath x 
    | T.any (=='/') x = T.tail . T.dropWhile (/='/') $ x
    | otherwise = x

toGitit p = "---\ntoc: no\ntitle:\n...\n\n" ++ p

-- | Convert a package/version path to a usable url name
toUrl :: T.Text -> T.Text
toUrl = T.toLower . T.replace "/" "."  

rstToHtml =  P.writeHtml P.def . P.readRST P.def 

htmlToRst =  P.writeRST P.def . P.readHtml P.def 

sortPackages p = formatPackageList . 
        sortBy (compare `on` T.toLower . displayName) $ p 

runListingTemplate t tit p = 
    setAttribute "pagetitle" tit $ 
    setAttribute "packages" p t
    
runVersionTemplate t p = 
    setAttribute "pagetitle" ("Package " `T.append` package p) $ 
    setAttribute "versions" (versions p) t

runHelpTemplate t v = 
    setAttribute "pagetitle" ("Module " `T.append` fullName v) $ 
    setAttribute "helptext" v t

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
    let Just t = getStringTemplate "package_list" templ in
    render $ runListingTemplate t tit p

renderVersionTemplate templ p = 
    let Just t = getStringTemplate "package_versions" templ in
    render $ runVersionTemplate t p

renderHelpTemplate templ v = 
    let Just t = getStringTemplate "package_help" templ in
    render $ runHelpTemplate t v

packageVersionUrl p = toUrl (package p)

packageHelpUrl v = toUrl (fullName v)

packageVersionFile p ext = (T.unpack $ toUrl (package p)) ++ ext

packageHelpFile v ext = (T.unpack $ toUrl (fullName v)) ++ ext

htmlReplaceMap :: [(T.Text, T.Text)]
htmlReplaceMap =  
    map packBoth  [   
          ("<", "&lt;")
        , (">", "&gt;")
        , ("\"", "&quot;")
        , ("\'", "&#39;")
        , ("&", "&amp;") ]
    where packBoth xy = (T.pack $ fst xy, T.pack $ snd xy)
 
escapeHtmlString :: T.Text -> T.Text
escapeHtmlString =
  foldl1 (.) $ map (uncurry T.replace) htmlReplaceMap

