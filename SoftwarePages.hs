--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate HTML from Lmod packageList using Hamlet
module SoftwarePages (
      Page(..) 
    , renderPage
    , formatPackage
    , formatPage
    , sortPackageList
    , rstToHtml
    , htmlToRst
    , urlify
    ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Char
import Data.Function (on)
import Text.Regex.Posix
import Text.Blaze.Html (Html, toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Hamlet (hamlet, hamletFile, HtmlUrl)
import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Paths_spiderman as Paths
import LmodPackage 

data Page = 
      IndexPage { 
          pageTitle :: T.Text
        , pageName :: T.Text
        , packageList :: [Package]
        }
    | VersionPage { 
          pageTitle :: T.Text
        , pageName :: T.Text
        , package :: Package
        }
    | HelpPage {
          pageTitle :: T.Text
        , pageName :: T.Text
        , package :: Package
        , versionInfo :: Version
        }

data Route = Home
    | BaseCss
    | PrintCss
    | Base T.Text
    | Images T.Text

type FileNameFormatter = T.Text -> T.Text 

-- Routes should be configurable from a file
renderUrl :: Route -> [(T.Text, T.Text)] -> T.Text
renderUrl Home _ = "/"
renderUrl BaseCss _ = "css/custom.css"
renderUrl PrintCss _ = "css/print.css"
renderUrl (Images x) _ = "img/" `T.append` x 
renderUrl (Base x) _ 
    | "http" `T.isPrefixOf` x = x
    | otherwise = x 

formatPage :: FileNameFormatter-> Page -> Page
formatPage f page = case page of
    IndexPage _ pn ps -> page {
          pageName = f pn
        , packageList = map (formatPackage f) ps }
    VersionPage _ pn p -> page { 
          pageName = f pn
        , package = formatPackage f p }
    HelpPage _ pn p v -> page { 
          pageName = f pn
        , package = formatPackage f p
        , versionInfo = formatVersion f v }

formatPackage :: FileNameFormatter -> Package -> Package
formatPackage f p = p { 
      moduleName = trimPackageName . packageName $ p
    , defaultVersion = formatVersion f $ getDefaultVersion p 
    , packageIndexName = f $ packageName p 
    , category  = T.toLower . category $ p
    , keywords = map T.toLower $ keywords p 
    , versions = HM.map (formatVersion f) $ versions p
    }
    
formatVersion :: FileNameFormatter -> Version -> Version
formatVersion f v = v { 
      helpText = T.pack . rstToHtml . T.unpack . helpText $ v
    , helpPageHref = x
    } 
    where 
        p = helpPageName v 
        x   | "http" `T.isPrefixOf` p = p
            | not $ T.null p = f p
            | otherwise = p 

-- | Render the toplevel page template w/ contents
renderPage page = 
    T.pack . renderHtml $ pageTemplate page contents renderUrl
    where contents = case page of
            IndexPage   _ _ _   -> packageIndexTemplate page renderUrl
            VersionPage _ _ p   -> versionListTemplate page p renderUrl
            HelpPage    _ _ p v -> helpTemplate p v renderUrl
 
pageTemplate :: Page -> Html -> HtmlUrl Route
pageTemplate page content = $(hamletFile "templates/page.hamlet")

packageIndexTemplate :: Page -> HtmlUrl Route
packageIndexTemplate page = $(hamletFile "templates/packages.hamlet")

versionListTemplate :: Page -> Package -> HtmlUrl Route
versionListTemplate page pkg = $(hamletFile "templates/versions.hamlet")

helpTemplate :: Package -> Version -> HtmlUrl Route
helpTemplate pkg ver = $(hamletFile "templates/help.hamlet")

header :: T.Text -> HtmlUrl Route
header title = $(hamletFile "templates/header.hamlet")

sitenav = $(hamletFile "templates/sitenav.hamlet")

logo = $(hamletFile "templates/logo.hamlet")

footer = $(hamletFile "templates/footer.hamlet")

tabs = "This is a tab" :: T.Text

trimPackageName pkg =
    let p = T.takeWhile (/= '/') . T.reverse $ pkg in
        if p == T.empty then "" else T.reverse p

-- | Remove first part of a package path, up until first '/' 
cleanPath x 
    | T.any (=='/') x = T.tail . T.dropWhile (/='/') $ x
    | otherwise = x

addGititHeaders p = "---\ntoc: no\ntitle:\n...\n\n" `T.append` p

-- | Convert a package/version path to a usable url name
urlify = T.toLower . T.replace "/" "." 

rstToHtml =  P.writeHtmlString P.def . P.readRST P.def 

htmlToRst =  T.pack . P.writeRST P.def . P.readHtml P.def . T.unpack

sortPackageList = sortBy (compare `on` T.toLower . displayName)  

helpPageName v = 
    let (a, b, c, g) = t =~ pat :: (String, String, String, [String]) in
    case g of
        [s, u] -> if s == "Site" then T.pack u else ""  
        [s, a1, u, a2] -> if s == "Site" then T.pack u else ""  
        -- return url to help page to be generated
        [] -> urlify $ fullName v
    where 
        t = T.unpack . helpText $ v
        pat = "(Site|Off-site) help:" ++ 
            "*(<a .*>)* *(http://[^ \t]*) *(</a>)*$" :: String

