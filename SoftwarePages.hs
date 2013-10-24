--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate HTML from Lmod(ualtor) packageList using HStringTemplates.
module SoftwarePages (
      Page(..) 
    , renderIndexPage
    , renderVersionPage
    , renderHelpPage
    , formatPackage
    , formatPackageList
    , sortPackageList
    , rstToHtml
    , htmlToRst
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
import LmodPackage 
import Paths_spiderman

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
        , versionList :: [Version]
        }

data Route = Home
    | BaseCss
    | PrintCss
    | Base T.Text
    | Images T.Text

renderUrl :: Route -> [(T.Text, T.Text)] -> T.Text
renderUrl Home _ = "/"
renderUrl BaseCss _ = "/css/custom.css"
renderUrl PrintCss _ = "/css/print.css"
renderUrl (Images x) _ = "/img/" `T.append` x 
renderUrl (Base x) _  = x
--     | "http" `T.isPrefixOf` x = x
--     | otherwise = "software/" `T.append` x 

formatPackageList :: [Package] -> [Package]
formatPackageList ps = map formatPackage ps

formatPackage :: Package -> Package
formatPackage p = p { 
      moduleName = trimPackageName . packageName $ p
    , defaultVersion = formatVersion $ getDefaultVersion p 
    , packageIndexName = urlify $ packageName p 
    , category  = T.toLower . category $ p
    , keywords = map T.toLower $ keywords p 
    , versions = HM.map formatVersion $ versions p
    }
    
formatVersion v = v { 
      helpText = T.pack . rstToHtml . T.unpack . helpText $ v
    , helpPageHref = helpPageName v 
    } 
        
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

tabs = "This is a tab" :: T.Text

pageTemplate :: Page -> Html -> HtmlUrl Route
pageTemplate page content = $(hamletFile "templates/page.hamlet")

packageIndexTemplate :: (T.Text -> T.Text) -> Page -> HtmlUrl Route
packageIndexTemplate fileT page = $(hamletFile "templates/packages.hamlet")
-- helpTemplate = $(hamletFile "templates/help.hamlet")
-- versionsTemplate = $(hamletFile "templates/versions.hamlet")

logo = $(hamletFile "templates/logo.hamlet")
footer = $(hamletFile "templates/footer.hamlet")

header :: T.Text -> HtmlUrl Route
header title = $(hamletFile "templates/header.hamlet")

sitenav = $(hamletFile "templates/sitenav.hamlet")

renderIndexPage page = 
    T.pack . renderHtml $ pageTemplate page indexP renderUrl
    where 
        indexP = packageIndexTemplate 
            (\f -> f `T.append` ".html") page renderUrl
    
renderVersionPage page = 
    T.pack "version"

renderHelpPage page v = 
    "help"

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

