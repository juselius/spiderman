--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | Generate HTML from Lmod(ualtor) Packages.
module SoftwarePage (
      renderListingPage
    , renderVersionPage
    , renderHelpPage
    , toLinkString
    , toGitit
    , rstToHtml
    , htmlToRst
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
import qualified Data.HashMap.Strict as HM

getDefaultHelpText :: Package -> H.Html
getDefaultHelpText x = 
    case HM.lookup (defaultVersion x) (versions x) of
    Just v -> H.a . H.toHtml . helpText $ v
    otherwise -> text ""

-- | Generate a standalone html page from a body
toPage :: T.Text -> H.Html -> H.Html
toPage t b = H.docTypeHtml $ do
    H.head $ do 
        H.title (H.toHtml t)
        H.link ! A.href "lmod.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.body b 

-- | Generate a Package table from a list of packages w/ a title
toListingPage :: T.Text -> [Package] -> H.Html
toListingPage t p = toPage t  $ do
        H.h1 . H.toHtml $ t
        H.table $ -- ! A.class_ "listing_table" $ 
            forM_ sortP toListingRow
    where sortP = sortBy (compare `on` T.toLower . displayName) p

-- | Pick out a non-failing value from a Maybe
extract (Just x) = x

-- | Fetch the default Version object from a package
getDefaultVersion p = extract $ HM.lookup (defaultVersion p) (versions p)

-- | Generate a Package row in the listing table
toListingRow :: Package -> H.Html
toListingRow x = H.tr $ do 
    H.td name
    H.td pkg
    H.td ver
    H.td . H.toHtml . T.intercalate ", " . keywords $ x
    H.td desc 
    where 
        name = H.a ! A.href (H.toValue . url $ x) $ H.toHtml . displayName $ x
        pkg = let p = takeWhile (/='/') . reverse . T.unpack . package $ x in
            if null p then 
                "" 
            else 
                H.a ! A.href (H.toValue . toLinkName 
                    . fullName . getDefaultVersion $ x) $ 
                    H.toHtml $ reverse p
        ver = H.a ! A.href (H.toValue . toLinkName . package $ x) $ 
            H.toHtml $ defaultVersion x
        desc = H.toHtml . T.take 80 . description $ x
        
-- | Create a version page for a package 
toVersionPage :: Package -> H.Html
toVersionPage p = toPage (displayName p) $ do
    H.h1 . H.toHtml $ T.append "Package " (displayName p)
    H.table $ -- H.span ! A.class_ "version_table" $ 
        forM_ (HM.elems $ versions p) toVersionRow 

-- | Make a version infomation row for a version page
toVersionRow v = H.tr $ do 
    H.td $ H.toHtml vv
    H.td $ H.a ! A.href (H.toValue $ toLinkName fn) $ 
        H.toHtml $ cleanPath fn
    where 
        vv = version v
        fn = fullName v

-- | Generate a help page for a package version
toHelpPage :: Version -> H.Html
toHelpPage v = toPage t $ do
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
toLinkName :: T.Text -> T.Text
toLinkName = T.toLower . T.replace "/" "."  

toLinkString :: T.Text -> String
toLinkString = T.unpack . toLinkName 

toHtmlLinkName :: T.Text -> T.Text
toHtmlLinkName p = toLinkName p `T.append` ".html"

rstToHtml =  P.writeHtml P.def . P.readRST P.def 

htmlToRst =  P.writeRST P.def . P.readHtml P.def 

renderListingPage t pkgs = renderHtml . toListingPage (T.pack t) $ pkgs

renderVersionPage p = renderHtml . toVersionPage $ p

renderHelpPage v = renderHtml . toHelpPage $ v

