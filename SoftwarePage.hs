--
-- Generate HTML from Lmod Packages
--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module SoftwarePage (
      toListingPage
    , toVersionPage
    , toHelpPage
    , toHtmlFileName
    ) where

import Control.Applicative
import Control.Monad
import Lmodulator 
import Data.List
import Data.Char
import Data.Function (on)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Internal (text)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

getDefaultHelpText :: Package -> H.Html
getDefaultHelpText x = 
    case HM.lookup (defaultVersion x) (versions x) of
    Just v -> H.a . H.toHtml . helpText $ v
    otherwise -> text ""

toPage :: String -> H.Html -> H.Html
toPage t b = H.docTypeHtml $ do
    H.head $ do 
        H.title (H.toHtml t)
        H.link ! A.href "lmod.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.body b 

toListingPage :: String -> [Package] -> H.Html
toListingPage t p = toPage t  $ do
        H.table $ H.span ! A.class_ "listing_table" $ forM_ sortP toListingRow
    where sortP = sortBy (compare `on` T.toLower . displayName) p

extract (Just x) = x

getDefaultVersion p = extract $ HM.lookup (defaultVersion p) (versions p)

toListingRow :: Package -> H.Html
toListingRow x = H.tr $ do 
    H.td $ name
    H.td $ pkg
    H.td $ ver
    H.td . H.toHtml . T.intercalate "," . keywords $ x
    H.td . H.toHtml . description $ x
    where 
        name = H.a ! A.href (H.toValue . url $ x) $ H.toHtml . displayName $ x
        pkg = let p = takeWhile (/='/') . reverse . T.unpack . package $ x in
            if null p then 
                "" 
            else 
                H.a ! A.href (H.toValue . toHtmlFileName 
                    . fullName . getDefaultVersion $ x) $ 
                    H.toHtml $ reverse p
        ver = H.a ! A.href (H.toValue . toHtmlFileName . package $ x) $ 
            H.toHtml $ defaultVersion $ x
        
toVersionPage :: Package -> H.Html
toVersionPage p = toPage (T.unpack . displayName $ p)  $ do
    H.h1 . H.toHtml . displayName $ p
    H.table $ H.span ! A.class_ "version_table" $ 
        forM_ (HM.elems $ versions p) toVersionRow 

toVersionRow v = H.tr $ do 
    H.td $ H.toHtml vv
    H.td $ H.a ! A.href (H.toValue $ toHtmlFileName fn) $ 
        H.toHtml $ cleanPath $ fn
    where 
        vv = version v
        fn = fullName v

toHelpPage :: Version -> H.Html
toHelpPage v = toPage (T.unpack t)  $ do
    H.h1 . H.toHtml $ t
    H.div ! A.class_ "help_page" $ H.toHtml . helpText $ v
    where t = cleanPath . fullName $ v
            
-- | Remove first part, up until first '/' 
cleanPath x 
    | T.any (=='/') x = T.tail . T.dropWhile (/='/') $ x
    | otherwise = x

toHtmlFileName p =  
    (T.unpack . T.toLower . T.replace "/" "_"  $ p)  ++ ".html"
