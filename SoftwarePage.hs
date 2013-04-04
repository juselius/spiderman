--
-- Generate HTML from Lmod Packages
--
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module SoftwarePage (
    toListingPage
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

toListingPage :: [Package] -> H.Html
toListingPage x = H.docTypeHtml $ do
    H.head $ do 
        H.title "Packages"
    H.body $ do
        H.table $ H.span ! A.class_ "pTable" $ forM_ sx toTableRow
    where sx = sortBy (compare `on` T.toLower . displayName) x

toTableRow :: Package -> H.Html
toTableRow x = H.tr $ H.span ! A.class_ "rRow" $ do 
    H.td . H.toHtml $ name
    H.td . H.toHtml $ pkg
    H.td . H.toHtml . defaultVersion $ x
    H.td . H.toHtml . T.intercalate "," . keywords $ x
    H.td . H.toHtml . description $ x
    where 
        name = 
            H.a ! A.href (H.toValue . url $ x) $ H.toHtml . displayName $ x
        pkg = let p = takeWhile (/='/') . reverse . T.unpack . package $ x in
            if null p then "XXXX" else reverse p
        
