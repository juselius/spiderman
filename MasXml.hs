--
-- (c) jonas.juselius@uit.no, 2013
--
-- Note to self: xmlgen does not play well with OverloadedStrings
-- 
-- | Generate MetaDoc software page XML for MAPI
{-# LANGUAGE OverloadedStrings #-}

module MasXml (
      genMasXml
    , renderMasXml
    ) where

import LmodPackage
import Data.Monoid
import Text.XML.Generator 
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

renderMasXml site baseUrl p = 
    xrender $ genMasXml site baseUrl p

genMasXml :: T.Text -> T.Text -> [Package] -> Xml Doc
genMasXml site baseUrl p = doc defaultDocInfo $ 
        xelem "MetaDoc" $
        xattr "version" "1.3.0" <> xattr "site_name" site <#> 
        xelem "software" (xelems $ map (genMasPackageInfo baseUrl) p)

genMasPackageInfo :: T.Text -> Package -> Xml Elem
genMasPackageInfo baseUrl p = xelem "sw_entry" $ 
    xattr "progName" (displayName p)  <> 
    xattr "version"  (defaultVersionName p) <> 
    xattr "license" (license p) <> 
    xattr "infoURL" (baseUrl `T.append` packageIndexName p) 
