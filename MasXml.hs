--
-- (c) jonas.juselius@uit.no, 2013
--
-- Note to self: xmlgen does not play well with OverloadedStrings
-- 
-- | Generate MetaDoc software page XML for MAPI
module MasXml (
      genMasXml
    , renderMasXml
    ) where

import Lmodulator
import SoftwarePage (packageVersionUrl)
import Data.Monoid
import Text.XML.Generator 
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

renderMasXml baseUrl p = xrender $ genMasXml (T.pack baseUrl) p

genMasXml :: T.Text -> [Package] -> Xml Doc
genMasXml baseUrl p = doc defaultDocInfo $ xelem "software" $
        xelems $ map (genMasPackageInfo baseUrl) p

genMasPackageInfo :: T.Text -> Package -> Xml Elem
genMasPackageInfo baseUrl p = xelem "sw_entry" $ 
    xattr "progName" (displayName p)  <> 
    xattr "version"  (defaultVersion p) <> 
    xattr "license" (license p) <> 
    xattr "infoUrl" (baseUrl `T.append` packageVersionUrl p) 
