--
-- (c) jonas.juselius@uit.no, 2013
--
-- | Generate MetaDoc software page XML for MAPI
module MasXml (
      genMasXml
    , renderMasXml
    ) where

import Lmodulator
import Data.Monoid
import Text.XML.Generator 
import qualified Data.ByteString.Lazy.Char8 as BS

-- genMasXml' :: Xml Doc
-- genMasXml' = 
--     let people = [("Stefan", "32"), ("Judith", "4")] 
--     in doc defaultDocInfo $ xelem "people"  "f"
--             xelems $ map (\(name, age) -> xelem "person" (xattr "age" age <#> xtext name)) people


renderMasXml p = xrender $ genMasXml p

genMasXml :: [Package] -> Xml Doc
genMasXml p = doc defaultDocInfo $ xelem "software" $
        xelems $ map genMasPackageInfo p

genMasPackageInfo :: Package -> Xml Elem
genMasPackageInfo p = xelem "sw_entry" $ 
    xattr "progName" n <> 
    xattr "version" v <> 
    xattr "license" l <> 
    xattr "infoUrl" u
    where 
        n = displayName p
        v = defaultVersion p
        l = ""
        u = ""
