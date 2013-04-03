
module SoftwarePage (
    toListHTML
    ) where

import Lmodulator 
import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

nameOrURL :: Package -> String
nameOrURL x = case url x of
    Just u -> "<a href=" ++ T.unpack u ++ ">" 
        ++ T.unpack (name x) ++ "</a>"
    otherwise -> T.unpack (name x)


extractText :: Maybe T.Text -> String
extractText (Just t) = T.unpack t
extractText _ = mzero

getDefaultHelpText :: Package -> String
getDefaultHelpText x = 
    case HM.lookup (defaultVersion x) (versions x) of
        Just v -> 
            "<a href=" ++ extractText (helpText v) ++ ">" 
            ++ T.unpack (name x) ++ "</a>"
        otherwise -> mzero

toListHTML x = "<tr>" 
        ++ "<td> " ++ nameOrURL x ++ " </td>"
        ++ "<td> " ++ extractText (keywords x) ++ " </td>"
        ++ "<td> " ++ T.unpack (defaultVersion x) ++ " </td>"
        ++ "<td> " ++ T.unpack (description x) ++ " </td>"
        ++ "<td> " ++ getDefaultHelpText x ++ " </td>"
        ++ "</tr>"

