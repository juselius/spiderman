#!/usr/bin/env runhaskell 
--
-- Parse Lmod JSON to Package representation
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import SoftwarePage
import MasXml
import Data.Aeson
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import qualified Lmodulator as L
import qualified Control.Exception as Except
import qualified System.IO.Error as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.StringTemplate as ST
 
data Flags = Flags {
      outdir :: FilePath
    , inpfile :: FilePath
    , templatedir :: FilePath
    , category :: String
    , keyword :: String
    , format :: String
    , url :: String
    } deriving (Data, Typeable, Show, Eq)

flags = Flags {
      outdir = "" &= typDir &= help "Output directory"
    , templatedir = "" &= typDir &= help "Template directory for HTML pages"
    , inpfile = def &= argPos 0 &= typFile
    , category = "" &= typ "CATEGORY" &= 
        help "Generate pages only for CATEGORY"
    , keyword = "" &= typ "KEYWORD" &= 
        help "Generate pages for KEYWORD"
    , format = "html" &= help "Output format: html, rst, gitit, mas"
    , url = "" &= help "Base url for links"
    } 
    &= verbosity 
    &= help "Convert Lmod/JSON to HTML pages" 
    &= summary "Lmodulator v1.0.0, (c) Henry H. Juxtapose" 
    &= details [
         "Process JSON into a HTML tree."
        ,"Create the appropriate JSON file using the 'runspider.sh' script."
        , ""
        ]

dirname args  
    | null $ outdir args = fst . splitExtension . inpfile $ args 
    | otherwise = outdir args

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    Except.catch (createDirectoryIfMissing True (dirname args)) handler
    Except.catch (setCurrentDirectory (dirname args)) handler
    pkgs <- getPackages ason
    let 
        p = filterCategory (category args) . filterKeyword (keyword args) $ 
            pkgs 
        t = titulator (category args, keyword args) in
            case format args of
                "html" -> mkPackagePages t ".html" id p
                "rst" -> mkPackagePages t ".rst" htmlToRst p
                "gitit" -> mkPackagePages t ".page" (toGitit . htmlToRst) p
                "mas" -> mkMasXml (url args) "software" p
                _ -> error "Invalid output format!"
--     templates <- ST.directoryGroup "/home/jonas/src/spiderman/data/templates/" :: IO (ST.STGroup T.Text)
--     let Just st = ST.getStringTemplate "page" templates in
--         TIO.putStrLn $ ST.render st

titulator (a, b) 
    | null a = p ++ kw
    | a' `isPrefixOf` "development" = "Development " ++ p ++ kw
    | a' `isPrefixOf` "library" = "Libraries" ++ kw
    | a' `isPrefixOf` "compiler" = "Compilers" ++ kw
    | a' `isPrefixOf` "application" = "Applications" ++ kw
    | otherwise = p ++ kw
    where 
        a' = map toLower a
        p = "Packages"
        kw = if null b then "" else ": " ++ b

filterCategory x 
    | null x = id
    | otherwise = filter (\y -> lowerText x `T.isPrefixOf` L.category y) 


filterKeyword x 
    | null x = id
    | otherwise = filter (any (lowerText x `T.isInfixOf`) . L.keywords)

lowerText = T.toLower . T.pack

printKeyword = fmap print $ map L.keywords

getPackages :: BS.ByteString -> IO [L.Package]
getPackages ason =
    case (decode ason :: Maybe L.Packages) of
        Just x -> return $ L.getPackages x
        otherwise -> return []

mkPackagePages :: String -> String -> (String -> String) -> [L.Package] -> IO ()
mkPackagePages t ext fmt pkgs = do
    writeFile ("index" ++ ext) $ fmt . renderListingPage t $ pkgs
    mapM_ (mkVersionPage ext fmt) pkgs 
    mapM_ (mkHelpPages ext fmt) pkgs

mkVersionPage :: String -> (String -> String) -> L.Package -> IO ()
mkVersionPage ext fmt p = 
    writeFile (packageVersionFile p ext) $ 
        fmt . renderVersionPage $ p

mkHelpPages :: String -> (String -> String) -> L.Package -> IO ()
mkHelpPages ext fmt p = 
    mapM_ (\v -> writeFile (packageHelpFile v ext) 
        (fmt . renderHelpPage $ v)) (HM.elems $ L.versions p)

mkMasXml :: String -> String -> [L.Package] -> IO ()
mkMasXml baseUrl f p = writeFile (f ++ ".xml") $ 
    BS.unpack $ renderMasXml baseUrl p
        
handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "File or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permissions denied!"  
    | otherwise = ioError e  
