#!/usr/bin/env runhaskell 
--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import SoftwarePage
import Data.Aeson
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Text.Blaze.Html.Renderer.Pretty
import qualified Lmodulator as L
import qualified Control.Exception as Except
import qualified System.IO.Error as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data Flags = Flags {
      outdir :: FilePath
    , inpfile :: FilePath
    , category :: String
    , keyword :: String
    , rst :: Bool
    } deriving (Data, Typeable, Show, Eq)

flags = Flags {
      outdir = "" &= typDir &= help "Output directory"
    , inpfile = def &= argPos 0 &= typFile
    , category = "" &= typ "CATEGORY" &= 
        help "Generate pages only for CATEGORY"
    , keyword = "" &= typ "KEYWORD" &= 
        help "Generate pages for KEYWORD"
    , rst = def  &= help "Generate reStructuredText"
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
            if rst args then 
                mkRstPackagePages t p 
            else 
                mkHtmlPackagePages t p

titulator (a, b) 
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

mkHtmlPackagePages :: String -> [L.Package] -> IO ()
mkHtmlPackagePages t pkgs = do
    writeFile "index.html" $ renderListingsPage t pkgs
    mapM_ mkHtmlVersionPage pkgs
    mapM_ mkHtmlHelpPages pkgs

mkHtmlVersionPage :: L.Package -> IO ()
mkHtmlVersionPage p = 
    writeFile (toLinkName (L.package p) ++ ".html") $ renderVersionPage p

mkHtmlHelpPages :: L.Package -> IO ()
mkHtmlHelpPages p = 
    mapM_ (\v -> 
        writeFile (toLinkName (L.fullName v) ++ ".html") $ renderHelpPage v) 
        (HM.elems $ L.versions p)

mkRstPackagePages :: String -> [L.Package] -> IO ()
mkRstPackagePages t pkgs = do
    writeFile "index.rst" $ 
        htmlToRst . renderListingsPage t $ pkgs
    mapM_ mkRstVersionPage pkgs
    mapM_ mkRstHelpPages pkgs

mkRstVersionPage :: L.Package -> IO ()
mkRstVersionPage p = 
    writeFile (toLinkName  (L.package p) ++ ".rst") $ 
        htmlToRst . renderVersionPage $ p

mkRstHelpPages :: L.Package -> IO ()
mkRstHelpPages p = 
    mapM_ (\v -> 
        writeFile (toLinkName (L.fullName v) ++ ".rst") $ 
            htmlToRst . renderHelpPage $ v) 
        (HM.elems $ L.versions p)

renderListingsPage t pkgs = renderHtml . toListingPage t $ pkgs

renderVersionPage p = renderHtml . toVersionPage $ p

renderHelpPage v = renderHtml . toHelpPage $ v

handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "File or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permissions denied!"  
    | otherwise = ioError e  
