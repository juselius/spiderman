#!/usr/bin/env runhaskell 
--
-- Parse Lmod JSON to Package representation
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import SoftwarePageTemplate
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
    , templatedir = "/home/jonas/src/spiderman/data/templates" 
        &= typDir &= help "Template directory for HTML pages"
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
    pkgs <- getPackages ason
    let 
        p = filterCategory (category args) . filterKeyword (keyword args) $ 
            pkgs 
        t = titulator (category args, keyword args) in
        if format args == "mas" then 
            mkMasXml (url args) "software" p
        else
            dispatchTemplates args t p

dispatchTemplates :: Flags -> String -> [L.Package] -> IO ()
dispatchTemplates args t p = do
    Except.catch (createDirectoryIfMissing True (dirname args)) handler
    Except.catch (setCurrentDirectory (dirname args)) handler
    templates <- ST.directoryGroup (templatedir args) :: 
        IO (ST.STGroup T.Text)
    case format args of
        "html" -> writeHtmlPackagePages templates t p
        "rst" -> writePackagePages templates t ".rst" htmlToRst p
        "gitit" -> writePackagePages templates t ".page" (toGitit.htmlToRst) p
        _ -> error "Invalid output format!"

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
        Just x -> return $ sortPackages . L.getPackages $ x
        otherwise -> error "Parsing packages failed"

-- HTML writers --
writeHtmlPackagePages templ t pkgs = do
    TIO.writeFile "index.html" $ renderHtmlListingTemplate templ t pkgs
    mapM_ (writeHtmlVersionPage templ) pkgs 
    mapM_ (writeHtmlHelpPages templ) pkgs

writeHtmlVersionPage templ p = 
    TIO.writeFile (packageVersionFile p ".html") $ 
        renderHtmlVersionTemplate templ p

writeHtmlHelpPages templ p = 
    mapM_ (\v -> TIO.writeFile (packageHelpFile v ".html") 
        (renderHtmlHelpTemplate templ v)) (HM.elems $ L.versions p)

-- Generic writers --
writePackagePages templ t ext fmt pkgs = do
    TIO.writeFile ("index" ++ ext) $ fmt $ renderListingTemplate templ t pkgs
    mapM_ (writeVersionPage templ ext fmt) pkgs 
    mapM_ (writeHelpPages templ ext fmt) pkgs

writeVersionPage templ ext fmt p = 
    TIO.writeFile (packageVersionFile p ext) $ 
        fmt $ renderVersionTemplate templ p

writeHelpPages templ ext fmt p = 
    mapM_ (\v -> TIO.writeFile (packageHelpFile v ext) 
        (fmt $ renderHelpTemplate templ v)) (HM.elems $ L.versions p)

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
