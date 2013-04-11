--
-- Parse Lmod JSON to Package representation
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings #-}

#ifdef CABAL_BUILD
import Paths_spiderman
#endif

import SoftwarePageTemplate
import MasXml
import Data.Aeson
import Data.Char
import Data.List
import Data.Version
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
    &= summary ("Version " ++ ver ++ ", (c) Jonas Juselius 2013")
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
    defTemplDir <- getDataFileName "templates"
    Except.catch (createDirectoryIfMissing True (dirname args)) handler
    Except.catch (setCurrentDirectory (dirname args)) handler
    let templdir = if null $ templatedir args then defTemplDir else templatedir args 
    templates <- ST.directoryGroup (templdir) :: 
        IO (ST.STGroup T.Text)
    case format args of
        "html" -> writeHtmlPackagePages fname templates t p
        "rst" -> writePackagePages fname templates t  htmlToRst p
        "gitit" -> writePackagePages fname templates t (toGitit.htmlToRst) p
        _ -> error "Invalid output format!"
    where fname = mkIndexFileName args

mkIndexFileName args = 
    let fext = case format args of
            "html" -> ".html"
            "rst" ->  ".rst"
            "gitit" -> ".page"
            _ -> error "Invalid output format!"
        catg = category args
        keyw = keyword args
        fname = 
            if null catg && null keyw then "index" else
            if null catg then keyw else 
            if null keyw then catg else catg ++ "_" ++ keyw in
    fname ++ fext

getFileExt = dropWhile (/='.')
    
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
        Just x -> return $ sortPackages . formatPackageList . L.getPackages $ x
        otherwise -> error "Parsing packages failed"

-- HTML writers --
writeHtmlPackagePages fname templ t pkgs = do
    TIO.writeFile fname $ renderHtmlListingTemplate templ t pkgs
    mapM_ (writeHtmlVersionPage templ) pkgs 
    mapM_ (writeHtmlHelpPages templ) pkgs

writeHtmlVersionPage templ p = 
    TIO.writeFile (packageVersionFile ".html" p) $ 
        renderHtmlVersionTemplate templ p

writeHtmlHelpPages templ p = 
    mapM_ (\v -> TIO.writeFile (packageHelpFile ".html" v) 
        (renderHtmlHelpTemplate templ v)) (HM.elems $ L.versions p)

-- Generic writers --
writePackagePages fname templ t fmt pkgs = do
    let ext = getFileExt fname 
    TIO.writeFile fname $ fmt $ renderListingTemplate templ t pkgs
    mapM_ (writeVersionPage ext templ fmt) pkgs 
    mapM_ (writeHelpPages ext templ fmt) pkgs

writeVersionPage ext templ fmt p = 
    TIO.writeFile (packageVersionFile ext p) $ 
        fmt $ renderVersionTemplate templ p

writeHelpPages ext templ fmt p = 
    mapM_ (\v -> TIO.writeFile (packageHelpFile ext v) 
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

ver = showVersion version

#ifndef CABAL_BUILD
version = Version [1, 0] []
getDataFileName x = do 
    cwd <- getCurrentDirectory 
    return $ cwd </> "data" </> x
#endif
