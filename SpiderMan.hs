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

import Data.Aeson
import Data.Char
import Data.List
import Data.Version
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import MasXml
import SoftwarePages
import LmodPackage (emptyPackage)
import qualified LmodPackage as L
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
    , site :: String
    , mainpage :: Bool
    } deriving (Data, Typeable, Show, Eq)

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    pkgs <- fmap (filterPackages args) (getPackages ason) 
    templates <- initializeTemplates (templatedir args)
    gotoOutdir args
    dispatchTemplates args pkgs templates

gotoOutdir args = do
    Except.catch (createDirectoryIfMissing True dir) handler
    Except.catch (setCurrentDirectory dir) handler
    where dir = nameOutdir (outdir args) (inpfile args)

initializeTemplates templdir = do
    defTemplDir <- getDataFileName "templates"
    let templates = if null templdir then defTemplDir else templdir in
        ST.directoryGroup templates :: IO (ST.STGroup T.Text)

dispatchTemplates :: Flags -> [L.Package] -> ST.STGroup T.Text -> IO ()
dispatchTemplates args pkgs templ = 
    case format args of
        "html" -> writeHtml p
        "rst" -> writePkgs htmlToRst p
        "gitit" -> writePkgs (toGitit . htmlToRst) p
        "mas" -> writeMasXml (site args) (url args) ("software" ++ ext page) p
        _ -> error "Invalid output format!"
    where 
        page = PageInfo { 
              title = makeTitle (category args) (keyword args) 
            , ext = outputFileExt $ format args
            , mainTemplate = getMainTemplate args templ
            , pkg = emptyPackage
            }
        p = formatPackageList page pkgs
        fname = nameIndexFile args ++ ext page
        writeHtml = if mainpage args 
            then writeListingPage fname id
            else writeSoftwarePages fname id
        writePkgs = if mainpage args 
            then writeListingPage fname 
            else writeSoftwarePages fname 

getMainTemplate args templs =
    case format args of
        "html" -> getTemplate "page" templs
        _ -> getTemplate "package" templs

getTemplate name templs =
    case ST.getStringTemplate name templs of
        Just t -> t
        Nothing -> error "Invalid template"

outputFileExt fmt =
    case fmt of
        "html" -> ".html"
        "rst" ->  ".rst"
        "gitit" -> ".page"
        "mas" -> ".xml"
        _ -> error "Invalid output format!"

nameIndexFile args 
    | null catg && null keyw = "index" 
    | null catg = keyw 
    | null keyw = catg 
    | otherwise = catg ++ "_" ++ keyw 
    where
        catg = category args
        keyw = keyword args 

nameOutdir dir filename
    | null dir = takeBaseName filename 
    | otherwise = dir

getFileExt = dropWhile (/='.')
    
makeTitle a b
    | null a' = p ++ kw
    | a' `isPrefixOf` "development" = "Development " ++ p ++ kw
    | a' `isPrefixOf` "library" = "Libraries" ++ kw
    | a' `isPrefixOf` "compiler" = "Compilers" ++ kw
    | a' `isPrefixOf` "application" = "Applications" ++ kw
    | otherwise = p ++ kw
    where 
        a' = map toLower a
        p = "Packages"
        kw = if null b then "" else ": " ++ b

filterPackages args = 
    filterCategory (category args) . filterKeyword (keyword args) 

filterCategory x 
    | null x = id
    | x == "all" = id
    | otherwise = filter (\y -> lowerText x `T.isPrefixOf` L.category y) 

filterKeyword x 
    | null x = id
    | x == "all" = id
    | otherwise = filter (any (lowerText x `T.isInfixOf`) . L.keywords)

lowerText = T.toLower . T.pack

printKeyword = fmap print $ map L.keywords

getPackages :: BS.ByteString -> IO [L.Package]
getPackages ason =
    case (eitherDecode ason :: Either String L.Packages) of
        Right x -> return $ sortPackages . L.getPackages $ x
        Left x -> error ("Parsing packages failed: " ++ x)

skipHelpPage page v
    | null url || "http" `isPrefixOf` url = False
    | otherwise = True   
    where url = T.unpack . packageHelpUrl page $ v

writeSoftwarePages fn formatter pages = do
    TIO.writeFile fn . formatter $ renderListingTemplate pages
    mapM_ (writeVersionPage formatter) pages
    mapM_ (writeHelpPages formatter) pages

writeListingPage fn formatter page = 
    TIO.writeFile fn . formatter $ renderListingTemplate page

writeVersionPage formatter page = 
    TIO.writeFile (packageVersionFileName page) $ 
        formatter $ renderVersionTemplate page 

writeHelpPages formatter page = 
    mapM_ (\v -> TIO.writeFile (packageHelpFileName (ext page) v) 
        (formatter $ renderHelpTemplate page v)) vers
    where 
        v = HM.elems $ L.versions (pkg page)
        vers = if ext page == "html" then 
            filter (skipHelpPage page) v else v

writeMasXml :: String -> String -> String -> [PageInfo] -> IO ()
writeMasXml site baseUrl f pages = 
--     let page = PageInfo site "" "" (ST.StringTemplate "")
--         p = formatPackageList page pkgs in
    writeFile f $ BS.unpack $ renderMasXml site baseUrl p
    where p = map pkg pages
        
handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "File or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permission denied!"  
    | otherwise = ioError e  

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
    , site = "" &= help "Site name"
    , mainpage = False &= help "Only generate main listing page"
    } 
    &= verbosity 
    &= help "Convert Lmod/JSON to HTML pages" 
    &= summary ("Version " ++ ver ++ ", (c) Jonas Juselius 2013")
    &= details [
         "Process JSON into a HTML tree."
        ,"Create the appropriate JSON file using the 'runspider.sh' script."
        , ""
        ]

ver = showVersion version

#ifndef CABAL_BUILD
version = Version [1, 0] []
getDataFileName x = do 
    cwd <- getCurrentDirectory 
    return $ cwd </> "data" </> x
#endif
