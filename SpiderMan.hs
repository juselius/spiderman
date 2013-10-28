--
-- Parse Lmod JSON to Package representation
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Paths_spiderman
import Data.Aeson
import Data.Char
import Data.List
import Data.Version
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import MasXml
import SoftwarePages
import qualified LmodPackage as L
import qualified Control.Exception as Except
import qualified System.IO.Error as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
 
-- | Data for command line flags
data Flags = Flags {
      outdir :: FilePath
    , inpfile :: FilePath
    , category :: String
    , keyword :: String
    , format :: String
    , url :: String
    , site :: String
    , mainpage :: Bool
    } deriving (Data, Typeable, Show, Eq)

-- | Process command line arguments
flags = Flags {
      outdir = "" &= typDir &= help "Output directory"
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
    &= summary ("Version " ++ showVersion version ++ ", (c) Jonas Juselius 2013")
    &= details [
         "Process JSON into a HTML tree."
        ,"Create the appropriate JSON file using the 'runspider.sh' script."
        , ""
        ]

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    gotoOutdir args
    let catg = T.pack $ category args
        kw = T.pack $ keyword args
    pkgs <- fmap (filterPackages catg kw) (decodePackages ason)
    dispatchTemplates args pkgs
    putStrLn $ "*** Processed " ++ show (length pkgs) ++  " packages."

-- | Dispatch to the correct writer based on the command line
dispatchTemplates :: Flags -> [L.Package] -> IO ()
dispatchTemplates args pkgs =
    case format args of
        "html" -> writeHtmlPages p
        "rst" -> writeRstPages p
        "gitit" -> writeGititPages p
        "mas" -> writeMasXml (T.pack $ site args) (T.pack $ url args) page
        _ -> error "Invalid output format!"
    where 
        page = IndexPage { 
              pageTitle = makeTitle (T.pack $ category args) 
                (T.pack $ keyword args) 
            , pageName = indexFileName args 
            , packageList = pkgs
            }
        p = formatPage (\f -> urlify f `T.append` outputFileExt args) page

-- | Generic package/module writer, used by the formatters
writePages :: [(FilePath, T.Text)] -> IO ()
writePages = mapM_ (uncurry TIO.writeFile)

writeHtmlPages :: Page -> IO ()
writeHtmlPages x = writePages $ makeHtmlPages x

writeRstPages :: Page -> IO ()
writeRstPages x = writePages $ makeRstPages x

writeGititPages :: Page -> IO ()
writeGititPages x = writePages $ makeGititPages x

writeMasXml :: T.Text -> T.Text -> Page -> IO ()
writeMasXml site baseUrl page = 
    writeFile fname $ BS.unpack $ renderMasXml site baseUrl p
    where 
        fname = T.unpack $ pageName page
        p = packageList $ formatPage (\f -> 
            urlify f `T.append` ".html") page

-- | Change directory to the output directory, creating it if it doesn't exist
gotoOutdir args = do
    Except.catch (createDirectoryIfMissing True dir) handler
    Except.catch (setCurrentDirectory dir) handler
    where dir = nameOutdir (outdir args) (inpfile args)

outputFileExt args =
    case format args of
        "html" -> ".html"
        "rst" ->  ".rst"
        "gitit" -> ".page"
        "mas" -> ".xml"
        _ -> error "Invalid output format!"

-- | Figure out what the "index" page should be called
indexFileName args 
    | format args == "mas" = "software.xml" 
    | null catg && null keyw = "index" 
    | null catg = T.pack keyw 
    | null keyw = T.pack catg 
    | otherwise = T.pack $ catg ++ "_" ++ keyw 
    where
        catg = category args
        keyw = keyword args 

nameOutdir dir filename
    | null dir = takeBaseName filename 
    | otherwise = dir

-- | Generate page title string 
makeTitle a b
    | T.null a' = p `T.append` kw
    | a' `T.isPrefixOf` "development" = "Development " `T.append` p `T.append` kw
    | a' `T.isPrefixOf` "library" = "Libraries" `T.append` kw
    | a' `T.isPrefixOf` "compiler" = "Compilers" `T.append` kw
    | a' `T.isPrefixOf` "application" = "Applications" `T.append` kw
    | otherwise = p `T.append` kw
    where 
        a' = T.toLower a
        p = "Packages"
        kw = if T.null b then "" else ": " `T.append` b

-- | Run json parser and warn on failed entries
decodePackages :: BS.ByteString -> IO [L.Package]
decodePackages ason =
    case decode ason of
        Just x -> let failed = L.failures x in
            if not (null failed)
            then do putStrLn $ warnFailed failed
                    return $ goodPackages x
            else return $ goodPackages x
        Nothing -> error "Parsing failed."

-- | Predicate for filtering out succeeded packages
goodPackages p = sortPackageList . HM.elems $ L.packages p

warnFailed flist = init $ foldl (\s (n, w) -> s
    ++ "Record " ++ show n ++ ": "
    ++ w
    ++ "\n") "" flist

-- | If the help page is a reference to an external page, skip it
skipHelpPage v
    | T.null href || "http" `T.isPrefixOf` href = False
    | otherwise = True   
    where 
        href = L.helpPageHref v

-- | Oh no
handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "File or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permission denied!"  
    | otherwise = ioError e  

