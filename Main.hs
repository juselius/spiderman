#!/usr/bin/env runhaskell 
--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
-- TODO: filter on Category and Keywords
--
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
import SoftwarePage
import Lmodulator
import Data.Aeson
import System.Directory
import System.FilePath
import System.Console.CmdArgs
import Text.Blaze.Html.Renderer.Pretty
import qualified Control.Exception as Except
import qualified System.IO.Error as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM

data Flags = Flags {
    outdir :: FilePath,
    inpfile :: FilePath
    } deriving (Data, Typeable, Show, Eq)

flags = Flags {
      outdir = "" &= typDir &= help "Output directory"
    , inpfile = def &= argPos 0 &= typFile
    } 
    &= verbosity 
    &= help "Convert Lmod/JSON to HTML pages" 
    &= summary "Lmodulator v1.0.0, (c) Henry H. Juxtapose" 
    &= details ["Create the appropriate JSON file with spider -o softwarePage"]

dirname args  
    | null $ outdir args = fst . splitExtension . inpfile $ args 
    | otherwise = outdir args

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    Except.catch (createDirectoryIfMissing True (dirname args)) handler
    Except.catch (setCurrentDirectory (dirname args)) handler
    case (decode ason :: Maybe Packages) of
        Just x -> mkPackagePages $ getPackages x
        otherwise -> putStrLn "damn. failed."
--     print $ (decode (ason) :: Maybe Value)

mkPackagePages :: [Package] -> IO ()
mkPackagePages pkgs = do
    writeFile "index.html" $ renderListingsPage pkgs
    mapM_ mkVersionPage pkgs
    mapM_ mkHelpPages pkgs
--     writeFile "help.html" $ 
--         renderHelpPage . snd . head . HM.toList . versions . head $ pkgs

mkVersionPage :: Package -> IO ()
mkVersionPage p = 
    writeFile (toHtmlFileName $ package p) $ renderVersionPage p

mkHelpPages :: Package -> IO ()
mkHelpPages p = 
    mapM_ (\v -> 
        writeFile (toHtmlFileName $ fullName v) $ renderHelpPage v) 
        (HM.elems $ versions p)
    

renderListingsPage pkgs = renderHtml . toListingPage "Packages" $ pkgs

renderVersionPage p = renderHtml . toVersionPage $ p

renderHelpPage v = renderHtml . toHelpPage $ v

handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "File or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permissions denied!"  
    | otherwise = ioError e  
