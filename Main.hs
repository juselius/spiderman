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
{-# LANGUAGE DeriveDataTypeable #-}
import SoftwarePage
import Lmodulator
import Data.Aeson
import System.Directory
import System.Console.CmdArgs
import qualified Control.Exception as Except
import qualified System.IO.Error as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS

data Flags = Flags {
    outdir :: FilePath,
    inpfile :: FilePath
    } deriving (Data, Typeable, Show, Eq)

flags = Flags {
      outdir = "html" &= typDir &= help "Output directory"
    , inpfile = def &= argPos 0 &= typFile
    } 
    &= verbosity 
    &= help "Convert Lmod/JSON to HTML pages" 
    &= summary "Lmodulator v1.0.0, (c) Henry H. Juxtapose" 
--     &= details ["Foo"]

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    Except.catch (createDirectoryIfMissing True (outdir args)) handler
    Except.catch (setCurrentDirectory (outdir args)) handler
    case (decode ason :: Maybe Packages) of
        Just x -> putStrLn $ unlines (map toListHTML (getPackages x))
        otherwise -> putStrLn "damn. failed."
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)

handler :: IO.IOError -> IO ()  
handler e  
    | IO.isDoesNotExistError e =    
        putStrLn "The file or directory doesn't exist!"  
    | IO.isPermissionError e = 
        putStrLn "Permissions denied!"  
    | otherwise = ioError e  
