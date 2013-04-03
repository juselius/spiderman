#!/usr/bin/env runhaskell 
--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
{-# LANGUAGE DeriveDataTypeable #-}
import SoftwarePage
import Lmodulus
import Data.Aeson
import System.Console.CmdArgs
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
    &= summary "Lmodulus v1.0.0, (c) Henry H. Juxtapose" 
--     &= details ["Foo"]

main = do
    args <- cmdArgs flags
    ason <- BS.readFile (inpfile args)
    case (decode ason :: Maybe Packages) of
        Just x -> putStrLn $ unlines (map toListHTML (getPackages x))
        otherwise -> putStrLn "damn. failed."
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)


