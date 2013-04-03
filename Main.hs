--
-- Parse Lmod JSON to Package representation:
--
-- $ /path/to/spider -o softwarePage /opt/modulefiles > lmod.json
-- 
-- (c) jonas.juselius@uit.no, 2013
--
import SoftwarePage
import Lmodulus
import Data.Aeson
import qualified System.Environment as Env
import qualified Data.ByteString.Lazy.Char8 as BS

main = do
    args <- Env.getArgs
    ason <- BS.readFile (head args)
    putStrLn "<table>"
    case (decode ason :: Maybe Packages) of
        Just x -> putStrLn $ unlines (map toListHTML (getPackages x))
        otherwise -> putStrLn "damn. failed."
    putStrLn "</table>"
--     print $ foo ason
--     print $ (decode (ason) :: Maybe Value)


