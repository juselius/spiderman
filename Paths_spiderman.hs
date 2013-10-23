module Paths_spiderman where
import Data.Version

version = Version [1, 0] []
ver = showVersion version

getDataFileName name = return ("data/" ++ name)

