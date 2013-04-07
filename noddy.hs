{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Data.Typeable
import Data.Data
import Text.StringTemplate
import Text.StringTemplate.GenericStandard

data Package = Package 
    { package :: String
    , displayName :: String
    } deriving (Eq, Show, Data, Typeable)

tdata = [("1", Package "pack" "name"), ("2", Package "apa" "gorilla")]

-- apa t1 =
--    foldl (\acc f -> acc ++ toString (setAttribute "name" f t1)) "" 

main = do
--     print $ render $ setAttribute "stuff" (apa t1 a) t2
    let Just x = getStringTemplate "t2" tg
    putStrLn $ render $ setAttribute "name" (tdata :: [(String, Package)]) x 
--         $ setAttribute "foo" ("bar" :: String) x
    where 
        t1 = newSTMP "Hello $it.0$ $it.1.package$ $it.1.displayName$ " :: StringTemplate String
        t2 = newSTMP "$name:t1()$ $foo$" :: StringTemplate String
        tg = groupStringTemplates [("t1",t1), ("t2",t2)]

