import System.Directory (listDirectory, doesDirectoryExist)
import System.IO
import Control.Monad
import Data.List
import System.Environment (getArgs)

fileName = "something.txt"

readFileLines :: String -> IO [String]
readFileLines target = lines <$> readFile target


overwriteWith :: (String -> String) -> String -> IO()
overwriteWith modifier name = do
    content <- readFile name
    length content `seq` writeFile name $ modifier content


findFiles :: FilePath -> String -> IO [FilePath]
findFiles target directory = let containsTarget = isInfixOf target in 
    filter containsTarget <$> listDirectory directory 


findFile :: FilePath -> String -> IO (Maybe FilePath)
findFile target directory = head' <$> findFiles target directory


head' [] = Nothing
head' (x:xs) = Just x

-- glob :: String -> FilePath -> IO [FilePath]
-- glob = getFiles

findDirectories path = let joinBase = joinPaths path in
    map joinBase <$> listDirectory path >>= filterM doesDirectoryExist


joinPaths :: String -> String -> String
joinPaths [] a = a
joinPaths a [] = a
joinPaths x b
    | last x == '/' && head b == '/' = init x ++ b
    | last x == '/' || head b == '/' = x ++ b
    | otherwise = intercalate "/" [x, b]

main = do
    args <- getArgs
    findDirectories "./test/inner"
    -- case args of 
    --     [x] -> findDirectories x
    --     [] -> findDirectories "test"
