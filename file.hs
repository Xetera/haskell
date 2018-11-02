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


findFiles :: FilePath -> FilePath -> IO [FilePath]
findFiles target directory = let containsTarget = isInfixOf target in 
    filter containsTarget <$> listDirectory directory 


findFile :: FilePath -> String -> IO (Maybe FilePath)
findFile target directory = head' <$> findFiles target directory


head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

glob :: String -> FilePath -> IO [FilePath]
glob pattern path 
    | null <$> files = return []
    | otherwise = return []
    where files = listDirectory path

findDirectories path = let joinBase = joinPaths path in
    map joinBase <$> listDirectory path >>= filterM doesDirectoryExist


joinPaths :: String -> String -> String
joinPaths [] b = b
joinPaths x [] = x
joinPaths x b
    | last x == '/' && head b == '/' = init x ++ b
    | last x == '/' || head b == '/' = x ++ b
    | otherwise = intercalate "/" [x, b]

main = do
    args <- getArgs
    findDirectories "./test"
    -- case args of 
    --     [x] -> findDirectories x
    --     [] -> findDirectories "test"
