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

glob :: FilePath -> IO [FilePath]
glob path = do
    files <- listDirectory path
    case files of 
        [] -> return []
        (xs) -> do
            let joinDir = joinPaths path
            directories <- filterM doesDirectoryExist xs
            dirFiles <- filterM (liftM not <$> doesDirectoryExist) xs
            let concatFiles = liftM2 (++)
            let rFiles = return $ map joinDir dirFiles
            case directories of
                [] -> rFiles
                _ -> concat <$> mapM (\dir -> rFiles `concatFiles` (glob $ joinDir dir)) directories
            

-- findDirectories path = let joinBase = joinPaths path in
--     map joinBase <$> listDirectory path >>= filterM doesDirectoryExist


joinPaths :: String -> String -> String
joinPaths [] b = b
joinPaths x [] = x
joinPaths x b
    | last x == '/' && head b == '/' = init x ++ b
    | last x == '/' || head b == '/' = x ++ b
    | otherwise = intercalate "/" [x, b]

extractExtension :: String -> String
extractExtension = dropWhile (/= '.')

filterExtension :: String -> [FilePath] -> [FilePath]
filterExtension extension files = filter (\file -> extractExtension file == extension) files

filterDir :: String -> FilePath -> IO [FilePath]
filterDir extension directory = filterExtension extension <$> listDirectory directory

-- renameAll :: String -> String -> FilePath -> IO Int
-- renameAll fromExt toExt path = do

    


main = do
    -- args <- getArgs
    -- -- findDirectories "./test"
    -- case args of 
    -- filterDir "hs" "."
    glob "."
        
    -- case args of 
    --     [x] -> findDirectories x
    --     [] -> findDirectories "test"
