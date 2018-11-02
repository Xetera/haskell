import System.Directory (listDirectory)
import System.IO
import Control.Monad
import Data.List

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


main = do
    findFiles "." "." 
    
