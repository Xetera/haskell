import System.Directory
import System.IO
import Control.Monad (when)

fileName = "something.txt"

overwriteWith :: String -> (String -> String) -> IO()
overwriteWith name modifier = do
    content <- readFile name
    let replacer = modifier content
    when (length content > 0) $
        writeFile fileName replacer

main = do
    overwriteWith fileName (head . lines)
