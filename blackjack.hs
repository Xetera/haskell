import System.Random
import Control.Monad

data Suite = Hearts | Spades | Diamonds | Clubs deriving (Show)
data Hand = Card Suite Int deriving (Show)
suites = [Hearts, Spades, Diamonds, Clubs] :: [Suite]

randomCardNumber :: IO Int
randomCardNumber = randomRIO (1, 13)

randomTo max = randomRIO (0, max)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle items = do 
  i <- randomTo $ (length items) - 1
  let (firstHalf, (item:secondHalf)) = splitAt i items
  (:) item <$> shuffle (firstHalf ++ secondHalf)


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n items 
  | n > 0 = (take n items) : (chunk n $ drop n items)
  | otherwise = error "Cannot group a negative amount"

compiledSuite suite = map (Card suite) [1..13]
compiledDeck = concatMap compiledSuite suites

draw deck = let (first:second) = take 2 deck in
  (first, head second)

main = do
  initial <- chunk 2 <$> shuffle compiledDeck
  foldM_ (\deck hands -> do
    -- input <- getLine
    print deck
    return $ hands ++ deck
    ) [] initial

