{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative (empty)
import Control.Monad (liftM2)
import Data.Set (fromList)
import Data.Text (Text)
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Parsec

exec s = parse s ""

someFunc :: IO ()
someFunc = putStrLn "yeet"
