module Main (main) where

import           Control.Applicative
import           System.Environment
import           Datatypes
import           Parser
import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString.Char8

main :: IO ()
main = do
    x <- head <$> getArgs
    file <- BS.readFile x
    print (parseOnly program file)