{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (mapM_, when)
import           Data.Char
import           Data.Text
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.IO
import           Prelude hiding (dropWhile, takeWhile)

takeVersion :: Text -> Text
takeVersion =  takeWhile ok . dropWhile (not . isDigit)
  where ok c = isDigit c || c == '.'

getDirective :: Text -> Handle -> IO Text
getDirective directive file = do
  line <- TIO.hGetLine file
  let (name, rest) = breakOn ":" line
  if directive == name
    then return $ takeVersion rest
    else getDirective directive file

main :: IO ()
main = do
  getArgs >>= mapM_ run
  where
    run path = do
      v <- withFile path ReadMode (getDirective "Version")
      r <- withFile path ReadMode (getDirective "Release")
      TIO.putStrLn $ v <> "-" <> r
