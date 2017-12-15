module Main where

import Data.Word

data Instruction = Subleq Word16 Word16 Word16
    deriving (Show)

main :: IO ()
main = do
    putStrLn $ show $ Subleq 10 10 10
