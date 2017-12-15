module Main where

import Data.Word
import Data.Bits

data Instruction = Subleq Word16 Word16 Word16
    deriving (Show)

wordToByte :: Word16 -> (Word8, Word8)
wordToByte x = (left x, right x) where
    left :: Word16 -> Word8
    left x  = (x (.&.) 0xFF00) `shiftR` 8
    right :: Word16 -> Word8
    right x = x (.&.) 0x00FF

main :: IO ()
main = do
    putStrLn $ show $ Subleq 10 10 10
