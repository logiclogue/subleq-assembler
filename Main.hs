module Main where

import Data.Word
import Data.Bits
import qualified Data.ByteString as B

data Instruction = Subleq Word16 Word16 Word16
    deriving (Show)

instructionToWords :: Instruction -> [Word16]
instructionToWords (Subleq x y z) = [x, y, z]

wordToBytes :: Word16 -> [Word8]
wordToBytes x = [mostSignificant x, leastSignificant x] where
    mostSignificant = toWord8 . (flip shiftR) 8
    leastSignificant = toWord8 . (fromInteger 0xFF .&.)
    toWord8 = fromInteger . toInteger

instructionsToByteString :: [Instruction] -> B.ByteString
instructionsToByteString = B.pack . concat . map wordToBytes . concat . map instructionToWords

subleq :: Word16 -> Word16 -> Word16 -> [Instruction]
subleq x y z = [Subleq x y z]

jmp :: Word16 -> [Instruction]
jmp address = subleq 0 0 address

main :: IO ()
main = do
    putStrLn $ show $ instructionsToByteString $ jmp 100
