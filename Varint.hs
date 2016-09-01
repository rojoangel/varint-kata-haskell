module Varint where
import Data.Char (digitToInt)


toBinary :: Int -> [Char]
toBinary 0 = []
toBinary n = toBinary (n `quot` 2) ++ show (n `mod` 2)

topUpArray :: [Char] -> [Char]
topUpArray xs = take gap (repeat '0') ++ xs
    where gap = 7 - (length xs `mod` 7)

reverse7BitGroups :: [Char] -> [Char]
reverse7BitGroups [] = []
reverse7BitGroups xs = let (first7Bits, lastBits) = splitAt 7 xs in reverse7BitGroups lastBits ++ first7Bits

addMostSignificantBits :: [Char] -> [Char]
addMostSignificantBits [] = []
addMostSignificantBits xs = let (firstBits, last7Bits) = splitAt (length xs - 7) xs in recursivelyPrepend1 firstBits ++ '0' : last7Bits
    where recursivelyPrepend1 [] = []
          recursivelyPrepend1 xs = let (firstBits, last7Bits) = splitAt (length xs - 7) xs in recursivelyPrepend1 firstBits ++ '1' : last7Bits

format :: [Char] -> [Char]
format xs = init (format' xs)
  where format' [] = []
        format' xs = let (first4B, lastB) = splitAt 4 xs in first4B ++ [' '] ++ (format' lastB)

toBase128Binary :: Int -> [Char]
toBase128Binary x = addMostSignificantBits(reverse7BitGroups (topUpArray (toBinary x)))

toVarint :: Int -> String
toVarint i = format (toBase128Binary i)

unformat :: [Char] -> [Char]
unformat xs = filter (\x -> x `elem` "01") xs

dropMostSignificantBits :: [Char] -> [Char]
dropMostSignificantBits [] = []
dropMostSignificantBits xs = let (first8B, lastB) = splitAt 8 xs in drop 1 first8B ++ dropMostSignificantBits lastB

fromBinary :: [Char] -> Int
fromBinary xs = sum (zipWith (\ x y -> digitToInt x * y) xs [ 2^y| y <- [length xs - 1 , length xs -2 .. 0]])

fromVarint :: String -> Int
fromVarint xs = fromBinary(reverse7BitGroups(dropMostSignificantBits(unformat xs)))
