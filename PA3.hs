-- Jesse Alsing
-- 6/1/18
import Data.Char
import Data.List

-- converts letters to numbers between 0-25
let2nat :: Char -> Int
let2nat n =  ord n - 97

-- converts 0-25 to its appropriate letter
nat2let :: Int -> Char
nat2let n = chr (n + 97)

-- shifts a word by x and has gaurds for wraps and non lowercase letters
shift :: Int -> Char -> Char
shift x y 
    | let2nat (y) > 25 = y
    | let2nat (y) < 0 = y
    | let2nat (y) + x > 25 = nat2let((let2nat (y) + x) `mod` 26)
    | let2nat (y) + x < 25 = nat2let((let2nat (y) + x) `mod` 26)
    | otherwise = nat2let(let2nat (y) + x)

--encodes a string by shifting each letter x times
encode :: Int ->String -> String
encode x xs = [shift x y | y <- xs]

--decodes a string by shifting back letters x times
decode :: Int -> String -> String
decode x xs = [shift (-x) y | y <- xs]

--gives an integer with how many lowercase letters are in a string
lowers :: String -> Int
lowers xs = length [ x | x <- xs, isLower x]

-- gives a count of how many of a given char are in a string
count :: Char -> String -> Int
count x xs = length [y | y <- xs, y == x]

--gives the percent from x to y 
percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

--gives the percentage frequencies of each letter in a string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z'] ]
    where n = lowers xs

--rotatates a string x characters to the left
rotate :: Int -> [a] -> [a]
rotate x xs = drop x xs ++ take x xs

--using list comprehension we find the summation of chi square for a list of frequencies 
chisqr :: [Float] -> [Float] -> Float 
chisqr xs ys = sum [((x-y)^2)/y | (x,y) <- zip xs ys]

--gives the position of the first encounter of an integer in a list
position :: Eq a => a -> [a] -> Int
position x xs = head [ i' | (x', i') <- zip xs [0..n], x==x']
    where n = length xs - 1

--frequency of average percentages in the alphabet	
table :: [Float ]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- cracks a message by finding the frequency of each letter in a string. Then
-- the chisqr value of each rotation is calculated with the given average values
-- the position of the minimum value is used to decode the message
crack :: String -> String
crack xs = decode (factor) xs
           where factor = position (minimum chitable) chitable
                 chitable = [ chisqr (rotate x tables) table | x <- [0..25] ]
                 tables = freqs xs