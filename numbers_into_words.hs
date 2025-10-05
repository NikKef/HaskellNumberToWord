-- Implement a function that takes a number as an argument
-- and returns the number as a word, less than 1 million
units, teens, tens :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twevle", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Define convert1 which deals with numbers 0 <= n < 10
convert1 :: Int -> String
convert1 n = units!!n

-- Define convert2 which deals with numbers 0 <= n < 100
digit2 :: Int -> (Int, Int)
digit2 n = (div n 10, mod n 10)

combine2 :: (Int, Int) -> String
combine2 (0, u) = units!!u
combine2 (1, u) = teens!!u
combine2 (t, 0) = tens!!(t-2)
combine2 (t, u) = tens!!(t-2) ++ "-" ++ units!!u

convert2 :: Int -> String
convert2 = combine2 . digit2

-- Define convert3 which deals with numbers 0 <= n <1000
digit3 :: Int -> (Int, Int, Int)
digit3 n = (div n 100, mod (div n 10) 10, mod n 10)

combine3 :: (Int, Int, Int) -> String
combine3 (0, t, u) = combine2(t, u)
combine3 (h, 0, 0) = units!!h ++ " hundred"
combine3 (h, t, u) = units!!h ++ " hundred " ++ combine2(t, u)

convert3 :: Int -> String
convert3 = combine3 . digit3

-- Define convert6 which deals with the initial problem 0 <= n < 1 000 000
type Hundred = (Int, Int, Int)
digit6 :: Int -> (Hundred, Hundred)
digit6 n = (digit3 (div n 1000), digit3 (mod n 1000))

link :: Hundred -> String
link (0, b, c) = " and "
link (a, b, c) = " "

combine6 :: (Hundred, Hundred) -> String
combine6 ((0,0,0), a) = combine3 a
combine6 (a, (0,0,0)) = combine3 a ++ " thousand"
combine6 (a, b) = combine3 a ++ " thousand" ++ link b ++ combine3 b

convert6 :: Int -> String
convert6 = combine6 . digit6