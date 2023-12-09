-- questao 1
mdc :: Int -> Int -> Int
mdc a b
    | b == 0 = a
    | b > 0 = mdc b (a `mod` b)
    | b < 0 = mdc a (-b)

-- questao 2
numDiv :: Integral a => a -> a -> a
numDiv a 0 = 0
numDiv 0 b = 0
numDiv a b
    | a `mod` b == 0 = 1 + numDiv (a `div` b) b
    | otherwise = 0

-- questao 3
unicos :: [Integer] -> [Integer]
unicos [] = []
unicos (x:xs)
  | elem x xs == True = unicos [i | i <- xs, i /= x]
  | otherwise = [x] ++ unicos xs

-- questao 4
halve :: [t] -> ([t], [t])
halve [] = ([], [])
halve list =  (halveOdd list 1, halveEven list 1)

halveEven :: [t] -> Int -> [t]
halveEven [] _ = []
halveEven (x:xs) i
    | i `mod` 2 == 0 = [x] ++ halveEven xs (i+1)
    | otherwise = halveEven xs (i+1)

halveOdd :: [t] -> Int -> [t]
halveOdd [] _ = []
halveOdd (x:xs) i
    | i `mod` 2 == 1 = [x] ++ halveOdd xs (i+1)
    | otherwise = halveOdd xs (i+1)

-- questao 5
remDiv :: Int -> [a] -> ([a], [a])
remDiv _ [] = ([], [])
remDiv i list = (remDivFirst (i-1) list, remDivSecond (i) (i+1) list)

remDivFirst :: Int -> [a] -> [a]
remDivFirst _ [] = []
remDivFirst 0 _ = []
remDivFirst i (x:xs) = [x] ++ remDivFirst (i-1) xs

remDivSecond :: Int -> Int -> [a] -> [a]
remDivSecond _ _ [] = []
remDivSecond _ 0 _ = []
remDivSecond 0 i (x:xs) = [x] ++ remDivSecond 0 (i-1) xs
remDivSecond i j (x:xs) = remDivSecond (i-1) j xs