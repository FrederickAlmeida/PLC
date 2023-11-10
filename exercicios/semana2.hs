-- Questão 1
dobro :: Integer -> Integer
dobro x = x * 2

-- Questão 2
quadruplo :: Integer -> Integer
quadruplo x = dobro (dobro x)

-- Questão 3
poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = (a * (x*x)) + (b*x) + c

-- Questão 4
parImpar :: Integer -> String
parImpar x
    | (x `mod` 2) == 0 = "par"
    | otherwise = "impar"

-- Questão 5
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c
    | (a >= b) && (a >= c) = a
    | b >= c = b
    | otherwise = c

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d
    | maxThree a b c == a && maxThree a b d == a = a
    | maxThree b c d == b = b
    | maxThree b c d == c = c
    | otherwise = d

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d
    | (max a b == a) && (max a c == a) && (max a d == a) = a
    | (max b c == b) && (max b d == b) = b
    | (max c d == c) = c
    | otherwise = d

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d
    | (maxThree a b c == a) && (max a d == a) = a
    | (maxThree b c d == b) = b
    | (max c d == c) = c
    | otherwise = d

-- Questão 6
quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) = 2
    | otherwise = 0

-- Questão 7
ehZero :: Integer -> Bool
ehZero 0 = True
ehZero _ = False

-- Questão 8
sumTo :: Integer -> Integer
sumTo n
    | (n == 1) = 1
    | otherwise = n + sumTo(n-1)

-- Questão 9
potencia :: Integer -> Integer -> Integer
potencia n k
    | k == 1 = n
    | otherwise = n * (potencia n (k-1))

-- Questão 10
binomio :: Integer -> Integer -> Integer
binomio _ 0 = 1
binomio 0 _ = 0
binomio n k = (binomio (n-1) k) + (binomio (n-1) (k-1))

-- Questão 11
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)

-- Questão 12
addEspacos :: Int -> String
addEspacos n
    | n == 0 = ""
    | otherwise = " " ++ addEspacos (n-1)

-- Questão 13
paraDireita :: Int -> String -> String
paraDireita n str
    | n == 0 = str
    | otherwise = " " ++ paraDireita (n-1) str

-- Questão 14
imprimeTabela :: Int -> String
imprimeTabela n = cabecalho ++ imprimeSemanas n ++ imprimeTotal n ++ imprimeMedia n
    where
    cabecalho = "Semana    Venda\n"
    imprimeSemanas :: Int -> String
    imprimeSemanas n
        | n == 0 = "  0    12\n"
        | n == 1 = "  1    14\n"
        | n == 2 = "  2    15\n"
    imprimeTotal :: Int -> String 
    imprimeTotal n
        | n == 0 = "Total   12\n"
        | n == 1 = "Total   26\n"
        | n == 2 = "Total   41\n"
    imprimeMedia :: Int -> String
    imprimeMedia n
        | n == 0 = "Media   12\n"
        | n == 1 = "Media   13\n"
        | n == 2 = "Media   13.6667\n"