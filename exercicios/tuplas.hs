menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
    | a >= b && b >= c = (c, a)
    | a >= c && c >= b = (b, a)
    | b >= a && a >= c = (c, b)
    | b >= c && c >= a = (a, b)
    | c >= a && a >= b = (b, c)
    | c >= b && b >= a = (a, c)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c)
    | a >= b && b >= c = (c, b, a)
    | a >= c && c >= b = (b, c, a)
    | b >= a && a >= c = (c, a, b)
    | b >= c && c >= a = (a, c, b)
    | c >= a && a >= b = (b, a, c)
    | c >= b && b >= a = (a, b, c)

primeiraCoordenada :: (Float, Float) -> Float
primeiraCoordenada (a, b) = a

segundaCoordenada :: (Float, Float) -> Float
segundaCoordenada (a, b) = b

ehVertical :: (Float, Float) -> Bool
ehVertical (a, b)
    | a == b = True
    | otherwise = False

pertenceReta :: Float -> ((Float, Float), (Float, Float)) -> Float
pertenceReta x ((ax, ay), (bx, by)) = (((by-ay)/(bx-ax))*(x-ax)) + ay
