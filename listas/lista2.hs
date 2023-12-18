-- Questão 1
somaSqrt :: [Float] -> Float
somaSqrt [] = 0
somaSqrt (a:as)
    | a <= 0 = somaSqrt as
    | otherwise = sqrt(a) + somaSqrt as

-- Questão 2
unzip2 :: [(a, b)] -> ([a], [b])
unzip2 [] = ([], [])
unzip2 l = (foldr (\x var -> [fst x] ++ var) [] l, foldr (\x var -> [snd x] ++ var) [] l)

-- Questão 3
type Texto = String
type Id = String
type DataHoraPub = Int

data Post = Post (Id, DataHoraPub) Texto
    deriving (Show, Eq)

data Thread = Nil | T Post (Thread)

-- a
instance Show Thread where
    show (T (Post (n, h) t) Nil) = "(" ++ n ++ " " ++ show h ++ " " ++ t ++ ")"
    show (T (Post (n, h) t) x) = "(" ++ n ++ " " ++ show h ++ " " ++ t ++ ")" ++ show x

-- b
inserirPost :: Post -> Thread -> Thread
inserirPost (Post (n, h) t) (Nil) = T (Post (n, h) t) Nil
inserirPost (Post (n, h) t) (T x y) = T (Post (n, h) t) (T x y)

-- c
threadToList :: Thread -> [Post]
threadToList Nil = []
threadToList (T (Post (n, h) t) x) = [(Post (n, h) t)] ++ threadToList x

-- d
listToThread :: [Post] -> Thread
listToThread [] = Nil
listToThread ((Post (n, h) t) : as) = T (Post (n, h) t) (listToThread as)

-- e
removerPost :: (Id, DataHoraPub) -> Thread -> Thread
removerPost a (T x y) = listToThread (filter (isPost a) (threadToList (T x y)))

isPost :: (Id, DataHoraPub) -> Post -> Bool
isPost a (Post (n, h) _)
    | fst a == n && snd a == h = False
    | otherwise = True