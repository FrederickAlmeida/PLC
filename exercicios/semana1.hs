{-
Questão 1:
it: serve para referenciar o resultado da última expressão avaliada.
let: serve para definir uma variável local. -}

{-
Questão 3:
"double 2 3": dá um erro, pois a expressão aceita apenas um argumento, mas está recebendo 2
"double square": dá um erro, pois square necessita de um argumento, porém não está sendo passado nenhum
"2 double": problema similar ao último, double não tem argumentos suficientes, e outro erro levantado é que não existe instância de "(Num ((Integer -> Integer) -> ()))"-}


-- exercicio 3.9:
{-
Define a function

threeDifferent :: Integer -> Integer -> Integer -> Bool

so that the result of threeDifferent m n p is True only if all three of the
numbers m, n and p are different.

What is your answer for threeDifferent 3 4 3? Explain why you get the
answer that you do.
-}
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p
    | (m /= n) && (n /= p) && (m /= p) = True
    | otherwise = False

-- a resposta é falso, uma vez que m == p

-- exercício 3.10:
{-
This question is about the function

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool

which returns the value True only if all four of its arguments are equal.

Give a definition of fourEqual modelled on the definition of threeEqual
above. Now give a definition of fourEqual which usesthe function threeEqual
in its definition. Compare your two answers.
-}
treeEqual ::  Integer -> Integer -> Integer -> Bool
treeEqual m n p
    | (m == n) && (n == p) = True
    | otherwise = False

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q
    | (m == n) && (n == p) && (p == q) = True
    | otherwise = False

fourEqualTE :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqualTE m n p q
    | (treeEqual m n p) && (treeEqual n p q) = True
    | otherwise = False