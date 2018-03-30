--Pedro Henrique Parreira
-- Atividade 01 - Inteligência na Web e Big Data, 2018
-- Exercícios Matrizes

main :: IO()

--Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
-- a = n+1-i
lista1 :: Integral a => a -> a -> [a]
lista1 a n = (elemento a n) : prox a (n-1)
    where
      elemento a n = if(a==n) then 1 else 0
      prox a 0 = []
      prox a n = (elemento a n) : prox a (n-1)

funcao1 :: Integral a => a -> [[a]]
funcao1 n = [lista1 (n+1-a) n | a <-[1..n]]


--Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.   

funcao2 :: Integral a => [[a]] -> a
funcao2 (x:xs) = x !!0 + funcao2' 1 xs
    where
      funcao2' a [] = 0
      funcao2' a (x:xs) = (x !! a) + funcao2' (a+1) xs
     
--Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz
funcao3 :: Integral a => [[a]] -> a
funcao3 (x:xs) = x !!(length(x)-1) + funcao3' (length(x)-1) xs
    where
      funcao3' a [] = 0
      funcao3' a (x:xs) = (x !! (a-1)) + funcao3' (a-1) xs

main = do
-- Teste Exercicio1
print(funcao1 7)
-- Teste Exercicio2
let m1 = [[1,2,3],[2,7,3],[4,3,3]]
print(funcao2 m1)
-- Teste Exercicio3
print(funcao3 m1)
