-- Pedro Henrique Parreira

--Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.

listadiv = [11, 13, 14, 16, 17, 18, 19, 20]
divisivel20 :: Integer -> Bool
divisivel20 a = if length( filter (>0) $ map (\x -> a `mod`x) listadiv) >0 then False else True

--Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.

-- Menor possível de "primeira" seria
-- 11x13x14x16x16x18x19x20
projectEuler5 = projectEuler5' (mult listadiv) (length(listadiv)-1)
    where
      mult [] = 1
      mult (x:xs) = (x)*mult(xs)
      numero i = mult listadiv `div` (listadiv !! i)
      projectEuler5' menor 0 = menor
      projectEuler5' menor i = if((menor > (numero i)) && (divisivel20 (numero i) == True ) ) then projectEuler5' (numero i) (i-1) else projectEuler5' menor (i-1)

--Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
fib = 1 : 2 : prox fib
    where
      prox (x : t@(y:xs)) = (x+y) : prox t
      
--Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
somaFib (x:xs) = somaFib' (x:xs) 0
    where 
      ehPar a = if a `mod` 2 == 0 then a else 0
      somaFib' (x:xs) s = if ehPar(x) <= 4000000 then somaFib'(xs) (ehPar(x) + s) else s

--Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
produtoint  []  [] = 0
produtoint  _   [] = 0
produtoint  []  _  = 0
produtoint (x:xs) (y:ys) = x*y + (produtoint xs ys)

-- Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.
collatz x
  | par x = x `div` 2
  | otherwise = 3*x + 1
  where
    par x = if x `mod` 2 == 0 then True else False

-- Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
collatzLen a =  length(collatzLen' a)
    where
      collatzLen' 1 = []
      collatzLen' a = (collatz a):collatzLen'((collatz a))
      
-- Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
collatzLenMax = buscarMaiorSeq 1000000 0
    where
      maior x y = if y > x then y else x
      buscarMaiorSeq 0 b = b
      buscarMaiorSeq a b = buscarMaiorSeq (a-1) (maior b (collatzLen a)) 

main = do
-- Teste Exercicio 1
print(divisivel20 20)
-- Teste Exercicio 2
print(projectEuler5)
-- Teste Exercicio 3 
print(take 20 fib)
-- Teste Exercicio 4
print( somaFib fib)
-- Teste Exercicio 5 
print (produtoint [1,2,3] [1,2,3])
-- Teste Exercicio 6 
print( collatz 320)
-- Teste Exercicio 7 
print( collatzLen 67)
-- Teste Exercicio 8
print(collatzLenMax)