--Pedro Henrique Parreira
-- Atividade 01 - Intelig�ncia na Web e Big Data, 2018
-- Exerc�cios Listas

main :: IO()


--Exerc�cio 01: Crie uma fun��o divisivel20 x que retorna verdadeiro se x for divis�vel por todos os n�meros de 1 a 20.
listadiv = [11, 13, 14, 16, 17, 18, 19, 20]
divisivel20 :: Integer -> Bool
divisivel20 a = if length( filter (>0) $ map (\x -> a `mod`x) listadiv) >0 then False else True

--Exerc�cio 02: Crie uma fun��o projectEuler5 que retorna o primeiro n�mero natural que retorna True para a fun��o do exerc�cio anterior. Pense em como reduzir o custo computacional.

-- Menor poss�vel de "primeira" seria
-- 11x13x14x16x16x18x19x20
projectEuler5 = projectEuler5' (mult listadiv) (length(listadiv)-1)
    where
      mult [] = 1
      mult (x:xs) = (x)*mult(xs)
      numero i = mult listadiv `div` (listadiv !! i)
      projectEuler5' menor 0 = menor
      projectEuler5' menor i = if((menor > (numero i)) && (divisivel20 (numero i) == True ) ) then projectEuler5' (numero i) (i-1) else projectEuler5' menor (i-1)

--Exerc�cio 03: Crie a lista de n�meros de Fibonacci utilizando uma fun��o geradora.
fib = 1 : 2 : prox fib
    where
      prox (x : t@(y:xs)) = (x+y) : prox t
      
--Exerc�cio 04: Utilizando a lista anterior, calcule a soma dos n�meros de Fibonacci pares dos valores que n�o excedem 4.000.000. (Project Euler 2)
somaFib (x:xs) = somaFib' (x:xs) 0
    where 
      ehPar a = if a `mod` 2 == 0 then a else 0
      somaFib' (x:xs) s = if ehPar(x) <= 4000000 then somaFib'(xs) (ehPar(x) + s) else s

--Exerc�cio 05: Fa�a uma fun��o para calcular o produto escalar entre dois vetores.
produtoint  []  [] = 0
produtoint  _   [] = 0
produtoint  []  _  = 0
produtoint (x:xs) (y:ys) = x*y + (produtoint xs ys)

-- Exerc�cio 06: Crie a fun��o collatz x que retorna x/2, se x for par e (3x+1) se for �mpar.
collatz x
  | par x = x `div` 2
  | otherwise = 3*x + 1
  where
    par x = if x `mod` 2 == 0 then True else False

-- Exerc�cio 07: Implemente uma fun��o collatzLen x que retorna o tamanho da lista formada pela aplica��o repetida de collatz sobre o valor x at� que essa chegue no n�mero 1.
collatzLen a =  length(collatzLen' a)
    where
      collatzLen' 1 = []
      collatzLen' a = (collatz a):collatzLen'((collatz a))
      
-- Exerc�cio 08: Encontre o n�mero x entre 1 e 1.000.000 que tem a maior sequ�ncia de Collatz. (Project Euler 14)
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
