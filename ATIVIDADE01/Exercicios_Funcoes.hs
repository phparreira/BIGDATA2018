-- Pedro Henrique Parreira

--Exerc�cio 01: Crie uma fun��o ehTriangulo que determina se tr�s lados x, y, z podem formar um tri�ngulo.

ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo a b c
    | abs(b-c) > a || a > (b*c) = False
    | abs(a-c) > b || b > (a*c) = False
    | abs(a-b) > c || c > (b*a) = False
    | otherwise = True

-- Exerc�cio 02: Crie uma fun��o tipoTriangulo que determina o tipo do tri�ngulo formado pelos tr�s lados x, y, z.

tipoTriangulo :: Double -> Double -> Double -> String
tipoTriangulo a b c
    | ehTriangulo a b c == False = "Nao eh Triangulo"
    | (a==b) || (b==c) || (a==c) = "Triangulo Isosceles"
    | (a==b) && (b==c) && (a==c) = "Triangulo Equilatero"
    | (a/=b) && (b/=c) && (a/=c) = "Triangulo Escaleno"

-- Exerc�cio 03: Implemente uma fun��o que faz a multiplica��o et�ope entre dois n�meros.
multEtiope :: Integer -> Integer -> Integer
multEtiope x y = divide x y 0
      where
        soma x y      = if (x `mod` 2 == 0) then 0 else y
        divide 1 y s = s + y
        divide x y s = divide (x `div` 2) (2*y) ((soma x y) + s)

-- Exerc�cio 04: Fa�a uma fun��o que determine se um n�mero � primo.
ehPrimo :: Integer -> Bool
ehPrimo x = ehPrimoh x (x-1)
      where
        ehPrimoh a 1 = True
        ehPrimoh a b = if (a `mod` b == 0) then False 
                       else ehPrimoh a (b-1)
    

-- Exerc�cio 05: Fa�a uma fun��o que calcule a soma dos d�gitos de um n�mero.
somaNumero :: Integer -> Integer
somaNumero x = somando x 0
    where
      dividir x = x `mod` (10)
      somando   0 r  = r
      somando   x r  = somando (x `div` 10) (dividir x + r)

-- Exerc�cio 06: Fa�a uma fun��o que calcule a persist�ncia aditiva de um n�mero.
persisAdi :: Integer -> Integer
persisAdi x = contagem x 0 - 1
    where
      dividir x = x `mod` (10)
      somando   0 r 1 = 0
      somando   0 r i = r
      somando   x r i = somando (x `div` 10) (dividir x + r) (i+1) 
      contagem  0 r = r
      contagem  x r = contagem (somando x 0 0) (r+1) 
      

-- Exerc�cio 07: Fa�a uma fun��o que calcule o coeficiente binomial de (m,n).
coefBinominal :: Integer -> Integer -> Integer
coefBinominal m n  
    | (m<0) || (n<0) = error "Numeros negativos"
    | otherwise = 
       (fatorial m) `div` ( fatorial n * fatorial (m-n))
    where
      fatorial 1  = 1
      fatorial b  = b*fatorial (b-1)
      
-- Exerc�cio 08: Fa�a uma fun��o que calcule o elemento (i,j) do tri�ngulo de pascal.
trianguloPascal :: Integer -> Integer -> Integer
trianguloPascal i j = calcular i j
    where
      fatorial 1 s = s
      fatorial x s = fatorial (x-1)(s*x)
      calcular i j = (fatorial (i+j) 1) `div` (fatorial j 1)


main = do
-- Teste Exercicio 1
print( ehTriangulo 10 5 3 )
-- Teste Exercicio 2
print( tipoTriangulo 10 5 3 )
-- Teste Exercicio 3
print(multEtiope 5 2)
-- Teste Exercicio 4
print(ehPrimo 7)
-- Teste Exercicio 5
print(somaNumero 131)
-- Teste Exercicio 6
print(persisAdi 1351)
-- Teste Exercicio 7
print(coefBinominal 5 2)
-- Teste Exercicio 8
print(trianguloPascal 2 2)