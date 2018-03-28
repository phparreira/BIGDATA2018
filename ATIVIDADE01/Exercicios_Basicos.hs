--Pedro Henrique Parreira

-- Exerc�cio 02: Fa�a uma fun��o mult3 x que retorne True caso a entrada seja m�ltiplo de 3 e False caso contr�rio.
mult3 x = ( x `rem` 3 == 0 ) 
          
-- Exerc�cio 03: Fa�a uma fun��o mult5 x que retorne True caso a entrada seja m�ltiplo de 5 e False caso contr�rio.
mult5 x = ( x `rem` 5 == 0 ) 

-- Exerc�cio 04: Fa�a uma fun��o mult35 x que retorne True caso a entrada seja m�ltiplo de 3 e 5 e False caso contr�rio.
mult35 x = ( x `rem` 3 == 0 && x `rem` 5 == 0 ) 

-- Exerc�cio 05: Fa�a um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E m�ltiplo de 2), e False caso contr�rio.
prog1 x = ( x < -1) || (x > 1 && x `rem` 2 == 0 )

-- Exerc�cio 06: Fa�a uma fun��o que recebe um tipo Integer e retorna ele dividido por 2

div2d x = x / 2

-- Exerc�cio 07: Faa uma fun��o que receba um �ngulo a e retorne uma tupla contendo o seno da metade desse �ngulo utilizando a identidade:
prog2 x  = (x1,x2)
    where
      x1 = calc
      x2 = (-1)*calc
      calc = sqrt( (1 - cos(x)) / 2 )


-- Exerc�cio 08: Crie uma lista de anos bissextos desde o ano 1 at� o atual.
lista1 = [x| x<-[1..2018], (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0)) ]

-- Exerc�cio 09: Encontre os 10 primeiros anos bissextos
lista2 = take 10 lista1
lista3 = [lista1 !! x| x<-[length(lista1)-11..length(lista1)-1]]

-- Exerc�cio 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.

primeirametade = [lista1 !! x| x<-[0..(length(lista1) `div` 2)]]
segundametade = [lista1 !! x| x<-[(length(lista1) `div` 2)+1..(length(lista1)-1)]]
lista4 = [(primeirametade,segundametade)]

--Exerc�cio 11: Crie um concatenador de strings que concatena duas strings separadas por espa�o.

conca :: [Char] -> [Char]  -> [Char] 
conca palavra1 palavra2 = palavra1 ++ " " ++palavra2


--Exerc�cio 12: Dada a string �0123456789�, crie uma lista com os d�gitos em formato Integer.

sequencia = "0123456789"
lista5 = [read(map(\c -> [c]) sequencia !! x)::Integer|x<-[0..(length(sequencia)-1)]]


main = do

--Exerc�cio 01: Execute as seguintes opera��es utilizando o menor n�mero de par�nteses:
--2�3+5
print(2*3+5)
--2+2�3+1
print(2+2*3+1)
--3^4+5�^25+1
print(3^4+5*(2^5)+1)

-- Teste Exercicio 2
print(mult3 36)
-- Teste Exercicio 3
print(mult5 65)
-- Teste Exercicio 4
print(mult5 15)
-- Teste Exercicio 5
print(prog1 24)
-- Teste Exercicio 6
print(div2d 8)
-- Teste Exercicio 7
print(prog2 0)
-- Teste Exercicio 8
print(lista1)
-- Teste Exercicio 9
print(lista2)
print(lista3)
-- Teste Exercicio 10
print(lista4)
-- Teste Exercicio 11
print(conca "Teste1" "Teste2")
-- Teste Exercicio 12
print(lista5)