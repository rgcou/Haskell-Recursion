{-Ejercicio 1. Implementar la función fibonacci: Integer -> Integer que devuelve el i-ésimo número de Fibonacci. -}

fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

{-Ejercicio 2. Implementar una función parteEntera :: Float ->Integer según la siguiente especificación:

problema parteEntera (x: R) : Z {
requiere: {T rue }
asegura: { resultado ≤ x < resultado + 1 }
-}

parteEntera :: Float -> Integer
parteEntera n | n == 0 = 0 
              | (n > 0) = parteEnteraAux1 n 0
              | (n < 0) = parteEnteraAux1 n 0

parteEnteraAux1 :: Float -> Integer -> Integer
parteEnteraAux1 n k | (fromIntegral k < n) = parteEnteraAux1 n (k+1)
                    | (fromIntegral k > n) = (k-1)

parteEnteraAux2 :: Float -> Integer -> Integer
parteEnteraAux2 n k | (fromIntegral k > n) = parteEnteraAux1 n (k-1)
                    | (fromIntegral k < n) = k

{-Ejercicio 3. Implementar la función esDivisible :: Integer -> Integer ->Bool 
que dados dos números naturales determinar si el primero es divisible por el segundo. 
No está permitido utilizar las funciones mod ni div.-}

esDivisible :: Integer -> Integer -> Bool
esDivisible a b | (((a>0) && (b>0)) && (b>a)) || (((a<0) && (b<0)) && (b<a))  = False
                | (((a>0) && (b<0)) && ((abs b) > a)) ||(((a<0) && (b>0)) && (b > (abs a))) = False
                | ((a>0) && (b>0)) || ((a<0) && (b<0)) = esDivisibleAux1 a b 1
                | ((a>0) && (b<0)) || ((a<0) && (b>0)) = esDivisibleAux2 a b (-1) 
               

esDivisibleAux1 :: Integer -> Integer -> Integer -> Bool
esDivisibleAux1 a b k | (k >= (abs a)) = False
                      | (b*k) == a = True
                      | (k < (abs a)) = esDivisibleAux1 a b (k+1)

esDivisibleAux2 :: Integer -> Integer -> Integer -> Bool
esDivisibleAux2 a b k | (k >= (abs a)) = False
                      | (b*k) == a = True
                      | (k > a) = esDivisibleAux2 a b (k-1)


{-Ejercicio 4. Implementar la función sumaImpares :: Integer -> Integer que dado n ∈ N sume los primeros n números
impares. Por ejemplo: sumaImpares 3 = 1+3+5 ⇝ 9.-}

sumaImpares :: Integer -> Integer 
sumaImpares a = sumaImparesAux a 1 1

sumaImparesAux :: Integer -> Integer -> Integer -> Integer
sumaImparesAux a j k | j == a = k
                     | otherwise = k + (sumaImparesAux a (j+1) (k+2))

{- Ejercicio 5. Implementar la función medioFact :: Integer -> Integer que dado n ∈ N calcula n!! = n (n−2)(n−4) ... -}

medioFact :: Integer -> Integer
medioFact a | a == 0 = 0
            | a == 1 = 1
            | esDivisible a 2 == True = medioFactAux1 a
            | esDivisible a 2 == False = medioFactAux2 a

medioFactAux1 :: Integer -> Integer
medioFactAux1 a | a == 0 = 1
                | otherwise = a * medioFactAux1 (a-2)

medioFactAux2 :: Integer -> Integer
medioFactAux2 a | a == 1 = 1
                | otherwise = a * medioFactAux2 (a-2)

{-Ejercicio 6. Implementar la función sumaDigitos :: Integer -> Integer que calcula la suma de dígitos de un número
natural. Para esta función pueden utilizar div y mod-}    

sumaDigitos :: Integer -> Integer
sumaDigitos n | ((n >=1) && (n<=9)) = n  
              | otherwise =  (n `mod` 10) + (sumaDigitos (n `div` 10))

{-Ejercicio 7. Implementar la función todosDigitosIguales :: Integer -> Bool que determina si todos los dígitos de un
número natural son iguales-}

elUltimoDigito :: Integer -> Integer -> Integer
elUltimoDigito a b | (a - (10*b)) < 10 = (a - (10*b))
                   | otherwise = elUltimoDigito a (b+1)

fcionAux :: Integer -> Integer -> Integer
fcionAux d r | r == 0 = d
             | otherwise = d*(10^r) + fcionAux d (r-1)

fcionAux2 :: Integer -> Integer -> Integer -> Integer -> Bool               
fcionAux2 m n d j | (n > m) = False 
                  | (n == m) = True
                  | (n < m) = fcionAux2 m (fcionAux d j) d (j+1)  

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales m = fcionAux2 m (elUltimoDigito m 1) (elUltimoDigito m 1) 1

{- Ejercicio 8. Implementar la función iesimoDigito :: Integer -> Integer -> Integer que dado un n ∈ N ≥ 0 y un i ∈ N
menor o igual a la cantidad de dígitos de n, devuelve el i-ésimo dígito de n.-} --PENSARLO SIN MOD NI DIV

cantidadDigitosAux1 :: Integer -> Integer
cantidadDigitosAux1 n = n `div` 10

cantidadDigitosAux2 :: Integer -> Integer -> Integer -> Integer
cantidadDigitosAux2 m n j | n < 10 = j
                          | (n >= 10) = cantidadDigitosAux2 m (cantidadDigitosAux1 n) (j+1) 

cantidadDigitos :: Integer -> Integer
cantidadDigitos m = cantidadDigitosAux2 m m 1

iesimoDigitoaux1 :: Integer -> Integer -> Integer
iesimoDigitoaux1 a b = a `div` b 

iesimoDigitoaux2 :: Integer -> Integer -> Integer -> Integer -> Integer
iesimoDigitoaux2 m i j k | i == k = m `mod` 10
                         | i == j = m `mod` 10
                         | i < k = iesimoDigitoaux2 (iesimoDigitoaux1 m 10) i (j-1) k

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = iesimoDigitoaux2 n i (cantidadDigitos n) (cantidadDigitos n)

{-Ejercicio 9. Implementar una función esCapicua :: Integer -> Bool que dado n ∈ N ≥ 0 determina si n es un número
capicúa-} --PENSARLO SIN MOD NI DIV

esCapicua :: Integer -> Bool 
esCapicua n | ((cantidadDigitos n) `mod` 2 == 0) = esCapicuaAux1 n 1 (cantidadDigitos n) 
            | ((cantidadDigitos n) `mod` 2 /= 0) = esCapicuaAux2 n 1 (cantidadDigitos n)

esCapicuaAux1 :: Integer -> Integer -> Integer -> Bool
esCapicuaAux1 n i j | i > j = True
                    | ((iesimoDigito n i) /= (iesimoDigito n j)) = False
                    | ((iesimoDigito n i) == (iesimoDigito n j)) = esCapicuaAux1 n (i+1) (j-1) 

esCapicuaAux2 :: Integer -> Integer -> Integer -> Bool
esCapicuaAux2 n i j | i == ((cantidadDigitos n + 1) `div` 2) = True
                    | ((iesimoDigito n i) /= (iesimoDigito n j)) = False
                    | ((iesimoDigito n i) == (iesimoDigito n j)) = esCapicuaAux2 n (i+1) (j-1) 

--10a)

f1Aux :: Integer -> Integer -> Integer
f1Aux n i | i == n = 2^n
          | (i < n) = (2^i) + f1Aux n (i+1)

f1 :: Integer -> Integer
f1 n = f1Aux n 0

--10b) 

f2Aux :: Integer -> Integer -> Float -> Float
f2Aux n i q | i == n = (q^n)
            | (i < n) = (q^i) + f2Aux n (i+1) q

f2 :: Integer -> Float -> Float
f2 n q = f2Aux n 1 q

--10c)

f3Aux :: Integer -> Integer -> Float -> Float
f3Aux n i q | i == (2*n) = (q^(2*n))
            | (i < (2*n)) = (q^i) + f3Aux n (i+1) q

f3 :: Integer -> Float -> Float
f3 n q = f3Aux n 1 q

--10d)

f4Aux :: Integer -> Integer -> Float -> Float
f4Aux n i q | i == (2*n) = (q^(2*n))
            | (i < (2*n)) = (q^i) + f4Aux n (i+1) q

f4 :: Integer -> Float -> Float
f4 n q = f4Aux n n q

--11a) 

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)

eAproxAux :: Integer -> Integer -> Float 
eAproxAux n i | i == n = (1/(fromIntegral(factorial n)))
              | (i < n) = (1/(fromIntegral(factorial i))) + (eAproxAux n (i+1))

eAprox :: Integer -> Float
eAprox n = eAproxAux n 0

--11b)

constantee :: Float
constantee = eAprox 9

--12)

a_n :: Integer -> Float
a_n k | k == 1 = 2
      | otherwise = (fromIntegral 2) + ((fromIntegral 1)/(a_n (k-1)))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = (a_n n) - (fromIntegral 1) 

--13)

f13Aux :: Integer -> Integer -> Integer -> Integer -> Integer
f13Aux j m i n | i == n = n^j
               | j == m = (i^j) + f13Aux 1 m (i+1) n
               | (j < m) = (i^j) + f13Aux (j+1) m i n

f13 :: Integer -> Integer -> Integer
f13 n m = f13Aux 1 m 1 n


{- Ejercicio 14. Implementar una función sumaPotencias :: Integer -> Integer -> Integer -> Integer que dados tres
naturales q, n, m sume todas las potencias de la forma q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m -}

f14Aux :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
f14Aux q a n b m | b == m = q^(a+m)
                 | a == n = (q^(n+b)) + f14Aux q 1 n (b+1) m
                 | (a < n) = (q^(a+b)) + f14Aux q (a+1) n b m

f14 :: Integer -> Integer -> Integer -> Integer
f14 q n m = f14Aux q 1 n 1 m

{-Ejercicio 15. Implementar una función sumaRacionales :: Integer -> Integer -> Float que dados dos naturales n, m
sume todos los números racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m -}

f15Aux :: Integer -> Integer -> Integer -> Integer -> Float
f15Aux p n q m | q == m =  (fromIntegral p/fromIntegral m) 
               | p == n = (fromIntegral n/fromIntegral q) + f15Aux 1 n (q+1) m
               | (p < n) = (fromIntegral p/fromIntegral q) + f15Aux (p+1) n q m

f15 :: Integer -> Integer -> Float
f15 n m = f15Aux 1 n 1 m

{- 16a) Implementar menorDivisor :: Integer -> Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
como parámetro-}

menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux n a | a == n = n 
                    | n `mod` a == 0 = a
                    | otherwise = menorDivisorAux n (a+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux n 2

{- 16b) Implementar la función esPrimo :: Integer -> Bool que indica si un número natural pasado como parámetro es primo.-}

esPrimo :: Integer -> Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False

{-16c) Implementar la función sonCoprimos :: Integer -> Integer -> Bool que dados dos números naturales indica si no
tienen algún divisor en común mayor estricto que 1-}

divisoresAux1 :: Integer -> Integer -> [Integer]
divisoresAux1 n x | x == 1 = 1:[]
                  | n `mod` x == 0 = x:(divisoresAux1 n (x-1))
                  | otherwise = divisoresAux1 n (x-1)

{-16d) Implementar la función nEsimoPrimo :: Integer -> Integer que devuelve el n-ésimo primo (n ≥ 1). Recordar que el
primer primo es el 2, el segundo es el 3, el tercero es el 5, etc -}


nEsimoPrimoAux :: Integer -> Integer -> Integer -> Integer
nEsimoPrimoAux n i j | i == n = (j-1)
                     | esPrimo j == True = nEsimoPrimoAux n (i+1) (j+1)
                     | esPrimo j == False = nEsimoPrimoAux n i (j+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 0 2 

{- Ejercicio 17. Implementar la función esFibonacci :: Integer -> Bool según la siguiente especificación:
problema esFibonacci (n: Z) : B {
requiere: { n ≥ 0 }
asegura: { resultado = True ↔ (∃i : Z)(i ≥ 0 ∧ n = fib(i)) } -}

esFibonacciAux :: Integer -> Integer -> Bool
esFibonacciAux n i | n == 0 = True
                   | n == 1 = True
                   | i > n = False
                   | n == fib (i) = True
                   | otherwise = esFibonacciAux n (i+1)

esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciAux n 1