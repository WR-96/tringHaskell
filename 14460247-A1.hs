import Data.List
--Viernes 23 de Febrero de 2018
--Funcion para desplegar un mensaje de saludo con un texto adicional

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

multres :: Num a => a -> a
multres y = y * 3

--Ejercicios de tarea 

cuadrado :: Num a => a -> a
cuadrado x = x * x

raices :: Floating a => a -> a -> a -> (a, a)
raices a b c = ((-b + (getSqrt a b c ))/(2*a), (-b - (getSqrt a b c))/(2*a))

getSqrt :: Floating a => a -> a -> a -> a
getSqrt a b c = sqrt(b^2 - 4 * a * c)


convFC :: Fractional a => a -> a
convFC a = (a -32) / 1.8

invertir :: [a] -> [a]
invertir x = reverse x

--Ejercicios 26 de febrero
--Día de suerte, uso de multiples opciones
lucky :: (Integral a) => a -> String
lucky 7 = "Es tu dia de suerte!"
lucky x = "Lo siento, hoy no es tu dia de suerte!"

--Días de la semana
dia :: (Integral a) => a -> String
dia 1 = "Lunes"
dia 2 = "Martes"
dia 3 = "Miercoles"
dia 4 = "Jueves"
dia 5 = "Viernes"
dia x = "Ingresa un valor entre 1 y 5"

--Factorial
fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

--Fibonacci. Obtinene el numero de fibonacci en la posición 'n'
fibo :: (Integral a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

--Sucesion Fibonacci. Despliega la susesion de Fibonacci hasta la posicion 'n'
sFibo :: Num a => Int -> [a]
sFibo 0 = [0]
sFibo 1 = sFibo 0 ++ [1] 
sFibo n = sFibo (n-1) ++ [ ( sFibo (n-1) !! (n-1) ) + ( sFibo (n-1) !! (n-2) ) ]

-- TAREA Insertar un elemento en la mitad de una lista (27 de febrero)
insMitadLista :: Num a => [a] -> a -> [a]
insMitadLista lista x = (take ((length lista) `div` 2) lista ) ++ [x] ++ (drop ((length lista)`div`2) lista)

insElemList :: [a] -> Int -> a -> [a]
insElemList lista pos ele = (take pos lista) ++  [ele] ++ (drop pos lista)

-- Funciones del 5 de Marzo estas son las dos primeras
rotarIzq :: [a] -> [a]
rotarIzq a = tail a ++ [head a]
rotarIzq' (a:as) = as ++ [a]

rotarDer :: [a] -> [a]
rotarDer a = last a : init a

rotate :: [a] -> Int -> Char -> [a]
rotate list times dir
    | dir == 'I' = rotateList times'
    | dir == 'D' = rotateList times''
    | otherwise = error "Movimiento invalido"
    where
        rotateList a = drop a list ++ take a list
        times'' = length list - times' 
        times' = if times >= length list
        then mod times (length list) 
        else times

-- Funcion para eliminar la primer incidencia de un numero en una lista
--delNum :: Eq a => [a] -> a -> [a]
delNum list x = 
    if elem x list
    then list \\ [x]
    else list

-- Funcion para determinar si una cadena es palindromo
esPalindromo :: [Char] -> [Char]
esPalindromo frase =
    if frase' == reverse frase' 
    then "Si es palindromo"
    else "No es palindromo"
    where frase' = deleteWhiteSpaces frase

deleteWhiteSpaces :: [Char] -> [Char]
deleteWhiteSpaces list = [x | x <- list, x /= ' ']

-- Tarea Lunes 12 de marzo

numPerfect n = [x | x <- [1..n], x == sum (init (factores x))]
factores n = [x | x <- [1..n], mod n x == 0] 

pitagoricas :: ( Enum a, Eq a, Num a) => a -> [(a,a,a)]
pitagoricas x = [(a,b,c) | c <- [1..x], b <- [1..x], a <- [1..x], a^2 + b^2 == c^2]

--Funcion que dado un numero devuelve una lista con los numeros primos hasta ese numero
numPrimos x = numPrimos' [2..x] x
numPrimos' (x:xs) y =
    if x^2 < y
    then [x] ++ numPrimos' [b | b <- xs, mod b x /= 0] y
    else [x] ++ xs

--Funcion que devuelve el minimo comun multiplo de dos numeros
mcm :: Integral a => a -> a -> a
mcm a b = div (a * b) (mcd (a,b))
--Funcion que devuelve el maximo comun divisor de dos numeros
mcd (a,0) = a
mcd (a,b) = mcd (b, mod a b)

myZip :: (Ord a) => [a] -> [a] -> [a]
myZip [] [] = []
myZip x [] = x
myZip [] y = y
myZip (x:xs) (y:ys) = 
    if x < y
    then [x] ++ myZip xs (y:ys)
    else [y] ++ myZip (x:xs) ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    quickSort (filter (<=x) xs)
    ++ [x] ++ 
    quickSort (filter (>x) xs)
