-- Funcion para obtener el factorial de un numero dado
fac 0 = 1
fac n = n * fac(n-1)

-- Funcion para calcular el numero de conbinaciones posibles usando dos numeros
comb n k = fac(n) / (fac(k) * fac(n-k)) 

-- Funcion que utiliza la formula de Heron para calcular el area de un triangulo
-- Recibe como parametros los lados de un triangulo
area :: Floating a => a -> a -> a -> a
area a b c = sqrt(s*(s-a)*(s-b)*(s-c)) 
    where s = (a+b+c)/2

-- Implemetacion de mapeo 
-- Se recibe una funcion y esta es aplicada a cada elemento de una lista
myMap :: (a -> b) -> [a] -> [b]  
myMap _ [] = []  
myMap f (x:xs) = f x : map f xs  

-- Implentacion de filter
-- Filtra los elementos de una lista segun el parametro dado
myFilter :: (a -> Bool) -> [a] -> [a]  
myFilter _ [] = []  
myFilter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs

-- Obtine la lista de los factores de un numero
factores n = [x | x <- [1..n], mod n x == 0]

-- Comprueba si un numero x es divisible entre y comprobando que y sea factor de x
divisible x y = elem y (factores x) 

--Funcion que recibe como parametro el dia mes y año y devuelbe el dia de la semana de esa fecha
dia d m a
    | x == 0 = "Domingo"
    | x == 1 = "Lunes"
    | x == 2 = "Martes"
    | x == 3 = "Miercoles"
    | x == 4 = "Jueves"
    | x == 5 = "Viernes"
    | x == 6 = "Sabado"
        where x = dia' d m a

-- Utiliza una formula para calcular el dia de la semana de una fecha dada
dia' d x y = mod (mod a 7 + (div a 4 - 3 * mod (div (div a 100 + 1) 4) 7 + m + mod d 7)) 7
    where m = modMes y x
          a = y -1

-- Verifica que un año dado sea bisiesto
esBisiesto :: Int -> Bool
esBisiesto x = (mod x 400 == 0) || (mod x 4 == 0) && not (mod x 100 == 0)

-- Obtiene el modulo de un mes para la formula de la funcion dia
-- Toma la tupla que correspode a los posibles modulos de un mes dado
-- y devuelve uno o el otro dependiendo de si el año es bisiesto o no
modMes a m =
    if bisiesto
    then snd(last(take m l))
    else fst(last(take m l))
        where bisiesto = esBisiesto a
              l = [(0,0),(3,3),(3,4),(6,0),(1,2),(4,5),(6,0),(2,3),(5,6),(0,1),(3,4),(5,6)]

-- Recibe un numero y lo combierte en cadena
int2str :: Integer -> String 
int2str n = show n 

-- Genera el enesimo renglon del triangulo de pascal
-- utilizando la relacion que este tiene con los coeficientes binomiales
pascal 0 = [1.0]
pascal a = [comb n k | k <- [0..n]]
    where n = a-1
