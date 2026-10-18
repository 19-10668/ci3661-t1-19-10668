-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Gaston Garcia
--  Carnet: 19-10668
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado): ninguno
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================


-- se me habia ocurrido separar el arreglo en x : z : y donde la x y y serian cabeza y cola
-- entonces compararia x con y, pero me parecio complicado hacerlo entonces hice un reverse a mano y ya
miReverse :: String -> String
miReverse [] = []
miReverse (x:xs) = miReverse (xs) ++ [x]

compactar :: String -> String
compactar [] = []
compactar (x:xs) 
    | isAlphaNum x = [x] ++ compactar xs
    | not (isAlphaNum x) = compactar xs 

minimizar :: String -> String
minimizar [] = []
minimizar (x:xs) = [toLower x] ++ minimizar xs
-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo a = ( minimizar (compactar a) == minimizar (compactar (miReverse a)))

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- el unico que hace la recursion es mayorYmenor, es decir, es el unico que hace
-- el loop para recorrer la lista del primer elemento al ultimo; cuando llega al ultimo
-- se va devolviendo el par y se va comparando en funciones separadas la cabeza de ese
-- entoces con el correspondiente elemento que voy llevando

mn :: Ord a => a -> Maybe (a, a) -> a
mn a (Just b) 
    | a > snd b = snd b
    | a < snd b = a
    | otherwise = a

my :: Ord a => a -> Maybe (a, a) -> a
my a (Just b)
    | a > fst b = a
    | a < fst b = fst b
    | otherwise = a

-- soy la mera verga nojoda (odio los valoress nuneables)
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:[]) = Just (x,x) 
mayorYMenor (x:xs) = Just (my x (mayorYMenor xs), mn x (mayorYMenor xs))

conve :: Char -> Char -> Int
conve a b = (digitToInt a * 10) + (digitToInt b)

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora z 
  | length z /= 5 = Left "formato inválido"
leerHora (d1:d2:p:m1:m2:f)
  | not (isDigit d1 && isDigit d2 && isDigit m1 && isDigit m2) = Left "formato inválido"
  | p /= ':' || f /= [] = Left "formato inválido"
  | conve d1 d2 > 24 = Left "hora fuera de rango"
  | conve m1 m2 > 60 = Left "minutos fuera de rango"
  | otherwise = Right (conve d1 d2, conve m1 m2)
leerHora _ = Left "formato inválido"


date2Mim :: Either String (Int, Int) -> Either String Int
date2Mim (Right (x,y)) = Right (x * 60 + y)
date2Mim (Left a) = Left a

duracion :: String -> String -> Either String Int
duracion a b = case (date2Mim(leerHora a), date2Mim(leerHora b)) of
    (Right x, Right y) 
        | x > y -> Right ((1440 - x) + y)
        | x <= y -> Right (y - x)
    (Left x, Left y) -> Left x
    (Left x, Right y) -> Left x
    (Right x, Left y) -> Left y

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral _ [] = Nothing
promedioSobreUmbral u l = Just ((\(h,j) -> h / j) (foldr (\x (acc, b) -> if x <= u then (acc, b) else (acc + x, b + 1)) (0,0) l))

-- B2
-- yo imagino que se pondra siempre el caballo dentro del tablero pero bueno, por si a las moscas dejamos
-- las restricciones finales
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (x,y) = [(a,b) | (a,b) <- [(x-2,y-1), (x-2,y+1), (x-1,y-2), (x-1,y+2), (x+1,y-2), (x+1,y+2), (x+2,y-1), (x+2,y+1)], a > 0, b > 0, a < 9, b < 9, x > 0, y > 0, x < 9, y < 9]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos (x,y) = 
    let l = foldl (++) [] (map movimientosCaballo (movimientosCaballo (x,y)))
    in length ( foldr (\x acc -> if (elem x acc) then acc else x : acc ) [] l )

-- estas dos funciones son para debuggear, y ademas vienen de un intento fallido antes de que menterara de la existencia de elem
aV :: (Int, Int) -> [(Int, Int)]
aV (x,y) = 
    let l = foldl (++) [] (map movimientosCaballo (movimientosCaballo (x,y)))
    in  foldr (\x acc -> if elem x acc then acc else x : acc ) [] l 

comp :: [(Int, Int)] -> (Int, Int) -> Int
comp l (a,b) =  length (filter (== True) (map (== (a,b)) l))
--


-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f l = foldr (\y acc -> f y : acc) [] l  

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter _ [] = []
miFilter f l = foldr (\y acc -> if (f y) then acc else y : acc) [] l

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)


