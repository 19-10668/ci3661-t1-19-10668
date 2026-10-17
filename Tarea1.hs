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
  , conve
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo = undefined

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor = undefined

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



duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined


-- borrable

main = do
  putStrLn("pruebas:")

  putStrLn( show (leerHora "20:44"))

