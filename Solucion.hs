module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula x = (ord x >= 97) && (ord x <= 122) 

-- EJ 2
letraANatural :: Char -> Int
letraANatural l = ord l - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar x 0 = x
desplazar x y | not (esMinuscula x) = x
              | (ord x) + y > 122 = chr (96 - 122 + (ord x) + y) 
              | (ord x) + y < 97 = chr (122 + y + (ord x)-96) 
              | otherwise = chr ((ord x) + y)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (l:rl) n = (desplazar l n):(cifrar rl n)

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (l:rl) n = (desplazar l (-n)):(descifrar rl n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista p = cifrarListaAux p 0
cifrarListaAux :: [String] -> [Int] -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (p:rp) n = (cifrar p n):(cifrarLista rp (n + 1))

-- EJ 7
frecuencia :: String -> [Float]
frecuencia _ = [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente [] _ = (' ', 0)
cifradoMasFrecuente (x:xs) y | obtenerEnesimoELemento (frecuencia (x:xs)) 0 ((ord x)-97) == maximo (frecuencia (x:xs)) = (x, maximo(frecuencia (x:xs)))
                             | otherwise = cifradoMasFrecuente (xs++[x]) y
-- el primer int siempre tiene que ser 0 para que arranque a contar desde 0
obtenerEnesimoELemento:: [String] -> Int-> Int-> String
obtenerEnesimoELemento (x:xs) z n | z==n = x
                                  | otherwise = obtenerEnesimoELemento xs (z+1) n
-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado "" "" = True
esDescifrado "" _ = False
esDescifrado _ "" = False
esDescifrado (x1:[]) (y1:[]) = True
esDescifrado (x1:x2:xs) (y1:y2:ys) = (ord x1) - (ord y1) == ord(x2) - ord(y2) && esDescifrado (x2:xs) (y2:ys)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados _ = [("compu", "frpsx"), ("frpsx", "compu")]

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ _ = "compucom"

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere "" _ = ""
cifrarVigenere (x:xs) (y:ys) | length (x:xs) /= length (y:ys) = cifrarVigenere (x:xs) (expandirClave (y:ys) (length (x:xs)))
                             | otherwise = (head (cifrar [x] ((ord y) - 97))) : (cifrarVigenere xs ys)

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere "" _ = ""
descifrarVigenere (x:xs) (y:ys) | length (x:xs) /= length (y:ys) = descifrarVigenere (x:xs) (expandirClave (y:ys) (length (x:xs)))
                                | otherwise = (head (cifrar [x] (123 -(ord y)))):(descifrarVigenere xs ys)

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
