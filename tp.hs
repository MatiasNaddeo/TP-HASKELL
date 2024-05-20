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
              | (ord x) + y < 97 = chr (122 + y + (ord x) - 96) 
              | otherwise = chr ((ord x) + y)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n = (desplazar x n) : (cifrar xs n)

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) n = (desplazar x (-n)) : (descifrar xs n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista p = cifrarListaAux p 0
cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (p:rp) n = (cifrar p n):(cifrarListaAux rp (n + 1))
--ARREGLADO

-- EJ 7
frecuencia :: String -> [Float]
frecuencia x = frecuenciaAux x (length x) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

-- cuento las minusculas en la palabra para despues sacar el porcentaje
cuantasMinusculas :: String -> Int
cuantasMinusculas "" = 0
cuantasMinusculas (x:xs) | esMinuscula x == True = 1 + cuantasMinusculas xs
                         | otherwise = cuantasMinusculas xs

-- cuento cuantas apariciones tiene una letra en una palabra
cuantasHay :: (Eq t) => t -> [t] -> Int
cuantasHay _ [] = 0
cuantasHay n (x:xs) | n == x = 1 + cuantasHay n xs
                    | otherwise = cuantasHay n xs

-- dada una frecuencia y su posicion, la agrega a la lista de frecuencias
agregarFrecuencia :: Float -> Int -> [Float] -> [Float]
agregarFrecuencia n m (x:xs) | m == 1 = (n*100) : xs
                             | otherwise = x : agregarFrecuencia n (m - 1) xs

-- agrega frecuencias una cantidad igual a la longitud de la palabra
frecuenciaAux :: String -> Int -> [Float] -> [Float]
frecuenciaAux (x:xs) n (y:ys) | n == 0 = (y:ys)
                              | otherwise = frecuenciaAux (xs ++ [x]) (n-1) (agregarFrecuencia (fromIntegral (cuantasHay x (x:xs))/fromIntegral(cuantasMinusculas (x:xs))) ((letraANatural x) + 1) (y:ys))

--Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente [] _ = (' ', 0)
cifradoMasFrecuente (x:xs) y | obtenerEnesimoElemento (frecuencia (x:xs)) ((ord x) - 96) == maximo (frecuencia (x:xs)) = (desplazar x y, maximo(frecuencia (x:xs)))
                             | otherwise = cifradoMasFrecuente (xs ++ [x]) y

maximo :: [Float] -> Float
maximo [] = 0
maximo [x] = x
maximo (x:xs) | x <= head xs = maximo xs
              | otherwise = maximo (x:(tail xs))


obtenerEnesimoElemento:: [t] -> Int -> t
obtenerEnesimoElemento (x:xs) n | n == 1 = x
                                | otherwise = obtenerEnesimoElemento xs (n-1)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado "" "" = True
esDescifrado "" _ = False
esDescifrado _ "" = False
esDescifrado (x1:[]) (y1:[]) = True
esDescifrado (x1:x2:xs) (y1:y2:ys) = (ord x1) - (ord y1) == ord(x2) - ord(y2) && esDescifrado (x2:xs) (y2:ys)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados [x] = []
todosLosDescifrados (x:xs) = duplasDescifradas (x:xs) (length (x:xs))

-- determina si dado un string, hay algun otro string que sea su descifrado
hayDescifrados :: String -> [String] -> Bool
hayDescifrados _ [] = False
hayDescifrados n (x:xs) | esDescifrado n x == True = True
                        | otherwise = hayDescifrados n xs

-- coloca al primer string con todos sus descifrados, luego lo saca de la lista y sigue hasta que la lista es vacia
duplasDescifradas :: [String] -> Int -> [(String, String)]
duplasDescifradas [x] _ = []
duplasDescifradas (x:y:ys) n | n == 1 = duplasDescifradas (y:ys) (length (y:ys))
                             | hayDescifrados x (y:ys) == False = duplasDescifradas (y:ys) (length (y:ys))
                             | esDescifrado x y == True = [(x, y),(y, x)] ++ duplasDescifradas (x: (ys++[y])) (n-1)
                             | otherwise = duplasDescifradas (x:(ys ++ [y])) (n-1)

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave clave n = primerosNCaracteres n (auxExpandir clave n)

--repite la clave hasta alcanzar o exceder n
auxExpandir :: String -> Int -> String
auxExpandir clave n
    | length clave >= n = clave
    | otherwise = clave ++ auxExpandir clave (n - length clave)

--toma los primeros n caracteres de una lista
primerosNCaracteres :: Int -> String -> String
primerosNCaracteres 0 _ = []
primerosNCaracteres _ [] = []
primerosNCaracteres n (x:xs) = x : primerosNCaracteres (n-1) xs

--si alguno necesita podria ver de hacerlas genericas

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere _ _ = "kdueciirqdv"

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
