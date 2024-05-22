import Test.HUnit
import Untitled

tests1 = test [
  "componentes repetidas" ~: (relacionesValidas [("ana", "ana")]) ~?= False,
  "tupla repetida" ~: (relacionesValidas [("ana", "pedro"), ("ana", "pedro")]) ~?= False,
  "tupla repetida invertida" ~: (relacionesValidas [("ana", "pedro"), ("pedro", "ana")]) ~?= False,
  "todas diferentes" ~: (relacionesValidas [("ana", "pedro"), ("ana", "carlos")]) ~?= True
  ]

tests2 = test [
  "lista vacia" ~: (personas []) ~?= [],
  "n repetido" ~: (personas [("ana", "pedro"), ("ana", "juan")]) ~?= ["pedro","ana","juan"],
  "una sola tupla" ~: (personas [("ana","pedro")]) ~?= ["ana","pedro"],
  "se repiten dos personas" ~: (personas [("ana","pedro"),("ana","juan"),("juan","carlos")]) ~?= ["pedro","ana","juan","carlos"]
  ]


tests3 = test  [
  "lista vacia" ~: (amigosDe "lol" []) ~?= [],
  "ningun amigo" ~: (amigosDe "marta" [("ana", "pedro"), ("ana", "juan")]) ~?= [],
  "un solo amigo" ~: (amigosDe "ana" [("ana","pedro"),("pedro","carlos")]) ~?= ["pedro"],
  "mas de un amigo" ~: (amigosDe "ana" [("ana","pedro"),("ana","juan"),("juan","carlos")]) ~?= ["pedro","juan"]
  ]

tests4 = test  [
  "lista vacia" ~: (personaConMasAmigos []) ~?= [],
  "una sola tupla" ~: (personaConMasAmigos [("ana", "pedro")]) ~?= "ana",
  "todos un amigo" ~: (personaConMasAmigos [("ana","pedro"),("juan","carlos")]) ~?= "ana",
  "mas de un amigo" ~: (personaConMasAmigos [("ana","pedro"),("ana","juan"),("marta","carlos")]) ~?= "ana",
  "empate con mas de un amigo" ~: (personaConMasAmigos [("ana","pedro"),("ana","juan"),("pedro","carlos")]) ~?= "ana" 
  ]

-- -- EJEMPLOS

-- USUARIO1 = "JUAN"
-- USUARIO2 = "NATALIA"
-- USUARIO3 = "PEDRO"

-- RELACION1_2 = (USUARIO1, USUARIO2)
-- RELACION1_1 = (USUARIO1, USUARIO1)
-- RELACION1_3 = (USUARIO1, USUARIO3)


-- -- FUNCIONES PARA TESTING, NO BORRAR
-- -- EXPECTANY PERMITE SABER SI EL ELEMENOT QUE DEVUELVE LA FUNCIÓN ES ALGUNO DE LOS ESPERADOS
-- EXPECTANY ACTUAL EXPECTED = ELEM ACTUAL EXPECTED ~? ("EXPECTED ANY OF: " ++ SHOW EXPECTED ++ "\N BUT GOT: " ++ SHOW ACTUAL)


-- -- SONIGUALES PERMITE VER QUE DOS LISTAS SEAN IGUALES SI NO IMPORTA EL ORDEN
-- QUITAR :: (EQ T) => T -> [T] -> [T]
-- -- REQUIERE X PERTENECE A Y
-- QUITAR X (Y:YS)
-- | X == Y = YS
-- | OTHERWISE = Y : QUITAR X YS

-- INCLUIDO :: (EQ T) => [T] -> [T] -> BOOL
-- INCLUIDO [] L = TRUE
-- INCLUIDO (X:C) L = ELEM X L && INCLUIDO C (QUITAR X L)

-- SONIGUALES :: (EQ T) => [T] -> [T] -> BOOL
-- SONIGUALES XS YS = INCLUIDO XS YS && INCLUIDO YS XS 
