module Solucion where

-- Nombre de Grupo: PuntoDeEncuentro
-- Integrante 1: Astor Martinez Belizzi, astormbelizzi@gmail.com, 781/21
-- Integrante 2: Gonzalo Nicolas Moreira Valdez, gn.moreiravaldez@gmail.com, 712/23
-- Integrante 3: Iván Alejandro Miguel Viola, ivanmiguelviola@gmail.com, 711/23
-- Integrante 4: Andreas Pierre André Giacomini, andreasgiacomini2@gmail.com, 772/23

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas
usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicio 1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = sacarRepetidos listaNombres
                    where listaNombres = proyectarNombresAux us
-- proyectarNombres quita los repetidos de la lista de nombres de usuario

proyectarNombresAux :: [Usuario] -> [String]
proyectarNombresAux [] = []
proyectarNombresAux (x:xs) = nombreDeUsuario x : proyectarNombresAux xs
-- proyectarNombresAux crea una lista con los nombres de usuario

-- Ejercicio 2
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = amigosDeAux (relaciones red) us

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux ((r1,r2):xs) us | r1 == us = r2 : amigosDeAux xs us
                            | r2 == us = r1 : amigosDeAux xs us
                            | otherwise = amigosDeAux xs us

-- Ejercicio 3 

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos rc us = longitud (amigosDe rc us)

-- Ejercicio 4 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

comparacionAmigos :: RedSocial -> Usuario -> Usuario -> Usuario
comparacionAmigos red x y | (cantidadDeAmigos red x) > (cantidadDeAmigos red y) = x
                          | otherwise = y

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario     
usuarioConMasAmigosAux _ [x] = x
usuarioConMasAmigosAux red (x:y:xs) = usuarioConMasAmigosAux red (masAmigosxy : xs)                       
                                    where masAmigosxy = comparacionAmigos red x y

{- usuarioConMasAmigosAux compara quien tiene mas amigos entre los primeros 2 elementos y hace una recursion 
   mantniendo el mas grande en la cabecera. Al final de la recursion solo queda el usuario con mas amigos-}

-- Ejercicio 5 

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosAux red (usuarios red)

estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux _ [] = False
estaRobertoCarlosAux red (x:xs) | (cantidadDeAmigos red x) > 10 = True
                            | otherwise = estaRobertoCarlosAux red xs
                            
-- Ejercicio 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = publicacionesDeAux (publicaciones red) us


publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (x:xs) us | usuarioDePublicacion x == us = x : publicacionesDeAux xs us
                             | otherwise = publicacionesDeAux xs us
--La funcion auxiliar crea la lista de publicaciones del usuario

-- Ejercicio 7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = sacarRepetidos (publicacionesQueLeGustanAAux (publicaciones red) us)

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] _ = []
publicacionesQueLeGustanAAux (x:xs) us | pertenece us (likesDePublicacion x) = x : publicacionesQueLeGustanAAux xs us
                                       | otherwise = publicacionesQueLeGustanAAux xs us

-- Ejercicio 8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (mismoselementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2))

-- Ejercicio 9
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us | publicacionesDe red us /= [] = tieneUnSeguidorFielAux red (usuarios red) us
                           | otherwise = False

tieneUnSeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
tieneUnSeguidorFielAux red [] us = False
tieneUnSeguidorFielAux red (seguidor:xs) us | seguidor == us = tieneUnSeguidorFielAux red xs us
                                            | esFiel red us seguidor = True
                                            | otherwise = tieneUnSeguidorFielAux red xs us
-- Revisa si us tiene un seguidor fiel con una recursion y si lo encuentra devuelve True. 
-- Si termina la recursion sin encontrar uno da False

esFiel :: RedSocial -> Usuario -> Usuario -> Bool
esFiel red us seguidor = incluido (publicacionesDe red us) (publicacionesQueLeGustanA red seguidor)
-- seguidor es fiel si le dio like a todas las publicaciones del us

-- Ejercicio 10


-- Funciones auxiliares

-- Determina la longitud de una lista.
longitud :: (Eq t) => [t] -> Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

-- Indica si un valor de entrada es un elemento en una lista determinada.
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
        | x == y = True 
        | otherwise = pertenece x ys

sonAmigos :: Relacion -> RedSocial -> Bool
sonAmigos (u1, u2) red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)

-- Indica si una lista contiene los mismos elementos sin importar el orden ni las repeticiones              
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos l1 l2 = incluido l1 l2 && incluido l2 l1

-- Verifica si entre un x cualquiera y una lista, ese x es el elemento final de la lista
terminaCon :: Eq t => t -> [t] -> Bool
terminaCon y xs = y == ultimo xs

-- Verifica que tiene id y que tiene nombre 
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0

incluido :: Eq t => [t] -> [t] -> Bool
incluido [] _ = True
incluido (x:l1) l2 | pertenece x l2 = incluido l1 l2
                   | otherwise = False

sinRepetidos :: Eq t => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs = False
                    | otherwise = sinRepetidos xs
                    
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red us = incluido us (usuarios red)

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos [] = []
sacarRepetidos (x:xs) | pertenece x xs = sacarRepetidos xs
                      | otherwise = x : sacarRepetidos xs

-- Verifica si en la lista de usuarios hay relaciones en cadena, es decir us1 con us2, us2 con us3, us3 con us4 y asi sucesivamente
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = False
cadenaDeAmigos [x] _ = False
cadenaDeAmigos [x,y] red = sonAmigos (x,y) red
cadenaDeAmigos (x:y:xs) red | sonAmigos (x,y) red = cadenaDeAmigos (y:xs) red
                            | otherwise = False

quitar :: Eq t => t -> [t] -> [t]
quitar _ [] = []
quitar y (x:xs)
        | y == x = xs
        | y /= x = (x : (quitar y xs))

quitartodos :: Eq t => t -> [t] -> [t]
quitartodos y (x:xs) | pertenece y (x:xs) == True = quitartodos y (quitar y xs)
                     | otherwise = (x:xs)

empiezaCon :: Eq t => t -> [t] -> Bool
empiezaCon _ [] = False
empiezaCon y (x:xs) | (x == y) = True
                    | otherwise = False

ultimo :: [t] -> t
ultimo [] = undefined
ultimo [x] = x
ultimo (x:xs) = ultimo xs

quitarUltimo :: (Eq t) => [t] ->[t]
quitarUltimo ls = quitar (ultimo ls) ls

-------------------------------
{-
amigosDeRecursivo :: [Relacion] -> [Usuario] -> [[Usuario]]
amigosDeRecursivo [] _ = []
amigosDeRecursivo rel (x:xs) = amigosDeAux rel x : amigosDeRecursivo rel (xs)
-}

--sonAmigosMOD hace exactamente lo mismo que sonAmigos pero con las relaciones directamente
sonAmigosMOD :: Relacion -> [Relacion] -> Bool
sonAmigosMOD (u1, u2) rel = pertenece (u1,u2) rel || pertenece (u2,u1) rel
{-
-- relacionesDe junta en una lista todas las relaciones en las que participa el usuario que le sea solicitado.
relacionesDe :: [Relacion] -> Usuario -> [Relacion]
relacionesDe [] _ = []
relacionesDe ((r1,r2):xs) us | r1 == us || r2 == us = (r1,r2) : relacionesDe (xs) us
                             | otherwise = relacionesDe xs us
 -}
{-
-- quitarRecursiva le quita todos los elementos en una lista a otra lista.
quitarRecursiva :: Eq t => [t] -> [t] -> [t]
quitarRecursiva [] recipiente = recipiente
quitarRecursiva (x:xs) recipiente = quitarRecursiva xs (quitar x recipiente) -}

quitarRelacionesDe :: [Relacion] -> Usuario -> [Relacion]
quitarRelacionesDe [] _ = []
quitarRelacionesDe ((r1,r2):xs) u1 | r1 == u1 || r2 == u1 = quitarRelacionesDe xs u1
                                   | otherwise = (r1,r2) : quitarRelacionesDe xs u1

{- SECUENCIADEAMIGOS V2
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAux (relaciones red) u1 u2

existeSecuenciaDeAmigosAux :: [Relacion] -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigosAux rel u1 u2 | pertenece u2 (amigosDeAux rel u1) = True
                                     | amigosDeAux rel u2 == [] = False
                                     | otherwise = verificador relacionesFiltradas (amigosDeAux rel u1) u2
                                     where relacionesFiltradas = quitarRelacionesDe rel u1
 -}
 
 {- VERIFICADOR V2
verificador :: [Relacion] -> [Usuario] -> Usuario -> Bool
verificador  _ []  _ = False
verificador [] _  _ = False
verificador rel (x:xs) u2 | sonAmigosMOD (x,u2) rel = True
                          | otherwise = existeSecuenciaDeAmigosAux rel x u2 || verificador rel xs u2
  -}
 
{- SECUENCIADEAMIGOS V1
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = existeSecuenciaDeAmigosAux red u1 u2

existeSecuenciaDeAmigosAux :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigosAux red u1 u2 | pertenece u2 (amigosDe red u1) = True
                                     | otherwise = verificador (quitarRecursiva (relacionesDe (relaciones red) u1) (relaciones red)) (amigosDe red u1) u1 u2
 -}
 
 {- VERIFICADOR V1
verificador :: [Relacion] -> [Usuario] -> Usuario -> Usuario -> Bool
verificador [] _ _ _ = False
verificador rel (x:xs) u1 u2 | sonAmigosMOD (x,u2) rel = True
                             | otherwise = verificadortriple rel x u2 || verificador rel (xs) u1 u2 -}
 
 
 
{- Verifica primero si u1 y u2 son amigos, si no lo son procede a enviar todos los recursos necesarios a verificador,
el quitarRecursiva que aparece al principio esta para remover todas las relaciones en las que aparece u1 de la lista de rel a analizar.-}

{- Por que le di un doble rel a verificador? porque voy a utilizar el primer rel como el normal que utilizo para verificar si
u2 es amigo de cualquiera de los amigos de los amigos de u1, mientras que voy a usar el segundo rel como aquel al cual 
(en caso de que ninguno de los amigos de los amigos de u1 sea u2) le puedo quitar las rel que estan en u1, y asi por consecuente ir
descartando las relaciones que ya no sirven hasta que la lista de relaciones se vacie. Para quitar las relaciones, voy a implementar
una funcion quitar pero recursiva-} 

{- O AL MENOS ESE ERA MI PLAN AL PRINCIPIO, HASTA QUE:
Me di cuenta de que puedo hacer funciones que recuperen las relaciones que necesito, como es relacionesDe y directamente hacer verif
sin uso de red ni de dos rel, simplemente cuando los pase por verificadorDoble les quito las relaciones para cada caso, cosa de 
asegurarme de no sacar relaciones de mas.-}

-- Hice el sonAmigosMOD porque amigos de necesita de la red y yo quiero usar las relaciones sin las relaciones que ya escanee 
-- previamente, y no puedo hacer eso con relaciones red

-- SAQUE EL PRIMER REL EN SI, APARENTEMENTE NO NECESITO UN REL DESCARTABLE APARTE, CON TENER LOS REL DESDE UN PRINCIPIO BASTA

{- Bueno, la funcion de verificador que NO esta comentada funciona bien para los casos en los que deberia, pero para los que no me
termina tirando non-exhausitive patterns como error. Tambien deje otra version de verificador que es la que esta explicada arriba,
pero termine probando otra que funciona (la que esta sin comment) pero no para cuando tiene que parar, incluso aunque le voy sacando 
relaciones.-}

{- verificador :: [Relacion] -> [Usuario] -> Usuario -> Usuario -> Bool
verificador [] _ _ _ = False
verificador rel (x:xs) u1 u2 | sonAmigosMOD (x,u2) rel = True
                             | sonAmigosMOD (x,u2) rel == False && longitud (x:xs) == 1 = verificadorDoble rel 
                             | otherwise = verificador rel (xs) u1 u2 -}
-- SECUENCIA V3
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | sonAmigos (u1,u2) red = True
                                  | amigosDe red u2 == [] = False
                                  | otherwise = verificador rel (amigosDe red u1) u2
                                  where rel = relaciones red
 
-- VERIFICADOR V3
verificador :: [Relacion] -> [Usuario] -> Usuario -> Bool
verificador  _ []  _ = False
verificador [] _  _ = False
verificador rel (x:xs) u2 | sonAmigosMOD (x,u2) rel = True
                          | otherwise =  verificador rel xs u2 || verificador relsFiltradas (amigosDeAux rel x) u2
                          where relsFiltradas = quitarRelacionesDe rel x


{- verificadorDoble :: [Relacion] -> [[Usuario]] -> Usuario -> Usuario -> Bool
verificadorDoble rel (x:xs) u1 u2 | verificador rel x u1 u2 = True
                                  | otherwise = verificadorDoble rel xs u1 u2 -}
{- USAR PARA VERIFICADOR V1
verificadortriple :: [Relacion] -> Usuario -> Usuario -> Bool
verificadortriple rel u1 u2 = verificador (quitarRecursiva (relacionesDe (rel) u1) (rel)) (amigosDeAux rel u1) u1 u2
 -}

red1 = ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),(8,"h")],[((1,"a"),(2,"b")),((2,"b"),(3,"c")),((3,"c"),(4,"d"))], [])

red2 = ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),(8,"h")],[((1,"a"),(3,"c")),((2,"b"),(3,"c")),((4,"d"),(2,"b")),((6,"f"),(7,"g"))], [])

