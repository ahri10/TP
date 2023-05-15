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
proyectarNombres us | sinRepetidos listaNombres = listaNombres
                    | otherwise = sacarRepetidos listaNombres
                    where listaNombres = proyectarNombresAux us
-- proyectarNombres revisa si hay repetidos
proyectarNombresAux :: [Usuario] -> [String]
proyectarNombresAux [] = []
proyectarNombresAux (x:xs) = nombreDeUsuario x : proyectarNombres xs
-- proyectarNombresAux crea una lista con los nombres de usuario

-- Ejercicio 2
amigosDe :: RedSocial -> Usuario -> [Relacion]
amigosDe red us = sacarRelacionesRepetidas (listaAmigos)
                where listaAmigos = amigosDeAux (relaciones red) us

sacarRelacionesRepetidas :: [Relacion] -> [Relacion]
sacarRelacionesRepetidas [] = []
sacarRelacionesRepetidas (x:xs) | perteneceRelaciones x xs = sacarRelacionesRepetidas xs
                                | otherwise = x : sacarRelacionesRepetidas xs

perteneceRelaciones :: Relacion -> [Relacion] -> Bool
perteneceRelaciones _ [] = False
perteneceRelaciones rel (x:xs) | relacionesIguales rel x = True
                               | otherwise = perteneceRelaciones rel xs


relacionesIguales :: Relacion -> Relacion -> Bool
relacionesIguales (us1,us2) (us3,us4) = ((us1 == us3) && (us2 == us4)) || ((us1 == us4 && us2 == us3))


amigosDeAux :: [Relacion] -> Usuario -> [Relacion]
amigosDeAux [] _ = []
amigosDeAux ((r1,r2):xs) us | r1 == us || r2 == us = (r1,r2) : amigosDeAux xs us
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
usuarioConMasAmigosAux red (x:y:xs) | longitud (x:y:xs) == 1 = x
                                    | comparacionAmigos red x y == x = usuarioConMasAmigosAux red (x:xs)
                                    | comparacionAmigos red x y == y = usuarioConMasAmigosAux red (y:xs)
                                    
-- Ejercicio 5 (Falta un poco mas de testing) 

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = estaRobertoCarlosAux red (usuarios red)

estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux _ [] = False
estaRobertoCarlosAux red (x:xs) | (cantidadDeAmigos red x) > 10 = True
                            | otherwise = estaRobertoCarlosAux red xs
                            
-- ejercicio 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = sacarRepetidos (publicacionesDeAux (publicaciones red) us)


publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] _ = []
publicacionesDeAux (x:xs) us | usuarioDePublicacion x == us = x : publicacionesDeAux xs us
                             | otherwise = publicacionesDeAux xs us
--La funcion auxiliar crea la lista de publicaciones del usuario y publicacionesDe saca los repetidos

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

-- Predicados

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

-- Indica si una lista contiene los mismos elementos sin importar el orden ni las repeticiones              
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos l1 l2 = incluido l1 l2 && incluido l2 l1

-- Provee el elemento final de una lista             
terminacon :: [t] -> t
terminacon xs = head (reverse xs)

-- Verifica que tiene id y que tiene nombre 
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0

incluido :: Eq t => [t] -> [t] -> Bool
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


-- Publicaciones Validas

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = incluido (listaDeUsuariosPubs pubs) us && likeDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs

listadeIds :: [Usuario] -> [Integer]
listadeIds [] = []
listadeIds (x:xs) = idDeUsuario x : listadeIds xs

noHayIdsRepetidosaux :: [Integer] -> Bool
noHayIdsRepetidosaux (x:xs) = sinRepetidos (x:xs)

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos (x:xs) = noHayIdsRepetidosaux (listadeIds (x:xs))

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas pubs = idsListasSinRepetidos (listaDeLikes pubs) && noHayIdsRepetidos  (listaDeUsuariosPubs pubs)

idsListasSinRepetidos :: [[Usuario]] -> Bool
idsListasSinRepetidos [] = True
idsListasSinRepetidos (x:xs) = noHayIdsRepetidos x && idsListasSinRepetidos xs

listaDeLikes :: [Publicacion] -> [[Usuario]]
listaDeLikes [] = []
listaDeLikes (x:xs) = likesDePublicacion x : listaDeLikes xs

listaDeUsuariosPubs :: [Publicacion] -> [Usuario]
listaDeUsuariosPubs [] = []
listaDeUsuariosPubs (x:xs) = usuarioDePublicacion x : listaDeUsuariosPubs xs


likeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
likeDePublicacionSonUsuariosDeRed us [] = True
likeDePublicacionSonUsuariosDeRed us ((u,pub, likes) : pubs) = incluido likes us && likeDePublicacionSonUsuariosDeRed us pubs


--usuariosValidos
usuariosValidosAux :: [Usuario] -> Bool
usuariosValidosAux [] = True
usuariosValidosAux (x:xs) | usuarioValido (x) == False = False
                          | usuarioValido (x) = usuariosValidosAux (xs)

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = False
usuariosValidos us = usuariosValidosAux (us) && noHayIdsRepetidos (us)

empiezaCon :: [t] -> t
empiezaCon xs = head xs 

-- funcion de RedSocialValida
redSocialValida :: RedSocial -> Bool
redSocialValida (us,rs,ps) = usuariosValidos us && relacionesValidas us rs && publicacionesValidas us ps

-- Relaciones Validas

usuariosIguales :: Usuario -> Usuario -> Bool
usuariosIguales us1 us2 = idDeUsuario us1 == idDeUsuario us2 && nombreDeUsuario us1 == nombreDeUsuario us2


relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (x : xs)  | pertenece (reverseRelacion x) xs = False 
                                | otherwise = relacionesAsimetricas xs  


reverseRelacion :: Relacion -> Relacion
reverseRelacion (us1,us2) = (us2,us1)


usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos us (x : xs) = pertenece (fst x) us && pertenece (snd x) us && relacionValida x && usuariosDeRelacionValidos us xs


relacionValida :: Relacion -> Bool
relacionValida (us1,us2) | usuariosIguales us1 us2 = False
                         | otherwise = True

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas [] _ = False
relacionesValidas _ [] = False 
relacionesValidas us rs = relacionesAsimetricas rs && sinRepetidos rs && usuariosDeRelacionValidos us rs


-- Ejercicio 9 (NO COMPLETO)
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us = tieneUnSeguidorFielAux (usuarios red) us

{-tieneUnSeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
tieneUnSeguidorFielAux red (x:xs) us = (pertenece (x (likesDePublicacion (head (publicacionesDe red us)))))-}

tieneUnSeguidorFielAux :: RedSocial -> [Usuario] -> Bool
tieneUnSeguidorFielAux red (x:xs) | esFiel red x = True
                                  | xs /= [] = tieneUnSeguidorFielAux 
                                  | otherwise = False
esFiel :: RedSocial -> Usuario -> Bool
esFiel red us | longitud publicacionesDe red us == 0 = False
              | otherwise = (pertenece (us (likesDePublicacion (head (publicacionesDe red us)))))
