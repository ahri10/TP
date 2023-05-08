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

-- Predicados 

pertenece :: Eq t => t -> [t] -> Bool
pertenece x ys 
        | ys == [] = False
        | x == head ys = True 
        | otherwise = (pertenece x (tail ys)) 
        
mismoselementos :: Eq t => [t] -> [t] -> Bool
mismoselementos xs ys 
                | pertenece (head xs) ys && tail xs == [] = True
                | pertenece (head xs) ys == False && tail xs == [] = False
                | otherwise = (mismoselementos (tail xs) ys) && (pertenece (head xs) ys)

usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && (length (nombreDeUsuario u)) > 0
