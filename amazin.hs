import Text.Show.Functions
import Text.Show()


data Usuario = UnUsuario{
    nick :: String,
    felicidad :: Int,
    libros :: [Libro],
    leidos :: [Libro]

}  deriving (Eq, Show)

data Libro = UnLibro{
    titulo :: String,
    autor :: String,
    paginas :: Int,
    tipo :: Tipo,
    efecto :: Usuario -> Usuario

}  deriving (Eq, Show)

data Tipo = Cuento | NovelaCorta | Novela deriving (Eq, Show)

comediaDramatica :: Usuario -> Usuario
comediaDramatica usuario = usuario

comediaAbsurda:: Usuario -> Usuario
comediaAbsurda usuario = usuario {felicidad = felicidad usuario + 5}

comediaSatirica :: Usuario -> Usuario
comediaSatirica usuario = usuario {felicidad = felicidad usuario * 2}

comedia :: Usuario -> Usuario
comedia usuario = usuario {felicidad = felicidad usuario + 10}

sciFi:: Usuario -> Usuario
sciFi usuario = usuario {nick = reverse (nick usuario) }

terror :: Usuario -> Usuario
terror usuario = usuario {libros = [] }

leerLibro ::  Libro -> Usuario  -> Usuario
leerLibro libro usuario  
    |  libro `notElem` libros usuario = efecto libro (agregarLibro libro usuario)
    |  otherWise = usuario

agregarLibro :: Libro -> Usuario -> Usuario
agregarLibro libro usuario = usuario {leidos = libro : leidos usuario}

leerTodosLosLibros :: Usuario -> Usuario
leerTodosLosLibros  usuario = foldl (flip leerLibro) usuario (libros usuario)

esFanatica :: Usuario -> Bool
esFanatica usuario = sonTodosIguales (listaDeAutores (libros usuario))

listaDeAutores :: [Libro] -> [String]
listaDeAutores = map autor 

sonTodosIguales :: Eq a => [a] -> Bool
sonTodosIguales [] = True 
sonTodosIguales (x:xs) = all (== x) xs 

clasificarLibros :: Libro -> Libro
clasificarLibros libro
    | paginas libro < 100 = libro {tipo = Cuento}
    | paginas libro > 100 && paginas libro < 200 = libro {tipo = NovelaCorta}
    | otherwise = libro {tipo = Novela}

