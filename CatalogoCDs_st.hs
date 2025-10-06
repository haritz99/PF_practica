-------------------------------------------------------------------------------
--  PRACTICA: Gestion de Catalogo de Contenido Digital     
--  PF  2025-2026

--  Num. del equipo registrado en la egela: Eq26
-- Apellidos del primer integrante: Galarraga Insausti
-- Apellidos del segundo integrante: Gomez Sarasola
-------------------------------------------------------------------------------
-- GRUPO C: Desarrollo sobre Series
-------------------------------------------------------------------------------
module CatalogoCD where
import Data.List (group, sort, (\\))
--

type Titulo = String
type Serie = (Titulo, NTemporadas, EpisodiosXTemporada, DuracionM, GeneroS, Edad )
data GeneroS = Accion | Animacion | Comedia | Drama | Documental | SciFic | Suspense | Romance | Terror
    deriving (Show, Eq, Ord)
type Edad = Int                -- edad minima para consumir el contenido 
type NTemporadas = Int
type EpisodiosXTemporada = Int -- promedio
type DuracionM = Int           -- minutos, promedio de duracion de los episodios

-- ====================================
-- Funciones extractoras y auxiliares BASICAS: Series
-- ====================================


-- Extrae el titulo de la serie.
getTituloS :: Serie -> Titulo
getTituloS (t, _, _, _, _, _) = t
-- Extrae el num. de temporadas.
getTemporadas :: Serie -> NTemporadas
getTemporadas (_, n, _, _, _, _) = n

-- Extrae la duracion por episodio.
getDuracionEp :: Serie -> DuracionM
getDuracionEp (_, _, _, d, _, _) = d


-- Extrae el genero de la serie AV.
getGeneroS :: Serie -> GeneroS
getGeneroS (_, _, _, _, g, _) = g


-- Extrae la edad minima recomendada.
getEdad :: Serie -> Edad 
getEdad (_, _, _, _, _, e) = e

getEpisodiosPorTemporada :: Serie -> EpisodiosXTemporada
getEpisodiosPorTemporada (_, _, ept, _, _, _) = ept

-- Titulo, Nº de Temporadas, y Edad minima de la serie, seguido de salto de linea
printSerie :: Serie -> String
printSerie (t, numTem, _, _, _, edadMin) = "Titulo: " ++ t ++ ", " ++ "Numero de Temporadas: " ++ show numTem ++ ", " ++ "Edad mínima recomendada: " ++ show edadMin ++ "\n"

-- Imprime la lista completa de canciones (playlist), formateada
printSeries :: [Serie] -> IO ()
printSeries = putStr .concat .map printSerie


-- Implementacion del quicksort por clave
qsortBy :: Ord b => (a -> b) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x:xs) = qsortBy f [ y | y <- xs, f y < f x]++
    (x: qsortBy f [ y | y <-  xs, f y == f x]++
        qsortBy f [ y | y <-  xs, f y > f x])


-- ====================================
-- Funciones principales sobre Series
-- ====================================


-- 1
-- Dado un listado de series, calcula en numero de series por genero
-- incluido en el mismo
contarNumSeriesXGenero :: [Serie] -> [(GeneroS, Int)]
contarNumSeriesXGenero [] = []
contarNumSeriesXGenero xs = map (\g -> (head g, length g)) zs 
    where
        zs = group(sort(map getGeneroS xs))
--2	
-- Dada la edad y un listado de series, selecciona todas las series cuya edad
-- recomendada sea igual o superior a la dada
seriesParaMayoresDe :: Edad -> [Serie]-> [Serie]
seriesParaMayoresDe edad [] = []
seriesParaMayoresDe edad (x:xs)
    | getEdad(x) >= edad = x : seriesParaMayoresDe edad xs
    | otherwise = seriesParaMayoresDe edad xs

-- 3
-- Dado un numero de temporadas y un listado de series, extrae los títulos de 
-- lass series que tienen a los sumo ese numero de temporadas
titulosSconPocasTemporadas :: NTemporadas -> [Serie] -> [Titulo]
titulosSconPocasTemporadas nTemporadas [] = []
titulosSconPocasTemporadas nTemporadas (x:xs)
    | getTemporadas(x) >= nTemporadas = getTituloS(x) : titulosSconPocasTemporadas nTemporadas (xs)
    | otherwise = titulosSconPocasTemporadas nTemporadas (xs)

-- 4
-- Dado n el numero de series, dm la duracion maxima en minutos y un listado de 
-- series, selecciona n series del listado con duracion menor o igual a dm 
miSeleccionDeSeriesMasCortasQue :: Int -> DuracionM -> [Serie]-> [Serie]
miSeleccionDeSeriesMasCortasQue n dm [] = []
miSeleccionDeSeriesMasCortasQue n dm (x:xs)
    | n == 0 = []
    | getDuracionEp(x) <= dm = x : miSeleccionDeSeriesMasCortasQue (n-1) dm xs
    | otherwise = miSeleccionDeSeriesMasCortasQue n dm xs

-- 5
-- Dado un listado de series, determina la duración total (en minutos)
-- de todos los episodios de todas sus temporadas
totalMinutosCatalogo :: [Serie] -> DuracionM
totalMinutosCatalogo [] = 0
totalMinutosCatalogo (x:xs) = totalMinutosSerie(x) + totalMinutosCatalogo xs


-- 6
-- Dado un listado de series, identifica el genero (de series) con el más series
generoSMasProlifico :: [Serie] -> GeneroS
generoSMasProlifico [] = error ""
generoSMasProlifico xs = fst (last ys)
    where
        ys = qsortBy (\y -> snd y) zs
        zs = contarNumSeriesXGenero xs
-- 7	
-- Listado de series ordenado decrecientemente por número total de episodios
rankingSeriesPorNumTotalEpisodios:: [Serie] -> [(GeneroS, Int)]
rankingSeriesPorNumTotalEpisodios [] = []
rankingSeriesPorNumTotalEpisodios xs = map (\x -> (getGeneroS x, getTemporadas x * getEpisodiosPorTemporada x)) zs
   where 
    zs = reverse (qsortBy (\y -> getTemporadas y * getEpisodiosPorTemporada y) xs)
                    

-- 8 	
-- Listado de series ordenado crecientemente por duración total (en minutos), 
-- considerando todos los episodios de todas sus temporadas
rankingSeriesMasBreves:: [Serie] -> [(Serie, Int)]
rankingSeriesMasBreves xs = map (\x -> (x, totalMinutosSerie x)) zs 
    where 
        zs = qsortBy (\y -> totalMinutosSerie y) xs


-- 9
-- Dado un listado de series, identifica los generos (de serie) que NO estan 
-- representados (que faltan) con respecto al conjunto completo de generos definidos
generosSerieSinRepresentacion :: [Serie] -> [GeneroS]
generosSerieSinRepresentacion [] = []
generosSerieSinRepresentacion xs = getGeneroS' \\ getGeneroS'' xs

-- =============================
-- Resto de funciones auxiliares (para gestionar el catalogo de series)
-- ============================
totalMinutosSerie :: Serie -> Int
totalMinutosSerie ( _, nTemporadas, episodiosXTemporada, duracionM, _, _ ) =
  nTemporadas * episodiosXTemporada * duracionM

getGeneroS' :: [GeneroS]
getGeneroS' = [Accion,Animacion,Comedia,Drama,Documental,SciFic,Suspense,Romance,Terror]

getGeneroS'' :: [Serie] -> [GeneroS]
getGeneroS'' xs = map(\g -> head g) zs
   where
       zs = group(sort(map getGeneroS xs))
-- ======================================
-- Catalogos/Listados de ejemplos: Datos de prueba de series
-- ======================================

misSeries :: [Serie]
misSeries =
  [ ("Breaking Bad", 5, 13, 47, Drama, 18)
  , ("Rick y Morty", 6, 10, 22, Animacion, 16)
  , ("Friends", 10, 24, 22, Comedia, 12)
  , ("Stranger Things", 4, 8, 50, SciFic, 14)
  , ("The Office", 9, 24, 22, Comedia, 12)
  , ("Narcos", 3, 10, 50, Accion, 18)
  , ("Planet Earth", 1, 11, 50, Documental, 6)
  , ("Dark", 3, 8, 55, Suspense, 16)
  , ("Outlander", 7, 12, 60, Romance, 16)
  , ("The Haunting of Hill House", 1, 10, 50, Terror, 16)
  ]

