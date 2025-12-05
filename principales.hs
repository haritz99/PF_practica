-------------------------------------------------------------------------------
--  PRACTICA: Gestion de Catalogo de Contenido Digital     
--  PF  2025-2026

--  Num. del equipo registrado en la egela: Eq26
-- Apellidos del primer integrante: Galarraga Insausti
-- Apellidos del segundo integrante: Gomez Sarasola
-------------------------------------------------------------------------------
-- GRUPO C: Desarrollo sobre Series
-------------------------------------------------------------------------------
module Principales where
import Data.List (group, sort, (\\))    
import Auxiliares        
import Tipos
--
-- ====================================
-- Funciones principales sobre Series
-- ====================================

-- Titulo, Nº de Temporadas, y Edad minima de la serie, seguido de salto de linea
printSerie :: Serie -> String
printSerie (t, numTem, _, _, _, edadMin) = "Titulo: " ++ t ++ ", " ++ "Numero de Temporadas: " ++ show numTem ++ ", " ++ "Edad mínima recomendada: " ++ show edadMin ++ "\n"

-- Imprime la lista completa de canciones (playlist), formateada
printSeries :: [Serie] -> IO ()
printSeries = putStr .concat .map printSerie


-- 1
-- Dado un listado de series, calcula en numero de series por genero
-- incluido en el mismo
contarNumSeriesXGenero :: [Serie] -> [(GeneroS, Int)]
-- contarNumSeriesXGenero [] = []
-- contarNumSeriesXGenero xs = map (\g -> (head g, length g)) zs
--     where
--         zs = group(sort(map getGeneroS xs))
contarNumSeriesXGenero = map (\g -> (head g, length g)) . group . sort . map getGeneroS
--2	
-- Dada la edad y un listado de series, selecciona todas las series cuya edad
-- recomendada sea igual o superior a la dada
seriesParaMayoresDe :: Edad -> [Serie]-> [Serie]
seriesParaMayoresDe edad [] = []
seriesParaMayoresDe edad xs = filter (\y -> getEdad y >= edad) xs

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
rankingSeriesPorNumTotalEpisodios:: [Serie] -> [(Serie, Int)]
rankingSeriesPorNumTotalEpisodios [] = []
rankingSeriesPorNumTotalEpisodios xs = map (\x -> (x, getTemporadas x * getEpisodiosPorTemporada x)) zs
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


