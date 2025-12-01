module Auxiliares where
import Data.List (group, sort, (\\))
import Tipos
    ( DuracionM,
      EpisodiosXTemporada,
      NTemporadas,
      Edad,
      GeneroS(..),
      Serie,
      Titulo )

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


-- =============================
-- Resto de funciones auxiliares (para gestionar el catalogo de series)
-- ============================

-- Implementacion del quicksort por clave
qsortBy :: Ord b => (a -> b) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x:xs) = qsortBy f [ y | y <- xs, f y < f x]++
    (x: qsortBy f [ y | y <-  xs, f y == f x]++
        qsortBy f [ y | y <-  xs, f y > f x])



totalMinutosSerie :: Serie -> Int
totalMinutosSerie ( _, nTemporadas, episodiosXTemporada, duracionM, _, _ ) =
  nTemporadas * episodiosXTemporada * duracionM

getGeneroS' :: [GeneroS]
getGeneroS' = [Accion,Animacion,Comedia,Drama,Documental,SciFic,Suspense,Romance,Terror]

getGeneroS'' :: [Serie] -> [GeneroS]    
getGeneroS'' xs = map(\g -> head g) zs
   where
       zs = group(sort(map getGeneroS xs))