module Tipos where 

type Titulo = String
type Serie = (Titulo, NTemporadas, EpisodiosXTemporada, DuracionM, GeneroS, Edad )
data GeneroS = Accion | Animacion | Comedia | Drama | Documental | SciFic | Suspense | Romance | Terror
    deriving (Show, Eq, Ord, Read)
type Edad = Int                -- edad minima para consumir el contenido 
type NTemporadas = Int
type EpisodiosXTemporada = Int -- promedio
type DuracionM = Int           -- minutos, promedio de duracion de los episodios