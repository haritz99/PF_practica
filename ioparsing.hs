module IOPARSING where
import Tipos (Serie)
import Principales (printSerie, misSeries, seriesParaMayoresDe, rankingSeriesPorNumTotalEpisodios)
import Auxiliares (parseLinea)
import Text.Read (readMaybe)
import Data.ByteString (toFilePath)
import Data.Maybe (mapMaybe)

cargarCatalogoDesdeFichero:: FilePath -> IO [Serie]
cargarCatalogoDesdeFichero path = do
    contenido <- readFile path
    let lineas = lines contenido
    let contenidoparseado = mapMaybe parseLinea lineas
    return contenidoparseado

imprimirCatalogoConsola:: [Serie] -> IO ()
imprimirCatalogoConsola xs = do 
    putStrLn "Catálogo de Series: "
    mapM_ (\(i, s) -> putStr (show i ++ ". " ++ printSerie s)) (zip [1..] xs)

mainCatalogo:: IO ()
mainCatalogo = do
    putStrLn "Introduce el nombre del fichero con los datos: "
    fichero <- getLine

    catalogo <- cargarCatalogoDesdeFichero fichero

    putStrLn "\nCatálogo completo: "
    imprimirCatalogoConsola catalogo

    putStrLn "\nIntroduce la edad mínima para filtrar series: "
    edad <- getLine
    case readMaybe edad of
        Nothing -> putStr "Edad invalida"
        Just edad -> do
            let filtradas = seriesParaMayoresDe edad catalogo
            putStrLn $ "\nSeries para mayores de " ++ show edad ++ ":"
            imprimirCatalogoConsola filtradas

    let ranking = rankingSeriesPorNumTotalEpisodios catalogo
    putStrLn "\nRanking de series por número total de episodios (de mayor a menor):"
    mapM_ (\(i,(serie,totalEp)) -> 
             putStrLn $ show i ++ ". " ++ printSerie serie ++ " - Total Episodios: " ++ show totalEp
          ) (zip [1..] ranking)