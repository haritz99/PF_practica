module IOPARSING where
import Tipos (Serie)
import Principales (printSerie, misSeries)
import Data.ByteString (toFilePath)


imprimirCatalogoConsola:: [Serie] -> IO ()
imprimirCatalogoConsola xs = do 
    putStrLn "Catálogo de Series: "
    mapM_ (\(i, s) -> putStr (show i ++ ". " ++ printSerie s)) (zip [1..] xs)
    