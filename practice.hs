import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    identificacion :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la u o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la u 
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idEstudiante tiempo estancia =
    Estudiante idEstudiante tiempo Nothing : estancia

-- Función para registrar la salida de un estudiante de la u
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEstudiante tiempo estancia =
    map (\v -> if idEstudiante == identificacion v then v { salida = Just tiempo } else v) estancia

-- Función para buscar un estudiante por su ID 
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEstudiante estancia =
    find (\v -> idEstudiante == identificacion v && isNothing (salida v)) estancia
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la u
tiempoEnLaU :: Estudiante -> IO NominalDiffTime
tiempoEnLaU estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarEstancia :: [Estudiante] -> IO ()
guardarEstancia estancia = do
    withFile "estancia.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante estancia))
    putStrLn "Estudiante guardado en el archivo estancia.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarEstancia :: IO [Estudiante]
cargarEstancia = do
    contenido <- withFile "estancia.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante identificacion entrada salida) =
    "Estudiante {identificacion = \"" ++ identificacion ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes 
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes con ingreso:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la estancia desde el archivo de texto
    estancia <- cargarEstancia
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal estancia

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estancia = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de un estudiante"
    putStrLn "2. Registrar salida de un estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el número de identificación del estudiante que ingresa:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let estanciaActualizada = registrarEntrada idEstudiante tiempoActual estancia
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " ingresado a la U."
            guardarEstancia estanciaActualizada
            cicloPrincipal estanciaActualizada

        "2" -> do
            putStrLn "Ingrese el número de identificación del estudiante que sale:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let estanciaActualizada = registrarSalida idEstudiante tiempoActual estancia
            putStrLn $ "Estudiante con ID " ++ idEstudiante ++ " salido de la U."
            guardarEstancia estanciaActualizada
            cicloPrincipal estanciaActualizada

        "3" -> do
            putStrLn "Ingrese el número de identificación del estudiante a buscar:"
            idEstudiante <- getLine
            case buscarEstudiante idEstudiante estancia of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnLaU estudiante
                    putStrLn $ "El estudiante con ID " ++ idEstudiante ++ " se encuentra en la U."
                    putStrLn $ "Tiempo en la U: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la U."
            cicloPrincipal estancia

        "4" -> do
            listarEstudiantes estancia
            cicloPrincipal estancia

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal estancia