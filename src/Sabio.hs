{- |
Module      : Sabio
Description : Modulo de un Sabio que maneja un Laberinto
Copyright   : (c) Christian Oliveros, 2018
                  David Rodriguez, 2018
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental

Módulo para que un Sabio maneje un Laberinto.
-}
module Main (
    main,
    -- * Control de Sabio
    SabioConocimiento,
    SabioState,
    sabioMain,
    getOpcion,
    leerOpciones,
    -- * Manejo de Sabio
    sabioNewLaberinto,
    sabioCheckPreguntarRuta,
    mostrarLab,
    sabioPreguntarContinuarONuevo,
    sabioManejarRuta,
    sabioAbrirPared,
    sabioDerrumbe,
    sabioTomarTesoro,
    sabioHallarTesoro,
    sabioGenerarArchivoLab,
    sabioLeerArchivoLab,
    -- * Lectura de Indicadores
    getIndicador,
    getIndicadorStart,
    -- * Lectura de Rutas
    RutaState,
    getRuta,
    getRutaStart
) where

import Data.Maybe
import Control.Monad.State 
import Text.Read hiding (lift, get)
import System.IO
import System.Directory

import Laberinto
    

-- | Inicio del Sabio
main :: IO ()
main = do
  putStrLn "Sabio de Christian Oliveros y David Rodriguez"
  
  sabio <- execStateT sabioMain (SabioConocimiento laberintoStart [])
  putStrLn "Gracias por usar el Sabio de Christian Oliveros y David Rodriguez"

-- * Control de Sabio

-- | Conocimientos del Sabio
data SabioConocimiento = SabioConocimiento { 
  -- | Laberinto que está pensando el Sabio
  lab :: Laberinto, 
  -- | Laberinto que se le dijo al Sabio
  camino :: Ruta
  }

-- | Monad State para manejar el conocimiento del sabio
type SabioState = StateT SabioConocimiento IO ()

-- | Lectura de Opciones para el Sabio
getOpcion :: IO Int
getOpcion = do 
  linea <- getLine
  case readMaybe linea :: Maybe Int of
    Nothing -> do
      putStrLn ("Error leyendo Opción. Input Invalido: " ++ linea)
      getOpcion
    Just i ->
      case elem i [0..9] of
        False -> do
          putStrLn ("Error de Opción: Fuera de Rango. El rango aceptado es de 0 a 9. \
            \ Input Invalido: " ++ linea)
          getOpcion
        True -> return i

-- | Muestra las Opciones del Sabio
leerOpciones :: IO ()
leerOpciones = 
  putStrLn "Las Opciones para hablar con el Sabio son:\n \
  \\t0) Leer opciones.\n \
  \\t1) Comenzar a hablar de un laberinto nuevo.\n \
  \\t2) Preguntar ruta.\n \
  \\t3) Reportar pared abierta.\n \
  \\t4) Reportar derrumbe.\n \
  \\t5) Reportar tesoro tomado.\n \
  \\t6) Reportar tesoro hallado.\n \
  \\t7) Dar nombre al laberinto.\n \
  \\t8) Hablar de un laberinto de nombre conocido. \n \
  \\t9) Salir."

-- | Ciclo de Pensamientos del Sabio
sabioMain :: SabioState
sabioMain = do 
  lift $ leerOpciones
  separator
  lift $ putStrLn "¿Qué desea solicitarle al Sabio?"
  ops <- lift $ getOpcion
  case ops of 
    0 -> do
      --lift $ leerOpciones
      sabioMain
    1 -> do
      sabioNewLaberinto
      sabioMain
    2 -> do 
      sabioManejarRuta
      sabioMain
    3 -> do 
      sabioAbrirPared
      sabioMain
    4 -> do 
      sabioDerrumbe
      sabioMain
    5 -> do 
      sabioTomarTesoro
      sabioMain
    6 -> do 
      sabioHallarTesoro
      sabioMain
    7 -> do 
      sabioGenerarArchivoLab
      sabioMain
    8 -> do 
      sabioLeerArchivoLab
      sabioMain
    9 -> lift $ putStrLn "Saliendo"  
  where separator = lift $ putStrLn "-------------------------------------------------------\n"

-- * Lectura de Indicadores

-- | Manejo de obtención de Indicador de Dirección.
-- No Imprime Instrucciones de uso
getIndicador :: IO Indicador
getIndicador = do 
  linea <- getLine
  case readMaybe linea :: Maybe Indicador of
    Nothing -> do 
                putStrLn ("Error leyendo parte de Indicador. Input Invalido: " ++ linea)
                putStrLn "Indicadores válidos son: Izq, Rect, Der"
                getIndicador
    Just x -> return (x)

-- | Manejo de obtención de Indicador de Dirección
-- Imprime Instrucciones de uso
getIndicadorStart :: IO Indicador
getIndicadorStart = do
  putStrLn "Escriba 'Izq', 'Der' o 'Rect' para Indicar la dirección"
  getIndicador

-- * Lectura de Rutas

-- | Monad State para manejar lectura de una ruta
type RutaState = StateT Ruta IO ()

-- | Manejo de Lectura de una Ruta. 
-- No Imprime Instrucciones de uso y devuelve la ruta al revéz
getRuta :: RutaState
getRuta = do 
  linea <- lift getLine
  case linea == "" of 
    True -> lift $ putStr "Ruta Leida Completa: " 
    False -> do 
      case readMaybe linea :: Maybe Indicador of
        Nothing -> do lift $ 
                        putStrLn ("Error leyendo parte de Ruta. Input Invalido: " ++ linea)
                      getRuta
        Just x -> do modify (\xs -> x:xs)
                     r <- get
                     lift $ putStrLn ("Ruta actual invertida: " ++ show r)
                     getRuta

-- | Manejo de Lectura de una Ruta. 
-- Imprime Instrucciones de uso y devuelve la ruta de forma correcta
getRutaStart :: RutaState
getRutaStart = do 
  lift $ putStrLn msg
  getRuta
  modify reverse
  r <- get 
  lift $ print r
  where msg = "Escriba 'Izq', 'Der' o 'Rect' para indicar una seccion de la ruta.\
              \ Luego presione enter para insertarlo en la ruta.\n\
              \Para indicar fin de ruta, envie una linea vacía.\n\
              \La Lista mostrada esta en orden inverso al insertado"
 
-- * Manejo de Sabio

-- | Comenzar a hablar de un laberinto nuevo: Si esta opción es seleccionada, 
-- se reemplaza el laberinto en memoria con un laberinto vacío y se pide una ruta 
-- que puede ser seguida en el mismo. Se usa esta ruta para poblar el laberinto inicialmente. 
-- La ruta debe suponerse terminada en un camino sin salida y no contener tesoros.
sabioNewLaberinto :: SabioState
sabioNewLaberinto = do
  lift $ putStrLn "Comenzando Laberinto nuevo.\nPor favor indique Ruta inicial."
  ruta <- lift $ execStateT getRutaStart []
  lift $ putStrLn "Laberinto Creado"
  put $ SabioConocimiento (llenarRuta ruta) [] 

-- | Sabio revisa las condiciones para Preguntar la Ruta a seguir  
sabioCheckPreguntarRuta :: Laberinto -> Ruta -> Bool
sabioCheckPreguntarRuta _ [] = True
sabioCheckPreguntarRuta l r = 
  case seguirRuta l r of 
    Nothing -> False
    Just Tesoro {} -> False
    _ -> True

-- | Función para ayudar al sabio a describir el final de una ruta
mostrarLab :: Maybe Laberinto -> IO ()

mostrarLab Nothing =
  putStrLn "Se ha llegado a una Trifurcación sin salida."

mostrarLab (Just (Tesoro desc fwd)) = do 
  putStrLn ("Encontrado tesoro con descripción: " ++ desc)
  case fwd of
    Nothing -> putStrLn "Este tesoro esta en un cuarto sin forma de avanzar."
    Just _ ->  putStrLn "Este tesoro esta en un cuarto donde se puede avanzar."

mostrarLab (Just (Trifurcacion Nothing Nothing Nothing)) = 
  putStrLn "Se ha llegado a una Trifurcación sin salida."

mostrarLab (Just (Trifurcacion lft fwd rgt)) = do 
  putStrLn "Se ha llegado a una Trifurcación donde se puede ir a: "
  case lft of 
    Nothing -> putStr ""
    Just _ -> putStrLn "\tLa Izquierda"
  case fwd of 
    Nothing -> putStr ""
    Just _ -> putStrLn "\tPara Adelante"
  case rgt of 
    Nothing -> putStr ""
    Just _ -> putStrLn "\tLa Derecha"

-- | Función para ayudar al sabio a preguntar sobre los deseos de la persona
sabioPreguntarContinuarONuevo :: IO Int
sabioPreguntarContinuarONuevo= do 
  putStrLn "Se desea Continuar la Ruta (1) o Preguntar Ruta Nueva (2):"
  ans <- getLine
  case readMaybe ans :: Maybe Int of
    Nothing -> do 
      putStrLn "Opción Inválida"
      sabioPreguntarContinuarONuevo
    Just 1 -> do
          putStrLn "Se Continuará la Ruta"
          return 1
    Just  2 -> do
          putStrLn "Se Preguntará por la Ruta Nueva"
          return 2
    Just _ -> do 
          putStrLn "Opción Inválida"
          sabioPreguntarContinuarONuevo

-- | Función para que el sabio describa que pasa al seguir una ruta
sabioManejarRuta :: SabioState
sabioManejarRuta = do 
  sabio <- get
  let ruta = camino sabio
  let l = lab sabio
  case sabioCheckPreguntarRuta l ruta of 
    True -> do 
        lift $ putStrLn "El Sabio pregunta por una Ruta a seguir: "
        r <- lift $ execStateT getRutaStart []
        put $ SabioConocimiento l r
        lift $ mostrarLab $ seguirRuta l r 
    False -> do
      i <- lift $ sabioPreguntarContinuarONuevo
      case i of
        1 -> do 
          lift $ putStrLn "Continuación de Ruta: "
          cr <- lift $ execStateT getRutaStart []
          let r = ruta ++ cr 
          put $ SabioConocimiento l r
          lift $ mostrarLab $ seguirRuta l r 
        2 -> do 
          lift $ putStrLn "Ruta a seguir desde el Inicio de Laberinto: "
          r <- lift $ execStateT getRutaStart []
          put $ SabioConocimiento l r
          lift $ mostrarLab $ seguirRuta l r 

-- | Función para ayudar al Sabio a abrir una pared en el laberinto
sabioAbrirPared :: SabioState
sabioAbrirPared = do 
  lift $ putStrLn "Ruta a seguir para Abrir una Pared: "
  r <- lift $ execStateT getRutaStart []
  sabio <- get
  let ruta = camino sabio
  let l = lab sabio
  let cr = ruta ++ r
  case seguirRuta l cr of 
    Nothing -> do 
      lift $ putStrLn "La Ruta abrió una pared."
      put $ SabioConocimiento (abrirPared l cr) ruta
    _ -> lift $ putStrLn "La Ruta no abrió ninguna pared."

-- | Función para ayudar al Sabio a derrumbar una parte del laberinto
sabioDerrumbe :: SabioState
sabioDerrumbe = do 
  lift $ putStrLn "Ruta a seguir para Derrumbar una Trozo del Laberinto: "
  r <- lift $ execStateT getRutaStart []
  lift $ putStrLn "Dirección para Derrumbar una Pared: "
  indi <- lift $ getIndicadorStart
  sabio <- get
  let ruta = camino sabio
  let l = lab sabio
  let cr = ruta ++ r
  put $ SabioConocimiento (derrumbe l cr indi) ruta


-- | Función para ayudar al Sabio a tomar un Tesoro del Laberinto
sabioTomarTesoro :: SabioState
sabioTomarTesoro = do 
  lift $ putStrLn "Ruta a seguir para tomar un Tesoro del Laberinto: "
  r <- lift $ execStateT getRutaStart []
  sabio <- get
  let ruta = camino sabio
  let l = lab sabio
  let cr = ruta ++ r
  case seguirRuta l cr of
    Just Tesoro{desc=d} ->  do
      lift $ putStrLn ("Tesoro Tomado con descripción: " ++ d)
      put $ SabioConocimiento (fromJust (tomarTesoro l cr)) ruta
    _ -> lift $ putStrLn "No se ha encontrado un Tesoro para tomar."
          

-- | Función para ayudar al Sabio a hallar un Tesoro en el Laberinto
sabioHallarTesoro :: SabioState
sabioHallarTesoro = do 
  lift $ putStrLn "Ruta a seguir para hallar un Tesoro en el Laberinto: "
  r <- lift $ execStateT getRutaStart []
  lift $ putStrLn "Descripción del Laberinto hallado: "
  s <- lift $ getLine
  sabio <- get
  let ruta = camino sabio
  let l = lab sabio
  let cr = ruta ++ r
  case cr of
    [] -> lift $ putStrLn "Ruta vacía. No se aplicaran cambios."
    _ -> do
      case seguirRuta l cr of
        Just Tesoro{} -> lift $ putStrLn "Se ha encontrado un Tesoro, no se ha realizado cambio."
        Nothing -> do 
          lift $ putStrLn "Advertencia: se encontró una Pared. Tal vez no se aplicaran cambios."
          put $ SabioConocimiento (hallarTesoro s l cr) ruta
        _ ->  do
          lift $ putStrLn "Colocando Tesoro"
          put $ SabioConocimiento (hallarTesoro s l cr) ruta
        

-- | Función para Guardar la ruta leida en un archivo
sabioGenerarArchivoLab :: SabioState
sabioGenerarArchivoLab = do 
  lift $ putStrLn "Indique el nombre del archivo a Crear"
  archivo <- lift $ getLine
  x <- lift $ doesFileExist archivo
  case x of
    True ->  do
      lift $ putStrLn "El archivo ya existe, desea, reemplazarlo? (Y/N)"
      y <- lift $ getLine
      case y of 
        "Y" -> do 
          lift $ removeFile archivo
          sabio <- get
          let l = lab sabio
          lift $ writeFile archivo (show l)
        _ -> lift $ putStrLn "Archivo no eliminado, no se hará nada"
    _ -> do
      sabio <- get
      let l = lab sabio
      lift $ writeFile archivo (show l)
    


-- | Función para Generar un nuevo laberinto leido desde un archivo
sabioLeerArchivoLab :: SabioState
sabioLeerArchivoLab = do
  lift $ putStrLn "Indique el nombre del archivo a Leer"
  archivo <- lift $ getLine
  x <- lift $ doesFileExist archivo

  case x of
    True -> do
      l <- lift $ readFile archivo

      case readMaybe l :: Maybe Laberinto of 
        Just laberinto -> do 
          put $ SabioConocimiento laberinto []
          lift $ putStrLn "Archivo Cargado"
        _ -> lift $ putStrLn "Archivo contiene un laberinto inválido"

    _ -> lift $ putStrLn "El archivo no existe"

 