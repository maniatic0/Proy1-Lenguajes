{-|
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
    -- * Manejo de Sabio
    SabioConocimiento,
    SabioState,
    sabioNewLaberinto,
    sabioCheckPreguntarRuta,
    mostrarLab,
    sabioPreguntarContinuarONuevo,
    sabioManejarRuta,
    sabioAbrirPared,
    -- * Lectura de Rutas
    RutaState,
    getRuta,
    getRutaStart
) where

import Data.Maybe
import Control.Monad.State 
import Text.Read hiding (lift, get)

import Laberinto
    
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  sabio <- execStateT sabioNewLaberinto (SabioConocimiento laberintoStart [])
  sabio2 <- execStateT sabioManejarRuta sabio
  sabio3 <- execStateT sabioAbrirPared sabio2
  print (lab sabio3)
  print (camino sabio3)


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

-- | Conocimientos del Sabio
data SabioConocimiento = SabioConocimiento { 
  -- | Laberinto que está pensando el Sabio
  lab :: Laberinto, 
  -- | Laberinto que se le dijo al Sabio
  camino :: Ruta
  }

-- | Monad State para manejar el conocimiento del sabio
type SabioState = StateT SabioConocimiento IO ()

-- | Comenzar a hablar de un laberinto nuevo: Si esta opción es seleccionada, 
-- se reemplaza el laberinto en memoria con un laberinto vacío y se pide una ruta 
-- que puede ser seguida en el mismo. Se usa esta ruta para poblar el laberinto inicialmente. 
-- La ruta debe suponerse terminada en un camino sin salida y no contener tesoros.
sabioNewLaberinto :: SabioState
sabioNewLaberinto = do
  lift $ putStrLn "Comenzando Laberinto nuevo.\nPor favor indique Ruta inicial."
  ruta <- lift $ execStateT getRutaStart []
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
        lift $ putStrLn "El Sabio regunta por una Ruta a seguir: "
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

          


        
