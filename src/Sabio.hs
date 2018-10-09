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
    laberintoNew,
    -- * Lectura de Rutas
    RutaState,
    getRuta,
    getRutaStart
) where

import Control.Monad.State 
import Text.Read hiding (lift, get)

import Laberinto
    
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  lab <- laberintoNew
  print lab


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
laberintoNew :: IO Laberinto
laberintoNew = do
  putStrLn "Comenzando Laberinto nuevo.\nPor favor indique Ruta inicial."
  ruta <- execStateT getRutaStart []
  return (foldr (\i l-> trifurcacionAdd laberintoStart l i) laberintoStart ruta)