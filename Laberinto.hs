{-|
Module      : Laberinto
Description : Modulo para manejar un Laberinto
Copyright   : (c) Christian Oliveros, 2018
                  David Rodriguez, 2018
License     : MIT
Maintainer  : sample@email.com
Stability   : experimental

Módulo para crear y manejar un Laberinto.
-}
module Laberinto (
  Laberinto,
  laberintoStart, 
  tesoroAdd
) where

import Data.Maybe
import Data.String
import Text.Read

-- | Conocimientos sobre un Laberinto
data Laberinto 
  -- | Tesoro oculto en un Laberinto
  = Tesoro {  -- | Descripción de un tesoro
              desc :: String
            -- | Laberinto indicando qué encontrarán si pasan por alto el tesoro
            , fwd :: Maybe Laberinto 
            }
  -- | Trifurcacion del Laberinto
  | Trifurcacion { -- | Laberinto indicando lo alcanzable al voltear a la izquierda, 
                   -- | o Nothing si no es posible voltear a la izquierda
                    lft :: Maybe Laberinto
                  -- | Laberinto indicando lo alcanzable al voltear a la derecha, 
                  -- | o Nothing si no es posible voltear a la derecha
                  , fwd :: Maybe Laberinto
                  -- | Laberinto indicando lo alcanzable al seguir a la recto, 
                  -- | o Nothing si no es posible seguir a la recto
                  , rgt :: Maybe Laberinto
                  }
    deriving (Show, Read)

-- | Función que genera una Trifurcación sin salida
laberintoStart :: Laberinto
laberintoStart = Trifurcacion Nothing Nothing Nothing

-- | Crea un tesoro, con su descripción y un Laberinto 
-- | indicando qué encontrarán si pasan por alto el tesoro
tesoroAdd :: String -> Laberinto -> Laberinto
tesoroAdd d l = Tesoro d (Just l)
