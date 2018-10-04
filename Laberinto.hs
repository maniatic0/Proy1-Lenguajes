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
  -- * Tipos de Datos
  Laberinto,
  Indicador,
  Ruta,
  -- * Funciones de Construcción
  laberintoStart, 
  tesoroAdd,
  trifurcacionAdd
) where

import Data.Maybe
import Data.String
import Text.Read

-- | Indicador de cuál camino relaciona una Trifurcacion y un Laberinto
data Indicador 
  = Izq
  | Rect
  | Der
  deriving (Enum, Show, Read)

-- | Ruta hecha por Indicadores
type Ruta = [Indicador]

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

-- | Conecta una Trifurcación a un Laberinto siguiendo la dirección de un Indicador
trifurcacionAdd :: Laberinto -> Laberinto -> Indicador -> Laberinto
trifurcacionAdd (Tesoro _ _) _ _ = error "Esta función es sólo para añadir Trifurcaciones"
trifurcacionAdd t l Izq = Trifurcacion {lft=Just l, fwd=fwd t, rgt=rgt t}
trifurcacionAdd t l Rect = Trifurcacion {lft=lft t, fwd=Just l, rgt=rgt t}
trifurcacionAdd t l Der = Trifurcacion {lft=lft t, fwd=fwd t, rgt=Just l}