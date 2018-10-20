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
  Laberinto(..),
  Indicador(..),
  Ruta(..),
  -- * Funciones de Construcción
  laberintoStart, 
  tesoroAdd,
  trifurcacionAdd,
  -- * Funciones de Acceso
  seguirRuta,
  seguirIzq,
  seguirRect,
  seguirDer,
  -- * Funciones de Modificación
  llenarRuta,
  abrirPared,
  derrumbe
) where

import Data.Maybe
import Data.String
import Text.Read

-- * Tipos de Datos

-- | Indicador de cuál camino relaciona una Trifurcacion y un Laberinto
data Indicador 
  = Izq
  | Rect
  | Der
  deriving ( Enum
           , Show
           , Read
           )

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
    deriving (Read, Show)

-- * Funciones de Construcción

-- | Función que genera una Trifurcación sin salida
laberintoStart :: Laberinto
laberintoStart = Trifurcacion Nothing Nothing Nothing

-- | Crea un tesoro, con su descripción y un Laberinto 
--  indicando qué encontrarán si pasan por alto el tesoro
tesoroAdd :: String -> Laberinto -> Laberinto
tesoroAdd d l = Tesoro d (Just l)

-- | Conecta una Trifurcación a un Laberinto siguiendo la dirección de un Indicador
trifurcacionAdd :: Laberinto -> Laberinto -> Indicador -> Laberinto
trifurcacionAdd Tesoro {} _ _ = error "Esta función es sólo para añadir Trifurcaciones"
trifurcacionAdd t l Izq = Trifurcacion {lft=Just l, fwd=fwd t, rgt=rgt t}
trifurcacionAdd t l Rect = Trifurcacion {lft=lft t, fwd=Just l, rgt=rgt t}
trifurcacionAdd t l Der = Trifurcacion {lft=lft t, fwd=fwd t, rgt=Just l}

-- * Funciones de Acceso

-- | Dado un Laberinto y una Ruta, devuelve el Laberinto alcanzado al seguir la Ruta
seguirRuta :: Laberinto -> Ruta -> Maybe Laberinto
seguirRuta l [] = Just l
-- Maybe es un monad, asi que lo aprovechamos
seguirRuta l (x:xs) = case x of 
  Izq -> seguirIzq l >>= seguir
  Rect -> seguirRect l >>= seguir
  Der -> seguirDer l >>= seguir
  where seguir lab = seguirRuta lab xs

-- | Dado un Laberinto, devuelve el Laberinto alcanzado al seguir a la Izquierda
seguirIzq :: Laberinto -> Maybe Laberinto
seguirIzq Tesoro {} = Nothing
seguirIzq Trifurcacion {lft=l} = l

-- | Dado un Laberinto, devuelve el Laberinto alcanzado al seguir Recto
seguirRect :: Laberinto -> Maybe Laberinto
seguirRect Tesoro {fwd=f} = f
seguirRect Trifurcacion {fwd=f} = f

-- | Dado un Laberinto, devuelve el Laberinto alcanzado al seguir a la Derecha
seguirDer :: Laberinto -> Maybe Laberinto
seguirDer Tesoro {} = Nothing 
seguirDer Trifurcacion {rgt=r} = r


-- * Funciones de Modificación

-- | Crea un laberinto siguiendo una ruta
llenarRuta :: Ruta -> Laberinto
llenarRuta rs = foldr (flip (trifurcacionAdd laberintoStart)) laberintoStart rs

-- | Seguir ruta para abrir una Pared en un Laberinto y continuar con el resto de la ruta.
-- Si no llega a pared, no hace nada
abrirPared :: Laberinto -> Ruta -> Laberinto
abrirPared l [] = l

abrirPared (Tesoro desc fwd) (Rect:xs) = 
  Tesoro desc $ (Just (maybe (llenarRuta xs) (\l -> abrirPared l xs) fwd))

abrirPared t@Tesoro{} (x:xs) = 
  case x of
    Izq -> Trifurcacion jl (Just t) Nothing
    Der -> Trifurcacion Nothing (Just t) jl 
  where jl = Just (llenarRuta xs)

abrirPared (Trifurcacion lft fwd rgt) (x:xs) = 
  case x of 
    Izq -> Trifurcacion (abrir lft) fwd rgt
    Rect -> Trifurcacion lft (abrir fwd) rgt
    Der -> Trifurcacion lft fwd (abrir rgt)
  where abrir lab = Just (maybe (llenarRuta xs) (\l -> abrirPared l xs) lab)


-- | Seguir ruta para derrumbar una Laberinto, de acuerdo a un Indicador, y continuar con el resto de la ruta.
-- Si el Indicador no es una dirección válida, no hace nada
derrumbe :: Laberinto -> Ruta -> Indicador -> Laberinto 

derrumbe Tesoro{desc=d}  [] Rect = Tesoro d Nothing
derrumbe t@Tesoro{}  [] _ = t

derrumbe (Tesoro desc fwd) (Rect:xs) i = Tesoro desc (fwd >>= (\l -> Just (derrumbe l xs i)))
derrumbe t@Tesoro{} (_:_) _ = t

derrumbe Trifurcacion{lft=lf, fwd=fd, rgt=rg} [] i = 
  case i of 
    Izq -> Trifurcacion Nothing fd rg 
    Rect -> Trifurcacion lf Nothing rg 
    Der -> Trifurcacion lf fd Nothing 

derrumbe Trifurcacion{lft=lf, fwd=fd, rgt=rg} (x:xs) i =
  case x of
    Izq -> Trifurcacion (lf >>= seguir) fd rg 
    Rect -> Trifurcacion lf (fd >>= seguir) rg 
    Der -> Trifurcacion lf fd (rg >>= seguir)
  where seguir lab = Just (derrumbe lab xs i)