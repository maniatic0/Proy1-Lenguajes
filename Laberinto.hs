module Laberinto () where

import Data.Maybe
import Data.String
import Text.Read


data Laberinto = Tesoro { desc :: String
                        , fwd :: Maybe Laberinto 
                        }
                | Trifurcacion { lft :: Maybe Laberinto
                               , fwd :: Maybe Laberinto
                               , rgt :: Maybe Laberinto
                               }
                deriving (Show, Read)