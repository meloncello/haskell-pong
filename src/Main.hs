module Main 
where

import Lib(playMultiPong)
import Graphics.Gloss

main = playMultiPong (300.0,300.0) black 60
