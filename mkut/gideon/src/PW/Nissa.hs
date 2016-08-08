{-# LANGUAGE FlexibleContexts #-}
module PW.Nissa where

import Control.Monad
import Data.ByteString.Builder
import Data.Problem
import Data.Ratio
import Data.Solution
import Geometry.Ratio
import System.IO
import Text.Parsec.Prim
import Spell.WorldlyTutor
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case mapM (parse parsePoint "") args of
        Left err     -> print err
        Right [a, b, c, d] -> hPutBuilder stdout . buildSolution $ summon a b c d