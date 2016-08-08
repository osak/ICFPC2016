{-# LANGUAGE FlexibleContexts #-}
module PW.Jace where

import Data.ByteString.Builder
import Data.Problem
import Data.Ratio
import Data.Solution
import Geometry.Ratio
import System.IO
import Text.Parsec.Prim
import Spell.TimeWalk

main :: IO ()
main = do
    input <- getContents
    case parse probAndSol "" input of
        Left err     -> print err
        Right (prob, sol) -> hPutBuilder stdout . buildSolution $ parallelWalk (diff prob sol) sol

probLeft :: Problem -> Ratio Integer
probLeft prob = minimum $ map fst $ concat $ silhouette prob

probBottom :: Problem -> Ratio Integer
probBottom prob = minimum $ map snd $ concat $ silhouette prob

solLeft :: Solution -> Ratio Integer
solLeft sol = minimum $ map fst $ destinations sol

solBottom :: Solution -> Ratio Integer
solBottom sol = minimum $ map snd $ destinations sol

diff :: Problem -> Solution -> Point
diff prob sol = (probLeft prob - solLeft sol, probBottom prob - solBottom sol)

probAndSol :: Stream s m Char => ParsecT s u m (Problem, Solution)
probAndSol = do
    prob <- parseProblem
    sol <- parseSolution
    return (prob, sol)