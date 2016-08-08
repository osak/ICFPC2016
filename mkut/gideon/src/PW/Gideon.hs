module PW.Gideon where

import Data.ByteString.Builder
import Data.Ratio
import Data.Solution
import Geometry.Ratio
import System.Environment
import System.IO
import Text.Parsec.Prim
import Text.Parsec.Error
import Spell.AncestralRecall
import Spell.Divination
import Spell.Ponder
import Spell.TimeWalk
import Spell.TimeSpiral

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    case spell args of
        Left err -> hPrint stderr err
        Right sp -> case parse parseSolution "" input of
            Left err     -> hPrint stderr err
            Right result -> hPutBuilder stdout . buildSolution $ sp result

spell :: [String] -> Either ParseError (Solution -> Solution)
spell (cmd:args) = case cmd of
    "mov" -> fmap parallelWalk $ parse parsePoint "" (args !! 0)
    "rot" -> fmap reincarnate $ parse parsePoint "" (args !! 0)
    "fold" -> fmap cruise $ do
        a <- parse parsePoint "" (args !! 0)
        b <- parse parsePoint "" (args !! 1)
        return (a, b)
    "mirror" -> fmap scry $ do
        a <- parse parsePoint "" (args !! 0)
        b <- parse parsePoint "" (args !! 1)
        return (a, b)
    "foldr" -> fmap (uncurry cruise') $ do
        a <- parse parsePoint "" (args !! 0)
        b <- parse parsePoint "" (args !! 1)
        cs <- mapM (parse parsePoint "") (drop 2 args)
        return ((a, b), cs)
    _ -> return id