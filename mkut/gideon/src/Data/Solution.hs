{-# LANGUAGE FlexibleContexts #-}
module Data.Solution where

import Data.ByteString.Builder
import Data.Foldable
import Data.Monoid
import Data.Ratio
import Geometry.Ratio
import Milib.IO
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Solution = Solution
  { sources :: [Point]
  , facets :: [[Int]]
  , destinations :: [Point]
  } deriving Show

parseSolution :: Stream s m Char => ParsecT s u m Solution
parseSolution = do
    n <- number
    ss <- count n parsePoint
    m <- number
    fs <- count m $ do
        n <- number
        count n number
    ds <- count n parsePoint
    return $ Solution ss fs ds

buildSolution :: Solution -> Builder
buildSolution sol =
       intDec (length ss)
    <> charUtf8 '\n'
    <> foldMap (\p -> buildPoint p <> charUtf8 '\n') ss
    <> intDec (length fs)
    <> charUtf8 '\n'
    <> flip foldMap fs (\f
        -> intDec (length f)
        <> charUtf8 ' '
        <> stringUtf8 (unwords $ map show f)
        <> charUtf8 '\n'
        )
    <> foldMap (\p -> buildPoint p <> charUtf8 '\n') ds
    where
        ss = sources sol
        fs = facets sol
        ds = destinations sol
