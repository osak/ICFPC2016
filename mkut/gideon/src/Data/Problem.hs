{-# LANGUAGE FlexibleContexts #-}
module Data.Problem where

import Data.ByteString.Builder
import Data.Monoid
import Geometry.Ratio
import Milib.IO
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Problem = Problem
  { silhouette :: [[Point]]
  , skeleton :: [Segment]
  } deriving Show

parseProblem :: Stream s m Char => ParsecT s u m Problem
parseProblem = do
    n <- number
    sil <- count n $ do
        m <- number
        count m parsePoint
    k <- number
    skel <- count k parseSegment
    return $ Problem sil skel

buildProblem :: Problem -> Builder
buildProblem prob =
       intDec (length sil)
    <> charUtf8 '\n'
    <> foldMap (\pol
        -> intDec (length pol)
        <> charUtf8 '\n'
        <> foldMap (\p -> buildPoint p <> charUtf8 '\n') pol
        ) sil
    <> intDec (length skel)
    <> foldMap (\seg -> buildSegment seg <> charUtf8 '\n') skel
    where
        sil = silhouette prob
        skel = skeleton prob