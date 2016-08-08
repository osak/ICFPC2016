{-# LANGUAGE FlexibleContexts #-}
module Geometry.Ratio
    ( Point
    , buildPoint
    , parsePoint

    , Segment
    , buildSegment
    , parseSegment

    , buildRatio
    , parseRatio
    ) where

import Data.ByteString.Builder
import Data.Monoid
import Data.Ratio
import Milib.IO
import Text.Parsec.Prim
import Text.Parsec.Combinator

type Point = (Ratio Integer, Ratio Integer)
type Segment = (Point, Point)

buildRatio :: Ratio Integer -> Builder
buildRatio r
    | d == 1 = integerDec n
    | otherwise = integerDec n <> charUtf8 '/' <> integerDec d
    where
        n = numerator r
        d = denominator r

buildPoint :: Point -> Builder
buildPoint (x, y) = buildRatio x <> charUtf8 ',' <> buildRatio y

buildSegment :: Segment -> Builder
buildSegment (x, y) = buildPoint x <> charUtf8 ' ' <> buildPoint y

parseRatio :: Stream s m Char => ParsecT s u m (Ratio Integer)
parseRatio = try divided <|> integer
    where
        divided = do
            a <- number
            char '/'
            b <- number
            return $ a % b
        integer = do
            n <- number
            return $ fromIntegral n

parsePoint :: Stream s m Char => ParsecT s u m Point
parsePoint = do
    x <- parseRatio
    char ','
    y <- parseRatio
    return (x, y)

parseSegment :: Stream s m Char => ParsecT s u m Segment
parseSegment = do
    x <- parsePoint
    char ' '
    y <- parsePoint
    return (x, y)