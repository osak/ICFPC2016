module Spell.AncestralRecall where

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Solution
import Geometry.Ratio

cruise :: Segment -> Solution -> Solution
cruise seg@((a, b), (c, d)) sol = Solution ss2 fs2 ds2
    where
        ss = sources sol
        fs = facets sol
        ds = destinations sol
        ss2 = ss ++ newSs
        fs2 = concatMap cut fs
        ds2 = map refl $ ds ++ newDs
        idxSegs = map head . group . sort . map order . concat $ map pairLoop fs
        sSegs = map (\(x, y) -> (ss!!x, ss!!y)) $ idxSegs
        dSegs = map (\(x, y) -> (ds!!x, ds!!y)) $ idxSegs
        newPower = map (crossPower seg) dSegs
        newSs = mapMaybe f $ zip sSegs newPower
            where
                f (((x1, y1), (x2, y2)), p)
                    | 0 < p && p < 1 = Just (x1 + (x2 - x1) * p, y1 + (y2 - y1) * p)
                    | otherwise = Nothing
        newDs = mapMaybe f $ zip dSegs newPower
            where
                f (((x1, y1), (x2, y2)), p)
                    | 0 < p && p < 1 = Just (x1 + (x2 - x1) * p, y1 + (y2 - y1) * p)
                    | otherwise = Nothing
        cut fs
            | all (\x -> ccw seg (ds!!x) /= LT) fs || all (\x -> ccw seg (ds!!x) /= GT) fs = [fs]
            | otherwise = [ if ccw seg (ds!!b1) == EQ then b1 : cut2 GT st1 else idx2 a1 b1 : cut2 GT (b1:st1)
                          , if ccw seg (ds!!b2) == EQ then b2 : cut2 LT st2 else idx2 a2 b2 : cut2 LT (b2:st2)
                          ]
                where
                    fs1 = dropWhile (\x -> ccw seg (ds!!x) /= GT) $ cycle fs
                    (a1:b1:st1) = map fst $ dropWhile (\x -> ccw seg (ds!!snd x) == GT) $ zip fs1 (tail fs1)
                    fs2 = dropWhile (\x -> ccw seg (ds!!x) /= LT) $ cycle fs
                    (a2:b2:st2) = map fst $ dropWhile (\x -> ccw seg (ds!!snd x) == LT) $ zip fs2 (tail fs2)
        cut2 stp (f:f2:fs)
            | ccw seg (ds!!f2) == stp = [f, idx2 f f2]
            | ccw seg (ds!!f2) == EQ = [f, f2]
            | otherwise = f: cut2 stp (f2:fs)
        idx r = length ss + (fromJust $ findIndex (==r) newSs)
        idx2 i1 i2 = idx $ (x1 + (x2-x1) * po, y1 + (y2-y1) * po)
            where
                v1@(x1, y1) = ss!!i1
                v2@(x2, y2) = ss!!i2
                po = crossPower seg (ds!!i1, ds!!i2)
        refl (x, y) = if cross (c - a, d - b) (x - s, y - t) > 0 then (x, y) else (2 * s - x, 2 * t - y)
            where
                (s, t) = proj (x, y)
        proj (x, y) = (a + (c - a) * scale, b + (d - b) * scale)
            where
                scale = ((c - a) * (x - a) + (d - b) * (y - b)) / ((c - a)^2 + (d - b)^2)
        order (x, y) = if x < y then (x, y) else (y, x)

cross :: Point -> Point -> Ratio Integer
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

crossPoint :: Segment -> Segment -> Maybe Point
crossPoint ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = if abs a == 0
    then Nothing
    else Just (x3 + (x4 - x3) * b / a, y3 + (y4 - y3) * b / a)
    where
        a = cross (x2 - x1, y2 - y1) (x4 - x3, y4 - y3)
        b = cross (x2 - x1, y2 - y1) (x2 - x3, y2 - y3)

crossPower :: Segment -> Segment -> Ratio Integer
crossPower line seg@((x1, y1),(x2, y2)) = po
    where
        po = case crossPoint seg line of
            Just (px, py) -> dot (x2 - x1, y2 - y1) (px - x1, py - y1) / dot (x2 - x1, y2 - y1) (x2 - x1, y2 - y1)
            Nothing -> -1

dot :: Point -> Point -> Ratio Integer
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

inner :: Segment -> Point -> Bool
inner ((x1, y1), (x2, y2)) (x, y) = dot (x - x1, y - y1) (x - x2, y - y2) < 0

pairLoop :: [a] -> [(a, a)]
pairLoop xs@(x:_) = zip xs (tail xs ++ [x])

ccw :: Segment -> Point -> Ordering
ccw ((x1, y1), (x2, y2)) (x3, y3) = compare o 0
    where o = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)