module Spell.Divination where

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Solution
import Geometry.Ratio

scry :: Segment -> Solution -> Solution
scry seg@((a, b), (c, d)) sol = Solution ss fs ds2
    where
        ss = sources sol
        fs = facets sol
        ds = destinations sol
        ds2 = map refl2 ds
        refl2 (x, y) = (2 * s - x, 2 * t - y)
            where
                (s, t) = proj (x, y)
        proj (x, y) = (a + (c - a) * scale, b + (d - b) * scale)
            where
                scale = ((c - a) * (x - a) + (d - b) * (y - b)) / ((c - a)^2 + (d - b)^2)