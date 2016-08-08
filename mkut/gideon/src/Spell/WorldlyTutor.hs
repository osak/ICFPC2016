module Spell.WorldlyTutor where

import Data.Ratio
import Data.Solution
import Geometry.Ratio

-- summon source center p12 p4
summon :: Point -> Point -> Point -> Point -> Solution
summon (s, t) (x1, y1) (x2, y2) (x3, y3) = Solution ss fs ds
    where
        ss =  rot (-1%2, -1%2) (1%2, 1%2)
           ++ rot (s - 1%2, -1%2) (1%2, 1%2)
           ++ rot (-1%2, t - 1%2) (1%2, 1%2)
           ++ rot (s - 1%2, t - 1%2) (1%2, 1%2)
        fs = [ [0, 4, 12, 8]
             , [4, 9, 13, 12]
             , [9, 1, 5, 13]
             , [13, 5, 10, 14]
             , [10, 2, 6, 14]
             , [14, 6, 11, 15]
             , [15, 11, 3, 7]
             , [7, 8, 12, 15]
             , [12, 13, 14, 15]
             ]
        ds =  rot (x0 + x2 - x1, y0 + y2 - y1) (x1, y1)
           ++ rot (x4 + x2 - x1, y4 + y2 - y1) (x1, y1)
           ++ rot (x8 + x2 - x1, y8 + y2 - y1) (x1, y1)
           ++ rot (x2 - x1, y2 - y1) (x1, y1)
        v4@(x4, y4) = (x3 - x2, y3 - y2)
        v8@(x8, y8) = (y4 * s / t, -x4 * s / t)
        v0@(x0, y0) = (x4 + x8, y4 + y8)

rot :: Point -> Point -> [Point]
rot (s, t) (x, y) =
    [ (x + s, y + t)
    , (x - t, y + s)
    , (x - s, y - t)
    , (x + t, y - s)
    ]