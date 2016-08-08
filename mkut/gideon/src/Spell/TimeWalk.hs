module Spell.TimeWalk where

import Data.Solution
import Geometry.Ratio

parallelWalk :: Point -> Solution -> Solution
parallelWalk (x, y) sol = Solution ss fs ds
    where
        ss = sources sol
        fs = facets sol
        ds = map (\(xx, yy) -> (x + xx, y + yy)) $ destinations sol