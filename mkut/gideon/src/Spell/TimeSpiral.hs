module Spell.TimeSpiral where

import Data.Solution
import Geometry.Ratio

reincarnate :: Point -> Solution -> Solution
reincarnate (x, y) sol = Solution ss fs ds
    where
        ss = sources sol
        fs = facets sol
        ds = map (\(xx, yy) -> (x * xx - y * yy, y * xx + x * yy)) $ destinations sol