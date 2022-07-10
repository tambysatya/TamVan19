module Confs where

import Zone_RA
import Design (Point, pointPerf)
import qualified Data.Array as A
import Control.Lens
import Data.Ix
confMaxHV = Conf "dynamic" False False False False False Nothing computeMaxHV
monoConf conf = conf {_cMonoLP = True,
                      _cName = "scal_" ++ _cName conf}
