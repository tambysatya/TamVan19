{-# LANGUAGE TemplateHaskell #-}
module Stats where
import Control.Lens
import Design

data Stats = Stats {_statsCPUTime    :: Double,
            _statsRealTime   :: Double,
            
            _statsNbModelsSolved :: Int,
            _statsNbInfeasibleModels :: Int,
            
            _statsNbNonDominatedPoints :: Int,
            
            _statsNbKilledWithoutExploration :: Int}
    | RestrictStats {_statsCPUTime    :: Double,
                     _statsRealTime   :: Double,
            
                     _statsNbModelsSolved :: Int,
                     _statsNbInfeasibleModels :: Int,
                     _statsNbRedundant :: Int,
                     _statsNbLuckyPoint :: Int,

                     _statsRestrictCost :: Double,
                     _statsInfeasibleCost :: Double,
                     _statsRedundantCost :: Double,
            
                     _statsNbNonDominatedPoints :: Int,
            
                     _statsNbKilledWithoutExploration :: Int,
                     _statsLMax :: Int,
                     _statsLTotal :: Int,
                     _statsNbCreated :: Int,

                     _statsChronology :: [(String,Double,Maybe Point)],
                     _statsHypervolCurve :: [(Double,Double)],
                     
                     _statsNewPointIndexes :: [Int],
		     _statsNbTimesOptInSubcritDirection :: Int}


makeLenses ''Stats


initStats = Stats 0 0 0 0 0 0
initRestrictStats = RestrictStats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [] [] [] 0

instance Show Stats where
        show st@(Stats _ _ _ _ _ _) = show (_statsCPUTime st) ++ ";" ++ show (_statsRealTime st) ++ ";" ++ show (_statsNbModelsSolved st) ++ ";" ++ show (_statsNbInfeasibleModels st) ++ ";"
                ++ show (_statsNbNonDominatedPoints st) ++ ";" ++ show (_statsNbKilledWithoutExploration st)
        show st@(RestrictStats _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = 

                                                        {- Models solved -}
                                                        show (_statsNbNonDominatedPoints st) ++ ";" 
                                                        ++ show (_statsNbModelsSolved st) ++ ";" 
                                                        ++ show (_statsNbInfeasibleModels st) ++ ";"  ++ show (_statsNbRedundant st) ++ ";"
                                                        ++ show (_statsLMax st) ++ ";"
							++ show (frac (fromIntegral $ _statsLTotal st) (fromIntegral $ _statsNbModelsSolved st))
                                  where ndptCost = _statsRestrictCost st - _statsInfeasibleCost st - _statsRedundantCost st



frac a b = fromInteger (truncate ((a / b) * 100)) / 100
