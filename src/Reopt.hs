module Reopt where


import Control.Monad
import qualified Data.Array.Unboxed as A

import IloCplex
import LP
import Design


data AuxLP = AuxLP { auxModel       :: LP,
                     auxNbDecVars   :: Int,
                     auxNbCrit      :: Int,
                     auxObjVars     :: [IloNumVar],
                     auxSolVars     :: [IloBoolVar],
                     auxObjCtrs     :: [IloRange]}

extractPerfs aux = A.listArray (1,auxNbCrit aux) . map (realToFrac.round) <$> forM (auxObjVars aux) ( auxModel aux `lpValueOf`)
extractSol aux = A.listArray (1,auxNbDecVars aux) . map (realToFrac . round) <$> forM (auxSolVars aux) (auxModel aux `lpValueOf`)



initReoptAux :: IloEnv -> Int -> Int -> [IloBoolVar] -> [IloNumVar] -> [IloRange] -> IO AuxLP
initReoptAux env n p solvars objvars ctrs = do 
        lp <- newLP env
        forM_ ctrs (lp `lpAdd`)
        setMinimize $ lpObj lp
        forM_ objvars $ flip (setLinearCoef (lpObj lp)) 1
        objCtrs <- forM objvars $ \vi -> do r <- lpNew lp
                                            lp `lpAdd` r
                                            setLinearCoef r vi 1
                                            return r
        return $ AuxLP (lp{lpConstraints=ctrs})  n p objvars solvars objCtrs

reopt :: AuxLP -> Point -> IO Point 
reopt aux pt = do 
                  zipWithM setUB (auxObjCtrs aux) (A.elems $ pointPerf pt)
                  let val = head $ A.elems $ pointPerf pt
                  putStrLn $ "reoptimizing from:" ++ show pt
                  mip <- lpNew (auxModel aux)
                  zipWithM_ (editMIPStart mip) (auxSolVars aux) (A.elems $ pointSol pt)
                  addMIPStart (lpCpx $ auxModel aux) mip
                  ret <- lpSolve (auxModel aux)
                  if ret 
                    then Point <$> extractPerfs aux <*> extractSol aux
                    else pure pt



reoptPartial aux pt k = do
        setLinearCoef (lpObj $ auxModel aux) (auxObjVars aux !! (k-1)) 0
        ret <- reopt aux pt
        setLinearCoef (lpObj $ auxModel aux) (auxObjVars aux !! (k-1)) 0
        pure ret
