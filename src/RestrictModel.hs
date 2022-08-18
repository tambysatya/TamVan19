{-# LANGUAGE TemplateHaskell, GADTs #-}

module RestrictModel where

import           Design
import           IloCplex
import           LP

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Ix
import           Data.Maybe
import Data.List
import Zone_RA
import Control.DeepSeq

import qualified Data.Array.Unboxed             as A




data RestrictLP = RestrictLP { _rModel :: LP,
                               _rNbDecVar :: Int,
                               _rNbCrit :: Int,
                               _rObjVars :: A.Array Int IloNumVar,
                               _rSolVars :: A.Array Int IloBoolVar,
                               _rObjCtrs :: A.Array Int IloRange,
                               _rMinimizedCriterionIndex :: Int,
                               _rNumberOfWrongStartingSol:: Int}

makeLenses ''RestrictLP

type RestrictLPT = StateT RestrictLP

initRestrictLP :: IloEnv -> Int -> Int -> [IloBoolVar] -> [IloNumVar] -> [IloRange] -> IO RestrictLP
initRestrictLP env n p solvars (f1:objvars) ctrs = do
        lp <- newLP env
        forM_ ctrs (lp `lpAdd`)
        setMinimize $ lpObj lp

        setLinearCoef (lpObj lp) f1 1

        objCtrs <- forM objvars $ \vi -> do r <- lpNew lp
                                            lp `lpAdd` r
                                            setLinearCoef r vi 1
                                            return r
        f1Ctr <- do r <- lpNew lp
                    setLinearCoef r f1 1
                    pure r
        let objtab = A.listArray (1,p) (f1:objvars)
            soltab = A.listArray (1,n) solvars
            ctrObjTab = A.listArray (1,p) (f1Ctr:objCtrs)
        return $ RestrictLP (lp{lpConstraints=ctrs})  n p objtab soltab ctrObjTab 1 0


favour :: (MonadIO m) => Int ->  RestrictLPT m ()
favour k = do pi <- use rMinimizedCriterionIndex
              when (pi /= k) $ do
                  rMinimizedCriterionIndex .= k
                  lp <- use rModel
                  objCtrTab <- use rObjCtrs
                  objTab <- use rObjVars
                  liftIO $ do setLinearCoef (lpObj lp) (objTab A.! pi) 0
                              setLinearCoef (lpObj lp) (objTab A.! k) 1

                              lp `lpAdd` (objCtrTab A.! pi)
                              lp `lpRemove` (objCtrTab A.! k)

{- Returns a new nondominated point (with bigM) -}

initMono :: (MonadIO m) => RestrictLPT m ()
initMono = do crits <- use rObjVars
              lp <- use rModel
              forM_ crits $ \ci -> liftIO $ setLinearCoef (lpObj lp) ci 1
initNaive :: (MonadIO m) => RestrictLPT m ()
initNaive = do obj <- use rObjCtrs
               lp <- use rModel
               liftIO $ lp `lpAdd` (obj A.! 1)
               initMono

{-| Set the favorized criterion in the scalarized case |-}
favour' :: (MonadIO m) => (A.Array Int Double,A.Array Int Double) -> Int ->  RestrictLPT m ()
favour' (ub,lb) k = do
              pi <- use rMinimizedCriterionIndex
              do
                  rMinimizedCriterionIndex .= k
                  lp <- use rModel
                  objCtrTab <- use rObjCtrs
                  objTab <- use rObjVars
                  liftIO $ do setLinearCoef (lpObj lp) (objTab A.! pi) 1
                              setLinearCoef (lpObj lp) (objTab A.! k) $ 1+sum [abs (ub A.! i - lb A.! i - 1) | i  <- range (A.bounds ub), i /= k]

                              lp `lpAdd` (objCtrTab A.! pi)
                              lp `lpRemove` (objCtrTab A.! k)

{-| Returns the objective value of a point (scalarized version) \Pi'(k,u) |-}
scalObjValue :: Bound -> Int -> Point -> Double
scalObjValue yI k pt = (weightK * (pointPerf pt A.! k)) + others
	where (_,p) = A.bounds yI
	      weightK = 1+sum [abs (pointPerf pt A.! i - fromVal (yI A.! i) - 1) | i <- [1..p], i /= k]
	      others = sum [pointPerf pt A.! i | i <- [1..p], i /= k]

restrictExplore :: (MonadIO m) => Maybe Point -> [Value] -> RestrictLPT m (Maybe Point)
restrictExplore ptM lUB = do 
                         lp <- use rModel
                         (p,n) <- (,) <$> use rNbCrit <*> use rNbDecVar
                         (dvars,ovars) <- (,) <$> use rSolVars <*> use rObjVars
                         (pi,objCtrs) <- (,) <$> use rMinimizedCriterionIndex <*> use rObjCtrs

                         when (isJust ptM) $ liftIO $ putStrLn $ "starting from : " ++ show (A.elems $ pointPerf $ fromJust ptM)


                         liftIO $ when (isJust ptM) $ do
                                         mip <- lpNew lp
                                         zipWithM_ (editMIPStart mip) (A.elems dvars) (A.elems $ pointSol $ fromJust ptM)
                                         addMIPStart (lpCpx lp) mip

			 let (bounded, unbounded) = partition ((/= M) . snd) . filter ((/= pi) . fst) $ zip [1..p] lUB

			 ret <- liftIO $ do 
				forM bounded $ \(k,vi) -> setUB (objCtrs A.! k) (fromVal vi - 0.5)
				forM unbounded $ \(k,_) -> lp `lpRemove` (objCtrs A.! k)
				lpSolve lp >>= getPoint lp ovars dvars p n
			 liftIO $ forM unbounded $ \(k,_) -> lp `lpAdd` (objCtrs A.! k)
			 pure ret
--                         liftIO $ do zipWithM (\k vi -> when (k/= pi) $ setUB (objCtrs A.! k) (vi - 0.5)) [1..p] lUB
--                                     lpSolve lp >>= getPoint lp ovars dvars p n
    where
          getPoint lp ovars dvars p n True = Just <$> (Point <$> extractPerfs lp ovars p <*> extractSol lp dvars n)
          getPoint _ _ _ _ _ _    = return Nothing
          extractPerfs lp ovars p = A.listArray (1,p) . map (realToFrac.round) <$> forM (A.elems ovars) ( lp  `lpValueOf`)
          extractSol lp dvars n = A.listArray (1,n) . map (realToFrac . round) <$> forM (A.elems dvars) (lp `lpValueOf`)

          restrictPred k ub ubi = and $ zipWith (<=) (proj k ubi) (proj k ub)

forceEval x = x `deepseq` x




