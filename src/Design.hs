module Design where

import qualified Data.Array as A
import Data.Maybe
import Control.Monad.State
import Control.DeepSeq

import IloCplex
import LP

type Perfs = A.Array Int Double

data Point = Point {-#UNPACK #-} !(A.Array Int Double) !(A.Array Int Double)
pointPerf (Point pt _) = pt
pointSol (Point _ pt) = pt
instance Show Point where 
        show pt = show (A.elems $ pointPerf pt)
instance Eq Point where
        pt1 == pt2 = pointPerf pt1 == pointPerf pt2
instance NFData Point where
	rnf (Point perf val) = A.elems perf `deepseq` A.elems val `deepseq` ()


type Domain = (Int,Int,[IloBoolVar],[IloNumVar],[IloRange])
type LPDomain = (Int,Int,[IloNumVar],[IloNumVar],[IloRange])

class Algorithm a where
        exploreSearchSpace :: (MonadIO m) => StateT a m (Maybe [Point])
        restrictSearchSpace :: (MonadIO m) => [Point] -> StateT a m ()


enumerateNDPT' :: (Algorithm a, MonadIO m) => StateT a m (Maybe ())
enumerateNDPT' = do pts <- exploreSearchSpace 
                    if isJust pts then restrictSearchSpace (fromJust pts) >> return (Just ())
                                  else return Nothing

enumerateNDPT :: (Algorithm a, MonadIO m) => StateT a m ()
enumerateNDPT = do retM <- enumerateNDPT'
                   unless (isNothing retM) enumerateNDPT



computeIdeal :: IloEnv -> Domain -> IO (A.Array Int (Double,Point))
computeIdeal env dom = computePerfs env dom setMinimize                                               
computeAntiIdeal env dom = computePerfs env dom setMaximize
computePerfs :: IloEnv -> Domain -> (IloObjective -> IO ()) -> IO (A.Array Int (Double, Point) )
computePerfs env dom sens = do lp <- newLP env
                               fst <$> runStateT (computePerfsLP env dom sens) lp
computePerfsLP :: IloEnv -> Domain -> (IloObjective -> IO ()) -> LPT IO (A.Array Int (Double, Point) )
computePerfsLP env (n,p,domVars, objVars,ctrs) sens = do 
                                                       (mdl,cpx,obj) <-  (,,) <$> gets lpMdl <*> gets lpCpx <*> gets lpObj
                                                       liftIO $ forM ctrs $ (mdl `add`)
                                                       liftIO $ sens obj

                                                       let compute = do 
                                                                        liftIO $ forM objVars $ \oi ->
                                                                                              do 
                                                                                                 setLinearCoef obj oi 1
                                                                                                 r <- solve cpx
                                                                                                 ret <- cpx `getValue` oi
                                                                                                 pt <- getPoint cpx objVars domVars p n r
                                                                                                 setLinearCoef obj oi 0
                                                                                                 return (ret,fromJust pt)
                                                       
                                                       yL <- A.listArray (1,p) <$> compute 
                                                       liftIO $ putStrLn $ show (fst <$> A.elems yL)
                                                       return yL
         where extractPerfs cpx ovars p = A.listArray (1,p) . map (realToFrac.round) <$> forM ovars ( cpx  `getValue`)
               extractSol cpx dvars n = A.listArray (1,n) . map (realToFrac . round) <$> forM dvars (cpx `getValue`)
               getPoint cpx ovars dvars p n True = Just <$> (Point <$> extractPerfs cpx ovars p <*> extractSol cpx dvars n)
               getPoint _ _ _ _ _ _ = pure Nothing
