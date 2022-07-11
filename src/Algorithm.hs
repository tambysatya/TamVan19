{-# LANGUAGE TemplateHaskell #-}
module Algorithm where

import           Reopt
import           RestrictModel
import           Zone_RA

import           Design
import           Stats

import           IloCplex
import           LP

import           Control.Lens hiding (Empty, index)
import           Control.Monad.State hiding (filterM)
import qualified Data.Array.Unboxed as A
import qualified Data.Array as AB
import           Data.Ix hiding (index)
import qualified Data.List as L
import           Data.Maybe

import           Foreign.Storable

import           Data.Time.Clock
import           System.CPUTime
import           System.Exit
import qualified Data.Set as S
import Data.Semigroup
import Data.Array.Lens
import Data.IORef
import Data.Sequence 
import Data.Sequences (filterM)
import Prelude hiding (null, zipWith, partition, zip3, filter, drop, length)


data PointEntry = PointEntry {_peValue :: Double,
                              _pePoints :: [Point]}  
                 
instance Eq PointEntry where (PointEntry v _) == (PointEntry v' _) = v == v'
instance Ord PointEntry where (PointEntry v _) `compare` (PointEntry v' _) = v `compare` v'
instance Show PointEntry where show (PointEntry v l) = show v ++ " ===> " ++ show l

data ModelEntry = ME {_meOpt :: !Double,
                      _meModels :: ![[Double]]}
  deriving Show
instance Eq ModelEntry where (ME o _) == (ME o' _) = o == o'
instance Ord ModelEntry where (ME o _) `compare` (ME o' _) = o `compare` o'
instance Semigroup ModelEntry where (ME o l) <> (ME _ l') = ME o $ l ++ l'

data RestrictAlgorithm = RestrictAlgorithm { _raZones :: Seq SearchZone,
                                             _raNDPT_List :: [Point],

                                             _raRestrictModel :: RestrictLP,
                                             _raReoptModel :: AuxLP,

                                             _raSolvedModels :: AB.Array Int (S.Set ModelEntry),

                                             _raNad :: [Double],

                                             _raConf :: Conf,
                                             _raStats :: Stats}

makeLenses ''PointEntry
makeLenses ''RestrictAlgorithm

{- Patching interface -}

tree <<? val = (`S.elemAt` tree) <$> S.lookupIndex val tree


{- done -}

type RestrictAlgorithmT = StateT RestrictAlgorithm
restrictSolvedPred :: (MonadIO m) =>  SearchZone -> RestrictAlgorithmT m Bool
restrictSolvedPred sub = do 
    trs <- AB.elems <$> use raSolvedModels
    test <- and <$> (forM (L.zip3 trs [1..] (A.elems $ _szUB sub)) $ \(tr,k,uk) -> check tr k uk)
    pure $ test -- && test2
  where check tr k uk = case tr <<? (ME uk [])  of
                          Nothing -> pure True
                          Just (ME _ l) -> pure $ not $ or $ rmPred k <$> l
        rmPred k ui = and $ L.zipWith (<=) (projs A.! k) ui
        projs = AB.listArray (1,p) $ flip proj (_szUB sub) <$> [1..p]
        p = snd $ A.bounds (_szUB sub)

addNewModel :: (Monad m) => Int -> [Double] -> Double -> RestrictAlgorithmT m ()
addNewModel k ub yk = raSolvedModels . ix k %= addToTree
  where modelEntry = ME yk [ub]
        addToTree tr = case S.lookupIndex  modelEntry tr of
          Nothing -> S.insert modelEntry tr
          Just index -> S.insert  (modelEntry <> S.elemAt index tr) (S.deleteAt index tr)

restrictPred k ub ubi = and $ L.zipWith (<=) (proj k ubi) (proj k ub)


{-| Modèle infeasible |-}
onProjOptFailure :: MonadIO m => Int -> SearchZone -> RestrictAlgorithmT m ()
onProjOptFailure k ub = do zones <- use raZones
                           let (toDel,toKeep) = partition (restrictPred k (view szUB ub) . view szUB) zones
                           zoom raStats $ statsNbInfeasibleModels += 1
                           raZones .= toKeep


onProjOptSuccess :: MonadIO m => Conf -> Int -> SearchZone -> Seq SearchZone -> Point -> RestrictAlgorithmT m (Maybe (Point,Seq SearchZone, Seq SearchZone))
onProjOptSuccess conf k ub zrest pt 
    {- Point belongs to the zone -}
    | checkProj k ub pt = do pt' <- raReopt
                             let (setA,rest) = over _1 (ub:<|) $ computeSets zrest $ fromJust pt'

                             raZones .= setA >< rest
                             pure $ Just (fromJust pt',setA,rest)
    | otherwise = do zoom raStats $ statsNbRedundant += 1
                     zones <- use raZones
                     raZones .= zrest
                     defpts <- liftIO $ sequence $ readIORef <$> _szDefiningPoints ub A.! k
                     if L.null [defpt | defpt <- defpts, and $ L.zipWith (<=) (A.elems $ pointPerf defpt) (A.elems $ pointPerf pt)]
                       then do 
                               raStats.statsNbLuckyPoint += 1
                               pt' <- raReopt
                               let (setA,rest) = computeSets zrest $ fromJust pt'
                               pure $ Just  (fromJust pt',setA,rest)
                       else do
                             pure Nothing

 where
       checkProj k ub pt = pointPerf pt A.! k < _szUB ub A.! k
       computeSets zones pt' = partition (\zi -> and $ L.zipWith (<) (A.elems $ pointPerf pt') (A.elems $ _szUB zi)) zones
       raReopt
                  | _cMonoLP conf = pure $ Just pt
                  | otherwise = do
                            aux <- use raReoptModel
                            pt' <- liftIO $ reopt aux pt
                            pure $ Just pt'
       
minimize_rest :: (Ord a) => (x -> a) -> Seq x-> (x,Seq x)
minimize_rest ev l = foldr f (l `index` 0, Empty) (drop 1 l)
  where f el (ret,rest) = if ev el < ev ret then (el,ret :<| rest)
                                            else (ret,el :<| rest)
restrictAlgo :: MonadIO m => (A.UArray Int Double, A.UArray Int Double) ->  RestrictAlgorithmT m Bool
restrictAlgo (yU,yI)  = do
        zones <- use raZones
        conf <- use raConf
        let siz = length zones
        --liftIO $ print zones
        zoom raStats $ do statsLMax %= max siz
                          statsLTotal %= (+siz)
        logM $ "|U(N)| = " ++ show siz

        ndpt_list <- use raNDPT_List
        if null zones then pure False
                      else do
                        let 
                            (zone,zrest) = minimize_rest (snd ._szVal) zones
                            p = snd $ A.bounds yU
                            k = fst $ view szVal zone

                        mdl <- use raRestrictModel
                        defpts <- liftIO $ sequence $ readIORef <$> _szDefiningPoints zone AB.! k
                
                        (ret,cpu) <- time $ zoom raRestrictModel $ do
                                    let 
                                      sumComponnent pt = sum $ A.elems $ pointPerf pt
                                      cmpFun c1 c2 
                                        |_cMonoLP conf =  scalObjValue (yU,yI) k c1 `compare` scalObjValue (yU,yI) k c2
                                        | otherwise = sumComponnent c1 `compare` sumComponnent c2
                                      candidates = L.sortBy cmpFun [ni  | ni <- defpts]
                                    if _cMonoLP conf
                                      then favour' (_szUB zone, _szLB zone) k
                                      else favour k
                                    if L.null ndpt_list || _szUB zone A.! k == yU A.! k + 1 
                                      then restrictExplore Nothing $ A.elems $ _szUB zone
                                      else do 
                                            restrictExplore (Just $ head candidates) $ A.elems $ _szUB zone
                        case ret of
                            Nothing -> do
                                zoom raStats $ do statsInfeasibleCost += cpu
                                                  statsNbModelsSolved += 1
                                onProjOptFailure k zone
                                pure True
                            Just pt -> do
                                   ptM' <- onProjOptSuccess conf k zone zrest pt

                                   zonesUpdated <- use raZones
                                   if (isJust ptM') then
                                                        let (pt',a1,r) = fromJust $! ptM'
                                                        in do
                                                                (subs,rest) <- liftIO $ updateSearchRegion (yU,yI) (_cEvalFun conf) (a1,r) pt'
                                                                mdlIt <- zoom raStats $ use statsNbModelsSolved
                                                                zoom raStats $ statsNbModelsSolved += 1
                                                                logM $ "\t" ++ show pt'

                                                                raNDPT_List %= (pt':)
                                                                when (_cComputeNad conf) $ raNad %= L.zipWith max (A.elems $ pointPerf pt')
                                                                
                                                                addNewModel k (proj k $ _szUB zone) (pointPerf pt' A.! k)

                                                                nsubs <- if _cNaive conf then pure subs else filterM (restrictSolvedPred ) subs
                                                                
                                                                (raZones .=) $! rest >< nsubs 
                                                                pure True

                                                    else do zoom raStats $ do statsRedundantCost += cpu
                                                                              statsNbModelsSolved += 1
                                                            addNewModel k (proj k $ _szUB zone) (pointPerf pt A.! k)
                                                            pure True

    where
          logM :: MonadIO m => String -> RestrictAlgorithmT m ()
          logM = liftIO . putStrLn
          idealZone = (buildInitZone (yU,yI)){_szUB = yI}
          x `dom` y = and $ zipWith (<) x y
restrictBuildNDPT            :: Conf -> [Point] -> (A.UArray Int Double, A.UArray Int Double) -> IloEnv ->  Domain -> IO RestrictAlgorithm
restrictBuildNDPT conf initPts (ub,ideal) env (n,p,solvars,objvars,ctrs)  = do
    restrict <- initRestrictLP env n p solvars objvars ctrs
    auxReopt <- initReoptAux env n p solvars objvars ctrs
    let initZone = buildInitZone (ub,ideal)

    {- Initialize with the lexicographical points -}
    ndpt <- forM initPts $ reopt auxReopt
    print initPts


    let algo = RestrictAlgorithm (singleton initZone)  [] restrict auxReopt (A.listArray (1,p) $ repeat S.empty)  (A.elems ideal) conf initRestrictStats 


    print (A.elems ub, A.elems ideal)
    r <- snd <$> runStateT (do 
                          when (_cComputeNad conf) $ raNad .= computeNad ndpt
                          when (_cMonoLP conf) $ void $ runStateT initMono restrict
                          rec)
                           algo
    pure r
 where rec' lmc = do
                     liftIO $ resetDoubles lmc
                     r <- restrictAlgo (ub,ideal)
                     if r then rec'  lmc else pure ()
       rec = do r <- restrictAlgo (ub,ideal)
                if r
                  then rec
                  else do
                       pure ()
       n `domL` n' = and $ L.zipWith (<=) (A.elems $ pointPerf n) (A.elems $ pointPerf n')
buildInitZone :: (Bound,Bound) -> SearchZone
buildInitZone (ub,ideal) = SZ (A.listArray (1,p) ((+1) <$> A.elems ub)) ideal  (1,0) (AB.listArray (1,p) (repeat [])) 
  where p = snd $ A.bounds ub

time :: (MonadIO m) => m a -> m (a, Double)
time act = do cpu <- liftIO getCPUTime
              r <- act
              cpu' <- liftIO getCPUTime
              pure (r,(fromIntegral $ cpu' - cpu) / (fromIntegral $ 10^12))

computeNad pts = foldr1 (L.zipWith max) $ A.elems . pointPerf <$> pts
fst3 (x,_,_) = x

