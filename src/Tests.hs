module Tests where

import qualified Data.Array             as AB
import qualified Data.Array.Unboxed             as A
import           Data.List

import           Control.Monad
import           Control.Monad.State
import           Data.Time.Clock
import           System.CPUTime


import           Algorithm
import           Confs
import Zone_RA


import           IloCplex
import           LP

import           Design
import           Stats

buildDomain                        :: IloEnv -> Int -> Int -> [[Double]] -> [Double] -> [[Double]] -> [Double] -> IO Domain
buildDomain env n p values lbs weights ubs = do solVars <- forM [1..n] (return $ newIloObject env) :: IO [IloBoolVar]
                                                objVars <- forM [1..p] (return $ newIloObject env) :: IO [IloNumVar]
                                                matCtrs <- buildMatCtrs solVars
                                                objCtrs <- buildObjCtrs solVars objVars
                                                return $ (n,p,solVars,objVars,matCtrs ++ objCtrs)
  where buildMatCtrs solVars = forM (zip3 lbs weights ubs) $ \(lb,coefs,ub) -> do ct <- newIloObject env
                                                                                  setLB ct lb >> setUB ct ub
                                                                                  zipWithM (setLinearCoef ct) solVars coefs
                                                                                  return ct
        buildObjCtrs solVars objVars = zipWithM (buildObjCtr solVars) objVars values
        buildObjCtr solVars objvar coefs = do ct <- newIloObject env
                                              setLB ct 0 >> setUB ct 0
                                              setLinearCoef ct objvar (-1)
                                              zipWithM_ (setLinearCoef ct) solVars coefs
                                              return ct

readDomain :: String -> IloEnv -> IO Domain
readDomain fname env = do x <- readFile fname
                          let (p:n:objs:lb:ctrs:ub) = lines x
                          buildDomain env (read n) (read p) (read objs) (read lb) (read ctrs) (read $ concat ub)

mainloop conf env dom = do
        rt <- getCurrentTime
        cpu <- getCPUTime


        yLPts <- computeIdeal env dom
        --yUBoxed <- fmap fst <$> computeAntiIdeal env dom
        let yLBoxed = fst <$> yLPts
            yPts = snd <$> yLPts
	    bounds = A.bounds yLBoxed
	    yL = A.listArray bounds $ AB.elems yLBoxed

        ret <- restrictBuildNDPT conf (A.elems yPts) (Val <$> yL) env dom


        cpu' <- getCPUTime
        rt' <- getCurrentTime
        let cputime = (fromIntegral $ cpu' - cpu) / (fromIntegral $ 10^12)
            realtime = diffUTCTime rt' rt
            stats = _raStats ret
        return $ ret {_raStats = stats {_statsCPUTime = cputime,
                                             _statsRealTime = realToFrac realtime,
                                             _statsNbNonDominatedPoints = length (_raNDPT_List ret)}}

readVC :: String -> IloEnv -> IO Domain
readVC fname env = do x <- readFile fname
                      let (p1:nObj:rest) = lines x
                          costs = matFromLines (read nObj, read nObj) <$> (read (concat rest) :: [[[Double]]])
                          (p,n,w,ctrs,g,d) = buildVC (read p1) (read nObj) costs
                      buildDomain env n p w g ctrs d

readSatsVC :: String -> IloEnv -> IO Domain
readSatsVC fname env = do x <- read <$> readFile fname
                          let p = length x
                              (_,(nbObj,_)) = A.bounds $ head x
                              (_,n,w,ctrs,g,d) = buildVC p nbObj x
                          buildDomain env n p w g ctrs d



readKS :: String -> IloEnv -> IO Domain
readKS infile env = do x <- readFile infile
                       let (p:n:w:rest) = lines x
                           costs = map (map (*(-1))) $ read (unlines  $ init rest)
                           weights = read $ last rest
                       buildDomain env (read n) (read p) costs [0] [weights] [read w]

buildVC :: Int -> Int -> [A.Array (Int,Int) Double] -> (Int,Int,[[Double]], [[Double]], [Double], [Double])
buildVC p nbObj w = (p,nbObj*nbObj,weights,matToList ctrs, initLine,initLine)
  where
        weights = map (\ci ->  [ ci AB.! (i,j) | i <- [1..nbObj], j <- [1..nbObj] ]) w
        initLine = take (fromIntegral nbObj*2) $  repeat 1
        initMat = A.listArray ((1,1),(nbObj*2, nbObj*nbObj)) $ repeat 0
        matToList mat = [[mat AB.! (i,j) | j <- [1..nbObj*nbObj]] | i <- [1..nbObj*2]]
        ctrsL = [((i+1, i*nbObj +j),1) | i <- [0..nbObj-1], j<- [1..nbObj]]
        ctrsR = [((nbObj+j, (i-1)*nbObj+j) ,1) | j <- [1..nbObj], i <- [1..nbObj] ]
        ctrs = initMat AB.// (ctrsL ++ ctrsR)

readRandomMatrix infile env = do (p,n,costs,g,w,d) <- read <$> readFile infile
                                 buildDomain env n p costs g w d



matFromLines (n,p) l = A.array ((1,1),(n,p)) $ concat $ [[((i,j),vi) | (j,vi) <- zip [1..] li] | (i,li) <- zip [1..] l ]
readMultiDimKS :: String -> IloEnv -> IO Domain
readMultiDimKS infile env = do x <- readFile infile
                               let (p:n:w:rest) = lines x
                                   costs = map (map (*(-1))) $ read (unlines  $ init rest)
                                   weights = read $ last rest
                               buildDomain env (read n) (read p) costs (repeat 0) weights (read w)

readSparseDomain :: String -> IloEnv -> IO Domain
readSparseDomain fname env = do x <- readFile fname
				let (p_:n_:objs_:lbs_:ctrs_:ubs_) = lines x
				    (p,n,objs,lbs,ctrs,ubs) = (read p_, read n_, read objs_, read lbs_, read ctrs_, read $ unlines ubs_)
					
				(ovars, dvars) <- buildVars p n
				objsctrs <- buildObjCtrs objs ovars dvars
				matctrs <- buildDomCtrs lbs ctrs ubs dvars
				pure (n,p, A.elems dvars, ovars, matctrs ++ objsctrs)
	where buildVars :: Int -> Int -> IO ([IloNumVar], A.Array Int IloBoolVar)
	      buildVars p n = do
			(objvars, dvars) <- (,) <$> forM [1..p] (pure $ newIloObject env) <*> forM [1..n] (pure $ newIloObject env)
			pure (objvars, A.listArray (1,n) dvars)
	      buildExpr dvars coefs expr = forM coefs $ \(ai,xi) -> do
						setLinearCoef expr (dvars A.! xi) ai
	      buildObjCtrs :: [[(Double,Int)]] -> [IloNumVar] -> A.Array Int IloBoolVar -> IO [IloRange]
	      buildObjCtrs objcoefs ovars dvars = forM (zip objcoefs ovars) $ \(fi,oi) -> do
							ctr <- newIloObject env
							setLinearCoef ctr oi (-1)
							setBounds ctr (0,0)
							buildExpr dvars fi ctr
						        pure ctr

	      buildDomCtrs :: [Double] -> [[(Double,Int)]] -> [Double] -> A.Array Int IloBoolVar -> IO [IloRange]
	      buildDomCtrs lbs domcoefs ubs dvars = forM (zip3 lbs domcoefs ubs) $ \(lb,fi,ub) -> do
											ctr <- newIloObject env
											setBounds ctr (lb,ub)
											buildExpr dvars fi ctr
											pure ctr
							
	     {-
	      buildObjCtrs objcoefs ovars dvars = forM (zip objcoef ovars) $ \(fi, oi) -> do
							ctr <- newIloObject env
							setLinearCoef ctr oi (-1)
							forM_ fi $ \(xi, ci) -> do
								setLinearCoef 
-}
	      


