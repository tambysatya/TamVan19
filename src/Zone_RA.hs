{-# LANGUAGE TemplateHaskell, MonadComprehensions #-}
module Zone_RA where
import           IloCplex
import Control.Lens hiding (Empty)
import qualified Data.Array as A
import Data.Ix
import Design
import Data.List hiding (zipWith, null)
import qualified Data.List as L (zipWith, null)

import Data.IORef
import Control.Monad
import Data.Sequence
import Prelude hiding (zipWith)

data Value = M | Val Double
	deriving (Show,Eq)
instance Ord Value where
	M `compare` M = EQ
	M `compare` _ = GT
	_ `compare` M = LT
	(Val x) `compare` (Val y) = x `compare` y

type Bound = A.Array Int Value
type GlobalBounds = (Bound,Bound)
type EvalFunction = (Bound,Bound) -> Bound -> (Int,Double)
data Conf = Conf {_cName :: String,
                  _cComputeNad :: Bool,
                  _cMonoLP :: Bool,
                  _cNaive :: Bool,
                  _cStatic :: Bool,
                  _cLexMinSolver :: Bool,
                  _cLexMinBuf :: Maybe (LexMinBuf,LexMinCallback),
                  _cEvalFun :: EvalFunction}


data SearchZone = SZ { _szUB :: Bound,
                       _szLB :: Bound,
                       _szVal :: (Int, Double),
                       _szDefiningPoints :: A.Array Int [IORef Point]}

instance Show SearchZone where show (SZ ub lb v _) = show (A.elems ub, A.elems lb, v)

makeLenses ''SearchZone



proj :: (Eq a, Ord a) => Int -> A.Array Int a -> [a]
proj k perf = [perf A.! i | i <- [b1..bp], i /= k]
    where (b1,bp) = A.bounds perf
  
definingPointP :: Int -> SearchZone -> Point -> Bool
definingPointP k zone pt = Val (perf A.! k) == u A.! k && and (L.zipWith (<) (Val <$> proj k perf) (proj k u))
  where perf = pointPerf pt
        u = _szUB zone

computeChild :: GlobalBounds -> EvalFunction -> Int -> SearchZone -> Point -> IORef Point -> IO SearchZone
computeChild bounds evalF i zone pt ptRef = do
	 newDefPts <- updatedDefiningPoints
	 pure $ SZ ui lb val $! newDefPts
  where perf = pointPerf pt
        ui = _szUB zone A.// [(i,Val $ perf A.! i)]
        defPoints = _szDefiningPoints zone
	updatedDefiningPoints :: IO (A.Array Int [IORef Point])
	updatedDefiningPoints = do
	 ret <- forM [1..p] $ \dim -> do
		if dim == i
			then pure [ptRef]
			else filterM (\ptiRef -> do
					pti <- readIORef ptiRef
					pure $ pointPerf pti A.! i < perf A.! i) (defPoints A.! dim)
	 pure $ A.listArray (1,p) $ ret

        val = evalF bounds ui
        lb = _szLB zone
	(_,p) = A.bounds perf
subdivideP :: GlobalBounds -> Point -> Int -> SearchZone -> IO Bool
subdivideP (yU,yI) pt k zone 
	| Val (pointPerf pt A.! k) == yI A.! k = pure False
	| otherwise =  and <$> (forM [i | i <- [1..p], i /= k, _szUB zone A.! i /= M ] $ \i -> do
		defpts <- sequence $ readIORef <$> (_szDefiningPoints zone A.! i)
		pure $ not $ L.null [pti | pti <- defpts, pointPerf pti A.! k < pointPerf pt A.! k])
	where (_,p) = A.bounds $ pointPerf pt
updateSearchRegion :: GlobalBounds -> EvalFunction -> (Seq SearchZone, Seq SearchZone) -> Point -> IO (Seq SearchZone, Seq SearchZone)
updateSearchRegion bounds evalF (setA,rest) pt = do
		ptRef <- newIORef pt
		newZones' <- ((forM setA $ \zi ->  updateZoneA zi ptRef)) :: IO (Seq (Seq SearchZone))
	        let updateZoneRest zone = let l = [i | i <- [1..p], definingPointP i zone pt] 
					  in if L.null l then zone 
						       else over (szDefiningPoints . ix (head l)) (ptRef:) zone 
		pure (join newZones', fmap updateZoneRest rest)
				
		
	where (_,p) = A.bounds (snd $ bounds)
	      updateZoneA :: SearchZone -> IORef Point -> IO (Seq SearchZone)	 
	      updateZoneA zi ptRef = do
			splitDirections <- filterM (\i -> subdivideP bounds pt i zi) [1..p]
			fromList <$> (forM splitDirections $ \i -> computeChild bounds evalF i zi pt ptRef)
computeHV :: (Bound,Bound) -> Bound -> Int -> Double
--computeHV (yU,yI) u k = - (sum $ L.zipWith (\ui yIi -> logBase 2 (ui - yIi)) (proj k u) (proj k yI))
computeHV (yU,yI) u k = - (sum [logBase 2 (v-yIi) | (i,ui, yIi) <- Prelude.zip3 (Prelude.filter (/=k) [1..p]) (proj k u) (proj k yI), 
						     let v = case ui of
								M -> yU A.! i
								Val x -> x ])


computeMaxHV :: EvalFunction
computeMaxHV bounds zone = minimumBy f [(k,computeHV bounds zone k) | k <- range $ A.bounds $ fst bounds]
  where f = curry $ compare <$> snd . fst <*> snd . snd

compute1HV :: EvalFunction
compute1HV bounds zone = (1,computeHV bounds zone 1)



isIn pt zone = and $ L.zipWith (<) (A.elems $ pointPerf pt) (A.elems $ _szUB zone)
ubIsIncludedLarge x y = and $ L.zipWith (<=) (A.elems x) (A.elems y)

fromValue :: GlobalBound -> Int -> Value -> Double
fromValue (yU,_) i M = yU A.! i
	
