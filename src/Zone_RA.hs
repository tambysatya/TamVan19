{-# LANGUAGE TemplateHaskell, MonadComprehensions #-}
module Zone_RA where
import           IloCplex
import Control.Lens hiding (Empty)
import qualified Data.Array as A
import Data.Ix
import Design
import Data.List hiding (zipWith, null)
import qualified Data.List as L (zipWith, null, partition)

import Data.IORef
import Data.Function
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
type EvalFunction = Bound -> Bound -> (Int,(Int,Double))
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
                       _szVal :: (Int, (Int,Double)),
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

computeChild :: Bound -> Int -> SearchZone -> Point -> IORef Point -> IO SearchZone
computeChild yI i zone pt ptRef = do
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

        val = (1,(1,0))
        lb = _szLB zone
	(_,p) = A.bounds perf
subdivideP :: Bound -> Point -> Int -> SearchZone -> IO Bool
subdivideP yI pt k zone 
	| Val (pointPerf pt A.! k) == yI A.! k = pure False
	| otherwise =  and <$> (forM [i | i <- [1..p], i /= k, _szUB zone A.! i /= M ] $ \i -> do
		defpts <- sequence $ readIORef <$> (_szDefiningPoints zone A.! i)
		pure $ not $ L.null [pti | pti <- defpts, pointPerf pti A.! k < pointPerf pt A.! k])
	where (_,p) = A.bounds $ pointPerf pt
updateSearchRegion :: Bound -> (Seq SearchZone, Seq SearchZone) -> Point -> IO (Seq SearchZone, Seq SearchZone)
updateSearchRegion yI (setA,rest) pt = do
		ptRef <- newIORef pt
		newZones' <- ((forM setA $ \zi ->  updateZoneA zi ptRef)) :: IO (Seq (Seq SearchZone))
	        let updateZoneRest zone = let l = [i | i <- [1..p], definingPointP i zone pt] 
					  in if L.null l then zone 
						       else over (szDefiningPoints . ix (head l)) (ptRef:) zone 
		pure (join newZones', fmap updateZoneRest rest)
				
		
	where (_,p) = A.bounds yI
	      updateZoneA :: SearchZone -> IORef Point -> IO (Seq SearchZone)	 
	      updateZoneA zi ptRef = do
			splitDirections <- filterM (\i -> subdivideP yI pt i zi) [1..p]
			fromList <$> (forM splitDirections $ \i -> computeChild yI i zi pt ptRef)
computeHV :: Bound -> Bound -> Int -> (Int,Double)
--computeHV (yU,yI) u k = - (sum $ L.zipWith (\ui yIi -> logBase 2 (ui - yIi)) (proj k u) (proj k yI))
computeHV yI u k = (- Prelude.length unbounded, - sum [logBase 2 (ui - yIi) | (Val ui, Val yIi) <- bounded])
	where (bounded, unbounded) = L.partition ((/= M). fst) $ Prelude.zip (proj k u) (proj k yI) 


computeMaxHV yI zone = minimumBy (compare `on` snd) [(k,computeHV yI zone k) | k <- range $ A.bounds yI ]

compute1HV yI zone = (1,computeHV yI zone 1)



isIn pt zone = and $ L.zipWith (<) (Val <$> (A.elems $ pointPerf pt)) (A.elems $ _szUB zone)
ubIsIncludedLarge x y = and $ L.zipWith (<=) (A.elems x) (A.elems y)

fromVal (Val v) = v
fromVal M = error "fromVal on unbounded value"
	
