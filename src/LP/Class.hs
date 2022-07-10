module LP.Class where

import IloCplex
import Control.Monad
import Control.Monad.State
import IloCplex.IloObject.MIPIncumbentCallback

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

data LP = LP {lpEnv :: IloEnv,
              lpCpx :: IloCplex,
              lpMdl :: IloModel,
              lpObj :: IloObjective,
              lpConstraints :: [IloRange],
              lpNumVars :: [IloNumVar],
              lpBoolVars :: [IloBoolVar]}
              

type LPT = StateT LP 

newLP :: IloEnv -> IO LP
newLP env = do 
           (cpx,mdl,obj) <- (,,) <$> newIloObject env <*> newIloObject env <*> newIloObject env
           cpx `extract` mdl
           mdl `add` obj

           return $ LP env cpx mdl obj [] [] []

lptAdd :: (IloAddable a, MonadIO m) => a -> LPT m ()
lptAdd x = gets lpMdl >>= \m -> liftIO $ m `add` x
lptRemove :: (IloAddable a, MonadIO m) => a -> LPT m ()
lptRemove x = gets lpMdl >>= \m -> liftIO $ m `remove` x

lptSolve :: (MonadIO m) => LPT m Bool
lptSolve = gets lpCpx >>= \x -> liftIO $ solve x
lptPopulate :: (MonadIO m) => LPT m Bool
lptPopulate = gets lpCpx >>= \x -> liftIO $ populate x

lptValueOf :: (MonadIO m, IloVar a) => a -> LPT m Double
lptValueOf v = do 
	cpx <- gets lpCpx 
	ret <- liftIO $ cpx `getValue` v
	pure $! ret

lptObjValue :: (MonadIO m) => LPT m Double
lptObjValue = gets lpCpx >>= \x -> liftIO $ getObjValue x

lptNew :: (IloObject a, MonadIO m) => LPT m a
lptNew = gets lpEnv >>= \x -> liftIO $ newIloObject x

lptNew_      :: (IloObject a, MonadIO m) => Int -> LPT m [a]
lptNew_ k    = forM [1..k] $ return lptNew

lpAdd :: (IloAddable a) => LP -> a -> IO ()
lpAdd lp v = fst <$> runStateT (lptAdd v) lp

lpRemove :: (IloAddable a) => LP -> a -> IO ()
lpRemove lp v = fst <$> runStateT (lptRemove v) lp


lpSolve :: LP -> IO Bool
lpSolve lp = fst <$> runStateT lptSolve lp
lpPopulate lp = fst <$> runStateT lptPopulate lp

lpValueOf :: (IloVar a) => LP -> a -> IO Double
lpValueOf lp v = (fst <$> runStateT (lptValueOf v) lp) >>= (return $!)

lpObjValue :: LP -> IO Double
lpObjValue lp = fst <$> runStateT lptObjValue lp

lpNew :: (IloObject a) => LP -> IO a
lpNew lp = fst <$> runStateT lptNew lp


lptMaximize :: (MonadIO m) => LPT m ()
lptMaximize = do obj <- gets lpObj
                 liftIO $ setMaximize obj

lptMinimize :: (MonadIO m) => LPT m ()
lptMinimize = do obj <- gets lpObj
                 liftIO $ setMinimize obj


lptSetLexMin :: (MonadIO m) => (IloNumVar,IloRange) -> [IloNumVar]-> [IloBoolVar] -> [IloRange] -> LPT m (LexMinBuf,LexMinCallback)
lptSetLexMin chosen crits dvars ctrs = do cpx <- gets lpCpx
                                          env <- gets lpEnv
                                          liftIO $ setLexMin env cpx chosen crits dvars ctrs
