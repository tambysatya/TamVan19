module IloCplex.IloObject.Class where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad

import IloCplex.IloObject.IloEnv

class IloObject a where
        getImpl :: a -> ForeignPtr ()
        newIloObject_ :: Ptr () -> IO a

newIloObject :: (IloObject a) => IloEnv -> IO a
newIloObject (IloEnv e)  = withForeignPtr e newIloObject_ 

newIloObjects :: (IloObject a) => Int -> IloEnv -> IO [a]
newIloObjects k env = forM [1..k] $ return $ newIloObject env


