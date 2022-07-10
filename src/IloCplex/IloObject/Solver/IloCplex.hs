{-# LANGUAGE TypeOperators,ForeignFunctionInterface, MagicHash #-}

module IloCplex.IloObject.Solver.IloCplex where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import IloCplex.IloObject.Class
import IloCplex.IloObject.IloModel
import IloCplex.IloObject.Variable
import GHC.Exts

foreign import ccall unsafe "cbits/IloCplex.cpp new_iloCplex"
    new_iloCplex :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloCplex.h &delete_iloCplex"
    delete_iloCplex :: FunPtr(Ptr () -> IO ())

foreign import ccall "include/IloCplex.h cpx_extract"
    cpx_extract :: Ptr () -> Ptr () -> IO ()
foreign import ccall "include/IloCplex.h cpx_solve"
    cpx_solve :: Ptr () -> IO CInt
foreign import ccall "include/IloCplex.h cpx_populate"
    cpx_populate :: Ptr () -> IO CInt
foreign import ccall unsafe "include/IloCplex.h cpx_getObjValue"
    cpx_getObjValue :: Ptr () -> IO Double
foreign import ccall unsafe "include/IloCplex.h cpx_getValue_num"
    cpx_getValue_num :: Ptr () -> Ptr () -> IO Double
foreign import ccall unsafe "include/IloCplex.h cpx_getValue_bool"
    cpx_getValue_bool :: Ptr () -> Ptr () -> IO Double
foreign import ccall "include/IloCplex.h cpx_exportModel"
    cpx_exportModel :: Ptr () -> CString -> IO ()
foreign import ccall "include/IloCplex.h cpx_boolvar_setPriority"
    cpx_boolvar_setPriority :: Ptr () -> Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloCplex.h cpx_boolvar_setDirection"
    cpx_boolvar_setDirection :: Ptr () -> Ptr () -> CInt -> IO ()

newtype IloCplex = IloCplex (ForeignPtr ())

instance IloObject IloCplex where
        getImpl (IloCplex x) = x
        newIloObject_ env= do  
			       ptr <- new_iloCplex env
		       	       r <- IloCplex <$>  newForeignPtr delete_iloCplex ptr
			       pure r


instance IloAlgorithm IloCplex where
        extract cpx mdl = withForeignPtr (getImpl cpx) $ \sptr ->
                          withForeignPtr (getImpl mdl) $ \mptr ->
                           cpx_extract sptr mptr 
        solve cpx = (>0) <$> withForeignPtr (getImpl cpx) cpx_solve
        getValue cpx v = withForeignPtr (getImpl cpx) $ \sptr ->
                         withForeignPtr (getImpl v) $ \vptr ->
                            realToFrac <$> f sptr vptr
              where f = case getVarType v of
                        NumVar -> cpx_getValue_num
                        BoolVar -> cpx_getValue_bool

getObjValue :: IloCplex -> IO Double
getObjValue cpx = realToFrac <$> withForeignPtr (getImpl cpx) cpx_getObjValue

exportModel :: IloCplex -> String -> IO ()
exportModel cpx name = withForeignPtr (getImpl cpx) $ \sptr ->
                       withCString name $ \str ->
                         cpx_exportModel sptr str

populate cpx = (>0) <$> withForeignPtr (getImpl cpx) cpx_populate


setPriority :: IloCplex -> IloBoolVar -> Double -> IO ()
setPriority cpx bv pri = withForeignPtr (getImpl cpx) $ \sv -> withForeignPtr (getImpl bv) $ \bptr -> cpx_boolvar_setPriority sv bptr (realToFrac pri)

data BranchDirection = BranchDown | BranchGlobal | BranchUp

setDirection cpx bv dir = withForeignPtr (getImpl cpx) $ \sv -> withForeignPtr (getImpl bv) $ \bptr -> cpx_boolvar_setDirection sv bptr dir'
  where dir' = case dir of
                BranchDown -> -1
                BranchGlobal -> 0
                BranchUp -> 1
