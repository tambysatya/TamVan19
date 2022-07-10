{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.Variable.IloNumVar where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types

import IloCplex.IloObject.Class

foreign import ccall "include/IloNumVar.h new_iloNumVar"
    new_iloNumVar :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloNumVar.h &delete_iloNumVar"
    delete_iloNumVar :: FunPtr(Ptr () -> IO ())
foreign import ccall "include/IloNumVar.h iloNumVar_setLB"
    c_iloNumVar_setLB :: Ptr () -> CDouble -> IO ()

newtype IloNumVar = IloNumVar (ForeignPtr ())

instance IloObject IloNumVar where
        getImpl (IloNumVar x) = x
        newIloObject_ env = IloNumVar <$> (new_iloNumVar env >>= newForeignPtr delete_iloNumVar)

iloNumVar_setLB (IloNumVar v) val = withForeignPtr v $ \ptr -> c_iloNumVar_setLB ptr (realToFrac val)




