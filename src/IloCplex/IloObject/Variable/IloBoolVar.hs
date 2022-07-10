{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.Variable.IloBoolVar where

import Foreign.ForeignPtr
import Foreign.Ptr

import IloCplex.IloObject.Class

foreign import ccall "include/IloBoolVar.h new_iloBoolVar"
    new_iloBoolVar :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloBoolVar.h &delete_iloBoolVar"
    delete_iloBoolVar :: FunPtr(Ptr () -> IO ())

newtype IloBoolVar = IloBoolVar (ForeignPtr ())

instance IloObject IloBoolVar where
        getImpl (IloBoolVar x) = x
        newIloObject_ env = IloBoolVar <$> (new_iloBoolVar env >>= newForeignPtr delete_iloBoolVar)




