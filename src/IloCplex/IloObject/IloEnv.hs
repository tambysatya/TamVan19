{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.IloEnv where

import Foreign.Ptr
import Foreign.ForeignPtr

foreign import ccall "include/IloEnv.h new_iloEnv"
    new_iloEnv :: IO (Ptr ())
foreign import ccall "include/IloEnv.h &delete_iloEnv"
    delete_iloEnv :: FunPtr(Ptr () -> IO ())


newtype IloEnv = IloEnv (ForeignPtr ())

newIloEnv :: IO IloEnv
newIloEnv = IloEnv <$> (new_iloEnv >>= newForeignPtr delete_iloEnv)

