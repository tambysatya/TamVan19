{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.IloObjective.Class where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types

import IloCplex.IloObject.Class
import IloCplex.IloObject.Variable
import IloCplex.IloObject.IloModel

foreign import ccall "include/IloObjective.h new_iloObjective"
    new_iloObjective :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloObjective.h &delete_iloObjective"
    delete_iloObjective :: FunPtr(Ptr () -> IO ())

foreign import ccall "include/IloObjective.h obj_setLinearCoefBool"
    obj_setLinearCoefBool :: Ptr () -> Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloObjective.h obj_setLinearCoefNum"
    obj_setLinearCoefNum :: Ptr () -> Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloObjective.h obj_setMinimize"
    obj_setMinimize :: Ptr () -> IO ()
foreign import ccall "include/IloObjective.h obj_setMaximize"
    obj_setMaximize :: Ptr () -> IO ()

foreign import ccall "include/IloModel.h add_iloObjective"
    add_iloObjective :: Ptr () -> Ptr () -> IO ()
foreign import ccall "include/IloModel.h rm_iloObjective"
    rm_iloObjective :: Ptr () -> Ptr () -> IO ()


newtype IloObjective = IloObjective (ForeignPtr ())

instance IloObject IloObjective where
        getImpl (IloObjective x) = x
        newIloObject_ env = IloObjective <$> (new_iloObjective env >>= newForeignPtr delete_iloObjective)

instance LinearCombination IloObjective where
        setLinearCoef obj v val = withForeignPtr (getImpl obj) $ \optr ->
                                  withForeignPtr (getImpl v) $ \vptr -> f optr vptr (realToFrac val)
           where f = case getVarType v of
                    NumVar -> obj_setLinearCoefNum
                    BoolVar -> obj_setLinearCoefBool

instance IloAddable IloObjective where
        mdl `add` obj = withForeignPtr (getImpl mdl) $ \mptr ->
                        withForeignPtr (getImpl obj) $ \optr ->
                            add_iloObjective mptr optr
        mdl `remove` obj = withForeignPtr (getImpl mdl) $ \mptr ->
                           withForeignPtr (getImpl obj) $ \optr ->
                             rm_iloObjective mptr optr


setMaximize :: IloObjective -> IO ()
setMaximize obj = withForeignPtr (getImpl obj) obj_setMaximize

setMinimize :: IloObjective -> IO ()
setMinimize obj = withForeignPtr (getImpl obj) obj_setMinimize


