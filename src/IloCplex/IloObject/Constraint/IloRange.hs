{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.Constraint.IloRange where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import IloCplex.IloObject.Class
import IloCplex.IloObject.Variable
import IloCplex.IloObject.IloModel

foreign import ccall "include/IloRange.h new_iloRange"
    new_iloRange :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloRange.h &delete_iloRange"
    delete_iloRange :: FunPtr(Ptr () -> IO ())

foreign import ccall "include/IloRange.h range_setLinearCoefBool"
    range_setLinearCoefBool :: Ptr () -> Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloRange.h range_setLinearCoefNum"
    range_setLinearCoefNum :: Ptr () -> Ptr () -> CDouble -> IO ()


foreign import ccall "include/IloRange.h range_setLB"
    range_setLB :: Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloRange.h range_setUB"
    range_setUB :: Ptr () -> CDouble -> IO ()
foreign import ccall "include/IloRange.h range_setBounds"
    range_setBounds :: Ptr () -> CDouble -> CDouble -> IO ()

foreign import ccall "include/IloModel.h add_iloRange"
    add_iloRange :: Ptr () -> Ptr () -> IO ()

foreign import ccall "include/IloModel.h rm_iloRange"
    rm_iloRange :: Ptr () -> Ptr () -> IO ()

newtype IloRange = IloRange (ForeignPtr ())


instance IloObject IloRange where
        getImpl (IloRange x) = x
        newIloObject_ env = IloRange <$> (new_iloRange env >>= newForeignPtr delete_iloRange)

instance LinearCombination IloRange where
        setLinearCoef rng v val = withForeignPtr (getImpl rng) $ \cptr ->
                                  withForeignPtr (getImpl v) $ \vptr -> f cptr vptr (realToFrac val)
           where f = case getVarType v of
                    NumVar -> range_setLinearCoefNum
                    BoolVar -> range_setLinearCoefBool

instance IloAddable IloRange where
        mdl `add` rng = withForeignPtr (getImpl mdl) $ \mptr ->
                        withForeignPtr (getImpl rng) $ \cptr ->
                            add_iloRange mptr cptr
        mdl `remove` rng = withForeignPtr (getImpl mdl) $ \mptr ->
                            withForeignPtr (getImpl rng) $ \cptr ->
                                rm_iloRange mptr cptr




setLB :: IloRange -> Double -> IO ()
setLB rng v = withForeignPtr (getImpl rng) $ flip range_setLB (realToFrac v)

setUB :: IloRange -> Double -> IO ()
setUB rng v = withForeignPtr (getImpl rng) $ flip range_setUB (realToFrac v)

setBounds :: IloRange -> (Double,Double) -> IO ()
setBounds rng (lb,ub) = withForeignPtr (getImpl rng) $ \x -> range_setBounds x (realToFrac lb) (realToFrac ub)
