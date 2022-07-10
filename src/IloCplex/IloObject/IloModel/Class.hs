{-# LANGUAGE ForeignFunctionInterface, ConstrainedClassMethods #-}

module IloCplex.IloObject.IloModel.Class where


import Foreign.Ptr
import Foreign.ForeignPtr

import IloCplex.IloObject.Class
import IloCplex.IloObject.Variable

foreign import ccall "include/IloModel.h new_iloModel"
    new_iloModel :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloModel.h &delete_iloModel"
    delete_iloModel :: FunPtr(Ptr () -> IO ())


newtype IloModel = IloModel (ForeignPtr ())


instance IloObject IloModel where
        getImpl (IloModel x) = x
        newIloObject_ env= IloModel <$> (new_iloModel env >>= newForeignPtr delete_iloModel)
        


class (IloObject a) => IloAddable a where
        add :: (IloObject a) => IloModel -> a -> IO () 
        remove :: (IloObject a) => IloModel -> a -> IO ()

class (IloObject a) => IloAlgorithm a where
        extract :: a -> IloModel -> IO ()
        solve :: a -> IO Bool
        getValue :: (IloVar b) => a -> b -> IO Double
