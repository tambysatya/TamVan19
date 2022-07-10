module IloCplex.IloObject.MIPStart where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import IloCplex.IloObject.IloEnv
import IloCplex.IloObject.Class
import IloCplex.IloObject.Solver.IloCplex
import IloCplex.IloObject.Variable.IloBoolVar
import Control.Monad


foreign import ccall unsafe "include/IloBoolVar.h new_mip_start" 
    new_mip_start :: Ptr () -> IO (Ptr ())
foreign import ccall unsafe "include/IloBoolVar.h &delete_mip_start"
    delete_mip_start :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "include/IloBoolVar.h edit_mip_start"
    edit_mip_start :: Ptr () -> Ptr () -> CDouble -> IO ()

foreign import ccall unsafe "include/IloBoolVar.h add_mip_start"
    add_mip_start :: Ptr () -> Ptr () ->  IO ()

foreign import ccall unsafe "include/IloBoolVar.h mk_mip_start"
    mk_mip_start :: Ptr () -> CInt -> Ptr (Ptr ())  -> Ptr CDouble -> IO (Ptr ())

foreign import ccall unsafe "include/IloBoolVar.h create_empty_mip_start"
    create_empty_mip_start :: Ptr () -> CInt -> Ptr (Ptr ()) -> IO (Ptr ())

foreign import ccall unsafe "include/IloBoolVar.h modify_mip_start"
    modify_mip_start :: Ptr () -> CInt -> CDouble -> IO ()

newtype MIPStart = MIPStart (ForeignPtr ())
instance IloObject MIPStart where 
        getImpl (MIPStart st) = st
        newIloObject_ env = MIPStart <$> (new_mip_start env >>= newForeignPtr delete_mip_start)


editMIPStart :: MIPStart -> IloBoolVar -> Double -> IO ()
editMIPStart mip bv val = withForeignPtr (getImpl mip) $ \mptr ->
                          withForeignPtr (getImpl bv)  $ \vptr ->
                            edit_mip_start mptr vptr (realToFrac val)


addMIPStart :: IloCplex -> MIPStart -> IO ()
addMIPStart cpx mip = withForeignPtr (getImpl cpx) $ \cptr ->
                      withForeignPtr (getImpl mip) $ \mptr ->
                        add_mip_start cptr mptr


mkMIPStart :: IloEnv -> [IloBoolVar] -> [Double] -> IO MIPStart
mkMIPStart (IloEnv env) dvars dvals = do
        dvarsarray <- foreignptrlist dvars >>= newArray 
        dvalsarray <- newArray $ realToFrac <$> dvals
        ret <- withForeignPtr env $ \envptr -> MIPStart <$> (mk_mip_start envptr (fromIntegral $ length dvals) dvarsarray dvalsarray >>= newForeignPtr delete_mip_start)
        free dvarsarray
        free dvalsarray
        forM_ (getImpl <$> dvars) touchForeignPtr
        pure ret

mkEmptyMIPStart :: IloEnv -> [IloBoolVar] -> IO MIPStart
mkEmptyMIPStart (IloEnv env) dvars = do
    dvarsarray <- foreignptrlist dvars >>= newArray
    ret <- withForeignPtr env $ \envptr -> MIPStart <$> (create_empty_mip_start envptr (fromIntegral $ length dvars) dvarsarray >>= newForeignPtr delete_mip_start)
    free dvarsarray
    pure ret

adjustMIPStart :: MIPStart -> [Double] -> IO ()
adjustMIPStart (MIPStart mip) vals = withForeignPtr mip $ \mipptr -> forM_ (zip [0..n-1] vals) $ \(i,vi) -> modify_mip_start mipptr (fromIntegral i) (realToFrac vi)
    where n = length vals

foreignptrlist :: [IloBoolVar] -> IO [Ptr ()]
foreignptrlist [] = pure []
foreignptrlist (x:xs) = do
              ptr <- withForeignPtr (getImpl x) $ \ptr -> pure ptr
              ret <- foreignptrlist xs
              pure (ptr : ret)

                                                

