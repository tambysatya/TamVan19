module IloCplex.IloObject.MIPStart where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr

import IloCplex.IloObject.Class
import IloCplex.IloObject.Solver.IloCplex
import IloCplex.IloObject.Variable.IloBoolVar

foreign import ccall "include/IloBoolVar.h new_mip_start"
    new_mip_start :: Ptr () -> IO (Ptr ())
foreign import ccall "include/IloBoolVar.h &delete_mip_start"
    delete_mip_start :: FunPtr (Ptr () -> IO ())

foreign import ccall "include/IloBoolVar.h edit_mip_start"
    edit_mip_start :: Ptr () -> Ptr () -> CDouble -> IO ()

foreign import ccall "include/IloBoolVar.h add_mip_start"
    add_mip_start :: Ptr () -> Ptr () ->  IO ()


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
