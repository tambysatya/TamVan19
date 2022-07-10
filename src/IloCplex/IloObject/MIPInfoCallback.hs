{-# LANGUAGE ForeignFunctionInterface #-}


module IloCplex.IloObject.MIPInfoCallback where

import IloCplex.IloObject.Class
import IloCplex.IloObject.IloEnv
import IloCplex.IloObject.Variable
import IloCplex.IloObject.Solver.IloCplex

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc


foreign import ccall "include/MIPInfoCallback.h &delete_point"
    delete_point :: FunPtr (Ptr () -> IO ())

foreign import ccall "include/MIPInfoCallback.h point_getSol"
    point_getSol :: Ptr () -> IO (Ptr CDouble)
foreign import ccall "include/MIPInfoCallback.h point_getPerfs"
    point_getPerfs :: Ptr () -> IO (Ptr CDouble)

newtype MIPCallbackPoint = MIPCallbackPoint (ForeignPtr ())

pointGetSol     :: Int -> MIPCallbackPoint -> IO [Double]
pointGetSol n (MIPCallbackPoint pt) = withForeignPtr pt $ \ptptr -> do
                                          arrPtr <- point_getSol ptptr
                                          map realToFrac <$> peekArray n arrPtr

pointGetPerfs                         :: Int -> MIPCallbackPoint -> IO [Double]
pointGetPerfs p (MIPCallbackPoint pt) = withForeignPtr pt $ \ptptr -> do
                                          arrPtr <- point_getPerfs ptptr
                                          map realToFrac <$> peekArray p arrPtr


foreign import ccall "include/MIPInfoCallback.h new_pointList"
    new_pointList :: IO (Ptr ())
foreign import ccall "include/MIPInfoCallback.h &delete_pointList"
    delete_pointList :: FunPtr (Ptr () -> IO ())
foreign import ccall "include/MIPInfoCallback.h is_pointListEmpty"
    is_pointListEmpty :: Ptr () -> IO CInt 
foreign import ccall "include/MIPInfoCallback.h pointList_nextPoint"
    pointList_nextPoint :: Ptr () -> IO (Ptr ())

newtype MIPCallbackPointList = MIPCallbackPointList (ForeignPtr ())
newPointList :: IO MIPCallbackPointList
newPointList = MIPCallbackPointList <$> (new_pointList >>= newForeignPtr delete_pointList)

isPointListEmpty :: MIPCallbackPointList -> IO Bool
isPointListEmpty (MIPCallbackPointList ptr) = withForeignPtr ptr $ 
                                              \ptptr -> do ret <- fromIntegral <$> is_pointListEmpty ptptr
                                                           case ret of
                                                             1 -> return True
                                                             otherwise -> return False

pointListNextPoint :: MIPCallbackPointList -> IO MIPCallbackPoint
pointListNextPoint (MIPCallbackPointList ptr) = withForeignPtr ptr $ 
                                                 \ptptr -> do retPtr <- pointList_nextPoint ptptr
                                                              MIPCallbackPoint <$> newForeignPtr delete_point retPtr



foreign import ccall "include/MIPInfoCallback.h add_to_solver"
    add_to_solver :: Ptr () -> Ptr () -> CInt -> CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr ()  -> IO ()



withForeignPtrList' :: ([Ptr ()] -> IO ()) -> [ForeignPtr ()] -> [Ptr ()] -> IO ()
withForeignPtrList' f [] ret = f ret
withForeignPtrList' f (x:xs) ret = withForeignPtr x $ \xptr -> withForeignPtrList' f xs (ret ++ [xptr])

withForeignPtrList f l = withForeignPtrList' f l []

addCallbackToSolver :: IloEnv -> IloCplex -> Int -> Int -> [IloBoolVar] -> [IloNumVar] -> MIPCallbackPointList -> IO ()
addCallbackToSolver (IloEnv env)  cpx n p solvars perfvars (MIPCallbackPointList pointlist) = withForeignPtr env $ \envptr->
                                                                                              withForeignPtr (getImpl cpx) $ \cpxptr->
                                                                                              withForeignPtrList (f envptr cpxptr) (map getImpl solvars)
       where f envptr cpxptr solvarsPtrs = withForeignPtrList (g envptr cpxptr solvarsPtrs) (map getImpl perfvars)
             g envptr cpxptr solvarsPtrs perfvarsPtrs = do solArray <- newArray solvarsPtrs
                                                           perfArray <- newArray perfvarsPtrs
                                          --                 putStrLn $ "sizeof solArray=" ++ show (length solvarsPtrs) ++ ", perfArray" ++ show (length perfvarsPtrs)
                                                           withForeignPtr pointlist $ \pointlistptr -> 
                                                            add_to_solver envptr cpxptr (fromIntegral n) (fromIntegral p) solArray perfArray pointlistptr 
                                                           free perfArray >> free solArray



