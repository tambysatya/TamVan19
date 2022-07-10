{-# LANGUAGE ForeignFunctionInterface #-}
module IloCplex.IloObject.MIPIncumbentCallback where

import IloCplex.IloObject.Class
import IloCplex.IloObject.IloEnv
import IloCplex.IloObject.Constraint
import IloCplex.IloObject.Variable
import IloCplex.IloObject.Solver.IloCplex
import IloCplex.IloObject.MIPInfoCallback hiding (withForeignPtrList, withForeignPtrList')

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall "include/MIPIncumbentCallback.h setLexMin"
    set_lex_min :: Ptr () -> Ptr () -> CInt -> CInt -> Ptr () -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr (Ptr ()) ->  Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO (Ptr ())

foreign import ccall "include/MIPIncumbentCallback.h isEmpty"
    is_empty :: Ptr () -> IO CInt
foreign import ccall "include/MIPIncumbentCallback.h getSol"
    get_sol :: Ptr () -> CInt -> IO CDouble
foreign import ccall "include/MIPIncumbentCallback.h getPerf"
    get_perf :: Ptr () -> CInt -> IO CDouble
foreign import ccall "include/MIPIncumbentCallback.h &deleteRetBuf"
    delete_ret_buf :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "include/MIPIncumbentCallback.h resetBuf"
    reset_buf:: Ptr () -> IO ()
foreign import ccall unsafe "include/MIPIncumbentCallback.h resetDouble"
    reset_double :: Ptr CDouble -> IO ()

data LexMinCallback = LexMinCallback {
        
                                        lmcSeuils :: (Ptr CDouble, Ptr CDouble),
                                        lmcObjVarsArray :: Ptr (Ptr ()),
                                        lmcDVarsArray :: Ptr (Ptr ()),
                                        lmcChosenInt :: Ptr CInt,
                                        lmcUtils :: (IloNumVar,IloRange),
                                        lmcBudgetCtrs :: Ptr (Ptr ())
                                     }

newtype LexMinBuf = LexMinBuf (ForeignPtr ())

withForeignPtrListRet' :: ([Ptr ()] -> IO a) -> [ForeignPtr ()] -> [Ptr ()] -> IO a
withForeignPtrListRet' f [] ret = f ret
withForeignPtrListRet' f (x:xs) ret = withForeignPtr x $ \xptr -> withForeignPtrListRet' f xs (ret ++ [xptr])

withForeignPtrListRet f l = withForeignPtrListRet' f l []


buildObjVarArray :: (IloObject a, IloVar a) => [a] -> IO (Ptr (Ptr ()))
buildObjVarArray vars = withForeignPtrListRet newArray (getImpl <$> vars)

{- WARNING :: Mettre le free() (pas mis actuellement) -}
initLexMinCallback objVars dvars utils ctrs = do (d1,d2) <- (,) <$> malloc <*> malloc
                                                 chosen <- malloc
                                                 poke chosen 1
                                                 varsArray <- buildObjVarArray objVars 
                                                 dvarsArray <- buildObjVarArray dvars
                                                 budget <- withForeignPtrListRet newArray (getImpl <$> ctrs)
                                                 pure $ LexMinCallback (d1,d2) varsArray dvarsArray chosen utils budget

resetDoubles :: LexMinCallback -> IO ()
resetDoubles lmc = reset_double (fst $ lmcSeuils lmc) >> reset_double (snd $ lmcSeuils lmc)

setLexMin :: IloEnv -> IloCplex -> (IloNumVar,IloRange) -> [IloNumVar] -> [IloBoolVar] -> [IloRange] -> IO (LexMinBuf,LexMinCallback)
setLexMin (IloEnv env) cpx utils@(chosen,_) crits dvars budgetCtrs = withForeignPtr env $ \e -> 
                                                                          withForeignPtr (getImpl cpx) $ \c ->
                                                                          withForeignPtr (getImpl chosen) $ \chos->
                                                                                        do 
                                                                                             lmc <- initLexMinCallback crits dvars utils budgetCtrs
                                                                                             resetDoubles lmc
                                                                                             ptr <- set_lex_min e c (fromIntegral $ length crits) (fromIntegral $ length dvars) chos (lmcObjVarsArray lmc) (lmcDVarsArray lmc) (lmcBudgetCtrs lmc) (lmcChosenInt lmc) (fst $ lmcSeuils lmc) (snd $ lmcSeuils lmc)
                                                                                             lms <- newForeignPtr delete_ret_buf ptr
                                                                                             pure (LexMinBuf lms,lmc)

lexResetBuf (LexMinBuf solv) = withForeignPtr solv reset_buf
lexValueOfSol :: LexMinBuf -> Int ->  IO Double
lexValueOfSol (LexMinBuf solv) i = realToFrac <$> (withForeignPtr solv $ \sv -> get_sol sv $ fromIntegral i)
lexValueOfPerf :: LexMinBuf -> Int -> IO Double
lexValueOfPerf (LexMinBuf solv) i = realToFrac <$> (withForeignPtr solv $ \sv -> get_perf sv $ fromIntegral i)
lexIsEmpty (LexMinBuf solv)  = withForeignPtr solv $ \sv -> do r <- is_empty sv
                                                               case r of 
                                                                    1 -> pure True
                                                                    _ -> pure False
{-

setLexMin :: IloEnv -> IloCplex -> [IloNumVar]  ForeignPtr CDouble -> ForeignPtr CDouble ->  IO (Ptr (Ptr ()))
setLexMin (IloEnv env) cpx crits crit2 d1 d2 = withForeignPtr env $ \e ->
                                withForeignPtr (getImpl cpx) $ \c ->
                                  do vars <- buildObjVarArray crits
                                     chosen <- malloc
                                     withForeignPtr d1 $ \dob1 ->
                                     withForeignPtr d2 $ \dob2 ->
                                        set_lex_min e c vars dob1 dob2

resetDoubles :: (ForeignPtr CDouble, ForeignPtr CDouble) -> IO ()
resetDoubles (d1,d2) = withForeignPtr d1 $ \dob1 ->
                     withForeignPtr d2 $ \dob2 ->
                       reset_double dob1 >> reset_double dob2
                       -}
