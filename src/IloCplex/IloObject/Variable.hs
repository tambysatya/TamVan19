module IloCplex.IloObject.Variable (
        module IloCplex.IloObject.Variable.IloNumVar,
        module IloCplex.IloObject.Variable.IloBoolVar,
        VarType(..),
        IloVar, LinearCombination, getVarType, setLinearCoef)
where

import IloCplex.IloObject.Class
import IloCplex.IloObject.Variable.IloNumVar
import IloCplex.IloObject.Variable.IloBoolVar

data VarType = NumVar | BoolVar | IntVar

class (IloObject a) => IloVar a where
        getVarType :: a -> VarType

instance IloVar IloNumVar where
        getVarType _ = NumVar
instance IloVar IloBoolVar where
        getVarType _ = BoolVar
        

class LinearCombination a where
        setLinearCoef :: (IloVar b) => a -> b -> Double -> IO ()
