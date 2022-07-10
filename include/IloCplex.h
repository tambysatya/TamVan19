#ifndef ILOOBJECT_ILOCPLEX_H
#define ILOOBJECT_ILOCPLEX_H

#include "IloEnv.h"

extern "C"{
    IloCplex* new_iloCplex(IloEnv* e){
//	std::cout << "[C] new cpx" << std::endl;
	IloCplex* ret= NULL;
    	try {
	
        ret = new IloCplex(*e);
//	std::cout << "[C] cpx dne" << std::endl;
        ret->setOut(e->getNullStream());
        ret->setParam(IloCplex::MIPDisplay,0);
        ret->setParam(IloCplex::Threads, 1); 		//number of threads
        ret->setParam(IloCplex::ParallelMode, -1);

        ret->setParam(IloCplex::NodeLim, 100000000000);	//MIP node limit	
//        ret->setParam(IloCplex::NodeLim, 1000000000);	//MIP node limit	
//        ret->setParam(IloCplex::TreLim, 1000000000);	//tree memory limit
//        ret->setParam(IloCplex::EpGap, 10^(-9));	//relative MIP gap tolerance
//        ret->setParam(IloCplex::EpGap, 0.0);	//relative MIP gap tolerance
//        ret->setParam(IloCplex::ItLim, 1000000000);	//absolute MIP gap iteration limit
        ret->setParam(IloCplex::EpAGap,0);
        ret->setParam(IloCplex::EpGap,0);
        ret->setParam(IloCplex::MIPDisplay, 0);	//Dislay option
        ret->setParam(IloCplex::AdvInd, 1);

//        ret->setParam(IloCplex::Param::MIP::Strategy::HeuristicFreq,-1);
//        ret->setParam(IloCplex::Param::MIP::Strategy::RINSHeur,-1);
//	ret->setParam(IloCplex::RootAlg,IloCplex::Network);

	}
	catch (IloException e){
		std::cout << e << std::endl;
	}
	return ret;
    };
    void delete_iloCplex(IloCplex* v){v -> end(); delete v;};

    double cpx_getObjValue (IloCplex* cpx) {return cpx->getObjValue();}
    double cpx_getValue_num(IloCplex* cpx, IloNumVar* nv){return cpx->getValue(*nv);}
    double cpx_getValue_bool(IloCplex* cpx, IloBoolVar* bv){return cpx->getValue(*bv);}

    void cpx_extract (IloCplex* cpx, IloModel* mdl){cpx->extract(*mdl);}
    int cpx_solve (IloCplex* cpx){
            try{
            bool ret = cpx->solve();
            }catch (IloException e){
                std::cout << e << std::endl;
                exit(0);
            }
            IloAlgorithm::Status st = cpx->getStatus();
            if (IloAlgorithm::Infeasible == st){
		std::cout << "[C] Infeasible program solved" << std::endl;
		return -1;
	    }
            if (IloAlgorithm::InfeasibleOrUnbounded == st){
		    std::cout << "[C] Infeasible or Unbounded program solved" << std::endl;
                    return (-1);
	    }
            else return 1;}

    int cpx_populate(IloCplex* cpx){
        try{
            bool ret = cpx->populate();
            }catch (IloException e){
                std::cout << e << std::endl;
                exit(0);
            }
            IloAlgorithm::Status st = cpx->getStatus();
            if (IloAlgorithm::Infeasible == st ||
                IloAlgorithm::InfeasibleOrUnbounded == st)
                    return (-1);
            else return 1;}


    

    void cpx_exportModel (IloCplex* cpx, const char* str) {
            cpx->exportModel (str); 
    }

    void cpx_boolvar_setPriority (IloCplex* cpx, IloBoolVar* bv, double pri){cpx->setPriority(*bv,pri);}
    void cpx_boolvar_setDirection (IloCplex* cpx, IloBoolVar* bv, int dir){ 
               if (-1 == dir)
                       cpx->setDirection(*bv,IloCplex::BranchDown);
               if (0 == dir)
                       cpx->setDirection(*bv,IloCplex::BranchGlobal);
               else 
                       cpx->setDirection (*bv,IloCplex::BranchUp);

    }
}


#endif
