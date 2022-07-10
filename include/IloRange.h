#ifndef ILOOBJECT_ILORANGE_H
#define ILOOBJECT_ILORANGE_H

#include "IloEnv.h"

extern "C"{
    IloRange* new_iloRange(IloEnv* e){return (new IloRange(*e,-IloInfinity,IloInfinity)); };
    void delete_iloRange(IloNumVar* v){v -> end(); delete v;};

    void range_setLinearCoefNum(IloRange* rng, IloNumVar* v, double val){rng->setLinearCoef(*v,val);}
    void range_setLinearCoefBool(IloRange* rng, IloBoolVar* v, double val){rng->setLinearCoef(*v,val);}

    void range_setLB (IloRange* rng, double lb) {rng->setLB(lb);}
    void range_setUB (IloRange* rng, double ub) {
            try {rng->setUB(ub);}
            catch (IloException e){
                std::cout << e.getMessage() << std::endl;
                exit (-1);
            }}
    void range_setBounds (IloRange* rng, double lb, double ub){
            try{
            rng->setBounds(lb,ub);
            }
            catch (IloException e){
                    std::cout << e.getMessage() << std::endl;
                exit (-1);
            }
    }
}


#endif
