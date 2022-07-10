#ifndef ILONUMVAR_H
#define ILONUMVAR_H

#include "IloEnv.h"

extern "C"{
    IloNumVar* new_iloNumVar(IloEnv* e){return (new IloNumVar(*e, -IloInfinity, IloInfinity)); };
    void iloNumVar_setLB (IloNumVar* v, double val){v->setLB(val);}
    void delete_iloNumVar(IloNumVar* v){v -> end(); delete v;};
}


#endif
