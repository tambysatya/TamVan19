#ifndef ILOOBJECT_ILOBOOLVAR_H
#define ILOOBJECT_ILOBOOLVAR_H

#include "IloEnv.h"

struct mip_start{
    IloBoolVarArray vars;
    IloNumArray vals;
};

extern "C"{
    IloBoolVar* new_iloBoolVar(IloEnv* e){return (new IloBoolVar(*e)); };
    void delete_iloBoolVar(IloBoolVar* v){v -> end(); delete v;};

    struct mip_start* new_mip_start(IloEnv* e){
        struct mip_start* ret = (struct mip_start*) malloc (sizeof (struct mip_start));
        ret->vars = IloBoolVarArray (*e);
        ret->vals = IloNumArray (*e);
        return ret;
    }

    void delete_mip_start (struct mip_start* st){
        st->vars.end();
        st->vals.end();
        free(st);
    }
    
    void edit_mip_start (struct mip_start* st, IloBoolVar* bv, double val){
            st->vars.add(*bv);
            st->vals.add(val);
    }
    
    void add_mip_start (IloCplex* cpx, struct mip_start* st){
            cpx->addMIPStart (st->vars.toNumVarArray(), st->vals, IloCplex::MIPStartSolveFixed, "started");
    }
    
}


#endif
