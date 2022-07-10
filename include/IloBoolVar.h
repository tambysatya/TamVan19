#ifndef ILOOBJECT_ILOBOOLVAR_H
#define ILOOBJECT_ILOBOOLVAR_H

#include "IloEnv.h"

struct mip_start{
    IloNumVarArray vars;
    IloNumArray vals;
};

extern "C"{
    IloBoolVar* new_iloBoolVar(IloEnv* e){return (new IloBoolVar(*e)); };
    void delete_iloBoolVar(IloBoolVar* v){v -> end(); delete v;};

    struct mip_start* new_mip_start(IloEnv* e){
        std::cout << "creating mipstart \n";
        struct mip_start* ret = (struct mip_start*) malloc (sizeof (struct mip_start));
        ret->vars = IloNumVarArray (*e);
        ret->vals = IloNumArray (*e);
        return ret;
    }

    void delete_mip_start (struct mip_start* st){
        st->vars.end();
        st->vals.end();
        std::cout << "deleting mipstart \n";
        free(st);
    }
    
    void edit_mip_start (struct mip_start* st, IloBoolVar* bv, double val){
        try{
            st->vars.add(*bv);
            st->vals.add(val);
        }
        catch (IloException e){
            std::cout << e << std::endl;
            exit(0);
        }
    }

    struct mip_start* mk_mip_start (IloEnv* e, int siz, IloBoolVar** dvars, double* vals){
        struct mip_start* ret = (struct mip_start*) malloc (sizeof (struct mip_start));
        ret->vars = IloNumVarArray (*e);
        ret->vals = IloNumArray (*e);
        for (int i=0; i < siz; i++){
            ret->vars.add(*(dvars[i]));
            ret->vals.add(vals[i]);
        }
        return ret;
           
    }
    struct mip_start* create_empty_mip_start (IloEnv* e, int siz, IloBoolVar** dvars){
        struct mip_start* ret = (struct mip_start*) malloc (sizeof (struct mip_start));
        ret->vars = IloNumVarArray (*e,siz);
        ret->vals = IloNumArray (*e,siz);
        for (int i=0; i < siz; i++){
            ret->vars[i] = *(dvars[i]);
        }
        return ret;

    }
    void modify_mip_start (struct mip_start* st, int i, double val){
        st->vals[i] = val;
    }
    
    void add_mip_start (IloCplex* cpx, struct mip_start* st){
        try{
             cpx->addMIPStart (st->vars, st->vals, IloCplex::MIPStartSolveFixed, "started");
        }
        catch (IloException e){
            std::cout << e << std::endl;
            exit(0);
        }

            //cpx->addMIPStart (st->vars.toNumVarArray(), st->vals, IloCplex::MIPStartSolveFixed, "started");
    }


    // Crée et ajoute un mipstart 
    void start_from (IloEnv* e, IloCplex* cpx, int n, IloBoolVar** dvars, double* dvals, int p, IloNumVar** objvars, double* ovals){

    }
    
}


#endif
