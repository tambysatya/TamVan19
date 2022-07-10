#ifndef ILOOBJECT_ILOOBJECTIVE_H
#define ILOOBJECT_ILOOBJECTIVE_H

#include "IloEnv.h"

extern "C"{
    IloObjective* new_iloObjective(IloEnv* e){return (new IloObjective(*e)); };
    void delete_iloObjective(IloNumVar* v){v -> end(); delete v;};

    void obj_setLinearCoefNum(IloObjective* obj, IloNumVar* v, double val){obj->setLinearCoef(*v,val);}
    void obj_setLinearCoefBool(IloObjective* obj, IloBoolVar* v, double val){obj->setLinearCoef(*v,val);}

    void obj_setMinimize (IloObjective* obj) {obj->setSense(IloObjective::Minimize);}
    void obj_setMaximize (IloObjective* obj) {obj->setSense(IloObjective::Maximize);}
}


#endif
