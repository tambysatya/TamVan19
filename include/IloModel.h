#ifndef C_ILOMODEL_ILOMODEL_H
#define C_ILOMODEL_ILOMODEL_H

#include "IloEnv.h"

extern "C"{
	IloModel* new_iloModel(IloEnv*);
	void delete_iloModel(IloModel*);

    void add_iloObjective (IloModel* mdl, IloObjective* v){mdl->add(*v);}
    void add_iloRange (IloModel* mdl, IloRange* rng){mdl->add(*rng);}

    void rm_iloObjective (IloModel* mdl, IloObjective* v) {mdl->remove(*v);}
    void rm_iloRange (IloModel* mdl, IloRange* rng) {mdl -> remove (*rng);}

}

#endif
