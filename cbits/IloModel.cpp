#include "../include/IloModel.h"

IloModel* new_iloModel (IloEnv* env){
	IloModel* mdl = NULL;
	try{
	mdl = new IloModel(*env);
	}
	catch (IloException e){
		std::cout << e << std::endl;
	}
	return mdl;

}

void delete_iloModel (IloModel* mdl){
	mdl->end();
	delete mdl;
    return ;
}
