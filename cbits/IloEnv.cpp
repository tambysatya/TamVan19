#include "../include/IloEnv.h"

IloEnv* new_iloEnv(void){
    return (new IloEnv());
}

void delete_iloEnv(IloEnv* e){
    delete e;
    std::cout << "===========+> deleting env" << std::endl;
    return;
}
