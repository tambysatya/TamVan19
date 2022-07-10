#ifndef ILOENV_H
#define ILOENV_H
#include <ilcplex/ilocplex.h>
#include <cstdlib>
#include <ilconcert/ilolinear.h>

extern "C"{
    IloEnv* new_iloEnv(void);
    void delete_iloEnv(IloEnv*);
}

#endif
