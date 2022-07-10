#ifndef MIPINFOCALLBACK_H
#define MIPINFOCALLBACK_H

#include "IloEnv.h"
#include <list>

struct Point{
    double opt;
    double* sol;
    double* perfs;
};

typedef std::list<struct Point*> PointList;

extern "C"{


void delete_point (struct Point* pt) {
      free (pt->sol);
      free (pt->perfs);
      free(pt);
}

double* point_getSol (struct Point* pt){return pt->sol;}
double* point_getPerfs (struct Point* pt) {return pt->perfs;}




PointList* new_pointList(void){
    return (new PointList());
}
void delete_pointList (PointList* l){
   while (l->empty () == false){
         delete_point (l->front());
         l->pop_front(); 
   }
   delete l;
   return;
}

int is_pointListEmpty (PointList* l){
    if (true == l->empty())
            return 1;
    else return 0;
}
struct Point* pointList_nextPoint (PointList* l){
    struct Point* pt = l->front();
    l->pop_front();
    return pt;
}




void add_to_solver (IloEnv* env, IloCplex* cpx, int n, int p, IloBoolVar** dvars, IloNumVar** nvars,PointList* pl);

}




#endif
