#include "../include/MIPInfoCallback.h"

using namespace std;


double* new_doubleArray (int n){return (double*) malloc (n*sizeof(double));}

ILOMIPINFOCALLBACK5 (myCallback, int, p, IloNumVarArray, objvars, int, n, IloBoolVarArray, boolvars, PointList*, pl){
    if (hasIncumbent()){
            double* perfs = new_doubleArray (p);
            for (int i=0; i < p; i++){
                    perfs[i] = getIncumbentValue (objvars[i]);
//                    cout << perfs[i] << "\t";
            }
//            cout << endl;

            bool addable = false;
            if (false == pl->empty()){
                struct Point* pt = pl->front();
                for (int i=0; i < p; i++){
                    if (pt->perfs[i] != perfs[i]){
                            addable=true;
                            break;
                    }
                }

            }else addable=true;
            
            if (addable){
                    struct Point* ret = (struct Point*) malloc (sizeof(struct Point));
                    double* sol = new_doubleArray(n);
                    for (int i=0; i < n; i++){
                        sol[i] = getIncumbentValue (boolvars[i]);
                    }
                    ret->opt = getIncumbentObjValue();
                    ret->sol = sol;
                    ret->perfs=perfs;
                    pl->push_front(ret);
  //                  cout << "new_inc" << endl;
                    


            }
            else{
                free(perfs);
    //            cout << "duplicate inc, don't adding it" << endl;
            }
            

//            cout << "opt = " << getIncumbentObjValue() << endl;
//            cout << "val = " << getIncumbentValue (objvars[0]) << endl;
//            getIncumbentValues (objvals,objvars);
//            cout << "\t" << objvals << endl;
//            getIncumbentValues (boolvals,boolvars);
//            cout << objvals << "\t" << boolvals << endl;

    }
}

void add_to_solver (IloEnv* env, IloCplex* cpx, int n, int p, IloBoolVar** dvars, IloNumVar** nvars,PointList* pl){
        IloBoolVarArray bv(*env,n);
        IloNumVarArray objvars (*env,p);

        for (int i=0; i < n; i++)
                   bv[i] = *(dvars[i]);
        for (int i=0; i < p; i++)
                   objvars[i] = *(nvars[i]);
                   //objvars.add (*(nvars[i]));
        
        cpx->setParam(IloCplex::NodeSel,0);
        cpx->use(myCallback (*env,p,objvars,n,bv,pl));
}

