#ifndef MIPINCUMBENTCALLBACK_H
#define MIPINCUMBENTCALLBACK_H

#include "IloEnv.h"
#include <math.h>
class BranchData : public IloCplex::MIPCallbackI::NodeData
{
    public:
            BranchData(double v) {lastVal=v;};
            double lastVal;

};

extern "C"{
struct retBuf {
    int p;
    int n;
    bool emptyP;
    bool toAddP;
    double lastVal;

    IloNumVar* opt2;
    IloNumArray perfs;

    IloNumArray buf;
    IloBoolVarArray dvars;
    IloNumArray sol;
    
    IloRange** ctrs;

};

/*
ILOHEURISTICCALLBACK1 (lexMinHeuristic,
                      struct retBuf*, ret){
    if (true == ret->toAddP){
        ret->toAddP = false;
//        std::cout << "set solution " << ret->buf << std::endl;
        setSolution(ret->dvars,ret->buf);
    }
}
ILOBRANCHCALLBACK3   (lexMinBranch,
                          double*, opt1,
                          double*, opt2,
                          struct retBuf*, ret){

            try {
                        BranchData* data = (BranchData*) getNodeData();
                        double val1 = IloRound(getBestObjValue());
                        if (val1 > *opt1){
//                            std::cout << "[BRANCH] " << val1 << "\trejected  [" << *opt1 << "," << *opt2 << "]" << std::endl;
                            prune();
                        }
                        else if (val1 == *opt1 && (NULL == data || val1 < data->lastVal)){
//                            std::cout << "[BRANCH] adding cuts  [" << *opt1 << "," << *opt2 << "]" << std::endl;
                            ret->ctrs[0].setUB(*opt1);
                            ret->ctrs[1].setUB(*opt2-0.5);

                            ret->lastVal = val1;
                            makeBranch(ret->ctrs,getObjValue(),new BranchData (val1));
                            }
                        }
        catch (IloException e){
//                std::cout << e << std::endl;
                exit(0);
            }

                
        /*

        if (hasIncumbent()){



        double val1 = round(getIncumbentValue (*(crits[*chosen-1])));
        double val2 = 0;
        //double val2 = val1;
        for (int i=0; i < ret->p; i++){
                //if (i != *chosen - 1)
                        val2 += round(getIncumbentValue (*(crits[i])));
        }


        std::cout << "[Branch] \t[" << val1 << ";" << val2 << "]";
        if (val1 < *opt1 || (val1 == *opt1 && val2 < *opt2) ){
            std::cout << "\taccepted" << std::endl;
            *opt1=val1;
            *opt2=val2;
            ret->ctrs[0].setUB(val1); 
            makeBranch(ret->ctrs);
        }
        else{
            std::cout << "\trejected  [" << *opt1 << "," << *opt2 << "]" << std::endl;
            prune();
        }
        }
}
*/
ILOINCUMBENTCALLBACK6  (lexMinCallback,
                          IloNumVar**, objvar,
                          IloBoolVar**, sols,
                          int* , chosen,
                          double*, opt1,
                          double*, opt2,
                          struct retBuf*, ret){
        double precOpt1 = *opt1;
        double val1 = IloRound(getObjValue()); 
        double val2 = IloRound(getValue(*(ret->opt2)));
        /*
        std::cout << "[INCUMBENT]\t";
        for (int i=0; i < ret->p; i++)
                std::cout << IloRound (getValue (*(objvar[i]))) << "\t";
*/
        if (val1 < *opt1 || (val1 == *opt1 && val2 < *opt2) ){
            if (val1 < *opt1){
                /*On accepte le buffer si elle améliore opt1*/
                ret->toAddP=true;
                for (int i=0; i < ret->n; i++)
                        ret->buf[i] = ret->sol[i];
            }
            *opt1=val1;
            *opt2=val2;
            ret->emptyP=false;
            (ret->ctrs[*chosen - 1])->setUB(*opt1);
            for (int i=0; i <ret->p; i++)
                    ret->perfs[i]=IloRound(getValue(*(objvar[i])));


            for (int i=0; i < ret->n;i++){
                    ret->sol[i] = IloRound(getValue (*(sols[i])));
            }
            std::cout << " found [" << val1 << "," << val2 << "]" << std::endl;
            //reject();
        }
        if (val1 == *opt1)
                reject();
//        std::cout << std::endl;
       
       /* else if (val1 == *opt1 && val2 >= *opt2){
                //On rejete car il n'améliore pas strictement l'opt1 (ou est égal et améliroe l'opt2)
                //On ne peut le garder car cplex peut prune les branches ou f_1(b) == opt1 (même si f_2(b) < opt2)
                std::cout << "[INCUMBENT] rejecting solution [" << val1 << "," << val2 << "] : not lexicographically optimal [" << val1 << "," << val2 << "]" << std::endl;
                reject();
        }
        else{
              std::cout << " passed to cplex" << std::endl;
       //     On accepte pour que cplex puisse prune
        }*/
}

void resetDouble (double* d){
    *d = IloInfinity;
}

// Il faut une variable contenant la valeur du critère secondaire
struct retBuf* setLexMin (IloEnv* env, IloCplex* cpx, int p, int n, IloNumVar* crit, IloNumVar** perfs,  IloBoolVar** dvars, IloRange** ctrs, int* chosen, double* d1, double* d2){
    struct retBuf* ret = (struct retBuf*) malloc(sizeof(struct retBuf));
    IloNumArray sol(*env,n);
    IloNumArray buf (*env,n);
    IloNumArray perfsTab(*env,p);
    IloBoolVarArray dvarsTab(*env,n);
    for (int i=0; i <n; i++)
            dvarsTab[i] = *(dvars[i]);



    ret ->p=p;
    ret->n=n;
    ret->emptyP=true;
    ret->lastVal=IloInfinity;
    ret->opt2=crit;
    ret->sol = sol;
    ret->buf = buf;
    ret->toAddP = false;
    ret->perfs = perfsTab;
    ret->dvars = dvarsTab;
    ret->ctrs = ctrs;

//    cpx->use(lexMinHeuristic(*env,ret));
//    cpx->use(lexMinBranch(*env,d1,d2,ret));
    cpx->use(lexMinCallback(*env,perfs,dvars,chosen,d1,d2,ret));

   /* Paramètres faisant marcher cplex lex  
    cpx->setParam( IloCplex::Param::Preprocessing::Presolve,0);
    cpx->setParam(IloCplex::Symmetry,0);
    cpx->setParam(IloCplex::SolnPoolAGap,0);

    */
//    cpx->setParam( IloCplex::Param::Preprocessing::Presolve,1);
//    cpx->setParam(IloCplex::Symmetry,0);
    cpx->setParam(IloCplex::SolnPoolAGap,0);
//    cpx->setParam(IloCplex::EpAGap,0);
//    cpx->setParam(IloCplex::EpGap,0);



//    printf("[C] populateLim\n");
//    cpx->setParam(IloCplex::PopulateLim,20);
//    printf("[C] ok\n");
    return ret;
}

int isEmpty(struct retBuf* ret){if (ret->emptyP) return 1; else return 0; }
int resetBuf(struct retBuf* ret){ret->emptyP=true;
                                // ret->ctrs[0].setBounds(IloInfinity,IloInfinity);
                                // ret->ctrs[1].setUB(IloInfinity);
                                 ret->lastVal=IloInfinity;}
double getPerf(struct retBuf* ret, int i) {return ret->perfs[i-1];}
double getSol(struct retBuf* ret, int i) {return ret->sol[i-1];}
void deleteRetBuf(struct retBuf* ret) {ret->perfs.end(); ret->dvars.end(); ret->buf.end(); ret->sol.end();   free(ret);}


}


#endif
