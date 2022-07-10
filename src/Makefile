

CPLEXDIR=/home/sat/cplex12
VERSION=x86-64_linux

INCLUDES=-I$(CPLEXDIR)/cplex/include -I$(CPLEXDIR)/concert/include
LIBSDIR=-L. -L$(CPLEXDIR)/cplex/lib/$(VERSION)/static_pic -L$(CPLEXDIR)/concert/lib/$(VERSION)/static_pic
LIBS= -lgomp -lconcert -lilocplex -lcplex -lm -lpthread -lstdc++
FLAGS=-debug -O0 -O -fPIC -DNDEBUG -DIL_STD 
CFLAGS= -DNDEBUG -DIL_STD -g
CC=g++ 
HSFLAGS = -threaded -debug -fffi -rtsopts #-O2 -threaded 
BUILDDIR=obj
VPATH=cbits
INCLDIR=include


FILENAME= obj/IloEnv.o obj/IloModel.o obj/IloBoolVar.o obj/IloNumVar.o obj/IloObjective.o obj/IloRange.o obj/IloCplex.o
#all: $(BUILDDIR)/IloEnv.o
#	ghc Main.hs ${FILENAME} ${HSFLAGS} $(INCLUDES) $(LIBSDIR) $(LIBS)
#
all:
#	ghc -osuf .ho -prof -fprof-auto -O --make Main 
#	ghc -O --make Main -O3 
	ghc -O --make Main -O5 -dynamic 


$(BUILDDIR)/%.o: %.c 
	g++ $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	find -type f -name "*.dyn_hi" -delete
	find -type f -name "*.dyn_o" -delete
	find -type f -name "*.hi" -delete
	find -type f -name "*.ho" -delete
	find -type f -name "*.o" -delete
