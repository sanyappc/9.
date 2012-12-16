HC = ghc
BINDIR = ./bin
PROG = $(BINDIR)/graph
MODULESDIR = ./modules/
MODULE = NDType.hs NDActionHandlers.hs NDAction.hs NDParse.hs NDGraph.hs
MODULES = $(addprefix $(MODULESDIR), $(MODULE))
SOURCES = $(MODULES) ./graph.hs

all: cons cleano
cons:
	@if [ ! -d $(BINDIR) ] ; then mkdir $(BINDIR) ; fi
	$(HC) $(SOURCES) -o $(PROG)
clean: cleano cleanb
cleano:	
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o)
cleanb:
	rm -f $(PROG)
