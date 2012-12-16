HC = ghc
BINDIR = ./bin
#BINDIR = /usr/lib/cgi-bin
PROG = $(BINDIR)/cgi
MODULESDIR = ./modules/
MODULE = NDType.hs NDActionHandlers.hs NDAction.hs NDParse.hs Runtime.hs
MODULES = $(addprefix $(MODULESDIR), $(MODULE))
SOURCES = $(MODULES) ./cgi.hs

all: cons cleano
cons:
	@if [ ! -d $(BINDIR) ] ; then mkdir $(BINDIR) ; fi
	$(HC) $(SOURCES) -o $(PROG)
clean: cleano cleanb
cleano:	
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o)
cleanb:
	rm -f $(PROG)
