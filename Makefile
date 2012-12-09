CC = ghc
#EXECUTABLE = 9dot
#CFLAGS = 
MODULESDIR = ./modules/
MODULE = NDType.hs NDActionHandlers.hs NDAction.hs NDParse.hs Runtime.hs
MODULES = $(addprefix $(MODULESDIR), $(MODULE))
#TARGET = main.hs

all: con
check:
	if [ ! -d bin ] ; then mkdir ./bin ; fi
con:
	if [ ! -d bin ] ; then mkdir ./bin ; fi
	$(CC) $(MODULES) ./main.hs -o ./bin/con
	make cleano

cgi:
	if [ ! -d bin ] ; then mkdir ./bin ; fi
	$(CC) $(MODULES) ./cgi.hs -o ./bin/cgi
	make cleano

gui:
	if [ ! -d bin ] ; then mkdir ./bin ; fi
	$(CC) $(MODULES) ./gtk.hs -o ./bin/gui
	make cleano

#start:
#	$(CC) $(CFLAGS) $(MODULES) ./$(TARGET) -o $(EXECUTABLE)
#finish:	
#	if [ ! -d bin ] ; then mkdir ./bin ; fi 
#	mv $(EXECUTABLE) ./bin/
clean: cleano cleanb
cleano:	
	find . -name '*.hi' -delete
	find . -name '*.hs.*' -delete
	find . -name '*.o' -delete
	find . -name '*.hs~' -delete
cleanb:
	rm -f ./bin/*
