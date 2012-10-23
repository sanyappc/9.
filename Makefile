CC = ghc
EXECUTABLE = 9dot
CFLAGS = 
MODULESDIR = ./modules/
MODULE = NDType.hs NDActionHandlers.hs NDAction.hs NDParse.hs Runtime.hs
MODULES = $(addprefix $(MODULESDIR), $(MODULE))
TARGET = main.hs

all:
	$(CC) $(CFLAGS) $(MODULES) ./$(TARGET) -o $(EXECUTABLE)
	if [ ! -d bin ] ; then mkdir ./bin ; fi 
	mv $(EXECUTABLE) ./bin/
	find . -name '*.hi' -delete
	find . -name '*.hs.*' -delete
	find . -name '*.o' -delete
	find . -name '*.hs~' -delete
clean:	
	rm -rf ./bin
