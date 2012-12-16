all: cons

cons: 
	@make -f ./makefiles/Makecons.mk
gui: 
	@make -f ./makefiles/Makegui.mk
cgi:
	@make -f ./makefiles/Makecgi.mk
graph:
	@make -f ./makefiles/Makegraph.mk
clean: 
	@make -f ./makefiles/Makecons.mk clean
	@make -f ./makefiles/Makegui.mk clean
	@make -f ./makefiles/Makegraph.mk clean
	@make -f ./makefiles/Makecgi.mk clean
