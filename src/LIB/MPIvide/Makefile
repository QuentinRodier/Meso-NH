VPATH=$(DIR_OBJ)
#######################################
DIR_OBJ = ./$(ARCH)

DIR_CONF:=$(shell pwd|sed -e 's/lib\/.*/conf/')

include $(DIR_CONF)/config.$(ARCH)
include Rules.$(ARCH)

libmpivide.a : $(DIR_OBJ)/.dummy mpivide.o
	cd $(DIR_OBJ); $(AR) -cr $@ mpivide.o


mpivide.o : mpivide.c
	$(CC) -c $(CFLAGS) -DFUJI -I./include $< -o $(DIR_OBJ)/$(*F).o


$(DIR_OBJ)/.dummy:
	mkdir -p $(DIR_OBJ)
	@touch $(DIR_OBJ)/.dummy

clean:
	(if [ -d $(DIR_OBJ) ] ; then cd $(DIR_OBJ); rm -f cpp_*.f90 *.o ; fi)

distclean:
	rm -rf $(DIR_OBJ)
