VPATH = src:$(DIR_OBJ)
#######################################
LIBLFI = libNEWLFI_ALL.a
DIR_OBJ = ./$(ARCH)

DIR_CONF:=$(shell pwd|sed -e 's/lib\/.*/conf/')

include $(DIR_CONF)/config.$(ARCH)
include Rules.$(ARCH)

%.o : %.f
	$(CPP) $(CPPFLAGS) $< > $(DIR_OBJ)/cpp_$(*F).f
	$(F77) -c $(F77FLAGS) -o $(DIR_OBJ)/$(*F).o $(DIR_OBJ)/cpp_$(*F).f

%.o : %.f90
	$(CPP) $(INC) $(CPPFLAGS) $< > $(DIR_OBJ)/cpp_$(*F).f90
	$(F90) $(INC) -c $(F90FLAGS) -o $(DIR_OBJ)/$(*F).o $(DIR_OBJ)/cpp_$(*F).f90

%.o : %.c
	$(CC) $(INC) $(CFLAGS) $(CPPFLAGS) -c -o $(DIR_OBJ)/$(*F).o  $<



$(LIBLFI) : $(DIR_OBJ)/.dummy $(OBJS)
	cd $(DIR_OBJ); $(AR) crv $@ $(OBJS)

$(DIR_OBJ)/.dummy :
	mkdir -p $(DIR_OBJ)
	@touch $(DIR_OBJ)/.dummy

clean:
	(if [ -d $(DIR_OBJ) ] ; then cd $(DIR_OBJ); rm -f cpp_*.f* *.o ; fi)

distclean:
	rm -rf $(DIR_OBJ)
