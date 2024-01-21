
CXXFLAGS = $(BASIC_C_FLAGS) -I.

ifeq ($(strip $(TOP_LEVEL)),)
all: error
else
#
# If .depend exists, it is included it here.
# Else, .depend is made before building.
#
ifeq (.depend,$(wildcard .depend))
all: libmisc.a
include .depend
else
all: dep libmisc.a
endif
endif

SRC = vllist.cc pool.cc 
OBJ = vllist.o pool.o 

error:
	@echo '+-------------------------------------------+'
	@echo '| Please run the top level Makefile instead |'
	@echo '|            of this one                    |'
	@echo '+-------------------------------------------+'

libmisc.a:	$(OBJ)
	rm -f libmisc.a
	ar -rc libmisc.a $(OBJ)
	ranlib libmisc.a

clean:
	rm -f *.a *.o *.*~ *~ .depend

dep:
	rm -f .depend
	gcc -c $(CXXFLAGS) -M $(SRC) > .depend