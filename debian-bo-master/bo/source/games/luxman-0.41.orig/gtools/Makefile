# Binaries to make and install
BINS = map2c maped mapsize # grx2map

CXXFLAGS = $(BASIC_C_FLAGS) -I..

ifeq ($(strip $(TOP_LEVEL)),)
all: err_msg
else
#
# If .depend exists, it is included it here.
# Else, .depend is made before building.
#
ifeq (.depend,$(wildcard .depend))
all: install
include .depend
else
all: dep install
endif
endif

# libgtools sources
SRC1 = bitmap.cc button.cc font.cc geom.cc gr.cc grid.cc \
	handle.cc mouse.cc screen.cc util.cc fmalloc.cc \
	input.cc

OBJ1 = bitmap.o button.o font.o geom.o gr.o grid.o \
	handle.o mouse.o screen.o util.o fmalloc.o \
	input.o

SRCX = maped.cc map2c.cc mapsize.cc # grx2map.cc

MAPED_MAPS = ./maps/darrow.map ./maps/larrow.map ./maps/lr_arrow.map \
	./maps/rarrow.map ./maps/uarrow.map ./maps/ud_arrow.map

err_msg:
	@echo '+-------------------------------------------+'
	@echo '| Please run the top level Makefile instead |'
	@echo '|            of this one                    |'
	@echo '+-------------------------------------------+'

clean:
	rm -f *.a *.o *.*~ *~ $(BINS) maped_maps.c .depend core

libgtools.a:	$(OBJ1)
	rm -f libgtools.a
	ar -rc libgtools.a $(OBJ1)
	ranlib libgtools.a

map2c: map2c.o libgtools.a
	gcc $(LDFLAGS) -o map2c map2c.o libgtools.a -lvga -lm

maped_maps.c:	$(MAPED_MAPS) 
	./map2c maped_maps.c $(MAPED_MAPS)

maped.cc: maped_maps.c

maped: maped.o libgtools.a
	gcc $(LDFLAGS) -o maped maped.o libgtools.a -lvga -lm

grx2map: grx2map.o libgtools.a
	gcc $(LDFLAGS) -o grx2map grx2map.o libgtools.a -lgrx -lvga -lm

mapsize: mapsize.o libgtools.a
	gcc $(LDFLAGS) -o mapsize mapsize.o libgtools.a -lvga -lm

install: $(BINS)

dep:	
	rm -f .depend
	gcc -c $(CXXFLAGS) -M -DDEPEND $(SRC1) $(SRCX) > .depend


