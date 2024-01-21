INCDIR = ../include
GL_LIBS = generic.o -L../lib -lMesaaux -lMesaGLU -lMesaGL $(XLIBS)
LIB_DEP = ../lib/$(GL_LIB) ../lib/$(GLU_LIB) ../lib/$(TK_LIB) ../lib/$(AUX_LIB)
PROGS = chess cone_ball material tea teapots texgen nurbs surface\
	checker checker2 texturesurf sphere cone font model disk

all: generic.o $(PROGS)
	(cd MesaView; gnumake)

generic.o:
	$(CC) -c -I$(INCDIR) $(CFLAGS) $< -o $@

.c: $(LIB_DEP) generic.o
	$(CC) -I$(INCDIR) $(CFLAGS) $< $(GL_LIBS) -framework AppKit -o $@

clean:
	rm -f $(PROGS) generic.o
	(cd MesaView; gnumake clean)
