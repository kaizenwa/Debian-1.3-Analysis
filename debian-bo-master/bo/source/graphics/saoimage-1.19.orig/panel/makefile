
#CFLAGS  = -c  -O -DPSCRIPT
CFLAGS  = -g -DPSCRIPT
LIBS = ../btnlib/tool/libbtool.a ../btnlib/libbtn.a

all:	main.o
	cc $(CFLAGS) main.o $(LIBS) -lX11 -o makepanel
	makepanel
	rm *.o makepanel

main.o:	main.mnu etc.mnu cmap.mnu color.mnu cursor.mnu pan.mnu region.mnu \
	scale.mnu ../btnlib/buttons.h ../btnlib/tool/btnmenu.h
