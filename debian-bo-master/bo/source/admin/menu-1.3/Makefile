CXXFLAGS=-O0 -g -Wall -fhandle-exceptions 
#Although I always test-compile this with -O0, the debian/rules file
#will call this makefile with CFLAGS=-O2, so your release is always optimised.

all: update-menus common.old.o


update-menus.o: adstring.h  update-menus.cc update-menus.h
	rm -f update-menus.o
	g++ -c -o update-menus.o $(CXXFLAGS) update-menus.cc

update-menus: update-menus.o adstring.o
	rm -f update-menus
	g++ -o update-menus $(CXXFLAGS) update-menus.o adstring.o

update-menus.old.o:update-menus.old.cc common.h update-menus.old.h
	rm -f update-menus.old.o
	g++ -c -o update-menus.old.o $(CXXFLAGS) update-menus.old.cc

update-menus.old: update-menus.old.o common.old.o
	rm -f update-menus.old
	g++ -o update-menus.old $(CXXFLAGS) update-menus.old.o common.old.o

clean   :
	rm -f *.o update-menus core update-menu update-new build


adstring.o: adstring.cc adstring.h
	g++ -c $(CXXFLAGS) -o adstring.o adstring.cc


backup  :
	#Sorry, but this is just so that I can backup more easily
	make clean
	make -C menu-method clean
	make -C fvwmgen clean
	-rm -r debian/tmp
	tar -cvzf $$GM0HOME/menu.tar.gz .

