#$Id: domegcc.mak 1.1 1996/01/07 21:08:31 RICK Exp RICK $
#
#DOMEGCC.MAK - A Make File for compiling DOME 4.2 under GNU g++
#	       Copyright (C) 1996 Christopher J. Fearnley
#This program is free software; you can redistribute it and/or modify 
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
#any later version.
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

CC = g++
CFLAGS = -O2 -Wall
OBJS = dome.o geodesic.o dxfsave.o povsave.o wrlsave.o
INCLUDES = -I.
LFLAGS = -lm

all: dome dxftopov

dome:  $(OBJS)
    $(CC) $(CFLAGS) $(INCLUDES) -o dome $(OBJS) $(LFLAGS)

dxftopov:  dxftopov.cpp
    $(CC) $(CFLAGS) $(INCLUDES) -o dxftopov dxftopov.cpp

clean:
    rm *.o dome dxftopov

# Uses GNU make's Static Pattern Rules (May not be portable)
$(OBJS): %.o: geodesic.hpp %.cpp
    $(CC) $(CFLAGS) $(INCLUDES) -c $*.cpp -o $@

#With this make file, building the package under any Unix system with
#and g++ and GNU make goes like this:
#
#cjf@syntropy:/space/debian/my.deb.packages/dome-4.0$ make clean
#rm *.o dome dxftopov
#cjf@syntropy:/space/debian/my.deb.packages/dome-4.0$ make
#g++ -O2 -Wall -I. -c dome.cpp -o dome.o
#dome.cpp:100: warning: `char rcsid[59]' defined but not used
#g++ -O2 -Wall -I. -c geodesic.cpp -o geodesic.o
#g++ -O2 -Wall -I. -c dxfsave.cpp -o dxfsave.o
#g++ -O2 -Wall -I. -c wrlsave.cpp -o wrlsave.o
#g++ -O2 -Wall -I. -o dome dome.o geodesic.o dxfsave.o povsave.o wrlsave.o -lm
#g++ -O2 -Wall -I. -o dxftopov dxftopov.cpp
#dxftopov.cpp:75: warning: return type for `main' changed to integer type
#dxftopov.cpp:70: warning: `char rcsid[77]' defined but not used

