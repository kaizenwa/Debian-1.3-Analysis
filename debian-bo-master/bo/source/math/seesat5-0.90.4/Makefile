# This Makefile builds seesat5

SHELL = /bin/sh

srcdir = .

CC = gcc -c
LN = gcc -o
OBJ = seesat.o driver.o util.o sgp4.o astro.o readel.o
LIB = -L/usr/lib/ 

all : seesat5

seesat5 : $(OBJ) cr.o
	$(LN) seesat5 $(LIB) $(OBJ) -lm 
	$(LN) cr cr.o

seesat.o : seesat.c SEESAT.H
	$(CC) seesat.c
driver.o : driver.c SEESAT.H
	$(CC) driver.c
util.o : util.c SEESAT.H
	$(CC) util.c
sgp4.o : sgp4.c SEESAT.H
	$(CC) sgp4.c
astro.o : astro.c SEESAT.H
	$(CC) astro.c
readel.o : readel.c SEESAT.H  
	$(CC) readel.c
cr.o : cr.c
	$(CC) cr.c
clean :
	rm seesat5 seesat.o driver.o util.o sgp4.o astro.o readel.o
	rm cr cr.o
