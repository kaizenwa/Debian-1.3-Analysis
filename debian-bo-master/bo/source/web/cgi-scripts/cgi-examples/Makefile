# For gcc
CC= gcc
# For ANSI compilers
#CC= cc

#For Optimization
CFLAGS= -O2 -s

RM= /bin/rm -f

EXE = jj phf

#--- You shouldn't have to edit anything else. ---

.c.o: 
	$(CC) -c $(CFLAGS) $<

all: $(EXE)

jj: jj.o util.o
	$(CC) $(LDFLAGS) -o $@ jj.o util.o
phf: phf.o util.o
	$(CC) $(LDFLAGS) -o $@ phf.o util.o

clean:
	$(RM) *.o

distclean: clean
	$(RM) $(EXE)

