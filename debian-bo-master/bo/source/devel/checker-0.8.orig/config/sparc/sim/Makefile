CFLAGS=-g -Wall #-O3 -Wall -fPIC -D__PIC__
ASFLAGS=-fPIC
OBJS= instr.o run_orig.o vprintf.o

#CFLAGS=-O -Wall -g
#OBJS= instr.o run_orig.o vprintf.o
#TARGET=dum1

sstep: sstep.o ss_disassem.o syscallname.o
	gcc -o sstep sstep.o ss_disassem.o syscallname.o

ss_disassem.o: disassem.c
	$(CC) -o ss_disassem.o -DSSTEP disassem.c -c

$(TARGET): $(OBJS) $(TARGET).o
	gcc -o $(TARGET) $(TARGET).o $(OBJS)

tp: $(OBJS) tp.o
	gcc -o tp tp.o $(OBJS)

sim.so.1: $(OBJS)
	$(LD) -G -z text -B symbolic -e _init -o sim.so.1 $(OBJS) -lc

clean:
	rm -f *.o *~ sstep tp

instr.o: instr.c instr.h
run_orig.o: run_orig.c instr.h
