CC     = gcc
CFLAGS = -Wall -DPOSIX
LDFLAGS= 

testgl: libgetline.a testgl.o
	$(CC) $(LDFLAGS) $(CFLAGS) -o testgl testgl.o -L. -lgetline

libgetline.a: getline.o
	ar cr libgetline.a getline.o
	-ranlib libgetline.a

clean:
	rm -f *.o *.a testgl
