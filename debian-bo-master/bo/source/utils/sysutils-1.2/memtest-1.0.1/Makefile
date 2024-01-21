CC = cc
CFLAGS = -v
REMOVE = rm

MEMTEST_SRC = memtest.c

all: memtest

makemem: $(CC) $(FLAGS) $(MEMTEST_SRC) -o memtest

install:
	chmod 755 memtest

clean:
	rm memtest
