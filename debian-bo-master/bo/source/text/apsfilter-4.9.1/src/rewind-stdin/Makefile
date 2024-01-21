# not very tricky but portable ;-)
CC	= cc
CFLAGS	= -s -O

all: rewindstdin

rewindstdin: rewindstdin.c
	$(CC) $(CFLAGS) -o rewindstdin rewindstdin.c

install:
	./install_it

clean:
	rm -f *.o rewindstdin
