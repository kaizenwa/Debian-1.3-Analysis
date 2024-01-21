LDFLAGS += -N
CFLAGS = $(O) -Wall

all: netdate

netdate: netdate.o

install: netdate
	install -s -m 0755 netdate /sbin/netdate
	install -m 0644 netdate.8 /usr/man/man8

clean:
	rm -f netdate *.o core

