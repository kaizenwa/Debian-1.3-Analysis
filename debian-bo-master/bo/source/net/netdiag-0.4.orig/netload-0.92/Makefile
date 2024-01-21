CC=		gcc
CLIBS=		-lncurses
TARGET=		"/usr/bin/"	

all :
	$(CC) netload.c -o netload $(CLIBS)

install:
	install -m 755 netload $(TARGET) 
clean:
	rm -f ./netload
