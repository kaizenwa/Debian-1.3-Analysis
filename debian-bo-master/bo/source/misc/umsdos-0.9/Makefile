GPP=g++
GCC=gcc
MAKECMD=$(MAKE) GCC=$(GCC) GPP=$(GPP)
all:
	$(MAKECMD) -C util all
	$(MAKECMD) -C tests utstgen utstspc

install:
	$(MAKECMD) -C util install

clean:
	$(MAKECMD) -C util clean
	$(MAKECMD) -C tests clean

