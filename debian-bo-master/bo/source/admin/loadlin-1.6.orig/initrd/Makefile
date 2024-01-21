CC = gcc
CFLAGS = -m386 -O2  -fomit-frame-pointer -static

ifneq (1,$(shell if grep __ELF__ `gcc -v 2>&1|head -n1|cut -d' ' -f4` 1>/dev/null 2>&1;then echo -n 1;fi))
  @echo ""; \
   echo "needing ELF development tools, giving up";
  exit 1;
endif

all: initdisk freeramdisk Makefile

linuxrc: linuxrc.c
	$(CC) $(CFLAGS) -o linuxrc linuxrc.c

initdisk: linuxrc Makefile freeramdisk mkdisk
	mkdisk linuxrc

freeramdisk: freeramdisk.c
	$(CC) -m386 -O2 -o freeramdisk freeramdisk.c

clean:
	rm -f hello initdisk freeramdisk *~ *.o
