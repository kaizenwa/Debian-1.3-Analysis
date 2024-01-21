#
# Makefile for LINUXSYS
#

CC=bcc -a- -O2
ASM=tasm /zi /la /m99
LINK=tlink /v
LINKT=$(LINK) /t
EXE2BIN=exe2bin

all: syslinux.exe

clean:
        -del *.obj
        -del *.map
        -del ldlinux.sys
        -del ldlinux.bin
        -del ldlinux.exe
        -del ldlinux.lst
        -del mkrls.exe
        -del asmcode.c
        -del syslinux.exe

list: ldlinux.lst

print: ldlinux.lst
        copy lptcodes.prn+ldlinux.lst lpt1

ldlinux.lst: ldlinux.asm
        -$(ASM) ldlinux.asm, ldlinux.obj, ldlinux.lst

install: all
        syslinux a:

.c.obj:
        $(CC) -c $<

.asm.obj:
        $(ASM) $<

.obj.exe:
        $(LINK) $<

.exe.bin:
        $(EXE2BIN) $<

ldlinux.sys: ldlinux.bin mkrls.exe
        mkrls ldlinux.bin asmcode.c ldlinux.sys

asmcode.c: ldlinux.sys

asmcode.obj: asmcode.c

syslinux.obj: syslinux.c

ldlinux.bin: ldlinux.exe

ldlinux.exe: ldlinux.obj

ldlinux.obj: ldlinux.asm

mkrls.exe: mkrls.c
        $(CC) mkrls.c

syslinux.exe: syslinux.obj asmcode.obj
        $(CC) -esyslinux.exe syslinux.obj asmcode.obj
        hdropt syslinux.exe
        pklite syslinux.exe
