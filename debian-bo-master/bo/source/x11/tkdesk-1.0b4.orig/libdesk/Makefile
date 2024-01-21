#-*- makefile -*-
#
# Makefile for libdesk (part of TkDesk)
#

OBJECTS = init.o dsk_ls.o misc.o util.o ot.o o_FileListbox.o tixImgXpm.o

LIBNAME = libdesk.a

# --- Rules: -----------------------------------------------------------------

default:
	cd ..; make -k shell

shell:
	cd ..; make -k shell

$(LIBNAME): $(OBJECTS)
	ar rc $@ $(OBJECTS)
	-ranlib $@

clean:
	rm -f $(LIBNAME) *.o #*#
