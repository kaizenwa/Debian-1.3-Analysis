#
# Makefile for stdio
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I../ldouble -I../ldouble -I../ldouble -I../ldouble

DIRS:=

SRC1S= clearerr.c feof.c ferror.c fgetc.c fileno.c \
	fputc.c freopen.c fseek.c getc.c getchar.c pclose.c putc.c \
	putchar.c rewind.c setbuf.c setfileno.c tempnam.c \
	tmpfile.c tmpnam.c vfscanf.c vprintf.c vscanf.c getw.c \
	putw.c stdio.c setlinebuf.c snprintf.c vsnprintf.c
#	getline.c
# These are compiled from the corresponding ../ioFOO.c files.
IOSRCS= fclose.c fflush.c fgetpos.c fgets.c fopen.c \
	fprintf.c fputs.c fread.c fscanf.c fsetpos.c ftell.c \
	fwrite.c gets.c perror.c printf.c puts.c scanf.c \
	setbuffer.c setvbuf.c sprintf.c sscanf.c ungetc.c \
	vsprintf.c vsscanf.c popen.c
SRC2S= remove.c vfprintf.c fdopen.c $(IOSRCS)

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets

realclean clean::
	$(RM) -f $(IOSRCS)

fclose.c:
	@echo "#undef HAVE_GNU_LD" > fclose.c
	@echo "#define HAVE_GNU_LD" >> fclose.c
	@echo "#include <gnu-stabs.h>" >> fclose.c
	@echo "#undef fclose" >> fclose.c
	@echo "symbol_alias (_IO_fclose, fclose);" >> fclose.c

fflush.c:
	@echo "#undef HAVE_GNU_LD" > fflush.c
	@echo "#define HAVE_GNU_LD" >> fflush.c
	@echo "#include <gnu-stabs.h>" >> fflush.c
	@echo "#undef fflush" >> fflush.c
	@echo "symbol_alias (_IO_fflush, fflush);" >> fflush.c

fgetpos.c:
	@echo "#undef HAVE_GNU_LD" > fgetpos.c
	@echo "#define HAVE_GNU_LD" >> fgetpos.c
	@echo "#include <gnu-stabs.h>" >> fgetpos.c
	@echo "#undef fgetpos" >> fgetpos.c
	@echo "symbol_alias (_IO_fgetpos, fgetpos);" >> fgetpos.c

fgets.c:
	@echo "#undef HAVE_GNU_LD" > fgets.c
	@echo "#define HAVE_GNU_LD" >> fgets.c
	@echo "#include <gnu-stabs.h>" >> fgets.c
	@echo "#undef fgets" >> fgets.c
	@echo "symbol_alias (_IO_fgets, fgets);" >> fgets.c

fopen.c:
	@echo "#undef HAVE_GNU_LD" > fopen.c
	@echo "#define HAVE_GNU_LD" >> fopen.c
	@echo "#include <gnu-stabs.h>" >> fopen.c
	@echo "#undef fopen" >> fopen.c
	@echo "symbol_alias (_IO_fopen, fopen);" >> fopen.c

fprintf.c:
	@echo "#undef HAVE_GNU_LD" > fprintf.c
	@echo "#define HAVE_GNU_LD" >> fprintf.c
	@echo "#include <gnu-stabs.h>" >> fprintf.c
	@echo "#undef fprintf" >> fprintf.c
	@echo "symbol_alias (_IO_fprintf, fprintf);" >> fprintf.c

fputs.c:
	@echo "#undef HAVE_GNU_LD" > fputs.c
	@echo "#define HAVE_GNU_LD" >> fputs.c
	@echo "#include <gnu-stabs.h>" >> fputs.c
	@echo "#undef fputs" >> fputs.c
	@echo "symbol_alias (_IO_fputs, fputs);" >> fputs.c

fread.c:
	@echo "#undef HAVE_GNU_LD" > fread.c
	@echo "#define HAVE_GNU_LD" >> fread.c
	@echo "#include <gnu-stabs.h>" >> fread.c
	@echo "#undef fread" >> fread.c
	@echo "symbol_alias (_IO_fread, fread);" >> fread.c

fscanf.c:
	@echo "#undef HAVE_GNU_LD" > fscanf.c
	@echo "#define HAVE_GNU_LD" >> fscanf.c
	@echo "#include <gnu-stabs.h>" >> fscanf.c
	@echo "#undef fscanf" >> fscanf.c
	@echo "symbol_alias (_IO_fscanf, fscanf);" >> fscanf.c

fsetpos.c:
	@echo "#undef HAVE_GNU_LD" > fsetpos.c
	@echo "#define HAVE_GNU_LD" >> fsetpos.c
	@echo "#include <gnu-stabs.h>" >> fsetpos.c
	@echo "#undef fsetpos" >> fsetpos.c
	@echo "symbol_alias (_IO_fsetpos, fsetpos);" >> fsetpos.c

ftell.c:
	@echo "#undef HAVE_GNU_LD" > ftell.c
	@echo "#define HAVE_GNU_LD" >> ftell.c
	@echo "#include <gnu-stabs.h>" >> ftell.c
	@echo "#undef ftell" >> ftell.c
	@echo "symbol_alias (_IO_ftell, ftell);" >> ftell.c

fwrite.c:
	@echo "#undef HAVE_GNU_LD" > fwrite.c
	@echo "#define HAVE_GNU_LD" >> fwrite.c
	@echo "#include <gnu-stabs.h>" >> fwrite.c
	@echo "#undef fwrite" >> fwrite.c
	@echo "symbol_alias (_IO_fwrite, fwrite);" >> fwrite.c

gets.c:
	@echo "#undef HAVE_GNU_LD" > gets.c
	@echo "#define HAVE_GNU_LD" >> gets.c
	@echo "#include <gnu-stabs.h>" >> gets.c
	@echo "#undef gets" >> gets.c
	@echo "symbol_alias (_IO_gets, gets);" >> gets.c

perror.c:
	@echo "#undef HAVE_GNU_LD" > perror.c
	@echo "#define HAVE_GNU_LD" >> perror.c
	@echo "#include <gnu-stabs.h>" >> perror.c
	@echo "#undef perror" >> perror.c
	@echo "symbol_alias (_IO_perror, perror);" >> perror.c

printf.c:
	@echo "#undef HAVE_GNU_LD" > printf.c
	@echo "#define HAVE_GNU_LD" >> printf.c
	@echo "#include <gnu-stabs.h>" >> printf.c
	@echo "#undef printf" >> printf.c
	@echo "symbol_alias (_IO_printf, printf);" >> printf.c

puts.c:
	@echo "#undef HAVE_GNU_LD" > puts.c
	@echo "#define HAVE_GNU_LD" >> puts.c
	@echo "#include <gnu-stabs.h>" >> puts.c
	@echo "#undef puts" >> puts.c
	@echo "symbol_alias (_IO_puts, puts);" >> puts.c

scanf.c:
	@echo "#undef HAVE_GNU_LD" > scanf.c
	@echo "#define HAVE_GNU_LD" >> scanf.c
	@echo "#include <gnu-stabs.h>" >> scanf.c
	@echo "#undef scanf" >> scanf.c
	@echo "symbol_alias (_IO_scanf, scanf);" >> scanf.c

setbuffer.c:
	@echo "#undef HAVE_GNU_LD" > setbuffer.c
	@echo "#define HAVE_GNU_LD" >> setbuffer.c
	@echo "#include <gnu-stabs.h>" >> setbuffer.c
	@echo "#undef setbuffer" >> setbuffer.c
	@echo "symbol_alias (_IO_setbuffer, setbuffer);" >> setbuffer.c

setvbuf.c:
	@echo "#undef HAVE_GNU_LD" > setvbuf.c
	@echo "#define HAVE_GNU_LD" >> setvbuf.c
	@echo "#include <gnu-stabs.h>" >> setvbuf.c
	@echo "#undef setvbuf" >> setvbuf.c
	@echo "symbol_alias (_IO_setvbuf, setvbuf);" >> setvbuf.c

sprintf.c:
	@echo "#undef HAVE_GNU_LD" > sprintf.c
	@echo "#define HAVE_GNU_LD" >> sprintf.c
	@echo "#include <gnu-stabs.h>" >> sprintf.c
	@echo "#undef sprintf" >> sprintf.c
	@echo "symbol_alias (_IO_sprintf, sprintf);" >> sprintf.c

sscanf.c:
	@echo "#undef HAVE_GNU_LD" > sscanf.c
	@echo "#define HAVE_GNU_LD" >> sscanf.c
	@echo "#include <gnu-stabs.h>" >> sscanf.c
	@echo "#undef sscanf" >> sscanf.c
	@echo "symbol_alias (_IO_sscanf, sscanf);" >> sscanf.c

ungetc.c:
	@echo "#undef HAVE_GNU_LD" > ungetc.c
	@echo "#define HAVE_GNU_LD" >> ungetc.c
	@echo "#include <gnu-stabs.h>" >> ungetc.c
	@echo "#undef ungetc" >> ungetc.c
	@echo "symbol_alias (_IO_ungetc, ungetc);" >> ungetc.c

vsprintf.c:
	@echo "#undef HAVE_GNU_LD" > vsprintf.c
	@echo "#define HAVE_GNU_LD" >> vsprintf.c
	@echo "#include <gnu-stabs.h>" >> vsprintf.c
	@echo "#undef vsprintf" >> vsprintf.c
	@echo "symbol_alias (_IO_vsprintf, vsprintf);" >> vsprintf.c

vsscanf.c:
	@echo "#undef HAVE_GNU_LD" > vsscanf.c
	@echo "#define HAVE_GNU_LD" >> vsscanf.c
	@echo "#include <gnu-stabs.h>" >> vsscanf.c
	@echo "#undef vsscanf" >> vsscanf.c
	@echo "symbol_alias (_IO_vsscanf, vsscanf);" >> vsscanf.c

popen.c:
	@echo "#undef HAVE_GNU_LD" > popen.c
	@echo "#define HAVE_GNU_LD" >> popen.c
	@echo "#include <gnu-stabs.h>" >> popen.c
	@echo "#undef popen" >> popen.c
	@echo "symbol_alias (_IO_popen, popen);" >> popen.c
