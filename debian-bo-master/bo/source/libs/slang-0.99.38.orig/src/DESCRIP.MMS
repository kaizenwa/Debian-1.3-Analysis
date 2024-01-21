# Do not run mms/mmk directly.  Do @vmsmake which will call mms/mmk.
#
#  MMS file to build S-Lang (Thanks to Hunter Goatley)
#  Modified by zinser@axp602.gsi.de
#
.IFDEF __MMK__
.ELSE
EXE = .EXE
OBJ = .OBJ
OLB = .OLB
.ENDIF

OBJS =	SLANG$(OBJ),SLPARSE$(OBJ),SLMATH$(OBJ),SLFILE$(OBJ),-
	SLSTD$(OBJ),SLARRAY$(OBJ),SLREGEXP$(OBJ),SLERR$(OBJ),-
	SLKEYMAP$(OBJ),SLRLINE$(OBJ),SLTOKEN$(OBJ),SLGETKEY$(OBJ),-
	SLVMSTTY$(OBJ),SLMEMCMP$(OBJ),SLMEMCHR$(OBJ),SLMEMCPY$(OBJ)-
	SLDISPLY$(OBJ),SLSMG$(OBJ),SLSEARCH$(OBJ),SLCMD$(OBJ)-
	SLMALLOC$(OBJ),SLMEMSET$(OBJ),SLMISC$(OBJ),SLPREPR$(OBJ),-
	SLCLASS$(OBJ),SLTYPES$(OBJ),SLSCROLL$(OBJ),SLSIGNAL$(OBJ), -
	SLKEYPAD$(OBJ),SLERRNO$(OBJ),SLCURSES$(OBJ)

HFILES = SLANG.H,SLARRAY.H,_SLANG.H

all : calc$(exe) worm$(exe)

CALC$(EXE) : CALC$(OBJ),SLANG$(OLB)($(OBJS))
        $(LINK)$(LINKFLAGS)/NOTRACE CALC$(OBJ),client.opt/opt

CALC$(OBJ) :	CALC.C CONFIG.H

WORM$(EXE) : WORM$(OBJ),SLANG$(OLB)($(OBJS))
        $(LINK)$(LINKFLAGS)/NOTRACE WORM$(OBJ),client.opt/opt

WORM$(OBJ) :	WORM.C CONFIG.H
#
#  Most modules depend on most of the .H files, so I'm going to be lazy
#  and just make them all depend on all of them.
#
$(OBJS) : $(HFILES) CONFIG.H
