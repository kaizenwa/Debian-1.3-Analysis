# Top level makefile to build Windows 3.1 version of ILU runtime & winio (no stubbers) using
# Microsoft Visual C++ Version 1.51
#
# Note you must have ILUHOME and ILUSRC and WSOCKHDR environment vars set before running this makefile 
# MODIFY the environment variables in the SUBDIRECTORIES target according to your locations

!IF "$(DEBUG)" == ""
DEBUG=0
!MESSAGE No configuration specified.  Defaulting to No Debug.
!MESSAGE Possible choices for configuration are: DEBUG=1 DEBUG=0
!ENDIF 


################################################################################

SUBDIRECTORIES= winio.dir runtime.dir
CLEANSUBS= winio.cln runtime.cln

################################################################################

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib $(SUBDIRECTORIES)

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

$(SUBDIRECTORIES) :
	SET PATH=E:\MSVC\BIN;C:\WINDOWS;C:\DOS
	SET LIB=E:\MSVC\LIB;E:\MSVC\MFC\LIB
	SET INCLUDE=E:\msvc\include;E:\msvc\MFC\include;$(ILUSRC)\RUNTIME\CPP;$(ILUSRC)\RUNTIME\C;$(ILUSRC)\RUNTIME\KERNEL;$(ILUSRC)\WINIO
	SET WSOCKHDR=E:\msvc\include\winsock.h
	cd $*
	nmake -f iluwin31.mak DEBUG=$(DEBUG)
	cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f iluwin31.mak DEBUG=$(DEBUG) clean
	cd ..


install : $(ILUHOME)\intrface $(ILUHOME)\include $(ILUHOME)\examples INSTLIBS

INSTLIBS:
	copy $(ILUSRC)\winio\winio16w.lib $(ILUHOME)\lib
	copy $(ILUSRC)\runtime\kernel\ilu16w.lib $(ILUHOME)\lib
	copy $(ILUSRC)\runtime\c\iluc16w.lib $(ILUHOME)\lib
	copy $(ILUSRC)\runtime\cpp\ilucp16w.lib $(ILUHOME)\lib


$(ILUHOME)\intrface : 
    if not exist $(ILUHOME)\intrface\nul mkdir $(ILUHOME)\intrface
	copy $(ILUSRC)\stubbers\parser\ilu.isl $(ILUHOME)\intrface


$(ILUHOME)\include : 
    if not exist $(ILUHOME)\include\nul mkdir $(ILUHOME)\include
	copy $(ILUSRC)\runtime\cpp\ilu.H $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\ilubasic.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\c\iluchdrs.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iludebug.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluerror.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluerrs.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluhash.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluntrnl.h $(ILUHOME)\include
	copy $(ILUSRC)\stubbers\parser\iluptype.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluxport.h $(ILUHOME)\include
	copy $(ILUSRC)\winio\winio.h $(ILUHOME)\include
	copy $(ILUSRC)\winio\winiodef.h $(ILUHOME)\include

$(ILUHOME)\examples : 
    if not exist $(ILUHOME)\examples\nul mkdir $(ILUHOME)\examples
	xcopy $(ILUSRC)\examples $(ILUHOME)\examples /s 


 
# End 
################################################################################
