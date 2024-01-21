# Top level makefile to build Windows 3.1 version of ILU examples using
# Microsoft Visual C++ Version  1.51
#
# Note you must have ILUHOME and ILUSRC WSOCKLIB and WSOCKHDR environment vars set before running this makefile 
# MODIFY the environment variables in the SUBDIRECTORIES target according to your locations

!IF "$(DEBUG)" == ""
DEBUG=0
!MESSAGE No configuration specified.  Defaulting to No Debug.
!MESSAGE Possible choices for configuration are: DEBUG=1 DEBUG=0
!ENDIF 



################################################################################

SUBDIRECTORIES= test1.dir
CLEANSUBS= test1.cln

################################################################################

ALL : $(SUBDIRECTORIES)

$(SUBDIRECTORIES) :
	SET PATH=D:\MSVC\BIN;D:\WINDOWS;C:\DOS
	SET LIB=D:\MSVC\LIB;d:\MSVC\MFC\LIB
	SET INCLUDE=$(ILUSRC)\RUNTIME\CPP;$(ILUSRC)\RUNTIME\C;$(ILUSRC)\RUNTIME\KERNEL;$(ILUSRC)\WINIO;D:\MSVC\INCLUDE;D:\MSVC\MFC\INCLUDE
	SET WSOCKHDR=d:\msvc\include\winsock.h
	SET WSOCKLIB=d:\msvc\lib\winsock.lib
	cd $*
	nmake -f iluwin31.mak DEBUG="$(DEBUG)"
	cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f iluwin31.mak DEBUG="$(DEBUG)" clean
	cd ..

 
# End 
################################################################################
