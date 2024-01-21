# Top level makefile to build Windows NT 3.5 version of ILU using
# Microsoft Visual C++ Version 2.00
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc20\bin)
# Assumes your INCLUDE environment variable includes the appropriate directories (e.g. msvc20\include) 
# Assumes your LIB environment variable includes the appropriate directories (e.g. msvc20\lib) 

!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!MESSAGE Possible choices for configuration are: "Win32 Release" "Win32 Debug"
!ENDIF 


################################################################################

SUBDIRECTORIES= winio.dir runtime.dir stubbers.dir etc.dir
CLEANSUBS= winio.clean runtime.clean stubbers.clean etc.clean

################################################################################

ALL : $(ILUHOME) $(ILUHOME)\bin $(ILUHOME)\lib $(SUBDIRECTORIES)

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

$(ILUHOME)\lib : 
    if not exist $(ILUHOME)\lib\nul mkdir $(ILUHOME)\lib

$(ILUHOME)\bin : 
    if not exist $(ILUHOME)\bin\nul mkdir $(ILUHOME)\bin

$(SUBDIRECTORIES) :
	cd $*
	nmake -f ilunt35.mak CFG="$(CFG)"
	cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f ilunt35.mak CFG="$(CFG)" clean
	cd ..


install : $(ILUHOME)\interfaces $(ILUHOME)\include $(ILUHOME)\examples $(ILUHOME)\pythonfiles


$(ILUHOME)\interfaces : 
    if not exist $(ILUHOME)\interfaces\nul mkdir $(ILUHOME)\interfaces
	copy $(ILUSRC)\stubbers\parser\ilu.isl $(ILUHOME)\interfaces
	copy $(ILUSRC)\stubbers\parser\http.isl $(ILUHOME)\interfaces


$(ILUHOME)\include : 
    if not exist $(ILUHOME)\include\nul mkdir $(ILUHOME)\include
	copy $(ILUSRC)\runtime\cpp\ilu.hh $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\ilubasic.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\c\iluchdrs.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iludebug.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluerror.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluerrs.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluhash.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluntrnl.h $(ILUHOME)\include
	copy $(ILUSRC)\stubbers\parser\iluptype.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluxport.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\ilusock.h $(ILUHOME)\include
	copy $(ILUSRC)\runtime\kernel\iluwin.h $(ILUHOME)\include
	copy $(ILUSRC)\winio\winio.h $(ILUHOME)\include
	copy $(ILUSRC)\winio\winiodef.h $(ILUHOME)\include

$(ILUHOME)\examples : 
    if not exist $(ILUHOME)\examples\nul mkdir $(ILUHOME)\examples
	xcopy $(ILUSRC)\examples $(ILUHOME)\examples /s 

$(ILUHOME)\pythonfiles :	$(ILUHOME)\lib
	copy $(ILUSRC)\runtime\python\*.py $(ILUHOME)\lib
	
 
# End 
################################################################################
