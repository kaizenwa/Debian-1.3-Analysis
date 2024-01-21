# Top level makefile to build Windows NT 3.5 version of ILU examples using
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

SUBDIRECTORIES= timeit.dir test1.dir test2.dir httest.dir iioptest1.dir
CLEANSUBS= timeit.clean test1.clean test2.clean httest.clean iioptest1.clean

################################################################################

ALL : $(SUBDIRECTORIES)

$(SUBDIRECTORIES) :
	cd $*
	nmake -f ilunt35.mak CFG="$(CFG)"
	cd ..

clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f ilunt35.mak CFG="$(CFG)" clean
	cd ..

 
# End 
################################################################################
