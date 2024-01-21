# Top level makefile to build Windows NT 3.5 version of ILU using
# Microsoft Visual C++ Version 2.00
#
# Assumes your PATH has the appropriate bin directory in it (e.g. msvc20\bin)
# Assumes your INCLUDE environment variable includes the appropriate directories (e.g. msvc20\include) 
# Assumes your LIB environment variable includes the appropriate directories (e.g. msvc20\lib) 

!IF "$(CFG)" == ""
CFG=Win32 Release
!MESSAGE No configuration specified.  Defaulting to Win32 Release.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug" && "$(CFG)" != "clean"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "ilunt35.mak" CFG="Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release"
!MESSAGE "Win32 Debug"
!MESSAGE "clean"
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 


################################################################################

SUBDIRECTORIES= sbfile.dir

CLEANSUBS=  sbfile.clean

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
