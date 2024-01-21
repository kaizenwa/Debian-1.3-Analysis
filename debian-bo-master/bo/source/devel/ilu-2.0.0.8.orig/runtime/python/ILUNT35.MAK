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

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
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
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 



################################################################################

ALL : PMAKE

clean :
    if exist WinDebug\nul del /q WinDebug\*
    if exist WinDebug\nul rmdir WinDebug
    if exist WinRel\nul del /q Winrel\*
    if exist WinRel\nul rmdir Winrel

PMAKE :
!MESSAGE WARNING: By default, the Python Language Specific Runtime is NOT built!
!MESSAGE To build it, set PYTHONSRC to point to your Python directory which contains
!MESSAGE the Python header (.h) files, the Python/Windows Import Library (python.lib)
!MESSAGE and the Python/Windows C Import Library (pythonc.lib)
!MESSAGE If you need these files, please retrieve them from http://www.python.org, as
!MESSAGE they are NOT distributed with ILU. (NOTE: pythonc.lib is created by building python
!MESSAGE and will be made available on the ILU pages)
!MESSAGE
!MESSAGE After doing this, uncomment the nmake line below, and re-start the make process.
!MESSAGE You will need to put $ILUHOME/lib on your PYTHONPATH before
!MESSAGE using ILU with Python.
!MESSAGE NOTE!!!  Due to some uses of the NDEBUG preprocessor symbol in the python source
!MESSAGE file object.h, building a Release version of the Python runtime is problematic.
!MESSAGE Until this is resolved, we ignore CFG here and always build Win32 Debug.
	copy pythonversion.win pythonversion.h
#	nmake -f iluPr.mak CFG="Win32 Debug"
#	nmake -f iluPr.mak CFG="$(CFG)"


# End 
################################################################################
