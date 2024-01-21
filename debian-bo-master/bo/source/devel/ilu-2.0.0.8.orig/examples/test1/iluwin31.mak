# makefile to build Windows 3.1 version of ILU test1 examples using
# Microsoft Visual C++ Version 1.51
#


!IF "$(DEBUG)" == ""
DEBUG=0
!MESSAGE No configuration specified.  Defaulting to No Debug.
!MESSAGE Possible choices for configuration are: DEBUG=1 DEBUG=0
!ENDIF 



################################################################################

ALL : TEST1MAKE


TEST1MAKE :
	nmake -f clntw16.mak DEBUG="$(DEBUG)"
	nmake -f srvrw16.mak DEBUG="$(DEBUG)"
	nmake -f cplntw16.mak DEBUG="$(DEBUG)"
	nmake -f cpsvrw16.mak DEBUG="$(DEBUG)"

clean :
    del *.obj
	del *.pch
	del *.exe
	del *.res
	del *.pdb



# End 
################################################################################
