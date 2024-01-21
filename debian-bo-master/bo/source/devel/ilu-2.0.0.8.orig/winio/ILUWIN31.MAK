# makefile to build Windows 3.1 version of ILU using
# Microsoft Visual C++ Version 1.51
# assumes PATH etc. are appropriately set

!IF "$(DEBUG)" == ""
DEBUG=0
!MESSAGE No configuration specified.  Defaulting to No Debug.
!MESSAGE Possible choices for configuration are: DEBUG=1 DEBUG=0
!ENDIF 


################################################################################

ALL : $(ILUHOME) WINIOMAKE

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)

WINIOMAKE :
	nmake -f winio16w.mak DEBUG=$(DEBUG)

clean :
    del *.obj
	del *.lib
	del *.pdb
	del *.res



# End 
################################################################################
