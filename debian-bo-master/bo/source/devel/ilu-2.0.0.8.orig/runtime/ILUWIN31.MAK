# makefile to build Windows 3.1 version of ILU using
# Microsoft Visual C++ Version 1.51
# assumes PATH etc. are appropriately set

!IF "$(DEBUG)" == ""
DEBUG=0
!MESSAGE No configuration specified.  Defaulting to No Debug.
!MESSAGE Possible choices for configuration are: DEBUG=1 DEBUG=0
!ENDIF 


################################################################################

SUBDIRECTORIES= kernel.dir c.dir cpp.dir
CLEANSUBS=  kernel.cln c.cln cpp.cln

################################################################################

ALL : $(ILUHOME) $(SUBDIRECTORIES)

$(ILUHOME) : 
    if not exist $(ILUHOME)\nul mkdir $(ILUHOME)


$(SUBDIRECTORIES) :
	cd $*
	nmake -f iluwin31.mak DEBUG=$(DEBUG)
	cd ..


clean : $(CLEANSUBS)

$(CLEANSUBS) :
	cd $*
	nmake -f iluwin31.mak DEBUG=$(DEBUG) clean
	cd ..
	


# End 
################################################################################
