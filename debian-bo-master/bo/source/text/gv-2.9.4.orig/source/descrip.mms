!XCOMM
!XCOMM  descrip.mms
!XCOMM
!XCOMM ** Copyright (C) 1995-1997 Johannes Plass
!XCOMM 
!XCOMM  This program is free software; you can redistribute it and/or modify
!XCOMM  it under the terms of the GNU General Public License as published by
!XCOMM  the Free Software Foundation; either version 2 of the License, or
!XCOMM  (at your option) any later version.
!XCOMM 
!XCOMM  This program is distributed in the hope that it will be useful,
!XCOMM  but WITHOUT ANY WARRANTY; without even the implied warranty of
!XCOMM  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!XCOMM  GNU General Public License for more details.
!XCOMM 
!XCOMM  You should have received a copy of the GNU General Public License
!XCOMM  along with this program; if not, write to the Free Software
!XCOMM  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!XCOMM 
!XCOMM  Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
!XCOMM            Department of Physic
!XCOMM            Johannes Gutenberg-University
!XCOMM            Mainz, Germany
!XCOMM 

TARGET = GV
TARGET_TYPE = executable

.include X11_ROOT:[GV]CONFIG.VMS

CC_DEFS = VMS,GV_CODE,GV_LIB="""$(GV_LIB)"""
!MEMDEBUG = 1

.ifdef STATIC
.ifdef USE_FALLBACK_RESOURCES 
.else
USE_FALLBACK_RESOURCES = 1
.endif
.ifdef USE_FALLBACK_FILES 
.else
USE_FALLBACK_FILES     = 1
.endif
XMU_LIBRARY   = XMULIB
XAW3D_LIBRARY = XAW3DLIB
.else
XMU_LIBRARY   = XMULIBSHR
XAW3D_LIBRARY = XAW3DLIBSHR
.endif

.ifdef USE_FALLBACK_RESOURCES
CC_DEF_USE_FALLBACK_RESOURCES = ,USE_FALLBACK_RESOURCES
.endif 
.ifdef USE_FALLBACK_FILES
CC_DEF_USE_FALLBACK_FILES = ,USE_FALLBACK_FILES
.endif 
CC_DEFINES = /DEF=($(CC_DEFS)$(CC_DEF_USE_FALLBACK_RESOURCES)$(CC_DEF_USE_FALLBACK_FILES))

.ifdef __DECC__
CC_QUALIFIER = /NODEB/EXT=STRICT/PREF=ALL
.endif

.ifdef __VAXC__
CC_QUALIFIER = /NODEB/OPT=(D,I)
.endif

.include X11_ROOT:[COMMAND]X11_RULES.MMS

!### Objects #################################################################

.ifdef MEMDEBUG
DEBUG_OBJS =\
D_MEM.$(OBJ_EXT),\
D_XTMEM.$(OBJ_EXT),
.endif

OBJS =\
AAA.$(OBJ_EXT),\
AAA_BISON.$(OBJ_EXT),\
AAA_LEX.$(OBJ_EXT),\
ACTION_MAG.$(OBJ_EXT),\
ACTIONS.$(OBJ_EXT),\
CALLBACKS.$(OBJ_EXT),\
CLIP.$(OBJ_EXT),\
$(DEBUG_OBJS)\
DIALOG.$(OBJ_EXT),\
DOC_MISC.$(OBJ_EXT),\
ERROR.$(OBJ_EXT),\
FILE.$(OBJ_EXT),\
FILESEL.$(OBJ_EXT),\
FRAME.$(OBJ_EXT),\
GHOSTVIEW.$(OBJ_EXT),\
INFO.$(OBJ_EXT),\
MAIN.$(OBJ_EXT),\
MAIN_PARSECMDLINE.$(OBJ_EXT),\
MISC.$(OBJ_EXT),\
MISC_PRIVATE.$(OBJ_EXT),\
NOTE.$(OBJ_EXT),\
OPTIONS.$(OBJ_EXT),\
PROCESS.$(OBJ_EXT),\
POPUP.$(OBJ_EXT),\
POPUP_MISC.$(OBJ_EXT),\
PS.$(OBJ_EXT),\
SAVE.$(OBJ_EXT),\
STRCASECMP.$(OBJ_EXT),\
VERSION.$(OBJ_EXT),\
VMS_DIR.$(OBJ_EXT),\
WIDGETS_MISC.$(OBJ_EXT),\
ZOOM.$(OBJ_EXT)

GV_CONFIG_FILE      = [-]config.vms
GV_FONT_RES_DAT     = gv_font_res.dat
GV_MAKE_RES_DAT     = gv_make_res.dat
GV_MISC_RES_DAT     = gv_misc_res.dat
GV_USER_RES_DAT     = gv_user_res.dat
GV_INTERN_RES_DAT   = gv_intern_res_vms.dat
GV_INTERN_RES_H     = gv_intern_res_vms.h
GV_LAYOUT_RES_DAT   = gv_layout_res.dat
GV_COPYRIGHT_DAT    = gv_copyright.dat
GV_DUMMY_DAT        = gv_dummy.dat
GV_MESSAGES_DAT     = gv_messages.dat
GV_MESSAGES_H       = gv_messages.h
APP_DEFAULTS_H      = app-defaults.h
GV_CLASS_NAME       = GV
GV_CLASS_DAT        = gv_class.dat
GV_USER_DAT         = gv_user.dat
GV_SOURCE_IMAKEFILE = descrip.mms

.ifdef USE_FALLBACK_RESOURCES
APP_DEFAULTS_H_TARGET = $(APP_DEFAULTS_H)
.else
APP_DEFAULTS_H_TARGET =  
.endif

.ifdef USE_FALLBACK_FILES
GV_MESSAGES_H_TARGET = $(GV_MESSAGES_H)
.else
GV_MESSAGES_H__TARGET =  
.endif

!### Targets ##########################################################

DEFAULT :  $(TARGET).$(EXE_EXT)
 @  write_ sys$output ""
 @  write_ sys$output "  $(TARGET).$(EXE_EXT) available"
 @  write_ sys$output ""

$(TARGET).$(EXE_EXT) :	$(GV_CLASS_DAT) \
			$(GV_USER_DAT) \
			$(GV_MESSAGES_H__TARGET) $(APP_DEFAULTS_H_TARGET) \
			$(OBJS) $(XMU_LIBRARY) $(XAW3D_LIBRARY)
 @ write_ sys$output "  linking $(TARGET).$(EXE_EXT) ..." 
 @ LINK_/NODEB/NOTRACE -
       /EXE=$(TARGET).$(EXE_EXT) -
       $(OBJS),X11_LIBRARY:XAW3D_CLIENT.OPT/OPT
.ifdef USE_FALLBACK_RESOURCES
.else
 @ write_ sys$output "  copying $(GV_CLASS_DAT)		   -> X11_ROOT:[DEFAULTS]GV_CLASS.DAT"
 @ COPY_/NOLOG/NOCONF $(GV_CLASS_DAT) X11_ROOT:[DEFAULTS]$(GV_CLASS_DAT);
.endif
.ifdef USE_FALLBACK_FILES
.else
 @ write_ sys$output "  copying $(GV_MESSAGES_DAT)	-> $(GV_LIB)$(GV_MESSAGES_DAT)"
 @ COPY_/NOLOG/NOCONF  $(GV_MESSAGES_DAT) $(GV_LIB)$(GV_MESSAGES_DAT);
 @ write_ sys$output "  copying *.XBM			-> $(GV_LIB)*.XBM"
 @ COPY_/NOLOG/NOCONF  *.XBM $(GV_LIB)*.XBM;
.endif
 @ write_ sys$output "  copying GV_SPARTAN.DAT    -> $(GV_LIB)GV_SPARTAN.DAT"
 @ COPY_/NOLOG/NOCONF GV_SPARTAN.DAT $(GV_LIB)GV_SPARTAN.DAT;
 @ write_ sys$output "  copying $(GV_CLASS_DAT)	   -> $(GV_LIB)$(GV_CLASS_DAT)"
 @ COPY_/NOLOG/NOCONF $(GV_CLASS_DAT) $(GV_LIB)$(GV_CLASS_DAT);
 @ write_ sys$output "  copying $(GV_USER_DAT)	   -> $(GV_LIB)$(GV_USER_DAT)"
 @ COPY_/NOLOG/NOCONF $(GV_USER_DAT) $(GV_LIB)$(GV_USER_DAT);

$(APP_DEFAULTS_H) : AD2C.$(EXE_EXT) $(GV_CLASS_DAT)
 @ write_ sys$output "  generating $(APP_DEFAULTS_H) ..." 
 @ ad2c__ := $X11_ROOT:[GV.SOURCE]AD2C.$(EXE_EXT)
 @ ad2c__ $(GV_CLASS_DAT) $(APP_DEFAULTS_H)

$(GV_MESSAGES_H) : AD2C.$(EXE_EXT) $(GV_CLASS_DAT)
 @ write_ sys$output "  generating $(GV_MESSAGES_H) ..." 
 @ ad2c__ := $X11_ROOT:[GV.SOURCE]AD2C.$(EXE_EXT)
 @ ad2c__ $(GV_MESSAGES_DAT) $(GV_MESSAGES_H)

$(GV_INTERN_RES_H) : AD2C.$(EXE_EXT) $(GV_INTERN_RES_DAT)
 @ write_ sys$output "  generating $(GV_INTERN_RES_H) ..." 
 @ ad2c__ := $X11_ROOT:[GV.SOURCE]AD2C.$(EXE_EXT)
 @ ad2c__ $(GV_INTERN_RES_DAT) $(GV_INTERN_RES_H)

$(GV_USER_DAT) : $(GV_MAKE_RES_DAT) $(GV_INTERN_RES_DAT)
 @ write_ sys$output "  generating $(GV_USER_DAT) ..." 
 @ COPY_/NOLOG $(GV_DUMMY_DAT)	$(GV_USER_DAT);
 @ OPEN_/APPEND file $(GV_USER_DAT);
 @ WRITE_ file "!"
 @ WRITE_ file "!  gv_user.dat"
 @ WRITE_ file "!  User specific application defaults for gv."
 @ WRITE_ file "!  Copyright (C) 1995-1997  Johannes Plass"
 @ WRITE_ file "!"
 @ WRITE_ file ""
 @ CLOSE_ file
 @ APPEND_ $(GV_USER_RES_DAT)		$(GV_USER_DAT);
 @ APPEND_ $(GV_INTERN_RES_DAT)		$(GV_USER_DAT);
 @ APPEND_ $(GV_MAKE_RES_DAT)		$(GV_USER_DAT);


$(GV_CLASS_DAT) : $(GV_FONT_RES_DAT) $(GV_LAYOUT_RES_DAT)\
		 $(GV_MAKE_RES_DAT) $(GV_MISC_RES_DAT)   $(GV_USER_RES_DAT)
 @ write_ sys$output "  generating $(GV_CLASS_DAT) ..." 
 @ COPY_/NOLOG $(GV_COPYRIGHT_DAT)		$(GV_CLASS_DAT)
 @ OPEN_/APPEND file $(GV_CLASS_DAT);
 @ WRITE_ file "!"
 @ WRITE_ file "!  gv_class.dat"
 @ WRITE_ file "!  Application class defaults for gv."
 @ WRITE_ file "!  Copyright (C) 1995-1997  Johannes Plass"
 @ WRITE_ file "!"
 @ WRITE_ file ""
 @ CLOSE_ file
 @ APPEND_ $(GV_USER_RES_DAT)		$(GV_CLASS_DAT);
 @ APPEND_ $(GV_MAKE_RES_DAT)		$(GV_CLASS_DAT);
 @ APPEND_ $(GV_FONT_RES_DAT)		$(GV_CLASS_DAT);
 @ APPEND_ $(GV_MISC_RES_DAT)		$(GV_CLASS_DAT);
 @ APPEND_ $(GV_LAYOUT_RES_DAT)		$(GV_CLASS_DAT);


$(GV_MAKE_RES_DAT) : $(GV_CONFIG_FILE) $(GV_SOURCE_IMAKEFILE)
 @ write_ sys$output "  generating $(GV_MAKE_RES_DAT) ..." 
 @ COPY_/NOLOG $(GV_DUMMY_DAT)	$(GV_MAKE_RES_DAT);
 @ OPEN_/APPEND file $(GV_MAKE_RES_DAT);
 @ WRITE_ file ""
 @ WRITE_  file "!########## gv_make_res.dat (generated by makefile)"
 @ WRITE_ file ""
 @ WRITE_ file "GV.scratchDir:		$(SCRATCH_DIR)"
 @ WRITE_ file "GV.defaultSaveDir:	$(SAVE_DIR)"
 @ WRITE_ file ""
.ifdef USE_FALLBACK_FILES
 @ IF_NOT_FF = "!"
.else
 @ IF_NOT_FF = ""
.endif
 @ WRITE_ file "''IF_NOT_FF'GV.documentBitmap:	$(DOC_BMP)"
 @ WRITE_ file "''IF_NOT_FF'GV.iconPixmap:		$(ICON_BMP)"
 @ WRITE_ file "''IF_NOT_FF'GV.selectedBitmap:	$(SELECTED_BMP)"
 @ WRITE_ file "''IF_NOT_FF'GV.messageFile:		$(MESSAGES_DAT)"
 @ WRITE_ file ""
 @ CLOSE_ file


.ifdef PRODUCE_PS_LEVEL_ONE
PS_LEVEL = ""-dPSLevel1""
.endif

$(GV_INTERN_RES_DAT) : $(GV_CONFIG_FILE) $(GV_SOURCE_IMAKEFILE)
 @ write_ sys$output "  generating $(GV_INTERN_RES_DAT) ..." 
 @ COPY_/NOLOG $(GV_DUMMY_DAT)	$(GV_INTERN_RES_DAT);
 @ OPEN_/APPEND file $(GV_INTERN_RES_DAT);
 @ WRITE_ file ""
 @ WRITE_ file "!########## gv_intern_res.dat (generated by makefile)"
 @ WRITE_ file ""
 @ WRITE_ file "GVintern.gsInterpreter:	gs"
 @ WRITE_ file "GVintern.gsCmdScanPDF:	gs ""-dNODISPLAY"" ""-dQUIET"" ""-sPDFname""=%s ""-sDSCname""=%s pdf2dsc.ps -c quit"
 @ WRITE_ file "GVintern.gsCmdConvPDF:	gs ""-dNODISPLAY"" ""-dQUIET"" $(PS_LEVEL) ""-dNOPAUSE"" ""-sPSFile""=%s %s -c quit"
 @ WRITE_ file "GVintern.gsX11Device:	""-sDEVICE=x11"""
 @ WRITE_ file "GVintern.gsX11AlphaDevice:""-dNOPLATFONTS"" ""-sDEVICE=x11alpha"""
 @ WRITE_ file "GVintern.gsSafer:	True"
 @ WRITE_ file "GVintern.gsQuiet:	True"
 @ WRITE_ file "GVintern.gsArguments:"
 @ WRITE_ file "GVintern.printCommand:	$(PRINT_COMMAND)"
 @ CLOSE_ file

AD2C.$(EXE_EXT) :
 @ write_ sys$output "  compiling: AD2C.C		   -> AD2C.$(OBJ_EXT)" 
 @ CC/NODEB AD2C.C/OBJ=AD2C.$(OBJ_EXT)
 @ write_ sys$output "  linking AD2C.$(EXE_EXT) ..."
 @ LINK/NOTRACE/NOMAP AD2C.$(OBJ_EXT)/EXE=AD2C.$(EXE_EXT)
 @ write_ sys$output "  AD2C.$(EXE_EXT) available."

main.$(obj_ext) :  $(GV_INTERN_RES_H)

.ifdef USE_FALLBACK_FILES
main.$(obj_ext) :  $(GV_MESSAGES_H)
.endif

.ifdef USE_FALLBACK_RESOURCES
main.$(obj_ext) :  $(APP_DEFAULTS_H)
.endif
