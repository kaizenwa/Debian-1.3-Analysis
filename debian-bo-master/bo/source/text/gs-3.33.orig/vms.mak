$ ! Copyright (C) 1989, 1995 Aladdin Enterprises.
$ ! All rights reserved.
$ ! 
$ ! This file is part of GNU Ghostscript.
$ ! 
$ ! GNU Ghostscript is distributed in the hope that it will be useful, but
$ ! WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
$ ! anyone for the consequences of using it or for whether it serves any
$ ! particular purpose or works at all, unless he says so in writing.  Refer
$ ! to the GNU Ghostscript General Public License for full details.
$ ! 
$ !
$ !
$ ! VMS "makefile" for Ghostscript.
$ !
$ WSO = "WRITE SYS$OUTPUT"
$ ON ERROR THEN GOTO DONE
$ ON CONTROL_Y THEN GOTO DONE
$ JPEGC_DONE = 0
$ !
$ ! Check input parameters
$ !
$ IF P1 .NES. "" .AND. P1 .NES. "DEBUG" .AND. P1 .NES. "LINK" .AND. -
   P1 .NES. "BUILD" THEN GS_LIB_DEFAULT = P1
$ IF P2 .NES. "" .AND. P2 .NES. "DEBUG" .AND. P2 .NES. "LINK" .AND. -
   P2 .NES. "BUILD" THEN GS_LIB_DEFAULT = P2
$ !
$ IF P1 .NES. "DEBUG" .AND. P2 .NES. "DEBUG" THEN GOTO NODEBUG
$ CC_QUAL = CC_QUAL + "/DEFINE=(""DEBUG"")/NOOPTIMIZE/DEBUG"     ! US
$ !CC_QUAL = CC_QUAL + "/DEFINE=(""A4"",""DEBUG"")/NOOPTIMIZE/DEBUG" ! Europe
$ L_QUAL  = L_QUAL + "/DEBUG"
$ !
$ NODEBUG:
$ If P1 .EQS. "LINK" .OR. P2 .EQS. "LINK" THEN GOTO LINK_ONLY
$ If P1 .EQS. "BUILD" .OR. P2 .EQS. "BUILD" THEN GOTO BUILD_EXES
$ !
$ !
$ ! Compile and link genarch.c and then run it to create the arch.h header file
$ !
$ WSO "''CC_COMMAND'''CC_QUAL'/NOLIST/OBJECT=GENARCH.OBJ GENARCH.C"
$ 'CC_COMMAND''CC_QUAL'/NOLIST/OBJECT=GENARCH.OBJ GENARCH.C
$ LINK/NOMAP/EXE=GENARCH.EXE GENARCH.OBJ,RTL.OPT/OPT
$ GENARCH = "$" + F$ENVIRONMENT("DEFAULT") + "GENARCH.EXE"
$ GENARCH ARCH.H
$ DELETE GENARCH.EXE.*,GENARCH.OBJ.*
$ PURGE ARCH.H
$ !
$ !
$ ! Compile and link echogs.c; define ECHOGS as a command
$ !
$ WSO "''CC_COMMAND'''CC_QUAL'/NOLIST/OBJECT=ECHOGS.OBJ ECHOGS.C"
$ 'CC_COMMAND''CC_QUAL'/NOLIST/OBJECT=ECHOGS.OBJ ECHOGS.C
$ LINK/NOMAP/EXE=ECHOGS.EXE ECHOGS.OBJ,RTL.OPT/OPT
$ ECHOGS = "$" + F$ENVIRONMENT("DEFAULT") + "ECHOGS.EXE"
$ DELETE ECHOGS.OBJ;*
$ PURGE ECHOGS.EXE
$ !
$ !
$ ! Compile and link genconf.c; define GENCONF as a command
$ !
$ WSO "''CC_COMMAND'''CC_QUAL/NOLIST/OBJECT=GENCONF.OBJ GENCONF.C"
$ 'CC_COMMAND''CC_QUAL'/NOLIST/OBJECT=GENCONF.OBJ GENCONF.C
$ LINK/NOMAP/EXE=GENCONF.EXE GENCONF.OBJ,RTL.OPT/OPT
$ GENCONF = "$" + F$ENVIRONMENT("DEFAULT") + "GENCONF.EXE"
$ DELETE GENCONF.OBJ;*
$ PURGE GENCONF.EXE
$ !
$ !
$ ! Create GSSETDEV.COM, GSSETMOD.COM, and GSADDMOD.COM.
$ ! (These aren't distributed with the fileset because the .COM suffix
$ ! causes certain development tools to treat them incorrectly.)
$ !
$ !
$ OPEN/WRITE SETDEV GSSETDEV.COM
$   WRITE SETDEV "$ echogs -w 'p1'.dev - -dev 'p1' -obj 'p2 'p3 'p4 'p5 'p6 'p7 'p8"
$ CLOSE SETDEV
$ !
$ OPEN/WRITE SETMOD GSSETMOD.COM
$   WRITE SETMOD "$ if p2 .nes. """""
$   WRITE SETMOD "$ then"
$   WRITE SETMOD "$   echogs -w 'p1'.dev - 'p2 'p3 'p4 'p5 'p6 'p7 'p8"
$   WRITE SETMOD "$ else"
$   WRITE SETMOD "$   echogs -w 'p1'.dev"
$   WRITE SETMOD "$ endif"
$ CLOSE SETMOD
$ !
$ OPEN/WRITE ADDMOD GSADDMOD.COM
$   WRITE ADDMOD "$ if (p2 .nes. """") then echogs -a 'p1'.dev - 'p2 'p3 'p4 'p5 'p6 'p7 'p8"
$ CLOSE ADDMOD
$ !
$ !
$ ! Define SETDEV, SETMOD, ADDMOD as commands to execute GSSETDEV.COM,
$ ! GSSETMOD.COM and GSADDMOD.COM respectively.  Those three command
$ ! procedures make use of ECHOGS.EXE and the ECHOGS verb.
$ !
$ SETDEV = "@GSSETDEV.COM"
$ SETMOD = "@GSSETMOD.COM"
$ ADDMOD = "@GSADDMOD.COM"
$ !
$ !
$ ! Build GCONFIG_.H
$ !
$ ECHOGS -w gconfig_.h #define SYSTIME_H
$ !
$ ! Build GCONFIGV.H
$ !
$ ECHOGS -w gconfigv.h #define USE_ASM 0
$ ECHOGS -a gconfigv.h #define USE_FPU 1
$ !
$ ! Now generate *.dev files
$ !
$ DEV_LIST_NAMES = "FEATURE_DEVS DEVICE_DEVS DEVICE_DEVS1 DEVICE_DEVS2 DEVICE_DEVS3 DEVICE_DEVS4 DEVICE_DEVS5 DEVICE_DEVS6 DEVICE_DEVS7 DEVICE_DEVS8 DEVICE_DEVS9 DEVICE_DEVS10 DEVICE_DEVS11 DEVICE_DEVS12 DEVICE_DEVS13 DEVICE_DEVS14 DEVICE_DEVS15"
$ DEV_MODULES = " "
$ I = 0
$ DEVS_OUTER:
$   DEV_LIST = F$ELEMENT(I," ",DEV_LIST_NAMES)
$   IF DEV_LIST .EQS. " " THEN GOTO DEVS_DONE
$   I = I+1
$   IF F$TYPE('DEV_LIST') .EQS. "" THEN GOTO DEVS_OUTER
$   J = 0
$   DEVS_INNER:
$     ACTION = F$ELEMENT(J," ",'DEV_LIST')
$     IF ACTION .EQS. " " THEN GOTO DEVS_OUTER
$     J = J+1
$     ! Replace "." with "_"
$     IF F$LOCATE(".",ACTION) .NE. F$LENGTH(ACTION) THEN -
$	ACTION = F$ELEMENT(0,".",ACTION) + "_" + F$ELEMENT(1,".",ACTION)
$     GOSUB 'ACTION
$   GOTO DEVS_INNER
$ !
$ DEVS_DONE:
$ !
$ DEV_MODULES = F$EDIT(DEV_MODULES,"TRIM")
$ !
$ !
$ ! And now build gconfig.h and gconfigf.h
$ !
$ GOSUB GCONFIG_H
$ GOSUB GCONFIGF_H
$ !
$ !
$ ! Create an empty object library
$ !
$ LIBRARY/CREATE GS.OLB
$ !
$ ! NOW COMPILE AWAY!
$ !
$ OPEN/READ/ERROR=NO_MODULES MODULE_LIST MODULES.LIS
$ OPEN/WRITE OPT_FILE GS.OPT
$ OPT_LINE = "GS.OLB/LIBRARY/INCLUDE=("
$ COMMA = ""
$ !
$ DEFDIR = F$PARSE(F$ENVIRONMENT("DEFAULT"),,,"DIRECTORY","SYNTAX_ONLY")
$ COMPILE_LOOP:
$   READ/END=END_COMPILE MODULE_LIST MODULE
$   NAME = F$PARSE(MODULE,,,"NAME","SYNTAX_ONLY")
$   DIR  = F$PARSE(MODULE,,,"DIRECTORY","SYNTAX_ONLY") - DEFDIR
$   IF DIR .NES. ""
$   THEN
$     COPY 'MODULE'.C []
$     INC = "/INCLUDE_DIRECTORY=(''DEFDIR',''DIR')"
$   ELSE
$     INC = ""
$   ENDIF
$   WSO "''CC_COMMAND'''CC_QUAL'''INC'/NOLIST/OBJECT=''MODULE'.OBJ ''MODULE'.C"
$   'CC_COMMAND''CC_QUAL''INC'/NOLIST/OBJECT='NAME'.OBJ 'NAME'.C
$   LIBRARY/INSERT GS.OLB 'NAME'.OBJ
$   DELETE 'NAME'.OBJ.*
$   IF DIR .NES. "" THEN DELETE 'NAME'.C;
$   IF F$LENGTH(OPT_LINE) .GE. 70
$   THEN
$     OPT_LINE = OPT_LINE + COMMA + "-"
$     WRITE OPT_FILE OPT_LINE
$     OPT_LINE = NAME
$   ELSE
$     OPT_LINE = OPT_LINE + COMMA + NAME
$   ENDIF
$   COMMA = ","
$ GOTO COMPILE_LOOP
$ !
$ END_COMPILE:
$ !
$ ! Now compile device modules found in symbol DEV_MODULES
$ !
$ I = 0
$ COMPILE_DEV_LOOP:
$   MODULE = F$ELEMENT(I," ",DEV_MODULES)
$   IF MODULE .EQS. " " THEN GOTO END_COMPILE_DEV
$   I = I + 1
$   NAME = F$PARSE(MODULE,,,"NAME","SYNTAX_ONLY")
$   DIR  = F$PARSE(MODULE,,,"DIRECTORY","SYNTAX_ONLY") - DEFDIR
$   IF DIR .NES. ""
$   THEN
$     COPY 'MODULE'.C []
$     INC = "/INCLUDE_DIRECTORY=(''DEFDIR',''DIR')"
$   ELSE
$     INC = ""
$   ENDIF
$   WSO "''CC_COMMAND'''CC_QUAL'''INC'/NOLIST/OBJECT=''MODULE'.OBJ ''MODULE'.C"
$   'CC_COMMAND''CC_QUAL''INC'/NOLIST/OBJECT='NAME'.OBJ 'NAME'.C
$   LIBRARY/INSERT GS.OLB 'NAME'.OBJ
$   DELETE 'NAME'.OBJ.*
$   IF DIR .NES. "" THEN DELETE 'NAME'.C;
$   IF F$LENGTH(OPT_LINE) .GE. 70
$   THEN
$     OPT_LINE = OPT_LINE + COMMA + "-"
$     WRITE OPT_FILE OPT_LINE
$     OPT_LINE = NAME
$   ELSE
$     OPT_LINE = OPT_LINE + COMMA + NAME
$   ENDIF
$   COMMA = ","
$ GOTO COMPILE_DEV_LOOP
$ !
$ END_COMPILE_DEV:
$ !
$ OPT_LINE = OPT_LINE + ")"
$ WRITE OPT_FILE OPT_LINE
$ IF F$SEARCH("SYS$SHARE:DECW$XMLIBSHR12.EXE") .NES. ""
$ THEN
$   WRITE OPT_FILE "SYS$SHARE:DECW$XMLIBSHR12.EXE/SHARE"
$   WRITE OPT_FILE "SYS$SHARE:DECW$XTLIBSHRR5.EXE/SHARE"
$   WRITE OPT_FILE "SYS$SHARE:DECW$XLIBSHR.EXE/SHARE"
$ ELSE
$   WRITE OPT_FILE "SYS$SHARE:DECW$XMLIBSHR.EXE/SHARE"
$   WRITE OPT_FILE "SYS$SHARE:DECW$XTSHR.EXE/SHARE"
$   WRITE OPT_FILE "SYS$SHARE:DECW$XLIBSHR.EXE/SHARE"
$ ENDIF
$ WRITE OPT_FILE "Ident=""gs 3.33"""
$ CLOSE MODULE_LIST
$ CLOSE OPT_FILE
$ !
$ !
$ ! Is the DECwindows environment about?  Must be installed in order to
$ ! build the executable program gs.exe.
$ !
$ IF F$SEARCH("SYS$SHARE:DECW$XLIBSHR.EXE") .NES. "" THEN GOTO CHECK2
$ WSO "DECwindows user environment not installed;"
$ WSO "unable to build executable programs."
$ GOTO DONE
$ !
$ CHECK2:
$ IF F$TRNLNM("DECW$INCLUDE") .NES. "" THEN GOTO BUILD_EXES
$ WSO "You must invoke @DECW$STARTUP before using this"
$ WSO "command procedure to build the executable programs."
$ GOTO DONE
$ !
$ ! Build the executables
$ !
$ BUILD_EXES:
$ !
$ DEFINE X11 DECW$INCLUDE
$ !
$ LINK_ONLY:
$ WSO "''CC_COMMAND'''CC_QUAL'/NOLIST/OBJECT=GCONFIG.OBJ GCONFIG.C"
$ 'CC_COMMAND''CC_QUAL/NOLIST/OBJECT=GCONFIG.OBJ GCONFIG.C
$ !
$ WSO "''CC_COMMAND'''CC_QUAL'/NOLIST/OBJECT=GSMAIN.OBJ GSMAIN.C"
$ 'CC_COMMAND''CC_QUAL/NOLIST/OBJECT=GSMAIN.OBJ GSMAIN.C
$ !
$ WSO "''CC_COMMAND'''CC_QUAL'/NOLIST/OBJECT=GS.OBJ GS.C"
$ 'CC_COMMAND''CC_QUAL/NOLIST/OBJECT=GS.OBJ GS.C
$ !
$ WSO "Linking ... "
$ WSO "LINK''L_QUAL'/NOMAP/EXE=GS.EXE GS,GSMAIN,GCONFIG,GS.OPT/OPT"
$ LINK'L_QUAL/NOMAP/EXE=GS.EXE GS,GSMAIN,GCONFIG,GS.OPT/OPT
$ !
$ DELETE GSMAIN.OBJ.*,GS.OBJ.*,GCONFIG.OBJ.*
$ !
$ GOTO DONE
$ !
$ !
$ BTOKEN_DEV:
$   btoken_ = "iscanbin.obj zbseq.obj"
$   SETMOD btoken 'btoken_'
$   ADDMOD btoken -oper zbseq_l2
$   ADDMOD btoken -ps gs_btokn
$   RETURN
$ !
$ CMYKCORE_DEV:
$   cmykcore_ = "gscolor1.obj gsht1.obj"
$   SETMOD cmykcore 'cmykcore_'
$   RETURN
$ !
$ CMYKREAD_DEV:
$   cmykread_ = "zcolor1.obj zht1.obj"
$   SETMOD cmykread 'cmykread_'
$   ADDMOD cmykread -oper zcolor1 zht1
$   RETURN
$ !
$ CIE_DEV:
$   ciecore_ = "gscie.obj"
$   cieread_ = "zcie.obj"
$   cie_     = "''ciecore_' ''cieread_'"
$   SETMOD cie 'cie_'
$   ADDMOD cie -oper zcie_l2
$   RETURN
$ !
$ COLOR_DEV:
$   GOSUB CMYKCORE_DEV
$   GOSUB CMYKREAD_DEV
$   SETMOD color -include cmykcore cmykread
$   RETURN
$ !
$ UPATH_DEV:
$   upath_ = "zupath.obj ibnum.obj"
$   SETMOD upath 'upath_'
$   ADDMOD upath -oper zupath_l2
$   RETURN
$ !
$ PSF1CORE_DEV:
$   psf1core_ = "gstype1.obj gxhint1.obj gxhint2.obj gxhint3.obj"
$   SETMOD psf1core 'psf1core_'
$   RETURN
$ !
$ PSF1READ_DEV:
$   psf1read_ = "seexec.obj zchar1.obj zfont1.obj zmisc1.obj"
$   SETMOD psf1read 'psf1read_'
$   ADDMOD psf1read -oper zchar1 zfont1 zmisc1
$   ADDMOD psf1read -ps gs_type1
$   RETURN
$ !
$ BCP_DEV:
$   bcp_ = "sbcp.obj zfbcp.obj"
$   SETMOD bcp 'bcp_'
$   ADDMOD bcp -oper zfbcp
$   RETURN
$ !
$ HSB_DEV:
$   hsb_ = "gshsb.obj zhsb.obj"
$   SETMOD hsb 'hsb_'
$   ADDMOD hsb -oper zhsb
$   RETURN
$ !
$ PATH1_DEV:
$   path1_ = "gspath1.obj zpath1.obj"
$   SETMOD path1 'path1_'
$   ADDMOD path1 -oper zpath1
$   RETURN
$ !
$ TYPE1_DEV:
$   GOSUB PSF1CORE_DEV
$   GOSUB PSF1READ_DEV
$   SETMOD type1 -include psf1core psf1read
$   RETURN
$ !
$ LEVEL1_DEV:
$   GOSUB BCP_DEV
$   GOSUB HSB_DEV
$   GOSUB PATH1_DEV
$   GOSUB TYPE1_DEV
$   SETMOD level1 -include bcp hsb path1 type1
$   ADDMOD level1 -emulator """PostScript""" """PostScriptLevel1"""
$   RETURN
$ !
$ DPS2CORE_DEV:
$   dps2core_ = "gsdps1.obj"
$   SETMOD dps2core 'dps2core_'
$   RETURN
$ !
$ DPS2READ_DEV:
$   dps2read_ = "ibnum.obj zchar2.obj zdps1.obj zvmem2.obj"
$   SETMOD dps2read 'dps2read_'
$   ADDMOD dps2read -oper zvmem2
$   ADDMOD dps2read -oper igc_l2 zchar2_l2 zdps1_l2
$   ADDMOD dps2read -ps gs_dps1
$   RETURN
$ !
$ DPSAND2_DEV:
$   GOSUB BTOKEN_DEV
$   GOSUB COLOR_DEV
$   GOSUB UPATH_DEV
$   GOSUB DPS2CORE_DEV
$   GOSUB DPS2READ_DEV
$   SETMOD dpsand2 -include btoken color upath dps2core dps2read
$   RETURN
$ !
$ DPS_DEV:
$   dps_ = ""
$   GOSUB DPSAND2_DEV
$   SETMOD dps 'dps_' -include dpsand2
$   ADDMOD dps -obj 'dps_'
$   RETURN
$ !
$ PSF0CORE_DEV:
$   psf0core_ = "gschar0.obj gsfont0.obj"
$   SETMOD psf0core 'psf0core_'
$   RETURN
$ !
$ PSF0READ_DEV:
$   psf0read_ = "zchar2.obj zfont0.obj"
$   SETMOD psf0read 'psf0read_'
$   ADDMOD psf0read -oper zfont0 zchar2
$   ADDMOD psf0read -ps gs_type0
$   RETURN
$ !
$ COMPFONT_DEV:
$   GOSUB PSF0CORE_DEV
$   GOSUB PSF0READ_DEV
$   SETMOD compfont -include psf0core psf0read
$   RETURN
$ !
$ PSL2CORE_DEV:
$   psl2core_ = "gscolor2.obj"
$   SETMOD psl2core 'psl2core_'
$   RETURN
$ !
$ PSL2READ_DEV:
$   psl2read1_ = "iutil2.obj zcolor2.obj zcsindex.obj"
$   psl2read2_ = "zht2.obj zimage2.obj zmisc2.obj"
$   psl2read_  = "''psl2read1_' ''psl2read2_'"
$   SETMOD psl2read 'psl2read1_'
$   ADDMOD psl2read -obj 'psl2read2_'
$   ADDMOD psl2read -oper zmisc2
$   ADDMOD psl2read -oper zcolor2_l2 zcsindex_l2
$   ADDMOD psl2read -oper zht2_l2 zimage2_l2
$   ADDMOD psl2read -ps gs_lev2 gs_res
$   RETURN
$ !
$ DEVCTRL_DEV:
$   devctrl_ = "zdevice2.obj ziodev2.obj"
$   SETMOD devctrl 'devctrl_'
$   ADDMOD devctrl -oper zdevice2_l2 ziodev2_l2
$   ADDMOD devctrl -iodev null ram
$   ADDMOD devctrl -ps gs_setpd
$   RETURN
$ !
$ PATTERN_DEV:
$   patcore_ = "gspcolor.obj gxclip2.obj gxpcmap.obj"
$   patread_ = "zpcolor.obj"
$   pattern_ = "''patcore_' ''patread_'"
$   SETMOD pattern 'pattern_'
$   ADDMOD pattern -oper zpcolor_l2
$   RETURN
$ !
$ SEPR_DEV:
$   seprcore_ = "gscsepr.obj"
$   seprread_ = "zcssepr.obj"
$   sepr_     = "''seprcore_' ''seprread_'"
$   SETMOD sepr 'sepr_'
$   ADDMOD sepr -oper zcssepr_l2
$   RETURN
$ !
$ LEVEL2_DEV:
$   GOSUB CIE_DEV
$   GOSUB COMPFONT_DEV
$   GOSUB DCT_DEV
$   GOSUB DEVCTRL_DEV
$   GOSUB DPSAND2_DEV
$   GOSUB FILTER_DEV
$   GOSUB LEVEL1_DEV
$   GOSUB PATTERN_DEV
$   GOSUB PSL2CORE_DEV
$   GOSUB PSL2READ_DEV
$   GOSUB SEPR_DEV
$   SETMOD level2 -include cie compfont dct devctrl dpsand2 filter
$   ADDMOD level2 -include level1 pattern psl2core psl2read sepr
$   ADDMOD level2 -emulator """PostScript""" """PostScriptLevel2"""
$   RETURN
$ !
$ FDECODE_DEV:
$   scfd_    = "scfd.obj scfdtab.obj scftab.obj sbits.obj"
$   slzwd_   = "slzwd.obj slzwc.obj"
$   fd2_     = "sfilter2.obj zfdecode.obj"
$   fdecode_ = "''fd2_' ''scfd_' ''slzwd_'"
$   SETMOD fdecode 'fd2_'
$   ADDMOD fdecode 'scfd_'
$   ADDMOD fdecode 'slzwd_'
$   ADDMOD fdecode -oper zfdecode
$   RETURN
$ !
$ FILTER_DEV:
$   scfe_    = "scfe.obj scfetab.obj scftab.obj shc.obj sbits.obj"
$   slzwe_   = "slzwe.obj slzwc.obj"
$   xfilter_ = "sbhc.obj sbwbs.obj shcgen.obj"
$   filter_  = "''scfe_' ''slzwe_' ''xfilter_' zfilter2.obj"
$   GOSUB FDECODE_DEV
$   SETMOD filter -include fdecode
$   ADDMOD filter -obj zfilter2.obj
$   ADDMOD filter -obj 'scfe_'
$   ADDMOD filter -obj 'slzwe_'
$   ADDMOD filter -obj 'xfilter_'
$   ADDMOD filter -oper zfilter2
$   RETURN
$ !
$ SPDIFF_DEV:
$   spdiff_  = "spdiff.obj zfpdiff.obj"
$   SETMOD spdiff 'spdiff_'
$   ADDMOD spdiff -oper zfpdiff
$   RETURN
$ !
$ PDF_DEV:
$   GOSUB COLOR_DEV
$   GOSUB DCTD_DEV
$   GOSUB DPS2CORE_DEV
$   GOSUB DPS2READ_DEV
$   GOSUB FDECODE_DEV
$   GOSUB TYPE1_DEV
$   GOSUB PDFFONTS_DEV
$   GOSUB PSL2CORE_DEV
$   GOSUB PSL2READ_DEV
$   GOSUB PDFREAD_DEV
$   SETMOD pdf -include color dctd dps2core dps2read
$   ADDMOD pdf -include fdecode type1
$   ADDMOD pdf -include pdffonts psl2core psl2read pdfread
$   ADDMOD pdf -emulator """PDF"""
$   RETURN
$ !
$ PDFFONTS_DEV:
$   SETMOD pdffonts -ps gs_mex_e gs_mro_e gs_pdf_e gs_wan_e
$   RETURN
$ !
$ PDFREAD_DEV:
$   GOSUB SPDIFF_DEV
$   SETMOD pdfread -include spdiff
$   ADDMOD pdfread -ps gs_pdf pdf_base pdf_draw pdf_font pdf_main pdf_2ps
$   RETURN
$ !
$ JPEGC_DEV:
$   IF JPEGC_DONE THEN RETURN
$   JSRCDIR = "[.jpeg-5a]"
$   jpegc_ = "jcomapi.obj jutils.obj sjpegerr.obj jmemmgr.obj"
$   COPY GSJCONF.H JCONFIG.H
$   COPY GSJMOREC.H JMORECFG.H
$   COPY 'JSRCDIR'JMORECFG.H []JMCORIG.H
$   COPY 'JSRCDIR'JERROR.H   []JERROR.H
$   COPY 'JSRCDIR'JINCLUDE.H []JINCLUDE.H
$   COPY 'JSRCDIR'JPEGLIB.H  []JPEGLIB.H
$   COPY 'JSRCDIR'JVERSION.H []JVERSION.H
$   SETMOD JPEGC 'jpegc_'
$   JPEGC_DONE = 1
$   RETURN
$ !
$ JPEGE_DEV:
$   jpege_1 = "jcapi.obj jccoefct.obj jccolor.obj jcdctmgr.obj "
$   jpege_2 = "jchuff.obj jcmainct.obj jcmarker.obj jcmaster.obj"
$   jpege_3 = "jcparam.obj jcprepct.obj jcsample.obj jfdctint.obj"
$   GOSUB JPEGC_DEV
$   SETMOD jpege
$   ADDMOD jpege -include jpegc
$   ADDMOD jpege -obj 'jpege_1'
$   ADDMOD jpege -obj 'jpege_2'
$   ADDMOD jpege -obj 'jpege_3'
$   RETURN
$ !
$ JPEGD_DEV:
$   jpegd_1 = "jdapi.obj jdcoefct.obj jdcolor.obj"
$   jpegd_2 = "jddctmgr.obj jdhuff.obj jdmainct.obj jdmarker.obj"
$   jpegd_3 = "jdmaster.obj jdpostct.obj jdsample.obj jidctint.obj"
$   GOSUB JPEGC_DEV
$   SETMOD jpegd
$   ADDMOD jpegd -include jpegc
$   ADDMOD jpegd -obj 'jpegd_1'
$   ADDMOD jpegd -obj 'jpegd_2'
$   ADDMOD jpegd -obj 'jpegd_3'
$   RETURN
$ !
$ DCTE_DEV:
$   dcte_ = "''dctc_' sdcte.obj sjpege.obj zfdcte.obj"
$   GOSUB JPEGE_DEV
$   SETMOD dcte -include jpege
$   ADDMOD dcte -obj 'dcte_'
$   ADDMOD dcte -oper zfdcte
$   RETURN
$ !
$ DCTD_DEV:
$   dctd_ = "''dctc_' sdctd.obj sjpegd.obj zfdctd.obj"
$   GOSUB JPEGD_DEV
$   SETMOD dctd -include jpegd
$   ADDMOD dctd -obj 'dctd_'
$   ADDMOD dctd -oper zfdctd
$   RETURN
$ !
$ DCT_DEV:
$   dctc_ = "sdctc.obj sjpegc.obj zfdctc.obj"
$   GOSUB DCTE_DEV
$   GOSUB DCTD_DEV
$   SETMOD dct -include dcte dctd
$   RETURN
$ !
$ CCFONTS_DEV:
$   GOSUB TYPE1_DEV
$   SETMOD ccfonts -include type1
$   ADDMOD ccfonts -obj iccfont.obj
$   ADDMOD ccfonts -obj 'ccfonts1_
$   ADDMOD ccfonts -obj 'ccfonts2_
$   ADDMOD ccfonts -obj 'ccfonts3_
$   ADDMOD ccfonts -obj 'ccfonts4_
$   ADDMOD ccfonts -obj 'ccfonts5_
$   ADDMOD ccfonts -obj 'ccfonts6_
$   ADDMOD ccfonts -obj 'ccfonts7_
$   ADDMOD ccfonts -obj 'ccfonts8_
$   ADDMOD ccfonts -obj 'ccfonts9_
$   ADDMOD ccfonts -obj 'ccfonts10_
$   ADDMOD ccfonts -obj 'ccfonts10_
$   ADDMOD ccfonts -obj 'ccfonts11_
$   ADDMOD ccfonts -obj 'ccfonts12_
$   ADDMOD ccfonts -obj 'ccfonts13_
$   ADDMOD ccfonts -obj 'ccfonts14_
$   ADDMOD ccfonts -obj 'ccfonts15_
$   ADDMOD ccfonts -oper ccfonts
$   ADDMOD ccfonts -ps gs_ccfnt
$   RETURN
$ !
$ GCONFIGF_H:
$   SETMOD ccfonts_ -font "''ccfonts1'"
$   ADDMOD ccfonts_ -font "''ccfonts2'"
$   ADDMOD ccfonts_ -font "''ccfonts3'"
$   ADDMOD ccfonts_ -font "''ccfonts4'"
$   ADDMOD ccfonts_ -font "''ccfonts5'"
$   ADDMOD ccfonts_ -font "''ccfonts6'"
$   ADDMOD ccfonts_ -font "''ccfonts7'"
$   ADDMOD ccfonts_ -font "''ccfonts8'"
$   ADDMOD ccfonts_ -font "''ccfonts9'"
$   ADDMOD ccfonts_ -font "''ccfonts10'"
$   ADDMOD ccfonts_ -font "''ccfonts11'"
$   ADDMOD ccfonts_ -font "''ccfonts12'"
$   ADDMOD ccfonts_ -font "''ccfonts13'"
$   ADDMOD ccfonts_ -font "''ccfonts14'"
$   ADDMOD ccfonts_ -font "''ccfonts15'"
$   GENCONF ccfonts_.dev -f gconfigf.h
$   RETURN
$ !
$ CCINIT_DEV:
$   SETMOD ccinit iccinit.obj gs_init.obj
$   ADDMOD ccinit -oper ccinit
$   RETURN
$ !
$ GS_DEV:
$   INT1  = "ialloc.obj idebug.obj idict.obj idparam.obj"
$   INT2  = "igc.obj igcref.obj igcstr.obj iinit.obj"
$   INT3  = "ilocate.obj iname.obj interp.obj iparam.obj isave.obj"
$   INT4  = "iscan.obj iscannum.obj iscantab.obj istack.obj iutil.obj"
$   INT5  = "sfile.obj sfilter1.obj sstring.obj stream.obj"
$   Z1    = "zarith.obj zarray.obj zcontrol.obj zdict.obj"
$   Z1OPS = "zarith zarray zcontrol zdict"
$   Z2    = "zfile.obj zfileio.obj zfilter.obj zfname.obj zfproc.obj"
$   Z2OPS = "zfile zfileio zfilter zfproc"
$   Z3    = "zgeneric.obj ziodev.obj zmath.obj zmisc.obj zpacked.obj"
$   Z3OPS = "zgeneric ziodev zmath zmisc zpacked"
$   Z4    = "zrelbit.obj zstack.obj zstring.obj zsysvm.obj"
$   Z4OPS = "zrelbit zstack zstring zsysvm"
$   Z5    = "ztoken.obj ztype.obj zvmem.obj"
$   Z5OPS = "ztoken ztype zvmem"
$   Z6    = "zchar.obj zcolor.obj zdevice.obj zfont.obj zfont2.obj"
$   Z6OPS = "zchar zcolor zdevice zfont zfont2"
$   Z7    = "zgstate.obj zht.obj zmatrix.obj zpaint.obj zpath.obj"
$   Z7OPS = "zgstate zht zmatrix zpaint zpath"
$   SETMOD gs gsmain.obj gconfig.obj
$   ADDMOD gs -obj 'INT1'
$   ADDMOD gs -obj 'INT2'
$   ADDMOD gs -obj 'INT3'
$   ADDMOD gs -obj 'INT4'
$   ADDMOD gs -obj 'INT5'
$   ADDMOD gs -obj 'Z1'
$   ADDMOD gs -oper 'Z1OPS'
$   ADDMOD gs -obj 'Z2'
$   ADDMOD gs -oper 'Z2OPS'
$   ADDMOD gs -obj 'Z3'
$   ADDMOD gs -oper 'Z3OPS'
$   ADDMOD gs -obj 'Z4'
$   ADDMOD gs -oper 'Z4OPS'
$   ADDMOD gs -obj 'Z5'
$   ADDMOD gs -oper 'Z5OPS'
$   ADDMOD gs -obj 'Z6'
$   ADDMOD gs -oper 'Z6OPS'
$   ADDMOD gs -obj 'Z7'
$   ADDMOD gs -oper 'Z7OPS'
$   ADDMOD gs -iodev stdin stdout stderr lineedit statementedit
$   RETURN
$ !
$ DEVS_TR:
$ ! quote the dashes so that they are not interpreted as continuation
$ ! marks when the following DCL symbol is not defined!
$   ECHOGS -w devs.tr "-"  gs.dev
$   ECHOGS -a devs.tr "-" 'FEATURE_DEVS'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS1'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS2'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS3'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS4'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS5'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS6'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS7'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS8'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS9'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS10'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS11'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS12'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS13'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS14'
$   ECHOGS -a devs.tr "-" 'DEVICE_DEVS15'
$   RETURN
$ !
$ GCONFIG_H:
$   GOSUB GS_DEV
$   GOSUB DEVS_TR
$   GENCONF "@devs.tr" -h gconfig.h
$   ECHOGS -a gconfig.h "#define GS_LIB_DEFAULT ""''GS_LIB_DEFAULT'"""
$   ECHOGS -a gconfig.h "#define GS_DOCDIR ""''GS_DOCDIR'"""
$   ECHOGS -a gconfig.h "#define GS_INIT ""''GS_INIT'"""
$   RETURN
$ !  
$ X11DEV_INIT:
$   x11_  = "gdevx.obj gdevxini.obj gdevxxf.obj gdevemap.obj"
$   x11alt_ = "''x11_' gdevxalt.obj"
$   XLIBS = "Xt Xext X11"
$   RETURN
$ !
$ X11_DEV:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11DEV_INIT
$   SETDEV x11 'x11_'
$   ADDMOD x11 -lib 'XLIBS'
$   ADD_DEV_MODULES = "''x11_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ X11ALPHA_DEV:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11DEV_INIT
$   SETDEV x11alpha 'x11alt_'
$   ADDMOD x11alpha -lib 'XLIBS'
$   ADD_DEV_MODULES = "''x11alt_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ X11CMYK_DEV:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11DEV_INIT
$   SETDEV x11cmyk 'x11alt_'
$   ADDMOD x11cmyk -lib 'XLIBS'
$   ADD_DEV_MODULES = "''x11alt_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ X11MONO_DEV:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11DEV_INIT
$   SETDEV x11mono 'x11alt_'
$   ADDMOD x11mono -lib 'XLIBS'
$   ADD_DEV_MODULES = "''x11alt_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ BJDEV_INIT:
$   bj10e_ = "gdevbj10.obj gdevprn.obj"
$   RETURN
$ !
$ BJ10E_DEV:
$   IF F$TYPE(bj10e_) .EQS. "" THEN GOSUB BJDEV_INIT
$   SETDEV bj10e 'bj10e_'
$   ADD_DEV_MODULES = "''bj10e_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ BJ200_DEV:
$   IF F$TYPE(bj10e_) .EQS. "" THEN GOSUB BJDEV_INIT
$   SETDEV bj200 'bj10e_'
$   ADD_DEV_MODULES = "''bj10e_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ HPDEV_INIT:
$   HPPCL     = "gdevprn.obj gdevpcl.obj"
$   HPMONO    = "gdevdjet.obj ''HPPCL'"
$   cdeskjet_ = "gdevcdj.obj ''HPPCL'"
$   RETURN
$ !
$ DESKJET_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV deskjet 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ DJET500_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV djet500 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LASERJET_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV laserjet 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LJETPLUS_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV ljetplus 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LJET2P_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV ljet2p 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LJET3_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV ljet3 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LJET4_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV ljet4 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ LP2563_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV lp2563 'HPMONO'
$   ADD_DEV_MODULES = "''HPMONO'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ CDESKJET_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV cdeskjet 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ CDJCOLOR_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV cdjcolor 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ CDJMONO_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV cdjmono 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ CDJ550_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV cdj550 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ DECLJ250_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV declj250 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ DNJ650C_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV dnj650c 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ L4DITH_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV lj4dith 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PJ_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV pj 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PJXL_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV pjxl 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PJXL300_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB HPDEV_INIT
$   SETDEV pjxl300 'cdeskjet_'
$   ADD_DEV_MODULES = "''cdeskjet_'"
$   GOSUB ADD_DEV_MOD
$ !
$ BITDEV_INIT:
$   bit_ = "gdevbit.obj gdevprn.obj"
$   RETURN
$ !
$ BIT_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BITDEV_INIT
$   SETDEV bit 'bit_'
$   ADD_DEV_MODULES = "''bit_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ BITRGB_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BITDEV_INIT
$   SETDEV bitrgb 'bit_'
$   ADD_DEV_MODULES = "''bit_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ BITCMYK_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BITDEV_INIT
$   SETDEV bitcmyk 'bit_'
$   ADD_DEV_MODULES = "''bit_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PCXDEV_INIT:
$   pcx_ = "gdevpcx.obj gdevpccm.obj gdevprn.obj"
$   RETURN
$ !
$ PCXMONO_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCXDEV_INIT
$   SETDEV pcxmono 'pcx_'
$   ADD_DEV_MODULES = "''pcx_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PCXGRAY_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCXDEV_INIT
$   SETDEV pcxgray 'pcx_'
$   ADD_DEV_MODULES = "''pcx_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PCX16_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCXDEV_INIT
$   SETDEV pcx16 'pcx_'
$   ADD_DEV_MODULES = "''pcx_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PCX256_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCXDEV_INIT
$   SETDEV pcx256 'pcx_'
$   ADD_DEV_MODULES = "''pcx_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PCX24B_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCXDEV_INIT
$   SETDEV pcx24b 'pcx_'
$   ADD_DEV_MODULES = "''pcx_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PBMDEV_INIT:
$   pxm_ = "gdevpbm.obj gdevprn.obj"
$   RETURN
$ !
$ PBM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV pbm 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PBMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV pbmraw 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PGM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV pgm 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PGMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV pgmraw 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PPM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV ppm 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PPMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PBMDEV_INIT
$   SETDEV ppmraw 'pxm_'
$   ADD_DEV_MODULES = "''pxm_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ PSMONO_DEV:
$   ps_ = "gdevpsim.obj gdevprn.obj"
$   SETDEV psmono 'ps_'
$   ADD_DEV_MODULES = "''ps_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ FAXDEV_INIT:
$   tfax_ = "gdevtfax.obj gdevprn.obj"
$   RETURN
$ !
$ FAXG3_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV faxg3 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ FAXG32D_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV faxg32d 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ FAXG4_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV faxg4 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFCRLE_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tiffcrle 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFG3_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tiffg3 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFG32D_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tiffg32d 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFG4_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tiffg4 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFLZW_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tifflzw 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ TIFFPACK_DEV:
$   IF F$TYPE(tfax_) .EQS. "" THEN GOSUB FAXDEV_INIT
$   SETDEV tiffpack 'tfax_'
$   ADD_DEV_MODULES = "''tfax_'"
$   GOSUB ADD_DEV_MOD
$   RETURN
$ !
$ ADD_DEV_MOD:
$   II = 0
$   ADD_MORE:
$     DEV_NOW = F$EDIT(F$ELEMENT(II," ",ADD_DEV_MODULES),"UPCASE") - ".OBJ"
$     IF DEV_NOW .EQS. " " THEN RETURN
$     II = II + 1
$     ! add delimiters to avoid mistaken identities
$     ! (e.g., a search for "x11" finding "x11alpha")
$     IF F$LOCATE(" "+DEV_NOW+" ",DEV_MODULES) .NE. F$LENGTH(DEV_MODULES) THEN -
        GOTO ADD_MORE
$     DEV_MODULES = DEV_MODULES + DEV_NOW + " "
$     GOTO ADD_MORE
$ !
$ DONE:
$ !
$ DELETE *.DEV;*
$ IF F$SEARCH("DEVS.TR")       .NES. "" THEN DELETE DEVS.TR;*
$ IF F$SEARCH("ECHOGS.EXE")    .NES. "" THEN DELETE ECHOGS.EXE;*
$ IF F$SEARCH("GENCONF.EXE")   .NES. "" THEN DELETE GENCONF.EXE;*
$ IF F$LOGICAL("MODULE_LIST")  .NES. "" THEN CLOSE MODULE_LIST
$ IF F$LOGICAL("OPT_FILE")     .NES. "" THEN CLOSE OPT_FILE
$ IF F$LOGICAL("X11")          .NES. "" THEN DEASSIGN X11
$ !
$ ! ALL DONE
$ EXIT
$ !
$ NO_MODULES:
$ !
$ WSO "Error opening MODULES.LIS. Check this file!"
$ GOTO DONE
