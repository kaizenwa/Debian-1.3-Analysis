$ ! File vms-devs.mak created by makevms.tcl Thu Apr 13 10:53:49 PDT 1995
$ ! Derived from gs.mak jpeg.mak devs.mak
$ !
$ !
$ GSLIB_DEV:
$   LIB1 = "gsbitops.obj gschar.obj gscolor.obj gscoord.obj"
$   LIB2 = "gsdevice.obj gsdparam.obj gsfont.obj gsht.obj gshtscr.obj"
$   LIB3 = "gsimage.obj gsimage1.obj gsimage2.obj gsimage3.obj"
$   LIB4 = "gsimpath.obj gsiodev.obj gsiscale.obj"
$   LIB5 = "gsline.obj gsmatrix.obj gsmemory.obj gsmisc.obj"
$   LIB6 = "gspaint.obj gspath.obj gsstate.obj"
$   LIB7 = "gsutil.obj gxacpath.obj gxccache.obj gxccman.obj"
$   LIB8 = "gxcht.obj gxclist.obj gxclread.obj gxcmap.obj gxcpath.obj"
$   LIB9 = "gxdcconv.obj gxdither.obj gxdraw.obj gxfill.obj"
$   LIB10 = "gxht.obj gxpath.obj gxpath2.obj gxpcopy.obj gxstroke.obj"
$   LIB11 = "gdevabuf.obj gdevmem.obj gdevm1.obj gdevm2.obj gdevm4.obj"
$   LIB12 = "gdevm8.obj gdevm16.obj gdevm24.obj gdevm32.obj gdevmpla.obj"
$   ECHOGS -w gslib.dev
$   ECHOGS -a gslib.dev 'LIB1'
$   ECHOGS -a gslib.dev 'LIB2'
$   ECHOGS -a gslib.dev 'LIB3'
$   ECHOGS -a gslib.dev 'LIB4'
$   ECHOGS -a gslib.dev 'LIB5'
$   ECHOGS -a gslib.dev 'LIB6'
$   ECHOGS -a gslib.dev 'LIB7'
$   ECHOGS -a gslib.dev 'LIB8'
$   ECHOGS -a gslib.dev 'LIB9'
$   ECHOGS -a gslib.dev 'LIB10'
$   ECHOGS -a gslib.dev 'LIB11'
$   ECHOGS -a gslib.dev 'LIB12'
$   RETURN
$ !
$ GS_DEV:
$   INT1 = "ialloc.obj idebug.obj idict.obj idparam.obj"
$   INT2 = "igc.obj igcref.obj igcstr.obj iinit.obj"
$   INT3 = "ilocate.obj iname.obj interp.obj iparam.obj isave.obj"
$   INT4 = "iscan.obj iscannum.obj iscantab.obj istack.obj iutil.obj"
$   INT5 = "sfile.obj sfilter1.obj sstring.obj stream.obj"
$   Z1 = "zarith.obj zarray.obj zcontrol.obj zdict.obj"
$   Z1OPS = "zarith zarray zcontrol zdict"
$   Z2 = "zfile.obj zfileio.obj zfilter.obj zfname.obj zfproc.obj"
$   Z2OPS = "zfile zfileio zfilter zfproc"
$   Z3 = "zgeneric.obj ziodev.obj zmath.obj zmisc.obj zpacked.obj"
$   Z3OPS = "zgeneric ziodev zmath zmisc zpacked"
$   Z4 = "zrelbit.obj zstack.obj zstring.obj zsysvm.obj"
$   Z4OPS = "zrelbit zstack zstring zsysvm"
$   Z5 = "ztoken.obj ztype.obj zvmem.obj"
$   Z5OPS = "ztoken ztype zvmem"
$   Z6 = "zchar.obj zcolor.obj zdevice.obj zfont.obj zfont2.obj"
$   Z6OPS = "zchar zcolor zdevice zfont zfont2"
$   Z7 = "zgstate.obj zht.obj zmatrix.obj zpaint.obj zpath.obj"
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
$ LEVEL1_DEV:
$   GOSUB BCP_DEV
$   GOSUB HSB_DEV
$   GOSUB PATH1_DEV
$   GOSUB TYPE1_DEV
$   SETMOD level1 -include bcp hsb path1 type1
$   ADDMOD level1 -emulator PostScript PostScriptLevel1
$   RETURN
$ !
$ PATH1_DEV:
$   path1_ = "gspath1.obj zpath1.obj"
$   SETMOD path1 'path1_'
$   ADDMOD path1 -oper zpath1
$   RETURN
$ !
$ HSB_DEV:
$   hsb_ = "gshsb.obj zhsb.obj"
$   SETMOD hsb 'hsb_'
$   ADDMOD hsb -oper zhsb
$   RETURN
$ !
$ WRITEPPM_DEV:
$   writeppm_ = "zwppm.obj"
$   SETMOD writeppm 'writeppm_'
$   ADDMOD writeppm -oper zwppm
$   RETURN
$ !
$ BCP_DEV:
$   bcp_ = "sbcp.obj zfbcp.obj"
$   SETMOD bcp 'bcp_'
$   ADDMOD bcp -oper zfbcp
$   RETURN
$ !
$ TYPE1_DEV:
$   GOSUB PSF1CORE_DEV
$   GOSUB PSF1READ_DEV
$   SETMOD type1 -include psf1core psf1read
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
$ COLOR_DEV:
$   GOSUB CMYKCORE_DEV
$   GOSUB CMYKREAD_DEV
$   SETMOD color -include cmykcore cmykread
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
$ BTOKEN_DEV:
$   btoken_ = "iscanbin.obj zbseq.obj"
$   SETMOD btoken 'btoken_'
$   ADDMOD btoken -oper zbseq_l2
$   ADDMOD btoken -ps gs_btokn
$   RETURN
$ !
$ UPATH_DEV:
$   upath_ = "zupath.obj ibnum.obj"
$   SETMOD upath 'upath_'
$   ADDMOD upath -oper zupath_l2
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
$ DPS_DEV:
$   dps_ = ""
$   GOSUB DPSAND2_DEV
$   SETMOD dps -include dpsand2
$   ADDMOD dps -obj 'dps_'
$   RETURN
$ !
$ COMPFONT_DEV:
$   GOSUB PSF0CORE_DEV
$   GOSUB PSF0READ_DEV
$   SETMOD compfont -include psf0core psf0read
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
$ CIDFONT_DEV:
$   GOSUB COMPFONT_DEV
$   SETMOD cidfont -include compfont
$   ADDMOD cidfont -ps gs_cidfn
$   RETURN
$ !
$ CIE_DEV:
$   ciecore_ = "gscie.obj"
$   cieread_ = "zcie.obj"
$   cie_ = "''ciecore_' ''cieread_'"
$   SETMOD cie 'cie_'
$   ADDMOD cie -oper zcie_l2
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
$   sepr_ = "''seprcore_' ''seprread_'"
$   SETMOD sepr 'sepr_'
$   ADDMOD sepr -oper zcssepr_l2
$   RETURN
$ !
$ LEV2MIN_DEV:
$   GOSUB CIE_DEV
$   GOSUB COMPFONT_DEV
$   GOSUB DCTD_DEV
$   GOSUB DEVCTRL_DEV
$   GOSUB COLOR_DEV
$   GOSUB DPS2CORE_DEV
$   GOSUB DPS2READ_DEV
$   GOSUB FDECODE_DEV
$   GOSUB PATH1_DEV
$   GOSUB TYPE1_DEV
$   GOSUB PATTERN_DEV
$   GOSUB PSL2CORE_DEV
$   GOSUB PSL2READ_DEV
$   SETMOD lev2min -include cie compfont dctd devctrl color
$   ADDMOD lev2min -include dps2core dps2read fdecode path1 type1
$   ADDMOD lev2min -include pattern psl2core psl2read
$   ADDMOD lev2min -emulator PostScript PostScriptLevel1 PostScriptLevel2
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
$   ADDMOD level2 -emulator PostScript PostScriptLevel2
$   RETURN
$ !
$ PSL2CORE_DEV:
$   psl2core_ = "gscolor2.obj"
$   SETMOD psl2core 'psl2core_'
$   RETURN
$ !
$ PSL2READ1__INIT:
$   psl2read1_ = "iutil2.obj zcolor2.obj zcsindex.obj"
$   RETURN
$ !
$ PSL2READ2__INIT:
$   psl2read2_ = "zht2.obj zimage2.obj zmisc2.obj"
$   RETURN
$ !
$ PSL2READ_DEV:
$   IF F$TYPE(psl2read1_) .EQS. "" THEN GOSUB PSL2READ1__INIT
$   IF F$TYPE(psl2read2_) .EQS. "" THEN GOSUB PSL2READ2__INIT
$   SETMOD psl2read 'psl2read1_'
$   ADDMOD psl2read -obj 'psl2read2_'
$   ADDMOD psl2read -oper zmisc2
$   ADDMOD psl2read -oper zcolor2_l2 zcsindex_l2
$   ADDMOD psl2read -oper zht2_l2 zimage2_l2
$   ADDMOD psl2read -ps gs_lev2 gs_res
$   RETURN
$ !
$ DEVCTRL_DEV:
$   devctrl_ = "zdevice2.obj ziodev2.obj zdevcal.obj"
$   SETMOD devctrl 'devctrl_'
$   ADDMOD devctrl -oper zdevice2_l2 ziodev2_l2
$   ADDMOD devctrl -iodev null ram calendar
$   ADDMOD devctrl -ps gs_setpd
$   RETURN
$ !
$ SCFD__INIT:
$   scfd_ = "scfd.obj scfdtab.obj scftab.obj sbits.obj"
$   RETURN
$ !
$ SLZWD__INIT:
$   slzwd_ = "slzwd.obj slzwc.obj"
$   RETURN
$ !
$ FD2__INIT:
$   fd2_ = "sfilter2.obj zfdecode.obj"
$   RETURN
$ !
$ FDECODE_DEV:
$   IF F$TYPE(fd2_) .EQS. "" THEN GOSUB FD2__INIT
$   IF F$TYPE(scfd_) .EQS. "" THEN GOSUB SCFD__INIT
$   IF F$TYPE(slzwd_) .EQS. "" THEN GOSUB SLZWD__INIT
$   SETMOD fdecode 'fd2_'
$   ADDMOD fdecode 'scfd_'
$   ADDMOD fdecode 'slzwd_'
$   ADDMOD fdecode -oper zfdecode
$   RETURN
$ !
$ SCFE__INIT:
$   scfe_ = "scfe.obj scfetab.obj scftab.obj shc.obj sbits.obj"
$   RETURN
$ !
$ SLZWE__INIT:
$   slzwe_ = "slzwe.obj slzwc.obj"
$   RETURN
$ !
$ FILTER_DEV:
$   IF F$TYPE(scfe_) .EQS. "" THEN GOSUB SCFE__INIT
$   IF F$TYPE(slzwe_) .EQS. "" THEN GOSUB SLZWE__INIT
$   xfilter_ = "sbhc.obj sbwbs.obj shcgen.obj"
$   GOSUB FDECODE_DEV
$   SETMOD filter -include fdecode
$   ADDMOD filter -obj zfilter2.obj
$   ADDMOD filter -obj 'scfe_' 'slzwe_'
$   ADDMOD filter -obj 'xfilter_'
$   ADDMOD filter -oper zfilter2
$   RETURN
$ !
$ SPDIFF_DEV:
$   spdiff_ = "spdiff.obj zfpdiff.obj"
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
$   ADDMOD pdf -emulator PDF
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
$ DCT_DEV:
$   GOSUB DCTE_DEV
$   GOSUB DCTD_DEV
$   SETMOD dct -include dcte dctd
$   RETURN
$ !
$ DCTC__INIT:
$   dctc_ = "sdctc.obj sjpegc.obj zfdctc.obj"
$   RETURN
$ !
$ DCTE_DEV:
$   IF F$TYPE(dctc_) .EQS. "" THEN GOSUB DCTC__INIT
$   dcte_ = "''dctc_' sdcte.obj sjpege.obj zfdcte.obj"
$   GOSUB JPEGE_DEV
$   SETMOD dcte -include jpege
$   ADDMOD dcte -obj 'dcte_'
$   ADDMOD dcte -oper zfdcte
$   RETURN
$ !
$ DCTD_DEV:
$   IF F$TYPE(dctc_) .EQS. "" THEN GOSUB DCTC__INIT
$   dctd_ = "''dctc_' sdctd.obj sjpegd.obj zfdctd.obj"
$   GOSUB JPEGD_DEV
$   SETMOD dctd -include jpegd
$   ADDMOD dctd -obj 'dctd_'
$   ADDMOD dctd -oper zfdctd
$   RETURN
$ !
$ CCFONTS_DEV:
$   ccfonts1_ = "pagk.obj pagko.obj pagd.obj pagdo.obj"
$   ccfonts2_ = "pbkl.obj pbkli.obj pbkd.obj pbkdi.obj"
$   ccfonts3_ = "bchr.obj bchri.obj bchb.obj bchbi.obj"
$   ccfonts4_ = "ncrr.obj ncri.obj ncrb.obj ncrbi.obj"
$   ccfonts5_ = "phvr.obj phvro.obj phvb.obj phvbo.obj phvrrn.obj"
$   ccfonts6_ = "pncr.obj pncri.obj pncb.obj pncbi.obj"
$   ccfonts7_ = "pplr.obj pplri.obj pplb.obj pplbi.obj"
$   ccfonts8_ = "psyr.obj ptmr.obj ptmri.obj ptmb.obj ptmbi.obj"
$   ccfonts9_ = "zcr.obj zcro.obj zcb.obj pzdr.obj"
$   ccfonts10_ = ""
$   ccfonts11_ = ""
$   ccfonts12_ = ""
$   ccfonts13_ = ""
$   ccfonts14_ = ""
$   ccfonts15_ = ""
$   ccfonts_ps = "gs_ccfnt"
$   GOSUB TYPE1_DEV
$   SETMOD ccfonts -include type1
$   ADDMOD ccfonts -obj iccfont.obj
$   ADDMOD ccfonts -obj 'ccfonts1_'
$   ADDMOD ccfonts -obj 'ccfonts2_'
$   ADDMOD ccfonts -obj 'ccfonts3_'
$   ADDMOD ccfonts -obj 'ccfonts4_'
$   ADDMOD ccfonts -obj 'ccfonts5_'
$   ADDMOD ccfonts -obj 'ccfonts6_'
$   ADDMOD ccfonts -obj 'ccfonts7_'
$   ADDMOD ccfonts -obj 'ccfonts8_'
$   ADDMOD ccfonts -obj 'ccfonts9_'
$   ADDMOD ccfonts -obj 'ccfonts10_'
$   ADDMOD ccfonts -obj 'ccfonts11_'
$   ADDMOD ccfonts -obj 'ccfonts12_'
$   ADDMOD ccfonts -obj 'ccfonts13_'
$   ADDMOD ccfonts -obj 'ccfonts14_'
$   ADDMOD ccfonts -obj 'ccfonts15_'
$   ADDMOD ccfonts -oper ccfonts
$   ADDMOD ccfonts -ps 'ccfonts_ps'
$   RETURN
$ !
$ GCONFIGF_H:
$   ccfonts1 = "agk agko agd agdo"
$   ccfonts2 = "bkl bkli bkd bkdi"
$   ccfonts3 = "chr chri chb chbi"
$   ccfonts4 = "crr cri crb crbi"
$   ccfonts5 = "hvr hvro hvb hvbo hvrrn"
$   ccfonts6 = "ncr ncri ncb ncbi"
$   ccfonts7 = "plr plri plb plbi"
$   ccfonts8 = "syr tmr tmri tmb tmbi"
$   ccfonts9 = "zcr zcro zcb zdr"
$   ccfonts10 = ""
$   ccfonts11 = ""
$   ccfonts12 = ""
$   ccfonts13 = ""
$   ccfonts14 = ""
$   ccfonts15 = ""
$   SETMOD ccfonts_ -font 'ccfonts1'
$   ADDMOD ccfonts_ -font 'ccfonts2'
$   ADDMOD ccfonts_ -font 'ccfonts3'
$   ADDMOD ccfonts_ -font 'ccfonts4'
$   ADDMOD ccfonts_ -font 'ccfonts5'
$   ADDMOD ccfonts_ -font 'ccfonts6'
$   ADDMOD ccfonts_ -font 'ccfonts7'
$   ADDMOD ccfonts_ -font 'ccfonts8'
$   ADDMOD ccfonts_ -font 'ccfonts9'
$   ADDMOD ccfonts_ -font 'ccfonts10'
$   ADDMOD ccfonts_ -font 'ccfonts11'
$   ADDMOD ccfonts_ -font 'ccfonts12'
$   ADDMOD ccfonts_ -font 'ccfonts13'
$   ADDMOD ccfonts_ -font 'ccfonts14'
$   ADDMOD ccfonts_ -font 'ccfonts15'
$   GENCONF ccfonts_.dev -f gconfigf.h
$   RETURN
$ !
$ CCINIT_DEV:
$   SETMOD ccinit iccinit.obj gs_init.obj
$   ADDMOD ccinit -oper ccinit
$   RETURN
$ !
$ JPEGC_DEV:
$   jpegc_ = "jcomapi.obj jutils.obj sjpegerr.obj jmemmgr.obj"
$   SETMOD jpegc 'jpegc_'
$   RETURN
$ !
$ JPEGE_DEV:
$   jpege_1 = "jcapi.obj jccoefct.obj jccolor.obj jcdctmgr.obj"
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
$ EGAVGA_INIT:
$   IF F$TYPE(PCFBASM) .EQS. "" THEN GOSUB PCFBASM_INIT
$   EGAVGA = "gdevevga.obj gdevpcfb.obj gdevpccm.obj ''PCFBASM'"
$   RETURN
$ !
$ EGA_DEV:
$   IF F$TYPE(EGAVGA) .EQS. "" THEN GOSUB EGAVGA_INIT
$   SETDEV ega 'EGAVGA'
$   RETURN
$ !
$ VGA_DEV:
$   IF F$TYPE(EGAVGA) .EQS. "" THEN GOSUB EGAVGA_INIT
$   SETDEV vga 'EGAVGA'
$   RETURN
$ !
$ SVGA16_DEV:
$   IF F$TYPE(EGAVGA) .EQS. "" THEN GOSUB EGAVGA_INIT
$   SETDEV svga16 'EGAVGA'
$   RETURN
$ !
$ SVGA_INIT:
$   IF F$TYPE(PCFBASM) .EQS. "" THEN GOSUB PCFBASM_INIT
$   SVGA = "gdevsvga.obj gdevpccm.obj ''PCFBASM'"
$   RETURN
$ !
$ ATIW_DEV:
$   IF F$TYPE(SVGA) .EQS. "" THEN GOSUB SVGA_INIT
$   SETDEV atiw 'SVGA'
$   RETURN
$ !
$ TSENG_DEV:
$   IF F$TYPE(SVGA) .EQS. "" THEN GOSUB SVGA_INIT
$   SETDEV tseng 'SVGA'
$   RETURN
$ !
$ TVGA_DEV:
$   IF F$TYPE(SVGA) .EQS. "" THEN GOSUB SVGA_INIT
$   SETDEV tvga 'SVGA'
$   RETURN
$ !
$ VESA_DEV:
$   IF F$TYPE(SVGA) .EQS. "" THEN GOSUB SVGA_INIT
$   SETDEV vesa 'SVGA'
$   RETURN
$ !
$ S3VGA_DEV:
$   IF F$TYPE(SVGA) .EQS. "" THEN GOSUB SVGA_INIT
$   s3vga_ = "''SVGA' gdevs3ga.obj gdevsvga.obj gdevpccm.obj"
$   SETDEV s3vga 's3vga_'
$   RETURN
$ !
$ BGI_DEV:
$   bgi_ = "gdevbgi.obj cgaf.obj"
$   SETDEV bgi 'bgi_'
$   ADDMOD bgi -lib {$(LIBDIR)graphics}
$   RETURN
$ !
$ HERC_DEV:
$   herc_ = "gdevherc.obj"
$   SETDEV herc 'herc_'
$   RETURN
$ !
$ PE_DEV:
$   pe_ = "gdevpe.obj"
$   SETDEV pe 'pe_'
$   RETURN
$ !
$ MSWIN_DEV:
$   mswin_ = "gdevmswn.obj gdevmsxf.obj gdevwdib.obj gdevemap.obj gdevpccm.obj"
$   SETDEV mswin 'mswin_'
$   RETURN
$ !
$ MSWINDLL_DEV:
$   mswindll_ = "gdevmswn.obj gdevmsxf.obj gdevwdib.obj gdevemap.obj gdevpccm.obj"
$   SETDEV mswindll 'mswindll_'
$   RETURN
$ !
$ MSWINPRN_DEV:
$   mswinprn_ = "gdevwprn.obj gdevmsxf.obj"
$   SETDEV mswinprn 'mswinprn_'
$   RETURN
$ !
$ OS2PM_DEV:
$   os2pm_ = "gdevpm.obj gdevpccm.obj"
$   SETDEV os2pm 'os2pm_'
$   RETURN
$ !
$ OS2DLL_DEV:
$   os2dll_ = "gdevpm.obj gdevpccm.obj"
$   SETDEV os2dll 'os2dll_'
$   RETURN
$ !
$ ATT3B1_DEV:
$   att3b1_ = "gdev3b1.obj"
$   SETDEV att3b1 'att3b1_'
$   RETURN
$ !
$ SXLCRT_DEV:
$   sxlcrt_ = "gdevln03.obj gdevprn.obj"
$   SETDEV sxlcrt 'sxlcrt_'
$   RETURN
$ !
$ APPLEDMP__INIT:
$   appledmp_ = "gdevadmp.obj gdevprn.obj"
$   RETURN
$ !
$ APPLEDMP_DEV:
$   IF F$TYPE(appledmp_) .EQS. "" THEN GOSUB APPLEDMP__INIT
$   SETDEV appledmp 'appledmp_'
$   RETURN
$ !
$ IWHI_DEV:
$   IF F$TYPE(appledmp_) .EQS. "" THEN GOSUB APPLEDMP__INIT
$   SETDEV iwhi 'appledmp_'
$   RETURN
$ !
$ IWLO_DEV:
$   IF F$TYPE(appledmp_) .EQS. "" THEN GOSUB APPLEDMP__INIT
$   SETDEV iwlo 'appledmp_'
$   RETURN
$ !
$ IWLQ_DEV:
$   IF F$TYPE(appledmp_) .EQS. "" THEN GOSUB APPLEDMP__INIT
$   SETDEV iwlq 'appledmp_'
$   RETURN
$ !
$ BJ10E__INIT:
$   bj10e_ = "gdevbj10.obj gdevprn.obj"
$   RETURN
$ !
$ BJ10E_DEV:
$   IF F$TYPE(bj10e_) .EQS. "" THEN GOSUB BJ10E__INIT
$   SETDEV bj10e 'bj10e_'
$   RETURN
$ !
$ BJ200_DEV:
$   IF F$TYPE(bj10e_) .EQS. "" THEN GOSUB BJ10E__INIT
$   SETDEV bj200 'bj10e_'
$   RETURN
$ !
$ HPPCL_INIT:
$   HPPCL = "gdevprn.obj gdevpcl.obj"
$   RETURN
$ !
$ HPMONO_INIT:
$   IF F$TYPE(HPPCL) .EQS. "" THEN GOSUB HPPCL_INIT
$   HPMONO = "gdevdjet.obj ''HPPCL'"
$   RETURN
$ !
$ DESKJET_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV deskjet 'HPMONO'
$   RETURN
$ !
$ DJET500_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV djet500 'HPMONO'
$   RETURN
$ !
$ LASERJET_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV laserjet 'HPMONO'
$   RETURN
$ !
$ LJETPLUS_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV ljetplus 'HPMONO'
$   RETURN
$ !
$ LJET2P_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV ljet2p 'HPMONO'
$   RETURN
$ !
$ LJET3_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV ljet3 'HPMONO'
$   RETURN
$ !
$ LJET3D_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV ljet3d 'HPMONO'
$   RETURN
$ !
$ LJET4_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV ljet4 'HPMONO'
$   RETURN
$ !
$ LP2563_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV lp2563 'HPMONO'
$   RETURN
$ !
$ OCE9050_DEV:
$   IF F$TYPE(HPMONO) .EQS. "" THEN GOSUB HPMONO_INIT
$   SETDEV oce9050 'HPMONO'
$   RETURN
$ !
$ CDESKJET__INIT:
$   IF F$TYPE(HPPCL) .EQS. "" THEN GOSUB HPPCL_INIT
$   cdeskjet_ = "gdevcdj.obj ''HPPCL'"
$   RETURN
$ !
$ CDESKJET_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV cdeskjet 'cdeskjet_'
$   RETURN
$ !
$ CDJCOLOR_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV cdjcolor 'cdeskjet_'
$   RETURN
$ !
$ CDJMONO_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV cdjmono 'cdeskjet_'
$   RETURN
$ !
$ CDJ500_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV cdj500 'cdeskjet_'
$   RETURN
$ !
$ CDJ550_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV cdj550 'cdeskjet_'
$   RETURN
$ !
$ DECLJ250_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV declj250 'cdeskjet_'
$   RETURN
$ !
$ DNJ650C_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV dnj650c 'cdeskjet_'
$   RETURN
$ !
$ LJ4DITH_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV lj4dith 'cdeskjet_'
$   RETURN
$ !
$ PJ_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV pj 'cdeskjet_'
$   RETURN
$ !
$ PJXL_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV pjxl 'cdeskjet_'
$   RETURN
$ !
$ PJXL300_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV pjxl300 'cdeskjet_'
$   RETURN
$ !
$ BJC600_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV bjc600 'cdeskjet_'
$   RETURN
$ !
$ ESCP_DEV:
$   IF F$TYPE(cdeskjet_) .EQS. "" THEN GOSUB CDESKJET__INIT
$   SETDEV escp 'cdeskjet_'
$   RETURN
$ !
$ DJET500C_DEV:
$   IF F$TYPE(HPPCL) .EQS. "" THEN GOSUB HPPCL_INIT
$   djet500c_ = "gdevdjtc.obj ''HPPCL'"
$   SETDEV djet500c 'djet500c_'
$   RETURN
$ !
$ CP50_DEV:
$   cp50_ = "gdevcp50.obj gdevprn.obj"
$   SETDEV cp50 'cp50_'
$   RETURN
$ !
$ EPSON__INIT:
$   epson_ = "gdevepsn.obj gdevprn.obj"
$   RETURN
$ !
$ EPSON_DEV:
$   IF F$TYPE(epson_) .EQS. "" THEN GOSUB EPSON__INIT
$   SETDEV epson 'epson_'
$   RETURN
$ !
$ EPS9MID_DEV:
$   IF F$TYPE(epson_) .EQS. "" THEN GOSUB EPSON__INIT
$   SETDEV eps9mid 'epson_'
$   RETURN
$ !
$ EPS9HIGH_DEV:
$   IF F$TYPE(epson_) .EQS. "" THEN GOSUB EPSON__INIT
$   SETDEV eps9high 'epson_'
$   RETURN
$ !
$ IBMPRO_DEV:
$   IF F$TYPE(epson_) .EQS. "" THEN GOSUB EPSON__INIT
$   SETDEV ibmpro 'epson_'
$   RETURN
$ !
$ EPSONC_DEV:
$   epsonc_ = "gdevepsc.obj gdevprn.obj"
$   SETDEV epsonc 'epsonc_'
$   RETURN
$ !
$ ESCP2_INIT:
$   ESCP2 = "gdevescp.obj gdevprn.obj"
$   RETURN
$ !
$ AP3250_DEV:
$   IF F$TYPE(ESCP2) .EQS. "" THEN GOSUB ESCP2_INIT
$   SETDEV ap3250 'ESCP2'
$   RETURN
$ !
$ ST800_DEV:
$   IF F$TYPE(ESCP2) .EQS. "" THEN GOSUB ESCP2_INIT
$   SETDEV st800 'ESCP2'
$   RETURN
$ !
$ STCOLOR_DEV:
$   stcolor_ = "gdevstc.obj"
$   SETDEV stcolor 'stcolor_'
$   RETURN
$ !
$ PJET_INIT:
$   IF F$TYPE(HPPCL) .EQS. "" THEN GOSUB HPPCL_INIT
$   PJET = "gdevpjet.obj ''HPPCL'"
$   RETURN
$ !
$ LJ250_DEV:
$   IF F$TYPE(PJET) .EQS. "" THEN GOSUB PJET_INIT
$   SETDEV lj250 'PJET'
$   RETURN
$ !
$ PAINTJET_DEV:
$   IF F$TYPE(PJET) .EQS. "" THEN GOSUB PJET_INIT
$   SETDEV paintjet 'PJET'
$   RETURN
$ !
$ PJETXL_DEV:
$   IF F$TYPE(PJET) .EQS. "" THEN GOSUB PJET_INIT
$   SETDEV pjetxl 'PJET'
$   RETURN
$ !
$ IMAGEN_DEV:
$   imagen_ = "gdevimgn.obj gdevprn.obj"
$   SETDEV imagen 'imagen_'
$   RETURN
$ !
$ JETP3852_DEV:
$   jetp3852_ = "gdev3852.obj gdevprn.obj"
$   SETDEV jetp3852 'jetp3852_'
$   RETURN
$ !
$ LBP8_DEV:
$   lbp8_ = "gdevlbp8.obj gdevprn.obj"
$   SETDEV lbp8 'lbp8_'
$   RETURN
$ !
$ LIPS3_DEV:
$   SETDEV lips3 'lips3_'
$   RETURN
$ !
$ LN03__INIT:
$   ln03_ = "gdevln03.obj gdevprn.obj"
$   RETURN
$ !
$ LN03_DEV:
$   IF F$TYPE(ln03_) .EQS. "" THEN GOSUB LN03__INIT
$   SETDEV ln03 'ln03_'
$   RETURN
$ !
$ LA50_DEV:
$   IF F$TYPE(ln03_) .EQS. "" THEN GOSUB LN03__INIT
$   SETDEV la50 'ln03_'
$   RETURN
$ !
$ LA70_DEV:
$   IF F$TYPE(ln03_) .EQS. "" THEN GOSUB LN03__INIT
$   SETDEV la70 'ln03_'
$   RETURN
$ !
$ LA75_DEV:
$   IF F$TYPE(ln03_) .EQS. "" THEN GOSUB LN03__INIT
$   SETDEV la75 'ln03_'
$   RETURN
$ !
$ LA75PLUS_DEV:
$   IF F$TYPE(ln03_) .EQS. "" THEN GOSUB LN03__INIT
$   SETDEV la75plus 'ln03_'
$   RETURN
$ !
$ LA70T_DEV:
$   la70t_ = "gdevla7t.obj gdevprn.obj"
$   SETDEV la70t 'la70t_'
$   RETURN
$ !
$ M8510_DEV:
$   m8510_ = "gdev8510.obj gdevprn.obj"
$   SETDEV m8510 'm8510_'
$   RETURN
$ !
$ NECP6_DEV:
$   necp6_ = "gdevnp6.obj gdevprn.obj"
$   SETDEV necp6 'necp6_'
$   RETURN
$ !
$ OKI182_DEV:
$   oki182_ = "gdevo182.obj gdevprn.obj"
$   SETDEV oki182 'oki182_'
$   RETURN
$ !
$ R4081_DEV:
$   r4081_ = "gdev4081.obj gdevprn.obj"
$   SETDEV r4081 'r4081_'
$   RETURN
$ !
$ SONYFB_DEV:
$   sonyfb_ = "gdevsnfb.obj gdevprn.obj"
$   SETDEV sonyfb 'sonyfb_'
$   RETURN
$ !
$ NWP533_DEV:
$   nwp533_ = "gdevn533.obj gdevprn.obj"
$   SETDEV nwp533 'nwp533_'
$   RETURN
$ !
$ SPARC_DEV:
$   sparc_ = "gdevsppr.obj gdevprn.obj"
$   SETDEV sparc 'sparc_'
$   RETURN
$ !
$ SJ48_DEV:
$   sj48_ = "gdevsj48.obj gdevprn.obj"
$   SETDEV sj48 'sj48_'
$   RETURN
$ !
$ SUNVIEW_DEV:
$   sunview_ = "gdevsun.obj"
$   SETDEV sunview 'sunview_'
$   ADDMOD sunview -lib suntool sunwindow pixrect
$   RETURN
$ !
$ T4693D__INIT:
$   t4693d_ = "gdev4693.obj gdevprn.obj"
$   RETURN
$ !
$ T4693D2_DEV:
$   IF F$TYPE(t4693d_) .EQS. "" THEN GOSUB T4693D__INIT
$   SETDEV t4693d2 't4693d_'
$   RETURN
$ !
$ T4693D4_DEV:
$   IF F$TYPE(t4693d_) .EQS. "" THEN GOSUB T4693D__INIT
$   SETDEV t4693d4 't4693d_'
$   RETURN
$ !
$ T4693D8_DEV:
$   IF F$TYPE(t4693d_) .EQS. "" THEN GOSUB T4693D__INIT
$   SETDEV t4693d8 't4693d_'
$   RETURN
$ !
$ TEK4696_DEV:
$   tek4696_ = "gdevtknk.obj gdevprn.obj"
$   SETDEV tek4696 'tek4696_'
$   RETURN
$ !
$ PSFAX_DEV:
$   PSFAX = "gdevpfax.obj gdevprn.obj"
$   psfax_ = "''PSFAX'"
$   SETDEV psfax 'psfax_'
$   ADDMOD psfax -iodev Fax
$   RETURN
$ !
$ DFAX__INIT:
$   dfax1_ = "gdevdfax.obj gdevtfax.obj gdevprn.obj"
$   IF F$TYPE(scfe_) .EQS. "" THEN GOSUB SCFE__INIT
$   dfax_ = "''dfax1_' ''scfe_'"
$   RETURN
$ !
$ DFAXLOW_DEV:
$   IF F$TYPE(dfax_) .EQS. "" THEN GOSUB DFAX__INIT
$   SETDEV dfaxlow 'dfax_'
$   RETURN
$ !
$ DFAXHIGH_DEV:
$   IF F$TYPE(dfax_) .EQS. "" THEN GOSUB DFAX__INIT
$   SETDEV dfaxhigh 'dfax_'
$   RETURN
$ !
$ LVGA256_DEV:
$   lvga256_ = "gdevl256.obj"
$   SETDEV lvga256 'lvga256_'
$   ADDMOD lvga256 -lib vga vgagl
$   RETURN
$ !
$ VGALIB_DEV:
$   vgalib_ = "gdevvglb.obj"
$   SETDEV vgalib 'vgalib_'
$   ADDMOD vgalib -lib vga
$   RETURN
$ !
$ X11__INIT:
$   x11_ = "gdevx.obj gdevxini.obj gdevxxf.obj gdevemap.obj"
$   RETURN
$ !
$ X11_DEV:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11__INIT
$   IF F$TYPE(XLIBS) .EQS. "" THEN GOSUB XLIBS_INIT
$   SETDEV x11 'x11_'
$   ADDMOD x11 -lib 'XLIBS'
$   RETURN
$ !
$ X11ALT__INIT:
$   IF F$TYPE(x11_) .EQS. "" THEN GOSUB X11__INIT
$   x11alt_ = "''x11_' gdevxalt.obj"
$   RETURN
$ !
$ X11ALPHA_DEV:
$   IF F$TYPE(x11alt_) .EQS. "" THEN GOSUB X11ALT__INIT
$   IF F$TYPE(XLIBS) .EQS. "" THEN GOSUB XLIBS_INIT
$   SETDEV x11alpha 'x11alt_'
$   ADDMOD x11alpha -lib 'XLIBS'
$   RETURN
$ !
$ X11CMYK_DEV:
$   IF F$TYPE(x11alt_) .EQS. "" THEN GOSUB X11ALT__INIT
$   IF F$TYPE(XLIBS) .EQS. "" THEN GOSUB XLIBS_INIT
$   SETDEV x11cmyk 'x11alt_'
$   ADDMOD x11cmyk -lib 'XLIBS'
$   RETURN
$ !
$ X11MONO_DEV:
$   IF F$TYPE(x11alt_) .EQS. "" THEN GOSUB X11ALT__INIT
$   IF F$TYPE(XLIBS) .EQS. "" THEN GOSUB XLIBS_INIT
$   SETDEV x11mono 'x11alt_'
$   ADDMOD x11mono -lib 'XLIBS'
$   RETURN
$ !
$ XES_DEV:
$   xes_ = "gdevxes.obj gdevprn.obj"
$   SETDEV xes 'xes_'
$   RETURN
$ !
$ BIT__INIT:
$   bit_ = "gdevbit.obj gdevprn.obj"
$   RETURN
$ !
$ BIT_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BIT__INIT
$   SETDEV bit 'bit_'
$   RETURN
$ !
$ BITRGB_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BIT__INIT
$   SETDEV bitrgb 'bit_'
$   RETURN
$ !
$ BITCMYK_DEV:
$   IF F$TYPE(bit_) .EQS. "" THEN GOSUB BIT__INIT
$   SETDEV bitcmyk 'bit_'
$   RETURN
$ !
$ BMP__INIT:
$   bmp_ = "gdevbmp.obj gdevpccm.obj gdevprn.obj"
$   RETURN
$ !
$ BMPMONO_DEV:
$   IF F$TYPE(bmp_) .EQS. "" THEN GOSUB BMP__INIT
$   SETDEV bmpmono 'bmp_'
$   RETURN
$ !
$ BMP16_DEV:
$   IF F$TYPE(bmp_) .EQS. "" THEN GOSUB BMP__INIT
$   SETDEV bmp16 'bmp_'
$   RETURN
$ !
$ BMP256_DEV:
$   IF F$TYPE(bmp_) .EQS. "" THEN GOSUB BMP__INIT
$   SETDEV bmp256 'bmp_'
$   RETURN
$ !
$ BMP16M_DEV:
$   IF F$TYPE(bmp_) .EQS. "" THEN GOSUB BMP__INIT
$   SETDEV bmp16m 'bmp_'
$   RETURN
$ !
$ CGM__INIT:
$   cgm_ = "gdevcgm.obj gdevcgml.obj"
$   RETURN
$ !
$ CGMMONO_DEV:
$   IF F$TYPE(cgm_) .EQS. "" THEN GOSUB CGM__INIT
$   SETDEV cgmmono 'cgm_'
$   RETURN
$ !
$ CGM8_DEV:
$   IF F$TYPE(cgm_) .EQS. "" THEN GOSUB CGM__INIT
$   SETDEV cgm8 'cgm_'
$   RETURN
$ !
$ CGM24_DEV:
$   IF F$TYPE(cgm_) .EQS. "" THEN GOSUB CGM__INIT
$   SETDEV cgm24 'cgm_'
$   RETURN
$ !
$ CIF_DEV:
$   cif_ = "gdevcif.obj gdevprn.obj"
$   SETDEV cif 'cif_'
$   RETURN
$ !
$ MGR_INIT:
$   MGR = "gdevmgr.obj gdevpccm.obj gdevprn.obj"
$   RETURN
$ !
$ MGRMONO_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgrmono 'MGR'
$   RETURN
$ !
$ MGRGRAY2_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgrgray2 'MGR'
$   RETURN
$ !
$ MGRGRAY4_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgrgray4 'MGR'
$   RETURN
$ !
$ MGRGRAY8_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgrgray8 'MGR'
$   RETURN
$ !
$ MGR4_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgr4 'MGR'
$   RETURN
$ !
$ MGR8_DEV:
$   IF F$TYPE(MGR) .EQS. "" THEN GOSUB MGR_INIT
$   SETDEV mgr8 'MGR'
$   RETURN
$ !
$ PCX__INIT:
$   pcx_ = "gdevpcx.obj gdevpccm.obj gdevprn.obj"
$   RETURN
$ !
$ PCXMONO_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCX__INIT
$   SETDEV pcxmono 'pcx_'
$   RETURN
$ !
$ PCXGRAY_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCX__INIT
$   SETDEV pcxgray 'pcx_'
$   RETURN
$ !
$ PCX16_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCX__INIT
$   SETDEV pcx16 'pcx_'
$   RETURN
$ !
$ PCX256_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCX__INIT
$   SETDEV pcx256 'pcx_'
$   RETURN
$ !
$ PCX24B_DEV:
$   IF F$TYPE(pcx_) .EQS. "" THEN GOSUB PCX__INIT
$   SETDEV pcx24b 'pcx_'
$   RETURN
$ !
$ PXM__INIT:
$   pxm_ = "gdevpbm.obj gdevprn.obj"
$   RETURN
$ !
$ PBM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV pbm 'pxm_'
$   RETURN
$ !
$ PBMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV pbmraw 'pxm_'
$   RETURN
$ !
$ PGM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV pgm 'pxm_'
$   RETURN
$ !
$ PGMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV pgmraw 'pxm_'
$   RETURN
$ !
$ PPM_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV ppm 'pxm_'
$   RETURN
$ !
$ PPMRAW_DEV:
$   IF F$TYPE(pxm_) .EQS. "" THEN GOSUB PXM__INIT
$   SETDEV ppmraw 'pxm_'
$   RETURN
$ !
$ PSMONO_DEV:
$   ps_ = "gdevpsim.obj gdevprn.obj"
$   SETDEV psmono 'ps_'
$   RETURN
$ !
$ SGIRGB_DEV:
$   sgirgb_ = "gdevsgi.obj gdevprn.obj"
$   SETDEV sgirgb 'sgirgb_'
$   RETURN
$ !
$ TFAX1__INIT:
$   IF F$TYPE(slzwe_) .EQS. "" THEN GOSUB SLZWE__INIT
$   tfax1_ = "gdevtfax.obj gdevprn.obj sfilter1.obj ''slzwe_'"
$   RETURN
$ !
$ TFAX_DEV:
$   IF F$TYPE(tfax1_) .EQS. "" THEN GOSUB TFAX1__INIT
$   IF F$TYPE(scfe_) .EQS. "" THEN GOSUB SCFE__INIT
$   SETMOD tfax 'tfax1_'
$   ADDMOD tfax -obj 'scfe_'
$   RETURN
$ !
$ FAXG3_DEV:
$   GOSUB TFAX_DEV
$   SETDEV faxg3 -include tfax
$   RETURN
$ !
$ FAXG32D_DEV:
$   GOSUB TFAX_DEV
$   SETDEV faxg32d -include tfax
$   RETURN
$ !
$ FAXG4_DEV:
$   GOSUB TFAX_DEV
$   SETDEV faxg4 -include tfax
$   RETURN
$ !
$ TIFFCRLE_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tiffcrle -include tfax
$   RETURN
$ !
$ TIFFG3_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tiffg3 -include tfax
$   RETURN
$ !
$ TIFFG32D_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tiffg32d -include tfax
$   RETURN
$ !
$ TIFFG4_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tiffg4 -include tfax
$   RETURN
$ !
$ TIFFLZW_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tifflzw -include tfax
$   RETURN
$ !
$ TIFFPACK_DEV:
$   GOSUB TFAX_DEV
$   SETDEV tiffpack -include tfax
$   RETURN
