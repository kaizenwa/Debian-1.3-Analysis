$! LINK.COM
$! linker for VMS-fsp V2.6.5
$! 03-JAN-93 First version <S.A.Pechler@bdk.tue.nl>
$! 28-JAN-93 Modified for use with Multinet <S.A.Pechler@bdk.tue.nl>
$!
$define/nolog lnk$library sys$library:vaxcrtl.olb
$multinet = F$LENGTH(F$TRNLNM("MULTINET_ROOT","LNM$SYSTEM"))
$IF multinet .EQ. 0
$THEN
$ LINKOPT = "[.vms]ucxshare.opt"
$ELSE
$ LIB=F$SEARCH("sys$common:[multinetz.library]ucx$ipc.olb")
$ IF LIB .EQS. ""
$ THEN
$  LINKOPT = "[.vms]libshare.opt"
$ ELSE
$  LINKOPT = "[.vms]mulshare.opt"
$ ENDIF
$ENDIF
$!
$ln = "link/notrace"
$write sys$output "Linking: ''P1'.."
$goto 'P1
$!
$fspd:
$'LN'/exe=fspd.exe [.server]main,[.server]lib,[.server]host,[.server]conf,-
	   [.server]file,[.server]filecache,[.common]udp_io, -
           [.common]strdup,[.common]random,[.VMS_SRC]miscvms,-
	   [.VMS_SRC]vmsreaddir,[.VMS_SRC]convpath, 'LINKOPT'/opt
$!
$goto end
$!
$fls:
$'LN'/exe=flscmd.exe [.clients]flscmd,[.CLIENT]LIB,[.COMMON]UDP_IO,-
	   [.client]util,[.client]lock,-
           [.bsd_src]glob,[.bsd_src]cmp,[.bsd_src]ls,[.bsd_src]print, -
           [.bsd_src]util,[.VMS_SRC]getopt,[.vms_src]vmsmain,'LINKOPT'/opt
$goto end
$!
$fget:
$'LN'/exe=fgetcmd.exe [.clients]fgetcmd,[.CLIENT]LIB,[.COMMON]UDP_IO,-
		      [.client]util,[.CLIENT]LOCK,[.bsd_src]glob,-
                      [.VMS_SRC]getopt,[.VMS_SRC]miscvms,[.VMS_SRC]convpath,-
                      [.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fput:
$'LN'/exe=fput.exe [.clients]fput,[.CLIENT]LIB,[.COMMON]UDP_IO,[.CLIENT]UTIL,-
		   [.CLIENT]LOCK,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fcat:
$'LN'/exe=fcatcmd.exe [.clients]fcatcmd,[.CLIENT]LIB,[.COMMON]UDP_IO,-
		   [.CLIENT]UTIL,[.CLIENT]LOCK,[.vms_src]getopt,-
		   [.VMS_SRC]vmsmain,[.bsd_src]glob,'LINKOPT'/opt
$goto end
$!
$frm:
$'LN'/exe=frmcmd.exe [.clients]frmcmd,[.CLIENT]LIB,[.COMMON]UDP_IO,[.CLIENT]UTIL,-
		     [.CLIENT]LOCK,[.bsd_src]glob,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$frmdir:
$'LN'/exe=frmdircmd.exe [.clients]frmdircmd,[.CLIENT]LIB,[.COMMON]UDP_IO,-
		     [.CLIENT]UTIL,[.CLIENT]LOCK,[.bsd_src]glob,-
		     [.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fcd:
$'LN'/exe=fcdcmd.exe [.clients]fcdcmd,[.CLIENT]LIB,[.COMMON]UDP_IO,[.CLIENT]UTIL,-
		     [.CLIENT]LOCK,[.bsd_src]glob,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fmkdir:
$'LN'/exe=fmkdir.exe [.clients]fmkdir,[.CLIENT]LIB,[.COMMON]UDP_IO,[.CLIENT]UTIL,-
		     [.CLIENT]LOCK,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fgrab:
$'LN'/exe=fgrabcmd.exe [.clients]fgrabcmd,[.COMMON]UDP_IO,[.CLIENT]LIB,-
		     [.CLIENT]UTIL,[.CLIENT]LOCK,[.VMS_SRC]getopt,-
		     [.VMS_SRC]miscvms,[.VMS_SRC]convpath,[.VMS_SRC]vmsmain,-
		     [.bsd_src]glob,'LINKOPT'/opt
$goto end
$!
$fpro:
$'LN'/exe=fprocmd.exe [.clients]fprocmd,[.COMMON]UDP_IO,[.CLIENT]LIB,-
		      [.CLIENT]UTIL,[.CLIENT]LOCK,[.VMS_SRC]vmsmain,-
		      [.bsd_src]glob,'LINKOPT'/opt
$goto end
$!
$fver:
$'LN'/exe=fver.exe [.clients]fver,[.CLIENT]LIB,[.COMMON]UDP_IO,[.CLIENT]UTIL,-
		   [.CLIENT]LOCK,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fdu:
$'LN'/exe=fducmd.exe [.clients]fducmd,[.COMMON]UDP_IO,[.client]lib,-
		     [.client]util,[.client]lock,[.bsd_src]glob,-
		     [.vms_src]getopt,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$ffind:
$'LN'/exe=ffindcmd.exe [.clients]ffindcmd,[.COMMON]UDP_IO,[.client]lib,-
		       [.client]lock,[.client]util,[.bsd_src]glob,-
		       [.bsd_src]find,[.bsd_src]option,[.bsd_src]operator,-
		       [.bsd_src]function,[.bsd_src]fnmatch,[.vms_src]miscvms,-
		       [.vms_src]convpath,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fhost:
$'LN'/exe=fhostcmd.exe [.clients]fhostcmd,[.vms_src]getopt,[.vms_src]miscvms,-
		       [.vms_src]convpath,[.VMS_SRC]vmsmain,'LINKOPT'/opt
$goto end
$!
$fmerge:
$'LN'/exe=fmerge.exe fgetcmd_merge,fgrabcmd_merge,fcatcmd_merge,-
	  fput_merge,frmcmd_merge,flscmd_merge,fcdcmd_merge,-
          frmdircmd_merge,fprocmd_merge,fmkdir_merge,fver_merge,-
	  fducmd_merge,ffindcmd_merge,fhostcmd_merge,merge,[.CLIENT]LIB,-
	  [.COMMON]UDP_IO,[.CLIENT]UTIL,[.CLIENT]LOCK,-
	  [.bsd_src]glob,[.bsd_src]cmp,[.bsd_src]ls,[.bsd_src]print,-
          [.bsd_src]util,[.bsd_src]find,[.bsd_src]fnmatch,[.bsd_src]function,-
	  [.bsd_src]operator,[.bsd_src]option,[.VMS_SRC]getopt,-
	  [.VMS_SRC]miscvms,[.VMS_SRC]convpath,[.VMS_SRC]vmsmain,-
          'LINKOPT'/opt
$goto end
$!
$end:
$exit 
