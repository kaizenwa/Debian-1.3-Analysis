MANS	= man1/fcat.1 man1/fcatcmd.1 man1/fcd.1 man1/fcdcmd.1 man1/fdu.1 \
	  man1/fducmd.1 man1/ffind.1 man1/ffindcmd.1 man1/fget.1 \
	  man1/fgetcmd.1 man1/fgrab.1 man1/fgrabcmd.1 man1/fhost.1 \
	  man1/fhostcmd.1 man1/fls.1 man1/flscmd.1 man1/fmkdir.1 \
	  man1/fpro.1 man1/fprocmd.1 man1/fput.1 man1/frm.1 man1/frmcmd.1 \
	  man1/frmdir.1 man1/frmdircmd.1 man1/fspd.1 man1/fver.1 \
	  man5/fsp_prof.5

install:
	for A in ${MANS}; do ${DINSTAL} $$A ${MAN}/$$A; done

de-install:
	for A in ${MANS}; do ${RM} -f ${MAN}/$$A; done
