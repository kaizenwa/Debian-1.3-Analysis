/*
 * List of predefined aliases
 */

  {(void *) 0, "binfmt-2", "binfmt_aout"},	/* m68k */
  {(void *) 0, "binfmt-0107", "binfmt_aout"},	/* m68k */
  {(void *) 0, "binfmt-204", "binfmt_aout"},
  {(void *) 0, "binfmt-263", "binfmt_aout"},
  {(void *) 0, "binfmt-264", "binfmt_aout"},
  {(void *) 0, "binfmt-267", "binfmt_aout"},
  {(void *) 0, "binfmt-387", "binfmt_aout"},
  {(void *) 0, "binfmt-332", "iBCS"},
  {(void *) 0, "binfmt-310", "binfmt_java"},

  {(void *) 0, "block-major-1", "rd"},
  {(void *) 0, "block-major-2", "floppy"},
  {(void *) 0, "block-major-3", "ide-probe"},
  {(void *) 0, "block-major-7", "loop"},
  {(void *) 0, "block-major-8", "sd_mod"},
  {(void *) 0, "block-major-11", "sr_mod"},
  {(void *) 0, "block-major-13", "xd"},
  {(void *) 0, "block-major-15", "cdu31a"},
  {(void *) 0, "block-major-16", "gscd"},
  {(void *) 0, "block-major-17", "optcd"},
  {(void *) 0, "block-major-18", "sjcd"},
  {(void *) 0, "block-major-20", "mcdx"},
  {(void *) 0, "block-major-22", "ide-probe"},
  {(void *) 0, "block-major-23", "mcd"},
  {(void *) 0, "block-major-24", "sonycd535"},
  {(void *) 0, "block-major-25", "sbpcd"},
  {(void *) 0, "block-major-26", "sbpcd"},
  {(void *) 0, "block-major-27", "sbpcd"},
  {(void *) 0, "block-major-29", "aztcd"},
  {(void *) 0, "block-major-32", "cm206"},
  {(void *) 0, "block-major-33", "ide-probe"},
  {(void *) 0, "block-major-34", "ide-probe"},

  {(void *) 0, "char-major-4", "serial"},
  {(void *) 0, "char-major-5", "serial"},
  {(void *) 0, "char-major-6", "lp"},
  {(void *) 0, "char-major-9", "st"},
  {(void *) 0, "char-major-10", "misc"},		/* was: mouse */
  {(void *) 0, "char-major-10-0", "busmouse"},	/* /dev/logibm Logitech bus mouse */
  {(void *) 0, "char-major-10-1", "psaux"},	/* /dev/psaux PS/2-style mouse port */
  {(void *) 0, "char-major-10-2", "msbusmouse"},	/* /dev/inportbm Microsoft Inport bus mouse */
  {(void *) 0, "char-major-10-3", "atixlmouse"},	/* /dev/atibm ATI XL bus mouse */
					/* /dev/jbm J-mouse */
  {(void *) 0, "char-major-10-4", "amigamouse"},	/* /dev/amigamouse Amiga mouse (68k/Amiga) */
  {(void *) 0, "char-major-10-5", "atarimouse"},	/* /dev/atarimouse Atari mouse */
					/* /dev/sunmouse Sun mouse */
					/* /dev/beep Fancy beep device */
					/* /dev/modreq Kernel module load request */
  {(void *) 0, "char-major-10-130", "wdt"},	/* /dev/watchdog Watchdog timer port */
  {(void *) 0, "char-major-10-131", "wdt"},	/* /dev/temperature Machine internal temperature */
					/* /dev/hwtrap Hardware fault trap */
					/* /dev/exttrp External device trap */
  {(void *) 0, "char-major-10-139", "openprom"},	/* /dev/openprom Linux/Sparc interface */
  {(void *) 0, "char-major-14", "sound"},
  {(void *) 0, "char-major-19", "cyclades"},
  {(void *) 0, "char-major-20", "cyclades"},
  {(void *) 0, "char-major-21", "sg"},
  {(void *) 0, "char-major-27", "ftape"},
  {(void *) 0, "char-major-34", "scc"},
  {(void *) 0, "char-major-35", "tclmidi"},
  {(void *) 0, "char-major-36", "netlink"},
  {(void *) 0, "char-major-37", "ide-tape"},
  {(void *) 0, "char-major-48", "riscom8"},
  {(void *) 0, "char-major-49", "riscom8"},
  {(void *) 0, "char-major-63", "kdebug"},

  {(void *) 0, "dos", "msdos"},
  {(void *) 0, "dummy0", "dummy"},
  {(void *) 0, "dummy1", "dummy"},
  {(void *) 0, "eth0", "off"},
  {(void *) 0, "iso9660", "isofs"},
  {(void *) 0, "md-personality-1", "linear"},
  {(void *) 0, "md-personality-2", "raid0"},
  {(void *) 0, "net-pf-3", "off"},
				/* PF_UNIX      1  Unix domain sockets */
				/* PF_INET      2  Internet IP Protocol */
				/* PF_AX25      3  Amateur Radio AX.25 */
  {(void *) 0, "net-pf-4", "ipx"},		/* PF_IPX       4  Novell IPX */
  {(void *) 0, "net-pf-5", "appletalk"},	/* PF_APPLETALK 5  Appletalk DDP */
				/* PF_NETROM    6  Amateur radio NetROM */
				/* PF_BRIDGE    7  Multiprotocol bridge */
				/* PF_AAL5      8  Reserved for Werner's ATM */
				/* PF_X25       9  Reserved for X.25 project */
				/* PF_INET6     10 IP version 6 */

  {(void *) 0, "netalias-2", "ip_alias"},
  {(void *) 0, "plip0", "plip"},
  {(void *) 0, "plip1", "plip"},
  {(void *) 0, "ppp0", "ppp"},
  {(void *) 0, "ppp1", "ppp"},
  {(void *) 0, "scsi_hostadapter", "off"},	/* if not in config file */
  {(void *) 0, "slip0", "slip"},
  {(void *) 0, "slip1", "slip"},
  {(void *) 0, "tty-ldisc-1", "slip"},
  {(void *) 0, "tty-ldisc-3", "ppp"},
