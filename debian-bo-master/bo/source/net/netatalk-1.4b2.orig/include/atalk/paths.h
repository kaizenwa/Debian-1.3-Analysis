/*
 * papd paths
 */
#define _PATH_PAPDPRINTCAP	"/etc/printcap"
#ifdef ultrix
#define _PATH_PAPDSPOOLDIR	"/usr/spool/lpd"
#else ultrix
#define _PATH_PAPDSPOOLDIR	"/var/spool/lpd"
#endif ultrix
#ifdef BSD4_4
#define _PATH_DEVPRINTER	"/var/run/printer"
#else BSD4_4
#define _PATH_DEVPRINTER	"/dev/printer"
#endif BSD4_4

/*
 * atalkd paths
 */
#define _PATH_ATALKDEBUG	"/tmp/atalkd.debug"
#define _PATH_ATALKDTMP		"atalkd.tmp"

/*
 * psorder paths
 */
#define _PATH_TMPPAGEORDER	"/tmp/psorderXXXXXX"

/*
 * afpd paths
 */
#define _PATH_AFPTKT		"/tmp/AFPtktXXXXXX"
