/* tacacs-server.h
 *   Include file for tacacs-server.c.
 *
 * Last modified: 28-Dec-1990/al
 *
 * 28-Dec-1990  al      Add string arrays to format log info
 * 24-Dec-1990  al      Include USER_* codes here
 * 11-Nov-1990  al      First version
 *
 */

 /*
  * #module tacacs   "V0.1" *//* the compiler don't like this directive for
  * some reason
  */

#define TACACS_UAF      "TACACS_UAF"	/* user auth file for TACACS.  N.B.:
					 * this LNM *must* be defined in EXEC
					 * mode! */

#define TACACS_LOG      "SYS$COMMON:[SYSMGR]TACACS_LOG"	/* log file for TACACS
							 * connections */
#define TACACS_ERRLOG   "TACACS_ERRLOG"	/* log file for TACACS errors */
#define TACACS_PORT     49	/* default UDP port for TACACS */
#define TACACS_INBUF_SIZE       2048	/* max size of a UPD packet (?) */
#define BIG_STRING      512	/* generous buffer for strings */

/* the TA_* codes are from Greg Satz' tacacsd.c from ftp.cisco.com */

#define	TA_VERSION		0

/*
 * Operations
 */

#define	TA_QUERY		1
#define	TA_ANSWER		2
#define	TA_CHANGE		3
#define	TA_FOLLOW		4

/*
 * ANSWER responses
 */

#define	TA_A_ACCEPTED	1
#define	TA_A_REJECTED	2

/*
 * Reasons included in ANSWER
 */

#define	TA_A_NONE	0
#define	TA_A_EXPIRING	1
#define	TA_A_PASSWORD	2
#define	TA_A_DENIED	3

/*
 * TACACS packet structure and sizes
 */

struct tacacspkt
{
	unsigned char   version;
	unsigned char   type;
	unsigned short  trans;
	                variant_union
	{
		unsigned char   namelen;
		unsigned char   response;
	}               tacacspkt_namelen_overlay;

	variant_union
	{
		unsigned char   pwlen;
		unsigned char   reason;
	} tacacspkt_pwlen_overlay;
};

#define TACACS_PASSWORD_THRESHOLD 14	/* days of expiration warning */

/* user status codes used by verify_user():
 * USER_BOGUS     - no such user, bad password, etc.
 * USER_EXPIRED   - user password expired
 * USER_EXPIRING  - user password < TACACS_PASSWORD_THRESHOLD days from expiring
 * USER_OK        - user is authorized to do his thing
 */

#define USER_BOGUS      1
#define USER_EXPIRED    2
#define USER_EXPIRING   3
#define USER_OK         4

/* string arrays for the benefit of formatting log information */
char           *tacacs_responses[] = {"", "ACCEPTED", "REJECTED"};
char           *tacacs_reasons[] = {"NONE", "EXPIRING", "PASSWORD", "DENIED"};

/* end of tacacs-server.h */
