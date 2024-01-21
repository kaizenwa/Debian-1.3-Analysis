/*
 * tacacs.h - TAC access protocol support
 *
 */

#define	TA_VERSION		0
#define XTA_VERSION		0x80

/*
 * global flags in tacacs_extended
 */
#define TACACS_EXTEND		0x01	/* extended tacacs is enabled */
#define TACACS_ASKCONNECT	0x02	/* ask about connection */
#define TACACS_NOTECONNECT	0x04	/* notifiy about connections */
#define TACACS_ASKSLIP		0x08	/* ask about SLIP command */
#define TACACS_NOTESLIP		0x10	/* notify about SLIP command */
#define TACACS_ASKENA		0x20	/* ask about "enable" */
#define TACACS_NOTEENA		0x40	/* notify about enable */
#define TACACS_NOTELOGOUT	0x80	/* notify about logout/disconnect */

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

#define TA_A_NONE	0
#define	TA_A_EXPIRING	1
#define	TA_A_PASSWORD	2
#define	TA_A_DENIED	3

/*
 * TACACS packet structure and sizes
 */

typedef struct tacacstype_
{
	u_char          version;
	u_char          type;
	u_short         trans;
	u_char          namelen;
	u_char          pwlen;
}               tacacstype;

#define	TACACS_SIZE	6


/*
 * eXtended TACACS
 * Try to keep longwords longword aligned.
 */
typedef struct xtacacstype_
{
	u_char          version;/* version of protocol */
	u_char          type;	/* Type of query/response */
	u_short         trans;	/* transaction ID */
	/*---------------*/
	u_char          namelen;/* length of name */
	u_char          pwlen;	/* length of password */
	u_char          response;	/* response code */
	u_char          reason;	/* reason for response */
	/*---------------*/
	u_long          uuid;	/* user id code assigned. */
	/*---------------*/
	u_long          dhost;	/* destination host */
	/*---------------*/
	u_short         dport;	/* destination port */
	u_short         lport;	/* local line number */
	/*---------------*/
	u_long          flags;	/* misc flags */
	/*---------------*/
	u_short         accesslist;	/* access list for user */
	/* user name */
	/* password */
}               xtacacstype;

#define XTACACSSIZE 26
/*
 * "types"
 */

#define	XTA_LOGIN	1
#define	XTA_ANSWER	2
#define	XTA_CHANGE	3
#define	XTA_FOLLOW	4
#define XTA_CONNECT	5
#define XTA_ENABLE	6
#define XTA_LOGOUT	7
#define XTA_RELOAD	8
#define XTA_SLIPON	9
#define XTA_SLIPOFF    10
#define XTA_SLIPADDR   11

/*
 * ANSWER responses
 */

#define	XTA_A_ACCEPTED	1
#define	XTA_A_REJECTED	2

/*
 * reasons for rejection of request
 */

#define XTA_A_NONE	0
#define	XTA_A_EXPIRING	1
#define	XTA_A_PASSWORD	2
#define	XTA_A_DENIED	3
/*
 * The following are reasons for "logout" and "slipoff" xtacacs messages
 */
#define XTA_R_QUIT	4	/* user quit normally */
#define XTA_R_IDLE	5	/* idle timeout */
#define XTA_R_DROP	6	/* carrier dropped */
#define XTA_R_BAD	7	/* too many bad passwords */

/*
 * flags used
 */
#define XTA_F_NOENABLE	   1	/* user may not enable */
#define XTA_F_NOENAPASS	   2	/* user may enable without password */
#define XTA_F_NOSLIP	   4	/* user may not run SLIP */
#define XTA_F_SLIPPASS	   8	/* user needs ok to run SLIP */
#define XTA_F_CONNPASS	0x10	/* user needs ok to make connections */
#define XTA_F_ACCESS	0x20	/* use the provided access list number */
