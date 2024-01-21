/* tacacs-routines.c
 *   Support routines for the tacacs server.
 *
 * Last modified: 25-Dec-1990/al
 *
 * 25-Dec-1990  al      Busy little elves
 * 24-Dec-1990  al      Yanked out of big tacacs-server.c
 *
 */

#include "includes.h"

/* init_tacacs_uaf
 *   Do some razzledazzle with logical names so that $GETUAI can find the
 *   TACACS user authorization file without interfering with the real
 *   system files.
 *
 *   When the dust settles, things will look like this:
 *   * LNM$FILE_DEV will be defined in EXEC mode in the LNM$PROCESS_DIRECTORY
 *     table as "LNM$PROCESS", "LNM$JOB", "LNM$GROUP", "LNM$SYSTEM"
 *   * SYSUAF will be defined in EXEC mode in the LNM$PROCESS table as
 *     "TACACS_UAF"
 *   * RIGHTSLIST and NETPROXY will be defined in EXEC mode in the LNM$PROCESS
 *     table as "_NLA0:"
 *
 *   All of this foolishness will be undone by the exit handler
 *   tacacs_exit_handler().
 *
 *   The logical name TACACS_UAF *must* be defined in EXEC mode.
 *
 *   Returns status from SYS$TRNLNM(), SYS$CRELNM() or stat().
 */

#define LNMPROCESS  "LNM$PROCESS"
#define LNMJOB      "LNM$JOB"
#define LNMGROUP    "LNM$GROUP"
#define LNMSYSTEM   "LNM$SYSTEM"
#define NLA0        "_NLA0:"
unsigned long int 
init_tacacs_uaf ()
{
	unsigned long int status,
	                SYS$CRELNM (),
	                SYS$TRNLNM ();
	int             stat ();

	stat_t          statbuf;
	char           *lnmprocess = LNMPROCESS;
	char           *lnmjob = LNMJOB;
	char           *lnmgroup = LNMGROUP;
	char           *lnmsystem = LNMSYSTEM;
	char           *nla0 = NLA0;
	BYTE            acmode = PSL$C_EXEC;

	$DESCRIPTOR (processtable, "LNM$PROCESS_TABLE");
	$DESCRIPTOR (processdirectory, "LNM$PROCESS_DIRECTORY");
	$DESCRIPTOR (file_dev, "LNM$FILE_DEV");
	$DESCRIPTOR (lognam, TACACS_UAF);
	$DESCRIPTOR (sysuaf, "SYSUAF");
	$DESCRIPTOR (rightslist, "RIGHTSLIST");
	$DESCRIPTOR (netproxy, "NETPROXY");
	ITEM            cre_sysuaf_itmlst[] = {
		{(WORD) sizeof (TACACS_UAF) - 1, LNM$_STRING, lognam.dsc$a_pointer, 0},
		{0, 0, 0, 0}
	};
	ITEM            cre_file_dev_itmlst[] = {
		{(WORD) sizeof (LNMPROCESS) - 1, LNM$_STRING, lnmprocess, 0},
		{(WORD) sizeof (LNMJOB) - 1, LNM$_STRING, lnmjob, 0},
		{(WORD) sizeof (LNMGROUP) - 1, LNM$_STRING, lnmgroup, 0},
		{(WORD) sizeof (LNMSYSTEM) - 1, LNM$_STRING, lnmsystem, 0},
		{0, 0, 0, 0}
	};
	ITEM            cre_nla0_itmlst[] = {
		{(WORD) sizeof (NLA0) - 1, LNM$_STRING, nla0, 0},
		{0, 0, 0, 0}
	};

/* First - Declare the exit handler. */
	void            tacacs_exit_handler ();
	EXH_BLK         exh_blk = {0, tacacs_exit_handler, 0, &status};

	status = SYS$DCLEXH (&exh_blk);
	if ((status & 0x1) == 0)
		exit (status);

	/* Define LNM$FILE_DEV in exec mode in the process directory table */
	status = SYS$CRELNM (0, &processdirectory, &file_dev, &acmode,
			     &cre_file_dev_itmlst);
	iferr (status)
	{
		printf ("Can't create logical name\n");
		exit (status);
	}

	/* Check to make sure that "TACACS_UAF" is defined in exec mode. */
	status = SYS$TRNLNM (0, &file_dev, &lognam, &acmode, 0);
	iferr (status)
	{
		printf ("Can't get TACACS_UAF logical name\n");
		exit (status);
	}
	/* Check to see whether this file is accessible */
	status = stat (TACACS_UAF, &statbuf);
	if (status != 0)
	{
		printf ("Can;t stat on TACACS_UAF\n");
		exit (vaxc$errno);
	}

	/*
	 * Define SYSUAF in exec mode in the process LNM table as
	 * "TACACS_UAF"
	 */
	status = SYS$CRELNM (0, &processtable, &sysuaf, &acmode, &cre_sysuaf_itmlst);
	iferr (status)
	{
		printf ("Can't define SYSUAF to be TACACS-UAF\n");
		exit (status);
	}
	/* Define RIGHTSLIST in exec mode in the process LNM table as "_NLA0" */
	status = SYS$CRELNM (0, &processtable, &rightslist, &acmode,
			     &cre_nla0_itmlst);
	iferr (status)
	{
		printf ("Can't define RIGHTLIST\n");
		exit (status);
	}
	/* Define NETPROXY in exec mode in the process LNM table as "_NLA0" */
	status = SYS$CRELNM (0, &processtable, &netproxy, &acmode,
			     &cre_nla0_itmlst);
	iferr (status)
	{
		printf ("Can't define NETPROXY\n");
		exit (status);
	}
}				/* init_tacacs_uaf */


/* logerror
 *   Log an error message.  By default, log to OPCOM; if TACACS_ERRLOG
 *   is defined, log to it.
 *
 *   Returns status.
 */

#define MAX_OPCOM_MSG	255

typedef struct _opc_mess_buf	/* message buffer for $SNDOPR */
{
	char            type;	/* OPC$B_MS_TYPE */
	unsigned        target:24;	/* OPC$B_MS_TARGET */
	unsigned long   rqstid;	/* OPC$B_MS_RQSTID */
	char            text[MAX_OPCOM_MSG + 1];	/* OPC$L_MS_TEXT */
}               opc_mess_buf;

LONG 
logerror (s)
char           *s;		/* string to write out to log */
{
	char           *getenv ();

	opc_mess_buf    mb;	/* message buffer for $SNDOPR call */
	unsigned long int status;
	char           *logfile;
	char           *opcom = "OPCOM";
	struct dsc$descriptor sndopr_desc;
	FILE           *lf;

	logfile = getenv (TACACS_ERRLOG);
	if (logfile == NULL)
		logfile = opcom;
	if (strcmp (logfile, "OPCOM") == 0)
	{
		if (strlen (s) > MAX_OPCOM_MSG)
			s[MAX_OPCOM_MSG] = '\0';

		mb.type = OPC$_RQ_RQST;
		mb.target = OPC$M_NM_NTWORK;	/* make OPCOM send to NETWORK
						 * opers */
		mb.rqstid = 0;
		strcpy (mb.text, s);

		sndopr_desc.dsc$w_length = sizeof mb.type + sizeof mb.target +
			sizeof mb.rqstid + strlen (s);
		sndopr_desc.dsc$b_dtype = DSC$K_DTYPE_T;
		sndopr_desc.dsc$b_class = DSC$K_CLASS_S;
		sndopr_desc.dsc$a_pointer = &mb;

		status = SYS$SNDOPR (&sndopr_desc, 0);
		return (status);
	}
	else
	{
		/* try logging the message to a file */

		lf = fopen (logfile, "a+");
		if (lf == NULL)
			return (vaxc$errno);
		fputs (s, lf);
		fputc ('\n', lf);
		fclose (lf);
		return (SS$_NORMAL);
	}

}				/* logerror */


/* verify_user
 *   Verify this username/password combination against the TACACS_UAF.
 *   Inputs: username, password.
 * All hyphens in the username are replaced with underscores for SLIP addresses.
 *   Returns VMS system call status - should always be SS$_NORMAL unless
 *   some unexpected strange system service status.
 *
 *   Returns one of the following in user_status:
 *   USER_BOGUS     - no such user, bad password, etc.
 *   USER_EXPIRED   - user password expired
 *   USER_EXPIRING  - user password < TACACS_PASSWORD_THRESHOLD days from
 *                    expiring
 *   USER_OK        - user is authorized to do his thing
 */

unsigned long int 
verify_user (username, password, user_status)
char           *username;
char           *password;
unsigned long int *user_status;
{
	void            upcase_string ();
	LONG            SYS$GETTIM (),
	                SYS$GETUAI (),
	                SYS$HASH_PASSWORD (),
	                LIB$ADD_TIMES (),
	                LIB$CVT_TO_INTERNAL_TIME ();

	LONG            status,
	                uai_flags;
	QUAD            uai_pwd_date,
	                uai_pwd_lifetime,
	                uai_pwd,
	                putative_pw,
	                uai_pwd_expiration,
	                tacacs_pw_thresh,
	                now,
	                warn_if_before;
	WORD            uai_salt;
	BYTE            uai_encrypt;
	struct dsc$descriptor usernameD,
	                passwordD;
	char           *p,
	                Username[256];

	/*
	 * $GETUAI this user.  Return the following fields: UAI$_FLAGS (we
	 * are interested in UAI$V_DISACNT, UAI$V_PWD_EXPIRED) UAI$_PWD_DATE
	 * (date of last pw change) UAI$_PWD_LIFETIME UAI$_ENCRYPT
	 * (encryption algorithm used) UAI$_SALT UAI$_PWD
	 */

	ITEM            itmlst[] = {
		{4, UAI$_FLAGS, &uai_flags, 0},
		{8, UAI$_PWD_DATE, uai_pwd_date, 0},
		{8, UAI$_PWD_LIFETIME, uai_pwd_lifetime, 0},
		{1, UAI$_ENCRYPT, &uai_encrypt, 0},
		{2, UAI$_SALT, &uai_salt, 0},
		{8, UAI$_PWD, uai_pwd, 0},
		{0, 0, 0, 0}
	};

/* If the password or the username are empty - return error (HUJI) */
	if ((*username == '\0') || (*password == '\0'))
		return RMS$_RNF;/* No such user */

/* Do not ruin the username - copy it elsewhere */
	strcpy (Username, username);
	upcase_string (Username);
	upcase_string (password);

/* Replace all - in username to _ */
	while ((p = strchr (Username, '-')) != NULL)
		*p = '_';
/* If there is .SLIP in it then remove it */
	if ((p = strchr (Username, '.')) != NULL)
		if (strncmp (p, ".SLIP", 5) == 0)
			*p = '\0';

	str2desc (usernameD, Username);
	status = SYS$GETUAI (0, 0, &usernameD, &itmlst, 0, 0, 0);
	if (status == RMS$_RNF || (UAI$M_DISACNT & uai_flags))
	{

		/*
		 * A record was not found for this username, or the guy's
		 * disuser'd
		 */
		*user_status = USER_BOGUS;
		return (SS$_NORMAL);
	}
	iferr (status)
		return (status);/* unexpected error of some kind */

	/*
	 * Now hash the putative password to see if it matches the one on
	 * file.
	 */

	str2desc (passwordD, password);
	status = SYS$HASH_PASSWORD (&passwordD, uai_encrypt, uai_salt, &usernameD,
				    putative_pw);
	iferr (status)
		return (status);
	if ((putative_pw[0] != uai_pwd[0]) || putative_pw[1] != uai_pwd[1])
	{
		*user_status = USER_BOGUS;
		return (SS$_NORMAL);
	}

	/* The putative password matches the one on file, but has it expired? */

	if (UAI$M_PWD_EXPIRED & uai_flags)
	{
		*user_status = USER_EXPIRED;
		return (SS$_NORMAL);
	}

	/* Is this password < TACACS_PASSWORD_THRESHOLD days from expiring? */

	if (uai_pwd_lifetime[0] == 0 && uai_pwd_lifetime[1] == 0)
	{
		/* I.e. this password will never expire */
		*user_status = USER_OK;
		return (SS$_NORMAL);
	}

	LIB$ADD_TIMES (uai_pwd_date, uai_pwd_lifetime, uai_pwd_expiration);
	LIB$CVT_TO_INTERNAL_TIME (&LIB$K_DELTA_DAYS, &TACACS_PASSWORD_THRESHOLD,
				  tacacs_pw_thresh);
	SYS$GETTIM (now);
	LIB$ADD_TIMES (now, tacacs_pw_thresh, warn_if_before);
	if (warn_if_before[1] > uai_pwd_expiration[1] ||
	((warn_if_before[1] == uai_pwd_expiration[1]) && (warn_if_before[0] >
						    uai_pwd_expiration[0])))
	{
		*user_status = USER_EXPIRING;
	}
	else
	{
		*user_status = USER_OK;
	}
	return (SS$_NORMAL);

}				/* verify_user */


/* log_transaction
 *   Log a tacacs authentication transaction to some file.
 */

log_transaction (client, name, tp)
struct sockaddr_in *client;
char           *name;
struct tacacspkt *tp;
{
	char           *getenv (),
	               *inet_ntoa (),
	               *ctime ();
	FILE           *fopen ();
	LONG            logerror ();
	int             sprintf (),
	                fputs (),
	                fclose ();
	time_t          time ();
	void           *malloc (),
	                free ();

	LONG            status;
	char           *logfilename,
	               *nowstring,
	               *in_addr_string;
	extern char    *tacacs_responses[],
	               *tacacs_reasons[];	/* in tacacs-server.h */
	char            outstring[BIG_STRING];
	FILE           *lf;
	time_t         *now;

	logfilename = getenv (TACACS_LOG);
	if (logfilename == NULL)
		return;

	lf = fopen (logfilename, "a+");
	if (lf == NULL)
	{
		sprintf (outstring, "error opening TACACS_LOG: %X", vaxc$errno);
		logerror (outstring);
		return;
	}
	now = malloc (sizeof (time_t));
	*now = time (NULL);
	nowstring = malloc (26);/* malloc room for the time string */
	nowstring = ctime (now);
	nowstring[24] = '\0';	/* trim off newline */
	in_addr_string = inet_ntoa (client->sin_addr);
	sprintf (outstring, "%s %s(%u) %s Response: %s Reason: %s\n",
		 nowstring, in_addr_string, client->sin_port, name,
		 tacacs_responses[tp->response], tacacs_reasons[tp->reason]);
	fputs (outstring, lf);
	free (now);
	free (nowstring);
	status = fclose (lf);
	if (status != 0)
	{
		sprintf (outstring, "error closing TACACS_LOG: %X", vaxc$errno);
		logerror (outstring);
	}

}				/* log_transaction */


/* upcase_string
 *   Upcase a string in place.
 */

void 
upcase_string (s)
char           *s;
{
	while (*s)
	{
		*s = _toupper (*s);
		s++;
	}

}				/* upcase_string */


/* tacacs_exit_handler
 *   Exit handler for the tacacs server.  Our main work here is to
 *   undo the logical name tomfoolery that init_tacacs_uaf() inflicted on
 *   our process.
 *
 */

void 
tacacs_exit_handler (exit_status)
LONG            exit_status;
{
	LONG            status,
	                SYS$DELLNM ();
	BYTE            acmode = PSL$C_EXEC;

	$DESCRIPTOR (processtable, "LNM$PROCESS_TABLE");
	$DESCRIPTOR (processdirectory, "LNM$PROCESS_DIRECTORY");
	$DESCRIPTOR (rightslist, "RIGHTSLIST");
	$DESCRIPTOR (netproxy, "NETPROXY");
	$DESCRIPTOR (sysuaf, "SYSUAF");
	$DESCRIPTOR (filedev, "LNM$FILE_DEV");

	status = SYS$DELLNM (&processdirectory, &filedev, &acmode);
	status = SYS$DELLNM (&processtable, &sysuaf, &acmode);
	status = SYS$DELLNM (&processtable, &rightslist, &acmode);
	status = SYS$DELLNM (&processtable, &netproxy, &acmode);

}				/* tacacs_exit_handler */

/* tacacs-routines.c */
