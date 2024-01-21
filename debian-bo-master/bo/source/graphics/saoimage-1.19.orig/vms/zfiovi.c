/*
 * ZFIOVI.C -- VMS Imtool driver.  These interface routines are used by the
 *	interactive binary graphics interface (ZFIOGD) and any image display
 *	programs that support the Imtool protocol.
 *
 *	Interface routines:
 *
 *		zopnvi		Open channel for VMS/IRAF-Imtool communication.
 *		zclsvi		Close channel.
 *		zardvi		Read from channel.
 *		zawrvi		Write to channel.
 *		zawtvi		Wait on read/write completion.
 *
 *	Supplementary interface routines (not used by VMS/IRAF ZFIOGD):
 *
 *		ZBufSize		Returns maximum i/o transfer size.
 *		ZPending		Check if data is waiting to be read.
 *		ZSelectAsyncInput	Allow asynchronous i/o notification.
 *
 * NOTES
 *
 *	For this prototype implementation, VMS mailboxes are used.  A limit
 *	of 8192 bytes per i/o operation is assumed by ZFIOGD and any image
 *	display program that uses these routines (e.g., SAOimage).  The i/o
 *	is effectively synchronous given the behavior of VMS mailboxes.
 *
 *	The supplementary interface routines are to support image display
 *	programs which need to wait on window events and Imtool "pipe" i/o
 *	simultaneously a la Unix select().  These functions are modelled
 *	after similar ones in Xwindows (which may be VMS-dependent also).
 *
 *	Since this will be linked into VMS/IRAF applications, the standard
 *	C library functions are not used.  Only VMS/IRAF-supported ones
 *	and VMS system routines are referenced here.
 *
 *	The mailbox device names are passed between the cooperating
 *	processes via locks.  This somewhat sneaky approach is used
 *	so that GRPNAM privilege is not required for VMS/IRAF users.
 *	This approach assumes that mailbox device names (_MBAnnnn:)
 *	will fit into the 16-byte lock value block, which is not a
 *	problem as of VMS V5.2.
 *
 * HISTORY
 *
 *	Oct-1989   Jay Travisano   Space Telescope Science Institute
 *		Prototype implementation to support SAOimage with VMS/IRAF.
 */

#include <descrip.h>	/* Descriptor definitions		*/
#include <dvidef.h>	/* Get device info definitions		*/
#include <jpidef.h>	/* Get job/process info definitions	*/
#include <lckdef.h>	/* Lock definitions			*/
#include <iodef.h>	/* I/O definitions			*/
#include <ssdef.h>	/* System status definitions		*/
#include <syidef.h>	/* Get system info definitions		*/

#ifdef IMTOOL
#define import_spp	/* Import the IRAF SPP constants	*/
#include <iraf.h>
#else
#define READ_ONLY	1	/* file access modes */
#define XERR		(-1)
#define XOK		0
#define XINT		int
#define XLONG		long
#endif

#define SZ_MBX		8192	/* maximum transfer size	*/
#define SZ_MBXNAME	48	/* size of logical mailbox name */

#define SZ_USERNAME	12	/* VMS size limits		*/
#define SZ_NODENAME     15
#define SZ_LOCKNAME	31
#define SZ_LOCKVALBLK	16

#define VMS_OKAY(s)    (s & 1)	/* VMS status code checking	*/
#define VMS_ERROR(s) (!(s & 1))


#define SET_DESCRIPTOR(name,value,length) \
		name.dsc$a_pointer = value; \
		name.dsc$w_length  = length; \
		name.dsc$b_dtype   = DSC$K_DTYPE_T; \
		name.dsc$b_class   = DSC$K_CLASS_S

typedef  struct dsc$descriptor_s  DESC;


struct io_status_block {
	short	status;
	short	count;
	long	pid;
};
typedef  struct io_status_block  IOSB;


struct lock_status_block {
	short	status;
	short	reserved;
	long	lkid;
	char	valblk[SZ_LOCKVALBLK];
};
typedef  struct lock_status_block  LOCK;


struct channel_info {
	short	mbx_chan;
	IOSB	iosb;
	int	lockid;
	int	qio_status;
	int	io_pending;
	int	(*ast)();
	int	ast_arg;
};
typedef  struct channel_info  CHANNEL;


#define NUM_CHANNELS  2

static  CHANNEL  channels[NUM_CHANNELS];


/*
 * ZOPNVI -- Open an IRAF-Imtool communication path using mailboxes.
 */

zopnvi (imtool_name, mode, chan)
char	*imtool_name;
XINT	*mode;
XINT	*chan;
{
	char	username[SZ_USERNAME+1];
	DESC	_username;

	char	mbxname[SZ_MBXNAME+1];
	DESC	_mbxname;

	char	devname[SZ_LOCKVALBLK];
	DESC	_devname;

	short	len, ch;
	int	stat, lockid, i;

	/* Generate mailbox logical name from Imtool name and username.
	 */

	SET_DESCRIPTOR (_username, username, sizeof(username)-1);

	lib$getjpi (&JPI$_USERNAME, 0, 0, 0, &_username, &len);
	username[len] = 0;
	while (username[--len] == ' ')		/* trim trailing blanks */
	    username[len] = 0;

	strcat (strcat (strcpy (mbxname, imtool_name), "_"), username);

	/* Open or create the mailbox.  First, enqueue a lock using the
	 * mailbox logical name just formatted.  If the mailbox already
	 * exists, the lock value returned will contain the device name.
	 * If not, we'll have to create the mailbox and update the lock
	 * with the actual device name.  Dequeue the lock on any error.
	 */

	if ((lockid = Enqueue_Lock (mbxname, devname)) == 0) {
	    *chan = XERR;
	    return;
	}

	if (devname[0] && strncmp (devname, "_MBA", 4) == 0) {

	    /* Mailbox already exists; assign a channel to it.
	     */

	    SET_DESCRIPTOR (_devname, devname, strlen(devname));

	    stat = sys$assign (&_devname, &ch, 0, 0);

	    if VMS_ERROR(stat) {
		Dequeue_Lock (lockid);
		putmsg (devname, 0);
		putmsg ("ZFIOVI: Mailbox assign error", stat);
		*chan = XERR;
		return;
	    }

	    Update_Lock (lockid, 0);

	} else {
	    /* Create the mailbox and update the lock with the
	     * actual device name.
	     */

	    int  bufquo;

	    if (*mode == READ_ONLY)
		bufquo = SZ_MBX * 2;
	    else
		bufquo = SZ_MBX;

	    SET_DESCRIPTOR (_mbxname, mbxname, strlen(mbxname));

	    stat = sys$crembx (0, &ch, SZ_MBX, bufquo, 0, 0, &_mbxname);

	    if VMS_ERROR(stat) {
		Dequeue_Lock (lockid);
		putmsg (mbxname, 0);
		putmsg ("ZFIOVI: Mailbox creation error", stat);
		*chan = XERR;
		return;
	    }

	    SET_DESCRIPTOR (_devname, devname, sizeof(devname));

	    lib$getdvi (&DVI$_DEVNAM, &ch, 0, 0, &_devname, &len);
	    devname[len] = 0;

	    Update_Lock (lockid, devname);
	}

#ifdef DEBUG
	/* Print logical and device names of mailbox.
	 */
	{
	char	dbgmsg[80];

	strcat (strcat (strcpy (dbgmsg, mbxname), " = "), devname);
	putmsg (dbgmsg, 0);
	}
#endif

	/* Get a free channel structure and initialize.
	 */

	for (i = 0; i < NUM_CHANNELS; i++)
	    if (channels[i].mbx_chan == 0)
		break;

	if (i == NUM_CHANNELS) {
	    putmsg ("ZFIOVI: No channel structures available", 0);
	    sys$dassgn (ch);
	    Dequeue_Lock (lockid);
	    *chan = XERR;
	} else {
	    CHANNEL	*c;

	    c = &channels[i];

	    c->mbx_chan   = ch;
	    c->lockid     = lockid;
	    c->qio_status = 0;
	    c->ast        = 0;
	    c->ast_arg    = 0;

	    if (*mode == READ_ONLY) {
		/* Check if there are messages waiting to be read.
		 * This number is in the two low-order bytes of the
		 * device-dependent longword.
		 */
		lib$getdvi (&DVI$_DEVDEPEND, &ch, 0, &c->io_pending);
		c->io_pending &= 0x0000FFFF;
	    } else {
		c->io_pending = 0;
	    }

	    *chan = c;
	}
}


/*
 * ZCLSVI -- Close IRAF-Imtool connection.
 */

zclsvi (chan, status)
XINT	*chan;
XINT	*status;
{
	CHANNEL	*c;
	int	stat;

	/* Deassign the mailbox channel, dequeue the associated lock,
	 * and "free" the channel structure.
	 */

	c = *chan;

	stat = sys$dassgn (c->mbx_chan);
	if VMS_ERROR(stat) {
	    putmsg ("ZFIOVI: Error deassigning mailbox channel", stat);
	    *status = XERR;
	} else {
	    *status = XOK;
	}

	Dequeue_Lock (c->lockid);

	c->mbx_chan = 0;
}


/*
 * ZARDVI -- Asynchronous read on IRAF-Imtool channel.
 */

zardvi (chan, buf, maxbytes, loffset)
XINT	*chan;
char	*buf;
XINT	*maxbytes;
XLONG	*loffset;			/* ignored */
{
	CHANNEL	*c;

	/* Post a read on the mailbox.
	 */

	c = *chan;

	c->qio_status = sys$qio (0, c->mbx_chan, IO$_READVBLK,
				&c->iosb, 0, 0,
				buf, *maxbytes, 0,0,0,0);
}


/*
 * ZAWRVI -- Asynchronous write to IRAF-Imtool channel.
 */

zawrvi (chan, buf, nbytes, loffset)
XINT	*chan;
char	*buf;
XINT	*nbytes;
XLONG	*loffset;			/* ignored */
{
	CHANNEL	*c;

	/* Write to the mailbox.
	 */

	c = *chan;

	c->qio_status = sys$qio (0, c->mbx_chan, IO$_WRITEVBLK | IO$M_NOW,
				&c->iosb, 0, 0,
				buf, *nbytes, 0,0,0,0);
}


/*
 * ZAWTVI -- Wait for asynchronous read/write to complete.
 */

zawtvi (chan, status)
XINT	*chan;
XINT	*status;
{
	CHANNEL	*c;

	/* Wait on i/o to complete.
	 */

	c = *chan;

	sys$synch (0, &c->iosb);

	if VMS_ERROR(c->qio_status) {
	    putmsg ("ZFIOVI: QIO error", c->qio_status);
	    *status = XERR;

	} else if (c->iosb.status == SS$_ENDOFFILE) {
	    putmsg ("ZFIOVI: Unexpected EOF on mailbox", c->iosb.status);
	    *status = 0;

	} else if VMS_ERROR(c->iosb.status) {
	    putmsg ("ZFIOVI: Mailbox i/o error", c->iosb.status);
	    *status = XERR;

	} else {
	    *status = c->iosb.count;

	    if (c->io_pending)
		c->io_pending--;
	}
}



/*
 * ZBufSize -- Return maximum i/o transfer size, in bytes.
 */

ZBufSize (CHANNEL *c)
{
	return SZ_MBX;
}


/*
 * ZPending -- Return data pending count.
 */

ZPending (CHANNEL *c)
{
	return c->io_pending;
}


/*
 * ZSelectAsyncInput -- Set up for asynchronous notification when i/o is
 *	ready on a given channel.  This must be reset by the application
 *	_after_ it has read the data from the mailbox.
 */

ZSelectAsyncInput (CHANNEL *c, int (*ast)(), int ast_arg)
{
	if (ast)
	    Set_Attn_AST (c);

	c->ast     = ast;
	c->ast_arg = ast_arg;
}


/*
 * Set_Attn_AST -- Set a write attention AST on the mailbox channel
 */

static
Set_Attn_AST (CHANNEL *c)
{
	int	Attn_AST();
	int	stat;


	stat = sys$qiow (0, c->mbx_chan, IO$_SETMODE | IO$M_WRTATTN,
				&c->iosb, 0, 0,
				Attn_AST, c, 0,0,0,0);
	if VMS_ERROR(stat)
	    putmsg ("ZFIOVI: Error setting write attn ast", stat);
	else if VMS_ERROR(c->iosb.status)
	    putmsg ("ZFIOVI: Error setting write attn ast", c->iosb.status);
}


/*
 * Attn_AST -- Write attention AST routine.  Increment the i/o pending
 *	count and invoke the application's AST routine.
 */

static
Attn_AST (CHANNEL *c)
{
	c->io_pending++;

	if (c->ast)
	    (* c->ast) (c->ast_arg);
}


/*
 * PUTMSG - Put an error message and/or an associated system error
 *	string to SYS$OUTPUT.
 */

static
putmsg (char *msg, int code)
{
	DESC	_msg;

	struct {
	    short	arg_count;
	    short	msg_options;
	    int		msg_code;
	} msgvec;

	if (msg) {
	    SET_DESCRIPTOR (_msg, msg, strlen(msg));

	    lib$put_output (&_msg);
	}

	if (code) {
	    msgvec.arg_count   = 1;
	    msgvec.msg_options = 0x0F;		/* output full message */
	    msgvec.msg_code    = code;

	    sys$putmsg (&msgvec);
	}
}


/*
 * ENQUEUE_LOCK -- Enqueue an exclusive lock.  Copies the value from the
 *	lock status block.  The function return value is the lock id for
 *	subsequent lock operations, or zero if there was an error.
 *
 *	Locks are cluster-wide by default, so the local node name is added
 *	to the specified lock name.  This will prevent conflicts in cases
 *	where shared accounts are used in a VAX cluster environment.
 */

static
int Enqueue_Lock (char *lockname, char *valblk)
{
	int	stat, sys$enqw();
	short	len;

	LOCK	lblock;

	char	nodename[SZ_NODENAME+1];
	DESC	_nodename;

	char	resnam[SZ_LOCKNAME*2];
	DESC	_resnam;

	/* Generate the lock resource name of the form: lockname@nodename.
	 * This string is truncated if necessary.
	 */

	SET_DESCRIPTOR (_nodename, nodename, sizeof(nodename)-1);

	lib$getsyi (&SYI$_NODENAME, 0, &_nodename, &len, 0, 0);
	nodename[len] = 0;

	strcat (strcat (strcpy (resnam, lockname), "@"), nodename);
	resnam[SZ_LOCKNAME] = 0;

	SET_DESCRIPTOR (_resnam, resnam, strlen(resnam));

	/* Enqueue the lock.
	 */

	stat = sys$enqw (
			0,			/* event flag		*/
			LCK$K_EXMODE,		/* lock mode		*/
			&lblock,		/* lock status block 	*/
			LCK$M_VALBLK,		/* flags (get value block) */
			&_resnam,		/* resource name 	*/
			0,			/* parent id		*/
			0,			/* AST address		*/
			0,			/* AST parameter	*/
			0,			/* Blocking AST		*/
			0,			/* access mode		*/
			0			/* null argument	*/
			);

	if VMS_ERROR(stat) {
	    putmsg ("ZFIOVI: Error enqueueing lock request", stat);
	    return 0;
	}
	if VMS_ERROR(lblock.status) {
	    putmsg ("ZFIOVI: Lock error from enqueue request", lblock.status);
	    return 0;
	}

	if (valblk)
	    lib$movc3 (&SZ_LOCKVALBLK, lblock.valblk, valblk);

	return lblock.lkid;
}


/*
 * UPDATE_LOCK -- Update a lock value block by converting it to null mode.
 *	This keeps the lock and its value block around for other processes
 *	to access later.
 */

static
Update_Lock (int lockid, char *valblk)
{
	int	stat, sys$enqw();
	int	flags;
	LOCK	lblock;

	lblock.lkid = lockid;

	flags = LCK$M_CONVERT;

	if (valblk) {
	    flags |= LCK$M_VALBLK;
	    lib$movc3 (&SZ_LOCKVALBLK, valblk, lblock.valblk);
	}

	/* Enqueue the lock (conversion).
	 */

	stat = sys$enqw (
			0,			/* event flag		*/
			LCK$K_NLMODE,		/* lock mode		*/
			&lblock,		/* lock status block 	*/
			flags,			/* flags		*/
			0,			/* resource name 	*/
			0,			/* parent id		*/
			0,			/* AST address		*/
			0,			/* AST parameter	*/
			0,			/* Blocking AST		*/
			0,			/* access mode		*/
			0			/* null argument	*/
			);

	if VMS_ERROR(stat)
	    putmsg ("ZFIOVI: Error enqueueing lock conversion request", stat);

	if VMS_ERROR(lblock.status)
	    putmsg ("ZFIOVI: Lock error from conversion request", lblock.status);
}


/*
 * DEQUEUE_LOCK - Dequeue a lock.
 */

static
Dequeue_Lock (int lockid)
{
	int	stat, sys$deq();

	stat = sys$deq (
			lockid,			/* lock identification	*/
			0,			/* value block		*/
			0,			/* access mode		*/
			0			/* $DEQ options		*/
			);

	if VMS_ERROR(stat)
	    putmsg ("ZFIOVI: Error dequeueing lock", stat);
}




#ifdef REQUIRES_GRPNAM_PRIVILEGE

	/**
	 ** This is here for reference only.  It's use has been superseded
	 ** by the use of locks above.  If GRPNAM privilege is assumed,
	 ** this function is called before a call to SYS$CREMBX, whether
	 ** or not the mailbox has already been created.
	 **/

#include <lnmdef.h>	/* Logical name definitions		*/
#include <prvdef.h>	/* Privilege mask definitions		*/

/*
 * SET_GROUP_TABLE -- Set logical so that mailbox logical name is put
 * into the group table.  Note that we cannot use LIB$SET_LOGICAL here
 * (the easier approach) since this software will be run as part of a
 * VMS/IRAF subprocess which has no CLI associated with it.  If the
 * process does not currently have GRPNAM privilege, issue a warning.
 * This will be more useful than getting a mailbox creation error.
 */

static
set_group_table ( )
{
	$DESCRIPTOR (_name,  "LNM$TEMPORARY_MAILBOX");
	$DESCRIPTOR (_value, "LNM$GROUP");
	$DESCRIPTOR (_table, "LNM$PROCESS_DIRECTORY");

	struct item_list {
	    short	len;		/* length of buffer */
	    short	code;		/* service-dependent option code */
	    char	*buf;		/* user buffer */
	    short	*blen;		/* return length for data in buf */
	} itmlst[2];

	int	stat, curpriv;
	char	fallback_msg[] = 
		"Cannot set GROUP table for mailbox name; using JOB table";

	lib$getjpi (&JPI$_CURPRIV, 0, 0, &curpriv, 0, 0);

	if (curpriv & PRV$M_GRPNAM) {

	    itmlst[0].len  = _value.dsc$w_length;
	    itmlst[0].code = LNM$_STRING;
	    itmlst[0].buf  = _value.dsc$a_pointer;
	    itmlst[0].blen = 0;
	    itmlst[1].len  = 0;
	    itmlst[1].code = 0;

	    stat = sys$crelnm (&LNM$M_NO_ALIAS, &_table, &_name, 0, itmlst);
	    if VMS_ERROR(stat)
		putmsg (fallback_msg, stat);
	} else {
	    putmsg (fallback_msg, 0);
	}
}

#endif

