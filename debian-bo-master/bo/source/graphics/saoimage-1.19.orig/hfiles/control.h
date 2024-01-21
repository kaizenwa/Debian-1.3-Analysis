#ifndef lint
static char SccsCtrlId[] = "%W%  %G%";
#endif

/* Module:	Control.h
 * Purpose:	Define the struct for event loop control
 * Modified:	{0} Michael VanHilst	initial version	     23 December 1988
 *		{1} MVH added generalized connection struct     30 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

/* everything needed for single connection to a remote process */
struct connectRec {
  int type;		/* named pipe, socket, or VMS mailbox */
  int protocol;		/* packet type (IRAF, AIPS, HRC, CMD) */
  int fd;		/* file descriptor */
  int open;		/* 0 if closed, else open */
  int direction;	/* read, write, read/write, listen */
  char *name;		/* pipe device name or net socket address */
  int address;		/* network address or parent listener fd */
  void (*func)();	/* function to call when this connection signalled */
  int mask[4];		/* mask bit for file descriptor */
  struct connectRec *next;	/* linklist queue for event handling */
  struct connectRec *affiliate;	/* internal binding for listener/connection */
};

struct controlRec {
  int mode;		/* active event response mode */
  int *response;	/* widget returned data pointer */
  int eventmask;	/* current event mask */
  int priority;		/* special event tracking mask */
  int completed;	/* event response completed flag */
  int verbose;		/* print-messages-about-process-statuses */
  int tracking;		/* magnifier and color graph tracking */
  int magni_track;	/* track mouse with magni box */
  int coord_track;	/* track mouse with coord string */
  int print_buttons;	/* include buttons in hardcopy output */
  int printer;		/* PostScript, Imagen Impress, etc. */
  int look_and_feel;	/* SAO, Motif, OpenLook */

  int remote_connected;	/* must select for more than just X events */
  int select_size;	/* size of mask to check for event */
  int select_mask[4];	/* combined select masks for all connections */

  struct connectRec Xserver;
  struct connectRec IRAF_in;
  struct connectRec IRAF_out;
  struct connectRec AIPS_in;
  struct connectRec AIPS_out;
  struct connectRec aux_in;
  struct connectRec aux_out;

  XEvent event;		/* info about most recent X event */
};

/* constants used as codes */
#define IOP_pipe	1
#define IOP_socket	2
#define IOP_mailbox	3
#define IOP_SAO		0
#define IOP_Motif	1
#define IOP_OpenLook	2
#define IOP_IRAF	1
#define IOP_Imtool    257
#define IOP_PROS      513
#define IOP_AIPS	8
#define IOP_HRC        16
#define IOP_cmd        32
#define IOP_PostScript 10
#define IOP_Impress    12
/* Write, Read, and ReadWrite are fixed to match code in ctrldisk, ctrlpipe */
#define IOP_Write	1
#define IOP_Read       -1
#define IOP_ReadWrite	0
