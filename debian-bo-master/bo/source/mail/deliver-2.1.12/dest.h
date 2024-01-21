/* $Id: dest.h,v 1.1 1991/05/13 18:36:55 chip Exp $
 *
 * Description of a mail destination and its state.
 *
 * $Log: dest.h,v $
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

/*----------------------------------------------------------------------
 * Destination class.
 */

typedef enum {
    CL_USER,			/* User name, no mailbox                */
    CL_MBOX,			/* User name, with mailbox name         */
    CL_PROG,			/* Program to run with message on stdin */
    CL_UUCP			/* UUCP address (bang path)             */
} DCLASS;

/*----------------------------------------------------------------------
 * Destination state.
 */

typedef enum {
    ST_WORKING,			/* the "normal" state                   */
    ST_HOLD,			/* on hold during expansion             */
    ST_DONE,			/* all processing complete              */
    ST_ERROR			/* "something is horribly wrong"        */
} DSTATE;

/*----------------------------------------------------------------------
 * Types of destination errors.
 */

typedef enum {
    E_IVADDR,			/* invalid address string               */
    E_DFONLY,			/* "user:mbox" etc. for delfiles only   */
    E_NSUSER,			/* no such user                         */
    E_NSHOST,			/* no such host (UUCP addresses)        */
    E_CTPERM,			/* no permissions for that context      */
    E_CTLOST,			/* context lost (should never happen)   */
    E_MBOX,			/* can't write to mailbox               */
    E_PROG,			/* subprocess exited with non-zero      */
    E_PIPE,			/* can't pipe to subprocess (incl. uux) */
    E_ERRMSG			/* other user-described error           */
} DERROR;

/*----------------------------------------------------------------------
 * Structure describing a mail destination.
 */

#define DEST    struct dest
DEST {
    DEST *d_next;		/* next destination in the chain        */
    DEST *d_prev;		/* previous destination in the chain    */
    DCLASS d_class;		/* destination class                    */
    DSTATE d_state;		/* destination state                    */
    DERROR d_error;		/* error code (if state is ST_ERROR)    */
    char *d_errmsg;		/* error message (if error is E_ERRMSG) */
    int d_dfdone;		/* boolean -- delivery file was run     */
    char *d_name;		/* context for delivery                 */
    char *d_param;		/* parameter (mailbox or program name)  */
};

/*----------------------------------------------------------------------
 * Action macros.
 */

#define dest_err(d,m)   ((d)->d_state = ST_ERROR, (d)->d_error = (m))
