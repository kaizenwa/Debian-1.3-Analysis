/*
#ident	"@(#)smail/src:RELEASE-3_2:spool.h,v 1.5 1996/02/28 14:26:42 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * spool.h:
 *	interface file for routines in spool.c
 */

/* macros used in spool.c */
#define READ_FAIL	-2		/* spool file read failed */
#define WRITE_FAIL	-1		/* general-purpose write failed */
/* size of spool file basename */
#define SPOOL_FN_LEN	(sizeof("tttttt-iiiiiig") - 1)

/*
 * GETSPOOL fetches characters from the spool buffer
 * and calls read_spool to read more characters when the
 * end of the buffer is reached.
 * returns a char or EOF on end-of-file or READ_FAILED on read error
 */
#define GETSPOOL()	(msg_ptr < msg_max ?				\
			    (0xff & (*msg_ptr++)) :			\
			    ((msg_foffset + (msg_max - msg_buf) >= msg_size) ? \
				EOF :					\
				(read_spool() == FAIL) ?		\
				    READ_FAIL :				\
				    (0xff & (*msg_ptr++))))

/*
 * PUTSPOOL(c) writes a character to the spool file buffer and
 * flushes the buffer when it is full.
 */
#define PUTSPOOL(c)	(msg_max < end_msg_buf ?			\
			    (0xff &(*msg_max++ = (c))) :		\
			    write_spool() == FAIL ?			\
				EOF :					\
				(msg_foffset += msg_max - msg_buf,	\
				 msg_max = msg_buf,			\
				 (0xff & (*msg_max++ = (c)))))
