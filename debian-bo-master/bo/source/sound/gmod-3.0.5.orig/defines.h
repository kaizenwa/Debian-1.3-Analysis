// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __defines_h
#define __defines_h

#ifdef USE_X
#define IDENT "@(#) Xgmod 3.0.5 - Copyright (C) 1997 by Andrew J. Robinson"
#define HEADING "Xgmod 3.0.5"
#else
#define IDENT  "@(#) gmod version 3.0.5 - Copyright (C) 1997 by Andrew J. Robinson"
#define HEADING	"GMOD version 3.0.5\nCopyright (C) 1997 by Andrew J. Robinson\n"
#endif

#define MY_TRUE 	1
#define MY_FALSE	0

/* module types */
#define MODULE_NOT_S3M		0
#define MODULE_S3M		1
#define MODULE_XM               2

/* frequencies */
#define C2FREQ                  261632
#define NTSC_RATE               8363
#define PAL_RATE                8287

#define CVT_MOD_SPEED(x)        ((x) < 32 ? CMD_SET_TICKS : CMD_SET_BPM)

#define MIN(a, b)		((a) < (b) ? (a) : (b))

#define BYTE(x)		(*(u_char *)(x))
#define INTEL_SHORT(x)	(BYTE(x)  | (BYTE(x+1) <<8))
#define REV_SHORT(x)    (BYTE(x) <<8 | BYTE(x+1))
#define INTEL_LONG(x)	(INTEL_SHORT(x) | (INTEL_SHORT(x+2) <<16))

#define TICKS_PER_DIVISION(x)  (ticks_per_division = x)
#define TEMPO(x, y)               ((x) ? (tick_duration = ((double)y * 100.0 / (24.0 * x))) : 0 )

#define MAX_TRACK	32
#define MAX_PATTERN	256
#define MAX_POSITION	256
#define MAX_SAMPLES	255

#define HDR_SIZE        0x30	/* number of bytes to read to determine
				   module type */
#define VOL_SLIDE_RATE	4	/* rate for volume slides, bigger is faster */

/* slide defines */

#define SLIDE_UP	1	/* slide up */
#define SLIDE_DOWN      2	/* slide down */
#define SLIDE_ONCE	3	/* only slide once */
#define SLIDE_PORT	4	/* tone portamento */
#define SLIDE_OFF	5	/* turn channel off */

#define SLIDE_NEG       0	/* positive rate */
#define SLIDE_POS       1	/* negative rate */

#define SLIDE_PERIOD_LIN  0
#define SLIDE_FREQ_LIN    1
#define SLIDE_NOTE_LIN    2

#define SLIDE_RATE_669    2

/* panning flags */
#define PAN_NO_HARDWARE	0
#define PAN_HARDWARE	1

/* move types (bit flags) */

#define MOVE_LOOP	0x01
#define MOVE_JUMP	0x02
#define MOVE_EXIT	0x04
#define MOVE_FORWBACK   0x08
#define MOVE_BREAK	0x10

/* volume types */

#define VOL_LINEAR      0
#define VOL_LOG         1

/* patch types */
#define PATCH_NORMAL    0
#define PATCH_XM        1

/* octave types */
#define OCTAVE_LIMIT    0
#define OCTAVE_EXTEND   1
#define OCTAVE_AUTO     2

/* exit codes */
#define ERR_NOERROR     0	/* no error - normal termination */
#define ERR_BADARGS     50	/* bad arguments */
#define ERR_SEQUENCER   51	/* error accessing sequencer */
#define ERR_BADLOAD     52	/* error loading a module */
#define ERR_NOGUS       53	/* no GUS found */
#define ERR_FORK        54	/* error forking */

/* timer state */
#define TIMER_OFF     0
#define TIMER_ON       1

/* terminal state */
#define TERMINAL_RAW   0
#define TERMINAL_RESTORE  1

/* stop types (non-zero) */
#define STOP_NEXT      1	/* stop current, go to next */
#define STOP_EXIT      2	/* stop current and exit */
#define STOP_PREV      3	/* stop current, go to previous */
#define STOP_FORWBACK  4	/* move forward or backward patterns */
#ifdef USE_X
#define STOP_GOTO      5	/* go to a specific module */
#endif

/* special orders */
#define ORDER_STOP     -1	/* stop playing */
#define ORDER_SKIP     -2	/* skip position */

/* special notes */
#define NOTE_STOP      254	/* stop note playing */
#define NOTE_LAST      255	/* use last note played on channel */

/* mixer defines */
#define MIXER_MIN_VOL  0
#define MIXER_MAX_VOL 100

/* echo events */
#define ECHO_NONE	0x00	/* nothing */
#define ECHO_MESSAGE	0x01	/* time to display a message */
#define ECHO_END	0x02	/* end of module */
#define ECHO_SPEED0	0x03	/* speed 0 command */
#define ECHO_LOOP	0x04	/* infinite loop detected */
#define ECHO_PATTERN    0x05    /* position within the current pattern */
#define ECHO_STOP       0x06    /* playback halted */

#define NUM_PERIODS 127		/* number of periods in period table */
#define NUM_VIBRA 64		/* number of values in vibrato table */
#define NOTE_BASE 1		/* lowest note possible */

/* modified and added to by Peter Federighi */
#define GDECOMP_PGM	"gunzip -c "	/* program for decompression */
#define LHADECOMP_PGM	"lha pq "	/* lha decompression */
#define ZIPDECOMP_PGM	"unzip -p "	/* zip decompression */

#define GCOMPRESSED	2	/* start with 2 since FALSE */
#define LHACOMPRESSED	3	/* and TRUE take up 0 and 1 */
#define ZIPCOMPRESSED	4

/* rc file defines */
#define MAX_RC_LEN	161	/* maximum length of a line in the rc file */
#define RC_NAME		"/usr/etc/gmodrc"	/* name of system rc file */
#define USER_RC_NAME	"/.gmodrc"	/* name of a user's rc file */

/* string lengths */
#define NAME_LEN        33	/* length of module name (ULT longest) */
#define DESC_LEN        40	/* length of description */
#define SAMPNAME_LEN    33	/* length of sample name (ULT longest) */

/* print define */
#ifdef USE_NCURSES
#define FPUTS		addstr
#define PRINTF		printw
#else
#define FPUTS(s)	fputs(s, stdout)
#define PRINTF		printf
#endif

/* misc limits */
#define MAX_CUT         4      /* maximum amount to cut samples */

#endif
