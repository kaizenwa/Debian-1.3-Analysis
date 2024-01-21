/*
 * This file contains definitions for the various event codes and values
 * that go to make up event streams.
 */

#ifdef EventMon

/*
 * Allocation events use lowercase codes.
 */
#define E_Lrgint	'L'		/* Large integer allocation */
#define E_Real		'd'		/* Real allocation */
#define E_Cset		'e'		/* Cset allocation */
#define E_File		'g'		/* File allocation */
#define E_Record	'h'		/* Record allocation */
#define E_Tvsubs	'i'		/* Substring tv allocation */
#define E_External	'j'		/* External allocation */
#define E_List		'k'		/* List allocation */
#define E_Lelem		'm'		/* List element allocation */
#define E_Table		'n'		/* Table allocation */
#define E_Telem		'o'		/* Table element allocation */
#define E_Tvtbl		'p'		/* Table-element tv allocation */
#define E_Set		'q'		/* Set allocation */
#define E_Selem		't'		/* Set element allocation */
#define E_Slots		'w'		/* Hash header allocation */
#define E_Coexpr	'x'		/* Co-expression allocation */
#define E_Refresh	'y'		/* Refresh allocation */
#define E_Alien		'z'		/* Alien allocation */
#define E_Free		'Z'		/* Free region */
#define E_String	's'		/* String allocation */

/*
 * Some other monitoring codes.
 */
#define	E_BlkDeAlc	'-'		/* Block deallocation */
#define	E_StrDeAlc	'~'		/* String deallocation */
#define	E_Highlight	'H'		/* Allocation highlight */
#define	E_Offset	'+'		/* Address offset */
#define	E_Pause		';'		/* memory monitoring comment */

/*
 * These are not "events"; they are provided for uniformity in tools
 *  that deal with types.
 */
#define E_Integer	'@'		/* Integer value pseudo-event */
#define E_Null		'$'		/* Null value pseudo-event */
#define E_Proc		'%'		/* Procedure value pseudo-event */
#define E_Kywdint	'^'		/* Integer keyword value pseudo-event */
#define E_Kywdpos	'&'		/* Position value pseudo-event */
#define E_Kywdsubj	'*'		/* Subject value pseudo-event */

/*
 * Codes used for storage regions.
 */
#define	E_Base		'<'		/* Base address of storage region */
#define	E_Size		'>'		/* Region size */
#define	E_Used		'='		/* Space used */

/*
 * Symbol table events
 */
#define E_Pid		'.'		/* Symbol name */
#define E_Sym		'T'		/* Symbol table entry */

/*
 * Garbage collection region
 */
#define	E_Region	'?'		/* Region */

/*
 * Codes for main sequence events
 */

   /*
    * Timing events
    */
#define E_Tick		'.'		/* Clock tick */


   /*
    * Code-location event
    */
#define E_Loc		'|'		/* Location change */
#define E_Line		'\355'		/* Line change */

   /*
    * Virtual-machine instructions
    */
#define E_Opcode	'O'		/* Virtual-machine instruction */

   /*
    * Type-conversion events
    */
#define E_Aconv		'I'		/* Conversion attempt */
#define E_Tconv		'K'		/* Conversion target */
#define E_Nconv		'N'		/* Conversion not needed */
#define E_Sconv		'Q'		/* Conversion success */
#define E_Fconv		'J'		/* Conversion failure */

   /*
    * Structure events
    */
#define	E_Lbang		'\301'		/* List generation */
#define	E_Lcreate	'\302'		/* List creation */
#define	E_Lpop		'\303'		/* List pop */
#define	E_Lpull		'\304'		/* List pull */
#define	E_Lpush		'\305'		/* List push */
#define	E_Lput		'\306'		/* List put */
#define	E_Lrand		'\307'		/* List random reference */
#define	E_Lref		'\310'		/* List reference */
#define E_Lsub		'\311'		/* List subscript */
#define	E_Rbang		'\312'		/* Record generation */
#define	E_Rcreate	'\313'		/* Record creation */
#define	E_Rrand		'\314'		/* Record random reference */
#define	E_Rref		'\315'		/* Record reference */
#define E_Rsub		'\316'		/* Record subscript */
#define	E_Sbang		'\317'		/* Set generation */
#define	E_Screate	'\320'		/* Set creation */
#define	E_Sdelete	'\321'		/* Set deletion */
#define	E_Sinsert	'\322'		/* Set insertion */
#define	E_Smember	'\323'		/* Set membership */
#define	E_Srand		'\336'		/* Set random reference */
#define	E_Sval		'\324'		/* Set value */
#define	E_Tbang		'\325'		/* Table generation */
#define	E_Tcreate	'\326'		/* Table creation */
#define	E_Tdelete	'\327'		/* Table deletion */
#define	E_Tinsert	'\330'		/* Table insertion */
#define	E_Tkey		'\331'		/* Table key generation */
#define	E_Tmember	'\332'		/* Table membership */
#define	E_Trand		'\337'		/* Table random reference */
#define	E_Tref		'\333'		/* Table reference */
#define	E_Tsub		'\334'		/* Table subscript */
#define	E_Tval		'\335'		/* Table value */

   /*
    * Scanning events
    */

#define E_Snew		'\340'		/* Scanning environment creation */
#define E_Sfail		'\341'		/* Scanning failure */
#define E_Ssusp		'\342'		/* Scanning suspension */
#define E_Sresum	'\343'		/* Scanning resumption */
#define E_Srem		'\344'		/* Scanning environment removal */
#define E_Spos		'\346'		/* Scanning position */

   /*
    * Assignment
    */

#define E_Assign	'\347'		/* Assignment */
#define	E_Value		'\350'		/* Value assigned */


   /*
    * Sub-string assignment
    */

#define E_Ssasgn	'\354'		/* Sub-string assignment */
   /*
    * Interpreter stack events
    */

#define E_Intcall	'\351'		/* interpreter call */
#define E_Intret	'\352'		/* interpreter return */
#define E_Stack		'\353'		/* stack depth */

   /*
    * Expression events
    */
#define E_Ecall		'c'		/* Call of operation */
#define E_Efail		'f'		/* Failure from expression */
#define E_Eret		'r'		/* Return from expression */
#define E_Bsusp		'b'		/* Suspension from operation */
#define E_Esusp		'a'		/* Suspension from alternation */
#define E_Lsusp		'l'		/* Suspension from limitation */
#define E_Eresum	'u'		/* Resumption of expression */
#define E_Erem		'v'		/* Removal of a suspended generator */

   /*
    * Co-expression events
    */

#define E_Coact		'A'		/* Co-expression activation */
#define E_Coret		'B'		/* Co-expression return */
#define E_Cofail	'D'		/* Co-expression failure */

   /*
    * Procedure events
    */

#define E_Pcall		'C'		/* Procedure call */
#define E_Pfail		'F'		/* Procedure failure */
#define E_Pret		'R'		/* Procedure return */
#define E_Psusp		'S'		/* Procedure suspension */
#define E_Presum	'U'		/* Procedure resumption */
#define E_Prem		'V'		/* Suspended procedure removal */

#define E_Fcall		':'		/* Function call */
#define E_Ffail		'M'		/* Function failure */
#define E_Fret		'P'		/* Function return */
#define E_Fsusp		'W'		/* Function suspension */
#define E_Fresum	'Y'		/* Function resumption */
#define E_Frem		'['		/* Function suspension removal */

#define E_Ocall		'\\'		/* Operator call */
#define E_Ofail		']'		/* Operator failure */
#define E_Oret		'`'		/* Operator return */
#define E_Osusp		'{'		/* Operator suspension */
#define E_Oresum	'}'		/* Operator resumption */
#define E_Orem		'\177'		/* Operator suspension removal */

   /*
    * Garbage collections
    */

#define E_Collect	'G'		/* Garbage collection */
#define E_EndCollect	'\360'		/* End of garbage collection */
#define E_TenureString	'\361'		/* Tenure a string region */
#define E_TenureBlock	'\362'		/* Tenure a block region */

/*
 * Termination Events
 */
#define E_Error		'E'		/* Run-time error */
#define E_Exit		'X'		/* Program exit */

   /*
    * I/O events
    */
#define E_MXevent	'\370'		/* monitor input event */

/*
 * Codes used in event history files.
 */
#define E_Comment	'#'		/* Comment */

#endif					/* EventMon */
