/*
 * blt.h --
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#ifndef _BLT_H
#define _BLT_H

#ifndef _ANSI_ARGS_
#   define _ANSI_ARGS_(x)       ()
#endif

#define BLT_MAJOR_VERSION 	2
#define BLT_MINOR_VERSION 	1

typedef enum {
    BLT_VECTOR_NOTIFY_UPDATE=1, /* The vector's values has been updated */
    BLT_VECTOR_NOTIFY_DESTROY=2	/* The vector has been destroyed and the client
				 * should no longer use its data (calling 
				 * Blt_FreeVectorId) */
} Blt_VectorNotify;

typedef void (Blt_VectorChangedProc) _ANSI_ARGS_((Tcl_Interp *interp,
	ClientData clientData, Blt_VectorNotify notify));

typedef struct Blt_VectorId *Blt_VectorId;
typedef struct {
    double *valueArr;		/* Array of values (possibly malloc-ed) */
    int numValues;		/* Number of values in the array */
    int arraySize;		/* Size (in values) of the allocated space */
    double min, max;		/* Minimum and maximum values in the vector */
    int reserved;		/* Reserved for future use */
} Blt_Vector;

extern Blt_VectorId Blt_AllocVectorId _ANSI_ARGS_((Tcl_Interp *interp, 
	char *vecName));
extern void Blt_SetVectorChangedProc _ANSI_ARGS_((Blt_VectorId clientId,
	Blt_VectorChangedProc * proc, ClientData clientData));
extern void Blt_FreeVectorId _ANSI_ARGS_((Blt_VectorId clientId));
extern int Blt_GetVectorById _ANSI_ARGS_((Tcl_Interp *interp,
	Blt_VectorId clientId, Blt_Vector *vecPtr));
extern char *Blt_NameOfVectorId _ANSI_ARGS_((Blt_VectorId clientId));
extern int Blt_VectorNotifyPending _ANSI_ARGS_((Blt_VectorId clientId));

extern int Blt_CreateVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	int size, Blt_Vector *vecPtr));
extern int Blt_GetVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	Blt_Vector *vecPtr));
extern int Blt_VectorExists _ANSI_ARGS_((Tcl_Interp *interp, char *vecName));
extern int Blt_ResetVector _ANSI_ARGS_((Tcl_Interp *interp,  char *vecName,
	Blt_Vector *vecPtr, Tcl_FreeProc *freeProc));
extern int Blt_ResizeVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName,
	int newSize));
extern int Blt_DeleteVector _ANSI_ARGS_((Tcl_Interp *interp, char *vecName));

extern int Blt_Init _ANSI_ARGS_((Tcl_Interp *interp));


#endif /*_BLT_H*/
