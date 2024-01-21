/*----------------------------------------------------------
#
#	NewsWatcher	- Macintosh NNTP Client Application
#
#	Written by Steven Falkenburg
#	©1990 Apple Computer, Inc.
#
#-----------------------------------------------------------
#
#	util.c
#
#	This module contains miscellaneous utility routines.
#
#-----------------------------------------------------------*/

#pragma load "MacHeaders"

#include <string.h>

#include "MacTCPCommonTypes.h"

#include "macglob.h"
#include "macutil.h"


#define kButtonFrameSize	3	/* button frameUs pen size */
#define kButtonFrameInset	4	/* inset rectangle adjustment around button */

static CStr255 gStatusMsg;
static OSErr gMemError = noErr;
static Boolean gDlgReturnIsOK = true;
static Boolean gDlgHasCancelButton = true;

Boolean		gDone = true;				/* flag set true upon program termination */
Boolean 	gCancel = false;			/* flag set when user cancels an action */

Handle		gLifeBoat;					/* lifeboat memory -- de-allocated when 
										   memory gets low */
Boolean		gSinking = true;			/* flag set after lifeboat has been 
										   jettisoned */
Boolean 	gOutOfMemory = false;		/* flag set when out of memory - and luck */


/*	ErrorMessage issues an error message alert.	*/

void ErrorMessage (char *msg)
{
	printf("Error: %s\n",msg);	
}

/*
	UnexpectedErrorMessage issues an "unexpected error" alert.
*/

void UnexpectedErrorMessage (OSErr err)
{
#ifdef BROKEN
	Str255 errNumStr;
	DialogPtr theDlg;
	short item;

	if (err == -1 || err == noErr) return;
	if (err == connectionClosing || err == connectionDoesntExist ||
		err == connectionTerminated) {
		ErrorMessage("Lost connection to server.");
		return;
	}
	NumToString(err,errNumStr);
	ParamText(errNumStr, "\p", "\p", "\p");
	theDlg = MyGetNewDialog(kUnexpectedErrDlg);
	SysBeep(1);
	MyModalDialog((ModalFilterProcPtr)DialogFilter,&item,false,true);
	DisposDialog(theDlg);
#endif
	printf("unexpected error: %d\n");
}



/*	MyIOCheck can be called to display the result of a routine returning
	an OSErr.  If the value in err is zero, the routine simply terminates.
*/

OSErr MyIOCheck (OSErr err)
{
	if (err != noErr) {
		UnexpectedErrorMessage(err);
	}
	return err;
}


/*	LowMemory is called when the program runs out of useable memory.
	If this is the first time this has happened, the program de-allocates
	lifeboat memory which was allocated when the program was launched.
	Otherwise, the user had better quit.
*/

static Boolean LowMemory (void)
{
	Boolean result;
	
	if (MyMemErr() != memFullErr) {
		MyIOCheck(MyMemErr());
		return false;
	}
		
	if (gSinking) {
		result = false;
		gOutOfMemory = true;
		ErrorMessage("You have run out of memory");
	}
	else {
		HUnlock(gLifeBoat);
		DisposHandle(gLifeBoat);
		gSinking = true;
		result = true;
		ErrorMessage("Memory is getting low.  Some operations may fail.");
	}
	return result;
}


/*	This is a wrapper for the NewPtr routine which automatically checks
	the result of the call and takes appropriate action.
*/

Ptr MyNewPtr (Size byteCount)
{
	Ptr thePtr;
	
	thePtr = NewPtrClear(byteCount);
	if ((gMemError = MemError()) != noErr) {
		if (LowMemory())
			thePtr = MyNewPtr(byteCount);
		else
			thePtr = nil;
	}
	return thePtr;
}


/*	This is a wrapper for the NewHandle routine which automatically checks
	the result of the call and takes appropriate action.
*/

Handle MyNewHandle (Size byteCount)
{
	Handle theHndl;
	
	theHndl = NewHandleClear(byteCount);
	if ((gMemError = MemError()) != noErr) {
		if (LowMemory())
			theHndl = MyNewHandle(byteCount);
		else
			theHndl = nil;
	}
	return theHndl;
}

/*	This is a wrapper for the SetHandleSize routine which automatically checks
	the result of the call and takes appropriate action.
*/

void MySetHandleSize (Handle h, Size newSize)
{
	long oldSize;

	oldSize = GetHandleSize(h);
	SetHandleSize(h,newSize);
	if ((gMemError = MemError()) != noErr) {
		if (LowMemory())
			MySetHandleSize(h,newSize);
	} else if (oldSize < newSize) {
		memset(*h+oldSize, 0, newSize-oldSize);
	}
}


/*	This is a wrapper for the HandToHand routine which automatically checks
	the result of the call and takes appropriate action.
*/

OSErr MyHandToHand (Handle *theHndl)
{
	Handle oldHndl;
	OSErr result;
	
	oldHndl = *theHndl;
	result = gMemError = HandToHand(theHndl);
	if (result != noErr) {
		*theHndl = oldHndl;
		if (LowMemory())
			MyHandToHand(theHndl);
	}
	return result;
}


/*	This is a wrapper for the DisposPtr routine which automatically checks
	the result of the call and takes appropriate action.
	NT: extra errorchecking for stability incorporated
*/

OSErr MyDisposPtr (Ptr thePtr)
{
	if(thePtr != nil)
	{
		DisposPtr(thePtr);
		gMemError = MemError();
	}
	else
	{
		gMemError = noErr;
	}
	return MyIOCheck(gMemError);
}


/*	This is a wrapper for the DisposHandle routine which automatically checks
	the result of the call and takes appropriate action.
	NT: extra errorchecking for stability incorporated
*/

OSErr MyDisposHandle (Handle theHndl)
{
	if(theHndl != nil)
	{
		DisposHandle(theHndl);
		gMemError = MemError();
	}
	else
	{
		gMemError = noErr;
	}
	return MyIOCheck(gMemError);
}


/*	This is a wrapper for the MemError routine which automatically checks
	the result of the call and takes appropriate action.
*/

OSErr MyMemErr (void)
{
	return gMemError;
}


/*	IsAppWindow returns true if the window belongs to the application
*/

Boolean IsAppWindow (WindowPtr wind)
{
	short		windowKind;
	
	if (wind == nil)
		return(false);
	else {
		windowKind = ((WindowPeek) wind)->windowKind;
		return ((windowKind >= userKind) || (windowKind == dialogKind));
	}
}


/*	IsDAWindow returns true if the window is a DA window
*/

Boolean IsDAWindow (WindowPtr wind)
{
	if ( wind == nil )
		return(false);
	else	/* DA windows have negative windowKinds */
		return (((WindowPeek) wind)->windowKind < 0);
}

/*	The InitCursorCtl, SpinCursor, strcasecmp and strncasecmp functions 
	are missing in Think, so we include them here	*/

Handle gAcur = nil;

pascal void InitCursorCtl (Handle id)
{
	short NoFrames, i, CursId;
	CursHandle	TheCursHndl;

	if (id == nil) {
		gAcur = (Handle) GetResource('acur',0);
		if(gAcur == nil) return;
		HLock(gAcur);
		NoFrames = ((short *)(*gAcur))[0];
		for(i = 0; i < NoFrames; i++) {
			CursId = ((short *)(*gAcur))[(2*i)+2];
			TheCursHndl = GetCursor(CursId);
			((CursHandle *)(*gAcur))[i+1] = TheCursHndl;
			HLock((Handle)TheCursHndl);
		}
	} else {
		gAcur = id;
		HLock(gAcur);
		NoFrames = ((short *)(*gAcur))[0];
		for(i = 0; i < NoFrames; i++) {
			CursId = ((short *)(*gAcur))[(2*i)+2];
			TheCursHndl = GetCursor(CursId);
			((CursHandle *)(*gAcur))[i+1] = TheCursHndl;
			HLock((Handle)TheCursHndl);
		}
	}
	((short *)(*gAcur))[1] = 0;
	DetachResource(gAcur);
}


pascal void SpinCursor (short num)
{
	short	NoFrames, CurrentFrame, CurrentCounter;
	Cursor	CurrentCursor;

	if(gAcur == nil) InitCursorCtl(nil);
	NoFrames = ((short *)(*gAcur))[0];
	CurrentCounter = ((((short *)(*gAcur))[1] + num) % (NoFrames * 32));
	((short *)(*gAcur))[1] = CurrentCounter;
	CurrentFrame = CurrentCounter / 32;
	CurrentCursor = **(((CursHandle *)(*gAcur))[CurrentFrame+1]);
	SetCursor(&CurrentCursor);
}


/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef u_char
	#define u_char  unsigned char
#endif

/*
 * This array is designed for mapping upper and lower case letter
 * together for a case independent comparison.  The mappings are
 * based upon ascii character sequences.
 */
static u_char charmap[] = {
	'\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
	'\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
	'\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
	'\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
	'\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
	'\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
	'\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
	'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
	'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
	'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
	'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
	'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
	'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
	'\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};


short strcasecmp (char *s1, char *s2)
{
	register u_char	*cm = charmap,
			*us1 = (u_char *)s1,
			*us2 = (u_char *)s2;

	while (cm[*us1] == cm[*us2++])
		if (*us1++ == 0)
			return(0);
	return(cm[*us1] - cm[*--us2]);
}


short strncasecmp (char *s1, char *s2, register short n)
{
	register u_char	*cm = charmap,
			*us1 = (u_char *)s1,
			*us2 = (u_char *)s2;

	while (--n >= 0 && cm[*us1] == cm[*us2++])
		if (*us1++ == 0)
			return(0);
	return(n < 0 ? 0 : cm[*us1] - cm[*--us2]);
}
