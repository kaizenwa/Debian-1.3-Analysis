/*
Copyright (c) 1991, 1992, 1993, 1994 Xerox Corporation.  All Rights Reserved.

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

$Id: io.h,v 1.2 1994/09/01 17:57:08 severson Exp $
*/

extern void		ioTypeInput(Type t);
extern void		ioTypeOutSize(Type t, const char *argName,
				const char *prefix);
extern void		ioObjDiscrimInput(Type t);
extern void		ioObjDiscrimOutSize(Type t, const char *prefix);
extern void		ioArraySpecialElemInput(const char *eName,
				const long length);
extern void		ioArraySpecialElemOutSize(const char *eName,
				const char *argName, const char *prefix,
				const long length);
