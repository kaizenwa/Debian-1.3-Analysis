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

$Id: util.h,v 1.4 1996/05/29 19:10:29 janssen Exp $
*/

typedef iluparser_EnumProc	EnumProc;

extern char *		programName;
extern Interface	currentIfc;
extern boolean		generatingSkeleton;

extern void		sysFatal(const char *);
extern void		fatal(const char *, ...);

extern boolean		isPrefixOf(const char *prefix, const char *base);

extern char *		booleanImage(int value);
extern TypeDescription	baseTypeDescription(Type t);
extern char *		simpleTypeName(Type t);
extern char *		arraySpecialElemTypeName(Type t);
extern char *		sequenceSpecialElemTypeName(Type t);
extern int		methodResultCount(Procedure m);

extern char *		sol;
extern void		indent(int levelDelta);
extern void		newline(void);

extern void		printBanner(const char *part, Interface ifc);
extern void		printImportIfc(const char *ifcName, boolean skelToo);
extern void		printImportTable(void);
extern void		printArgList(list argList, int nPrevArgs);
extern void		printClassVarName(Type t, const char *varName);
extern void		printExceptionName(Exception e);
extern void		printTypeIoFuncName(Type t, const char *prefix);
