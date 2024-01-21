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

$Id: manifest.c,v 1.11 1994/11/22 21:58:59 severson Exp $
*/

#include "manifest.h"

const char	fmtEnumImageDict[]	= "imageOf___%s";
const char	fmtEnumLit[]		= "%s__%s";
const char	fmtFileName[]		= "%s.py";
const char	fmtFuncIo[]		= "_%s_%s";
const char	fmtFuncSkel[]		= "_%s__%s";
const char	fmtSkelModuleName[]	= "%s__skel";
const char	fmtUnionDiscrimLit[]	= "%s__%s";

const char	prefixIdlAttribute[]	= "ilu--prefix-idlAttribute-";

const char	prefixFuncEnd[]		= "End";
const char	prefixFuncInput[]	= "Input";
const char	prefixFuncOutput[]	= "Output";
const char	prefixFuncSizeOf[]	= "SizeOf";

const char	nameClassSkel[]		= "IluObjTrue";
const char	nameClassStub[]		= "IluObjSurr";
const char	nameExceptUnimpl[]	= "IluUnimplementedMethodError";
const char	nameFuncCatchExcept[]	= "_CatchException";
const char	nameFuncSendExcept[]	= "_SendException";
const char	nameModuleIlu[]		= "iluRt";
const char	nameVarCall[]		= "_call";
const char	nameVarClass[]		= "_IluClass";
const char	nameVarDiscrim[]	= "_d";
const char	nameVarExceptCode[]	= "_ecode";
const char	nameVarExceptName[]	= "_name";
const char	nameVarExceptValue[]	= "_value";
const char	nameVarIndex[]		= "_i";
const char	nameVarLength[]		= "_length";
const char	nameVarMstid[]		= "mstid";
const char	nameVarResult[]		= "_result";
const char	nameVarSbh[]		= "sbh";
const char	nameVarSelf[]		= "_self";
const char	nameVarSize[]		= "_size";
const char	nameVarValue[]		= "_value";
