// $Id: be_globals.hh,v 2.0 1994/06/28 22:07:30 severson Exp $

/*
 *======================================================================
 *
 * Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  
 *
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the above
 * copyright notice of Xerox Corporation and this paragraph.  Any
 * distribution of this software must comply with all applicable United
 * States export control laws.  This software is made available AS IS,
 * and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 * INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 * PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 * THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 * CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 * XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 *======================================================================
 */

struct BE_Options
{
	idl_bool	useImports;
	idl_bool	islInterfacePerTopIdlModule;
	idl_bool	dump;
};

class BE_Imports;

struct BE_Errors
{
	idl_bool	typeAny;
	idl_bool	typeObject;
};

struct BE_Globals
{
	BE_Options	opts;
	int		anonymousTypeIndex;
	AST_Module *	topModule;
	BE_Imports *	imports;
	int		nErrors;
	BE_Errors	errors;
};

extern BE_Globals	beGlobals;
