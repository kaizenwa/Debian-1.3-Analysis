
#ifndef HTSCRIPT_H
#define HTSCRIPT_H

#include "HTUtils.h"

#ifdef SHORT_NAMES
#define HTCallSc	HTCallScript
#endif /*SHORT_NAMES*/

/* PUBLIC							HTCallScript()
**		CALL A SCRIPT AND SEND RESULTS BACK TO CLIENT
** ON ENTRY:
**	req		the request.
**	req->script	script to call.
**	req->script_pathinfo
**			path info after script name part in URL.
**	req->arg_keywords
**			search keywords/form fields
**
**				/htbin/foo/bar/x/y
**
**			is called as:
**
**			    <HTBinDir>/foo /bar/x/y keywords...
**
**			and:
**				/htbin/foo
**
**			is called as:
**
**			    <HTBinDir>/foo '' keywords...
**
** ON EXIT:
**	returns		HT_LOADED on success.
*/
PUBLIC int HTCallScript PARAMS((HTRequest *	req));
#endif
