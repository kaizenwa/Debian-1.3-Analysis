
#ifndef HTLOG_H
#define HTLOG_H

#include "HTUtils.h"
#include "HTAccess.h"

PUBLIC BOOL HTLog_openAll NOPARAMS;
PUBLIC void HTLog_closeAll NOPARAMS;
PUBLIC void HTLog_access PARAMS((HTRequest *	req));
PUBLIC void HTLog_error PARAMS((CONST char * msg));
PUBLIC void HTLog_error2 PARAMS((CONST char *	msg,
				 CONST char *	param));
PUBLIC void HTLog_errorN PARAMS((CONST char *	msg,
				 int		num));
#endif
