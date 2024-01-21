/* $Id: vboxrc.h,v 1.3 1996/07/19 08:53:32 root Exp $ */
 
#ifndef _VBOXRC_H
#define _VBOXRC_H 1

#include "defines.h"

#include <unistd.h>
#include <limits.h>

/** Defines **************************************************************/

#define MAXVBOXRCLINELEN	(256)

/** Variables ************************************************************/

struct DebugModes
{
	char *Name;
	int	Mode;
};

struct UserSection
{
	char	StandardMessage[NAME_MAX + 1];
	int	Recordtime;
	int	Answer;
	int	Record;
	int	PlayMessage;
   int	PlayBeep;
	int	PlayTimeout;
};

/** Prototypes ***********************************************************/

extern FILE		*OpenVBoxRC(uid_t);
extern void		 CloseVBoxRC(void);
extern long		 GetRingsToWait(uid_t);
extern void		 GetNameFromCallerID(uid_t, char *, char *, char *, int);
extern boolean	 GetOwnerSettings(uid_t, char *, char *, struct UserSection *);
extern int		 GetDebugMode(uid_t);
extern void		 GetUserSectionDefaults(struct UserSection *);

#endif /* _VBOXRC_H */
