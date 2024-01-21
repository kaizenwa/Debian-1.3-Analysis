/* $Id: lock.h,v 1.1 1996/06/27 15:58:07 root Exp $ */

#ifndef _VBOX_LOCK_H
#define _VBOX_LOCK_H 1

#include "defines.h"

extern boolean LockPID(void);
extern boolean LockDevice(void);
extern boolean Lock(int, int);
extern void		Unlock(int);
extern void		UnlockPID(void);
extern void		UnlockDevice(void);

#endif /* _VBOX_LOCK_H */
