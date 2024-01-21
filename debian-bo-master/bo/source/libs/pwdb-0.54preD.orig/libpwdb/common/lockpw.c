
/* passwd/shadow database locking */

#include "../_pwdb_internal.h"

/*
 * lckpwdf - lock the password files
 */

int __pwdb_lckpwdf (void)
{
	int	i;

	/*
	 * We have 15 seconds to lock the whole mess
	 */

	for (i = 0;i < 15;i++)
		if (__pwdb_pw_lock ())
			break;
		else
			sleep (1);

	/*
	 * Did we run out of time?
	 */

	if (i == 15)
		return -1;

	/*
	 * Nope, use any remaining time to lock the shadow password
	 * file.
	 */

	for (;i < 15;i++)
		if (__pwdb_spw_lock ())
			break;
		else
			sleep (1);

	/*
	 * Out of time yet?
	 */

	if (i == 15) {
		__pwdb_pw_unlock ();
		return -1;
	}

	/*
	 * Nope - and both files are now locked.
	 */

	return 0;
}

/*
 * ulckpwdf - unlock the password files
 */

int __pwdb_ulckpwdf (void)
{

	/*
	 * Unlock both files.
	 */

	return (__pwdb_pw_unlock () && __pwdb_spw_unlock ()) ? 0:-1;
}
