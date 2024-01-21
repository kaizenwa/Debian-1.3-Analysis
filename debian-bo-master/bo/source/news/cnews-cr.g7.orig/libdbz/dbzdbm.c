/*
 * dbm emulation on top of dbz
 */

#include <dbz.h>

/*
 - dbminit - open a database, creating it (using defaults) if necessary
 */
int 				/* 0 success, -1 failure */
dbminit(name)
char *name;
{
	return(dbzdbminit(name));
}

/*
 - dbmclose - close a database
 */
int
dbmclose()
{
	return(dbzdbmclose());
}

/*
 - fetch - get an entry from the database
 */
datum				/* dptr NULL, dsize 0 means failure */
fetch(key)
datum key;
{
	return(dbzdbmfetch(key));
}

/*
 - store - add an entry to the database
 */
int				/* 0 success, -1 failure */
store(key, data)
datum key;
datum data;
{
	return(dbzdbmstore(key, data));
}
