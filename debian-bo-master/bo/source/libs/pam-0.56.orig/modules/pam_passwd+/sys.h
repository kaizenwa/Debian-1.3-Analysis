/*
 * system dependencies
 *
 * what you need to define:
 *
 * BASIC SYSTEM IDENTITY
 *	BSD4_2		for vanilla 4.2 BSD systems
 *	BSD4_3		for vanilla 4.3 BSD systems
 *	SYSV		for vanilla System V systems
 *	SUN		for Sun 3.0 OS on a client
 * If one of these is set, everything else is done automatically.
 * Note: your system must be vanilla for these to work; for example,
 * a Sun is not vanilla 4.3 (or 4.2 for that matter) since it uses
 * a System V password function, fgetpwent().
 *
 * ALTERNATE CAPABIOLITIES
 *	CHFN		if your machine uses the passwd command to allow users
 *			to change their finger information, define this
 *	GETUSERSHELL	if your machine uses the passwd command to allow users
 *			to change their login shell, set this to the function
 *			that returns valid user shells.  The calling sequence
 *			is to pass no arguments, and each call is expected to
 *			return a string with the name of a valid user shell in
 *			it, or NULL meaning end of list.
 * These let you alter other fields of the password file.  Useful.
 *
 * PASSWORD FILE FORMAT
 *	AGE_FIELD	set this if the password structure (see getpwent(3)
 *			in the manual) has the field "pw_age".
 *
 * ALTERNATE PASSWORD FILE
 *	FGETPWENT	define this function to take a FILE pointer to the
 *			password file, and return successive entries in the
 *			password files (or NULL when there are no more)
 *	SETPWFILE	define this to take the name of the password file
 *			as its single argument; it returns nothing, but all
 *			future calls to getpwnam and getpwuid get the entries
 *			from this file and not the default one.
 * Define only one of these (BSD systems use the latter, System V the former).
 * Look in the manual entry for getpwent() to which one to use.  If no such
 * function is given, you're stuck -- you can't use an alternate file.
 * don't define either.
 *
 * SYSTEM LIBRARIES
 *	SYSLOG		set to library to load syslog(3), openlog(3), and
 *			closelog(3) from; if standard C library, just define
 *			it; if not available, don't define it
 *	DBMLIB		DEFINE THIS ONLY IF YOUR PASSWORD FILE IS ALSO STORED
 *			USING DBM FORMAT; OTHERWISE THERE MAY BE PROBLEMS.
 *			set to library to load dbm routines; if standard C
 *			library, just define it; if not available, don't
 *			define it
 * These define libraries where functions may be found.  Note the warning
 * for DBMLIB -- we mean it!
 *
 * SYSTEM TYPES
 *	UID_TYPE	if getuid(2) does not return an int, set this to the
 *			type that getuid(2) returns
 * This is put in to keep lint happy, but mind the types and the data will
 * take care of itself, I always say ...
 *
 * SYSTEM FUNCTIONS
 *	RENAME		define this to be a function that renames files 
 *			atomically; it takes two arguments, the old name and
 *			the new name (in that order); if your system does not
 *			have one, do not define it, and the old password file
 *			will be unlinked and the new one linked to the name of
 *			the old one, leaving a small window of vulnerability if
 *			your system chooses that minute to crash *sigh*;
 *	OPENLOG		define this to have the appropriate arguments; the
 *			calls on 4.2 BSD and 4.3 BSD differ (used only if
 *			SYSLOG is defined)
 *	GETDOMAIN	define this to be a function of two arguments,
 *			char buf[] and int n, where n is the size in chars
 *			of buf[]; on return, buf[] contains the domain name
 *			(ie, the first n characters of it); the call made is
 *			GETDOMAIN(buf, BUFSIZ); if no such function available,
 *			do not define it
 *	GETHOST		define this to be a function of two arguments,
 *			char buf[] and int n, where n is the size in chars
 *			of buf[]; on return, buf[] contains the host name
 *			(ie, the first n characters of it); the call made is
 *			GETHOST(buf, BUFSIZ); if no such function available,
 *			do not define it
 * These functions are system calls on some machines, library functions
 * on other machines, and do not exist on still others.  If GETDOMAIN is
 * not defined, the host name is obtained and everything up to (and including)
 * the first '.' is clobbered.
 *
 * HOST NAME
 *	HOSTNAME	define this if GETHOST is not defined; it should be
 *			the full, DOMAIN NAME of your site (so if you are
 *			working on the host "prandtl", for example, define
 *			HOSTNAME as "prandtl.nas.nasa.gov")
 * If GETHOST is defined, this macro is ignored.
 *
 * DEBUGGING
 *	ALLOWCORE	if set this allows core dumps to be produced; it should
 *			only be used for debugging
 *	ROOTID		if set, this is the numeric UID of root; it is used in
 *			debugging to allow someone other than the superuser
 *			to use a test file other than the default.
 * Useful only for debugging.  NEVER RUN IN PRODUCTION MODE WITH THESE SET --
 * you guess why.
 */

#define GETHOST			gethostname
#define GETDOMAIN		getdomainname
#define OPENLOG(id,log,fac)	openlog(id,log)
#define RENAME(old,new)		rename(old,new)
#define UID_TYPE		int
