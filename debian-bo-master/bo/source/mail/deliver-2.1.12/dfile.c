/* $Id: dfile.c,v 1.5 1993/10/28 16:49:51 chip Exp $
 *
 * Filter destination(s) through delivery file(s).
 *
 * $Log: dfile.c,v $
 * Revision 1.5  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.4  1991/06/20  12:56:02  chip
 * Give type of temp file when reporting an error.
 *
 * Revision 1.3  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.2  1991/05/23  17:23:19  chip
 * Follow RFC822 definition of header syntax.
 * Guard isxxx() macros against negative values.
 *
 * Revision 1.1  1991/05/13  18:36:55  chip
 * Initial revision
 *
 */

#include "deliver.h"
#include <sys/stat.h>

static int glob_dfile();
static char **choose_args();
static void u_dfile();
static int run_dfile();
static int give_temps();

/*----------------------------------------------------------------------
 * Filter all valid destinations through the system delivery file.
 * Return 1 (executed), 0 (not executed), -1 (no attempt made).
 */

static int sys_ac;
static char **sys_av;

static char **
sys_args(pac)
int *pac;
{
    char **fav;
    int fac, a;

    fav = talloc(char *, sys_ac);
    fac = 0;

    for (a = 0; a < sys_ac; ++a)
    {
	char *addr;

	addr = sys_av[a];

	/* Note invalid address(es); report them later. */

	if (!addr_clean(addr))
	    (void) dest(addr, CL_USER, (char *) NULL);

	/* Note non-user address(es); report them later. */

	else if (addr_class(addr) != CL_USER)
	    (void) addr_dest(addr, (CONTEXT *) NULL);

	/* Let the system delivery file handle the rest. */

	else
	    fav[fac++] = addr;
    }

    /*
     * If there were any good names found, let loose the delivery
     * file.  Note the meaning of "good" is "well-formed", not "valid".
     * Thus the system delivery file has control over the handling of
     * all local deliveries, not just those to valid users.
     */

    if (fac <= 0)
    {
	free((char *) fav);
	fav = NULL;
    }

    *pac = fac;
    return fav;
}

int
sys_dfile(ac, av)
int ac;
char **av;
{
    sys_ac = ac;
    sys_av = av;
    return glob_dfile("system", sys_deliver, sys_args, FALSE);
}

/*----------------------------------------------------------------------
 * Filter some undelivered destinations through the post-user
 * delivery file.
 * Return 1 (executed), 0 (not executed), -1 (no attempt made).
 */

static int
post_choose(d)
DEST *d;
{
    if ((d->d_class == CL_USER || d->d_class == CL_UUCP)
	&& (d->d_state == ST_WORKING
	    || (d->d_state == ST_ERROR && d->d_error == E_NSUSER)))
    {
	d->d_state = ST_HOLD;
	return TRUE;
    }

    return FALSE;
}

static char **
post_args(pac)
int *pac;
{
    return choose_args(pac, post_choose);
}

int
post_dfile()
{
    return glob_dfile("post-user", post_deliver, post_args, FALSE);
}

/*----------------------------------------------------------------------
 * Filter broken (but well-formed) destinations through the error
 * delivery file.
 * Return 1 (executed), 0 (not executed), -1 (no attempt made).
 */

static int
err_choose(d)
DEST *d;
{
    return (d->d_state == ST_ERROR);
}

static char **
err_args(pac)
int *pac;
{
    return choose_args(pac, err_choose);
}

int
err_dfile()
{
    return glob_dfile("error", err_deliver, err_args, TRUE);
}

/*----------------------------------------------------------------------
 * Execute a global delivery file given description of delivery file,
 * its path, a function to get arguments, and a boolean to indicate whether
 * a lack of output is a normal condition.
 * Return 1 (executed), 0 (not executed), -1 (no attempt made).
 */

static int
glob_dfile(desc, dfile, args, silent_ok)
char *desc;
char *dfile;
char **(*args)();
int silent_ok;
{
    char **fav;
    char *gdel_path;
    int fac, n;

    /*
     * If the delivery file is missing, forget it.
     */

    gdel_path = relpath(eff_ct->ct_home, dfile);
    n = exists(gdel_path);
    if (verbose)
	message("%s %s delivery file %s\n",
		(n ? "found" : "missing"), desc, gdel_path);
    if (!n)
    {
	free(gdel_path);
	return -1;
    }

    /*
     * If we've been asked not to run delivery files, forget it.
     */

    if (!rundfiles)
    {
	if (verbose)
	    message("%s delivery file disabled\n", desc);
	return -1;
    }

    /*
     * Now we can get the argument list.
     * If the list is empty, that's all she wrote.
     */

    if ((fav = (*args)(&fac)) == NULL)
	return 0;

    /*
     * "Just do it."
     * If we get nothing back from the delivery file,
     * and if silence is not supposed to be permitted,
     * put the message in the "undelivered" mailbox.
     */

    if (run_dfile(eff_ct, gdel_path, fac, fav, (DEST *) NULL) <= 0
	&& !silent_ok)
    {
	if (verbose)
	    message("%s delivery file: no output\n", desc);
	dest_undel(eff_ct->ct_name);
    }

    /*
     * The argument function allocated the arguments; we free them.
     */

    free((char *) fav);

    /*
     * Return "done".
     */

    free(gdel_path);
    return 1;
}

/*----------------------------------------------------------------------
 * Generate a delivery file argument list.
 * We do this by getting an array of all destinations,
 * then keeping only the ones we want.
 */

static char **
choose_args(pac, choose)
int *pac;
int (*choose) ();
{
    DEST *d;
    char **fav;
    int count, fac;

    if ((count = dest_count()) <= 0)
	return NULL;

    fav = talloc(char *, count);
    fac = 0;

    for (d = first_dest(); d; d = next_dest(d))
    {
	if ((*choose) (d))
	    fav[fac++] = d->d_name;
    }

    if (fac <= 0)
    {
	free((char *) fav);
	fav = NULL;
    }

    *pac = fac;
    return fav;
}

/*----------------------------------------------------------------------
 * Filter all user destinations through their local delivery files.
 * Return 1 (some executed), 0 (none executed), -1 (no attempt made).
 */

int
user_dfiles()
{
    DEST *d;
    int nfound, ret;

    /*
     * If we've been asked not to run delivery files, forget it.
     */

    if (!rundfiles)
    {
	if (verbose)
	    message("user delivery files disabled\n");
	return -1;
    }

    /*
     * Continue to loop through all addresses until no destination
     * that needs expanding can be found.
     */

    ret = 0;
    do
    {
	nfound = 0;
	for (d = first_dest(); d; d = next_dest(d))
	{
	    if (d->d_class == CL_USER
		&& d->d_state == ST_WORKING
		&& !d->d_dfdone)
	    {
		u_dfile(d);
		d->d_dfdone = TRUE;
		ret = 1;
	    }
	}
    } while (nfound > 0);

    return ret;
}

/*----------------------------------------------------------------------
 * Run the user delivery file (if any) for the specified destination.
 */

static void
u_dfile(d)
DEST *d;
{
    struct stat st;
    CONTEXT *ct;
    char *udel_path;
    int n;

    if ((ct = name_context(d->d_name)) == NULL)
    {
	dest_err(d, E_CTLOST);
	return;
    }

    /*
     * If user's home directory is missing, forget it.
     * If user's home directory is writable to the world,
     * executing the delivery file would allow a security breach!
     * Thanks to Jon Zeeff for this hint...
     */

    if (stat(ct->ct_home, &st) == -1
	|| (st.st_mode & S_IFMT) != S_IFDIR)
    {
	if (verbose)
	    message("user %s: home directory %s is missing!\n",
		    ct->ct_name, ct->ct_home);
	return;
    }

    if (st.st_mode & 02)
    {
	if (verbose)
	    message("user %s: home directory is writable to the world!\n",
		    ct->ct_name);
	return;
    }

    /*
     * If there is no delivery file to execute, just return.
     */

    udel_path = relpath(ct->ct_home, user_deliver);
    n = exists(udel_path);
    if (verbose)
	message("user %s: %s delivery file %s\n",
		d->d_name, (n ? "found" : "missing"), udel_path);
    if (!n)
    {
	free(udel_path);
	return;
    }

    /*
     * Put this destination on hold.
     * It will be ignored unless it's named by a delivery file.
     */

    d->d_state = ST_HOLD;

    /*
     * Time to run the file!
     * If we get nothing back from the user delivery file,
     * put the message in the user's "undelivered" mailbox--
     * or ours, depending on the kind of failure.
     */

    n = run_dfile(ct, udel_path, 1, &d->d_name, d);
    if (n <= 0)
    {
	if (verbose)
	    message("u_dfile: no output\n");
	dest_undel((n == 0 ? ct : eff_ct)->ct_name);
    }

    free(udel_path);
}

/*----------------------------------------------------------------------
 * Execute a delivery file (global or user).
 * Return the count of valid destinations we got back from it.
 * If delivering to MBX_UNDEL is possible, errors return zero.
 * Otherwise, errors return -1.
 */

static int
run_dfile(ct, dfile, fac, fav, d)
CONTEXT *ct;
char *dfile;
int fac;
char **fav;
DEST *d;
{
    FILE *fp;
    char **av;
    int ac, a, fd, linecount;
    static char buf[BUFSIZ];

    if (!ct)
	return -1;

    if (!ok_context(eff_uid, real_uid, real_gid, ct))
    {
	if (d)
	    dest_err(d, E_CTPERM);
	else
	    message("No permissions to run as %s\n", ct->ct_name);

	return -1;
    }

    /*
     * We trust the superuser not to stomp on the temp files.
     * Other users get copies of the temp files.
     */

    if (ct->ct_uid == 0)
    {
	/* We trust the superuser; don't bother copying again */

	if (dont_copy() < 0)
	    return -1;
    }
    else
    {
	/* Copy the temp files again */

	if (copy_again() < 0)
	    return -1;

	/* Allow the given user to own and read the copies */

	if (give_temps(ct) < 0)
	    return -1;
    }

    /* Produce the arguments in the form we need. */

    av = talloc(char *, fac + 4);
    ac = 0;

    /* Process the "#!" hack. */

    if ((fd = open(dfile, O_RDONLY)) != -1)
    {
	char *p;
	int rd;
	static char hashbang[64];	/* arbitrary */

	rd = read(fd, hashbang, sizeof(hashbang) - 1);
	(void) close(fd);
	hashbang[rd > 0 ? rd : 0] = 0;

	if ((p = strchr(hashbang, '\n')) != NULL
	    && hashbang[0] == '#'
	    && hashbang[1] == '!')
	{
	    *p = 0;

	    /* Interpreter. */

	    p = hashbang + 2;
	    while (isspace(*p & 0xFF))
		++p;
	    av[ac++] = p;
	    while (*p && !isspace(*p & 0xFF))
		++p;
	    if (*p)
		*p++ = 0;

	    /* Only one argument; sorry. */

	    while (isspace(*p & 0xFF))
		++p;
	    if (*p)
		av[ac++] = p;
	}
    }

    /*
     * If no "#!" found, use the default shell.
     * Then add the delivery file, address(es), and a NULL.
     */

    if (ac == 0)
	av[ac++] = shell;
    av[ac++] = dfile;
    for (a = 0; a < fac; ++a)
	av[ac++] = fav[a];
    av[ac] = NULL;

    /* Here we go! */

    if (verbose)
	message("Processing delivery file as %s\n", ct->ct_name);

    fp = ct_fopenv(ct, av[0], av, "r");

    /* We don't need the argument vector any more. */

    free((char *) av);

    /* If something went wrong, bail out now. */

    if (fp == NULL)
    {
	error("can't execute delivery file as %s", ct->ct_name);
	return 0;
    }

    /*
     * Read the standard output of the delivery file.
     */

    linecount = 0;

    while (fgets(buf, GETSIZE(buf), fp) != NULL)
    {
	DEST *nd;
	char *p;

	if ((p = strchr(buf, '\n')) != NULL)
	    *p = 0;
	else
	{
	    int c;

	    while ((c = fgetc(fp)) != '\n' && c != EOF)
		;		/* keep reading */

	    error("invalid line from delivery file: '%s'", buf);
	    continue;
	}

	/* Debugging message: display input line. */

	if (verbose)
	    message("\t'%s'\n", buf);

	/* Okay-to-drop directive is a special case. */

	if (strcmp(buf, DFILE_DROP) == 0)
	{
	    if (verbose)
		message("\tDelivery file says OK to drop\n");
	    ++linecount;
	    continue;
	}

	/* Parse destination; if none, look for next line. */

	if ((nd = addr_dest(buf, ct)) == NULL)
	    continue;

	/* We got some output; remember that. */

	++linecount;

	/* If destination was on hold, it's not so anymore. */

	if (nd->d_state == ST_HOLD)
	    nd->d_state = ST_WORKING;
    }

    (void) ct_fclose(fp);

    return linecount;
}

/*----------------------------------------------------------------------
 * Make the temp files readable in the given context.
 * This is needed because the temps are readable by owner only.
 */

static int
give_temps(ct)
CONTEXT *ct;
{
    int err, t;

    if (!ct)
	return -1;

    err = 0;
    for (t = T_HDRCOPY; t <= T_BODYCOPY; ++t)
    {
	if (chmod(tfile[t], 0600) == -1)
	{
	    syserr("can't chmod %s file %s", ttype[t], tfile[t]);
	    ++err;
	}
	if (chown(tfile[t], ct->ct_uid, ct->ct_gid) == -1)
	{
	    syserr("can't chown %s file %s to %d/%d",
		   ttype[t], tfile[t], ct->ct_uid, ct->ct_gid);
	    ++err;
	}
    }

    return err ? -1 : 0;
}
