/*
 * $Id$
 *
 * This is a POSIX interface to libpwdb. It defines the two
 * getpw... functions as simple interfaces to the pwdb_locate()
 * functions.
 */

#include <stdio.h>
#include <pwdb/pwdb_public.h>

#include <pwdb/_pwdb_macros.h>

#define DEFAULT_DIR        ""
#define DEFAULT_SHELL       ""

static struct passwd *transcribe(const char *user, int uid)
{
    static int called_once=0;
    static struct passwd _pwd;
    static struct passwd pwd;
    const struct pwdb_entry
	*pwe_user=NULL,
	*pwe_uid=NULL,
	*pwe_gid=NULL,
	*pwe_dir=NULL,
	*pwe_shell=NULL;
    const struct pwdb *pw=NULL;
    int retval;

    D(("called."));

    if (user == PWDB_NAME_UNKNOWN && uid == PWDB_ID_UNKNOWN)
	return NULL;

    /* identify the user */

    D(("locating %s, %d", user, uid));
    retval = pwdb_locate("user", PWDB_DEFAULT, user, uid, &pw);
    if (retval != PWDB_SUCCESS || pw == NULL) {
	D(("could not find user/uid"));
	return NULL;
    }

    /* Forget last call's values */

    D(("need to drop old?"));
    if (called_once) {
	D(("droping old pwd"));
	pwd.pw_name  = _pwdb_delete_string(pwd.pw_name);
	pwd.pw_dir   = _pwdb_delete_string(pwd.pw_dir);
	pwd.pw_shell = _pwdb_delete_string(pwd.pw_shell);
	called_once = 0;
    }

    /*
     * obtain all the info about the user - if they have a name, a uid
     * and a gid, then we set defaults for the other bits.
     */

    D(("getting entries"));
    if (  pwdb_get_entry(pw, "user", &pwe_user) != PWDB_SUCCESS
	 || pwdb_get_entry(pw, "uid", &pwe_uid) != PWDB_SUCCESS
	 || pwdb_get_entry(pw, "gid", &pwe_gid) != PWDB_SUCCESS ) {
	pwdb_entry_delete(&pwe_user);
	pwdb_entry_delete(&pwe_uid);
	pwdb_entry_delete(&pwe_gid);

	return NULL;
    }

    _pwd.pw_uid = * (const uid_t *) pwe_uid->value;
    pwdb_entry_delete(&pwe_uid);
    _pwd.pw_gid = * (const gid_t *) pwe_gid->value;
    pwdb_entry_delete(&pwe_gid);
    _pwd.pw_name = _pwdb_dup_string((const char *)pwe_user->value);
    pwdb_entry_delete(&pwe_user);

    if (_pwd.pw_name == NULL) {
	return NULL;
    }

    /* We're committed to returning something now */

    D(("identify remaining stuff"));
    /* find a home for the user */
    if ( pwdb_get_entry(pw, "dir", &pwe_dir) != PWDB_SUCCESS ) {
	_pwd.pw_dir = _pwdb_dup_string(DEFAULT_DIR);
    } else {
	_pwd.pw_dir = _pwdb_dup_string((const char *)pwe_dir->value);
	pwdb_entry_delete(&pwe_dir);
    }

    /* find a shell for the user */
    if ( pwdb_get_entry(pw, "shell", &pwe_shell) != PWDB_SUCCESS ) {
	_pwd.pw_shell = _pwdb_dup_string(DEFAULT_SHELL);
    } else {
	_pwd.pw_shell = _pwdb_dup_string((const char *)pwe_shell->value);
	pwdb_entry_delete(&pwe_shell);
    }

    called_once = 1;

    if (_pwd.pw_shell == NULL || _pwd.pw_dir == NULL) {
	return NULL;
    }

    /*
     * return copy - will want to free() text next call so keep
     * original because it references the actual memory and will not
     * be overwritten by the user. POSIX should have required (const
     * char *) items here, to avoid trouble from the user attempting
     * to overwrite these entries. - AGM.
     */

    D(("copying"));
    memcpy(&pwd, &_pwd, sizeof(pwd));
    return (&pwd);
}

struct passwd *pwdb_posix_getpwnam(const char *user)
{
    struct passwd *user_entry;

    pwdb_start();                               /* initialize pwdb */
    user_entry = transcribe(user, PWDB_ID_UNKNOWN);
    pwdb_end();                                 /* terminate pwdb */
    return user_entry;
}

struct passwd *pwdb_posix_getpwuid(uid_t uid)
{
    struct passwd *uid_entry;

    pwdb_start();                               /* initialize pwdb */
    uid_entry = transcribe(PWDB_NAME_UNKNOWN, (int) uid);
    pwdb_end();                                 /* terminate pwdb */
    return uid_entry;
}

/*
 * $Log$
 */
