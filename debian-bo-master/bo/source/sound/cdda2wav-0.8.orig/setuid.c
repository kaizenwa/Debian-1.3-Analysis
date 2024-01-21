/* Security functions by zblaxell

   If these functions fail, it is because there was an installation error
   or a programming error, and we can't be sure about what privileges
   we do or do not have.  This means we might not be able to recover
   the privileges we need to fix anything that may be broken (e.g. the
   CDDA state of some interface types), and we may in fact do something
   quite dangerous (like write to the WAV file as root).

   In any case, it is unsafe to do anything but exit *now*.  Ideally we'd
   kill -9 our process group too, just to be sure.  Root privileges are not
   something you want floating around at random in user-level applications.

   If any signal handlers or child processes are introduced into this
   program, it will be necessary to call dontneedroot() or neverneedroot()
   on entry, respectively; otherwise, it will be possible to trick
   the program into executing the signal handler or child process with
   root privileges by sending signals at the right time.
 */

#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>

#include "setuid.h"

/* True at return from initsecurity */
static uid_t real_uid = (uid_t) (-1);
static uid_t effective_uid = (uid_t) (-1);
static gid_t real_gid = (gid_t) (-1);
static gid_t effective_gid = (gid_t) (-1);

/* Run this at the beginning of the program to initialize this code and
   to drop privileges before someone uses them to shoot us in the foot.
   Do not pass(go), do not dollars += 200. */

void initsecurity(void)
{
    int leffective_uid;

    alarm(0);			/* can be inherited from parent process */
    real_uid = getuid();
    leffective_uid = geteuid();
    if (real_uid != leffective_uid && leffective_uid != 0) { /* sanity check */
        fprintf(stderr, "Warning: setuid but not to root (uid=%d, euid=%d)\n", real_uid, leffective_uid);
        fprintf(stderr, "Dropping setuid privileges now.\n");
        neverneedroot();
    } else {
        effective_uid = leffective_uid;
    }
    real_gid = getgid();
    effective_gid = getegid();
    dontneedroot();
    dontneedgroup();
}

/* Temporarily gain root privileges. */

void needroot(void)
{
    if (effective_uid) {
	fprintf(stderr, "Fatal error:  require root privilege but not setuid root.\n");
	exit(1);
    }
    if (real_uid == (uid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setreuid(real_uid, effective_uid)) {
	perror("setreuid in needroot()");
	exit(1);
    }
    if (geteuid() != 0) {
	fprintf(stderr, "Fatal error:  did not get root privilege.\n");
	exit(1);
    }
}

/* Temporarily drop root privileges.  */

void dontneedroot(void)
{
    if (effective_uid)
	return;
    if (real_uid == (uid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setreuid(effective_uid, real_uid)) {
	perror("setreuid in dontneedroot()");
	exit(1);
    }
    if (geteuid() != real_uid) {
	fprintf(stderr, "Fatal error:  did not drop root privilege.\n");
	exit(1);
    }
}

/* Permanently drop root privileges.  */

void neverneedroot(void)
{
    if (real_uid == (uid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setreuid(real_uid, real_uid)) {
	perror("setreuid in neverneedroot()");
	exit(1);
    }
    if (geteuid() != real_uid || getuid() != real_uid) {
	fprintf(stderr, "Fatal error:  did not drop root privilege.\n");
	exit(1);
    }
    effective_uid = real_uid;
}

/* Temporarily gain group privileges. */

void needgroup(void)
{
    if (real_gid == (gid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setregid(real_gid, effective_gid)) {
	perror("setregid in needgroup()");
	exit(1);
    }
    if (getegid() != effective_gid) {
	fprintf(stderr, "Fatal error:  did not get group privilege.\n");
	exit(1);
    }
}

/* Temporarily drop group privileges.  */

void dontneedgroup(void)
{
    if (real_gid == (gid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setregid(effective_gid, real_gid)) {
	perror("setregid in dontneedgroup()");
	exit(1);
    }
    if (getegid() != real_gid) {
	fprintf(stderr, "Fatal error:  did not drop group privilege.\n");
	exit(1);
    }
}

/* Permanently drop group privileges.  */

void neverneedgroup(void)
{
    if (real_gid == (gid_t) (-1)) {
	fprintf(stderr, "Fatal error:  initsecurity() not called.\n");
	exit(1);
    }
    if (setregid(real_gid, real_gid)) {
	perror("setregid in neverneedgroup()");
	exit(1);
    }
    if (getegid() != real_gid || getgid() != real_gid) {
	fprintf(stderr, "Fatal error:  did not drop group privilege.\n");
	exit(1);
    }
    effective_gid = real_gid;
}
