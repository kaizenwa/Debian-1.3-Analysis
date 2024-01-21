/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.01 -- module username.c				     */
/*									     */
/*	Tries to compute a nice user- and hostname (e-mail address).	     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	changed October 1995						     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include "xsok.h"

#ifdef BSD_NETKIT
#include <netdb.h>
#endif

char username[256];

void buildusername(const char *name) {
    if (name) {
	if (strlen(name) > 255) {
	    strncpy(username, name, 255);
	    username[256] = '\0';
	} else
	    strcpy(username, name);
    } else {
	struct passwd *pp;
	const char *realname, *loginname;
	char fqdn[256];
	struct utsname buf;
#ifdef BSD_NETKIT
	struct hostent *hp;
#endif
	/* getlogin() fails when xsok is called from the fvwm window
	   manager menu (why?) and cuserid() has disappeared in POSIX
	   1990. We use getuid() now. */
	if ((pp = getpwuid(getuid()))) {
	    if (pp->pw_gecos && strchr(pp->pw_gecos, ','))
		*strchr(pp->pw_gecos, ',') = '\0';
	    realname = pp->pw_gecos;
	    loginname = pp->pw_name;
	} else {
	    /* unable to obtain passwd entry */
	    realname = NULL;
	    loginname = "unknown";
	}
	if (uname(&buf))
	    strcpy(buf.nodename, "unknown");
#ifdef BSD_NETKIT
	if ((hp = gethostbyname(buf.nodename)))
	    strcpy(fqdn, hp->h_name);
	else
#endif
	    sprintf(fqdn, "%s.(unknown)", buf.nodename);

	if (realname)
	    sprintf(username, "%s (%s@%s)", realname, loginname, fqdn);
	else
	    sprintf(username, "%s@%s", loginname, fqdn);
    }
}

#ifdef TESTING
int main(int argc, char *argv[]) {
    if (argc == 3 && !strcmp(argv[1], "-u"))
	buildusername(argv[2]);
    else
	buildusername(NULL);
    printf( /* "Automatically generated username is\n" */
	   "\"%s\"\n", username);
    return 0;
}
#endif
