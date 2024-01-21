/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include "domain.h"
#include "resource.h"
#include "widgets.h"
#include <pwd.h>

#include "sysdeps.h"

static void fix_domain_name(void)
{
#ifdef DOMAIN_NAME
    static char	dn[] = DOMAIN_NAME;
    char	buffer[1024];
    char	*c;

    if (dn[0] == '/') {
	FILE	*fp;

	fp = fopen(dn, "r");
	if (!fp)
	    perror(dn);
	else {
	    c = fgets(buffer, sizeof buffer - 1, fp);
	    fclose(fp);
	    if (c) {
		buffer[sizeof buffer - 1] = '\0';
		c = strchr(buffer, '\n');
		if (c)
		    *c = '\0';
		if (strchr(buffer, '.')) {
		    global.domain_name = XtNewString(buffer);
		    return;
		}
	    }
	}
    } else if (strchr(dn, '.')) {
	global.domain_name = XtNewString(dn);
	return;
    }
#endif

    global.domain_name = get_mailhostname();

    /* else global.domain_name == NULL, and posting will not be allowed */
}

static void fix_user_id(void)
{
    struct passwd	*pw;

    pw = getpwuid(getuid());

    if (pw) {
	if (pw->pw_name)
	    global.user_id = XtNewString(pw->pw_name);

	if (!getenv("NAME") && pw->pw_gecos) {
	    char	*c = strchr(pw->pw_gecos, ',');

	    if (c)
		*c = '\0';
	    res_set_pw_name(pw->pw_gecos);
	}
    }
}

void fix_domain_stuff()
{
    fix_domain_name();
    fix_user_id();

    if (!global.domain_name)
	fputs("knews: Couldn't determine domain name. "
	      "Posting will not be possible.\n", stderr);
    else if (!global.user_id)
	fputs("knews: Couldn't determain user id. "
	      "Posting will not be possible.\n", stderr);

    if (!global.mail_name || global.mail_name[0] == ' ')
	global.mail_name = global.user_id;
}
