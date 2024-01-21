/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	Reply address rewriting.
 */

#include "config.h"

#define TRACE


#ifdef HAVE_ROUTING

int
reroute(route, address)
char *route, *address;
{
    char *name, *atpos;
    register char *sp = NULL;
    register c = 0;

    if ((atpos = strchr(address, '@'))) {
	name = atpos;

	while (--name >= address)
	    if (isspace(*name) || *name == '<') {
		name++;
		break;
	    }
	if (name < address) name++;

	for (sp = atpos; (c = *sp); sp++)
	    if (isspace(c) || c == '>') break;

	*sp = NUL;
	strcpy(route, name);
	*sp = c;
    } else
	strcpy(route, address);
    return 1;
}

#else


#ifdef TRACE
FILE *route_trace = NULL;
#endif
static char cmdc;	/* we need this for trace output */


static char *cstreq(string, match)
char *string, *match;
{
    register char *s1, *s2;
    s1 = string;

next_part:
    s2 = match;

    while (isspace(*s1) || *s1 == ',') s1++;

    while (*s2) {
	if (*s1 == NUL || isspace(*s1)) return NULL;
	if (*s1 == ',') goto next_part;
	if (toupper(*s1) != toupper(*s2)) break;
	s1++, s2++;
    }

    if (*s2 == NUL && (*s1 == NUL || isspace(*s1) || *s1 == ',')) {
	if (*s1 == ',') while (*s1 && !isspace(*s1)) s1++;
#ifdef TRACE
	if (route_trace)
	    fprintf(route_trace, "/%c %s=%s -> %s", cmdc, string, match, s1);
#endif
	return s1;
    }

    while (*s1 && !isspace(*s1)) {
	if (*s1 == ',') goto next_part;
	s1++;
    }

    return NULL;
}


static char *cstrcpy(s1, s2)
register char *s1, *s2;
{
	while (*s2 && isspace(*s2)) s2++;
	while (*s2 && !isspace(*s2) && *s2 != ',') *s1++ = *s2++;
	*s1 = NUL;
	return s1;
}

/*
 * lookup site,domain in routes database
 * if not found and bang is non-empty, use bang default if it exist
 */

static find_route(route, remote_user, remote_host, remote_domain, bang)
char *route, *remote_user, *remote_host, *remote_domain, *bang;
{
    char line[512];		/* line from route file */
    register char *lp;		/* current line position */
    char *routep;	       	/* ptr into route */
    char *pattern;		/* pattern from line */
    int  dom_ok;		/* in right domain */
    int  host_ok;		/* right host */
    FILE *rf;			/* route file */
    char local_host[100];	/* callers host name */
    char local_domain[100];	/* this domain */

    if (bang && *bang == NUL) bang = NULL;
    if (remote_host == NULL || *remote_host == NUL) return 0;

    if (remote_domain && *remote_domain == NUL) remote_domain = NULL;

    nn_gethostname(local_host, 100);
    if (routep = strchr(local_host, '.')) *routep = NUL;
    local_domain[0] = NUL;

    rf = open_file(relative(lib_directory, "routes"), OPEN_READ);
    if (rf == NULL) {
#ifdef TRACE
	if (route_trace) fprintf(route_trace, "---No routes file\n");
#endif
	return 0;
    }

    dom_ok = host_ok = 1;
    routep = route;
    pattern = NULL;

    while (fgets(line, 512, rf) != NULL) {
	lp = line;
	while (*lp && isspace(*lp)) lp++;
	if (*lp == NUL || *lp == '#') continue;

	if (*lp == '/') {
	    lp++;
	    cmdc = *lp++;
	    while (*lp && isspace(*lp)) lp++;
	    if (*lp == '#') *lp = NUL;

	    if (cmdc == 'L') {		/* local (default) domain name(s) */
		cstrcpy(local_domain, lp);

		if (remote_domain == NULL ||
		    cstreq(lp, remote_domain) != NULL) {
		    dom_ok = 1;
		    if (strcmp(local_host, remote_host) == 0) {
			pattern = "%p%n";
			break;
		    }
		}
		continue;
	    }

	    if (cmdc == 'D') {		/* destination domain */
		if (*lp == NUL)
		    dom_ok = 1;
		else
		    dom_ok = (cstreq(lp, remote_domain) != NULL);
		continue;
	    }

	    if (!dom_ok) continue;

	    if (cmdc == 'H') {		/* local host */
		if (*lp == NUL)
		    host_ok = 1;
		else
		    host_ok = (cstreq(lp, local_host) != NULL);
		continue;
	    }

	    if (!host_ok) continue;

	    switch (cmdc) {

	     case 'N':	/* neighbouring (uucp) sites */
		if (cstreq(lp, remote_host) == NULL) continue;
		pattern = "%s!%n";
		break;

	     case 'P':
		if (*lp == '+')
		    routep = cstrcpy(routep, ++lp);
		else
		    routep = cstrcpy(route, lp);
		continue;

	     case 'G':
		pattern = lp;
		break;

	     case 'B':
		if (!bang) continue;
		pattern = lp;
		break;

	     default:
		continue;
	    }

	    break;
	}

	if (!dom_ok) continue;
	if (!host_ok) continue;

	if ((pattern = cstreq(lp, remote_host))!=NULL) break;
    }

    fclose(rf);

    if (pattern == NULL) return 0;

#ifdef TRACE
    if (route_trace) fprintf(route_trace, "   pattern='%s'\n", pattern);
#endif

    for (; *pattern != NL && *pattern != NUL; pattern++) {
	if (*pattern == SP || *pattern == TAB) continue;
	if (*pattern == '%') {
	    pattern++;
	    switch(*pattern) {
	     case 'n':
		routep = cstrcpy(routep, remote_user);
		continue;
	     case 's':
		routep = cstrcpy(routep, remote_host);
		continue;
	     case 'd':
		routep = cstrcpy(routep,
				 remote_domain ? remote_domain : local_domain);
		continue;
	     case 'b':
		routep = cstrcpy(routep, bang);
		continue;
	     case 'p':
		routep = route;
		continue;
	     case '%':
		break;
	     default:
		continue;
	    }
	}
	*routep++ = *pattern;
    }
    *routep = NUL;

    return 1;
}

reroute(route, address)
char *route, *address;
{
    char *name, *site, *domain;
    char *atpos, *dotpos;
    register char *sp;
    register c;
    int found;

#ifdef TRACE
    if (route_trace ||
	(route_trace = open_file(relative(nn_directory, "trace"),
				OPEN_APPEND | DONT_CREATE)))
	fprintf(route_trace, "--orig: '%s'\n", address);
#endif

    found = 0;

    /* if a sender (or receiver!) is not provided,
     * we first try the site from the 'Reply-To:'
     * and 'From:' lines  who@site.domain */

    if (atpos = strchr(address, '@')) {
	*atpos = NUL;
	name = atpos;

	while (--name >= address)
	    if (isspace(*name) || *name == '<') {
		name++;
		break;
	    }
	if (name < address) name++;

	dotpos = atpos;
	site   = atpos + 1;

     next_dot:
	*dotpos = NUL;
	domain = dotpos + 1;
	for (sp = domain; c = *sp; sp++) {
	    if (isspace(c) || c == '>') break;
	    if (c == '.') {
		*dotpos = '.';
		dotpos = sp;
		goto next_dot;
	    }
	}
	*sp = NUL;
	if (site == domain)
	    domain = NULL;
	else
	    *atpos = NUL;	/* overwritten when first . is found */

#ifdef TRACE
	if (route_trace)
	    fprintf(route_trace,
		    "   @-type: name='%s' site='%s' domain='%s'\n",
		    name, site, domain ? domain : "");
#endif

	found = find_route(route, name, site, domain, (char *)NULL);

	if (dotpos) { *dotpos = '.'; *sp = c; }
	if (atpos) *atpos = '@';

	goto out;
    }

    /*
     * not domain address -- look for bang address
     */

    if (!(name = strrchr(address, '!'))) {
	/*
	 * neither domain nor bang -- suppose it is a local address
	 */
	strcpy(route, address);
	found = 1;
	goto out;
    }

    *name++ = NUL;
    if (site = strrchr(address, '!'))
	*site++ = NUL;
    else {
	site = address;
	address = NULL;
    }

#ifdef TRACE
    if (route_trace)
	fprintf(route_trace,
		"   !-type: name='%s' site='%s' bang='%s'\n",
		name, site, address ? address : "NONE");
#endif

    found = find_route(route, name, site, (char *)NULL, address);

    *--name = '!';
    if (address) *--site = '!';

 out:

#ifdef TRACE
    if (route_trace) {
	if (found)
	    fprintf(route_trace, "--route='%s'\n\n", route);
	else
	    fprintf(route_trace, "--NO ROUTE\n\n");
	fclose(route_trace);
	route_trace = NULL;
    }
#endif
    return found;
}

#endif
