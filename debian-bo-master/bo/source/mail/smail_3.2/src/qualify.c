/*
#ident	"@(#)smail/src:RELEASE-3_2:qualify.c,v 1.5 1996/02/26 18:12:48 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * qualify.c:
 *      Fully qualify a domain.  In lieu of full qualification rules,
 *      this routine appends the first visible domain.
 *
 *	This source file was contributed by Chip Salzenberg,
 *	chip@ateng.ateng.com.  It has been modified.
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "defs.h"
#include "smail.h"
#include "parse.h"
#include "dys.h"
#ifndef DEPEND
# include "extern.h"
#endif

struct qualify {
    char    *host;                      /* host name */
    char    *domain;                    /* domain where host lives */
};

static struct qualify *domains = NULL;  /* Array of host/domain pairs */
static int domain_count = 0;            /* Count of valid pairs */
static int domain_max = 0;              /* Count of allocated pairs */

/* variables imported from libc */
extern int errno;

char *
read_qualify_file()
{
    FILE *f;
    char *entry;
    struct stat statbuf;

    /*
     * ignore any previously-read qualifier data
     */
    domain_count = 0;

    /*
     * try to open qualifier file, stat file if possible
     */
    if (qualify_file == NULL || EQ(qualify_file, "-")) {
	return NULL;
    }
    f = fopen(qualify_file, "r");
    if (f == NULL) {
	if (require_configs) {
	    return xprintf("cannot open %s: %s", qualify_file, strerror(errno));
	}

	add_config_stat(qualify_file, (struct stat *)NULL);
	return NULL;
    }

    (void)fstat(fileno(f), &statbuf);
    add_config_stat(qualify_file, &statbuf);

    /* loop and read all of the table entries in the domains file */

    if (!domains) {
	domain_max = 16;
	domains = (struct qualify *) xmalloc((unsigned) (domain_max * sizeof(*domains)));
    }
    while (entry = read_entry(f)) {
	struct attribute *new;
	char *error;

	new = parse_table(entry, &error);
	if (new == NULL) {
	    continue;
	}
	if (domain_count >= domain_max) {
	    domain_max += 16;
	    domains = (struct qualify *) xrealloc((char *) domains,
						  (unsigned) (domain_max * sizeof(struct qualify)));
	}
	domains[domain_count].host = new->name;
	domains[domain_count].domain = new->value;
	++domain_count;
	xfree((char *)new);
    }
    (void) fclose(f);

    return NULL;
}

char *
qualify_domain(s)
    char *s;				/* domain to fully qualify */
{
    int i;

    if (index(s, '.') != NULL) {
	return NULL;
    }

    for (i = 0; i < domain_count; ++i) {
	char *host = domains[i].host;

	if (EQIC(host, s) || EQIC(host, "*")) {
	    return domains[i].domain;
	}
    }

    return NULL;
}

void
dump_qualify_config(f)
     FILE * f;
{
    int i;

    fputs("#\n# -- qualify configuration\n#\n", f);
    for (i = 0; i < domain_count; ++i) {
	fprintf(f, "%s\t%s\n",
		domains[i].host,
		domains[i].domain);
    }
    fputs("#\n# -- end of qualify configuration\n#\n", f);
}
