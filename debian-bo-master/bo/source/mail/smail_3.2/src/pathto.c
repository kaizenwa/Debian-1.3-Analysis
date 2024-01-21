/*
#ident	"@(#)smail/src:RELEASE-3_2:pathto.c,v 1.9 1996/02/28 06:47:14 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * main.c:
 *	process arguments, configure environment and process
 *	messages.
 *
 *	external functions: main, initialize_state, process_args,
 *	deliver_mail
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "main.h"
#include "transport.h"
#include "addr.h"
#include "exitcodes.h"
#ifndef DEPEND
# include "error.h"
# include "extern.h"
#endif

/* variables local to this file */
static int bang_paths_only;
static int disable_smarthost;
static int direct_and_route;
static int show_transport;

/* functions local to this file */
static void print_pathto_addr();
static void pathto_direct_and_route();
static char **pathto_setup();
static struct addr *pathto_route();
static void pathto_usage();
static void uupath_usage();

/*
 * pathto - return paths to the given addresses.
 */
void
pathto(argv)
    char **argv;			/* generally, argv from main */
{
    struct addr *addr;
    char *error;

    argv = pathto_setup(argv, "vdnstD:", pathto_usage);
    if (direct_and_route) {
	struct addr *okay;		/* list of routed addrs */
	struct addr *defer;		/* list of temp failed addrs */
	struct addr *fail;		/* list of failed addrs */

	pathto_direct_and_route(argv, &okay, &defer, &fail);
	while (defer) {
	    if (defer->error->info&ERR_CONFERR) {
		(void) fprintf(stderr, "%s: %s ... config error: %s\n",
			       program, defer->in_addr,
			       defer->error->message);
	    } else {
		(void) fprintf(stderr, "%s: %s ... temp error: %s\n",
			       program, defer->in_addr,
			       defer->error->message);
	    }
	    defer = defer->succ;
	}
	while (fail) {
	    (void) fprintf(stderr, "%s: %s ... failed: %s\n",
			   program, fail->in_addr, fail->error->message);
	    fail = fail->succ;
	}
	while (okay) {
	    print_pathto_addr(okay);
	    okay = okay->succ;
	}
    } else {
	for (; *argv; argv++) {
	    if ((addr = pathto_route(*argv, &error)) == NULL) {
		if (error) {
		    (void) fprintf(stderr, "%s: %s: %s\n",
				   program, *argv, error);
		}
		continue;
	    } else {
		if (addr->next_host == NULL) {
		    addr->next_addr = addr->remainder;
		}
		print_pathto_addr(addr);
	    }
	}
    }

    exit(EX_OK);
}

/*
 * pathto_direct_and_route - take an array of address strings and
 *			     return a list of resolved addresses
 */
static void
pathto_direct_and_route(addr_strings, okay, defer, fail)
    char **addr_strings;		/* input address strings */
    struct addr **okay;			/* successfully routed addrs */
    struct addr **defer;		/* temp routing failures */
    struct addr **fail;			/* unresolvable addrs */
{
    register struct addr *addr = NULL;

    *okay = NULL;
    *defer = NULL;
    *fail = NULL;
    while (*addr_strings) {
	register struct addr *new = alloc_addr();
	char *error;

	new->in_addr = *addr_strings;
	new->work_addr = preparse_address(new->in_addr, &error);
	if (new->work_addr == NULL) {
	    new->error = note_error(ERR_111,
				    xprintf("parse error: %s", error));
	    new->succ = *fail;
	    *fail = new;
	} else {
	    if (disable_smarthost) {
		new->flags |= ADDR_SMARTUSER|ADDR_SMARTHOST;
	    }
	    new->succ = addr;
	    addr = new;
	}
	addr_strings++;
    }

    /* resolve all of the addresses, removing duplicates */
    resolve_addr_list(addr, okay, defer, fail, TRUE);
}

/*
 * print_pathto_addr - output the given addr structure as an address
 */
static void
print_pathto_addr(addr)
    register struct addr *addr;
{
    if (show_transport) {
	printf("%-11s ", addr->transport? addr->transport->name: "local");
    }
    if (addr->next_host == NULL) {
	if (direct_and_route) {
	    (void) printf("%s\n", addr->next_addr);
	} else if (bang_paths_only) {
	    (void) printf("%s!%s\n", uucp_name, addr->next_addr);
	} else {
	    (void) printf("%s@%s\n", addr->next_addr, primary_name);
	}
    } else if (bang_paths_only) {
	char *error;
	char *path = build_uucp_route(addr->next_addr, &error,
				      addr->parseflags);

	if (path == NULL) {
	    putchar('\n');
	    (void) fprintf(stderr, "%s: %s: %s\n",
			   program, addr->in_addr, error);
	    return;
	} else {
	    (void) printf("%s!%s\n", addr->next_host, path);
	}
    } else {
	printf("%s :: %s\n", addr->next_host, addr->next_addr);
    }
    return;
}

/*
 * optto - return more optimal paths to the given addresses.
 */
/*ARGSUSED*/
void
optto(argv)
    char **argv;			/* generally, argv from main */
{
    (void) fprintf(stderr, "%s: optto not currently supported\n", program);
    exit(EX_UNAVAILABLE);
}

/*
 * uupath - return paths to the given host.
 */
void
uupath(argv)
    char **argv;			/* generally, argv from main */
{
    struct addr *addr;
    char *error;

    for (argv = pathto_setup(argv, "vstD:", uupath_usage); *argv; argv++) {
	char *host_path = build_uucp_route(*argv, &error, 0);
	char *in_addr;

	if (host_path == NULL) {
	    (void) fprintf(stderr, "%s: %s: %s\n", program, *argv, error);
	    continue;
	}
	in_addr = xmalloc(strlen(host_path) + sizeof("!~~user~~"));
	(void) sprintf(in_addr, "%s!~~user~~", host_path);
	if ((addr = pathto_route(in_addr, &error)) == NULL) {
	    if (error) {
		(void) fprintf(stderr, "%s: %s: %s\n", program, *argv, error);
	    }
	    continue;
	}
	if (show_transport) {
	    printf("%-11s ", addr->transport? addr->transport->name: "local");
	}
	if (addr->next_host == NULL) {
	    (void) printf("%%s\n");
	} else {
	    char *last_part;
	    char *path = build_uucp_route(addr->next_addr, &error,
					  addr->parseflags);

	    if (path == NULL) {
		(void) fprintf(stderr, "%s: %s: %s\n",
			       program, *argv, error);
	    } else {
		last_part = rindex(path, '!');
		if (last_part == NULL) {
		    (void) printf("%s\n", addr->next_host);
		} else {
		    *last_part = '\0';	/* chop off the ~~user~~ */
		    (void) printf("%s!%s\n", addr->next_host, path);
		}
	    }
	}
    }

    exit(EX_OK);
}

/*
 * pathto_setup - standard setup for the various pathto routines
 */
static char **
pathto_setup(argv, oargs, usage)
    char **argv;
    char *oargs;
    void (*usage)();
{
    char *error;
    int argc;
    extern char *optarg;
    extern int optind, opterr;
    int c;
    char **argp;

    /* handle relative configuration file names */
    config_file = make_lib_fn(config_file);

    /* read in the config and secondary config files */
    if ((error = read_config_file(config_file)) ||
	(second_config_file &&
	   (error = read_config_file(make_lib_fn(second_config_file)))))
    {
	panic(EX_OSFILE, "%s", error);
    }

    /* do the same thing with other variables */
    director_file = make_lib_fn(director_file);
    router_file = make_lib_fn(router_file);
    transport_file = make_lib_fn(transport_file);
    method_dir = make_lib_fn(method_dir);
    copying_file = make_lib_fn(copying_file);
    smail = make_lib_fn(smail);

    /* read the other configuration files */
    if ((error = read_transport_file()) ||
	(error = read_router_file()) ||
	(error = read_director_file()))
    {
	panic(EX_OSFILE, "%s", error);
    }

    /* All errors go to the terminal */
    error_processing = TERMINAL;

    --argv;				/* go back to program name */
    initialize_state();

    /* count arguments, why does getopt really need this? */
    for (argc = 0; argv[argc]; argc++) ;

    disable_smarthost = TRUE;
    bang_paths_only = TRUE;
    direct_and_route = FALSE;
    show_transport = FALSE;

    while ((c = getopt(argc, argv, oargs)) != EOF) {
	switch (c) {

	case 'd':
	    direct_and_route = TRUE;
	    break;

	case 'n':
	    bang_paths_only = FALSE;
	    break;

	case 't':
	    show_transport = TRUE;
	    break;

	case 's':
	    disable_smarthost = FALSE;
	    break;

	case 'v':
	    if (! debug)
		debug = 1;		/* turn on debugging */
	    break;

	case 'D':
	    debug = atoi(optarg);	/* set debugging level */
	    break;

	default:
	    (*usage)();
	    exit(EX_USAGE);
	}
    }

    if (argv[optind] == NULL) {
	(*usage)();
	exit(EX_USAGE);
    }

    for (argp = &argv[optind]; *argp; argp++) {
	strip_rfc822_comments(*argp);
    }

    /* setup all of the hostname information */
    build_host_strings();

    return &argv[optind];
}

static struct addr *
pathto_route(in_addr, error)
    char *in_addr;
    char **error;
{
    struct addr *cur = alloc_addr();
    struct addr *defer = NULL;
    struct addr *fail = NULL;
    struct addr *done = NULL;
    int form;

    cur->in_addr = in_addr;
    if (disable_smarthost) {
	    /*
	     * state we have already used the smarthost router to
	     * prevent its being used at all
	     */
	    cur->flags |= ADDR_SMARTHOST;
    }
    if ((cur->work_addr = preparse_address(in_addr, error)) == NULL) {
	return NULL;
    }

    while (cur &&
	   (form = parse_address(cur->work_addr, &cur->target,
				 &cur->remainder, &cur->parseflags)
	    ) != FAIL && form != PARSE_ERROR && form != LOCAL)
    {
	struct addr *retry;

	cur->flags &= ~ADDR_FORM_MASK;
	cur->flags |= form;

	if (islocalhost(cur->target)) {
	    cur->work_addr = cur->remainder;
	    continue;
	}
	done = NULL;
	defer = NULL;
	fail = NULL;
	retry = NULL;
	route_remote_addrs(cur, &done, &retry, &defer, &fail);
	cur = retry;
    }

    /* note if route_remote_addrs returned an error */
    if (defer) {
	*error = defer->error->message;
	return NULL;
    }
    if (fail) {
	*error = fail->error->message;
	return NULL;
    }

    /* nope, no error from route_remote_addrs */
    switch (form) {
    case PARSE_ERROR:
    case FAIL:
	*error = cur->remainder;
	return NULL;

    case LOCAL:
	cur->next_host = NULL;
	break;

    default:
	if (done) {
	    cur = done;
	} else {
	    *error = NULL;
	    return NULL;
	}
	break;
    }

    return cur;
}

static void
pathto_usage()
{
    (void) fprintf(stderr, "usage: %s [-vdsnt] addr ...\n", program);
}

static void
uupath_usage()
{
    (void) fprintf(stderr, "usage: %s [-vst] addr ...\n", program);
}
