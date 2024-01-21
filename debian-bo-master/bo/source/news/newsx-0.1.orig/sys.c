/*  VER 068  TAB P   $Id: sys.c,v 1.6 1996/11/22 12:31:52 src Exp $
 *
 *  handle the local sys or newsfeeds file
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"

/* 
 *  globals for sys
 */
typedef struct sys_grp {
    struct sys_grp *next;
    char not;
    char name[1]; /* extend as required... */
} SYS_GRP;

SYS_GRP *sysgrp= 0;

/*
 *  add group in sys-file
 */
static sys_group(char *group,int not)
{
    int n; 
    SYS_GRP *sp;

    n = sizeof(SYS_GRP) + strlen(group);

    if (!(sp = malloc(n))) {
	log_msg(L_ERRno,"out of memory");
	unlock_exit(1);
    }
    /* add to list of sys-groups */
    sp->not = not;
    strcpy(sp->name,group);
    sp->next = sysgrp;
    sysgrp = sp;
}

/*
 *  does group match pattern?
 *
 *  NOTE: the implementations probably only implements a straight
 *	  trailing match, we try to do some more 
 *  NOTE: should probably have used wildmat() for '*'-expressions
 */
static sys_match(char *group,char *pattern)
{
    /* INN style match */
    if (pattern[0]=='*') {
	if (!pattern[1]) {
	    return 1; /* trailing match */
	}
	if (pattern[1]=='.') {
	    while (*group) {
		if (*group++ == '.') break; 
	    }
	    return sys_match(group,pattern+2);			
	}
    }
    /* Cnews style match */
    if (pattern[0]=='a' && pattern[1]=='l' && pattern[2]=='l') {
	if (!pattern[3]) {
	    return 1; /* trailing match */
	}
	if (pattern[3]=='.') {
	    while (*group) {
		if (*group++ == '.') break; 
	    }
	    /* group is empty */
	    return sys_match(group,pattern+4);			
	}
    }
    /* non-wildcard case */
    while (*group) {
	if (pattern[0]=='*' && !pattern[1]) return 1; /* string wildcard */
	if (*group == '.') {
	    if (!*pattern) return 1; /* "comp.os.linux" with pattern "comp" */
	    if (*pattern != '.') return 0;
	    return sys_match(group+1,pattern+1);
	}
	if (*group++ != *pattern++) return 0;
    }
    /* group exhausted */
    if (!*pattern) return 1; /* exact match */
    if (*pattern!='.') return 0; /* no match */

    /* "comp.os" when pattern is "comp.os.*" or "comp.os.all" will match */
    return sys_match(group,pattern+1);
}

/*
 *  is group allowed in sys-file?
 */
int sys_allow(char *group)
{
    int ok = 0;
    int match;
    SYS_GRP *sp;

    for (sp = sysgrp; sp; sp = sp->next) {
	match = sys_match(group,sp->name);
	if (sp->not) {
	    if (match) return 0; /* definitely not */
	} else {
	    if (match) ok = 1; /* think so */
	}
    }
    return ok;
}

/*
 *  pick an item from a Cnews style sys file
 *  items are:
 *	text
 *	"!" 
 *	"/" 
 *	"," 
 *	":" 
 *	"#" 
 *	"\n"
 */
static void sys_item(char *item)
{
    static int state = 0;

    switch (state) {
	/* 
	 *  state: site
	 */
    case 0:
    default:		   
	switch (*item) {
	case '#':
	    state = 1;
	    break;
	case '\n':
	    break;
	case '!':
	case '/':
	case ',':
	case ':':
	    /* error */
	    log_msg(L_ERRno,"bad syntax in sys");
	    state = 1;
	    break;
	default:
	    if (strcmp(item,"ME") == 0) {
		/* BUG: ignoring info about ourselves... */
		log_msg(L_DEBUG3,"skipping ME sys-entry");
		state = 1;
	    } else if (strcmp(item,spoolname) == 0) {
		log_msg(L_DEBUG,"found sys-entry for %s",item);
		state = 2;
	    } else {
		log_msg(L_DEBUG3,"skipping sys-entry for %s",item);
		state = 1;
	    }
	    break;
	}
	break;

	/* 
	 *  state: skip rest of line
	 */
    case 1:		   
	switch (*item) {
	default:
	    break;
	case '\n':
	    state = 0;
	    break;
	}
	break;

	/* 
	 *  state: ignore exclusions
	 */
    case 2:
	switch (*item) {
	case ':':
	    state = 3;
	    break;
	case '#':
	    state = 1;
	    break;
	case '\n':
	    state = 0;
	    break;
	default:
	case '!':
	case ',':
	    /* ignore */
	    break;
	}
	break;

	/* 
	 *  state: groups
	 */
    case 3:
    case 4: /* seen a '!' */
	switch (*item) {
	case ':': /* ignore flags */
	case '/': /* ignore distlist */
	case '#':
	    state = 1;
	    break;
	case '\n':
	    state = 0;
	    break;
	case '!':
	    state = 4;
	    break;
	case ',':
	    state = 3; 
	    break;
	default:
	    log_msg(L_DEBUG3,"sys group %s%s",(state==4?"!":""),item);
	    sys_group(item,state==4);
	    state = 3;
	    break;
	}
    }
}

/*
 *  pick a line from a Cnews style sys file
 *
 *  lines may be continued by using a trailing slash
 *
 *  format is:
 *	site/exclusions:grouplist/distlist:flags:cmd
 *
 *  site is (short) name of spool
 *  exclusions is things in patch we don't want to be bothered with
 *  grouplist is:
 *	group
 *	group,grouplist
 *	!group
 *	all		    matches everything (like *)
 *	something.all	    matches something.*
 *  distlist is a list of distributions
 *
 *  BUG: assume items do not cross lines
 */
static void sys_line(char *line)
{
    char what[2];
    char *p;
    char *start = 0;
    int more = 0;

    for (p = line; ;++p) {
	switch (*p) {
	case '#':
	    goto sep;
	case '/':
	    goto sep;
	case '!':
	    goto sep;
	case ':':
	    goto sep;
	case ',':
	    goto sep;
	case '\n':
	    if (more) *p = '\0';
	  sep: 
	    what[0] = *p;
	    what[1] = '\0';
	    if (start) {
		*p = '\0';
		sys_item(start);
		*p = what[0];
		start = 0;
	    }
	    if (what[0]) sys_item(what);
	    more = 0;
	    break;
	case ' ':
	case '\t':
	    *p = '\0';
	    if (start) sys_item(start);
	    break;
	case '\\':
	    more = 1;
	    break;
	case '\0':
	    if (start) sys_item(start);
	    return;
	default:
	    if (!start) start = p;
	    break;
	}
    }
}

/*
 *  load local sys or newsfeeds file
 *
 *  lines may be continued by using a trailing slash
 *
 *  the Cnews format is:
 *	site/exclusions:grouplist/distlist:flags:cmds
 *  the INN format is:
 *	site/exclusions:grouplist/distlist:flags:param
 *
 *  site is (short) name of spool
 *  exclusions is things in patch we don't want to be bothered with
 *  grouplist is:
 *	group
 *	group,grouplist
 *	!group
 *	group/distlist
 *  group is:
 *	groupelem	    matches groupelem.*
 *	groupelem.groupelem
 *  groupelem is: 
 *	all		    matches everything (Cnews)
 *	*		    matches everything (INN)
 *	something	    matches something
 *  distlist is a list of distributions that we simply ignore
 */
void load_sys(void)
{
    FILE *f;
    char sysname[PATH_MAX];
    char buf[BUFSIZ];

    progtitle("read sys");

    if (inn_flag) {
	build_filename(sysname,NEWSCTL,I_NEWSFEEDS,NULL);
    } else {
	build_filename(sysname,NEWSCTL,C_SYS_FILE,NULL);
    }

    if (!(f = fopen(sysname,"r"))) {
	log_msg(L_ERRno,"cannot open '%s'",sysname);
	unlock_exit(8);
    }
    log_msg(L_DEBUG,"reading %s",sysname);

    /* load local sys file */
    buf[BUFSIZ-1] = '\0';
    while (fgets(buf,BUFSIZ-1,f)) {
	sys_line(buf);
    }
    fclose(f);
}
