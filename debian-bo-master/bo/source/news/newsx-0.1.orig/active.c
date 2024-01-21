/*  VER 079  TAB P   $Id: active.c,v 1.7 1996/11/22 12:31:52 src Exp $
 *
 *  handle the local active file
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"

/* 
 *  globals for active 
 *  BUG: improve speed!!!!
 */
typedef struct active_grp {
    struct active_grp *next;
    char seen;
    char name[1]; /* extend as required... */
} ACTIVE_GRP;

#define ACTIVE_HASH 199
ACTIVE_GRP *activegrp[ACTIVE_HASH] = { 0 };

/*
 *  find group on active list
 */
static ACTIVE_GRP *find_active(char *group)
{
    ACTIVE_GRP *ap;
    long h = hashindex(group,ACTIVE_HASH);

    for (ap = activegrp[h]; ap; ap = ap->next) {
	if (strcmp(ap->name,group)==0) return ap;
    }
    return 0; /* not in list... */
}

/*
 *  add group to list of active groups
 */
static active_group(char *group)
{
    int n; 
    ACTIVE_GRP *ap;
    long h = hashindex(group,ACTIVE_HASH);

    if (!sys_allow(group)) {
	log_msg(L_DEBUG3,"active group %s disallowed by sys",group);
	return;
    }

    /* BUG: check if already? */
    n = sizeof(ACTIVE_GRP) + strlen(group);

    if (!(ap = malloc(n))) {
	log_msg(L_ERRno,"out of memory");
	unlock_exit(1);
    }
    /* add to list of active groups */
    ap->seen = 0;
    strcpy(ap->name,group);
    ap->next = activegrp[h];
    activegrp[h] = ap;
}

/*
 *  see if group is on active list, and is not yet seen
 */
int is_active(char *group)
{
    ACTIVE_GRP *ap;

    if (!(ap = find_active(group))) {
	return 0; /* not in list... */
    }
    if (ap->seen) return 0; /* seen already... */
    ap->seen = 1;
    return 1;
}

/*
 *  go through all groups in the active list that is not yet seen    
 *  NOTE: the active list will be lost
 */
int unseen_active(char *group)
{
    int ok = 0;
    ACTIVE_GRP *ap;
    static int latest_h = 0;
    int h;

    for (h=latest_h; h<ACTIVE_HASH; ++h) {
	while (ap = activegrp[h]) {
	    if (!ap->seen) {
		strcpy(group,ap->name);
		ok = 1;
	    }
	    activegrp[h] = ap->next;
	    free(ap);
	    if (ok) {
		latest_h = h;
		return 1;
	    }
	}
    }
    latest_h = 0;
    return 0;
}

/*
 *  load local active file
 *  return true if more
 */
void load_active(void)
{
    FILE *f;
    char activename[PATH_MAX];
    char buf[BUFSIZ];
    char *p;
    char group[BUFSIZ];
    long a,b;
    char isact;

    load_sys();
    progtitle("read active");

    build_filename(activename,NEWSCTL,ACTIVE_FILE,NULL);

    if (!(f = fopen(activename,"r"))) {
	log_msg(L_ERRno,"cannot open '%s'",activename);
	unlock_exit(8);
    }
    log_msg(L_DEBUG,"reading %s",activename);

    /* load local active file */
    buf[BUFSIZ-1] = '\0';
    while (fgets(buf,BUFSIZ-1,f)) {
	p = buf;
	while (isspace(*p)) ++p;
	if (p[0] && p[0] != '#') {
	    if (sscanf(p,"%[^ \n\t!:] %ld %ld %c",group,&a,&b,&isact)!=4) {
		log_msg(L_ERRno,"bad syntax in active '%s'",buf);
	    } else {
		if (isact == 'y' || isact == 'm') {
		    /* group is active */
		    active_group(group);
		}
	    }
	}
    }
    fclose(f);
}

