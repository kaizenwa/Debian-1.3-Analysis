#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <pwd.h>
#include <grp.h>

#define	HASHSIZE	16			/* power of 2 */
#define	HASH(x)		((x) & (HASHSIZE - 1))


static struct pwbuf {
    uid_t uid;
    char name[12];
    struct pwbuf *next;
} *pwhash[HASHSIZE];

char *user_from_uid(uid_t uid)
{
    struct pwbuf **p;
    struct passwd *pw;

    p = &pwhash[HASH(uid)];
    while (*p) {
	if ((*p)->uid == uid)
	    return((*p)->name);
	p = &(*p)->next;
    }
    *p = (struct pwbuf *) malloc(sizeof(struct pwbuf));
    (*p)->uid = uid;
    if ((pw = getpwuid(uid)) == NULL)
	sprintf((*p)->name, "#%d", uid);
    else
	sprintf((*p)->name, "%-.8s", pw->pw_name);
    (*p)->next = NULL;
    return((*p)->name);
}

static struct grpbuf {
    gid_t gid;
    char name[12];
    struct grpbuf *next;
} *grphash[HASHSIZE];

char *group_from_gid(gid_t gid)
{
    struct grpbuf **g;
    struct group *gr;

    g = &grphash[HASH(gid)];
    while (*g) {
	if ((*g)->gid == gid)
	    return((*g)->name);
	g = &(*g)->next;
    }
    *g = (struct grpbuf *) malloc(sizeof(struct grpbuf));
    (*g)->gid = gid;
    if ((gr = getgrgid(gid)) == NULL)
	sprintf((*g)->name, "#%d", gid);
    else
	sprintf((*g)->name, "%-.8s", gr->gr_name);
    (*g)->next = NULL;
    return((*g)->name);
}

void bad_user_access_length() { }
