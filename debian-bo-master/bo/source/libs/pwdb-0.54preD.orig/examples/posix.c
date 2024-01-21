#include <stdio.h>
#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>

#include <pwdb/pwdb_public.h>

/*
 * prototype to shut off compiler warnings
 */

void pgroup(gid_t gid);
void puser(uid_t uid);
void pallg(void);

/* grp numbers in decimal with name */

void pgroup(gid_t gid)
{
    struct group *grp;

    grp = getgrgid(gid);
    if (grp == NULL) {
	fprintf(stderr, "gid(%ld) is an unknown group\n", (long int) gid);
    } else {
	printf("gid(%ld) = %s\nmembers:\n"
	       , (long int) grp->gr_gid, grp->gr_name);
	while (grp->gr_mem && *grp->gr_mem) {
	    printf("\t%s\n", *grp->gr_mem);
	    (grp->gr_mem)++;
	}
    }
}

/* uid in decimal with name */

void puser(uid_t uid)
{
    struct passwd *pwd;

    pwd = getpwuid(uid);
    if (pwd == NULL) {
	fprintf(stderr, "uid(%ld) is not known\n", (long int) uid);
    } else {
	printf("uid(%ld) = %s runs %s from %s\n"
		, (long int) pwd->pw_uid, pwd->pw_name
		, pwd->pw_shell, pwd->pw_dir);
	pgroup(pwd->pw_gid);
    }
}

/* list all groups */

void pallg(void)
{
    int ngs, i;
    gid_t *gids=NULL;

    ngs = getgroups(0, NULL);
    gids = calloc(ngs, sizeof(*gids));
    ngs = getgroups(ngs, gids);
    printf("Supplementary groups:\n");
    for (i=0; i<ngs; ++i) {
	pgroup(gids[i]);
    }
    printf("---------\n");
    free(gids);
}

void main(void)
{
    const char *user;
    struct passwd *pwd;

    /* identify user invoking this program */
    /* - logname */
    user = getlogin();

    pwd = getpwnam(user);
    if (pwd == NULL) {
	fprintf(stderr, "cannot identify log-name user %s\n", user);
    } else {
	printf("log-name = %s >\n", user);
	puser(pwd->pw_uid);
	printf("--\n");
    }

    /* - current user */
    printf("Invoking user >\n");
    puser(getuid());

    /* - (e)uid and (e)user */
    printf("Effective user >\n");
    puser(geteuid());

    exit(0);
}
