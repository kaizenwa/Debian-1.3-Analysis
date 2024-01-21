/* This silly little file is here because gcc messes up badly with
 * prototyping the getgroups() / setgroups() functions on SunOS.
 * The problem is that on SunOS 4.x, there are two versions of each of
 * these functions: the usual (non-SysV) version takes an array of ints:
 * 	[gs]etgroups(int, int *)
 * but the Sys V version takes an array of gid_t's:
 * 	[gs]etgroups(int, gid_t *).
 * Unfortunately, gcc insists on the latter.  This isn't right for
 * most compilations -- it won't work unless you link with the
 * /usr/5lib version of libc.

 * Hence this little file: it invokes [gs]etgroups declaring its
 * argument as pointer to void, and _hope_ that this is the same
 * size pointer as (gid_t *), (uid_t *), and (int *).  If it is,
 * all will work smoothly on any host, yet we avoid including
 * the file <sys/unistd.h> wherein getgroups is inappropriately
 * prototyped.
 */
#include "config.h"

#ifdef HAVE_GETGROUPS
int Setgroups(n, g)
int n;
void *g;
{
    int setgroups __P((int, void *));
    return setgroups(n, g);
}
 
int Getgroups(n, g)
int n;
void *g;
{
    int getgroups __P((int, void *));
    return getgroups(n, g);
}
#endif
