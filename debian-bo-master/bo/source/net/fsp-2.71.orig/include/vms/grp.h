/*
 *	@(#)grp.h	5.4 (Berkeley) 4/3/91
 *
 * 24-DEC-92  Modified for use with VMS-fsp.
 */

#ifndef _GRP_H_
#define	_GRP_H_

#ifndef _POSIX_SOURCE
#define	_PATH_GROUP		"/etc/group"
#endif

struct group {
	char	*gr_name;		/* group name */
	char	*gr_passwd;		/* group password */
	int	gr_gid;			/* group id */
	char	**gr_mem;		/* group members */
};

#endif /* !_GRP_H_ */
