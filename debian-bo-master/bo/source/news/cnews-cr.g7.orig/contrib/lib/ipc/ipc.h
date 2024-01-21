/* v10 ipc interface, from ipc(3x) */

typedef struct {
	int	reserved[5];
	char	*name;			/* that being dialed */
	char	*param;			/* parameters used to set up call */
	char	*machine;		/* machine id of caller */
	char	*user;			/* user name of caller */
	int	uid, gid;		/* uid, gid of caller */
} ipcinfo;

extern char *ipcpath();
extern ipcinfo *ipclisten();
extern char *errstr;
