/* Set this as the user id number you want dhttpd to use when    *
 * starting it as root.  You can get this info from /etc/passwd. */
#define UID		15

/* Set this to the proper group id number of UID above.          */
#define GID		16

/* Set your default port number.  If you aren't running as       *
 * root, you must use a value >= 1024, such as 8080.             */
#define DEFAULTPORT	80

/* This is the directory where the web pages are located.        */
#define WEBDIRPREFIX	"/web"

/* This defines the maximum number of child processes (i.e.      *
 * maximum sockets being serviced by dhttpd).                    */
#define MAXCHILDPROC 100
