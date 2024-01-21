/* filename: rlpr-common.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-common.h,v 1.10 1996/12/20 18:14:13 meem Exp $
 * contents: common #defines, headers for general-purpose rlpr functions
 *
 * Time-stamp: <1996/11/20 09:13 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPR_COMMON_H
#define RLPR_COMMON_H

#ifndef __GNUC__
#define __attribute__(x)  /* nothing */
#define __FUNCTION__      ""
#endif /* __GNUC__ */

#include <sys/types.h>
#include <errno.h>
#include <netinet/in.h>         /* for sockaddr_in struct */
#include <sys/socket.h>         /* for AF_INET definition */

#ifdef  OFF_T_LONG_LONG
#define OFF_T_S "%llu"
#else
#define OFF_T_S "%lu"
#endif

#ifndef HAVE_STRDUP
extern char * strdup(const char *s);
#endif

#ifndef HAVE_STRCSPN
extern size_t strcspn(const char *s, const char *reject);
#endif

#ifndef HAVE_STRSTR
extern char * strstr(const char *haystack, const char *needle);
#endif

#ifdef   MAXHOSTNAMELEN_BROKEN
#include <netdb.h>
#else
#include <sys/param.h>
#endif

/* this is the default port the proxy listens on (rlprd) and the
 * client connects to to contact the proxy. it can be whatever you
 * want as long as the client and the server are both in agreement.
 * note that putting it below 1024 is asinine.
 */

#define DEFAULT_RLPRD_TO_PORT   7290

#define DEFAULT_TMP_DIR        "/tmp"     /* fallback */

#define LO_LPD_FROM_PORT  721             /* INCLUSIVE */
#define HI_LPD_FROM_PORT  731             /* INCLUSIVE */
#define LPD_TO_PORT       515

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif /* not MIN */

/* this struct defines general properties that are common to just about
 * all applications..
 */

typedef struct props {
  int         quiet:1;
  int         debug:1;
  int         syslog:1;
} props;

props props_;			/* each separate program gets one of these */

extern char * name;		/* program name */

/* function prototypes */

enum level { DEBUG, INFO, WARNING, FATAL };
enum errno { NO_ERRNO, ERRNO };

void           init_sockaddr(struct sockaddr_in *sin,
                             const char *hostname, u_short port_hbo);
int            get_local_hostname(char *buf, size_t sz); 
int            bind_try_range(struct sockaddr_in * sin, int lo, int hi, int sock);
int            read_fd_to_fd(int rfd, int wfd);
char *         strlower(char *str);
off_t          filesz(int fd);
int            writen(int fd, const char *ptr, int nbytes);
void           toggle_euid(void);
const char *   h_strerror(void);
inline void *  rlpr_malloc(size_t sz);
inline void *  rlpr_strdup(char * orig);
void           rlpr_msg(enum level l, enum errno errno_set, char *fmt, ...);

#define check_ptr(x) \
  if ((x) == NULL) rlpr_msg(FATAL, NO_ERRNO, \
			    "%s: ouch, out of memory!", __FUNCTION__)

#define safe_writen(fd, buf, buflen) \
  if (writen((fd), (buf), (buflen)) < 0) \
    rlpr_msg(FATAL, ERRNO, "writen in %s", __FUNCTION__)
    
#endif /* RLPR_COMMON_H */
