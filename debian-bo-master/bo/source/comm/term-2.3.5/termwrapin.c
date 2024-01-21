/* replace normal networking commands with those in libtermnet, a bit
 * like termnet.h but using real functions rather than CPP macros
 */

/* note that we don't particularly care about types here, so everything
 * comes down to either a basic type or a void*
 */

int term_accept(int s,void *addr,int *addrlen);
int accept(int s,void *addr,int *addrlen)
{ return term_accept(s,addr,addrlen); }

int term_bind(int s,void *name,int namelen);
int bind(int s,void *name,int namelen)
{ return term_bind(s,name,namelen); }

int term_chroot(const char *path);
int chroot(const char *path)
{ return term_chroot(path); }

int term_close(int fd);
int close(int fd)
{ return term_close(fd); }

int term_connect(int s,void *name,int namelen);
int connect(int s,void *name,int namelen)
{ return term_connect(s,name,namelen); }

int term_dup(int fd);
int dup(int fd)
{ return term_dup(fd); }

int term_dup2(int fd1,int fd2);
int dup2(int fd1,int fd2)
{ return term_dup2(fd1,fd2); }

int term_fcntl(int fd,int cmd,void *arg);
int fcntl(int fd,int cmd,void *arg)
{ return term_fcntl(fd,cmd,arg); }

int term_fork();
int fork()
{ return term_fork(); }

void *term_gethostbyname(const char *name);
void *gethostbyname(const char *name)
{ return term_gethostbyname(name); }

void *term_gethostbyaddr(const char *addr,int len,int type);
void *gethostbyaddr(const char *addr,int len,int type)
{ return term_gethostbyaddr(addr,len,type); }

int term_gethostname(char *name,int namelen);
int gethostname(char *name,int namelen)
{ return term_gethostname(name,namelen); }

int term_getpeername(int s,void *name,int *namelen);
int getpeername(int s,void *name,int *namelen)
{ return term_getpeername(s,name,namelen); }

int term_getsockname(int s,void *name,int *namelen);
int getsockname(int s,void *name,int *namelen)
{ return term_getsockname(s,name,namelen); }

int term_listen(int s,int backlog);
int listen(int s,int backlog)
{ return term_listen(s,backlog); }

void term_perror(const char *s);
void perror(const char *s)
{ term_perror(s); }

int term_rcmd(char **ahost,unsigned short inport,char *luser,
         char *ruser,char *cmd,int *fd2p);
int rcmd(char **ahost,unsigned short inport,char *luser,
         char *ruser,char *cmd,int *fd2p)
{ return term_rcmd(ahost,inport,luser,ruser,cmd,fd2p); }

int term_recv(int s,char *buf,int len,int flags);
int recv(int s,char *buf,int len,int flags)
{ return term_recv(s,buf,len,flags); }

int term_recvfrom(int s,char *buf,int len,int flags,void *from,int *fromlen);
int recvfrom(int s,char *buf,int len,int flags,void *from,int *fromlen)
{ return term_recvfrom(s,buf,len,flags,from,fromlen); }

int term_send(int s,char *buf,int len,int flags);
int send(int s,char *buf,int len,int flags)
{ return term_send(s,buf,len,flags); }

int term_sendto(int s,char *buf,int len,int flags,void *to,int tolen);
int sendto(int s,char *buf,int len,int flags,void *to,int tolen)
{ return term_sendto(s,buf,len,flags,to,tolen); }

int term_shutdown(int s,int how);
int shutdown(int s,int how)
{ return term_shutdown(s,how); }

int term_socket(int domain,int type,int protocol);
int socket(int domain,int type,int protocol)
{ return term_socket(domain,type,protocol); }

char *term_strerror(int errnum);
char *strerror(int errnum)
{ return term_strerror(errnum); }

int term_vfork();
int vfork()
{ return term_vfork(); }
