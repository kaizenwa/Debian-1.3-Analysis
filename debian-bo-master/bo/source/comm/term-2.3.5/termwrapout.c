/* provide x__xxxxx stubs which will be called by the specially compiled
 * libtermnet (which included "termwrap.h") instead of the real libsocket and
 * libnsl functions.
 */

/* this only works on Solaris, so make it so... */
#if !(defined(sun) && (defined(SVR4) || defined(__SVR4) || defined(__svr4__)))
#error "termwrap.so only works on Solaris 2.x, sorry."
#endif



/* Here we just use standard interposing. See the Solaris "Linker and Libraries
 * Answerbook" for details. dlsym(RTLD_NEXT,"xxxxx") is the key to it all.
 *
 * To avoid retyping it every time, a simple CPP macro will suffice...
 *
 *   INTERPOSE(name) returns the address of the real version of
 *                   function "name" in the void* variable f.
 */
#include <sys/types.h>
#include <dlfcn.h>
#include <stdio.h>

#define INTERPOSE(name)						\
  static void *f = NULL;					\
  if (!f)							\
  {								\
    f = dlsym(RTLD_NEXT,name);					\
    if (!f)							\
    {								\
      fprintf(stderr,"dlopen(\"%s\"): %s\n",name,dlerror());	\
      exit(1);							\
    }								\
  }



/* note that we don't particularly care about types here, so everything
 * comes down to either a basic type or a void*
 */

int x__accept(int s,void *addr,int *addrlen)
{
  INTERPOSE("accept");
  {
    int (*real_accept)(int,void*,int*) = (int(*)(int,void*,int*))f;
    return real_accept(s,addr,addrlen);
  }
}

int x__bind(int s,void *name,int namelen)
{
  INTERPOSE("bind");
  {
    int (*real_bind)(int,void*,int) = (int(*)(int,void*,int))f;
    return real_bind(s,name,namelen);
  }
}

int x__chroot(const char *path)
{
  INTERPOSE("chroot");
  {
    int (*real_chroot)(const char*) = (int(*)(const char*))f;
    return real_chroot(path);
  }
}

int x__close(int fd)
{
  INTERPOSE("close");
  {
    int (*real_close)(int) = (int(*)(int))f;
    return real_close(fd);
  }
}

int x__connect(int s,void *name,int namelen)
{
  INTERPOSE("connect");
  {
    int (*real_connect)(int,void*,int) = (int(*)(int,void*,int))f;
    return real_connect(s,name,namelen);
  }
}

int x__dup(int fd)
{
  INTERPOSE("dup");
  {
    int (*real_dup)(int) = (int(*)(int))f;
    return real_dup(fd);
  }
}

int x__dup2(int fd1,int fd2)
{
  INTERPOSE("dup2");
  {
    int (*real_dup2)(int,int) = (int(*)(int,int))f;
    return real_dup2(fd1,fd2);
  }
}

int x__fcntl(int fd,int cmd,void *arg)
{
  INTERPOSE("fcntl");
  {
    int (*real_fcntl)(int,int,void*) = (int(*)(int,int,void*))f;
    return real_fcntl(fd,cmd,arg);
  }
}

int x__fork()
{
  INTERPOSE("fork");
  {
    int (*real_fork)() = (int(*)())f;
    return real_fork();
  }
}

void *x__gethostbyname(const char *name)
{
  INTERPOSE("gethostbyname");
  {
    void *(*real_gethostbyname)(const char*) = (void*(*)(const char*))f;
    return real_gethostbyname(name);
  }
}

void *x__gethostbyaddr(const char *addr,int len,int type)
{
  INTERPOSE("gethostbyaddr");
  {
    void *(*real_gethostbyaddr)(const char*,int,int) =
                                             (void *(*)(const char*,int,int))f;
    return real_gethostbyaddr(addr,len,type);
  }
}

int x__gethostname(char *name,int namelen)
{
  INTERPOSE("gethostname");
  {
    int (*real_gethostname)(char*,int) = (int(*)(char*,int))f;
    return real_gethostname(name,namelen);
  }
}

int x__getpeername(int s,void *name,int *namelen)
{
  INTERPOSE("getpeername");
  {
    int (*real_getpeername)(int,void*,int*) = (int(*)(int,void*,int*))f;
    return real_getpeername(s,name,namelen);
  }
}

int x__getsockname(int s,void *name,int *namelen)
{
  INTERPOSE("getsockname");
  {
    int (*real_getsockname)(int,void*,int*) = (int(*)(int,void*,int*))f;
    return real_getsockname(s,name,namelen);
  }
}

int x__listen(int s,int backlog)
{
  INTERPOSE("listen");
  {
    int (*real_listen)(int,int) = (int(*)(int,int))f;
    return real_listen(s,backlog);
  }
}

void x__perror(const char *s)
{
  INTERPOSE("perror");
  {
    void (*real_perror)(const char*) = (void(*)(const char*))f;
    real_perror(s);
  }
}

int x__rcmd(char **ahost,unsigned short inport,char *luser,
         char *ruser,char *cmd,int *fd2p)
{
  INTERPOSE("rcmd");
  {
    int (*real_rcmd)(char**,unsigned short,char*,char*,char*,int*) =
                        (int(*)(char**,unsigned short,char*,char*,char*,int*))f;
    return real_rcmd(ahost,inport,luser,ruser,cmd,fd2p);
  }
}

int x__recv(int s,char *buf,int len,int flags)
{
  INTERPOSE("recv");
  {
    int (*real_recv)(int,char*,int,int) = (int(*)(int,char*,int,int))f;
    return real_recv(s,buf,len,flags);
  }
}

int x__recvfrom(int s,char *buf,int len,int flags,void *from,int *fromlen)
{
  INTERPOSE("recvfrom");
  {
    int (*real_recvfrom)(int,char*,int,int,void*,int*) =
                                        (int(*)(int,char*,int,int,void*,int*))f;
    return real_recvfrom(s,buf,len,flags,from,fromlen);
  }
}

int x__send(int s,char *buf,int len,int flags)
{
  INTERPOSE("send");
  {
    int (*real_send)(int,char*,int,int) = (int(*)(int,char*,int,int))f;
    return real_send(s,buf,len,flags);
  }
}

int x__sendto(int s,char *buf,int len,int flags,void *to,int tolen)
{
  INTERPOSE("sendto");
  {
    int (*real_sendto)(int,char*,int,int,void*,int) =
                                         (int(*)(int,char*,int,int,void*,int))f;
    return real_sendto(s,buf,len,flags,to,tolen);
  }
}

int x__shutdown(int s,int how)
{
  INTERPOSE("shutdown");
  {
    int (*real_shutdown)(int,int) = (int(*)(int,int))f;
    return real_shutdown(s,how);
  }
}

int x__socket(int domain,int type,int protocol)
{
  INTERPOSE("socket");
  {
    int (*real_socket)(int,int,int) = (int(*)(int,int,int))f;
    return real_socket(domain,type,protocol);
  }
}

char *x__strerror(int errnum)
{
  INTERPOSE("strerror");
  {
    char *(*real_strerror)(int) = (char*(*)(int))f;
    return real_strerror(errnum);
  }
}

int x__vfork()
{
  INTERPOSE("vfork");
  {
    int (*real_vfork)() = (int(*)())f;
    return real_vfork();
  }
}
