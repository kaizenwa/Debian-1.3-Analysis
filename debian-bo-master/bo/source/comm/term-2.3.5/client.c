#define I_IOCTL
#define I_SYS
#define I_GETOPT
#define I_STAT
#define I_SIGNAL
#define I_STRING
#define I_ARGS
#define I_INET
#include "includes.h"

#include "client.h"

char *term_server = "";

int lcompression = -1;		/* defaults. */
int rcompression = -1;
int verbose = 0;
int priority = 0;
char com_result[1024];
char *command_result=NULL;
int savedeid     = -1;
long unsigned remote_term_version = 0;
unsigned int  term_remoteuser = 0;
long unsigned term_localaddr = INADDR_ANY;
long unsigned term_remoteaddr = INADDR_ANY;
char term_localhost[256];
char term_remotehost[256];

static int try_connect_server(int S,char *server) {
  char *path;
  char *ptr=NULL;
  int s;
 
  if (!server) server = "";

  if (share) {

    if (savedeid >= 0) {
      if (share == 2)
        setuid(savedeid);
      else
        setgid(savedeid);
    }
    do {
      if ((path = get_term_path(&ptr))) {
        strcat(path,"/tmp/private");
        strcat(path,server);
        if (eaccess(path, getuid() ? (X_OK | R_OK) : 0) >= 0) break;
      }else break;
    } while ( ptr );
#ifdef _POSIX_SAVED_IDS
    if(share == 2)
      setuid(getuid());
    else
      setgid(getgid());
#endif /* _POSIX_SAVED_IDS */
  }else{
    do {
      if ((path = get_term_path(&ptr))) {
        strcat(path,"/.term");
        if (access(path, X_OK | R_OK | W_OK) >= 0) break;
      }else break;
    } while ( ptr );
  };
  if (!path) {
    sprintf(com_result, "XX%cCan't find term directory.", I_NA);
    return -1;
  }
  if (!share) 
    sprintf(&path[strlen(path)],"/socket%s", server);
  else
    strcat(&path[strlen(path)],"/socket");

				/* Try and connect to term. */
#ifdef _POSIX_SAVED_IDS
  if (savedeid >= 0)
    if (share == 2)
      setuid(savedeid);
    else
      setgid(savedeid);
#endif /* _POSIX_SAVED_IDS */
  s = connect_unix(S,path);
#ifdef _POSIX_SAVED_IDS
  if (share == 2)
    setuid(getuid());
  else
    setgid(getgid());
#endif /* _POSIX_SAVED_IDS */


  if(s<0){
    sprintf(com_result,"XX%cFailed to connect to term socket '%s'",I_NA,path);
    if (!remote_term_version) {
      memset(term_remotehost,0,sizeof(term_remotehost));
      if (term_localaddr == INADDR_ANY) 
        get_term_localaddr(inet_addr("127.0.0.1"));
      strcpy(term_remotehost,term_localhost);
      term_remoteaddr = term_localaddr;
    }
    return -1;
  };

  if (!remote_term_version) {
    memset(term_remotehost,0,sizeof(term_remotehost));
    strcpy(term_remotehost,"remotehost");
    if (send_command(s, C_STATS, 1, "%d", -10) >= 0) 
      sscanf(command_result,"VERSION %lu %lx %s %u",&remote_term_version,
        &term_remoteaddr, term_remotehost, &term_remoteuser);
    if (! remote_term_version) {
      send_command(s, C_STATS, 0, "%d", -1);	/* This is a dummy command */
      if (send_command(s, C_STATS, 1, "%d", -10) >= 0) 
        sscanf(command_result,"VERSION %lu %lx %s %u",&remote_term_version,
          &term_remoteaddr, term_remotehost, &term_remoteuser);
      if (! remote_term_version)  
        remote_term_version = 10800;
    }
  }
  if (term_remoteaddr == INADDR_ANY)
     term_remoteaddr = ntohl(inet_addr("127.0.0.254"));

  if ( lcompression >=0)
    send_command(s, C_COMPRESS, 1, "%d", lcompression);
  if (rcompression >= 0) 
    send_command(s, C_COMPRESS, 0, "%d", rcompression);

  if (priority) {
    send_command(s, C_PRIORITY, 1, "%d", priority);
    send_command(s, C_PRIORITY, 0, "%d", priority);
  }

  return s;
}

				/* parse all the options, and then */
				/* return the first unused argument. */
int client_options(int argc, char *argv[], char *myopts, Callback callback)
{
  int c, i;
  char args[200];

  command_result = &com_result[3];

  /* make sure we get all the args to pass to getopt */
  if (getenv("TERMSERVER"))
    term_server = getenv("TERMSERVER");


  strcpy(args, "t:crp:S:");
  strcat(args, myopts);

  while ((c = term_getopt(argc, argv, args))!=EOF) {
    switch(c) {
    case 't':
      term_server = term_optarg;	
      break;
    case 'c': /* for compression */
      lcompression = 1;
      rcompression = 1;
      break;
    case 'r': /* for raw, no compression */
      lcompression = 0;
      rcompression = 0;
      break;
    case 'p':
      priority = atoi(term_optarg);
      fprintf(stderr, "Changing priority to %d\n", priority);
      break;
    case 'v':
      verbose++;
      break;
    case 'S':
      if ( !(i = atoi(term_optarg) ? atoi(term_optarg) : !strcmp(term_optarg,"on"))) 
        share = 0;
      else
        share = (share == 1) ? 1 : 2;
      break;
    case '?':
      sprintf(com_result,"XX%c'?' is an illegal for some unknown reason",I_NA);
      return -1;
    default:
      if ( (callback == NULL) || ( callback(c, term_optarg) == -1 ) )
	{
	  sprintf(com_result, "XX%cUnrecognized option '%s'",I_NA,
            argv[term_optind]);
	  return -1; 
	}
      break;
    }
  }
  return term_optind;
}
				/* Send a command to the Term */
				/* process.*/ 
#ifdef USE_VARARGS
int send_command(sock , comm, local , fmt, va_alist)
int sock , comm, local;
char *fmt;
va_dcl {
#else
int send_command(int sock, int comm, int local, char *fmt, ...) {
#endif
  char buff[1024];
  va_list v;
  unsigned long version;
  int i, blocking;
  char old = 0;
#ifdef USE_VARARGS
  va_start(v);
#else
  va_start(v, fmt);
#endif
				/* A command follows.. */
  buff[0] = C_SWITCH;
				/* Is it for the local term? or the */
				/* remote term? */
  if (local)
    buff[1] = C_SWITCH - 1;
  else
    buff[1] = C_SWITCH - 2;

  if (old)
    old = com_result[3];
  else
    old = I_NA;

  sprintf(com_result,"XX%cUnsupported term option", I_NA);
  command_result = &com_result[3];

  if (comm == C_WAIT) {
    comm = C_STATS;
    if (old == I_OK || old == I_FAIL) return 0;
  }

  if (local) 
    version = VERSION;
  else
    version = remote_term_version;

	/* Here I support older versions */
  switch (comm) {
  case C_DUMP:
    if (version < 20000)
      comm = C_DUMP_OLD ;
    break; 
  case C_NAME:
    if (version < 20000)
      comm = C_NAME_OLD;
    break; 
  case C_PRIORITY:
    if (version < 20000)
      comm = C_PRIORITY_OLD ;
    break; 
  case C_RESIZE:
    if (version < 20000) 
      comm = C_RESIZE_OLD ;
    break; 
  case C_X_SERVER:
    if (version < 11500)
      comm = C_SOCKET;
    break;
  case C_GETPEERNAME:
  case C_GETSOCKNAME:
    if (version < 11714)
      return -1;
    break;
  case C_UBIND:
  case C_UDPSET:
  case C_USOCK:
    if (version < 20000) 
      return -1;
    break;
  case C_PUTENV:
    if (version < 20000) 
      return -1;
    break;
  case C_CLIENTNUM:
    return -1;
    break; 
  case C_CHMOD:
    if (version < 20055)
      return -1;
    break;
  case C_DOWNLOAD:
    if (version < 20058)
      return -1;
    break;
  case C_SETPEERNAME:
    if (version < 20154)
      return -1;
    break;
  case C_LISTEN:
    if (version < 20155)
      return 0;
    break;
  }

				/* Set the command in. */
  buff[2] = comm;
  if (fmt)
    vsprintf(buff+3, fmt, v);
  else
    buff[3] = 0;

  va_end(v);

  switch (comm) {
  case C_GETHOSTNAME:
    if (! buff[3] || !strcmp(&buff[3],"remotehost") 
        || !strcmp(&buff[3],term_remotehost)) {
      if (version > 20053 || version < 11714) { 
        sprintf(com_result,"XX%c%lx %s", I_NA,
          term_remoteaddr, term_remotehost);
        return 0;
      }
    }else if (version < 11714) return -1;
    break;
  }

  blocking = set_block(sock);
				/* Ok. Buff holds the entire command. */
				/* Lets do the damned thing.. */
  sprintf(com_result,"XX%cWrite error", I_NA);
  if (write(sock, buff, strlen(buff)+1) < 0)
    return -2;

  memset(com_result,0,sizeof(com_result));
  sprintf(com_result,"XX%cno result forthcoming", I_NA);
				/* If the command is C_DUMB, then no */
				/* reply will be forthcoming. */
  switch (comm) {
  case C_CLCLOSE:
  case C_CLOSE:
  case C_CHMOD:
  case C_DUMB:
  case C_QUIT:
  case C_DUMP:
  case C_PUTENV:
  case C_NAME:
  case C_PRIORITY:
  case C_RESIZE:
  case C_SETPEERNAME:
  case C_LISTEN:
    return 1;
  }

  i = 0;
  do {
    if(read(sock, &com_result[i], 1)<0) return -2;
  } while (com_result[i++]);

  if (!blocking) set_nonblock(sock);

  if (com_result[2] != I_OK)
    return -1;
/*  printf("command return was (%d)%s", i, com_result);*/
  return 1;
}


/* This is a short routine to allow the user to try to connect a socket
 * to term.  If a negative file descriptor is supplied, a new socket 
 * will be created.
 */

int socket_connect_server(int S,char *server) {
  int s = -1;

  sprintf(com_result,"XX%cNo error reported", I_NA);
  command_result = &com_result[3];

  set_share_mode(0,share);
	/* If "share" is still undecided, I try both shared and unshared. */
  if(share == -1){
    set_share_mode(0,1);
    s=try_connect_server(S,server);
    if ( s < 0 ) {
      set_share_mode(0,2);
      s=try_connect_server(S,server);
      if ( s < 0 ) share = 0;
    }
  };
  if ( s < 0 ) 
    s=try_connect_server(S,server);
  if ( s < 0 )
    return -1;
  return s;
}

/* This is a short routine to allow the user to try to connect to a term
 * server.  The main difference between this and socket_connect_server with
 * a negative file descriptor, is this routine will never return if the
 * connection attempt fails.
 */

int connect_server(char *server) {
  int s = -1;

  sprintf(com_result,"XX%cNo error reported", I_NA);
  command_result = &com_result[3];

  set_share_mode(0,share);
	/* If "share" is still undecided, I try both shared and unshared. */
  if (share == -1){
    set_share_mode(0,1);
    s = try_connect_server(-1,server);
    if (s < 0) {
      set_share_mode(0,2);
      s = try_connect_server(-1,server);
      if (s < 0) share = 0;
    }
  };
  if (s < 0) s = try_connect_server(-1,server);
  if (s < 0) exit(1);  	/* Do not eliminate this line... If you do not */
			/* wish to exit use socket_connect_server(-1,server) */
			/* I know it is stupid, but it is needed for */
			/* backwards compatibility. */
  return s;
}


/* build_arg: build a string from char ** argv to be passed 
 * to C_EXEC family. 
 *
 * by: croutons
 *
 * Notes:
 *  returns a pointer to malloced space.
 *  takes a null pointer as the end of the array of char*.
 *  assumes null terminated strings. (for using string(3))
 *  returns NULL on error.
 *
 *  we assume '\377' is ok for the new terminator
 */
char *build_arg( char **arg )
{
	int i, s;
	static char *f = NULL;
        char *term = NULL;

        if (f) free(f);
	if (! arg) return NULL;
        if (*arg == NULL) return NULL;

	for (s = i = 0; NULL != arg[i]; i++) s += strlen(arg[i]);

        if (remote_term_version >= 11715) {
          term = getenv("TERM");
          if (term != NULL)
            s += strlen(term) + strlen("-DTERM=") + 1;
        }
        s += i + 2;

	if ( NULL == (f = (char *)malloc( s * sizeof(char) ) ) ) 
		return NULL;

		/* This passes the terminal type */

        if (term != NULL) 
          sprintf(f,"-DTERM=%s%c",term,'\377');
        else
          f[0] = '\0';

 	for ( i = 0; NULL != arg[i]; i++ ) {	
		strcat(f,arg[i]);
                strcat(f,"\377");
        }

	return f;
}

int use_term_command(int type) {
  char *ptr=NULL;
  char *path;
  static int use_term = -1;

  switch (type) {
  case PUBLIC:
#ifdef _POSIX_SAVED_IDS
    if (savedeid < 0) 
      set_share_mode(0,share);
#endif
     break;
  case PRIVILEGED:
  default:
    set_share_mode(1,share);
    break;
  }

  if (use_term >= 0) return use_term;
  if (getenv("TERMMODE")) return (use_term=1);
  do {
    if ((path = get_term_path(&ptr))) {
      strcat(path,"/termnet");
      if (access(path, 0) >= 0) break;
      strcat(path,term_server);
      if (access(path, 0) >= 0) break;
    }else break;
  }while (ptr);
  if ((ptr = getenv("TERMDEBUG")))
    term_debug = atoi(ptr);
  return (use_term = (path != NULL));
}

