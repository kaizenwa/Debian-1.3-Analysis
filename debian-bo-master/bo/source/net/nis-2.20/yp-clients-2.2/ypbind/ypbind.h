/*
 * $Log: ypbind.h,v $
 * Revision 1.1.1.1  1997/01/23 20:48:30  hjl
 * Import ypbind 3.0.
 *
 * Revision 1.3  1996/10/08 09:58:31  swen
 * Updated for release 3.0
 *
 * Revision 1.2  1995/11/15 10:06:31  swen
 * Added SYSV-IPC (from Michael Rausch <mrausch@ernie.mi.uni-koeln.de>)
 * Make mmap (not MAP_ANON) work again with latest kernels.
 *
 * Revision 1.1  1995/07/25  14:28:02  swen
 * ypbind version 2.0.
 *
 * Revision 2.4  1995/01/24  12:24:32  swen
 * Added RCS keywords.
 *
 */

#ifndef BINDINGDIR
#define BINDINGDIR "/var/yp/binding"
#endif
#define USE_BINDINGDIR 1
#ifndef _PATH_YPCONF
#define _PATH_YPCONF "/etc/yp.conf"
#endif
#ifndef _PATH_VARRUN
#ifdef SOLARIS
#define _PATH_VARRUN "/var/yp/binding/"
#else
#define _PATH_VARRUN "/var/run/"
#endif
#endif
#define _PATH_YPPIDFILE _PATH_VARRUN"ypbind.pid"
#define _MAXDOMAIN  8
#define _MAXSERVER  3
#define PING_INTERVAL     10 /* check binding every 10 seconds */
#define YPSET_NO	0
#define YPSET_LOCAL	1
#define YPSET_ALL	2

#define MMAP_SHARED_ANON_OK 0     /* can we share anonymous regions? (not yet in 2.0.21) */

struct bound_server
{
  char *host;
  struct in_addr server_addr;
  unsigned short int server_port;
  long int version;
  bool_t use_broadcast;
  CLIENT *client_handle;
  bool_t filled;
};

struct binding
{
  char domain[YPMAXDOMAIN];
  bool_t is_bound;
  bool_t is_alive;
  int lockfd;
  int active; /* index into server */
  struct bound_server server[_MAXSERVER];
};

typedef char *domainname;

extern struct ypservers *ypconf_read(const char *);

extern bool_t xdr_domainname_ypbind(XDR *, char *);

bool_t eachresult(bool_t *, struct sockaddr_in *);
void parse_config_file(const char *);
void broadcast(struct binding *);
void add_server(char *, struct sockaddr_in *, CLIENT *, const char *, bool_t);
void sighandler(int);
void toggle_debug(int);
void handle_hangup(int);
void terminate(void);
int get_entry(char *, struct binding *);
void update_entry(struct binding *);
int open_lockfile(void);
struct binding *find_entry(char *dom);
pid_t start_slave (void);
void bindto_server (char *, char *);
void ping_server(struct binding *, int);
void write_lock_binding(void);
void read_lock_binding(void);
void un_lock_read_binding(void);
void un_lock_write_binding(void);
void inststr(char **, int, const char *);
void init_binding(void);
void check_binding(void);
void create_pidfile(void);
void init_master_slave_communication(void);
void terminate_master_slave_communication(void);
