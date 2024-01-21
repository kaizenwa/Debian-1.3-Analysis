/*  VER 045  TAB P   $Id: proto.h,v 1.16 1996/11/22 15:45:42 src Exp $
 *
 *  function prototypes
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

/* doit.c */
int doit();

/* despool.c */
void despool(char *article_name);
char *despool_line(void);

/* putarticle.c */
char *submit_article(char *articlename);

/* lock.c */
void lock(char *dir_name,char *lock_name);
void unlock(void);
void build_filename(char *where,char *arg1,...);  
void unlock_exit(int n);

/* logmsg.c */
void log_open();
void log_msg(int type, const char *fmt, ...);

/* server.c */
int open_server(char *host,char *port);
void close_server();
int get_server_nntp(char *line, int size);
int get_server_msg(char *line, int size);
void put_server(char *line);
int put_server_msg(char *line);

/* socket.c */
int socket_open(char *hostname, char *service, SOCKET_D *sock);
void socket_close(SOCKET_D *sock);
int get_socket(char *line, int size, SOCKET_D *sock);
int put_socket(char *line, SOCKET_D *sock);

/* getarticle.c */
int select_group(char *group,long *firstp,long *lastp);
int fetch_article(long where,FILE *f);

/* pull.c */
void pull_cleanup(void);
void pull(char *hostname);

/* active.c */
int is_active(char *group);
int unseen_active(char *group);
void load_active(void);

/* sys.c */
void load_sys(void);
int sys_allow(char *group);

/* auth.c */
void get_authinfo(char *file);
void do_authinfo(char *username,char *password);
void do_mode_reader();

/* history.c */
void history_done(void);
int history_lookup(char *msgid);

/* msgid.c */
int new_msgid(char *msgid);

/* hash.c */
int hashindex(char *key,int hashlen);

/* exec.c */
int script(char *program, int in, int out);
void program_open(char *program, SOCKET_D *sock);

/* telnet.c */
int read_telnet(char *line, int size, SOCKET_D *sock);
int write_telnet(char *line, SOCKET_D *sock);

/* script.c */
void chat_update(char *line,SOCKET_D *sock);

/* proto.h */
void begintitle(int argc, char **argv, char **envp);
void settitle(char *name);
void progtitle(char *what);
void progtitle2(char *what,char *more);
