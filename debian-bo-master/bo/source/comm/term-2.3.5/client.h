#ifndef CLIENT_H
#define CLIENT_H
#include "config.h"
#include "terminal.h"

#ifndef un_char
#define un_char unsigned char
#endif

struct Buffer {
    un_char * data;
    int start, end;
    int size;
    int alloced;
    };

#define C_SWITCH        '@'
#define PUBLIC  0
#define PRIVILEGED 1

/* Client commands.  If you need to reserve a something for a patch you
 * are working on, let me know.  If we run out, we can start going to
 * two character commands for less common commands.  If you don't absolutely
 * need to add something don't as this breaks forward compatibility. 
 * (forcing people to upgrade...)
 *
 *   (bcr@physics.purdue.edu)
 */

/* First the obsolete switches */
#define C_WAIT		'\0'	/* This is just a delay */
#define C_PRIORITY_OLD  'B'	/* Raise/lower the priority of this */
				/* client. */
#define C_NAME_OLD      'G'	/* Set the name of this client. Only */
				/* used by C_STATS. */
#define C_RESIZE_OLD    'H'	/* For handling SIGWINCH */
#define C_DUMP_OLD      'L'	/* Go dumb for the next 'n' bytes. */
#define C_CLIENTNUM     'I'	/* Not used. Obsolete. */

/* Now the current ones */

#define C_OPEN		'1'	/* Open a file a file for uploading */
				/* without truncating it. */
#define	C_CLOSE		'2'	/* Close a connection. */
#define C_EXEC		'3'	/* Not used/implemented. */
#define C_PTYEXEC	'4'	/* Fork() a shell attached to a pty. */
#define C_DUMB		'5'	/* Go dumb. Escape all '@' received, */
				/* so no commands are processed. */
#define C_UPLOAD	'6'	/* Open a file for writing, creating */
				/* it if it doesn't exist, truncating */
				/* it if it does. */
#define C_DOWNLOAD	'7'	/* Open a file for reading. */
#define C_UNLINK	'Z'	/* Delete a file */
#define C_SOCKET        '8'	/* Connect to a unix-domain socket. */
#define C_PUTENV        '9'	/* Puts an environmental variable */
#define C_PORT          'A'	/* Connect to a TCP/IP port on the */
				/* specified host, and port number. */
#define C_PRIORITY      'd'	/* Raise/lower the priority of this */
				/* client. */
#define C_COMPRESS	'C'  	/* note, i jumped B :) (croutons). */
				/* Turn compression on or off. */
#define C_STAT          'D'	/* Get information on a remote file. */
#define C_SEEK          'E'	/* Execute an lseek() on the remote */
				/* file descriptor. Assumes the remote */
				/* is a file handle, and not a socket. */
#define C_STATS         'F'	/* Get information on various parts of */
				/* term.  */
#define C_NAME          'g'	/* Set the name of this client. Only */
				/* used by C_STATS. */
#define C_RESIZE        'h'	/* For handling SIGWINCH */
#define C_BIND          'J'	/* Bind a remote socket. */
#define C_ACCEPT        'K'	/* Accept a connection from a remotely */
				/* bound sockets. */
#define C_DUMP          'l'	/* Go dumb for the next 'n' bytes. */
#define C_CLCLOSE       'M'     /* Close the remote fd when the */
				/* buffers have been emptied. */
#define C_QUIT          'N'	/* Shutdown term. */
#define C_CHMOD		'O'	/* Change the mode of a file */
#define C_BINDN         'U'     /* Bind a remote unspecified socket. -ot */
#define C_BINDS         'S'     /* Allow connects one time only from a tcpip */
                                /* socket... */
#define C_UBIND         'T'     /* Bind a UDP socket */
#define C_USOCK         'V'     /* Create a UDP socket */
#define C_UDPSET        'W'     /* set UDP parameters */
#define C_X_SERVER	'X'	/* Open connection to X server */

#define C_GETSOCKNAME   'a'     /* Get the sockaddr */
#define C_GETPEERNAME   'b'     /* Get the peername */
#define C_GETHOSTNAME   'c'     /* Get the hostname */
#define C_SETPEERNAME   'e'     /* Set the peername */
#define C_LISTEN        'f'     /* Listens for connections */

/* Return status */

#define I_FAIL		'a'
#define I_CLOSE		'b'
#define I_EXIT		'c'
#define I_OK		'd'
#define I_NA		'X'

#define HEADER_SIZE 6

/* Prototypes */

int read_into_buff(int fd, struct Buffer *, int);
int write_from_buff(int fd, struct Buffer *, int);
int write_from_buff_async(int fd, struct Buffer *, int);


void set_share_mode(int, int);
int socket_connect_server(int,char *);
int connect_server(char *);

void set_nonblock(int);
int set_block(int);
int checkfd(int);
int checkstream(int,int);

void set_ttyraw(int);
void set_ttynormal(int);

typedef int (*Callback) ( char, char *);

int client_options(int argc, char *argv[], char *myopts, Callback callback);

#ifdef USE_VARARGS
int send_command();
#else
int send_command(int, int, int, char *, ...);
#endif

char * build_arg(char**);
extern int priority;
extern int verbose;

int open_unix(char *);
int connect_unix(int,char *);
int duplicate_connect(int, int);
int bind_tcp(unsigned int);
int bind_tcp_listen(unsigned int, int);
int bind_unix(char *);
int use_term_command(int);
void do_select_loop(int, int, int);
int getuserid(char *);
char *getconnname(int);
int getpassid(char *);
extern char *term_server;

extern char *command_result;

int eaccess(char *pathname, int mode);

void do_connect(int num, int *svs, int (*get_server)(int,struct sockaddr *)); /* -ot */

char *get_term_path(char **);

int term_putenv(char *);

void get_term_localaddr(unsigned long);

struct hostent *host_lookup(char *,int,int,int,struct hostent *);

void term_do_exit(void);
#if !defined(SHAREDIR) || !defined(_LIBC)
#ifdef I_INET
int term_getpeername(int, struct sockaddr *, int *);
int term_getsockname(int, struct sockaddr *, int *);
int term_bind(int, struct sockaddr *, int);
int term_accept(int, struct sockaddr *, int *);
int term_connect(int, struct sockaddr *, int);
#ifndef linux
int term_recvfrom( int, char *, int, unsigned int, struct sockaddr *, int *);
int term_sendto(int, void *, int, unsigned int, struct sockaddr *, int); 
#else
int term_recvfrom( int, void *, int, unsigned int, struct sockaddr *, int *);
int term_sendto(int, const void *, int, unsigned int, const struct sockaddr *, int); 
#endif
int term_gethostname(char *, size_t);
#endif
int term_shutdown(int, int);
int term_close(int);
int term_listen(int, int);
int term_socket(int, int, int);
int term_fork(void);
int term_vfork(void);
#ifndef linux
int term_recv(int, char *, int, unsigned int);
int term_rcmd(char **, unsigned short, char *, char *, char *, int *);
struct hostent *term_gethostbyname(char *);
#else
int term_recv(int, void *, int, unsigned int);
int term_rcmd(char **, unsigned short, const char *, const char *, const char *, int *);
struct hostent *term_gethostbyname(const char *);
#endif
void term_exit(int);
#endif

#endif

