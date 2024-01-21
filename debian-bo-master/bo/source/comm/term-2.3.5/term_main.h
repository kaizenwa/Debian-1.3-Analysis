
#include "client.h"

#if defined(_AIX) || defined(sgi)
#define SANITY(a) ((a) ? (void) 0 : (fprintf(stderr, "fatal: line %d file %s\n", \
 __LINE__, __FILE__), abort(), (void) 1))
#else
#define SANITY(a) ((a) ? 0 : (fprintf(stderr, "fatal: line %d file %s\n", \
 __LINE__, __FILE__), abort(),1)) /* '1' keeps Ultrix MIPS compiler happy */
#endif

/* #define BUFFER_SIZE     2048 Already defined in client.h */
/* global junk */
#define MAX_CLIENTS 	64
#define N_PACKETS       32
#define SWITCH		'@'

#define MAX_TYPE	6 /* There are 6 different types of packets */
/* these are: */
/* 0	uncompress packet */
/* 1	compressed packet */
/* 2    packed (old style sevenbit) packet */
/* 3	ack packet */
/* 4	super ack packet */
/* 5    packed (sevenbit) packet */

/* main.c */
extern unsigned long current_time, packet_timeout, remote_term_version,
  term_localaddr, term_remoteaddr;

extern char term_localhost[256], term_remotehost[256];
extern unsigned int term_remoteuser;
extern int
	remote,
	clients_waiting,
        term_debug, 
        rshtype,
        window_size,
        window_size_max,
        write_noise,
        byte_shift,
        termerrno,
        seven_bit_in,
        seven_bit_out,
        in_mask,
        out_mask,
        packet_len,
	block_size,
        max_cps,
        auto_retrain,
        use_term_socket,
        stat_comp_in,
        stat_comp_out,
        stat_modem_in,
        stat_modem_recv,
        stat_modem_out,
        stat_modem_ack,
	savedeid,
	share,
        term_inc;

extern char escapes[], ignores[];

#define T_SMART		1
#define T_LOCAL         2
#define T_RDFILE	4
#define T_WRFILE	8

#define T_UDP		16 /* tell term to use recvfrom_i_b and sendto_f_b instead of read_i_b etc */

/* clients[x].udp_type.. tells the client what to do 
 * with the incoming/outgoing udp packets
 */

#define UDP_RECADD 1
#define UDP_RECKEEP 2
#define UDP_SENDSTRP 4

#define UDP_T_SENDSTRIPHDR 1   /* strip header before sendto()ing... */
                               /* basically for the remote term... */
                               /* for now we use it on both ends. I'd like to
                                  use this for proper setting of host when recvfrom()'ing */
#define UDP_T_SENDIGNOREHDR 2  /* ignore the address in the header when sendto()ing */
#define UDP_T_RECADDHDR 4      /* add a header when receiving packets */

/* RECADDHDR and SENDSTRIPHDR are there in case the termnet-compiled program wants to handle 
 * the header itself. eg: for correct assignment of address in recvfrom (coming soon :)
 */


#define CL_CHILD        1
#define CL_SOCKET       2
#define CL_FILE         3
#define CL_BOUND        4
#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
#define CL_SPIPE	5
#endif

extern struct Client {
    struct Buffer in_buff,
		out_buff,
		in_msg,
		out_msg;
    int fd;			/* socket file descriptor. */
    int priority;		/* How many packets may be queued */
    int queue;
    int old_timeout;		/* The timeout before this one (Debugging) */
    int cl_type;		/* What type of fd are we watching. */
    int type;			/* dumb or smart */
				/* This basically gets down to being */
				/* remote or not, but Clients can */
				/* lower their status. */
    long dump_count;
    int state;			/* one of */
				/* not used */
				/* alive, or closing down */
				/* 5 = connection pending */
    unsigned long timeout;	/* What time the connection will time out */
    int compress;		/* does this client want compression? */
    int pid;
    int c_state;
    un_char control[1024];
    int c_len;
    int number;
    char name[20];


    unsigned long udp_host;	/* address to send the next incoming packet to */
    int udp_port;		/* in HOST format! must hton[sl] into struct sockaddr_in's */

    int udp_type; 		/* what to do with incoming, outgoing packets */
    int udp_size; 		/* size of outgoing udp packets */
    int parent;			/* This is needed for udp sockets, so they  */
				/* have some way of knowing when to close. */
    int owner;

    struct sockaddr_in peername; /* This is where the socket is connected from */

    } clients[];

extern int num_clients;
extern int do_shutdown;
extern struct Onetime {
    int socket;
    int owner;
    int errors;
    unsigned long timeout;
    } onetime_term[MAX_CLIENTS+1];

/* serial.c */
struct Packet_out {
    un_char data[262];
    int type;		/* flag for compressed or not */
    int len;
    unsigned long timeout;	/* What time it will timeout */
    unsigned long q_time;	/* What time the packet was queued */
    int trans;
    int *queue;
    };

struct Packet_in {
    un_char data[262];
    int type;
    int len;
    };

extern struct Packet_out p_out[N_PACKETS];
extern int p_out_s, p_out_e;
extern int p_out_num, p_mismatch;

extern struct Packet_in p_in[N_PACKETS+1];
extern int p_in_e;
extern int p_in_num;

extern struct Buffer serial_out,
     serial_in;

extern void do_serial_in(void);
extern void do_serial_out(int );
extern void serial_init(void);
extern int get_client_byte(void);
extern char *term_server;

extern int tok_byte_mask_in, tok_byte_width_in;
extern int tok_byte_mask_out, tok_byte_width_out;

#if 1
#define noise(a) do_noise(a)
#define alert(b) do_alert(b)
#else
#define noise(a)
#define alert(a)
#endif

/* misc.c */
extern void update_time(void);
extern void do_debug(int lev, char *c);

extern void set_nonblock(int);
extern int set_block(int);
#define log(a, b) debug(a, b)

extern void do_noise(int);
extern void do_alert(char *);

extern char ** rebuild_arg(char *);

/* pty.c */
extern void pty_init(void);
extern void setup_term(int);
extern int open_pty(char *);
extern int open_socket(char *);
extern int pty_pid;
/* compress.c */
int compress(un_char *, int, int);
int uncompress(un_char *data , int len , un_char *outpath);
int compress_init(void);

int write_from_buff(int, struct Buffer *, int size);
int read_into_buff(int, struct Buffer *, int size);
int add_to_buffer(struct Buffer *B, un_char);
int get_from_buffer(struct Buffer *);
int recvfrom_into_buff(struct Client *);
int sendto_from_buff(struct Client *); 

/* system.c */
int open_system(char *);

/* checksum.c */
unsigned short check_sum(un_char *d, int len, int mask);
unsigned short update_crc(unsigned short old, unsigned char c);

/* meta.c */
int meta_state(int);

/* sevenbit.c */
int e_2_s_put(un_char *out, un_char data, int key);
int s_2_e_buff(un_char *data, un_char *out, int len);

/* utils.c */
char *x__strerror(int errno);
char *str_version(unsigned long int version);
unsigned short file_crc(char *fname, long nbytes);

