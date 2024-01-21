#define I_SYS
#define I_ERRNO
#define I_GETOPT
#define I_STRING
#define I_IOCTL
#define I_STAT
#define I_SIGNAL
#define I_TIME
#define I_WAIT
#define I_CTYPE
#define I_LIMITS
#define I_INET
#include "includes.h"

#include "debug.h"

/*
 *
 * main file. Calls everything else. ;)
 * 
 * basically consists of select() and then calls things based on the select().
 *
 */
/*------------------------------------------------------------------------*/

#define INFINITE_BAUD 115200	/* Nothing is really unlimited.  This should */
				/* be the maximum baudrate you could achieve */
				/* when running with baudrate = 0.  As you */
				/* We need some sort of limit so baudrate 0 */
				/* doesn't use 30-40% of the cpu time. */
				/* Note: This # is really just accurate to */
				/* an order of magnitude. */
#define FUDGE_FACTOR 3

/* truly global vars. Used everywhere. */
unsigned long current_time = 0;
int do_shutdown	 = 0;
int term_debug = 0;
int compressing = 1;
struct Onetime onetime_term[MAX_CLIENTS+1];
/*------------------------------------------------------------------------*/
/* Various global variables. Note that most of these vars are local to    */
/* this file. Where possible (i.e. where it doesn't impinge on efficiency) */
/* this is enforced */
int remote = 0;

struct Client clients[MAX_CLIENTS];
int num_clients = 0;
unsigned long baudrate = 1;
int max_cps = 0;
int auto_retrain = 0; /* Don't automatically adjust timing values */
int use_term_socket = 1; /* This can be deactivated for a one-way term */
int bytes_left = _POSIX_PIPE_BUF; /* bytes available to send now.. */
int fudge_flow = 0; /* should we generate periodic control-Q's */
int byte_shift = 0; /* So we can map less frequently uses section down to */
                    /* 0-32 */
int window_size_max = -1; /* This will automatically be adjusted */

int window_size = 1; /* Don't change this */
unsigned long packet_timeout = 70; /* wait 3.5 seconds for a packet to time out */
int write_noise = 0; /* whether we should print out all the the serial stuff */
				/* we get that we don't understand.. */

int seven_bit_in = 0;		/* Are we on a line that ignores the */
				/* top bit. */
float stop_bits = 1;		/* # of stop bits being used */
static long unsigned delay = 20000;
				/* This is a 20000 micro-second delay to decrease */ 
				/* cpu usage. */
static int collisions = 0;	/* Use this if you have problems with data */
				/* Collisions */
int term_inc = 20;		/* This is the # of times / second term will */
				/* attempt to send data */
int in_mask = 255;
int seven_bit_out = 0;
int out_mask = 255;
int block_size = -1;		/* This is the size of block the serial devices uses */
int packet_len = -1;
int breakout_char = '0';
char breakout_string[256];
static int quiet = 0, rc_quiet = 0;
int user_share = -1;
int modem_noise = -1;
static int hangup_on_exit = 0;	/* tell local modem to hang up on exit */
static int explicit_hangup = 1;	/* local modem needs ath0 after +++ to hangup */
static int frequency; 	/* This determines the minimum delay frequency */

int stat_modem_in = 0,
  stat_modem_recv = 0,
  stat_modem_out = 0,
  stat_modem_ack = 0,
  stat_cooked_in = 0,
  stat_cooked_out = 0,
  stat_rare_out = 0;

char *share_p = NULL;
char *home_p = NULL;

int rshtype = 0;  /* Default to allow rsh and use login shell_command */

static char *private_path = NULL;
static char *private_path_end = NULL;
static char socket_path[PATH_MAX] = "\0";
static char ownroot[PATH_MAX] = "\0";
static char shell_command[PATH_MAX] = "\0";
char escapes[256];
char ignores[256];

unsigned int term_port = 0;

int modem_in = 0, modem_out = 1, modem_sock = -1;
/*------------------------------------------------------------------------*/
/* module function prototypes */

void get_term_localaddr(unsigned long);
void read_int_type(int *o, char *c);
void read_ulong_type(unsigned long *o, char *c);
void var_init(void);
void main_loop(int);
void main_init(void);
void do_link_in(void);
void do_link_out(void);
void clear_buffers(struct Client *cl);
/*------------------------------------------------------------------------*/
/* main. */


void read_int_type(int *o, char *c) {

	/* This reads an integer with "on" == 1, "off" == 0, and default == 1 */

  if (isdigit(*c))
    *o = atoi(c);
  else if (! strncmp(c, "tcp", 3)) 
    *o = 2;
  else
    *o = !!strncmp(c, "off", 3);
}
        
void read_ulong_type(unsigned long *o, char *c) {

	/* This reads an integer with "on" == 1, "off" == 0, and default == 1 */

  if (isdigit(*c))
    sscanf(c,"%lu",o);
  else
    *o = !!strncmp(c, "off", 3);
}
       
void var_init(void) {
  int k;
  extern int unix_domain;

#ifdef TERM_NFS_DIR
  unix_domain = 0;
#else
  if (use_term_socket == 2) unix_domain = 0;
#endif

  frequency = CLK_TCK;
  get_term_localaddr(inet_addr("127.0.0.1"));
  memset(term_remotehost, 0, sizeof(term_remotehost));
  strcpy(term_remotehost,"remotehost");

  for(k=0;k <= MAX_CLIENTS;k++) {
    onetime_term[k].socket = -1;
    onetime_term[k].timeout = 0;
    onetime_term[k].owner = -1;
    onetime_term[k].errors = 0;
  }
  if (! share) onetime_term[MAX_CLIENTS].owner = getuid();

  if (fudge_flow < 0)
    fudge_flow = 0;
  if (byte_shift < 0 || byte_shift > 255)
    byte_shift = 0;
  if (packet_timeout < 10 || packet_timeout > 1000){
    fprintf(stderr, "Invalid timeout value (%lu).\r\n", packet_timeout);
    auto_retrain = 1;
    packet_timeout = 70;
  }

  if (strlen(breakout_string) < 5) 
    sprintf(breakout_string,"%c%c%c%c%c",
      breakout_char,breakout_char,breakout_char,breakout_char,breakout_char);

  if ( seven_bit_out ) {
    out_mask = 127;
    tok_byte_mask_out = 127;
    tok_byte_width_out = 7;
  }

  if (seven_bit_in) {
    in_mask = 127;
    tok_byte_mask_in = 127;
    tok_byte_width_in = 7;
  }

  if (baudrate == 1) {
    baudrate = terminal_baud(modem_out);
  }else if (baudrate < 300 && baudrate) {
    baudrate = 300;
    fprintf(stderr, "baudrate set too low. Reset to 300\r\n");
  }else if (baudrate >= INFINITE_BAUD) 
    baudrate = 0;

  max_cps = baudrate ? (2 * baudrate) /
    (unsigned long) ((int)(0.5 + 2 * stop_bits) + (tok_byte_width_out << 1))
    : 0;  /* avoiding round-off problems */

  if ( block_size < 0) block_size = (max_cps) ? 512 : PIPE_BUFFER;
  if ( block_size < 127 || block_size > PIPE_BUFFER ) {
    fprintf(stderr, "Invalid packet length %d (must be 1..%d) resetting to %d\n",
      block_size, PIPE_BUFFER, 512);
    block_size = 512;
  }
  if (max_cps && block_size > max_cps) block_size = max_cps;

  if (packet_len < 0) 
    packet_len = out_mask + 1;
  else if (packet_len < 1 || packet_len > out_mask+1) {
    packet_len = out_mask + 1;
    fprintf(stderr, "packetsize has to be 1..%d.  Reset to %d\r\n", 
      packet_len, packet_len);
  }


  if ( !max_cps ) 
    term_inc = INFINITE_BAUD /
      (unsigned long) ((block_size > 512) ? 512 : block_size);
 
  if ( term_inc < 20 ) term_inc = 20;

  if ( term_inc < FUDGE_FACTOR * (max_cps / (2 * packet_len + 20))) {
    k = term_inc;
    term_inc = FUDGE_FACTOR * (max_cps / (2 * packet_len + 20)); 
    if (k == 20) packet_timeout = (packet_timeout * term_inc) / k;
  }

  if ( delay > 400000 / (unsigned long) term_inc)
    delay = 400000 / (unsigned long) term_inc;

  if ( max_cps / (packet_len + 10) > 0.9 * frequency ) delay = 0;

  if (window_size_max < 0 ) {
    window_size_max = 2 + (max_cps / 199);
    if (window_size_max >= N_PACKETS / 2 || window_size_max < 3)
      window_size_max = (N_PACKETS / 2) - 1;
  }else if (window_size_max > N_PACKETS / 2 ) {
    fprintf(stderr, "Invalid window size %d\r\n", window_size_max);
     window_size_max = (N_PACKETS / 2) - 1;
  }else if (window_size_max < 2) {
    fprintf(stderr, "Invalid window size %d\r\n", window_size_max);
    window_size_max = 2;
  }
  if (write_noise && write_noise < quiet + 1) write_noise += quiet;
  if (modem_noise < 0) 
    modem_noise = remote;
}

void main_init(void) {
  int i;

  for (i = 0; i < MAX_CLIENTS;++i) {
    clients[i].fd = -1;
    clients[i].state = -1;
    clients[i].in_buff.data = NULL;
    clients[i].out_buff.data = NULL;
    clients[i].in_buff.alloced = 0;
    clients[i].out_buff.alloced = 0;
  }
}
				/* In case of sudden death, do some */
				/* minimal clean up. */
void sig_quit(int dummy) {
  if (use_term_socket) unlink(socket_path);
  if (private_path) {
    *private_path_end = 0;
    rmdir(private_path);
  }
  set_block(0);
  set_block(1);
  terminal_restore(0,0);
  terminal_restore(1,0);
  set_block(modem_in);
  set_block(modem_out);
  exit(0);
}
				/* Drop a core. */
void sig_core(int dummy) {
  if (use_term_socket) unlink(socket_path);
  if (private_path) {
    *private_path_end = 0;
    rmdir(private_path);
  }
  set_block(0);
  set_block(1);
  terminal_restore(0,0);
  terminal_restore(1,0);
  set_block(modem_in);
  set_block(modem_out);
  abort();
}

				/* This closes the modem connection. */
void sig_hup(int dummy) { 
  if (term_port < 1)
    sig_quit(0);
  if (modem_in >= 0) 
    x__close(modem_in);
  modem_in = modem_out = -1;
}

				/* We ignore this signal. There was */
				/* some problem with it on linux. A */
				/* bit odd, and I can't track it down. */
				/* I suspect it is a bad shell_command. So we */
				/* just ignore it. */
void sig_ignore(int dummy) {
  signal(SIGALRM, sig_ignore);
  signal(SIGPIPE, sig_ignore);
}

void sig_child(int dummy) {
  int p, i;
#ifdef USE_WAITPID
  int stat_loc;

  p = waitpid((pid_t)-1, &stat_loc, WNOHANG);
#else
  p = wait3(0, WNOHANG, 0);
#endif /* SVR4 */
#ifndef SYSV
  signal(SIGCHLD, sig_child);
#endif /* SYSV */

  if (p < 1) return;
  DEBUG_MAIN(stderr, "%s:sig child kicked\n", term_server);
  for (i = 0; i < MAX_CLIENTS;++i) {
    if (clients[i].pid == p && clients[i].state > 0) {
#ifdef USE_NOEOF /* ++kay: let term read until EOF */
      clients[i].state = 2;
#endif
      return;
    }
  }
  /* Hmm. child that we don't know about died!?!?! */
  if (dummy)
    dummy = 0;
}

void read_rc(char *p) {
  FILE *f = NULL;
  char *file = NULL, *ptr = NULL;
  static int Chdir[256];
  static int seven_bit=0, login_type = 0;
#define INT_TYPE 1
#define CHAR_TYPE 2
#define RANGE_TYPE 3
#define ULONG_TYPE 4
#define FLOAT_TYPE 5
  static char int_type=INT_TYPE, char_type=CHAR_TYPE, range_type=RANGE_TYPE,
	ulong_type=ULONG_TYPE, float_type=FLOAT_TYPE;
#define RC_OPTIONS_SIZE 96
  int m, n, i;
  char *where="/.term";
  static char *rc_options[RC_OPTIONS_SIZE] = {
	"compress",	& int_type,	(char *) &compressing,
	"breakout",	& int_type,	(char *) &breakout_char,
	"remote",	& int_type,	(char *) &remote,
	"chdir",	& char_type,	(char *) Chdir,
	"escape",	& range_type,	(char *) escapes,
	"ignore",	& range_type,	(char *) ignores,
	"baudrate",	& ulong_type,	(char *) &baudrate,
	"retrain",	& int_type,	(char *) &auto_retrain,
	"shift",	& int_type,	(char *) &byte_shift,
	"window",	& int_type,	(char *) &window_size_max,
	"timeout",	& int_type,	(char *) &packet_timeout,
	"noise",	& int_type,	(char *) &write_noise,	
	"sevenbit",	& int_type,	(char *) &seven_bit,	
	"seven_out",	& int_type,	(char *) &seven_bit_out,
	"seven_in",	& int_type,	(char *) &seven_bit_in,
	"flowcontrol",	& int_type,	(char *) &fudge_flow,
	"login",	& int_type,	(char *) &login_type,	
	"denyrsh",	& int_type,	(char *) &rshtype,
	"chroot",	& char_type,	(char *) ownroot,
	"collisions",	& int_type,	(char *) &collisions,
	"increment",	& int_type,	(char *) &term_inc,
	"stopbits",	& float_type,	(char *) &stop_bits,
	"quiet",	& int_type,	(char *) &rc_quiet,
	"share",	& int_type,	(char *) &user_share,
	"shell",	& char_type,	(char *) shell_command,
	"blocksize",	& int_type,	(char *) &block_size,
	"packetsize",	& int_type,	(char *) &packet_len,
	"terminate",	& char_type,	(char *) breakout_string,
	"hangup_on_exit", & int_type,	(char *) &hangup_on_exit,
	"explicit_hangup", & int_type,	(char *) &explicit_hangup,
	"socket",	& int_type,	(char *) &use_term_socket,
	"frequency",	& int_type,	(char *) &frequency
      };

#define DEFAULT_PATH "/usr/local/lib/term:/usr/lib/term:/usr/etc:/etc" 

  set_share_mode(0,share);
  if(share == -1){
    share = (getgid() != getegid());
    set_share_mode(0,share);
  }

  setuid(geteuid());
  if (share != 1)
    setgid(getgid());
  else if (savedeid >= 0)
    setgid(savedeid);

  for(i=!! share;! file && i < 2 ; ++i) {
    ptr = NULL;
    if (i == 1) where = NULL;
    do {
      if ((file = get_term_path(&ptr))) {
        if (where)
          strcat(file, where);
        strcat(file, "/termrc");
        if (p != NULL) if ( p[0] )
          sprintf(&file[strlen(file)], ".%s",p);
        if (! eaccess(file, R_OK)) break;
      }else break;
    } while ( ptr );
  }

  if (file) {
    if (!quiet) 
      fprintf(stderr,"Reading file:  %s\r\n",file);
    f = fopen(file, "r");
  }

#ifdef _POSIX_SAVED_IDS
  if (share) {
    if(share == 2) 
      setuid(getuid());
    else
      setgid(getgid());
  }
#endif
  if (!file || !f) return;

  if (! quiet) quiet = rc_quiet;  
  Chdir[0] = '\0';
  while (!feof(f)) {
    char line[256];

    fgets(line,	256,	f);
				/* skip blank lines + comments. */
    if (!line[0] || line[0] == '\n' || line[0] == '#')
      continue;
			

    for(m = 0;m < RC_OPTIONS_SIZE;m += 3)
      if ((n = strlen((char *) rc_options[m])) < strlen(line))
        if (! strncmp((char *) rc_options[m], line, n)  
            && isspace(line[n])) {

      while (isspace(line[++n]));
 
      switch ( (int) *rc_options[m+1] ){
        case CHAR_TYPE:
        {
          int i, quote=0;
          if (  line[n] == '\47' ||
                line[n] == '"' ) {
            quote = line[n++];
            for(i=0;i<256 && line[n] != quote && line[n];++i,++n){
              if ( (rc_options[m+2][i] = line[n]) == '\\' && line[n+1] )
                rc_options[m+2][i] = line[++n];
            };
          }else {
            for(i=0;i<256 && ! isspace(line[n]) && line[n];++i,++n){
              if ( (rc_options[m+2][i] = line[n]) == '\\' && line[n+1] )
                rc_options[m+2][i] = line[++n];
            };
          };
          rc_options[m+2][i] = 0;
          break;
        }
        case RANGE_TYPE:
        {
          char *p;
          int i,j;
      
          if(strchr(&line[n],'#'))
            *strchr(&line[n],'#') = '\0';
          i = atoi(&line[n]);	/* get the number following. */
          if (i < 0 || i > 255) {	/* check for sanity. */
            fprintf(stderr, "Invalid escape/ignore %d in termrc\n", i);
            continue;
          }
          if ((p = strchr(&line[n], '-')) != NULL) { /* See if this is a */
  					     /* range.. */
            while (isspace(*++p));	/* skip whitespace. Note that it */
				/* automatically skips the '-'. */
            j = atoi(p);		/* if it is, then get the second number. */
            if (j < 0 || j > 255) {	/* sanity check again. */
              fprintf(stderr, "Invalid range limit %d in termrc\n", j);
              continue;
            }
            for (;i != j; i = (i+1) & 255) /* Ok. mark all the characters */
				/* in the range as being escaped. */
              rc_options[m+2][i] = '\1';   
          }
          else rc_options[m+2][i] = '\1';	/* else just set the one character as */
				/* to be escaped. */
          break;
        }

        case INT_TYPE:
          read_int_type((int *) rc_options[m+2],&line[n]);
          break;
        case ULONG_TYPE:
	{
	  if(strchr(&line[n],'#'))
	    *strchr(&line[n],'#') = '\0';
	  read_ulong_type((unsigned long *) rc_options[m+2],&line[n]);
          break;
        }
        case FLOAT_TYPE:
	{
	  if(strchr(&line[n],'#'))
	    *strchr(&line[n],'#') = '\0';
	  sscanf(&line[n],"%f",(float *) rc_options[m+2]); 
          break;
        }
      };
      break;	
    };
    if (m == RC_OPTIONS_SIZE){
      if(! strncmp("remote",line,6)){
         remote = 1;
      }else{
        fprintf(stderr, "Unrecognized line in %s\r\n",file);
        fprintf(stderr, "\t%s\r\n",line);
      }
    }
  }
  if(f != NULL) fclose(f);

  if (Chdir[0] != '\0')
    chdir((char *) Chdir);

  if(seven_bit){
    seven_bit_in = 1;
    seven_bit_out = 1;
  }

  if (! login_type && !rshtype)
    rshtype = -1;

  if (!user_share) {
    share = 0;
    savedeid = -1;
  }else if (user_share > 0) {
    share = (share == 1) ? 1 : 2;
  };

#if 0 /* Useful for debugging */
  for(m = 0;m < RC_OPTIONS_SIZE;m += 3)
    switch ( (int) *rc_options[m+1] ){
    case INT_TYPE:
    {
      int *o;
      o = (int *) rc_options[m+2];
      fprintf(stderr,"%s %d\r\n",rc_options[m],*o);
      break;
    }
  }
#endif
}

#define GETOPT_OPTS "Aac:d:f:l:n:ors:t:u:v:w:p:P:S:q"

int main(int argc, char *argv[]) 
{
  
  int s, c, i, hangup;
  char *ptr=NULL, *path=NULL;
  int umask_old;

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif

  breakout_string[0] = 0;
  term_opterr = 0;
  while ((c = term_getopt (argc, argv, GETOPT_OPTS)) != EOF) 
    switch(c) {
    case 'S':
      read_int_type(&user_share,term_optarg);
      if (!user_share) share=0;
      break;
    case 'q':
      ++quiet;
      break;
  }; term_optind = 1;

  if (quiet <= 1)
    fprintf(stderr, "Term version: %s\r\n",str_version(VERSION));

  /* initialize character escaping. */
  for (i = 0; i < 256;i++) {
    ignores[i] = 0;
    escapes[i] = 0;
  }

  escapes['^'] = 1;

  main_init();

  if (!term_server) {
    term_server=getenv("TERMSERVER");
    if (!term_server) term_server = "";
  }

  /* read in the termrc file. */
  read_rc(0);

  if(share > 0)
    fprintf(stderr, "Using shared mode.\r\n");

  /* then check env variables. */
  setbuf(stderr, 0);
  if (getenv("BAUDRATE")) 
    read_ulong_type(&baudrate,getenv("BAUDRATE"));

  /* Then check command line options */
  /*
    The code to parse the command line was written by the
    one and only Muhammad Saggaf. If you have any question
    about Linux, networking, Unix, or life in general, 
    don't ask him mate! :). Chances are he doesn't know the 
    answer.
    */
  
  term_opterr = 0;
  
  while ((c = term_getopt (argc, argv, GETOPT_OPTS)) !=EOF)
    switch(c) {
    case 'A':
      auto_retrain = 1;
      break;
    case 'a':
      {
	seven_bit_in = seven_bit_out = 1;
	break;
      }
    case 'f': 
      read_int_type(&fudge_flow,term_optarg);
      break;
    case 'l':
      i = open(term_optarg, O_RDWR | O_CREAT | O_TRUNC, 0644);
      if (i < 0) {
	x__perror("open");
	fprintf(stderr, "Unable to open log file %s\r\n", term_optarg);
	break;
      }else {
        modem_noise = 0;
      }
      if (i != 2) {
	x__close(2);		/* Just to make sure.. */
	x__dup2(i, 2);
	x__close(i);
      }
      break;
    case 'p':
      term_port=atoi(term_optarg);
      if(modem_in >= 0) x__close(modem_in);
      if(modem_out >= 0) x__close(modem_out);
      modem_in = modem_out = -1;
      break;
    case 'r': remote = 1; break;
    case 't':
      read_ulong_type(&packet_timeout,term_optarg);
      break;
    case 'v':
				/* add a device to the list. */
      i = open(term_optarg, O_RDWR);
      if (i < 0) {
	x__perror("open");
	fprintf(stderr,"Unable to open modem device %s\r\n", term_optarg);
	break;
      }
      if(modem_in>=0) x__close(modem_in);
      if(modem_out>=0) x__close(modem_out);
      modem_in = modem_out = i;
      break;
    case 'w':
      read_int_type(&window_size_max,term_optarg);
      break;
    case 'd': 
      read_int_type(&term_debug,term_optarg);
      fprintf(stderr, "Debugging = %x\n", term_debug);
      break;
    case 'c':
      read_int_type(&compressing,term_optarg);
      break;
    case 'n':
      read_int_type(&write_noise,term_optarg);
      break;
    case 's':
      read_ulong_type(&baudrate,term_optarg);
      break;
    case 'u':
      read_int_type(&use_term_socket,term_optarg);
      break;
    case 'q':
    case 'S':
      break;
    default:
      fprintf(stderr, "unrecognized or incomplete argument '%s'. Exiting\r\n",  argv[term_optind-1]);
    }

  if (term_optind < argc)
    term_server = argv[term_optind];
  
  for (s = 0; s < MAX_CLIENTS;++s) {
    clients[s].fd = -1;
  }

				/* Read in a specific termrc. */
  if (term_server[0])
    read_rc(term_server);

  { char tmp[1024];
    sprintf(tmp,"TERMSERVER=%s",term_server);
    term_putenv(tmp);
  }

  if (! use_term_socket) {
    DEBUG_FP(stderr,"WARNING: Term will be inaccessible on this side");
    s = -1;
  }else if (!share) {
    do {
      if ((path = get_term_path(&ptr))) {
        strcat(path,"/.term");
        mkdir(path,0700);
        if (access(path, X_OK | R_OK | W_OK) < 0) continue;
        if ((chmod(path,0700)  < 0)) {
          if (!quiet) 
            fprintf(stderr,"Can't set permissions on %s to 0700\r\n",
              path);
          continue;
        }
        break;
      }else break;
    } while ( ptr );
    if (use_term_socket && ! path) {
      fprintf(stderr, "Failed to find any of:\r\n");
      ptr = NULL;
      do {
        if ((path = get_term_path(&ptr))) {
          strcat(path,"/.term");
          fprintf(stderr,"\t%s\r\n",path);
        }else break;
      } while ( ptr );
      exit(1);
    }
    strcat(path,"/socket");
    strcat(path,term_server);
  }else{
    if (savedeid >= 0){ 
      if (share == 2)
        setuid(savedeid);
      else
        setgid(savedeid);
    }

/* set the protection to be explicitly what I define below */
    umask_old=umask(0);

    do {
      if ((path = get_term_path(&ptr))) {
        if (eaccess(path, getuid() ? (X_OK | R_OK) : 0) < 0) continue;
        break;
      }else break;
    } while ( ptr );
    if (! path) {
      fprintf(stderr, "Failed to find any of:\r\n");
      ptr = NULL;
      do {
        if ((path = get_term_path(&ptr))) {
          fprintf(stderr,"\t%s\r\n",path);
        }else break;
      } while ( ptr );
      exit(1);
    }

/* Create a link to "." as ".term" for backwards compatibility */

    i=strlen(path);
    strcat(path,"/.term");
    if (eaccess(path, X_OK | R_OK ) < 0) 
      symlink("./tmp/private",path);
    path[i]='\0';

/* If the needed directories don't exist, create them */
    strcat(path,"/tmp");
    mkdir(path,0777);

/* Make sure we can access the directory */

    if (eaccess(path, getuid() ?  (X_OK | W_OK | R_OK) : 0) < 0) 
      if (chmod(path,0777) < 0) {
      fprintf(stderr, "Can't read, write, &/or execute %s\r\n", path);
      exit(1);
    }

/* It seems that the protection on sock_unix is ignored, so we need a 
   a make a subdirectory, that only term can see. */

    strcat(path,"/private");
    strcat(path,term_server);

/* Remove any old sockets */
    private_path = path;
    private_path_end = &path[strlen(private_path)];
    strcat(path,"/socket");
    unlink(path);

/* Remove and recreate the private directory, so we can be sure we can see it */
    *private_path_end = 0;
    rmdir(private_path);
    if (share == 2) {
      mkdir(private_path, 0700|S_ISUID);
      chmod(private_path, 0700|S_ISUID);
    }else {
      mkdir(private_path, 0770|S_ISGID);
      chmod(private_path, 0770|S_ISGID);
    }
  
/* Finally make sure we can access the directory */

    if (eaccess(private_path, getuid() ? (X_OK | W_OK | R_OK) : 0) < 0) {
      fprintf(stderr, "Can't read, write, &/or execute %s\r\n",
        private_path);
      exit(1);
    }

    strcat(path,"/socket");

/* Restore the mask to what the user wants to use with tupload. */
    umask(umask_old);
  }

  /* initialize variables */

  var_init();

/* It doesn't hurt to unlink twice ... */  
  if (use_term_socket) {
    unlink(path);
    strcpy(socket_path,path);
    if ((s = bind_unix(path)) < 0) exit(1);
  }else {
    s = -1;
  }

  setuid(getuid());
  setgid(getgid());

  /* init modules */
 
  serial_init();
  compress_init();
  update_time();

  if (ownroot[0]) {
    if (x__chroot(ownroot) < 0) {
      x__perror("chroot");
      if (rshtype > 0) {
        fprintf(stderr, "Your chroot failed.  Exiting rather than risking invasion\n");
        exit(1);
      }
    }
  }else if (rshtype > 0) {
    if (!quiet) 
      fprintf(stderr, "Warning: If you use chroot, you probably want denyrsh"
        "as well\n");
  }
  
#ifdef SYSV
  sigset(SIGCHLD, sig_child);
#else
  signal(SIGCHLD, sig_child);
#endif /* SYSV */
  signal(SIGHUP, sig_hup);
  signal(SIGPIPE, sig_ignore);
  signal(SIGINT, sig_quit);
  signal(SIGQUIT, sig_quit);
  signal(SIGIOT, sig_core);
  signal(SIGSEGV, sig_core);
  signal(SIGALRM, sig_ignore);
  signal(SIGBUS, sig_ignore);

  terminal_save(0);
  if (s >= 0) set_nonblock(s);

  if (modem_in >= 0 || modem_out >= 0 ) {
#ifndef NO_TTYNAME
	/* Some OS's insist that streams must be opened with O_RDWR... */
    if (modem_in != modem_out) {
      int t;
      if (modem_in >= 0 && isatty(modem_in) &&
          (t = open(ttyname(modem_in),O_RDWR)) >= 0 ) {
        dup2(t,modem_in);
        close(t);
      }
      if (modem_out >= 0 && isatty(modem_out) &&
          (t = open(ttyname(modem_out),O_RDWR)) >= 0 ) {
        dup2(t,modem_out);
        close(t);
      }
    }
#endif
    set_nonblock(modem_in);
    set_nonblock(modem_out);
    terminal_raw(modem_in);
    terminal_raw(modem_out);
  }

  main_loop(s);

  if (private_path) {
    *private_path_end = 0;
    rmdir(private_path);
  }
  	/* with C_QUIT 2 or if hangup_on_exit and not C_QUIT 1 */
  hangup = do_shutdown == 2 || (hangup_on_exit && do_shutdown!=1);

  if (use_term_socket) unlink(path);
  set_block(modem_in);
  set_block(modem_out);
  terminal_restore(0,hangup);
  terminal_restore(1,hangup);

  if (s >= 0) set_block(s);

	/* If terminal_restore didn't hangup, this might. */
	/* This is way too specific for my blood.  This assumes your modem */
	/* will respond to +++ and the AT command set.  In most cases with */
	/* an annex port, it won't work. */
  if (!remote && hangup) {
    /* we've already slept 2 secs in terminal_restore 0 and 1 */
    if (write(modem_out, "+++", 3) > 0) {
      sleep(2);
      if (explicit_hangup) 
        write(modem_out, "ath0\r", 5);
    }
  }
  exit(0);
}

/*-----------------------------------------------------------------------*/
void check_client(int cl, int ret) {
  DEBUG_MAIN(stderr, "%s: termerrno == %d\n", term_server, termerrno);
  if (!termerrno && checkfd(clients[cl].fd) >= 0) return;
  if (clients[cl].state == 3 ) {
    DEBUG_FP(stderr, "%s:truncating out_buff\n", term_server);
    clients[cl].out_buff.size = 0;
    clients[cl].out_buff.start = clients[cl].out_buff.end;
    return;
  }
#if 0
  if (ret < 0)
    x__perror("client gave this");
#endif
				/* Ok. Close the descriptor. */
  if(clients[cl].fd) x__close(clients[cl].fd);
  clients[cl].fd = -1;
				/* And go to state 2. */
  clients[cl].state = 2;
}

int new_client(int fd, int uid) {
  int j;

  for (j = remote; j < MAX_CLIENTS;j += 2)
    if (clients[j].fd < 0 && clients[j].state < 0) break;

  if (j == MAX_CLIENTS) return -1; /* not maximum clients */

  DEBUG_FP(stderr, "%s: new client %d.\n", term_server, j);
  
  clear_buffers(&clients[j]);

  clients[j].fd = fd;
  clients[j].type = T_SMART | T_RDFILE | T_WRFILE;
  clients[j].udp_type = 0; 
  clients[j].udp_size = 0; 
  clients[j].dump_count = 0;
  clients[j].cl_type = CL_SOCKET;
  clients[j].compress = compressing;
  clients[j].state = 1;
  clients[j].c_state = 0;
  clients[j].number = j;
  clients[j].priority = 0;
  clients[j].queue = 0;
  clients[j].name[0] = 0;
  clients[j].parent = -1;
  clients[j].peername.sin_port = ~0;
  clients[j].peername.sin_addr.s_addr = htonl(term_localaddr);
  clients[j].peername.sin_family = ~0;
  clients[j].owner = uid;
  set_nonblock(fd);
  return j;
}
/*------------------------------------------------------------------------*/
/* Main loop. Hangs around waiting for things to get ready, and calls to  */
/* appropriate routines. 						  */

void main_loop(int sock) {
  struct timeval timeout;
  fd_set reads, writes, excepts;
  int i, j, k, empty = 1, csocket = -1, bytes_received = 0, script_fd = -1;
  int max;
  int on = 1;
  int cerrors = 0, select_error = 0;

  while (!do_shutdown) {
    max = -1;

/* First thing we do is make sure we have a modem connection */
    if (modem_in < 0 || modem_out < 0 || checkfd(modem_in) < 0) {
      if (term_port < 1) {
	exit(1);
      }else {
        struct sockaddr_in addr_in;
        int din = sizeof(addr_in);
        if(modem_in>=0) x__close(modem_in);
        modem_in = modem_out = -1;
        if ((modem_sock=bind_tcp_listen(term_port,1)) < 0) {
          if (modem_sock == -1) {
            fprintf(stderr,"Port %u is already bound\n",term_port);
          }else {
            fprintf(stderr, "Can't listen to port %u\n",term_port);
          }
          exit(1);
        }
        fprintf(stderr, "Listening to port %u for a modem connection\n",term_port);
        while ((modem_in = modem_out = 
          x__accept(modem_sock, (struct sockaddr *) &addr_in, &din)) < 0)
          x__perror("Accept from modem");
        set_nonblock(modem_in);
        setsockopt(modem_sock, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
        x__close(modem_sock);
        modem_sock = -1;
        setsockopt(modem_in, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
        fprintf(stderr,"Accepted modem connection\n");
      }
    }

/* If the serial out buffer is empty, try and put something in it */
    if (bytes_left > 0 && serial_out.size < bytes_left) {
      DEBUG_SER(stderr, "%s: doing serial out\n",
	       term_server);
      do_serial_out(0);
      if (!serial_out.size)
	do_link_out();
      if (!serial_out.size) 
	do_serial_out(0);
      DEBUG_SER(stderr, "%s:  serial out size %d\n",
	       term_server, serial_out.size);
    } 

/* Set up client stuff */
/* We select to read if: */
/*	The input buffer is empty */
/* We select to write if: */
/*      The output buffer is not empty */
/*      We're waiting for a connection to complete (state == 5) */

    FD_ZERO(&reads);
    FD_ZERO(&writes);
    FD_ZERO(&excepts);
    
    if (p_in_num && empty)
      do_link_in();
    
    for (i = 0; i < MAX_CLIENTS;++i) {
				/* If it's closing down, and the */
				/* buffers are empty, then kill it. */
      if ((clients[i].state == 3 || clients[i].state == 4) &&
	  (clients[i].type & T_UDP ||
           (!clients[i].in_buff.size && !clients[i].out_buff.size ))) {
        int l;

	DEBUG_FP(stderr, "%s:real close %d %d %d\n", term_server , i,
		 clients[i].state, clients[i].fd);
        for(l=0;l<MAX_CLIENTS;++l)
          if(clients[l].parent == i && clients[l].state == 1 )
            clients[l].state = 3;
	if (clients[i].fd > 0) x__close(clients[i].fd);
	clients[i].fd = -1;
	if (clients[i].state == 3)
	  clients[i].state = -1; 
	else clients[i].state = 1;
	continue;
      }
				/* If it's a file, we don't need to */
				/* select() on it. */
      if (clients[i].cl_type == CL_FILE) continue;
      if (select_error) check_client(i,0);
      if (clients[i].fd < 0) continue; /* If it's not a file, and */
				       /* there is no fd, then no */
				       /* select(). */
      if (clients[i].in_buff.size < 2 * packet_len &&
            (clients[i].type & T_RDFILE) && clients[i].state == 1) {
	FD_SET(clients[i].fd, &reads);
	if(max < clients[i].fd) max = clients[i].fd;
      }
      if ((clients[i].out_buff.size && (clients[i].type & T_WRFILE)) ||
	  (clients[i].state == 5)) {
	FD_SET(clients[i].fd, &writes);
	if(max < clients[i].fd) max = clients[i].fd;
      }
      if(FD_ISSET(clients[i].fd, &reads)||FD_ISSET(clients[i].fd, &writes))
        FD_SET(clients[i].fd, &excepts);
    }
    
/* Select for socket and modem */
/* We select read for socket if we aren't at the maximum number */
/* of clients */
    if (sock >= 0 && num_clients < MAX_CLIENTS 
        && (! select_error || (sock=checkfd(sock)) >= 0)) {
      FD_SET(sock, &reads);
      FD_SET(sock, &excepts);
      if (max < sock) max = sock;
      for (k=0;k < MAX_CLIENTS;k++) if(onetime_term[k].socket >= 0
        && (! select_error 
           || (onetime_term[k].socket=checkfd(onetime_term[k].socket)) >= 0)) {
        FD_SET(onetime_term[k].socket, &reads);
        FD_SET(onetime_term[k].socket, &excepts);
	if (max < onetime_term[k].socket) max = onetime_term[k].socket;
      }
    }

/* We select for read on the modem if the serial in buffer is empty */
    if (!serial_in.size) {
      FD_SET(modem_in, &reads);
      FD_SET(modem_in, &excepts);
      if (max < modem_in) max = modem_in;
    }

/* Now if there is anything in the serial out buffer, select for writing */

    if (serial_out.size && bytes_left > 0) {
      DEBUG_SER(stderr, "%s: dso: fd set\n",
	       term_server);
      FD_SET(modem_out, &writes);
      FD_SET(modem_out, &excepts);
      if (max < modem_out)  max = modem_out;
    }

/* We need to call the script from the main loop, since we want term up */
/* and running first. To avoid sending data too soon, the script won't run */
/* until the first term command has been sent by the user. This may cause */
/* some confusion, if for example the script is 'txconn' and the first */
/* command is 'trsh -s xterm', but I can't think of a way to avoid this. */

    if (shell_command[0] && remote_term_version) { 
      script_fd = open_socket(shell_command); 
      set_nonblock(script_fd);
      shell_command[0] = 0;
    }
    if (script_fd >= 0 
      && (! select_error || (script_fd=checkfd(script_fd)) >= 0)) {
      FD_SET(script_fd, &reads);
      if (max < script_fd) max = script_fd;
    }

/* This is intended as a short delay to help decrease CPU usage. */
    timeout.tv_sec = 0;
    timeout.tv_usec = (unsigned long int) delay;
    if(max_cps > 0 || max < 0) select(0, NULL, NULL, NULL, &timeout); 

/* Set the timeout value for select(). */
    timeout.tv_sec = 0;
    timeout.tv_usec = 500000; /* 0.5 seconds */

/* do select() */	
    if (max > -1) if (select(max+1, &reads, &writes, &excepts, &timeout) < 0) {
				/* This is perfectly normal. Things */
				/* that send signals will cause select */
				/* to exit with an error. */

      if (++cerrors < 10) 
        x__perror("select");
      select_error = 1;
      continue;
    }	
    select_error = 0;

/* Update current_time. This is maintained in 20th s of a second */
    update_time();

/* start checking to see what's ready and what's not */

/* See if it is just the script */

    if (script_fd >= 0 &&
       (FD_ISSET(script_fd, &reads)||FD_ISSET(script_fd, &excepts))) {
      memset(shell_command,0,sizeof(shell_command));
      j = read(script_fd, &shell_command[1], sizeof(shell_command)-1);
      if ( j <= 0 && errno != ERR_BLOCK) {
        x__close(script_fd);
        script_fd = -1;
      }else {   /* I copy this exactly to stderr */
        if (write_noise < 2) fwrite(&shell_command[1],j,sizeof(char),stderr);
      }
    }

/* Can we read from modem  ?? */
    bytes_received = 0;
    if (!serial_in.size && 
      (FD_ISSET(modem_in, &reads) || FD_ISSET(modem_in, &excepts))) {
      j = read_into_buff(modem_in, &serial_in, 0);
      if ( (j < 0 || (!j && termerrno && term_port > 0)) && 
        (errno != ERR_BLOCK || term_port > 0)) {
	x__perror("read from modem");
	x__close(modem_in);
	modem_in = modem_out = -1;
	continue;
      }
      else{
        stat_modem_in += j;
        if (collisions) bytes_received = j;
      }
    }

/* Now we process any new data before writing out to the modem. */

    if (serial_in.size) 
      do_serial_in();

/* test for new client */

    if (sock >= 0) for(k=0;k <= MAX_CLIENTS;k++){
      switch (k){
        case MAX_CLIENTS:
          csocket = sock;
          break;
        default:
          if (onetime_term[k].socket > 0 && 
              current_time > onetime_term[k].timeout) {
            x__close(onetime_term[k].socket);
            onetime_term[k].socket = -1;
          }
          csocket = onetime_term[k].socket;
          break;
      };
      if(csocket<0) continue;
      if (FD_ISSET(csocket, &reads)) { /* try for a connect. */

        struct sockaddr_in addr_in;
        int uid = -1;

#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
 	i = CheckClientConnection(csocket);
	if (i != -2) {			/* we have a streams pipe */
		if (i == -1) {
			fprintf(stderr, "can't add client\n");
			continue;
		} /* else fall through to add new client after "accept ..." */
	} else 		/* not a streams pipe */
#endif
        {
          int din = sizeof(addr_in);
          if ((i = x__accept(csocket , (struct sockaddr *) &addr_in, &din)) >= 0) {
            if (addr_in.sin_family == AF_INET) {
              char *user = NULL;

              if (( addr_in.sin_addr.s_addr != htonl(term_localaddr)
                    && addr_in.sin_addr.s_addr != INADDR_ANY 
                    && addr_in.sin_addr.s_addr != inet_addr("127.0.0.1") )
                 ||( (user = getconnname(i)) != NULL
                    && (uid = getuserid(user)) != onetime_term[k].owner
                    && uid != -1
                    && uid != 0
                    && onetime_term[k].owner != -1 )) {
                x__close(i);
                i = -1;
                fprintf(stderr,"%s: rejected connection from user %s@%s\n",
                  term_server, user ? user : "unknown", inet_ntoa(addr_in.sin_addr));
              }else {
                DEBUG_FP(stderr,"%s: accepted connection %s@%s:%u\n",
                  term_server, user ? user : "unknown", inet_ntoa(addr_in.sin_addr),
		  (unsigned int) ntohs(addr_in.sin_port));
              }
            }else {
              DEBUG_FP(stderr,"%s: accept connection from unix domain\n",
                term_server);
            } 
          }else {
            x__perror("Accept");
            if (++onetime_term[k].errors > 100) {
              if (k < MAX_CLIENTS) x__close(csocket); 
              onetime_term[k].socket = -1;
              onetime_term[k].errors = 0;
            }
            continue;
          }
        }
        if (k < MAX_CLIENTS) {
	  int on=1;
          onetime_term[k].socket = -1;
          x__close(csocket);
	  setsockopt(i, SOL_SOCKET, SO_REUSEADDR,
            (char *)&on, sizeof(on));
        }
        
        if (i >= 0) { /* a new client */
	  if((j = new_client(i,uid)) < 0) 
            x__close(i);
          else
            memcpy(&clients[i].peername,&addr_in,sizeof(clients[i].peername));
        }
        else if (termerrno != 1) {
#if 0
	  x__perror("accept");
#endif
        }
      }
    }

/* test for data being read from clients */
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd < 0) continue;
      if (clients[i].in_buff.size >= 2*packet_len) continue;
      switch (clients[i].cl_type) {
      case CL_CHILD:		/* fall through */
      case CL_SOCKET:		/* fall through */
#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
      case CL_SPIPE:
#endif
	if (!FD_ISSET(clients[i].fd, &reads) &&
	    !FD_ISSET(clients[i].fd, &excepts))
	  continue;
				/* Fall through. */
	if (!(clients[i].type & T_RDFILE)) continue;
        if (clients[i].type & T_UDP)
          j = recvfrom_into_buff(&clients[i]);
        else
	  j = read_into_buff(clients[i].fd, &clients[i].in_buff, 0); 
	DEBUG_FP(stderr, "%s:read %d bytes from client %d\n",
		 term_server , j, i);
				/* Did this client start sending?? */
	DEBUG_FP(stderr, "%s:j %d, inf_buff.size %d\n",
		 term_server , j, clients[i].in_buff.size);

				/* Hmmm. Something errored. Lets take */
				/* a look... */
	if (j <= 0)
	  check_client(i, j);
	if (termerrno == 1)
	  check_client(i, j);
	break;
      case CL_FILE:
	if (!(clients[i].type & T_RDFILE)) continue;
	if (! clients[i].dump_count) {
          x__close(clients[i].fd);
          clients[i].fd = -1;
          clients[i].cl_type = CL_SOCKET;
          clients[i].type = T_RDFILE | T_WRFILE;
          clients[i].state = 1;
          continue;
        }
	j = read_into_buff(clients[i].fd, &clients[i].in_buff,
          (clients[i].dump_count > 32767) ? 32767 
          : (int) clients[i].dump_count); 
	DEBUG_FP(stderr, "%s:read %d bytes from file client %d\n",
		 term_server , j, i);
	DEBUG_FP(stderr, "%s:j %d, inf_buff.size %d\n",
		 term_server , j, clients[i].in_buff.size);
	if (j <= 0)
	  check_client(i, j);
	if (termerrno == 1) termerrno = 0;
	break;
      case CL_BOUND:
	if (!FD_ISSET(clients[i].fd, &reads)) continue;
	{
	  un_char num[10]; int k;
				/* We have an accept ready... */
	  clients[i].type &= ~T_RDFILE; /* Don't try reading it until */
				/* the accept is done. */
	  sprintf((char *) num, "%d", i);
	  for (k =0 ;num[k];++k)
	    add_to_buffer(&clients[i].in_buff, num[k]);
	  add_to_buffer(&clients[i].in_buff, 0);
	}
	break;
      }
    } /* for clients loop */
    
#ifndef USE_CONNBLOCK
/* Test pending connections. */
    for (i = 0; i < MAX_CLIENTS; ++i) {
      if (clients[i].state == 5) {
	char	dummy;

	if (FD_ISSET(clients[i].fd, &writes)) {
          void ret_ok(struct Client *, int);

	  ret_ok(&clients[i], 0);
		/* safe to call from here? */
	  clients[i].state = 1;
	}
	else if (read(clients[i].fd, &dummy, 0) < 0) {
	  void ret_fail(struct Client *, int, int, char *);
	  switch (errno) {
          case ERR_BLOCK:
	  case ENOTCONN:	/* no status, but maybe timed out */
	    if (current_time >= clients[i].timeout) {
	      errno = ETIMEDOUT;
	      ret_fail(&clients[i], 0, 1, 
                "connect() timed out");
	      x__perror("async connect()");
	      clients[i].state = 2;
	    }
	    break;
	  default:
	    ret_fail(&clients[i], 0, 1,
              "connect() timed out");
	    x__perror("async connect()");
	    clients[i].state = 2;
	    break;
	  }
	}
      }
    }
#endif

/* test for data being sent to clients */
    empty = 1;
    for (i = 0; i < MAX_CLIENTS;++i) {
      if (clients[i].fd < 0) continue;
      if (!clients[i].out_buff.size) continue;
      if (clients[i].type & T_UDP &&
        clients[i].out_buff.size < clients[i].udp_size ) continue;
      switch (clients[i].cl_type) {
      case CL_CHILD:
      case CL_SOCKET:
#if defined(STREAMS_PIPE) || defined(X_STREAMS_PIPE)
      case CL_SPIPE:
#endif
	if  (!FD_ISSET(clients[i].fd, &writes)) continue;
      case CL_FILE:
	if (!(clients[i].type & T_WRFILE)) continue;
				/* something there! :) */
        if (clients[i].type & T_UDP) {
          j = sendto_from_buff(&clients[i]);
          if (!j) break;
        }else
  	  j = write_from_buff(clients[i].fd, &clients[i].out_buff,0);
	DEBUG_FP(stderr, "%s:write %d bytes to client %d\n", 
		 term_server, j, i);
	if (j <= 0)
	  check_client(i, j);
	else stat_cooked_in += j;
	break;
      case CL_BOUND:
	break;
      }				/* switch */
    }				/* for clients loop */
    
/* Ok. Can we write to modem ??? */
    if (FD_ISSET(modem_out, &writes) && bytes_left > 0) {
      int t = bytes_left;
      if (t > serial_out.size)
	t = serial_out.size;
      if (t > block_size)
        t = block_size;
      DEBUG_SER(stderr, "%s: dso: write from buff\n",
	       term_server);
      j = write_from_buff(modem_out, &serial_out, t);
      if (j < 1 && termerrno != 1) {
	errno = termerrno - 1;
	x__perror("write to modem");
	x__close(modem_out);
	modem_in = modem_out = -1;
	continue;
      }else {
	stat_modem_out += j;
        bytes_left -= j + bytes_received + (modem_noise ? modem_noise - 1 : 0);
      }
    }

  } /* while loop */

  if (script_fd >= 0) x__close(script_fd);
 
} /* function */

