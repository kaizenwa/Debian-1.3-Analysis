#define I_STRING
#define I_CTYPE
#define I_SIGNAL
#define I_TIME
#define I_LIMITS
#include "includes.h"

#include "terminal.h"

/*
 initially written by Michael O'Reilly
 rewritten by jeff grills (jefftep@cs.utexas.edu)

 to use, run remotely like:   linecheck remote.out
             locally:         linecheck stderr < /dev/modem > /dev/modem

 linecheck's first argument must be the name of a file to write the information
 into.  if you specify "stderr," then the output is written to the standard error
 stream, as normal.  this is what you prefer to do on your local machine so you can
 watch it work.  on the remote machine, you need to specify a file to record this
 information in.

 you can tell linecheck not to test certain chars by listing their decimal number
 on the command line.  for instance, if you know flow-control will get eaten,
 you can use "linecheck stderr 17 19", and it won't test those chars.  this is also
 useful for testing a set of escape sequences, to make sure it makes your line
 clean.

 if it says something needs escaped, that means it didn't get through okay
 this time. if you get an invalid packet printed, it means the packet wasn't
 sent by the linecheck on the other side, and may either be line static,
 or some very braindead terminal response to a (possibly series) of characters
 to what was printed over the line.  in this case, it's your responsibility
 to determine which, and escape the previously sent char if needed.  There is
 no way this program can identity a braindead terminal server from line static,
 so this is the way it has to be.

 if, for some reason, you get stuck out in lala land, and can't kill the program,
 try typing "00000".  That should kill it, and restore your terminal.

 It'll print "### sending char" and "### received valid".  Don't worry if these
 two number are out of sync.  That's fine.  Just worry, on either side, if you
 get some "Invalid packet: " lines.  Look at them closely, and see if it's line
 static, or a real problem.

 At the end, it'll print out a summary of what it thinks you should escape.
 This just means these chars didn't get received correctly this time.  Again,
 if line static munched something, some of these may be valid. 

 *** IF *** your terminal server generates extra responses for odd chars,
 then you may not be told to escape something, but need to anyway.  This will
 be evident from a "Invalid packet: " on the local side, after attempting to
 send a character.  Again, it may be line static. You have to make the call.

 if you're running it locally in a xterm, I suggest you turn on window logging.

 if you have problems with this program, and want me to look at it, mail me
 *both* the local and remote output, and label them appropriately.
*/

/* -------------------------------------------------- */

#define START_AT 0
#define STOP_AT  256

#define START      'A'
#define STOP       'B'
#define END_PACKET 'C'
#define XON        '\021'
#define DONE       "done"
#define RDONE      "rdone"
#define SLEEP      1
#define TRIES      1
#define BUMS       1

#define BUFFSIZE  200

/* -------------------------------------------------- */

unsigned char buff[BUFFSIZE];
unsigned char linecheck_mask = 255;

int valid[STOP_AT+1], skip[STOP_AT], pid, debugging;

/* -------------------------------------------------- */

int freqtest(void);

void sig_int(int x) {
	if (pid)
		kill(pid, SIGINT);
	exit(-1);
}

/* -------------------------------------------------- */

void debug(unsigned char *s)
{
  while (*s) {
		/* boy, there are some screwed up isprint()s out there */
    if ( isprint(*s) && (*s < 128) )
      fprintf(stderr, "%c", *(s++));
    else      
      fprintf(stderr, "<%d>", (int) *(s++));
  }
  fprintf(stderr, "\n");
}

/* -------------------------------------------------- */

int mgets(unsigned char *buff, int len)
{
  unsigned char ch;
  int i, zcount;
  
  i = 0;
  zcount = 0;
  
  while(1) {
		read(STDIN_FILENO, &ch, 1);

		ch &= linecheck_mask;
		/* check if we get five 0's and abort */
		if ( ch == '0' ) {
			if (++zcount == 5) {
				kill(pid, SIGINT);
				terminal_restore(0,0);
				exit(0);
			}
		}
		else
			zcount = 0;
		
		/* handle next char */
		if (ch == '\n')

			/* check if this is really inside the packet */
			if ( (i == 4) && (buff[0] == '1') && (buff[1] == '0') && (buff[2] == ' ') &&
					(buff[3] == START) )
				buff[i++] = ch;
			else {
				buff[i] = '\0';
				return i;
			}

		else {
			/* store this char in the buffer */
			buff[i++] = ch;
			
			/* don't let packet overflow */
			if ( (i+1) == len) {
			  buff[i] = '\0';
				return i;
			}
		}
	}
}

/* -------------------------------------------------- */

void handshake()
{
  int i;

	fprintf(stderr, "Handshaking\n");

	printf("\n%c\n", START);
	i = 0;
	while ( ! i ) { 
		mgets(buff, BUFFSIZE);

		if ( (buff[0] == START) && (buff[1] == '\0') ) {
			i = 1;
			printf("\n%c\n", STOP);
		}
		else
			if ( (buff[0] == STOP) && (buff[1] == '\0') )
				i = 1;
			else
				if (buff[0] != '\0' ) { 
					fprintf(stderr, "unexpected packet: ");
					debug(buff);
				}
	}
	
	fprintf(stderr, "Handshaking successful\n");
}

/* -------------------------------------------------- */

void skipchars(char **s)
{
	while(*s) {
                if (! strcmp(*s,"--seven_in"))
                  linecheck_mask = 127;
                else
		  skip[atoi(*s)] = 1;
                s++;
	}
}

/* -------------------------------------------------- */

void bum(char *s)
{
  int i;
	for (i=0; i<BUMS; i++) { 
		if ( s == NULL )
			printf("\n");
		else
			printf("\n%s\n", s);
		sleep(SLEEP);
	}
}

/* -------------------------------------------------- */

void check(unsigned char *s)
{
  int k;
  unsigned char *t;
  t = s;

	if (debugging) {
		fprintf(stderr, "incoming packet: ");
		debug(t);
	}

	/* convert int to number */
	k = 0;
	while ( isdigit(*s) )
		k = (k * 10) + (*(s++) - '0');

	/* verify this is a valid packet */
	if ( (s[0] == ' ') && (s[1] == START) 
          && (s[2] == (linecheck_mask&(unsigned char)k)) 
          && (s[3] == STOP) ) {
		if (s[2] == (unsigned char)k) {
		  fprintf(stderr, "%3d received valid\n",k);
                  valid[k] = 1;
                }else{
                  fprintf(stderr, "%3d was ignored.\n",k);
                }
	}
	else {
		if (debugging)
			fprintf(stderr, "was invalid packet\n");
		else {
			fprintf(stderr, "invalid packet: ");
			debug(t);
		}
	}
}

/* -------------------------------------------------- */

void do_reads()
{
	int ldone, rdone;

	ldone = 0;
	rdone = 0;

	while ( (!ldone) || (!rdone) ) {

		/* get a non-empty line */
		while( !mgets(buff, BUFFSIZE) );
		
		/* see if they are done sending test packets */
		if (strcmp((char *)buff,DONE) == 0) {
			rdone = 1;
			bum(RDONE);
		}
		else
			/* see if we are done sending packets to them */
			if (strcmp((char *)buff,RDONE) == 0)
				ldone = 1;
			else
				check(buff);
	}
}

/* -------------------------------------------------- */

void do_writes()
{
	int i, k;

  /* test all the chars */
  for (i = START_AT; i < STOP_AT; i++ ) {

		/* don't send chars we are told to skip */
    if ( skip[i] )
      continue;
		
    fprintf(stderr, "%3d sending char\n", i);

    /* attempt to send this char across, in a cute little packet */
		for (k=0; k < TRIES; k++) { 

			if ( !skip[(int)XON] )
				printf("\n%d %c%c%c%c%c\n", i, START, (unsigned char)i, STOP, XON, END_PACKET );
			else
				printf("\n%d %c%c%c%c\n", i, START, (unsigned char)i, STOP, END_PACKET );
			
			bum(NULL);
		}
	}

  bum(DONE);
}

/* -------------------------------------------------- */

void print_esc(int freq)
{
  int i, j;

  /* make printing at the end easier by making it more generic */
	valid[STOP_AT] = 1;

  /* print out the valid list */
  fprintf(stderr, "\n");

  fprintf(stderr,"The termrc options are required:\n\n");

  for (j = -1,i = START_AT; i <= STOP_AT; i++)
		if ( valid[i] && (j != -1) )
		{
                        if ((j <= 128) && (i == STOP_AT))
			{
				fprintf(stderr, "sevenbit_in on\n");
				if ( j < 127 )
					fprintf(stderr, "escape %d-%d\n", j, 127);
				if ( j == 127 )
					fprintf(stderr, "escape 127\n");
			}
			else
				if (j == (i-1))
					fprintf(stderr, "escape %d\n", j);
				else
					fprintf(stderr, "escape %d-%d\n", j, i-1);
			j = -1;
		}
		else
			if ( (!valid[i]) && (j == -1) )
				j = i; 

	if (freq < (CLK_TCK * 9)/10) 
		fprintf(stderr, "frequency %d\n", freq);

  fprintf(stderr,"\n\nBe sure to add the following options to");
  fprintf(stderr,"the other machine's termrc file:\n\n");

  for (j = -1, i = START_AT; i <= STOP_AT; i++)
		if ( valid[i] && (j != -1) )
		{
			if ((j <= 128 || linecheck_mask != 255)
                          && (i == STOP_AT))
			{
				fprintf(stderr, "sevenbit_out on\n");
				if ( j < 127 )
					fprintf(stderr, "ignore %d-%d\n", j, 127);
				if ( j == 127 )
					fprintf(stderr, "ignore 127\n");
			}
			else
				if (j == (i-1))
					fprintf(stderr, "ignore %d\n", j);
				else
					fprintf(stderr, "ignore %d-%d\n", j, i-1);
			j = -1;
		}
		else
			if ( (!valid[i]) && (j == -1) )
				j = i; 

  fprintf(stderr,"\n\t WARNING: In addition to the above you may need\n");
  fprintf(stderr,"\tto add 'escape 17', 'escape 19', and 'escape 43'\n");
  fprintf(stderr,"\tLinecheck does not properly check for these yet.\n");
  fprintf(stderr,"\n\n\tBE SURE TO LOOK AT THE OTHER MACHINE'S LOG FILE\n");
}

/* -------------------------------------------------- */

int main(int argc, char **argv)
{
	int i;
        int freq;

#ifdef SOCKS
	SOCKSinit(argv[0]);
#endif
	pid = 0;
	debugging = 0;

	argv++;
	argc--;

	/* check the output file */
        if (! argc || strcmp("--stderr",*argv)) {
        	freopen("linecheck.log", "w" , stderr);
	}else {
		argc--;
		argv++;
        }

  /* nothing has gotten through okay yet */	
  for (i = START_AT; i < STOP_AT; i++) {
		valid[i] = 0;
		skip[i] = 0;
	}

  skipchars(argv);

	signal(SIGINT, sig_int);

	terminal_save(0);
	terminal_raw(0);

  setbuf(stdout,NULL);
  setbuf(stderr,NULL);
	
  handshake();
	/* parent should be reader to remain in control */
	if ((pid = x__fork())) {
		do_reads();
	}	
	else {
		do_writes();
		exit(0);
	}	

  freq = freqtest();
  fflush(stderr);
  print_esc(freq);
  terminal_restore(0,0);
  sleep(1);
  exit(0);
}

