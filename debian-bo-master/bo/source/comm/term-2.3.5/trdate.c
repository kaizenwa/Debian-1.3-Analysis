/*
 * trdate/trdated
 *
 * A term client to set the local time to the remote time.
 * Can be run once as trdate or as a daemon trdated
 *
 * (C) 1994 Kevin Lentin (kevinl@cs.monash.edu.au)
 *
 */

#define I_ERRNO
#define I_SYS
#define I_STRING
#include "includes.h"

#include "client.h"

#define STAT(local, st) \
	if (send_command(s, C_STATS, local, "%d",st)< 0) { \
		fprintf(stderr,"C_STATS command failed. Abort.\n"); \
		exit(1); \
	}

#define VERBOSE if(verbose)printf

long delay = 300;

static int local_options ( char opt, char *term_optarg )
{
  switch(opt)
  {
  case 'd' :
    printf("optarg=%s\n", term_optarg);
    delay = atoi(term_optarg);
	if (delay < 10) {
		fprintf(stderr, "Minimum delay is 10 seconds\n");
		return -1;
	}
	VERBOSE("Delay set to %ld seconds\n", delay);
    break;
  default:
    return -1;
  }
  return 0;
}

#ifdef ONE_CLIENT
# define main trdate
#endif
int main(int argc, char *argv[])
{
	int s;
	int first;
	int daemon = 0;
	struct timeval t1,t2,t;
	long diff;
	long sec, usec;
	char args[10];

#ifdef SOCKS
  SOCKSinit(argv[0]);
#endif
	strcpy(args,"v");
	if (argv[0][strlen(argv[0])-1] == 'd') {
		daemon = 1;
		strcat(args,"d:");
	}

	priority = 10; /* We want immediate results */
	first = client_options(argc,argv,args, local_options);
	if (first == -1 || first > argc)
		exit (1);
	setbuf(stderr, 0);

	while (1) {
		if ((s = socket_connect_server(-1,term_server)) <0)
		{
			/* Move above later */
			if (daemon) {
				sleep(delay);
				continue; /* Continues the while */
			}
			fprintf(stderr,"Term: %s\n",command_result);
			x__close(s);
			exit(1);
		}
		gettime(&t1);
		VERBOSE("Our time is: %ld %ld\n", (long)t1.tv_sec, 
		  (long)t1.tv_usec);
		STAT(0,-9);
		VERBOSE("Got result:%s:\n", command_result);
		sscanf(command_result, "%ld %ld", &sec, &usec);
		t.tv_sec = sec;
		t.tv_usec = usec;
		gettime(&t2);
		VERBOSE("Our time is: %ld %ld\n", (long)t2.tv_sec, 
		  (long)t2.tv_usec);
		diff = (t2.tv_sec - t1.tv_sec) * 1000000 + t2.tv_usec - t1.tv_usec;
		VERBOSE("Diff = %ld\n", diff);
		t.tv_usec += diff;
		t.tv_sec += (t.tv_usec / 1000000);
		t.tv_usec %= 1000000;

		VERBOSE("Setting time to: %ld %ld\n", (long)t.tv_sec, 
		  (long)t.tv_usec);
		settime(&t);
		
		x__close(s);
		if (!daemon)
			break;
		sleep(delay);
	}
        exit(0);
}
