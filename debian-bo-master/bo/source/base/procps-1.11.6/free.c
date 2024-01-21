/* free.c - a /proc implementation of free */
/* Dec14/92 by Brian Edmonds */
/* Thanks to Rafal Maszkowski for the Total line */

#include "proc/version.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <getopt.h>

/* set this to whatever you want by default */
int byteshift = 10  ;
int total = 0;

int main( int argc, char **argv ) {
    char buf1[80], buf2[80];
    char *titles[6], name[32];
    int i, n = 0, col[6] = {0}, rtime = 0, old_fmt = 0, first_line;
    int old_meminfo = 0;

    static int sum[6]; /* statics get initialized to zero */

    set_linux_version();
    /* check startup flags */
    while( (i = getopt( argc, argv, "bkmos:tV") ) != -1 )
        switch (i) {
        case 'b': byteshift = 0; break;
        case 'k': byteshift = 10; break;
        case 'm': byteshift = 20; break;
        case 'o': old_fmt = 1; break;
        case 's': rtime = 1000000 * atof(optarg); break;
        case 't': total = 1; break;
	case 'V': display_version(); exit(0);
        default:
	  fprintf(stderr, "usage: %s [-b|-k|-m] [-o] [-s delay] [-t] [-V]\n", argv[0]);
	  return 1;
    }

    /* redirect stdin to /proc/meminfo */
    close(0);
    if (open( "/proc/meminfo", O_RDONLY ) < 0) {
        perror("open");
        return 2;
    }

    do {
	for(i=0; i<6; i++)
	    sum[i]=0;
	first_line = 1;
        /* get the column titles */
        fseek(stdin, 0L, SEEK_SET);
        fgets( buf1, 80, stdin );
        for (i=0; i<6; i++) {
            titles[i] = strtok( ( i ? NULL : buf1 ), " \t:\n" );
            if (!titles[i]) {
	       if (i != 5) {
		  fprintf( stderr, "free: error reading /proc/meminfo\n" );
		  return 3;
	       } else
		  ++old_meminfo;
            }
        }

	if (old_meminfo) {
	   fprintf(stdout, "%-7s %10s %10s %10s %10s %10s\n",
		    "", titles[0], titles[1], titles[2], titles[3],
		    titles[4]);
	} else {
	   fprintf(stdout, "%-7s %10s %10s %10s %10s %10s %10s\n",
		    "", titles[0], titles[1], titles[2], titles[3],
		    titles[4], titles[5]);
	}

        /* read and translate data lines */
        while (fgets(buf2, 80, stdin)) {
	   if (old_meminfo) {
	      n = sscanf(buf2, "%s %d %d %d %d %d", name,
			 &col[0], &col[1], &col[2], &col[3], &col[4]);
	   } else {
	      n = sscanf(buf2, "%s %d %d %d %d %d %d", name,
			 &col[0], &col[1], &col[2], &col[3], &col[4],
			 &col[5]);
	   }
	   if (n < 4)
	       continue;
	   fprintf( stdout, "%-7s", name );
	   for (i=1 ; i<n ; i++) {
	       fprintf(stdout, " %10d", col[i-1]>>byteshift);
	       sum[i-1] += col[i-1];
	   }
	   fputc('\n', stdout);
	   if (first_line && !old_fmt) {
	       first_line = 0;
	       fprintf( stdout, "-/+ buffers: %16d %10d\n",
			(col[1] - col[4] - col[5])>>byteshift,
			(col[2] + col[4] + col[5])>>byteshift);
	   }
        }
        if (total == 1) {
            fprintf( stdout, "Total: ");
            for( i=1 ; i<n ; i++ )
                fprintf(stdout, " %10d", sum[i-1]>>byteshift);
            fputc('\n', stdout);
        }
        if (rtime) {
            fputc('\n', stdout);
	    fputc('\n', stdout);
	    usleep(rtime);
	}
    } while (rtime);

    return 0;
}
