/* playcdda.c:  
   by 23.10.95 T.Niederreiter
   based on recplay.c by Hannu Savolainen
   
   Plays 16 bit, 44.1kHz, Stereo - raw files in big-endian-format
   (known as CDDA-Files).
   New switch -x to play little endian raw files
   
*/

#include <stdio.h>
#include <malloc.h>
#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>
#include <fcntl.h>
#include <strings.h>
#include <linux/soundcard.h>
#include <sys/stat.h>

#define DEFAULT_DSP_SPEED 44100
#define FRAMESIZE 2352       /* Framesize in CDR-Format */

#define AUDIO "/dev/dsp"

int dsp_speed = DEFAULT_DSP_SPEED;
int dsp_stereo = 1;
int samplesize = 16;
int quiet_mode = 0;
int swap = 1;
int audio, abuf_size;
int startframe = 0;
int tickdisplay = 0;
int tick = 0;
int doplay = 1;
char *audiobuf, c;
long totalbytes,bytessofar;
struct stat stat_buf;

char keybuffer[1024];
char audio_name[20] = AUDIO;

void playcdr (char *name);
void describe_error(void);

int main (int argc, char *argv[]) {
char *command;
int tmp;

  	command = argv[0];

  	while ((c = getopt (argc, argv, "qs:Sb:d:xf:T")) != EOF)
   	switch (c) {

      	case 'S':
		dsp_stereo = 1;
		break;
      	case 'q':
		quiet_mode = 1;
		break;
      	case 's':
		dsp_speed = atoi (optarg);
		if (dsp_speed < 300)
			dsp_speed *= 1000;
		break;
      	case 'b':
		samplesize = atoi (optarg);
		break;

      	case 'd':
        	strncpy(audio_name, optarg, 20);
        	break;
    
      	case 'x':
		swap = 0;
		break;

      	case 'f':
        	startframe = atoi (optarg);
        	break;

      	case 'T':
		tickdisplay = 1;
		break;

      	default:
		fprintf (stderr, 
	 	"Usage: %s [-qSxT] [-s Hz] [-b 8|12|16] [-d device] [-f startframe] [filename]\n", command);
		exit (-1);
      	}


  	audio = open (audio_name, O_WRONLY, 0);

  	if (audio == -1) {
      		perror (audio_name);
      		describe_error();
      		exit (-1);
    	}

  	tmp = samplesize;
  	ioctl(audio, SNDCTL_DSP_SAMPLESIZE, &samplesize);
  	if (tmp != samplesize) {
  		fprintf(stderr, "Unable to set the sample size\n");
  		exit(-1);
  	}

  	if (ioctl (audio, SNDCTL_DSP_STEREO, &dsp_stereo)==-1) {
      		fprintf (stderr, "%s: Unable to set mono/stereo\n", command);
      		perror (audio_name);
      		exit (-1);
  	}

  	if (ioctl (audio, SNDCTL_DSP_SPEED, &dsp_speed) == -1) {
      		fprintf (stderr, "%s: Unable to set audio speed\n", command);
      		perror (audio_name);
      		exit (-1);
    	}
  	if (!quiet_mode) {
      		fprintf (stderr, "Speed %d Hz ", dsp_speed);
      		if (dsp_stereo)
			fprintf (stderr, "(stereo)\n");
      		else
			fprintf (stderr, "(mono)\n");
      		if (samplesize != 8)
        		fprintf(stderr, "%d bits per sample\n", samplesize);
    	}

  	ioctl (audio, SNDCTL_DSP_GETBLKSIZE, &abuf_size);
  	if (abuf_size < 1024 || abuf_size > (2*65536)) {
      		if (abuf_size == -1)
			perror (audio_name);
      		else
			fprintf (stderr, "Invalid audio buffers size %d\n", abuf_size);
      		exit (-1);
    	}

  
  	if ((audiobuf = malloc (abuf_size)) == NULL) {
      		fprintf (stderr, "Unable to allocate input/output buffer\n");
      		exit (-1);
    	}

  	if (optind > argc - 1)
    		playcdr (NULL);
  	else
    		while (optind <= argc - 1) {
			playcdr (argv[optind++]);
      		}

  	close (audio);
  	return 0;
}

void playcdr (char *name) {
int fd, l, ii;
int oldtick = 0;
char tmpswap;

      	if (!name) {
	  	fd = 0;
	  	name = "stdin";
          	totalbytes = 0; /* unknown */
	} else {
	  	if ((fd = open (name, O_RDONLY, 0)) == -1) {
	      		perror (name);
	      		exit (-1);
	    	}
          	fstat(fd,&stat_buf);
          	totalbytes = stat_buf.st_size;  /* Find out filesize */
	}

      	if (startframe != 0) {
        	bytessofar= startframe*FRAMESIZE;
        	if (lseek(fd,startframe*FRAMESIZE,SEEK_SET) == -1) {
          		perror(name);
          		exit (-1);
        	}
      	}

      	fcntl(STDIN_FILENO,F_SETFL,O_NONBLOCK); /* stdin must be nonblocking! */

	printf("p0\n"); fflush(stdout); /* Update display before start */

      	while (1) {

		/* Watch for new commands in pipe*/ 
	  	if (gets(keybuffer) != NULL) {
            		if (strcmp(keybuffer,"stop") == 0) { 
				lseek(fd,0,SEEK_SET); 
				bytessofar=0; tick=0; doplay=0; 
				printf("s0\n"); fflush(stdout);
			} 
	    		if (strcmp(keybuffer,"play") == 0) { 
				doplay=1; 
			}
            		if (strcmp(keybuffer,"pause") == 0) { 
				doplay=1-doplay;
			 	printf("%c%d\n",doplay?'p':'s',tick);
				fflush(stdout);
			} 
	    		if (strncmp(keybuffer,"set",3) == 0) {
				lseek(fd,atoi(keybuffer+3)*FRAMESIZE*75,SEEK_SET);
				bytessofar=atoi(keybuffer+3)*FRAMESIZE*75;
				tick=atoi(keybuffer+3);
				printf("%c%d\n",doplay?'p':'s',tick);
				fflush(stdout);
	      		}
	    		if (strcmp(keybuffer,"quit") == 0) { 
				break; 
			} 
    		}	

	 	if (doplay) {

	  		if ((l = read (fd, audiobuf, abuf_size)) > 0) {
	      			if (swap) {
					for (ii=0; ii<l; ii+=2) {
                 				tmpswap=audiobuf[ii];
		 				audiobuf[ii]=audiobuf[ii+1];
		 				audiobuf[ii+1]=tmpswap;
                 			}
	        		}

	      			if (tickdisplay) {
		  			tick=(bytessofar/FRAMESIZE/75);
		  			if (tick != oldtick) {
		    				printf("%c%d\n",doplay?'p':'s',tick); /* These are seconds */
		    				fflush(stdout); 	/* Flush it for the pipe */
		    				oldtick=tick;
		  			}
				}

	      			if (write (audio, audiobuf, l) != l) {
		  			perror (audio_name);
		  			exit (-1);
				}
	      			bytessofar+=abuf_size;
	    		} else {
	      			if (l == -1) {
		  			perror (name);
		  			exit (-1);
				}
	      			doplay=0; /* We reached EOF, stop playing */	
	    			printf("%c%d\n",doplay?'p':'s',tick); /* These are seconds */
	    			fflush(stdout); 	/* Flush it for the pipe */
	    		}
 	  	} else {
	    		usleep(100);  /* Give me a break..saves CPU-Resources */
	  	}
	}

      	if (fd != 0) {
		close (fd);
	}
}


void describe_error(void) {

	switch (errno) {

	case ENXIO:
			/* fall through */
	case ENODEV:
		fprintf(stderr,	"The device file was found in /dev but\n"
				"there is no driver for it in the kernel.\n"
				"\n"
				"There is several possible reasons:\n"
				"- The sound driver is not present\n"
				"  in the kernel you are currently running.\n"
				"- The driver is in the kernel but there is\n"
				"  no soundcard on your system.\n"
				"- Configuration of the sound driver doesn't\n"
				"  match the hardware.\n"
				"\n"
				"Try cat /dev/sndstat for further info.\n");
		break;

	case ENOSPC:
		fprintf(stderr,	"The soundcard driver was not installed\n"
				"properly. The sound_mem_init() routine\n"
				"was not called during the boot procedure.\n");
		break;

	case ENOENT:
		fprintf(stderr,	"The device file is missing from /dev.\n"
				"Run the script at the end of file\n"
				"sound/linux/Readme (distributed with\n"
				"the sound driver.\n");
		break;

	default: 
		;
	}
}
