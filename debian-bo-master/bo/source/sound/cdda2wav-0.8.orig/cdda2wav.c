/*
 * Copyright: GNU Public License 2 applies
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2, or (at your option)
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * CDDA2WAV (C) 1995-1997 Heiko Eissfeldt heiko@colossus.escape.de
 * parts    (C) Peter Widow
 * parts    (C) Thomas Niederreiter
 * parts    (C) RSA Data Security, Inc.
 *
 * last changes:
 *   18.12.93 - first version,	OK
 *   01.01.94 - generalized & clean up HE
 *   10.06.94 - first linux version HE
 *   12.06.94 - wav header alignment problem fixed HE
 *   12.08.94 - open the cdrom device O_RDONLY makes more sense :-)
 *		no more floating point math
 *		change to sector size 2352 which is more common
 *		sub-q-channel information per kernel ioctl requested
 *		doesn't work as well as before
 *		some new options (-max -i)
 *   01.02.95 - async i/o via semaphores and shared memory
 *   03.02.95 - overlapped reading on sectors
 *   03.02.95 - generalized sample rates. all integral divisors are legal
 *   04.02.95 - sun format added
 *              more divisors: all integral halves >= 1 allowed
 *		floating point math needed again
 *   06.02.95 - bugfix for last track and not d0
 *              tested with photo-cd with audio tracks
 *		tested with xa disk 
 *   29.01.96 - new options for bulk transfer
 *   01.06.96 - tested with enhanced cd
 *   01.06.96 - tested with cd-plus
 *   02.06.96 - support pipes
 *   02.06.96 - support raw format
 *   04.02.96 - security hole fixed
 *
 * TODO: 
 *	 debug program for panasonic drives (have none)
 *       more items in file TODO
 */

#define USE_GETOPT_LONG

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/wait.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include "share.h"
#include "uti.h"
#include "wav.h"	/* wav file header structures */
#include "sun.h"	/* sun audio file header structures */
#include "raw.h"	/* raw file handling */
#include "interface.h"  /* low level cdrom interfacing */
#include "cdda2wav.h"
#include "resample.h"
#include "indexscan.h"
#include "setuid.h"

#ifdef	ECHO_TO_SOUNDCARD
#include <linux/soundcard.h>
#endif

#ifdef  MD5_SIGNATURES
#include "md5.h"

int md5blocksize, md5count;
MD5_CTX context;
unsigned char MD5_result[16];
#endif

const char * optstring = "D:A:I:O:c:b:r:a:t:i:d:o:n:v:"
#ifdef MD5_SIGNATURES
                         "M:"
#endif
                         "smw"
#ifdef	ECHO_TO_SOUNDCARD
                         "e"
#endif
                         "NqxPRBVh";

#ifdef USE_GETOPT_LONG
#include <getopt.h>
struct option options [] = {
	{"device",required_argument,NULL,'D'},
	{"auxdevice",required_argument,NULL,'A'},
	{"interface",required_argument,NULL,'I'},
	{"output-format",required_argument,NULL,'O'},
	{"channels",required_argument,NULL,'c'},
	{"bits-per-sample",required_argument,NULL,'b'},
	{"rate",required_argument,NULL,'r'},
	{"divider",required_argument,NULL,'a'},
	{"track",required_argument,NULL,'t'},
	{"index",required_argument,NULL,'i'},
	{"duration",required_argument,NULL,'d'},
	{"offset",required_argument,NULL,'o'},
	{"sectors-per-request",required_argument,NULL,'n'},
	{"verbose-level",required_argument,NULL,'v'},
#ifdef MD5_SIGNATURES
	{"md5",required_argument,NULL,'M'},
#endif
	{"stereo",no_argument,NULL,'s'},
	{"mono",no_argument,NULL,'m'},
	{"wait",no_argument,NULL,'w'},
#ifdef	ECHO_TO_SOUNDCARD
	{"echo",no_argument,NULL,'e'},
#endif
	{"no-write",no_argument,NULL,'N'},
	{"quiet",no_argument,NULL,'q'},
	{"max",no_argument,NULL,'x'},
	{"no-overlap",no_argument,NULL,'P'},
	{"dump-rates",no_argument,NULL,'R'},
	{"bulk",no_argument,NULL,'B'},
	{"version",no_argument,NULL,'V'},
	{"help",no_argument,NULL,'h'},
	{NULL,0,NULL,0}
};

#endif /* GETOPT_LONG */


/* global variables */
char dev_name [200] = CD_DEVICE;		/* device name */
char aux_name [200] = AUX_DEVICE;		/* device name */

int audio = -1;

static int (*InitAudioFile)( int audioflag, long channels, unsigned long rate,
			    long nBitsPerSample,
			    unsigned long expected_bytes);
static int (*ExitAudioFile)( int audioflag, unsigned long nBytesDone );
static unsigned long (*GetSndHdrSize)( void );

static unsigned long nSamplesDone = 0;

long cd_fd = -1;
int no_file = 0;

#ifdef	ECHO_TO_SOUNDCARD
int soundcard_fd;
int echo = 0;
#endif

static	int child_pid = 0xff;
int swap;
int OutSampleSize;
int sh_bits;
int channels = CHANNELS;
int SkippedSamples = 0;

static char fname_base[200] = FILENAME;
static long nSamplesToDo;
static long current_track;
static int bulk = 0;

long SamplesNeeded( long amount, long undersampling_val)
{
  long retval = ((undersampling_val * 2 + Halved)*amount)/2;
  if (Halved && (nSamplesToDo & 1))
    retval += 2;
  return retval;
}

#ifdef INFOFILES
int write_info_file(char *fname_baseval, long int track, 
		    unsigned long int SamplesDone, int numbered)
{
  FILE *info_fp;
  char fname[200];
  char datetime[30];
  time_t utc_time;
  struct tm *tmptr;

  /* write info file */
  if (!strcmp(fname_baseval,"standard output")) return 0;
  if (numbered)
    sprintf(fname, "%s.%02ld.info", fname_baseval, track);
  else
    sprintf(fname, "%s.info", fname_baseval);
  info_fp = fopen (fname, "w");
  if (!info_fp)
    return -1;

#ifdef MD5_SIGNATURES
  if (md5blocksize)
    MD5Final (MD5_result, &context);
#endif

  utc_time = time(NULL);
  tmptr = localtime(&utc_time);
  if (tmptr) {
    strftime(datetime, sizeof(datetime), "%x %X", tmptr);
  } else {
    strncpy(datetime, "unknown", sizeof(datetime));
  }
  fprintf(info_fp, "#created by cdda2wav "VERSION" %s\n"
	  "Track = %ld\n"
	  "Length = %ld\n"
	  "ISRC = %s\n"
#ifdef MD5_SIGNATURES
	  "MD-5(%d) = %02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\n"
#endif
	  , datetime
	  , track
	  , (SamplesDone*40) /3	/* in milliseconds */
	  , g_toc[track-1].ISRC
#ifdef MD5_SIGNATURES
	  , md5blocksize
	  , MD5_result[0]
	  , MD5_result[1]
	  , MD5_result[2]
	  , MD5_result[3]
	  , MD5_result[4]
	  , MD5_result[5]
	  , MD5_result[6]
	  , MD5_result[7]
	  , MD5_result[8]
	  , MD5_result[9]
	  , MD5_result[10]
	  , MD5_result[11]
	  , MD5_result[12]
	  , MD5_result[13]
	  , MD5_result[14]
	  , MD5_result[15]
#endif
	  );
  fclose(info_fp);
  return 0;
}
#endif

static void CloseAudio(char *fname_baseval, long int track, int bulkflag)
{
#ifdef INFOFILES
      /* define sizes in the header of the audio file */
      write_info_file(fname_base,track,nSamplesDone,bulkflag);
#endif

      /* define length */
      ExitAudioFile( audio, nSamplesDone*OutSampleSize*channels );

      close (audio);
      audio = -1;
}

/* On terminating:
 * define size-related entries in audio file header, update and close file */
static void CloseAll (void)
{
  int chld_return_status;
  int amichild;

  /* terminate child process first */
  amichild = child_pid == 0;

  if (amichild) {
      /* kill parent process */
/*      kill (getppid(), SIGINT);*/
      return;
  } else
      kill(child_pid,SIGINT);


  /* wait for child to terminate */
  wait(&chld_return_status);

  /* do general clean up */

#ifdef	ECHO_TO_SOUNDCARD
  if (echo) {
      close(soundcard_fd);
  }
#endif
  if (audio>=0) {
    CloseAudio(fname_base, current_track, bulk);
  }

  /* free shared memory and semaphores */
  free_semshm();

  /* switch to original mode and close device */
  if (cd_fd >= 0) {
      EnableCdda (FALSE);
      close(cd_fd);
      cd_fd = -1;
  }
}


/* report a fatal error, clean up and exit */
void FatalError (const char *szMessage, ...)
{
  va_list marker;

  va_start(marker, szMessage);

  vfprintf (stderr, szMessage, marker);

  va_end(marker);
  exit (1);
}

/* request a shared memory block */
int shm_request(key_t key, unsigned int size, unsigned char **memptr)
{
  int   ret_val;
  int   shmflag;
  int   SHMEM_ID;
  int    cmd;
  struct shmid_ds buf;

  shmflag = IPC_CREAT | 0600;
  ret_val = shmget(key,(signed)size,shmflag);
  if ( ret_val == -1 )
  {
    perror("shmget");
    return -1;
  }

  SHMEM_ID = ret_val;
  cmd = IPC_STAT;
  ret_val = shmctl(SHMEM_ID,cmd,&buf);
#ifdef IPCTST
  fprintf(stderr, "%d: shmctl STAT= %d, SHM_ID: %d, key %ld cuid %d cgid %d mode %3o size %d\n",
          getpid(),ret_val,SHMEM_ID,
          buf.shm_perm.key,buf.shm_perm.cuid,buf.shm_perm.cgid,
          buf.shm_perm.mode,buf.shm_segsz);
#endif

  *memptr = (unsigned char *) shmat(SHMEM_ID, NULL, 0);
  if (*memptr == (unsigned char *) -1) {
    *memptr = NULL;
    return -1;
  }
  return SHMEM_ID;
}


/* open the audio output file and prepare the header. 
 * the header will be defined on terminating (when the size
 * is known). So hitting the interrupt key leaves an intact
 * file.
 */
static void OpenAudio (char *fname, double rate, long nBitsPerSample, 
		       long channels_val, unsigned long expected_bytes)
{
  uid_t my_euid = geteuid();
  uid_t my_uid  = getuid();

  if (audio == -1) {
    /* release superuser permissions */
    seteuid(my_uid);

    audio = open (fname, O_CREAT | O_WRONLY | O_TRUNC
#ifdef SYNCHRONOUS_WRITE
		       | O_SYNC
#endif
		  , 0600);
    if (audio == -1) {
      perror("open audio");
      FatalError ("Could not open %s\n", fname);
    }

    if (my_uid != my_euid)
      fchown(audio, my_uid, getgid());
  }
  InitAudioFile( audio, channels_val, (unsigned long)rate, nBitsPerSample, expected_bytes );

#ifdef MD5_SIGNATURES
  if (md5blocksize)
    MD5Init (&context);
  md5count = md5blocksize;
#endif

  seteuid(my_euid);
}

long Remainder;
int quiet = 0;
int verbose = 3 + 32;


static void usage( void )
{
  fprintf( stderr,
"cdda2wav [-c chans] [-s] [-m] [-b bits] [-r rate] [-a divider]\n"
"         [-t track] [-i index] [-o offset] [-d duration]\n"
"         [-x] [-q] [-w] [-v] [-R] [-P] [-e] [-n sectors] [-N]\n"
"         [-D device] [-I interface] [-O audiotype] [-B]\n"
"         [-A auxdevice] [audiofile]\n"
"Version "VERSION"\n"
"cdda2wav copies parts from audio cd's directly to wav or au files.\n"
"It requires a supported cdrom drive to work.\n"
"options: -D device   : set the cdrom or scsi generic device.\n"
"         -A auxdevice: set the aux device (typically /dev/cdrom).\n"
"         -I interface: specify the interface for cdrom access.\n"
"                     : (generic_scsi or cooked_ioctl).\n"
"         -c channels : set 1 for mono, or 2 for stereo recording.\n"
"         -s          : set to stereo recording.\n"
"         -m          : set to mono recording.\n"
"         -x          : set to maximum quality (stereo/16-bit/44.1 KHz).\n"
"         -b bits     : set bits per sample per channel (8, 12 or 16 bits).\n"
"         -r rate     : set rate in samples per second. -R gives all rates\n"
"         -a divider  : set rate to 44100Hz / divider. -R gives all rates\n"
"         -R          : dump a table with all available sample rates\n"
"         -P          : switch off overlap sampling\n"
"         -n sectors  : read sectors per request.\n"
"         -t track    : select start track.\n"
"         -i index    : select start index.\n"
"         -o offset   : start 'offset' sectors behind start track/index.\n"
"                       one sector equivalents 1/75 second.\n"
"         -O audiotype: set wav or sun or raw audio format. Default is wav.\n"
"         -d duration : set recording time in seconds or 0 for whole track.\n"
"         -B          : record each track into a seperate file.\n"
"         -w          : wait for audio signal, then start recording.\n"
#ifdef	ECHO_TO_SOUNDCARD
"         -e          : echo audio data to sound device SOUND_DEV.\n"
#endif
"         -v level    : print informations on current cd (level: 0-63).\n"
"         -N          : no file operation.\n"
#ifdef MD5_SIGNATURES
"         -M count    : calculate MD-5 checksum for 'count' bytes.\n"
#endif
"         -q          : quiet operation, no screen output.\n"
#ifdef USE_GETOPT_LONG
"         -V          : print version information.\n"
"         -h          : this help screen.\n"  
#endif
"defaults: %s, %d bit, %g Hz, track 1, no offset, one track,\n"
"          type %s '%s', don't wait for signal, not quiet, not verbose,\n"
"          use %s, device %s, aux %s\n"
"parameters: (optional) a file name or - for standard output.\n",
	  CHANNELS-1?"stereo":"mono", BITS_P_S, 44100.0 / UNDERSAMPLING, 
          AUDIOTYPE, FILENAME, DEF_INTERFACE, CD_DEVICE, AUX_DEVICE);
  exit( 1 );
}

int need_big_endian;

static unsigned long SectorBurst;
#if (SENTINEL > CD_FRAMESIZE_RAW)
#error block size for overlap check has not to be > sector size
#endif

/* and finally: the MAIN program */
int main( long argc, char *argv [] )
{
  long lSector;
  long lSector_p1;
  long sector_offset = 0;
  unsigned long SamplesToWrite; 
  double rectime = 0.0;
  unsigned int track = 1;
  unsigned int cd_index = 1;
  double rate = 44100 / UNDERSAMPLING;
  double int_part;
  int bits = BITS_P_S;
  char fname[200];
  int just_the_toc = 0;
  unsigned char int_name[100] = DEF_INTERFACE;
  char audio_type[10] = AUDIOTYPE;
  long i;
  unsigned char *p;
  int c;
  int long_option_index=0;

  /* Control those set-id privileges... */
  initsecurity();
  neverneedroot();     /* we don't have scsi_ioctl any more */

  /* command options parsing */
#ifdef USE_GETOPT_LONG
  while ( (c = getopt_long (argc,argv,optstring,options,&long_option_index)) 
		!= EOF)
#else
  while ( (c = getopt(argc, argv, optstring)) != EOF )
#endif /* USE_GETOPT_LONG */
	{
    switch (c) {
#ifdef MD5_SIGNATURES
      case 'M':
	md5blocksize = atoi( optarg );
        break;
#endif
      case 'D':    /* override device */
        strncpy( dev_name, optarg, sizeof(dev_name) );
        dev_name[sizeof(dev_name)-1]=0;
	break;
      case 'A':    /* override device */
        strncpy( aux_name, optarg, sizeof(aux_name) );
        aux_name[sizeof(aux_name)-1]=0;
	break;
      case 'I':    /* override interface */
	strcpy( (char *) int_name, optarg );
	break;
      case 'O':    /* override audio type */
	strncpy( audio_type, optarg, 4);
	break;
      case 'c':    /* override channels */
	channels = atoi( optarg );
	break;
      case 'b':    /* override bits */
	bits = atoi( optarg );
	break;
      case 'r':    /* override rate */
	rate = atoi( optarg );
	break;
      case 'a':    /* override rate */
	if (strtod( optarg, NULL ) != 0.0)
	    rate = fabs(44100.0 / strtod( optarg, NULL ));
	else {
	    fprintf(stderr, "-a requires a nonzero, positive divider.\n");
	    usage();
	}
	break;
      case 't':    /* override start track */
	track = atoi( optarg );
	break;
      case 'i':    /* override start index */
	cd_index = atoi( optarg );
	break;
      case 'd':    /* override recording time */
	rectime = strtod( optarg, NULL );
	break;
      case 'o':    /* override offset */
	sector_offset = atol( optarg );
	break;
      case 'n':    /* read sectors per request */
	nsectors = atol( optarg );
	break;
      case 's':    /* stereo */
	channels = 2;
	break;
      case 'm':    /* mono */
	channels = 1;
	break;
      case 'x':    /* max */
	channels = 2; bits = 16; rate = 44100;
	break;
      case 'w':    /* wait for some audio intensity */
	waitforsignal = 1;
	break;
      case 'e':	   /* echo to sound acrd */
#ifdef	ECHO_TO_SOUNDCARD
	echo = 1;
#else
	fprintf(stderr, "There is no sound support compiled into %s.\n",argv[0]);
#endif
	break;	
      case 'v':    /* tell us more */
	verbose = atoi( optarg );
	break;
      case 'q':    /* be quiet */
	quiet = 1;
	verbose = 0;
	break;
      case 'N':    /* don't write to file */
	no_file = 1;
	break;
      case 'B':    /* bulk transfer */
	bulk = 1;
	if (rectime == 0.0)
	  rectime = 99999.0;
	break;
      case 'P':    /* prevent overlap reading */
	overlap = 0;
	break;
      case 'R':    /* list available rates */
	{ int ii;
	  fprintf(stderr, "Cdda2wav version "VERSION": available rates are:\n"
               "Rate   Divider      Rate   Divider      "
               "Rate   Divider      Rate   Divider\n");
	  for (ii = 1; ii <= 44100 / 880 / 2; ii++) {
	    long i2 = ii;
	    fprintf(stderr, "%-7g  %2ld         %-7g  %2ld.5       ",
                    44100.0/i2,i2,44100/(i2+0.5),i2);
	    i2 += 25;
	    fprintf(stderr, "%-7g  %2ld         %-7g  %2ld.5\n",
                    44100.0/i2,i2,44100/(i2+0.5),i2);
	    i2 -= 25;
	  }
	}
	exit(0);
#ifdef USE_GETOPT_LONG
	break;
		case 'V':
			fputs ("Cdda2wav version "VERSION, stderr);
			exit (0);
			break;
		case 'h':
			usage();
			break;
		case '?':
#if 0
				fputs ("use cdda2wav --help to get more information.", stderr);
				exit (1);
#else
                                usage();
#endif
				break;
#endif
      default:
        usage();
    }
  }

  if (!bulk) {
    strcat(fname_base, ".");
    strcat(fname_base, audio_type);
  }

  /* all options processed. Now a file name may follow */
  if ( optind < argc ) {
    if (!strcmp(argv[optind],"-")) {
      /* pipe mode */
      bulk = 0;
      audio = dup (fileno(stdout));
      strcpy( fname_base, "standard output" );
    } else {
      /* filename given */
      strncpy( fname_base, argv[optind], sizeof(fname_base)-8 );
      fname_base[sizeof(fname_base)-1]=0;
    }
  }

  /* check all parameters */
  if (verbose < 0 || verbose > 63) {
    fprintf(stderr, "Error: incorrect verbose level setting: %d\n",verbose);
    usage();
  }
  if (verbose == 0) quiet = 1;

  if ( channels != 1 && channels != 2 ) {
    fprintf(stderr, "Error: incorrect channel setting: %d\n",channels);
    usage();
  }

  if ( bits != 8 && bits != 12 && bits != 16 ) {
    fprintf(stderr, "Error: incorrect bits_per_sample setting: %d\n",bits);
    usage();
  }

  if ( rate < 827.0 || rate > 44100.0 ) {
    fprintf(stderr, "Error: incorrect sample rate setting: %g\n",rate);
    usage();
  }

  if ( modf( 2*44100.0 / rate, &int_part) >= 0.5 ) {
      int_part += 1.0;
      fprintf( stderr, "Nearest available sample rate is %g Hertz\n",
	      2*44100.0 / int_part);
  }
  Halved = ((int) int_part) & 1;
  rate = 2*44100.0 / int_part;
  undersampling = (int) int_part / 2.0;
  samples_to_do = undersampling;

  if ( rectime < 0 ) {
    fprintf(stderr, "Error: incorrect recording time setting: %f\n",rectime);
    usage();
  }

  if (!strcmp((char *)int_name,"generic_scsi"))
      interface = GENERIC_SCSI;
  else if (!strcmp((char *)int_name,"cooked_ioctl"))
      interface = COOKED_IOCTL;
  else  {
    fprintf(stderr, "Error: incorrect interface setting: %s\n",int_name);
    usage();
  }

#ifdef MD5_SIGNATURES
  if (md5blocksize < 0) {
    fprintf(stderr, "Error: incorrect i MD-5 count: %d\n",md5blocksize);
    usage();
  }
#endif

  /* check * init audio file */
  if (!strncmp(audio_type,"wav",3)) {
    need_big_endian = 0;
    InitAudioFile = InitWav;
    ExitAudioFile = ExitWav;
    GetSndHdrSize = GetWavHdrSize;
  } else if (!strncmp(audio_type, "sun", 3)) {
    need_big_endian = 1;
    InitAudioFile = InitSun;
    ExitAudioFile = ExitSun;
    GetSndHdrSize = GetSunHdrSize;
  } else if (!strncmp(audio_type, "raw", 3)) {
    need_big_endian = 1;	/* for cdwrite compatibility */
    InitAudioFile = InitRaw;
    ExitAudioFile = ExitRaw;
    GetSndHdrSize = GetRawHdrSize;
  } else {
    fprintf(stderr, "Error: incorrect audio type setting: %3s\n", audio_type);
    usage();
  }

  if (no_file) fname_base[0] = '\0';

  /* setup interface and open cdrom device */
  /* request semaphores and shared memory */
  SetupInterface( int_name );

  /* get table of contents */
  tracks = ReadToc( g_toc );

  if ( verbose != 0 )
    fputs( "Cdda2wav version "VERSION"\n", stderr );

  if ( verbose & (SHOW_TOC | SHOW_STARTPOSITIONS) )
    DisplayToc ();

  if (just_the_toc) exit(0);

  if (nsectors < 1+overlap) {
      fprintf(stderr, "Error: overlap (%d) is >= nsectors (%d)\n",overlap, nsectors);
      exit(-1);
  }
  if ( verbose != 0 && overlap == 0)
      fprintf(stderr, "No overlap sampling active\n");

  /* paranoia reigns supreme */
  sync();

  /* try to get some extra kicks */
  nice(-20);

  /* switch cdrom to audio mode */
  EnableCdda (TRUE);

  atexit ( CloseAll );

  if ( !FirstTrack () )
    FatalError ( "This disk has no audio tracks\n" );

  if ( verbose & (SHOW_MCN | SHOW_ISRC) )
    Read_MCN_ISRC();

  /* check if track is in range */
  if ( track < 1 || track > tracks ) {
    fprintf(stderr, "Error: incorrect track setting: %d\n",track);
    usage();
  }

  if (cd_index != 1)
    sector_offset += ScanIndices( track, cd_index );
  else if (verbose & SHOW_INDICES)
    ScanIndices( track, cd_index );

  lSector = GetStartSector ( track );
  lSector_p1 = GetEndSector ( track ) + 1;

  if ( lSector < 0 )
    FatalError ( "track %d not found\n", track );

  lSector += sector_offset;
  /* check against end sector of track */
  if ( lSector >= lSector_p1 ) {
    fputs( "sector offset exceeds track size (ignored)\n", stderr );
    lSector -= sector_offset;
  }
  if ( rectime == 0.0 ) {
    /* set time to track time */
    nSamplesToDo = (lSector_p1 - lSector) * CD_FRAMESAMPLES;
    rectime = (lSector_p1 - lSector) / 75.0;
  } else {
    /* Prepare the maximum recording duration.
     * It is defined as the biggest amount of
     * adjacent audio sectors beginning with the
     * specified track/index/offset. */

    long lSector_p2 = GetLastSectorOnCd( track );
    if ( rectime > (lSector_p2 - lSector) / 75.0 ) {
      rectime = (lSector_p2 - lSector) / 75.0;
      lSector_p1 = lSector_p2;
    }

    /* calculate # of samples to read */
    nSamplesToDo = (long)(rectime*44100.0 + 0.5);
  }

  OutSampleSize = (1+bits/12);
  if (nSamplesToDo/undersampling == 0L) {
      fprintf( stderr, "time interval too short. Duration > %g secs!\n", 
	       undersampling/44100.0);
      exit(-2);
  }
  SamplesToWrite = nSamplesToDo*2/(int)int_part;

#ifdef	ECHO_TO_SOUNDCARD
  if (echo) {
    if ((soundcard_fd = open(SOUND_DEV, O_WRONLY, 0)) == EOF) {
        fprintf(stderr, "Cannot open "SOUND_DEV"\n");
        echo = 0;
    } else { 
	int dummy;
	int garbled_rate = rate;
	int stereo = (channels == 2);
	if (ioctl(soundcard_fd, (signed)SNDCTL_DSP_GETBLKSIZE, &dummy) == -1) {
	    fprintf(stderr, "Cannot get blocksize for "SOUND_DEV"\n");
	    echo = 0;
	}
	if (ioctl(soundcard_fd, (signed)SNDCTL_DSP_SYNC, NULL) == -1) {
	    fprintf(stderr, "Cannot sync for "SOUND_DEV"\n");
	    echo = 0;
	}
	if (ioctl(soundcard_fd, (signed)SNDCTL_DSP_SETFMT, &bits) == -1) {
	    fprintf(stderr, "Cannot set %d bits/sample for "SOUND_DEV"\n",bits);
	    echo = 0;
	}

	/* limited sound devices may not support stereo */
	if (stereo && 
	    ioctl(soundcard_fd, (signed)SNDCTL_DSP_STEREO, &stereo) == -1) {
	  fprintf(stderr, "Cannot set stereo mode for "SOUND_DEV"\n");
	  stereo = 0;
	}
	if (!stereo && ioctl(soundcard_fd, (signed)SNDCTL_DSP_STEREO, &stereo) == -1) {
	  fprintf(stderr, "Cannot set mono mode for "SOUND_DEV"\n");
	  echo = 0;
	}
	if (ioctl(soundcard_fd, (signed)SNDCTL_DSP_SPEED, &garbled_rate) == -1) {
	    fprintf(stderr, "Cannot set rate %g Hz for "SOUND_DEV"\n",rate);
	    echo = 0;
	}
	if ( abs((long)rate - garbled_rate) > rate / 20) {
	    fprintf(stderr, "sound device: next best sample rate is %d\n",garbled_rate);
	}
    }
  }
#endif

  if ( verbose & SHOW_SUMMARY ) {
      fprintf (stderr, "recording %.5f seconds %s with %d bits @ %g Hz"
	      ,rectime ,channels == 1 ? "mono":"stereo", bits, rate);
      if (*fname_base) fprintf(stderr, " ->'%s'...", fname_base );
      fputs("\n", stderr);
      if ( !waitforsignal )
	if (!bulk)
	  fprintf(stderr, "samplefile size will be %lu bytes.\n", GetSndHdrSize() +
	  SamplesToWrite*OutSampleSize*channels  ); 
	else {
	  int tracks_included;
	  tracks_included = GetTrack(
			      (unsigned) (lSector + nSamplesToDo/CD_FRAMESAMPLES -1))
				     - track + 1;
	  fprintf(stderr, "samplefiles size total will be %lu bytes. %d tracks\n", 
		  tracks_included * GetSndHdrSize() +
	  SamplesToWrite*OutSampleSize*channels, tracks_included  ); 
	}
  }

  current_track = track;

  if ( !no_file ) {
    if (bulk)
      sprintf(fname, "%s.%02ld.%s",fname_base,current_track,audio_type);
    else
      strcpy(fname,fname_base);
    OpenAudio( fname, rate, bits, channels, 
	      (unsigned)(SamplesToWrite*OutSampleSize*channels) );
  }

  Remainder = (75 % nsectors)+1;

  /* Everything is set up. Now fork and let one process read cdda sectors
     and let the other one store them in a wav file */

  sh_bits = 16 - bits;		/* shift counter */

  i = nSamplesToDo;
  if (Halved && (i&1))
      i += 2;
  swap = 0;

  /* forking */
  child_pid = fork();

  if (child_pid < 0) {
    perror("fork()");
  }
  /*********************** fork **************************************/
  if (child_pid == 0) {
    /* child READER section */

    while (1) {

      /* switch buffers */
      p = get_next_read_buffer();

      /* how many sectors should be read */
      SectorBurst = min(nsectors, 
	    overlap + ((i + CD_FRAMESAMPLES-1) / CD_FRAMESAMPLES));

      /* wait for buffer to be defined */
      if (semrequest(sem_id,swap) != 0) {
	/* semaphore operation failed.
	   try again...
         */
fprintf(stderr, "reader\n");
	if (semrequest(sem_id,swap) != 0) {
	  exit(3);
	}
      }
#if 0
fprintf(stderr,"reading %02ld from sec %6ld to %6ld...\t i=%ld\n",
       SectorBurst, lSector, lSector+SectorBurst-1, i);
#endif
      ReadCdRom( p, lSector, SectorBurst );

      /* Set buffer state to free */
      semrelease(sem_id,swap ^ 2);

      i -= (SectorBurst - overlap) * CD_FRAMESAMPLES;

      if (i <= overlap * CD_FRAMESAMPLES)
	i += CD_FRAMESAMPLES;

      if (i < 1) {
	  swap = (++swap & 1);
	  semrelease(sem_id,swap ^ 2);

	  /* the end, die */
	  exit(0);
      }

      lSector += SectorBurst - overlap;
      swap = !swap;
    }
    exit(0);
  } else {
    /* parent WRITER  section */
    /* don't need these any more.  Good security policy says we get rid
       of them ASAP */
    neverneedroot();
    neverneedgroup();

    semrelease(sem_id,swap+1);

    while (SkippedSamples + nSamplesDone <= SamplesToWrite) {

      /* switch buffers */
      p = get_next_read_buffer();

      /* how many sectors */
      SectorBurst = min(nsectors, 
          overlap + /* ??? */
	            (i + CD_FRAMESAMPLES-1) / CD_FRAMESAMPLES);

      /* set buffer state to free */
      semrelease(sem_id,swap);
      /* wait for buffer to be defined */
      if (semrequest(sem_id,swap ^ 2) != 0) {
	/* semaphore operation failed.
	   try again...
         */
fprintf(stderr, "writer: swap = %d\n",swap);
	if (semrequest(sem_id,swap ^ 2) != 0) {
	  exit(4);
	}
      }

      /* when track end is reached, close current file and start a new one */

      while (bulk && SectorBurst*CD_FRAMESAMPLES > SamplesToWrite) {
	/* lSector+SectorBurst > g_toc[current_track].dwStartSector) {*/
        if ( lSector < g_toc[current_track].dwStartSector ) {

#ifdef MD5_SIGNATURES
	  if (md5count) {
	    unsigned how_much = min(md5count, 
		    (g_toc[current_track].dwStartSector - lSector) * CD_FRAMESIZE_RAW);
	    MD5Update (&context, p, how_much);
	    md5count -= how_much;
	  }
#endif
	  if ( SaveBuffer ( p, channels,
	        (g_toc[current_track].dwStartSector - lSector) * CD_FRAMESAMPLES,
	        &nSamplesDone,
	        (g_toc[current_track].dwStartSector -
	         g_toc[current_track-1].dwStartSector) * CD_FRAMESAMPLES ) ) {
	    /* kill child */
	    kill(child_pid, SIGINT);
	    exit(2);
	  }
#if 0
fprintf(stderr, "wrote %08ld from %08ld samples\n", nSamplesDone, SamplesToWrite);
#endif
	  SamplesToWrite -= nSamplesDone;
	  if (SamplesToWrite == 0)
            nSamplesDone = 0;
#if 0
fprintf(stderr, "wrote %08ld from %08ld samples\n", nSamplesDone, SamplesToWrite);
#endif

	  /* move residual samples upto buffer start */
	  memmove(p,
              p + (g_toc[current_track].dwStartSector - lSector)
                          * CD_FRAMESIZE_RAW,
              (SectorBurst - (g_toc[current_track].dwStartSector - lSector))
              * CD_FRAMESIZE_RAW);
	  i -= (g_toc[current_track].dwStartSector - lSector) * CD_FRAMESAMPLES;
	  SectorBurst -= g_toc[current_track].dwStartSector - lSector;
	  lSector = g_toc[current_track].dwStartSector;
	}

	if ( !no_file && SamplesToWrite <= 0) {
	  /* finish sample file for this track */
	  CloseAudio(fname_base, current_track, bulk);

	  if (verbose)
	    fprintf( stderr, "track %2ld successfully recorded\n", current_track);
	}
	if ( nSamplesDone == 0 && SamplesToWrite > 0 ) {
	  if ( !no_file ) {

	    /* build next filename */
	    sprintf(fname, "%s.%02ld.%s",fname_base,current_track+1,audio_type);
	    OpenAudio( fname, rate, bits, channels,
		    (g_toc[current_track].dwStartSector -
		     g_toc[current_track-1].dwStartSector)*CD_FRAMESIZE_RAW);
	  }
        }
	current_track++;
	jitterShift = 0;
        if (current_track > tracks)
          goto leave;
      }

      {
	int curr_limit;
	curr_limit = bulk ? min(i,(
                  g_toc[current_track].dwStartSector-lSector)*CD_FRAMESAMPLES)
			      : i;

	if (curr_limit > SectorBurst * CD_FRAMESAMPLES)
	  curr_limit = SectorBurst * CD_FRAMESAMPLES;

#ifdef MD5_SIGNATURES
	if (md5count) {
	    unsigned how_much = min(md5count, curr_limit * 4);
	    MD5Update (&context, p, how_much);
	    md5count -= how_much;
	}
#endif
	if ( SaveBuffer ( p, channels, curr_limit, &nSamplesDone, SamplesToWrite ) ) {
	  /* kill child */
	  kill(child_pid, SIGINT);
	  exit(2);
	}
#if 0
fprintf(stderr, "wrote %08ld from %08ld samples\n", nSamplesDone, SamplesToWrite);
#endif
      }
      if (nSamplesDone + SkippedSamples >= SamplesToWrite) {
        int child_state;

	if (verbose && bulk)
	  fprintf( stderr, "track %2ld successfully recorded\n", current_track);
leave:
	/* kill child */
	kill(child_pid, SIGINT);
        wait(&child_state);
	exit(0);
      }

      if (nsectors > SectorBurst) {
	/* move incompletebuffer content towards the buffer end */
	memmove(p + (nsectors - SectorBurst) * CD_FRAMESIZE_RAW, p,
	        SectorBurst * CD_FRAMESIZE_RAW);
      }

      if (i == SamplesToWrite)
	  i += overlap * CD_FRAMESAMPLES;
      i -= (SectorBurst - overlap) * CD_FRAMESAMPLES;
      if (i < CD_FRAMESAMPLES) i = CD_FRAMESAMPLES;
      lSector += SectorBurst - overlap;
      swap = (++swap & 1);
    }
  }
  if (!quiet) fputs( "\n", stderr );

  return 0;
}
