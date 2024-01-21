/****************************************************************************/
/* casio.c - CasioBoss - Version 2.00 */
/****************************************************************************
  revision history:
 
=========================================DOS==================
   version    date      programmer          comment
 
   1.00     8 aug 1993  L. Muhlenberg   Initial attempt
   1.01     9 aug 1993  L. Muhlenberg   Added XON/XOFF protocol
                                        Added version number
                                        Added progress indicator
   1.02    10 aug 1993  L. Muhlenberg   Cleaned up program
   1.03    11 aug 1993  L. Muhlenberg   Added info text (? or H option)
                                        Added 9600 baud support

=========================================LINUX==================
   version    date      programmer          comment

   2.00    25 oct 1995  J. Hadi Salim   refer to below 

   ported to LINUX (quiet a few changes; skeleton still the same)
   posix compliant -- which means it could be compiled on any
   Unix with (hopefully) minimal effort.

=========================================LINUX=&=Solaris 2.x====
   version    date      programmer          comment

   2.01    07 nov 1995  Birko Bergt     Added hton and ntoh stuff
                                        tty handling for Solaris 2.x
   2.01     16 Jan 1996  J. Hadi Salim  Added tty handling for sun4, sun5
 ****************************************************************************/

#include "casio.h"

#ifndef __i386__
#define swapl(x)	\
        ((x & 0xff000000U) >> 24) | \
        ((x & 0x00ff0000U) >>  8) | \
        ((x & 0x0000ff00U) <<  8) | \
        ((x & 0x000000ffU) << 24)
#define swaps(x)	\
        ((x & 0xff00) >> 8) | \
        ((x & 0x00ff) << 8)

#define ntohl(x)	swapl(x)
#define ntohs(x)	swaps(x)
#define htonl(x)	swapl(x)
#define htons(x)	swaps(x)

#define NTOHL(x)        (x) = ntohl((u_long)x)
#define NTOHS(x)        (x) = ntohs((u_short)x)
#define HTONL(x)        (x) = htonl((u_long)x)
#define HTONS(x)        (x) = htons((u_short)x)

#endif

/*
 * global variables:
 */
char *version = "2.01";
char *hello = "\nCASIO interface program Solaris 2.x , Sunos4.x, Sunos5.x and Linux v. %s\n\n";

char *info = "\
This program is a backup and restore utility for a CASIO digital diary.\n\
\n\
The syntax is:\n\
\n\
CASIO <-r|-w> <commport> <baudrate> <file>\n\
\n\
    -r|-w:    The direction of the data transfer. -r is used for reading\n\
              data from the CASIO to the PC. -w for the opposite direction.\n\
\n\
    commport: The PC's serial port that has to be used. Specify as COM1,\n\
              COM2,COM3 or COM4 or simply as 1,2,3 or 4. For Solaris and \n\
	      specify TTYA or TTYB or simply as A or B.\n\
\n\
    baudrate: Either 2400, 4800 or 9600 must be specified. Note that not\n\
              all communication speeds are supported by all CASIO's.\n\
\n\
    file:     The name of the file where the backup data are to be written\n\
              to or read from.\n\
\n\
The other communication parameters on the CASIO have to be set to 8N1 \n\
\n\
\n\
The layout of the used file is not directly in a readable format since\n\
all data records as needed for the CASIO are not really processed into\n\
a convenient format. The file data are used for backup/restore purposes\n\
only and should not be edited.\n\
\n\
CAUTION: Do not interface the CASIO directly to the PC's serial port, since\n\
         this may cause permanent damage to the CASIO. Always use the\n\
         circuit as described in circuit*.gif. It contains level converters\n\
         for converting the +12/-12 Volts levels on the PC side to the\n\
         +5/0 Volts levels that the CASIO uses.\n\
\n\
         Also keep in mind that the Secret Area of the CASIO has to be\n\
         backuped/restored separate.\n\
\n";

CasioHeader CHeader;

Header MHeader;

int Port;
unsigned
int Record;
int NOCHECK = 0;
int blkflg;

byte DataBuffer[512], WBuffer[1024], Direction;

byte nbytes;

byte Stopped;

byte LastRead, WriteStatus;
char buffer[20], bufin[20], ch, out;
static struct termios term;
static struct termios save_stdin;
static struct termios oldterm;
FILE *dbg;
FILE *data;
FILE *casiofile;
time_t now;
struct tm *tmstruct;
char tbuff[256];
char deb[256];
char dat[256];

int err;

main (int argc, char *argv[])
{
  Port = 0;
  /*
   * Check on argument count
   */
  if (argc > 1)
    {
      UpperCase (argv[1]);

      if (!strcmp (argv[1], "-?") || !strcmp (argv[1], "/?") ||
	  !strcmp (argv[1], "-H") || !strcmp (argv[1], "/H") ||
	  !strcmp (argv[1], "?") || !strcmp (argv[1], "H"))
	{
	  printf (hello, version);
	  printf ("%s", info);
	  return (0);
	}
    }

  if (argc != 5)
    {
      fprintf (stderr, hello, version);
      fprintf (stderr, "%s", info);
      return (-1);
    }

  /*
   * Read the data direction
   */
  if (!strcmp (argv[1], "-R"))
    {
      Direction = READ;
    }
  else if (!strcmp (argv[1], "-W"))
    {
      Direction = WRITE;
    }
  else
    {
      fprintf (stderr, "Direction must be \"-r\" or \"-w\"\n");
      return (-2);
    }
  time (&now);
  tmstruct = localtime (&now);
  strftime (tbuff, 10, ".%y%d%m", tmstruct);
  strcpy (deb, "debug");
  strcat (deb, tbuff);
  strcpy (dat, "data");
  strcat (dat, tbuff);

  dbg = fopen (deb, "w");
  data = fopen (dat, "w");


  /*
   * Read the COMM port that has to be used
   */
  UpperCase (argv[2]);

#if defined(Solaris2) || defined(Sunos)
  if (!strcmp (argv[2], "COM1") || !strcmp (argv[2], "1") ||
      !strcmp (argv[2], "TTYA") || !strcmp (argv[2], "A"))
    {
      if (debug)
        fprintf (dbg, "\nopened port ttya...\n");
      /* see if port can be opened for read/write */

      if ((Port = open ("/dev/ttya", O_RDWR)) < 0)
        {
          fprintf (stderr, "\nCan't open port ttya...\n");
          terminate ();
        }

    }
  else if (!strcmp (argv[2], "COM2") || !strcmp (argv[2], "2") ||
           !strcmp (argv[2], "TTYB") || !strcmp (argv[2], "B"))
    {
      /* see if port can be opened for read/write */
      if ((Port = open ("/dev/ttyb", O_RDWR)) < 0)
        {
          fprintf (stderr, "\nCan't open port ttyb...\n");
          terminate ();
        }
    }
  else
    {
      fprintf (stderr, "Unknown COM port\n");
      terminate ();
    }
#else
  if (!strcmp (argv[2], "COM1") || !strcmp (argv[2], "1"))
    {
      if (debug)
	fprintf (dbg, "\nopened port ttyS0...\n");
      /* see if port can be opened for read/write */

      if ((Port = open ("/dev/ttyS0", O_RDWR)) < 0)
	{
	  fprintf (stderr, "\nCan't open port ttyS0...\n");
	  terminate ();
	}

    }
  else if (!strcmp (argv[2], "COM2") || !strcmp (argv[2], "2"))
    {
      /* see if port can be opened for read/write */
      if ((Port = open ("/dev/ttyS1", O_RDWR)) < 0)
	{
	  fprintf (stderr, "\nCan't open port ttyS1...\n");
	  terminate ();
	}
    }
  else if (!strcmp (argv[2], "COM3") || !strcmp (argv[2], "3"))
    {
      /* see if port can be opened for read/write */
      if ((Port = open ("/dev/ttyS2", O_RDWR)) < 0)
	{
	  fprintf (stderr, "\nCan't open port ttyS2...\n");
	  terminate ();
	}
    }
  else if (!strcmp (argv[2], "COM4") || !strcmp (argv[2], "4"))
    {
      /* see if port can be opened for read/write */
      if ((Port = open ("/dev/ttyS3", O_RDWR)) < 0)
	{
	  fprintf (stderr, "\nCan't open port ttyS3...\n");
	  terminate ();
	}
    }
  else
    {
      fprintf (stderr, "Unknown COM port\n");
      terminate ();
    }
#endif /* defined(Solaris2) and Sunos*/

  if (Port == 0)
    {
      fprintf (stderr, "COM port not present\n");
      terminate ();
    }

#if !defined(Solaris2)
  if ((signal (SIGINT, sig_catch)) == SIG_ERR)	/* catch signals */
    fprintf (stderr, "signal(SIGINT) error %d", err);
#endif /* !defined(Solaris2) */
  if ((signal (SIGQUIT, sig_catch)) == SIG_ERR)
    fprintf (stderr, "signal(SIGINT) error %d", err);
  if ((signal (SIGTERM, sig_catch)) == SIG_ERR)
    fprintf (stderr, "signal(SIGINT) error %d", err);

/* even more error checking! */
  /* should be terminal device - exit if not */
  if (isatty (Port) == 0)
    {
      fprintf (stderr, "\nNot terminal device...\n");
      terminate ();
    }

  /* get port attributes, store in oldterm */
  if (tcgetattr (Port, &oldterm) < 0)
    {
      fprintf (stderr, "\ntcgetattr error...");
      terminate ();
    }
/* copy old into new; restore the old sometime when done */
  term = oldterm;

  /*
   * Read the baudrate
   */
  if (!strcmp (argv[3], "2400"))
    {
      term.c_cflag &= ~PARENB;
      term.c_cflag &= ~CSIZE;
      term.c_cflag |= B2400 | CREAD | CS8 | CLOCAL;
      cfsetispeed (&term, B2400);
      cfsetospeed (&term, B2400);
    }
  else if (!strcmp (argv[3], "4800"))
    {
      term.c_cflag &= ~PARENB;
      term.c_cflag &= ~CSIZE;
      term.c_cflag |= B4800 | CREAD | CS8 | CLOCAL;
      cfsetispeed (&term, B4800);
      cfsetospeed (&term, B4800);
    }
  else if (!strcmp (argv[3], "9600"))
    {
      term.c_cflag &= ~PARENB;
      term.c_cflag &= ~CSIZE;
      term.c_cflag |= B9600 | CREAD | CS8 | CLOCAL;
      cfsetispeed (&term, B9600);
      cfsetospeed (&term, B9600);
    }
  else
    {
      fprintf (stderr, "Baudrate must be 2400, 4800 or 9600\n");
      terminate ();
    }

  /* set non-canonical mode */
  term.c_lflag = 0;

/*
   Ignore bytes with parity errors and make terminal raw and dumb.
 */
  term.c_iflag = 0;
  term.c_iflag |= IGNBRK;
  term.c_iflag |= IGNPAR;

  term.c_oflag &= ~OPOST;
/* blocking read until 1 char arrives */
  term.c_cc[VMIN] = 1;
  term.c_cc[VTIME] = 0;

  /*
   * Open the required file
   */
  if (Direction == READ)
    {
      term.c_iflag |= IXOFF;
      term.c_cc[VSTART] = XON;
      term.c_cc[VSTOP] = XOFF;
      if ((casiofile = fopen (argv[4], "wb")) == (FILE *) 0)
	{
	  fprintf (stderr, "Cannot create output file\n");
	  terminate ();
	}
    }
  else
    {
      if ((casiofile = fopen (argv[4], "rb")) == (FILE *) 0)
	{
	  fprintf (stderr, "Cannot open input file\n");
	  terminate ();
	}
    }
/* now clean the serial line and activate the settings for casio */
  tcflush (Port, TCIOFLUSH);


  if (tcsetattr (Port, TCSAFLUSH, &term) < 0)
    {
      fprintf (stderr, "\n failed to set port attr ... exiting \n");
      terminate ();
    }

  Stopped = 0;
  WriteStatus = 0;
  LastRead = 0;
  printf (hello, version);
  /* set the stdin to cbreak mode */
  tty_cbreak (STDIN_FILENO);

  while (!Stopped)
    {
      if (Direction == READ)
	{
	  fprintf (stderr, "\nRead data from CASIO to file %s\n", argv[4]);
	  if (debug)
	    fprintf (dbg, "\nRead data to file %s\n", argv[4]);

	  WaitForCasio ();

	  if (!Stopped)
	    {
	      fprintf (stderr, "Casio is ready to send\n");
	      fprintf (stderr, "Hit ESC to terminate at any time \n");
	    }

	  while (!Stopped)
	    {
	      if (debug)
		fprintf (dbg, "\n++++++++++++++++++++++++++++++++++++++++++++\n");
	      ReadLine ();
	      if (debug2)
		fprintf (dbg, "\nwriting mheader to casiofile: \n");

	      fwrite (&MHeader, sizeof (MHeader), 1, casiofile);
	      if (debug)
		fprintf (dbg, "\nwriting Databuffer to casiofile: \n");
	      fwrite (DataBuffer, 1, MHeader.nbytes + 1, casiofile);
	      if (debug2)
		fprintf (dbg, "\ncalling display status\n");
	      DisplayStatus ();

	    }
	  if (debug2)
	    fprintf (dbg, "\n =========> stopped =1? real=%d\n", Stopped);
	}
      else
	{
	  fprintf (stderr, "\n\nWrite data from file %s to CASIO\n\n", argv[4]);
	  fprintf (stderr, "Hit ESC to terminate at any time \n");

	  /* as a hack around the original dos code -- set the read
	     to be non-blocking; establish connection; then set it back
	     to block; this is because the very first time we start and
	     try to read we block forever whereas the casio is waiting
	     for a CR-LF pair */

	  blkflg |= O_NONBLOCK;
	  if (fcntl (Port, F_SETFL, blkflg) < 0)
	    {
	      fprintf (stderr, "\nexiting ..\n");
	      terminate ();
	    }
	  WaitForCasio ();

	  blkflg &= ~O_NONBLOCK;
	  if (fcntl (Port, F_SETFL, blkflg) < 0)
	    {
	      fprintf (stderr, "\nexiting ..\n");
	      terminate ();
	    }
	  if (!Stopped)
	    {
	      fprintf (stderr, "Casio is ready to receive\n");
	      if (debug)
		fprintf (dbg, "Casio is ready to receive\n");
	    }

	  while (!Stopped)
	    {
	      WriteLine (casiofile);
	      DisplayStatus ();
	    }

	  Stopped = 1;
	}
    }

  terminate ();
}

/*
 * W a i t F o r C a s i o
 *
 *      Establish communication with the CASIO
 *      by exchanging the CR-LF and ACK controls
 */

void
WaitForCasio ()
{
  byte i;

  if (Direction == READ)
    {
      while (!Stopped)
	{
	  do
	    {
	      i = ReadByte (WAIT);
	    }
	  while (i != CR && !Stopped);
	  if (Stopped)
	    {
	      terminate ();
	    }

	  i = ReadByte (WAIT);

	  if (i == LF)
	    {
	      WriteByte (XON);
	      WriteStatus = XON;
	      return;
	    }
	}
    }
  else
    {
      NOCHECK = 1;
      while (!Stopped)
	{
	  if (debug2)
	    fprintf (dbg, "Write: Wait for casio\n");
	  WriteByte (CR);
	  WriteByte (LF);
	  /* somehow this is needed;inter-byte write time;
	     could probably set it in termios next time */
	  usleep (36000);

	  if ((ReadByte (NOWAIT) == XON) || (WriteStatus == XON))
	    {
	      return;
	    }
	}
      NOCHECK = 0;
    }
}

/*
 * R e a d H e a d e r
 *
 *      Read a record header from the CASIO
 */

void
ReadHeader (void)
{
  /*
   * Wait until ':'
   */
  if (debug2)
    fprintf (dbg, "\nReadheader waiting for a :\n");
  do
    {
      CHeader.marker = ReadByte (WAIT);
    }
  while (CHeader.marker != ':' && !Stopped);
  if (debug2)
    fprintf (dbg, "\nReadheader got a :\n");

  CHeader.nbytes = ((ReadByte (WAIT) & 0xff) << 8) |
    (ReadByte (WAIT) & 0xff);
  if (debug2)
    fprintf (dbg, "\nReadheader got nbytes %x \n", CHeader.nbytes);

  CHeader.type = ((ReadByte (WAIT) & 0xff) << 8) |
    (ReadByte (WAIT) & 0xff);

  if (debug2)
    fprintf (dbg, "\nReadheader got type %x \n", CHeader.type);

  CHeader.low = ((ReadByte (WAIT) & 0xff) << 8) |
    (ReadByte (WAIT) & 0xff);

  if (debug2)
    fprintf (dbg, "\nReadheader got low %x \n", CHeader.low);

  CHeader.high = ((ReadByte (WAIT) & 0xff) << 8) |
    (ReadByte (WAIT) & 0xff);

  if (debug2)
    fprintf (dbg, "\nReadheader got high %x \n", CHeader.high);

  /*
   * Casio header is read, now translate it
   * into something usefull
   */
  MHeader.nbytes = (atoh (CHeader.nbytes >> 8) << 4) |
    atoh (CHeader.nbytes & 0xff);

  if (debug2)
    fprintf (dbg, "\nReadheader translated nbytes %x \n", MHeader.nbytes);

  MHeader.type = (atoh (CHeader.type >> 8) << 4) |
    atoh (CHeader.type & 0xff);

  if (debug)
    fprintf (dbg, "\nReadheader: frame type %x \n", MHeader.type);

  MHeader.address = (atoh (CHeader.high >> 8) << 4) |
    atoh (CHeader.high & 0xff);

  MHeader.address *= 256;

  MHeader.address += (atoh (CHeader.low >> 8) << 4) |
    atoh (CHeader.low & 0xff);

  if (debug2)
    fprintf (dbg, "\nReadheader translated address %x \n", MHeader.address);
}

/*
 * R e a d L i n e
 *
 *      Read one complete record from the CASIO
 */

void
ReadLine ()
{
  /*
   * Read the CASIO record header
   */
  if (debug2)
    fprintf (dbg, "calling readheader\n");

  ReadHeader ();

  if (debug2)
    fprintf (dbg, "\nDone ReadHeader\n");
  if (MHeader.nbytes == 0x00)
    {
      if (debug)
	fprintf (dbg, "\nsaw last data frame\n");
      if (MHeader.address == 0x0100)
	{
	  if (debug)
	    fprintf (dbg, "\nsending ack to continue tarnsmission \n");
	  WriteByte (ACK);
	}
    }
  /*
   * Read the data bytes in the record
   */
  for (nbytes = 0; nbytes < MHeader.nbytes && !Stopped; nbytes++)
    {
      DataBuffer[nbytes] = ReadHex ();

      if (debug)
	fprintf (dbg, "\nDatabytes --->  %c\n", DataBuffer[nbytes]);

      /* store the databyte into the data file */
      fprintf (data, "%c", DataBuffer[nbytes]);
    }
  fprintf (data, "\n");

  /*
   * Read the checksum
   */
  DataBuffer[nbytes++] = ReadHex ();

  if (debug2)
    fprintf (dbg, "\nDone readline\n");
}

/*
 * W r i t e L i n e
 *
 *      Write one complete record from
 *      the file to the CASIO
 */
void
WriteLine (FILE * infile)
{
  int i;
  /*
   * Read the next record header from the file
   */
  if (fread (&MHeader, sizeof (MHeader), 1, infile) != 1)
    {
      Stopped = 1;
      return;
    }

  /*
   * Read the databytes and the checksum from the file
   */
  fread (DataBuffer, MHeader.nbytes + 1, 1, infile);

  /*
   * Convert record to something for CASIO
   */
  WBuffer[0] = ':';
  WBuffer[1] = htoa (MHeader.nbytes >> 4);
  WBuffer[2] = htoa (MHeader.nbytes);
  WBuffer[3] = htoa (MHeader.type >> 4);
  WBuffer[4] = htoa (MHeader.type);
  WBuffer[5] = htoa (MHeader.address >> 4);
  WBuffer[6] = htoa (MHeader.address);
  WBuffer[7] = htoa (MHeader.address >> 12);
  WBuffer[8] = htoa (MHeader.address >> 8);

  for (i = 0; i < MHeader.nbytes + 1; i++)
    {
      WBuffer[2 * i + 9] = htoa (DataBuffer[i] >> 4);
      WBuffer[2 * i + 10] = htoa (DataBuffer[i] & 0x0f);
    }

  WriteBuffer (2 * MHeader.nbytes + 11);

  /*
   * Wait for reply
   */
  if (MHeader.nbytes == 0 && MHeader.address == 0x0100)
    {
      while (!Stopped)
	{
	  i = ReadByte (WAIT);

	  switch (i)
	    {
	    case NACK:
	      fprintf (stderr, "\nTransmission error, stopped\n");
	      Stopped = 1;
	      WriteByte (STOP);
	      break;

	    case ACK:
	      return;

	    case STOP:
	    case XON:
	    case XOFF:
	      return;

	    default:
	      fprintf (stderr, "0x%02x\n", i);
	      return;
	    }
	}
    }
}

/*
 * W r i t e B u f f e r
 *
 *      Write the data in the CASIO Write Buffer
 *      to the CASIO
 */

void
WriteBuffer (int num)
{
  int i;

  for (i = 0; i < num; i++)
    {
      WriteByte (WBuffer[i]);
    }

}


/*
 * U p p e r C a s e
 *
 *  Turn a string into all uppercase characters
 */

void
UpperCase (char *s)
{
  while (*s)
    {
      if (*s >= 'a' && *s <= 'z')
	{
	  *s = *s - 'a' + 'A';
	}

      s++;
    }
}

/*
 * R e a d B y t e
 *
 *  Read a byte from the COM port
 */

int
ReadByte (byte mode)
{
  int i, j;

  long TimeOut;
  if (LastRead)
    {
      if (debug2)
	fprintf (dbg, "we did read %c before ==> just return \n", LastRead);
      i = LastRead;
      LastRead = 0;
      return (i);
    }


  while (1)
    {
      for (TimeOut = 0; TimeOut < TIMEOUT; TimeOut++)
	{
	  if (Stopped)
	    {
	      return (-1);
	    }

	  /*
	   * Check if the user pressed ESC to stop communication
	   */
	  if (kbhit ())
	    {
	      if (getchar () == 0x1b)
		{
		  fprintf (stderr, "\n\nESC pressed, stop communication\n");
		  Stopped = 1;
		  WriteByte (STOP);
		  return (-1);
		}
	    }

	  if (debug2)
	    fprintf (dbg, "\n trying to read\n ");

	  /* read a char and store in i */
	  j = read (Port, &i, 1);

#ifndef __i386__
          NTOHL(i);
#endif 
	  if (debug2)
	    fprintf (dbg, "\n read\n ");
	  if (j == 0)
	    {
	      if (debug2)
		fprintf (dbg, "\nEOF encountered\n");
	    }
	  if (j > 0)
	    {
	      if (debug2)
		fprintf (dbg, "\nsuccesful READ\n");
	    }
	  if (j == -1)
	    {
	      if (debug2)
		fprintf (dbg, "\nUnsuccessfull READ\n");
	    }
	  sprintf (bufin, "received %x\n", i & 0xff);
	  if (debug)
	    fprintf (dbg, "%s\n", bufin);

	  i &= 0xff;
	  if (i == STOP)
	    {
	      fprintf (stderr, "\nCASIO has stopped communication\n");
	      fprintf (dbg, "\nSTOP received \n");
	      Stopped = 1;
	    }

	  if (i == XOFF)
	    {
	      WriteStatus = XOFF;
	    }

	  if (i == XON)
	    {
	      WriteStatus = XON;
	    }
	  return (i);
	}
      fprintf (stderr, "\n timeout\n");


      if (mode == NOWAIT)
	{
	  return (-1);
	}
    }
}

/*
 * W r i t e B y t e
 *
 *  Write a byte to the COM port
 */

void
WriteByte (byte d)
{
  int i;
  long TimeOut;

  if (debug2)
    fprintf (dbg, "\nWRITEBYTE: writestatus = %x\n", WriteStatus);
  if (!NOCHECK && !Stopped)
    {

      if ((i = ReadByte (NOWAIT)) == XOFF)
	{
	  WriteStatus = XOFF;
	}
      else
	{
	  LastRead = i;
	}

      if (WriteStatus == XOFF)
	{
	  while (ReadByte (WAIT) != XON && !Stopped);

	  WriteStatus = XON;
	}
    }

  TimeOut = 0;
  if (debug)
    fprintf (dbg, "\n sending %x to serial port \n", d);

#ifndef __i386__
  HTONL(i);
#endif
  /* write the character to the serial port */
  if (write (Port, &d, 1) != 1)
    {
      fprintf (stderr, "\nERROR WriteByte: couldnt write the char %c to serial port \n", d);
      terminate ();
    }
  if (Direction == WRITE)
    {
      usleep (4000);
    }
}

/*
 * R e a d H e x
 *
 *      Read two ASCII digits from the COM port
 *      and convert these into a hexadecimal value
 */

byte
ReadHex ()
{
  return ((atoh (ReadByte (WAIT)) << 4) | (atoh (ReadByte (WAIT))));
}

/*
 * a t o h
 *
 *      Convert an ASCII number to a hexadecimal value
 */

byte
atoh (char c)
{
  if (c >= '0' && c <= '9')
    {
      return (c - '0');
    }

  if (c >= 'a' && c <= 'f')
    {
      return (c - 'a' + 10);
    }

  if (c >= 'A' && c <= 'F')
    {
      return (c - 'A' + 10);
    }

  return (0);
}

/*
 * h t o a
 *
 *      Convert a hexadecimal value to an ASCII number
 */

char
htoa (byte c)
{
  c &= 0xf;

  if (c <= 9)
    {
      return (c + '0');
    }

  return (c - 10 + 'A');
}

/*
 * D i s p l a y S t a t u s
 */
void
DisplayStatus ()
{
  if (debug2)
    {
      fprintf (dbg, "\n Dispaly status: Mheader,nbytes %x\n", MHeader.nbytes);
      fprintf (dbg, "\n Dispaly status: DatadBuffer[0] %x\n", DataBuffer[0]);
    }
  if (MHeader.nbytes == 0x02)
    {
      switch (DataBuffer[0])
	{
	case 0x90:
	  {
	    fprintf (stderr, "\nPhone:         ");
	    if (debug)
	      fprintf (dbg, "\n Data ==> phone\n");
	    fprintf (data, "\n===================== > phone < ==========\n");
	    Record = 0;
	    break;
	  }
	case 0xa0:
	  {
	    fprintf (stderr, "\nMemo:          ");
	    if (debug)
	      fprintf (dbg, "\n Data ==> memo\n");
	    fprintf (data, "\n===================== > MEMO < =============\n");
	    Record = 0;
	    break;
	  }
	case 0x91:
	  {
	    fprintf (stderr, "\nReminder:      ");
	    if (debug)
	      fprintf (dbg, "\n Data ==> reminder\n");
	    fprintf (data, "\n===================== > REMINDER < =============\n");
	    Record = 0;
	    break;
	  }
	case 0xb0:
	  {
	    fprintf (stderr, "\nSchedule:      ");
	    if (debug)
	      fprintf (dbg, "\n Data ==> schedule\n");
	    fprintf (data, "\n===================== > SCHEDULE < =============\n");
	    Record = 0;
	    break;
	  }
	case 0x80:
	  {
	    fprintf (stderr, "\nCalendar:      ");
	    if (debug)
	      fprintf (dbg, "\n Data ==> calendar\n");
	    fprintf (data, "\n===================== > Calendar < =============\n");
	    Record = 0;
	    break;
	  }
	default:
	  {
	    fprintf (data, "\n===================== > UNKNOWN? < =============\n");
	    fprintf (stderr, "\nSection 0x%02x:    ", DataBuffer[0]);
	  }

	  Record = 0;
	}
    }
  else if (MHeader.nbytes == 0x00)
    {
      if (MHeader.address == 0x0100)
	{
	  fprintf (stderr, "\b\b\b%3d", ++Record);
	}
      else if (MHeader.address == 0xff00)
	{
	  fprintf (stderr, "\nDONE!!\n");
	  Stopped = 1;
	}
    }
}


/******* poll the keyboard for a character **********/
/********* idea borrowed from The Linux Journal article
of Sept 95 issue 17 */
int
kbhit (void)
{

  struct timeval tv;
  fd_set read_fd;

/*do not wait at all, not even a microsecond */
  tv.tv_sec = 0;
  tv.tv_usec = 0;

/* must be done first to initilize read_fd */

  FD_ZERO (&read_fd);

/* makes select() ask if input is ready :
   * 0 is the file descriptor stdin  */
  FD_SET (0, &read_fd);

/* the first parameter is the number of the 
   * largest file descriptor to check + 1.  */

  if (select (1, &read_fd,
	      NULL,		/* NO writes */
	      NULL,		/* NO exceptions */
	      &tv)
      == -1)
    return 0;			/* An error occured */
/* read_fd now holds a bitmap of files that are
   * readable. We test the entry for the standard
   * input (file 0). */

  if (FD_ISSET (0, &read_fd))
/* character pending on stdin */
    return 1;

/* no charcaters were pending */

  return 0;
}

static void
sig_catch (int signo)
{
  fprintf (stderr, "\nsignal caught %d\n", signo);
  fprintf (dbg, "\nsignal caught %d\n", signo);
  /*inform */
  fprintf (stderr, "\nstopping contact with casio \n\n");
  /* should also reset the serial port being used by
     casio and close all files */
  terminate ();
}


int
tty_reset (int fd)		/* restore terminal's mode */
{
  if (fd == Port)
    {
      if (tcsetattr (fd, TCSAFLUSH, &oldterm) < 0)
	fprintf (stderr, "\nFailed to reset serial port\n");
      return (-1);
    }
  else if (fd == STDIN_FILENO)
    {
      if (tcsetattr (fd, TCSAFLUSH, &save_stdin) < 0)
	fprintf (stderr, "\nFailed to reset stdin\n");
      return (-1);
    }
  return (0);
}

/* the terminator */
void
terminate ()
{

  blkflg |= O_NONBLOCK;
  if (fcntl (Port, F_SETFL, blkflg) < 0)
    {
      fprintf (stderr, "\nexiting ..\n");
      exit (-1);
    }
  if (debug)
    fprintf (dbg, "\nterminate: writting stop to port\n");
  Stopped = 1;
  WriteByte (STOP);

/*reset terminals */
  if (debug)
    fprintf (dbg, "\n reseting stdin\n");
  tty_reset (STDIN_FILENO);
  if (debug)
    fprintf (dbg, "\n reseting port\n");
  tty_reset (Port);

/*close all open files */
  if (debug)
    fprintf (dbg, "\n closing casiofile\n");
  fclose (casiofile);
  if (debug)
    fprintf (dbg, "\n closing Port\n");
  close (Port);
  if (debug)
    fprintf (dbg, "\n closing data file\n");
  fclose (data);
  if (debug)
    fprintf (dbg, "\n closing debug file\n");
  fclose (dbg);
  exit (0);
}

/* borrowed from W. Richard Stevens book
Advanced Programming in the Unix Environment */

int
tty_cbreak (int fd)		/* put terminal into a cbreak mode */
{
  struct termios buf;

  if (fd == STDIN_FILENO)
    {
      if (tcgetattr (fd, &save_stdin) < 0)
	return (-1);
    }
  buf = save_stdin;		/* structure copy */

  buf.c_lflag &= ~(ECHO | ICANON);
  /* echo off, canonical mode off */

  buf.c_cc[VMIN] = 1;		/* Case B: 1 byte at a time, no timer */
  buf.c_cc[VTIME] = 0;

  if (tcsetattr (fd, TCSAFLUSH, &buf) < 0)
    return (-1);
  return (0);
}
