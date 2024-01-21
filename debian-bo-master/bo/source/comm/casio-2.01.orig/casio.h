#include    <stdio.h>
#include    <string.h>
#include    <errno.h>
#include    <sys/time.h>
#include    <sys/types.h>
#include    <sys/stat.h>
#include    <unistd.h>
#include    <termios.h>
#include    <fcntl.h>
#include    <signal.h>
#include    <time.h>
#include    <string.h>
#include    <stdlib.h>

/*
 * defines:
 */
#define READ    1
#define WRITE   0
#define WAIT    1
#define NOWAIT  0
#define debug 1
#define debug2 1
#define TIMEOUT  60500L

/*
 * Control codes for handshake
 */
#if defined(Solaris2)
#define CR              '\r'
#define LF              '\n'
#else
#define CR              0x0d
#define LF              0x0a
#endif /* defined(Solaris2) */
#define XON             0x11
#define XOFF            0x13
#define ACK             0x23
#define NACK            0x3f
#define STOP            0x21
typedef unsigned char   byte ;
typedef unsigned short  word ;

/*
 * Structure of a record header as it is
 * exhanged with the CASIO
 */
typedef struct
{
        byte    marker ;        /* ':' marks the start of a record              */
        word    nbytes ;        /* 2 digit ASCII, # databytes                   */
        word    type ;          /* 2 digit ASCII, record type                   */
        word    low ;           /* 2 digit ASCII, low memory address            */
        word    high ;          /* 2 digit ASCII, high memory address           */
} CasioHeader ;

/*
 * Internal representation of such a header
 */
typedef struct
{
        byte    nbytes ;        /* number of databytes in the record            */
        byte    type ;          /* record type                                  */
        word    address ;       /* load address in memory                       */
} Header ;


/*
 * function prototypes:
 */
void UpperCase (char *s);
int ReadByte (byte mode);
void ReadHeader (void);
void WriteByte (byte d);
void WaitForCasio (void);
void WriteLine (FILE * infile);
void WriteBuffer (int num);
byte ReadHex (void);
byte atoh (char c);
void ReadLine (void);
char htoa (byte c);
void DisplayStatus (void);
int kbhit (void);
int tty_cbreak (int fd);	/* put stdin into a raw mode */
int tty_reset (int fd);		/* restore stdins's mode */
static void sig_catch (int);	/*signal catcher  */
void terminate ();		/* terminator */
