// Program     : x10-amh
// Version     : 1.05
// Description : X10 Model CP-290 control program (available at Radio Shack)
//               (computer control for X10 modules)
//               This application allows you to control lights and appliances
//               in your home either directly, or through a crontab
// Author      : Copyright (C) 1995 Aaron Hightower (aaron@paradigmsim.com)
//               1217 Shirley Way
//               Bedford, TX  76022 (817)267-6001
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <strings.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>

#include "x10-amh.h"

#define X10_ON            2
#define X10_OFF           3
#define X10_DIM           5

#define X10_MODE_NORMAL   8
#define X10_MODE_SECURITY 9
#define X10_MODE_TODAY    4
#define X10_MODE_TOMORROW 2
#define X10_MODE_CLEAR    0

#define VERSION         "v1.05"

/*
  Write n bytes from file descriptor, and don't return until done
  or return -1 if failure
*/

static int
write_n ( int fd, u_char *buf, int len )
{
  for ( int num=0; num<len; num++ ) {
        // Inter-character delay, because the CP-290 uses a software UART and
        // would drop a character while it is performing other processing.
    usleep(30);

    if ( write( fd, &buf[num], 1) != 1 ) {
      printf("Error writing to file: \n");

      switch ( errno ) {

        case EBADF: printf("fd is not a valid file descriptor or is not open\n"
                           "for reading.\n");
          return -1;

        case EINVAL: printf("fd is attached to an object which is unsuitable\n"
                            "for reading.\n");
          return -1;

        case EFAULT: printf("buf is outside your accessible address space.\n");
          return -1;

        case EPIPE: printf("fd is connected to a pipe or socket whose reading\n"
                       "end is closed. When this happens the writing pro-\n"
                       "cess will receive a SIGPIPE signal; if it catches,\n"
                       "blocks or ignores this the error EPIPE is returned.\n");
          break;

        case EAGAIN: // Socket was unable to receive data immediately
          break;

        case EINTR: // The call was interrupted by a signal
          break;    // before any data was written.

        case ENOSPC: // The device containing the file referred to by fd
          break;     // has no room for the data.
                     // Other errors may occur, depending on the object
                     // connected to fd.
      }
      return -1;
    }
  }
  return len;
}

/*
  Read n bytes from file descriptor, and don't return until done
*/

static int
read_n ( int fd, u_char *buf, int len )
{
  int num, rval;

  for ( num=0; num<len; num+=rval ) {

    if ( ( rval = read( fd, &buf[num], len-num ) ) < 0 ) rval = 0;
  }

  return ( num>0 ) ? num : ( -1 );
}

/*
  Constructors
*/

x10::x10( int _fd )
{
  this->fd = _fd;

  listvalid = 0;

  for( int i=0; i<128; i++ ) for( int j=0; j<8; j++ ) events[i][j] = j==0 ? 255 : 0;

  config_serial( );
}

x10::~x10( )
{
  close( this->fd );
}

/*
  Wait for ACK from CP-290 - return 0 if wack is ok
*/

int
x10::wreport( )
{
  u_char result[7];

  read_n( this->fd, result, 7 );
  {
    int ok=1;

    for( int i=0; i<6; i++ ) if( result[i] != 0xff ) ok=0;

    if( !ok ) {

      printf( "Leading sync from command upload is corrupt\n" );
      return 1;
    }
    if( !result[6] )
      printf( "*** Warning: the CP-290's memory has been reset" );

    read_n( this->fd, result, 5 );

    if( this->verbose ) {

      printf( "Received report from CP-290 that something has changed\n" );
    }
  }
  return 0;
}

int
x10::wack( )
{
  u_char result[7];

  read_n( this->fd, result, 7 );
  {
    int ok=1;

    for( int i=0; i<6; i++ ) if( result[i] != 0xff ) ok=0;

    if( !ok ) {

      printf("ACK from CP-290 was not received properly.  Fatal error\n" );
      return 1;
    }
    if( !result[6] )
      printf( "*** Warning: the CP-290's memory has been reset" );
  }
  return 0;
}

/*
  Dump out each byte received from the CP-290 - used for debugging
*/

void
x10::dump( )
{
  u_char c;

  while( 1 ) {

    read_n( this->fd, &c, 1 );
    printf( "%d\n", c );
  }
}

/*
  Send 16 bytes of 0xff's then send a message of len bytes to the CP-290
*/

void
x10::msg( u_char *msg, int len )
{
  const             NUM_RETRIES = 4;
  int num_retries = NUM_RETRIES;

  static u_char sync[16]={ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                           0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };

  for( num_retries = 4; num_retries; num_retries-- ) {

    if( write_n( this->fd, sync, 16 ) == 16 ) break;
    tcdrain( this->fd );
    sleep(10);
  }
  if( ! num_retries ) printf( "Error sending sync\n" );

  for( num_retries = 4; num_retries; num_retries-- ) {
        // Flush characters waiting to be read - they are from a previous
        // operation or a button-press on the CP-290.
    tcflush(this->fd, TCIFLUSH);

    if( write_n( this->fd, msg, len ) == len ) break;
    tcdrain( this->fd );
    sleep(10);
  }
  if( ! num_retries ) printf( "Error sending message\n" );

  tcdrain( this->fd );
}

/*
  Set the base housecode for the unit
*/

void
x10::send_housecode( )
{
  static unsigned char hc[2]={ 0, 0 }, answer[2];

  hc[1]=this->hc;

  printf("Resetting your base housecode will erase any memory in your "
         "X10 control unit.\n\n"
         "Are you sure you want to set the base housecode on your "
         "control unit to %c? ", this->hc_char );

  scanf( "%1s", answer );

  if( tolower( answer[0] )=='y' ) msg( hc, 2                  );
  else                            printf( "Operation aborted!\n" );
}

/*
  Return sum of len bytes
*/

u_char
x10::checksum( u_char *bytes, int len )
{
  u_char sum = 0;

  for( int i=0; i<len; i++ ) sum+=bytes[i];

  return sum;
}

/*
  convert string of numbers in the form of "1,2,3,4" to a bitmap appropriate
  for the X10 control module
*/

static u_short
strtobm( const char *string )
{
#define ITOBM(i)                (((i)<=16&&(i)>0)?(1<<(16-(i))):(0))
  u_short rval=0, cur=0;

  if( string ) {

    for( int i=0; string[i]; i++ ) {

      if( isdigit( string[i] ) ) cur = cur*10 + string[i] - '0';
      else {

        rval |= ITOBM( cur );
        cur = 0;
      }
    }

    rval |= ITOBM( cur );
  }

  return rval;
}

static const char *
bmtostr( u_short bmap )
{
  static char result[1024];

  strcpy( result, "0" );
  int i = 0;

  if( !bmap ) return result;
  else while ( bmap & 0xffffUL ) {

    i++;

    if( bmap & 0x8000UL ) {

      if( result[0] != '0' ) strcat( result, "," );
      else result[0] = '\0';

      char tmp[3];
      sprintf( tmp, "%d", i );
      strcat( result, tmp );
    }

    bmap <<= 1;
  }

  return result;
}

/*
  Send a direct message to the CP-290
*/

void
x10::direct( u_char command, char *str )
{
#define X10_MKBM_1_8(a)         (((a)&0xff00)>>8)
#define X10_MKBM_9_16(a)        (((a)&0xff))

  static u_char dc[6];
  u_short       bm;

  if( (bm = strtobm( str )) ) {

    if( this->verbose ) {

      char *brightness[] = {"Full brightness", "94% brightness",
           "88% brightness", "80% brightness", "three fourths brightness",
           "68% brightness", "62% brightness", "56% brightness", "half bright",
           "43% brightness", "37% brightness", "31% brightness",
           "25% brightness", "18% brightness", "12% brightness",
           "06% brightness", "Totally dark (you might as well turn it off!)" };

      switch( command & 15 ) {

        case X10_DIM:
          printf( "Dimming to %s: ", brightness[( command&0xf0 ) >> 4] );
          break;

        case X10_ON : 
          printf( "Turning on: " );
          break;

        case X10_OFF:
          printf( "Turning off: " );
          break;
      }

      for( int i=0;i<16;i++ ) {
        if( bm & ( ITOBM( i ) ) ) printf( "%d,", i );
      }
      printf( "\b \n" );
    }

    {
      const int cmd = command&15;

      if( cmd != X10_DIM && cmd != X10_ON && cmd != X10_OFF ) {
        printf( "Attempt to perform unknown command!\n" );
        return;
      }
    }

    dc[0] = 1; /* Direct command */
    dc[1] = command;
    dc[2] = this->hc;
    dc[3] = X10_MKBM_9_16( bm );
    dc[4] = X10_MKBM_1_8( bm );
    dc[5] = checksum( dc+1, 4 );

    msg( dc, 6 );
  }
  wack( );
  wreport( );
}

/*
  Configure the serial port that the CP-290 is attached to
*/

void
x10::config_serial( )
{
  static struct termios tios;

  if( ! this->fd ) {

    printf("Must set_fd() before config_serial()");
    return;
  }

  tcgetattr( this->fd, &tios );

  tios.c_iflag &= ~( BRKINT | IGNPAR | PARMRK | INPCK |
                     ISTRIP | INLCR  | IGNCR  | ICRNL | IXON );
  tios.c_iflag |=    IGNBRK | IXOFF;

  tios.c_lflag &= ~( ECHO   | ECHOE  | ECHOK  | ECHONL |
                     ICANON | ISIG   | NOFLSH | TOSTOP );

  tios.c_oflag &= ~( OFILL );
  tios.c_oflag |= CR3 | NL1;

  tios.c_cflag &= ~(  CRTSCTS | CSIZE | CSTOPB | HUPCL  | PARENB );
  tios.c_cflag |=    CLOCAL | CREAD  | CS8 ;

  cfsetospeed( &tios, B600 );
  cfsetispeed( &tios, B600 );

  tcsetattr( this->fd, TCSAFLUSH, &tios );
}

/*
  Explain a little bit of how to use this app
*/

static void
usage( char *name )
{
  printf( "x10 "VERSION" control program "
         "by Aaron Hightower (aaron@paradigmsim.com)\n"
         "=== Usage ===\n"
         "%s: [-v] [-c housecode] [-n list] [-f list] [-d dimlevel,list]\n\n"
         "-v : verbose , -t : self-test, -q : query CP-290's day and time\n"
         "-s : set CP-290's time according to CPU's day and time\n"
         "-z [a-p] : set the rocker button housecode for the CP290\n"
         "-c [a-p] : use alternate house code (default \"a\")\n"
         "-n list : turn oN devices in list\n"
         "-f list : turn oFf devices in list\n"
         "-e : empty conents of CP-290's internal events\n"
         "-l : list contents of CP-290 to stdout\n"
         "-d dimlevel,list : dim devices in list to dimlevel\n"
         "list is a comma separated list of devices, each ranging from 1 to 16"
         "\ndimlevel is an integer from 0 to 15 (0 brightest)\n"
         "\nExamples:\n"
         "\tTurn on devices a1 a2 a3: \"%s -n 1,2,3 \"\n"
         "\tTurn off devices b4 b6  : \"%s -c b -f 4,6\"\n"
         "\tdim lights 2 and 3 to 5th brightest setting: \"%s -d 5,2,3\"\n\n"
         "Note: All commands use house code \"a\" unless you use the \"-c\" "
         "argument.\nThe house code set by the \"-z\" command is for the "
         "buttons on the CP-290.\n"
          , name, name, name, name );
  exit( 0 );
}

/*
  Issue the selftest command to the CP-290 and report what it says
*/

void
x10::selftest( )
{
  u_char stest=7, result[7];

  printf( "Performing self-test (This will take about 10 seconds)\n" );
  msg( &stest, 1 );
  read_n( this->fd, result, 7 );

  {
    int i, ok = 1;

    for( i=0; i<6; i++ ) if( result[i] != 0xff ) ok = 0;
    if( result[6]!=0 ) ok = 0;

    if( ok ) printf( "Everything is okay!\n" );
    else     printf( "Sorry, something is wrong\n" );
  }
}

/*
  Set the time of the CP-290 based on the time of the CPU
*/

void
x10::settime( )
{
  char  *days[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
  u_char dow[7] = { 64, 1, 2, 4, 8, 16, 32 };
  u_char set[5] = { 2, 30, 7, 1, 0 };
  struct tm *tp;
  time_t secs;

  secs = time( NULL );
  tp = localtime( &secs );

  set[2] = tp->tm_hour;
  set[1] = tp->tm_min;
  set[3] = dow[tp->tm_wday];

  printf( "Setting time to %s %d:%02d\n", days[tp->tm_wday], set[2], set[1] );
  set[4] = checksum( set+1, 3 );
  msg( set, 5 );
}

/*
  Find out what the time of the CP-290 is currently set to
*/

void
x10::gettime( )
{
  u_char ask=4, result[12];
  char *day;

  msg( &ask,    1 );
  read_n( this->fd, result, 12 );
  msg( &ask,    1 );
  read_n( this->fd, result, 12 );

  switch( result[9] ) {

    case 64: day = "Sun";break;
    case 32: day = "Sat";break;
    case 16: day = "Fri";break;
    case  8: day = "Thu";break;
    case  4: day = "Wed";break;
    case  2: day = "Tue";break;
    case  1: day = "Mon";break;
    default: day = "???";break;
  }

  printf( "The CP-290's internal clock is set to: %s, %d:%02d\n",
    day, result[8], result[7] );
}

void
x10::readtab( )
{
  int    i;
  int    j;
  u_char result[8],
         ask=5;

  for ( j = 0; j < 6; j++ ) {
          // This seems to fail sometimes because of a read-buffer overrun.
          // flush the read buffer and start over, and hope I get all of the
          // characters this time.
          tcflush(this->fd, TCIOFLUSH);
          msg( &ask, 1   );
          if( -1 == read_n( this->fd, result, 7 ) )
                continue;

          for( i=0; i<6; i++ ) {
                        if( result[i] != 255 )
                                continue;
          }
          break;
          sleep(5);
  }
  if ( j == 7 ) {
        fprintf( stderr, "Did not receive sync from CP-290\n" );
        return;
  }
  if( result[6] == 0 ) {

    fprintf( stderr, "Warning!  CP-290 has been reset.  All memory has been lost\n" );
    listvalid = 1;
    return;
  }

  for( i=0; i<128; i++ ) {
    fprintf( stderr, "Reading slot %d .. ", i );

    read_n( this->fd, & events[i][0], 1 );

    if( events[i][0] != 255 ) {
      read_n( this->fd, & events[i][1], 7 );
      fprintf( stderr, "\n" );
    }
    else fprintf( stderr, "empty\n" );
  }

  listvalid = 1;

  fprintf( stderr, "\n" );
}

void
x10::emptytab( void )
{
  fprintf( stderr, "Erasing all of CP-290's internal memory\n" );

  bzero( events, sizeof( events ) );

  writetab( 1 );
}

void
x10::writetab( int empty )
{
  event e;

  e.id = 3;

  for( int i=0; i<128; i++ ) {

    if( events[i][4] == 0 && events[i][5] == 0 && empty != 1 ) continue;

    printf( "Writing timer event %d\n", i );

    printevent( i );

    e.ev_num[0] = ( i << 3 ) & 0xfc;
    e.ev_num[1] = ( i >> 5 ) & 0x03;
    e.mode      = events[i][0];
    e.daymap    = events[i][1];
    e.hour      = events[i][2];
    e.minute    = events[i][3];
    e.devbm[0]  = events[i][4];
    e.devbm[1]  = events[i][5];
    e.housecode = events[i][6];
    e.lev_fun   = events[i][7];

    e.checksum = checksum( ( unsigned char * ) &e, sizeof( event ) - 1 );

    msg( ( unsigned char * ) &e, sizeof( event ) );
  }
}

void
x10::printevent( int i )
{
  if( events[i][0] != 255 ) {

    int mode, daymap, hour, minute, devbm, housecode, level, function;

    mode      = events[i][0] & 0x0f;
    daymap    = events[i][1] & 0x7f;
    hour      = events[i][2] & 0x1f;
    minute    = events[i][3] & 0x3f;
    devbm     = ( ( ( int )events[i][4] ) << 8 ) + ( ( int ) events[i][5] );
    housecode = events[i][6] >> 4;
    level     = events[i][7] >> 4;
    function  = events[i][7] & 0x0f;

    char *modedesc;

    switch( mode ) {

      case X10_MODE_NORMAL:   modedesc = "normal";   break;
      case X10_MODE_SECURITY: modedesc = "security"; break;
      case X10_MODE_TODAY:    modedesc = "today";    break;
      case X10_MODE_TOMORROW: modedesc = "tomorrow"; break;
      default:                modedesc = "normal";   break;
    }

    char *funcdesc;

    switch( function ) {

      case X10_ON:  funcdesc = "on";  break;
      case X10_OFF: funcdesc = "off"; break;
      case X10_DIM: funcdesc = "dim"; break;
      default:      funcdesc = "on";  break;
    }

    switch( housecode ) {

      case 0xe: housecode = 'b'; break;
      case 0x2: housecode = 'c'; break;
      case 0xa: housecode = 'd'; break;
      case 0x1: housecode = 'e'; break;
      case 0x9: housecode = 'f'; break;
      case 0x5: housecode = 'g'; break;
      case 0xd: housecode = 'h'; break;
      case 0x7: housecode = 'i'; break;
      case 0xf: housecode = 'j'; break;
      case 0x3: housecode = 'k'; break;
      case 0xb: housecode = 'l'; break;
      case 0x0: housecode = 'm'; break;
      case 0x8: housecode = 'n'; break;
      case 0x4: housecode = 'o'; break;
      case 0xc: housecode = 'p'; break;
      case 0x6: // House code 'a' doesn't print since it's the default
      default:  housecode =  0 ; break;
    }

                    printf( "event {\n" );
                    printf( "  devmap    %s\n", bmtostr( devbm ) );
                    printf( "  daymap    %s\n", bmtostr( daymap << 9 ) );
                    printf( "  function  %s\n", funcdesc );
                    printf( "  hour      %d\n", hour );
    if( minute    ) printf( "  minute    %d\n", minute );
                    printf( "  mode      %s\n", modedesc );
    if( housecode ) printf( "  housecode %c\n", housecode );
    if( level     ) printf( "  dimlevel  %d\n", level );

    printf( "}\n\n" );
  }
}

/*
  List out the events that have been set in the CP-290 memory
  Assumes table already defined
*/

void
x10::listtab( )
{
  for( int i=0; i<128; i++ ) {

    printevent( i );
  }

  printf( "\n" );
}

/*
  Turn on verbose mode printing
*/

void
x10::verbose_on( )
{
  this->verbose = 1;
}

/*
  Change which file descriptor to use for direct commands, etc
*/

void
x10::setFD( int _fd )
{
  this->fd = _fd;
}

static int
chartohc( char c )
{
  static char *housecodes = "meckogainfdlphbj";

  c = tolower( c );

  if( c<'a' || c>'p' ) c = 'a';

  return ( ( strlen( housecodes ) - strlen( index( housecodes, c ) ) ) <<4 );
}

/*
  Change which housecode to use for direct commands, etc
*/

void
x10::housecode( int c )
{
  c = tolower( c );

  if( c<'a' || c>'p' ) c = 'a';

  this->hc_char = c;
  this->hc      = chartohc( c );
}

void
x10::readtab( const char *filename )
{
  char buffer[1024];
  FILE *file;

  file = fopen( filename, "r" );

  int i=0;

  char *housecode = 0;
  char *devmap    = 0;
  char *daymap    = 0;
  char *mode      = 0;
  int   minute    = 0;
  int   hour      = 0;
  int   dimlevel  = 0;
  char *func      = 0;

  while( ! feof( file ) ) {

    if( NULL == fgets( buffer, 1023, file ) ) break;

    if( buffer[0] == '#' ) continue;

    char *word = strtok( buffer, " \t\n" );

    while( word ) {

      // Support for hash comments
      if( word[0] == '#' ) break;

      // Support for C++ comments
      else if( word[0] == '/' && word[1] == '/' ) break; 

      if( !strcmp( word, "event" ) ) {

        word = strtok( NULL, " \t\n" );

        if( strcmp( word, "{" ) ) { /* } */

          fprintf( stderr, "Expecting an open brace" );
        }
      }
      else if( !strcmp( word, "housecode" ) ) housecode = strdup( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "devmap"    ) ) devmap    = strdup( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "daymap"    ) ) daymap    = strdup( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "mode"      ) ) mode      = strdup( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "minute"    ) ) minute    = atoi  ( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "hour"      ) ) hour      = atoi  ( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "dimlevel"  ) ) dimlevel  = atoi  ( word = strtok( NULL, " \t\n" ) );
      else if( !strcmp( word, "function"  ) ) func      = strdup( word = strtok( NULL, " \t\n" ) );

      else if( !strcmp( word, "}" ) ) {

        int intfunc;

        if( !func ) intfunc = X10_ON;
        else if( !strcasecmp( func, "on"  ) ) intfunc = X10_ON;
        else if( !strcasecmp( func, "off" ) ) intfunc = X10_OFF;
        else if( !strcasecmp( func, "dim" ) ) intfunc = X10_DIM;
        else intfunc = X10_ON;

        if( intfunc != X10_DIM ) dimlevel = 0;

        int intmode;

             if( !strcasecmp( mode, "normal"   ) ) intmode = X10_MODE_NORMAL;
        else if( !strcasecmp( mode, "security" ) ) intmode = X10_MODE_SECURITY;
        else if( !strcasecmp( mode, "today"    ) ) intmode = X10_MODE_TODAY;
        else if( !strcasecmp( mode, "tomorrow" ) ) intmode = X10_MODE_TOMORROW;
        else if( !strcasecmp( mode, "clear"    ) ) intmode = X10_MODE_CLEAR;
        else intmode = X10_MODE_NORMAL;

        if( housecode ) printf( "housecode = %s\n", housecode );
        if( devmap    ) printf( "devmap    = %s\n", devmap    );
        if( daymap    ) printf( "daymap    = %s\n", daymap    );
        if( mode      ) printf( "mode      = %s\n", mode      );
        if( minute    ) printf( "minute    = %d\n", minute    );
        if( hour      ) printf( "hour      = %d\n", hour      );
        if( func      ) printf( "function  = %s\n", func      );

        if( dimlevel )
          printf( "dimlevel  = %d\n", dimlevel  );

        printf( "\n" );

        setEvent( i++, housecode ? housecode[0] : 'a',
                  devmap, daymap, intmode, minute, hour,
                  dimlevel, intfunc );

        if( housecode ) free( housecode );
        if( devmap    ) free( devmap    );
        if( daymap    ) free( daymap    );
        if( func      ) free( func      );
        if( mode      ) free( mode      );

        housecode = 0;
        devmap    = 0;
        daymap    = 0;
        mode      = 0;
        minute    = 0;
        hour      = 0;
        dimlevel  = 0;
        func      = 0;
      }

      word = strtok( NULL, " \t\n" );
    }
  }
}

void
x10::setEvent( int i, char housecode, const char *devmap, const char *daymap,
               int mode, int minute, int hour, int dimlevel, int func )
{
  events[i][0] = ( mode   & 0x0f );
  events[i][1] = ( ( strtobm( daymap ? daymap : "1,2,3,4,5,6,7" ) ) >> 9 );
  events[i][2] = ( hour   & 0x1f );
  events[i][3] = ( minute & 0x3f );
  events[i][4] = ( strtobm( devmap ? devmap : "1" ) >> 8 ) & 0xff;
  events[i][5] = ( strtobm( devmap ? devmap : "1" ) >> 0 ) & 0xff;
  events[i][6] = chartohc( housecode );
  events[i][7] = ( ( dimlevel & 0xf ) << 4 ) | ( func & 0xf );
}

/*
  Main
*/

int
main( int argc, char * const argv[] )
{
  if( argc <= 1 ) {usage( argv[0] );exit( 0 );}

  char *portname = ( char * )getenv( "X10_PORTNAME" );

  if( !portname ) portname = "/dev/x10";

  int fd;

  if( ( fd = open( portname, O_RDWR|O_NOCTTY, 0 ) ) == -1 ) {

    printf( "Could not access serial port %s\nPlease set environment variable "
           "X10_PORTNAME to proper serial device\n"
           "or else make the appropriate soft link as superuser.\n", portname );
    exit( -1 );
  }

  x10 *x = new x10( fd );

  x->housecode('a');

  int c;

  while( ( c = getopt( argc, argv, "z:d:elvhc:n:f:qst" ) ) != -1 ) {

    switch( c ) {

      case 'z':
        x->housecode( optarg?( *optarg ):'a' );
        x->send_housecode( );
        break;

      case 'v':
        x->verbose_on( );
        break;

      case 'c':
        x->housecode( optarg ? ( *optarg ) : 'a' );
        break;

      case 'd':
        if( optarg ) { /* First number is the dimlevel (0..15) */
          int dimlevel;
          char *dim;

          dim = strdup( optarg );
          sscanf( optarg, "%d,%s", &dimlevel, dim );
          x->direct( X10_DIM + ( ( dimlevel & 0xf ) << 4 ), dim );
        }
        break;

      case 'e':
        x->emptytab( );
        break;

      case 'l':
        x->readtab( );
        x->listtab( );
        break;

      case 'n':
        x->direct( X10_ON, optarg );
        break;

      case 'f':
        x->direct( X10_OFF, optarg );
        break;

      case 't':
        x->selftest( );
        break;

      case 's':
        x->settime( );
        x->gettime( );
        break;

      case 'q':
        x->gettime( );
        break;

      case 'h':
      case '?':
        usage( argv[0] );
        break;
    }
  }

  if (optind < argc)
  {
    if( optind == argc - 1 ) {

      x->readtab( argv[ optind ] );
      x->listtab();
      x->writetab();
    }
  }

  exit( 0 );

  return 0;
}
