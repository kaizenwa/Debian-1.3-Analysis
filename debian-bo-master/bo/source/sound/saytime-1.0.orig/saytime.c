/*
** saytime - audio time hack for the SPARCstation.
**
** Copyright (C) 1990 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include <time.h>
#include <sys/file.h>
#include <sys/fcntl.h>

/* Here are the defines for the thirty different phrases the program
** needs.  If you want to substitute your own voice for mine, I suggest
** you record the following lines:
**
**     The time is eight o'clock exactly.
**     The time is eight oh-eight and one second.
**     The time is eight oh-eight and eight seconds.
**     One.  Two.  Three.  Four.  Five.  Six.  Seven.  Eight.  Nine.  Ten.
**     Eleven.  Twelve.  Thirteen.  Fourteen.  Fifteen.  Sixteen.  Seventeen.
**     Eighteen.  Nineteen.  Twenty.  Thirty.  Forty.  Fifty.
**
** Then use Sun's sound demo to dissect the lines into separate files.
** It's not really that much work, it took me about an hour.
*/

#define PH_ONE		1
#define PH_TWO		2
#define PH_THREE	3
#define PH_FOUR		4
#define PH_FIVE		5
#define PH_SIX		6
#define PH_SEVEN	7
#define PH_EIGHT	8
#define PH_NINE		9
#define PH_TEN		10
#define PH_ELEVEN	11
#define PH_TWELVE	12
#define PH_THIRTEEN	13
#define PH_FOURTEEN	14
#define PH_FIFTEEN	15
#define PH_SIXTEEN	16
#define PH_SEVENTEEN	17
#define PH_EIGHTEEN	18
#define PH_NINETEEN	19
#define PH_TWENTY	20
#define PH_THIRTY	21
#define PH_FORTY	22
#define PH_FIFTY	23
#define PH_THE_TIME_IS	24
#define PH_OCLOCK	25
#define PH_OH		26
#define PH_EXACTLY	27
#define PH_AND		28
#define PH_SECOND	29
#define PH_SECONDS	30

void saynumber(), saydigit(), sayphrase(), sayfile(), sayclose();

main( argc, argv )
int argc;
char *argv[];
    {
    long clock;
    struct tm *t;

    clock = time( (long *) 0 );
    t = localtime( &clock );

    sayphrase( PH_THE_TIME_IS );

    if ( t->tm_hour == 0 )
	saynumber( 12, 0 );
    else if ( t->tm_hour > 12 )
	saynumber( t->tm_hour - 12, 0 );
    else
	saynumber( t->tm_hour, 0 );

    if ( t->tm_min == 0 )
	sayphrase ( PH_OCLOCK );
    else
	saynumber( t->tm_min, 1 );

    if ( t->tm_sec == 0 )
	sayphrase( PH_EXACTLY );
    else
	{
	sayphrase( PH_AND );
	saynumber( t->tm_sec, 0 );
	if ( t->tm_sec == 1 )
	    sayphrase( PH_SECOND );
	else
	    sayphrase( PH_SECONDS );
	}

    sayclose( );
    exit( 0 );
    }

void
saynumber( n, leadingzero )
int n, leadingzero;
    {
    int ones, tens;

    ones = n % 10;
    tens = n / 10;

    switch ( tens )
	{
	case 0:
	if ( leadingzero )
	    sayphrase( PH_OH );
	saydigit( ones );
	break;

	case 1:
	switch ( ones )
	    {
	    case 0:
	    sayphrase( PH_TEN );
	    break;

	    case 1:
	    sayphrase( PH_ELEVEN );
	    break;

	    case 2:
	    sayphrase( PH_TWELVE );
	    break;

	    case 3:
	    sayphrase( PH_THIRTEEN );
	    break;

	    case 4:
	    sayphrase( PH_FOURTEEN );
	    break;

	    case 5:
	    sayphrase( PH_FIFTEEN );
	    break;

	    case 6:
	    sayphrase( PH_SIXTEEN );
	    break;

	    case 7:
	    sayphrase( PH_SEVENTEEN );
	    break;

	    case 8:
	    sayphrase( PH_EIGHTEEN );
	    break;

	    case 9:
	    sayphrase( PH_NINETEEN );
	    break;

	    default:
	    (void) fprintf( stderr, "Shouldn't happen.\n" );
	    exit( 1 );
	    }
	break;

	case 2:
	sayphrase( PH_TWENTY );
	if ( ones != 0 )
	    saydigit( ones );
	break;

	case 3:
	sayphrase( PH_THIRTY );
	if ( ones != 0 )
	    saydigit( ones );
	break;

	case 4:
	sayphrase( PH_FORTY );
	if ( ones != 0 )
	    saydigit( ones );
	break;

	case 5:
	sayphrase( PH_FIFTY );
	if ( ones != 0 )
	    saydigit( ones );
	break;

	default:
	(void) fprintf( stderr, "Shouldn't happen.\n" );
	exit( 1 );
	}
    }

void
saydigit( n )
int n;
    {
    switch ( n )
	{
	case 1:
	sayphrase( PH_ONE );
	break;

	case 2:
	sayphrase( PH_TWO );
	break;

	case 3:
	sayphrase( PH_THREE );
	break;

	case 4:
	sayphrase( PH_FOUR );
	break;

	case 5:
	sayphrase( PH_FIVE );
	break;

	case 6:
	sayphrase( PH_SIX );
	break;

	case 7:
	sayphrase( PH_SEVEN );
	break;

	case 8:
	sayphrase( PH_EIGHT );
	break;

	case 9:
	sayphrase( PH_NINE );
	break;

	default:
	(void) fprintf( stderr, "Shouldn't happen.\n" );
	exit( 1 );
	}
    }

void
sayphrase( phrase )
int phrase;
    {
    switch ( phrase )
	{
	case PH_ONE:
	sayfile( "1.au" );
	break;

	case PH_TWO:
	sayfile( "2.au" );
	break;

	case PH_THREE:
	sayfile( "3.au" );
	break;

	case PH_FOUR:
	sayfile( "4.au" );
	break;

	case PH_FIVE:
	sayfile( "5.au" );
	break;

	case PH_SIX:
	sayfile( "6.au" );
	break;

	case PH_SEVEN:
	sayfile( "7.au" );
	break;

	case PH_EIGHT:
	sayfile( "8.au" );
	break;

	case PH_NINE:
	sayfile( "9.au" );
	break;

	case PH_TEN:
	sayfile( "10.au" );
	break;

	case PH_ELEVEN:
	sayfile( "11.au" );
	break;

	case PH_TWELVE:
	sayfile( "12.au" );
	break;

	case PH_THIRTEEN:
	sayfile( "13.au" );
	break;

	case PH_FOURTEEN:
	sayfile( "14.au" );
	break;

	case PH_FIFTEEN:
	sayfile( "15.au" );
	break;

	case PH_SIXTEEN:
	sayfile( "16.au" );
	break;

	case PH_SEVENTEEN:
	sayfile( "17.au" );
	break;

	case PH_EIGHTEEN:
	sayfile( "18.au" );
	break;

	case PH_NINETEEN:
	sayfile( "19.au" );
	break;

	case PH_TWENTY:
	sayfile( "20.au" );
	break;

	case PH_THIRTY:
	sayfile( "30.au" );
	break;

	case PH_FORTY:
	sayfile( "40.au" );
	break;

	case PH_FIFTY:
	sayfile( "50.au" );
	break;

	case PH_THE_TIME_IS:
	sayfile( "the_time_is.au" );
	break;

	case PH_OCLOCK:
	sayfile( "oclock.au" );
	break;

	case PH_OH:
	sayfile( "oh.au" );
	break;

	case PH_EXACTLY:
	sayfile( "exactly.au" );
	break;

	case PH_AND:
	sayfile( "and.au" );
	break;

	case PH_SECOND:
	sayfile( "second.au" );
	break;

	case PH_SECONDS:
	sayfile( "seconds.au" );
	break;

	default:
	(void) fprintf( stderr, "Shouldn't happen.\n" );
	exit( 1 );
	}
    }

int audiofd = -1;

void
sayfile( filename )
char *filename;
    {
    int filefd;
    int r, w;
    unsigned char buf[1024];
    char pathname[200];

    if ( audiofd == -1 )
	{
	audiofd = open( "/dev/audio", O_WRONLY | O_NDELAY );
	if ( audiofd < 0 )
	    {
	    perror( "opening /dev/audio" );
	    exit( 1 );
	    }
	}

    (void) sprintf( pathname, "%s/%s", SOUND_DIR, filename );
    filefd = open( pathname, O_RDONLY );
    if ( filefd < 0 )
	{
	perror( "opening audio file" );
	exit( 1 );
	}

    for ( ; ; )
	{
	r = read( filefd, buf, sizeof(buf) );
	if ( r < 0 )
	    {
	    perror( "reading from audio file" );
	    exit( 1 );
	    }
	if ( r == 0 )
	    break;

	for ( ; ; )
	    {
	    w = write( audiofd, buf, r );
	    if ( w < 0 )
		{
		perror( "writing to audio device" );
		exit( 1 );
		}
	    if ( w != 0 )
		break;
	    usleep( 1000 );
	    }
	if ( w != r )
	    {
	    (void) fprintf( stderr, "read returned %d, write returned %d\n", r, w );
	    exit( 1 );
	    }
	}
    close( filefd );
    }

void
sayclose( )
    {
    if ( audiofd != -1 )
	close( audiofd );
    audiofd = -1;
    }
