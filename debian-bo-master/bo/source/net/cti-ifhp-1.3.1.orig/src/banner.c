/*==========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                               =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>                          =
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995                       =
 ==========================================================================*/


/*==========================================================================
  Version CTI-LPRng-1.0
  =========================================================================*/


/*
 * $Id: banner.c,v 1.6 1996/11/14 19:56:23 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


int xpos = 50;
int ypos = 100;
int incr;


/******************************************************************************
 *	PCL Cover Description strings
 */

static char margins[] = {
	"\033&l0u0Z"
};
/* light grey bar at 30 X 50 Y */
static char lightbar[] = { 
	"\033*c1800a100b45g2P"
} ;

/* dark grey bar at 30 X 560 Y */
static char darkbar[]  = { 
	"\033*c1800a100b25g2P"
} ;

static char fontchange[] = {
	"\033(8U\033(s1p%dv0s0b4148T" 
};
static char position[] = {
	"\033*p%dx%dY"  /* position to  (X Y) */
};

void moveto( int x, int y )
{
	plp_snprintf( sendline, sizeof(sendline)-1, position, x, y );
	Out_line( sendline );
}

void fontsize( int size )
{
	/* size is in points (72nds of inch ) */
	incr = (size*300*1.1)/72;
	plp_snprintf( sendline, sizeof(sendline)-1, fontchange, size );
	Out_line( sendline );
}

extern int show_ctrl;

void textline( char *line, int start, int end )
{
	if( start ){
		moveto( xpos, ypos );
	}
	show_ctrl = 1;
	plp_snprintf( sendline, sizeof(sendline)-1, "%s", line );
	show_ctrl = 0;
	Out_line( sendline );
	if( end ){
		ypos += incr;
	}
}


void pcl_banner()
{
	char *s;
	static char *dups;
	int i = 0;


	log(4,"pcl_banner: starting");
	if( dups ) free(dups);
	dups = 0;
	if( bantitle ) dups = strdup( bantitle );
	bantitle = dups;

	i = 0;
    query[i++]=UEL;
    query[i++]=PCLRESETSTR;
    query[i++]=UELPJL;
	query[i++]= CRLFSTR;
	query[i++]= margins;

	pr_query(STDOUT,i);

	moveto( xpos, ypos );
	Out_line( lightbar );
	ypos += 100;

	/* set font size */
	fontsize( 24 );
	ypos += incr;
	moveto( xpos, ypos );

	s = filename;
	if( s == 0 || *s == 0 ) s = job;
	if( s && *s ){
		textline( s, 1, 1 );
	}
	s = username;
	if( s == 0 || *s == 0 ) s = login;
	if( s && *s ){
		textline( "User: ", 1, 0 );
		textline( s, 0, 1 );
	}

	if( (s = host) ){
		textline( "Host: ", 1, 0 );
		textline( s, 0, 1 );
	}

	if( (s = printer) ){
		textline( "Printer: ", 1, 0 );
		textline( s, 0, 1 );
	}


	fontsize( 12 );
	if( (s = class) ){
		textline( "Class: ", 1, 0 );
		textline( s, 0, 1 );
	}
	textline( "Date: ", 1, 0 );
	textline( Time_str(), 0, 1 );
	
	moveto( xpos, ypos );
	Out_line( darkbar );

	fontsize( 18 );
	ypos += 100+2*incr;
	
	if ( bantitle ){
		if( (s=strchr(bantitle, '/')) ){
			*s++ = 0;
		}
		textline( bantitle, 1, 1 );
	}

	fontsize( 12 );
	for(bantitle = s; bantitle ; bantitle = s ){
		if( (s=strchr(bantitle, '/')) ){
			*s++ = 0;
		}
		textline( bantitle, 1, 1 );
	}
	i = 0;
	query[i++]=FFEED;
    query[i++]=UEL;
    query[i++]=PCLRESETSTR;
    query[i++]=UELPJL;

	pr_query(STDOUT,i);

	log(4,"pcl_banner: done");
}

/*
 * of filter - read input until a STOP string is hit
 *  - non whitespace lines are ignored
 */
static char stop[] = "\031\001";    /* sent to cause filter to suspend */

/*
 * suspend():  suspends the output filter, waits for a signal
 */

static void suspend( int pid )
{
    log(4,"FILTER suspending");
    if( pid > 0 ) kill( pid, SIGSTOP);
    kill(getpid(), SIGSTOP);
	if( monitpid > 0 ) kill( monitpid, SIGCONT);
    log(4,"FILTER awake");
}

/*
 * parse the banner input line, looking for entries like:
 * name: value
 */

struct value banlist[] = {
	{"class", &class, STRV},
	{"host", &host, STRV},
	{"job", &job, STRV},
	{"logname", &login, STRV},
	{"printer", &printer, STRV},
	{"user", &username, STRV},
};

int banlen = sizeof(banlist)/sizeof(banlist[0]);

/*
 * parse the banner input line (read from stdin) and get the 
 * banner title values.  This has the form:
 *   class:name key:value key:value ...
 * or
 *   key:value  key:value ...
 */

static void parse_input()
{
	char *s, *end, *key, *value;
	static char *sdup;

	if( (s = strchr( sendline, '\n' )) ) *s = 0;
	if( sdup ) free(sdup);
	sdup = 0;
	s = sdup = strdup( sendline );
	/* find class and user */
	while( isspace( *s ) ) ++s;
	log(1,"banner string '%s'", s);
	/* find the next keyword */
	if( (value = strchr(s, ':' )) ){
		*value++ = 0;
		if( setvalue( s, value, banlist, banlen ) ){
			class = s;
			username = value;
		}
		s = strpbrk( value, " \t" );
		*s++ = 0;
	} else {
		s = 0;
	}
	for( ; s && *s; s = end ){
		/* find the next keyword */
		while( isspace( *s ) ) ++s;
		if( (value = strchr(s, ':' )) == 0 ) break;
		for( key = value-1; key > s && *key && !isspace( *key ); --key );
		log(1,"banner string key '%s'", key);
		*value++ = 0;
		while( value && isspace(*value) ) *value++ = 0;
		if( (end = strchr(value, ':' )) ){
			log(1,"banner string value '%s' end '%s'", value, end);
			for( end = end-1; *end && !isspace( *end ); --end );
			*end++ = 0;
		}
		if(value == end ) --value;
		log(1,"banner key='%s' value='%s'", key, value);
		setvalue( key, value, banlist, banlen );
	}
}

void resync()
{
	/* we will have to resynchronize */
	if( get_status ){
		/* we do this with the monitor asleep */
		if( monitpid > 0 ) kill( monitpid, SIGSTOP);
		/* header_info(); */
		if(pr_synch( STDOUT, 60 ) == 0 ){
			log(1,"do_of_stream: resynch with printer failed");
		}
		/* now we wake the monitor */
		if( monitpid > 0 ) kill( monitpid, SIGCONT);
	}
}

void do_of_stream(fp, sockfd)
FILE    *fp;
int    sockfd;
{
	int c;
	int state = 0;
	int linecount = 0;
	int err, i;
	int first_line = 0;

	log(4,"do_of_stream: starting transfer" );
	while( (c = getc(fp)) != EOF ){
		sendline[linecount++] = c;
		if( (stop || state) && c == stop[state] ){
			++state;
			if( stop[state] == 0 ){
				state = 0;
				i = linecount-strlen(stop);
				if( i > 0 ){
					log(2,"do_of_stream: writing %d (first = 0x%02x)", i,
						((unsigned char *)(sendline))[0]);
					if( writecn( sockfd, sendline, i) != i ){
						err = errno;
						log(4,"do_of_stream: write failed - %s",
							Errormsg(err) );
						fexit( FILTABORT );
					}
				}
				linecount = 0;
				suspend( monitpid );
				/* resync(); */
			}
		} else {
			state = 0;
		}
		if( c == '\n' || linecount >= sizeof(sendline)-1 ){
			log(2,"do_of_stream: writing %d", linecount);
			log(4,"do_of_stream: line '%s', bnr '%d'",
				sendline, bnr );
			sendline[linecount] = 0;
			if( bnr && first_line == 0 ){
				for( i = 0; i < linecount && isspace( sendline[i] ); ++i );
				if( i < linecount ){
					/* we parse the input line */
					log(4,"do_of_stream: banner line '%s'", sendline );
					parse_input( sendline );
					pcl_banner();
				}
			} else {
				if( writecn( sockfd, sendline, linecount ) != linecount ){
					err = errno;
					log(4,"do_of_stream: writecn failed - %s",
						Errormsg(err) );
					fexit( FILTABORT );
				}
			}
			first_line = 1;
			linecount = 0;
		}
	}
	log(2,"do_of_stream: EOF");
	if( ferror( stdin ) ){
		logerr(1,"do_of_stream: read error on stdin");
	}
	if( linecount > 0 ){
		if( writecn( sockfd, sendline, linecount) != linecount ){
			err = errno;
			log(4,"do_of_stream: write failed - %s",
				Errormsg(err) );
			fexit( FILTABORT );
		}
		linecount = 0;
	}

	log(4, "filter: finished transfer" );
	resync();
}
