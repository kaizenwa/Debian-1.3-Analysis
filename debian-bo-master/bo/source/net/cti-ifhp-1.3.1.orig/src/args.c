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
 * $Id: args.c,v 1.13 1996/11/14 19:56:23 papowell Exp papowell $
 */

#include "portable.h"
#include "hp4.h"
#include "common.h"


struct parm Parmlist[] = {
{'C', &class, STRV },
{'F', &format, STRV },
{'H', &fqdnhost, STRV },
{'J', &job, STRV },
{'L', &username, STRV },
{'N', &filename, STRV },
{'P', &printer, STRV },
{'R', &accntname, STRV },
{'S', &pr_commnt, STRV },
{'X', &seq_number, STRV },
{'Z', &zopts, STRV },
{'c', (char **)&literal, FLGV },
{'h', &host, STRV },
{'i', (char **)&indent, INTV },
{'k', &controlfile, STRV },
{'l', (char **)&length, INTV },
{'n', &login, STRV },
{'s', &statusfile, STRV },
{'v', (char **)&version, FLGV },
{'w', (char **)&width, INTV },
{'x', (char **)&xwidth, INTV },
{'y', (char **)&ylength, INTV } };

int Parmlen = sizeof(Parmlist)/sizeof(Parmlist[0]);

struct parm *findvar( int flag, struct parm *parmlist, int parmlen );
void setvar(  struct parm *parm, char *arg );

static struct value Valuelist[] = {
	{ "accounting", &Accounting_script, STRV },
	{ "autodetect", (char **)&autodetect, FLGV },
	{ "banner", (char **)&bnr, FLGV },
	{ "cartridge", (char **)&cartridge, FLGV },
	{ "debug", (char **)&debug, INTV },
	{ "defaultfont", &default_font, STRV },
	{ "dev", &device, STRV },
	{ "logall", (char **)&logall, FLGV },
	{ "model", &model_name, STRV },
	{ "pagecount", (char **)&pagecount, FLGV },
	{ "plp", (char **)&plp_compat, FLGV },
	{ "quiet", (char **)&quiet, FLGV },
	{ "resourcesave", (char **)&resourcesave, FLGV },
	{ "retries", (char **)&retries, INTV },
	{ "sleep", (char **)&wait_time, INTV },
	{ "status", (char **)&get_status, FLGV },
	{ "stty", &stty_args, STRV },
	{ "summary", &summaryfile, STRV },
	{ "title", &bantitle, STRV },
	{ "trace", (char **)&trace, FLGV },
	{ "wrap", (char **)&wrap, FLGV },
};
int Valuelen = sizeof(Valuelist)/sizeof(Valuelist[0]);

extern int getopt(), optind;
extern char *optarg;

void getargs(argc, argv)
	int argc;
	char **argv;
{
	int i;		/* argument index */
	int flag;	/* flag */
	char *arg;	/* argument */
	struct parm *parm;

	name = argv[0];

	for( i = 1; i < argc; ++i ){
		arg = argv[i];
		if( *arg++ != '-' ) break;
		flag = *arg++;
		if( flag == 0 ) continue;
		parm = findvar( flag, Parmlist, Parmlen );
		/* not found, assume -X argument format */
		if( *arg == 0 && (parm == 0 || parm->kind != FLGV) ){
			/* skip to next option */
			arg = argv[++i];
		}
		if( islower(flag) ){
			switch( flag ){
			case 'q': break;
			case 'p': break;
			case 't': break;
			case 'b': break;
			case 'c': break;
			default: Loweropts[flag-'a'] = arg; break;
			}
		} else if( isupper(flag) ){
			switch( flag ){
			case 'T': break;
			default: Upperopts[flag-'A'] = arg; break;
			}
		}
		if( parm ){
			setvar( parm, arg );
		} else if( flag == 'T' ){
			/*
			 * -T[dev=device][,model=(IV|II|III|IIIsi)][,CART,STTY=str]
			 */
			char *end, *value;
			for( ; arg && *arg; (arg = end) ){
				if( (end = strchr( arg, ',' )) ){
					*end++ = 0;
				}
				while( isspace(*arg) ) ++arg;
				if( (value = strchr( arg, '=' )) ){
					*value++ = 0;
				}
				if( setvalue( arg, value, Valuelist, Valuelen ) ){
					int j;
					fprintf( stderr, "Invalid -T flag '%s', valid flags are:", value );
					for( j = 0; j < Valuelen; ++j ){
						char *s;
						if( j % 4 ){
							fprintf( stderr, ", " );
						} else {
							fprintf( stderr, "\n   " );
						}
						switch( Valuelist[j].kind ){
							case STRV: s = "str"; break;
							case INTV: s = "num"; break;
							case FLGV: s = "[on|off]"; break;
							default: s = "???"; break;
						}
						fprintf( stderr, "%s=%s", Valuelist[j].flag, s );
					}
					fprintf( stderr, "\n" );
				}
			}
		}
	}
	if( i < argc ) accntfile = argv[i];
	newstatus();
	if(debug > 4){
		log(5,"lower case options:");
		for( i = 0; i < 26; ++i ){
			if( Loweropts[i] ) log(5,"opt[%c] = '%s'",i+'a',Loweropts[i] );
		}
		log(5,"upper case options:");
		for( i = 0; i < 26; ++i ){
			if( Upperopts[i] ) log(5,"opt[%c] = '%s'",i+'A',Upperopts[i] );
		}
	}
	log(5,"literal %d", literal );
}

/*
 * setvar(  struct parm *parm, char *arg )
 * 1. if STRV, then set 
 * 2. if INTV, then convert and set
 * 3. if FLGV, then set to 1
 */

void setvar( struct parm *parm, char *arg )
{

	switch( parm->kind ){
	case STRV: *parm->var = arg; break;
	case INTV: *(int *)parm->var = atoi(arg); break;
	case FLGV: *(int *)parm->var = 
		(arg == 0 || *arg == 0 || !strcasecmp( arg, "yes" )
			|| !strcasecmp( arg, "on" ));
		break;
	}
}

struct parm *findvar( int flag, struct parm *parmlist, int parmlen  )
{
	int u, l, i, c;	/* upper, lower, i */

	l = 0; u = parmlen;
	while( l <= u ){
		i = (u+l)/2;
		c = flag - parmlist[i].flag;
		if( 0 == c ){
			log(4, "found parm %c, %d", flag, i );
			return( &parmlist[i] );
		} else if( c < 0 ){
			 log(6, "down parm %c, %d", flag, i );
			u = i - 1 ;
		} else {
			 log(6, "up parm %c, %d", flag, i );
			l = i + 1 ;
		}
	}
	logerr(5,"Invalid option -%c", flag );
	return(0);
}

int setvalue( flag, arg, valuelist, valuelen )
	char *flag;
	char *arg;
	struct value *valuelist;
	int valuelen;
{
	int u, l, i, c;	/* upper, lower, i */

	l = 0; u = valuelen-1;
	while( l <= u ){
		i = (u+l)/2;
		c = strcasecmp( flag,valuelist[i].flag);
		if( 0 == c ){
			log(4, "found option %s, [%d], arg '%s'", flag, i, arg );
			switch( valuelist[i].kind ){
			case STRV: *valuelist[i].var = arg; break;
			case INTV: *(int *)valuelist[i].var = atoi(arg); break;
			case FLGV: *(int *)valuelist[i].var =
				(arg == 0 || !strcasecmp( arg, "yes")
					|| !strcasecmp( arg, "on" ));
				break;
			}
			return(0);
		} else if( c < 0 ){
			log(6, "down parm %s, %d", flag, i );
			u = i - 1 ;
		} else {
			log(6, "up parm %s, %d", flag, i );
			l = i + 1 ;
		}
	}
	return(1);
}
