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
 * $Id: check_code.c,v 1.5 1996/11/14 19:56:24 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"

/*
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Check out the HP PCL 5 reference manual set; it includes the PJL manual
which contains a complete (as far as I know) listing of status/error codes.
In case you don't have the manual handy, here's some code I used to handle
the status codes. The code should be fairly obvious. Free feel to use it.

Eelco van Asperen            /                 Erasmus University Rotterdam
----------------------------/   Informatievoorziening en Automatisering FEW
vanasperen@facb.few.eur.nl / PObox 1738, 3000 DR Rotterdam, The Netherlands

The header file, pjlcode.h:

-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
#ifndef __PJLCODE_H
#define	__PJLCODE_H

#define	PJLC_GROUP(c)	((c)/1000)
#define	PJLC_CODE(c)	((c)%1000)

#define	PJLC_TRAY(c)	(PJLC_CODE(c) / 100)
#define	PJLC_MEDIA(c)	(PJLC_CODE(c) % 100)

#define	PJLG_INFO	10
#define	PJLG_BGPAPER	11
#define	PJLG_PARSEERR	20
#define	PJLG_PARSEWARN	25
#define	PJLG_SEMANTERR	27
#define	PJLG_AUTOCONT	30
#define	PJLG_POSSOPER	35
#define	PJLG_OPER	40
#define	PJLG_FGPAPER	41

typedef struct {
	int	code;
	const char *msg;
} code_t;

extern	code_t	 groupcode[];
extern	const int num_groupcode;

extern	const char *lookup(code_t *tab, int ntab, int target);
extern	const char *pjl_message(int code);

#endif /* __PJLCODE_H */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

And the actual code, pjlcode.c:

  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

code_t	 groupcode[] = {
	{ PJLG_INFO,	"status" },
	{ PJLG_BGPAPER, "background paper mount" },
	{ PJLG_PARSEERR, "PJL parser error" },
	{ PJLG_PARSEWARN, "PJL parser warning" },
	{ PJLG_SEMANTERR, "PJL semantic error" },
	{ PJLG_AUTOCONT, "auto-continuable condition" },
	{ PJLG_POSSOPER, "possible operator intervention condition" },
	{ PJLG_OPER,	"operator intervention needed" },
	{ PJLG_FGPAPER, "waiting for paper" /* "Foreground Paper Mount" */ }
};

const int num_groupcode = (sizeof(groupcode)/sizeof(groupcode[0]));

static	code_t	pjlmsg[] = {
	/* Informational Message */
	{ 10000, "powersave mode" },
	{ 10001, "ready" },
	{ 10002, "not ready" },
	{ 10003, "warming up" },
	{ 10004, "self test" },
	{ 10005, "reset" },
	{ 10006, "toner low" },
	{ 10010, "status buffer overflow" },
	{ 10011, "auxiliary IO initialisation" },
	{ 10013, "self test" },
	{ 10014, "printing test" },
	{ 10015, "typeface list" },
	{ 10016, "engine test" },
	{ 10017, "demo page" },
	{ 10018, "menu reset" },
	{ 10019, "reset active IO" },
	{ 10020, "reset all IO" },
	{ 10022, "config page" },
	{ 10023, "processing job" },
	{ 10027, "remove paper from printer" },
	{ 10029, "formfeeding" },

	/* 11xxx (Background Paper Loading Messages */
	{ 11304, "tray 3 empty (tray 3=LC)" },

	/* Background paper tray status */
	{ 12201, "tray 2 open" },
	{ 12202, "tray 2 lifting" },
	{ 12301, "tray 3 open" },
	{ 12302, "tray 3 lifting" },


	/* PJL Parser Error */
	{ 20001, "generic syntax error" },
	{ 20002, "unsupported command" },
	{ 20004, "unsupported personality/system" },
	{ 20006, "illegal character or line terminated by the UEL command" },
	{ 20007, "whitespace or linefeed missing after closing quotes" },
	{ 20008, "invalid character in an alphanumeric value" },
	{ 20009, "invalid character in a numeric value" },
	{ 20010, "invalid character at the start of a string,"
			"alphanumeric value or numeric value" },
	{ 20011, "string missing closing double-quote character" },
	{ 20012, "numeric value starts with a decimal point" },
	{ 20013, "numeric value does not contain any digits" },
	{ 20014, "no alphanumeric value after command modifier" },
	{ 20015, "option name and equal sign encountered"
			" but value field is missing" },
	{ 20016, "more than one command modifier" },
	{ 20017, "command modifier encountered after an option" },
	{ 20018, "command not an alphanumeric value" },
	{ 20019, "numeric value encountered when"
			" an alphanumeric value expected" },
	{ 20020, "string encountered when an alphanumeric value expected" },
	{ 20021, "unsupported command modifier" },
	{ 20022, "command modifier missing" },
	{ 20023, "option missing" },
	{ 20024, "extra data received after option name" },
	{ 20025, "two decimal points in a numeric value" },

	/* PJL Parser Warning */
	{ 25001, "generic warning" },
	{ 25002, "PJL prefix missing" },
	{ 25003, "alphanumeric value too long" },
	{ 25004, "string too long" },
	{ 25005, "numeric value too long" },
	{ 25006, "unsupported option name" },
	{ 25007, "option name requires a value which is missing" },
	{ 25008, "option name requires value of a different type" },
	{ 25009, "option name received with a value,"
			" but this option does not support values" },
	{ 25010, "same option name received more than once" },
	{ 25011, "ignored option name due to value underflow or overflow" },
	{ 25012, "value truncated or rounded" },
	{ 25013, "value out of range; used closest supported limit" },
	{ 25014, "value out of range; ignored" },
	{ 25016, "option name received with an alphanumeric value,"
			" but this value is not supported" },
	{ 25017, "string empty, option ignored" },

	/* PJL Semantic Errors; */
	{ 27001, "generic sematic error" },
	{ 27002, "EOJ encountered without a previous matching JOB command" },
	{ 27004, "value of a read-only variable can not be changed" },

	/* Auto-Continuable Condition */
	{ 30010, "status buffer overflow" },
	{ 30016, "memory overflow" },
	{ 30017, "print overrun" },
	{ 30018, "communication error" },
	{ 30027, "IO buffer overflow" },
	{ 30034, "paper feed error" },
	{ 30035, "NVRAM error" },
	{ 30036, "NVRAM full" },

	/* Possible Operator Intervention Condition */
	{ 35029, "W1 image adapt" },
	{ 35031, "W2 invalid personality" },
	{ 35037, "W3 job aborted" },
	{ 35039, "W9 job 600/LTR" },
	{ 35040, "W0 job 600/A4" },
	{ 35041, "W8 job 600/OFF" },
	{ 35042, "W7 job 300/LGL" },
	{ 35043, "W5 job 300/LTR" },
	{ 35044, "W6 job 300/A4" },
	{ 35045, "W4 job 300/OFF" },
	{ 35078, "powersave mode" },

	/* Operator Intervention Needed */
	{ 40010, "no EP cartridge" },
	{ 40021, "printer open" },
	{ 40022, "paper jam" },
	{ 40024, "FE cartridge" },
	{ 40026, "PC install" },
	{ 40038, "toner low" },
	{ 40046, "insert cartridge" },
	{ 40047, "remove cartridge" },
	{ 40048, "[PJL OpMsg]" },
	{ 40049, "[PJL StMsg]" },
	{ 40050, "service error 50" },
	{ 40051, "temporary error 51" },
	{ 40052, "temporary error 52" },
	{ 40053, "memory error" },
	{ 40054, "54 error" },
	{ 40055, "temporary error 55" },
	{ 40056, "56 Error" },
	{ 40057, "service error 57" },
	{ 40058, "service error 58" },
	{ 40059, "59 error" },
	{ 40061, "RAM parity error" },
	{ 40062, "error during memory check" },
	{ 40063, "internal RAM error" },
	{ 40064, "internal service error 64" },
	{ 40065, "65 error" },
	{ 40067, "67 error" },
	{ 40069, "70 error" },
	{ 40070, "71 error" },
	{ 40071, "72 error" },
	{ 40079, "offline" },

	/* 41xxx (Background Paper Loading) handled by pjl_message() */

	{ 42202, "13.2 paper jam, remove 2 pages" },
};

static	const int num_pjlmsg = (sizeof(pjlmsg)/sizeof(pjlmsg[0]));

static	code_t	traytab[] = {
	{ 0, "MP" },
	{ 1, "Manual Feed" },
	{ 2, "PC/Upper/Tray2" },
	{ 3, "LC/Lower/Tray3" },
	{ 4, "EE" },
	{ 5, "HCI" }
};

static	const int num_traytab = (sizeof(traytab)/sizeof(traytab[0]));

static	code_t	mediatab[] = {
	{ 0, "Unknown paper" },
	{ 1, "Unknown envelope" },
	{ 2, "Letter paper" },
	{ 3, "Legal paper" },
	{ 4, "A4 paper" },
	{ 5, "Exec paper" },
	{ 6, "Ledger paper" },
	{ 7, "A3 paper" },
	{ 8, "COM10 envelope" },
	{ 9, "Monarch envelope" },
	{ 10, "C5 envelope" },
	{ 11, "DL envelope" },
	{ 12, "B4 paper" },
	{ 13, "B5 paper" },
	{ 14, "B5 envelope" },
	{ 15, "Custom media" },
	{ 16, "Hagaki" },
	{ 17, "Oufuku-Hagaki" },
};

static	const int num_mediatab = (sizeof(mediatab)/sizeof(mediatab[0]));

const char *
lookup(code_t *tab, int ntab, int target)
{
	int	max, min, mid, i;

	max = ntab - 1;
	min = 0;
	log(5,"lookup_code: code %d, max %d, min %d",target, max, min);
	while( min <= max ){
		mid = (max + min)/2;
		i = tab[mid].code - target;
		/*log(9,"lookup_code: mid %d, max %d, min %d, found %d, dir %d",
			mid, max, min,tab[mid].code,i); */
		if(i == 0 ){
			return tab[mid].msg;
		} else if( i > 0 ){
			max = mid-1;
		} else {
			min = mid+1;
		}
	}
	return("UNKNOWN");
}

const char *
pjl_message(int code)
{
	if (PJLC_GROUP(code) == PJLG_BGPAPER
	 || PJLC_GROUP(code) == PJLG_FGPAPER) {
		/* Background/Foreground Paper Loading */
		static char buf[100];
		const char *tray, *media;
		
		tray = lookup(traytab, num_traytab, PJLC_TRAY(code)),
		media = lookup(mediatab, num_mediatab, PJLC_MEDIA(code));
				
		plp_snprintf(buf, sizeof(buf), "Load `%s' into the `%s' tray",
				media ? media : "(unknown media)",
				tray ? tray : "(unknown)");
		return buf;
	}

	return lookup(pjlmsg, num_pjlmsg, code);
}

int check_code( cd )
int cd;
{
	char msg[128];
	const char *s;
	int logit = 1;
	int status = 0;


	s = pjl_message( cd );
	log(5,"check_code: status %d - '%s'",cd, s);
	if( quiet ) switch( cd ){
	case 10000:
	case 10001:
	case 10003:
	case 10023:
	case 35078:
		logit = 0;
		break;
	}
	if( logit ){
		plp_snprintf( msg, sizeof(msg)-1, "ALERT - printer status code %d - %s",
			cd, s );
		log(0, "%s", msg);
	}
	return(status);
}
