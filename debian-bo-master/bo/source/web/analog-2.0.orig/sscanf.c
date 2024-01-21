/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** sscanf.c; functions to replace sscanf(), which is far too slow, in
     certain specific cases ***/

#include "analhea2.h"

/*** Now the scanning routines ***/

int sscanf_date(char *inputline, int *date, int *monthno, int *year, int *hr,
		int *min)
{    /* scanning date from common/agent/referrer log */

  register char *cin = inputline;
  char *cout;
  char month[4];
  int i;

  if (!isdigit(*cin))
    return(0);
  else
    *date = 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(0);
  else
    *date += (*cin - '0');

  /* read in month */
  cin++;
  if (*cin != '/')
    return(1);
  cin++;
  cout = month;
  for (i = 0; i < 3 && *cin != '\0'; i++) {
    *cout = *cin;
    cout++;
    cin++;
  }
  if (*cin == '\0')
    return(1);
  *cout = '\0';
  if ((*monthno = strtomonth(month)) == ERR)
    return(1);

  /* read in year */
  if (*cin != '/')
    return(2);
  cin++;
  if (!isdigit(*cin))
    return(2);
  else
    *year = 1000 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(2);
  else
    *year += 100 * (*cin - '0');
  cin++;
  if (!isdigit(*cin)) {
    if (*cin != ':')
      return(2);
    else {     /* allow two digit years for Spyglass server */
      *year /= 100;
      *year += 1900;
      if (*year < 1970)
	*year += 100;
    }
  }
  else {
    *year += 10 * (*cin - '0');
    cin++;
    if (!isdigit(*cin))
      return(2);
    else
      *year += (*cin - '0');
    cin++;
    if (*cin != ':')
      return(3);
  }

  /* read in hour */
  cin++;
  if (!isdigit(*cin))
    return(3);
  else
    *hr = 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(3);
  else
    *hr += (*cin - '0');

  /* read in minute */
  cin++;
  if (*cin != ':')
    return(4);
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *min = 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *min += (*cin - '0');

  /* don't read in second, but check it for correct form */
  cin++;
  if (*cin != ':')
    return(4);
  cin++;
  if (!isdigit(*cin))
    return(4);
  cin++;
  if (!isdigit(*cin))
    return(4);

  return(5);
}

int sscanf_olddate(char *inputline, int *date, int *monthno, int *year,
		   int *hr, int *min)
{    /* the same thing for NCSA old-style and error logs */
  register char *cin = inputline;
  char *cout;
  char month[4];
  int i;

  /* ignore day of week, so scan until next ' ' */
  for (cin++; *cin != ' ' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(0);

  /* read in month */
  cin++;
  cout = month;
  for (i = 0; i < 3 && *cin != '\0'; i++) {
    *cout = *cin;
    cout++;
    cin++;
  }
  if (*cin == '\0')
    return(0);
  *cout = '\0';
  if ((*monthno = strtomonth(month)) == ERR)
    return(1);

  /* read in date */
  if (*cin != ' ')
    return(1);
  cin++;
  if (!isdigit(*cin) && *cin != ' ')
    return(1);
  else if (*cin != ' ')
    *date = 10 * (*cin - '0');
  else
    *date = 0;
  cin++;
  if (!isdigit(*cin))
    return(1);
  else
    *date += (*cin - '0');

  /* read in hour */
  cin++;
  if (*cin != ' ')
    return(2);
  cin++;
  if (!isdigit(*cin))
    return(2);
  else
    *hr = 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(2);
  else
    *hr += (*cin - '0');

  /* read in minute */
  cin++;
  if (*cin != ':')
    return(3);
  cin++;
  if (!isdigit(*cin))
    return(3);
  else
    *min = 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(3);
  else
    *min += (*cin - '0');
  
  /* ignore second (but check format) */
  cin++;
  if (*cin != ':')
    return(4);
  cin++;
  if (!isdigit(*cin))
    return(4);
  cin++;
  if (!isdigit(*cin))
    return(4);
  cin++;
  if (*cin != ' ')
    return(4);

  /* read year */
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *year = 1000 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *year += 100 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *year += 10 * (*cin - '0');
  cin++;
  if (!isdigit(*cin))
    return(4);
  else
    *year += (*cin - '0');
  return(5);
}

int sscanf_common(char *inputline, char hostn[MAXSTRINGLENGTH], int *date,
		  int *monthno, int *year, int *hr, int *min,
		  char filename[MAXSTRINGLENGTH],
		  char referrer[MAXSTRINGLENGTH], char agent[MAXSTRINGLENGTH],
		  int *code, char bytestr[16], size_t preflength)
{     /* scanning 'common' format logfile entries */
  extern flag bq, Bq, fq, case_insensitive;
  extern struct include *noexpandhead, *refexpandhead;

  register char *cin = inputline;      /* the character we are reading */
  register char *cout;                 /* where we are putting it */
  int i;

  /* read in hostname */
  i = 0;
  for (cout = hostn; *cin != ' ' && *cin != '\0' && i < MAXSTRINGLENGTH - 1;
       cin++) { 
    *cout = *cin;
    cout++;
    i++;
  }
  if (*cin != ' ')
    return(0);
  *cout = '\0';

  /* scan until next '[' */
  for (cin++; *cin != '[' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(1);

  /* read in date */
  cin++;
  if (sscanf_date(cin, date, monthno, year, hr, min) < 5)
    return(1);
  else
    cin += 20;
  
  /* ignore timezone; so scan to next '"' */
  for ( ; *cin != '"' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(6);

  /* ignore method; so read to next ' ' */
  for (cin++; *cin != ' ' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(6);

  /* read in filename */
  cin++;
  i = 0;
  for (cout = filename; *cin != ' ' && *cin != '\0' && *cin != '"' &&
       *cin != '?' && i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';
  if (*cin == '?') {
    if (case_insensitive)
      strtolower(filename);   /* if no question mark, strtolower later */
    if (!included(filename, UNSET, noexpandhead)) {
      for ( ; *cin != ' ' && *cin != '\0' && *cin != '"' &&  /* read in args */
	   i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
	*cout = *cin;
	cout++;
	i++;
      }
      *cout = '\0';
    }
  }
  if (*cin != ' ' && *cin != '"' && *cin != '?')
    return(6);

  /* scan to next " */
  for ( ; *cin != '"' && *cin != '\0' ; cin++)
    ;
  if (*cin == '\0')
    return(7);

  /* read in return code; always 3 digits, or a - (successes; call them 299) */
  cin++;
  if (*cin != ' ')
    return(7);
  cin++;
  if (!isdigit(*cin))
    if (*cin == '-' && *(cin + 1) == ' ')
      *code = 299;
    else
      return(7);
  else {
    *code = 100 * (*cin - '0');
    cin++;
    if (!isdigit(*cin))
      return(7);
    else
      *code += 10 * (*cin - '0');
    cin++;
    if (!isdigit(*cin))
      return(7);
    else
      *code += (*cin - '0');
  }

  /* read in bytestr */
  cin++;
  if (*cin != ' ')
    return (8);
  cin++;
  i = 0;
  for (cout = bytestr; *cin != ' ' && *cin != '\n' && *cin != '\0' && i < 16;
       cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';

  /* Finally, try and read in referrer and agent of NCSA combined format */
  if (*cin != ' ' || (!fq && !bq && !Bq))
    return(9);
  if (*(++cin) != '"')
    return(9);
  cin++;

  i = 0;
  for (cout = referrer; *cin != '\0' && *cin != '"' && *cin != '?' &&
       i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';                                    /* read in args */
  if (*cin == '?' && included(referrer, UNSET, refexpandhead)) {
    for ( ; *cin != '"' && *cin != '\0' && i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
    *cout = '\0';
  }
  if (*cin == '?') {
    while (*cin != '"' && *cin != '\0')
      cin++;
  }

  if (*cin != '"')
    return(9);
  if (*(++cin) != ' ')
    return(10);
  if (*(++cin) != '"')
    return(10);
  i = 0;
  cin++;
  for (cout = agent; *cin != '\0' && *cin != '"' && i < MAXSTRINGLENGTH - 1;
       cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';
  if (*cin != '"')
    return(10);
  else
    return(11);

}

int sscanf_ncsaold(char *inputline, char hostn[MAXSTRINGLENGTH], int *monthno,
		   int *date, int *hr, int *min, int *year,
		   char filename[MAXSTRINGLENGTH], size_t preflength)
{    /* scanning NCSA old-style logfile entries */
  extern struct include *noexpandhead;
  extern flag case_insensitive;

  register char *cin = inputline;   /* the character we are reading */
  register char *cout;              /* where we are putting it */
  int i;

  /* read in hostname */
  i = 0;
  for (cout = hostn; *cin != ' ' && *cin != '\0' && i < MAXSTRINGLENGTH - 1;
       cin++) { 
    *cout = *cin;
    cout++;
    i++;
  }
  if (*cin != ' ')
    return(0);
  *cout = '\0';

  /* scan until next '[' */
  for (cin++; *cin != '[' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(1);

  /* read in date */
  cin++;
  if (sscanf_olddate(cin, date, monthno, year, hr, min) < 5)
    return(1);
  else
    cin += 24;

  /* ignore method, so skip to second space */
  for ( ; *cin != ' ' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(6);

  for (cin++; *cin != ' ' && *cin != '\0'; cin++)
    ;
  if (*cin == '\0')
    return(6);

  /* finally, read in the filename */
  cin++;
  i = 0;
  for (cout = filename; *cin != ' ' && *cin != '\n' && *cin != '?' &&
       *cin != '\0' && i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
      *cout = *cin;
      cout++;
    i++;
  }
  *cout = '\0';
  if (*cin == '?') {
    if (case_insensitive)
      strtolower(filename);   /* if no question mark, strtolower later */
    if (!included(filename, UNSET, noexpandhead)) {
      for ( ; *cin != ' ' && *cin != '\0' && *cin != '"' &&  /* read in args */
	   i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
	*cout = *cin;
	cout++;
	i++;
      }
      *cout = '\0';
    }
  }
  return (7);

}

int sscanf_domains(char *inputline, char string1[MAXSTRINGLENGTH],
		   char string2[MAXSTRINGLENGTH])
{   /* scanning the domains file */
  register char *cin = inputline;
  register char *cout;
  int i;

  /* run past any white space */
  while (*cin == ' ' || *cin == '\t')
    cin++;

  /* if no strings on this line, return 0 */
  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(0);

  /* otherwise fill up string 1; coerce domains to lower case */
  i = 0;
  for (cout = string1; *cin != ' ' && *cin != '\t' && *cin != '#' &&
       *cin != '\0' && *cin != '\n' && i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = tolower(*cin);
    cout++;
    i++;
  }

  /* is that the end of the line (maybe after some white space)? */
  if (*cin == '#' || *cin == '\0' || *cin == '\n' || i == MAXSTRINGLENGTH - 1)
    return(1);

  *cout = '\0';
  cin++;

  while (*cin == ' ' || *cin == '\t')
    cin++;

  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(1);

  /* otherwise fill up string 2 */
  for (cout = string2; *cin != '#' && *cin != '\n' && *cin != '\0' &&
       i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }

  *cout = '\0';
  return(2);

}

int sscanf_config(char *inputline, char string1[MAXSTRINGLENGTH],
		  char string2[MAXSTRINGLENGTH],
		  char string3[MAXSTRINGLENGTH])
{   /* scanning the config file */
  register char *cin = inputline;
  register char *cout;
  int i;

  /* run past any white space */
  while (*cin == ' ' || *cin == '\t')
    cin++;

  /* if no strings on this line, return 0 */
  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(0);

  /* otherwise fill up string 1; convert arguments to upper case */
  i = 0;
  for (cout = string1; *cin != ' ' && *cin != '\t' && *cin != '#' &&
       *cin != '\0' && *cin != '\n' && i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }

  *cout = '\0';

  /* is that the end of the line (maybe after some white space)? */
  if (*cin == '#' || *cin == '\0' || *cin == '\n' || i == MAXSTRINGLENGTH - 1)
    return(1);

  cin++;

  while (*cin == ' ' || *cin == '\t')
    cin++;

  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(1);

  /* if string 2 starts with a quote mark, fill up until the next quote
     mark. Otherwise, just fill until the next space */

  if (*cin == '\'') {
    cin++;
    for (cout = string2; *cin != '\n' && *cin != '\0' && *cin != '\'' &&
	 i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }
  else if (*cin == '"') {
    cin++;
    for (cout = string2; *cin != '\n' && *cin != '\0' && *cin != '"' &&
	 i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }
  else {
    for (cout = string2; *cin != '#' && *cin != '\n' && *cin != '\0' &&
	 *cin != ' ' && *cin != '\t' && i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }

  *cout = '\0';

  /* is that the end of the line (maybe after some white space)? */
  if (*cin == '#' || *cin == '\0' || *cin == '\n' || i == MAXSTRINGLENGTH - 1)
    return(2);

  cin++;

  while (*cin == ' ' || *cin == '\t')
    cin++;

  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(2);

  /* otherwise fill up string 3 */
  if (*cin == '\'') {
    cin++;
    for (cout = string3; *cin != '\n' && *cin != '\0' && *cin != '\'' &&
	 i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }
  else if (*cin == '"') {
    cin++;
    for (cout = string3; *cin != '\n' && *cin != '\0' && *cin != '"' &&
	 i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }
  else {
    for (cout = string3; *cin != '#' && *cin != '\n' && *cin != '\0' &&
	 *cin != ' ' && *cin != '\t' && i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
  }

  *cout = '\0';

  /* is that the end of the line (maybe after some white space)? */
  if (*cin == '#' || *cin == '\0' || *cin == '\n' || i == MAXSTRINGLENGTH - 1)
    return(3);

  cin++;

  while (*cin == ' ' || *cin == '\t')
    cin++;

  if (*cin == '#' || *cin == '\n' || *cin == '\0')
    return(3);

  return(4);   /* we don't ever want to read a fourth string; just know if
		  there is one for error checking */

}


int sscanf_referrer(char *inputline, int *date, int *monthno, int *year,
		   int *hr, int *min, char from[MAXSTRINGLENGTH],
		   char to[MAXSTRINGLENGTH])
{   /* scanning the referrer log */
    /* The format is "[date] from -> to". The [date] is optional. */
  extern struct include *refexpandhead, *noexpandhead;

  register char *cin = inputline;
  register char *cout;
  int i;

  /* scan the date */
  if (*cin == '[') {
    cin++;
    if (sscanf_date(cin, date, monthno, year, hr, min) < 5)
      return(0);
    else
      cin += 20;
    if (*cin != ']')
      return(5);
    if (*(++cin) != ' ')
      return(5);
    cin++;
  }
  else
    *date = 0;   /* as marker */

  /* now fill up the from string */

  i = 0;
  for (cout = from; *cin != ' ' && *cin != '\0' && *cin != '?' &&
       i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';                                    /* read in args */
  if (*cin == '?' && included(from, UNSET, refexpandhead)) {
    for ( ; *cin != ' ' && *cin != '\0' && i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
    *cout = '\0';
  }

  /* check at this point that the line syntax is ok */

  if (*cin == '?') {
    while (*cin != ' ' && *cin != '\0')
      cin++;
  }

  if (*cin != ' ')
    return(5);
  cin++;
  if (*cin != '-')
    return(6);
  cin++;
  if (*cin != '>')
    return(6);
  cin++;
  if (*cin != ' ')
    return(6);
  cin++;

  /* and the to string */

  i = 0;
  for (cout = to; *cin != ' ' && *cin != '\0' && *cin != '\n'
       && i < MAXSTRINGLENGTH - 1; cin++) {
    *cout = *cin;
    cout++;
    i++;
  }
  *cout = '\0';
  if (*cin == '?' && !included(to, UNSET, noexpandhead)) {  /* read in args */
    for ( ; *cin != ' ' && *cin != '\0' && i < MAXSTRINGLENGTH - 1; cin++) {
      *cout = *cin;
      cout++;
      i++;
    }
    *cout = '\0';
  }

  if (*cin != '\n')
    return(6);
  else
    return(7);

}

/* The function sscanf_webstar() is due to Jason Linhart (jason@summary.net) */

#ifdef WEBSTAR
/*
  Mac WebSTAR log files have special lines to tell use which fields are present
  and in what order. Parse these so we know how to parse the other lines.
  This also provides a reliable indicator of whether the log is a WebStar
  log to begin with.

  A sample field name line:
  !!LOG_FORMAT DATE TIME RESULT URL FROM TRANSFER_TIME BYTES_SENT USER
  HOSTNAME REFERER

  The entries consist of TAB delimited fields. A sample line:
  12/14/95        19:00:04        OK
  :pages:Downloads.html           72      3176
  indy1.indy.net. http://www.motu.com/

  We require the following fields at a minimum:
  HOSTNAME, DATE, TIME, RESULT, URL, BYTES_SENT
  We will also read AGENT and REFERER if present.
*/

enum FieldKinds {
  INVALID = 0,
  HOSTNAME_K,
  DATE,
  URL,
  BYTES_SENT,
  TIME,
  RESULT,
  AGENT,
  FROM,
  METHOD,
  PATH_ARGS,
  REFERER_K,
  SEARCH_ARGS,
  TRANSFER_TIME,
  USER,
  LAST_FIELD
};

static const char *fnames[LAST_FIELD] = {
  "!!LOG_FORMAT",
  "HOSTNAME",
  "DATE",
  "URL",
  "BYTES_SENT",
  "TIME",
  "RESULT",
  "AGENT",
  "FROM",
  "METHOD",
  "PATH_ARGS",
  "REFERER",
  "SEARCH_ARGS",
  "TRANSFER_TIME",
  "USER"
};

#define FIELDS_MAX              20

static int fields[FIELDS_MAX];
static int field_cnt = 0;
long Mac_good_lines = 0, Mac_bad_lines = 0, Mac_tab_lines = 0;

extern flag warnq, anywarns;

int sscanf_webstar(char *inputline, char hostn[MAXSTRINGLENGTH], int *date,
		   int *monthno, int *year, int *hr, int *min,
		   char filename[MAXSTRINGLENGTH],
		   char referer[MAXSTRINGLENGTH], char agent[MAXSTRINGLENGTH],
		   int *code, char bytestr[16], size_t preflength)
{     /* scanning 'WebStar' format logfile entries */
  extern struct include *noexpandhead, *refexpandhead;
  extern flag case_insensitive;
  
  register char *cin = inputline;      /* the character we are reading */
  register char *cout;                 /* where we are putting it */
  int i, field;
  int flags;

  flags=0;
  if (*cin=='!' && !memcmp((void *)cin,(void *)fnames[0],strlen(fnames[0]))) {
    while (*cin && *cin!=' ') ++cin;
    if (*cin) ++cin;
    field_cnt=0;
    while (*cin && field_cnt<FIELDS_MAX) {
      for (i=1; i<LAST_FIELD && memcmp((void *)cin,(void *)fnames[i],
				       strlen(fnames[i]));
	   ++i) ;
      if (i>=LAST_FIELD) i=INVALID;
      fields[field_cnt]=i;
      ++field_cnt;
      flags|=(1<<i);
      while (*cin && *cin!=' ') ++cin;
      if (*cin) ++cin;
    }
    if (((flags>>1)&15)!=15 && warnq) {
      fprintf(stderr,"WebStar logs require at least the following fields:\n");
      fprintf(stderr,"  HOSTNAME, DATE, URL, and BYTES_SENT.\n");
      anywarns = ON;
    }
    return(0);
  }
  else if (!field_cnt) {
    ++Mac_bad_lines;
    if (Mac_bad_lines<20) {
      for (i=0; *cin; ++cin) if (*cin=='\t') ++i;
      if (i>=3) ++Mac_tab_lines;
    }
    if (Mac_bad_lines==1 && i==5) {
      field_cnt=6;
      fields[0]=DATE;
      fields[1]=TIME;
      fields[2]=RESULT;
      fields[3]=HOSTNAME_K;
      fields[4]=URL;
      fields[5]=BYTES_SENT;
    }
    else return(0);
  }
  *filename=0;
  *referer=0;
  *agent=0;
  *hr=12;
  *min=0;
  *code=299;
  for (field=0; field<field_cnt; ++field) {
    if (*cin=='\0') break;
    switch (fields[field]) {

    case HOSTNAME_K:  /* read in hostname */
      i = 0;
      for (cout = hostn; *cin != '\t' && *cin != '\0' &&
	   i < MAXSTRINGLENGTH - 1; cin++) {
	*cout = *cin;
	cout++;
	i++;
      }
      if (cout>hostn && *(cout-1)=='.') --cout;
      *cout = '\0';
      flags|=(1<<(HOSTNAME_K-1));
      break;
    case DATE:              /* read in month, day, year */
      /* read in month */
      if (!isdigit(*cin)) return(0);
      else *monthno = 10 * (*cin - '0');
      cin++;
      if (!isdigit(*cin)) return(0);
      else *monthno += (*cin - '0') - 1;
      cin++;

      if (*cin != '/') return(0);
      cin++;

      /* read in day */
      if (!isdigit(*cin)) return(0);
      else *date = 10 * (*cin - '0');
      cin++;
      if (!isdigit(*cin)) return(0);
      else *date += (*cin - '0');
      cin++;

      if (*cin != '/') return(0);
      cin++;

      /* read in year */
      if (!isdigit(*cin)) return(0);
      else *year = 1900 + 10 * (*cin - '0');
      cin++;
      if (!isdigit(*cin)) return(0);
      else *year += (*cin - '0');
      cin++;
      if (*year < 1970) *year += 100;

      flags|=(1<<(DATE-1));
      break;
    case TIME:              /* read in hour, minute, skip sec */
      /* read in hour */
      if (!isdigit(*cin)) return(0);
      else *hr = 10 * (*cin - '0');
      cin++;
      if (!isdigit(*cin)) return(0);
      else *hr += (*cin - '0');
      cin++;

      if (*cin != ':') return(0);
      cin++;

      /* read in minute */
      if (!isdigit(*cin)) return(0);
      else *min = 10 * (*cin - '0');
      cin++;
      if (!isdigit(*cin)) return(0);
      else *min += (*cin - '0');
      cin++;

      if (*cin != ':') return(0);
      cin++;

      /* don't read in second, but check it for correct form */
      if (!isdigit(*cin)) return(0);
      cin++;
      if (!isdigit(*cin)) return(0);
      cin++;

      flags|=(1<<(TIME-1));
      break;
    case URL:               /* read in filename */
      i = 0;
      for (cout = filename; *cin != '\t' && *cin != '\0' &&
	   i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
	if (*cin==':') *cout='/';
	else *cout = *cin;
	cout++;
	i++;
      }
      *cout = '\0';
      flags|=(1<<(URL-1));
      break;
    case SEARCH_ARGS:/* read in search args (part after '?') */
      if (case_insensitive)
	strtolower(filename);
      if (*filename && !included(filename, UNSET, noexpandhead)) {
	cout=filename+strlen(filename);        /* read in args */
	*cout++ = '?';
	for ( ; *cin != '\t' && *cin != '\0' &&
	     i < MAXSTRINGLENGTH - 1 - preflength; cin++) {
	  *cout = *cin;
	  cout++;
	  i++;
	}
	*cout = '\0';
      }
      break;
    case BYTES_SENT:/* read in bytes sent */
      i = 0;
      for (cout = bytestr; *cin != '\t' && *cin != '\0' && i < 16; cin++) {
	*cout = *cin;
	cout++;
      }
      *cout = '\0';
      flags|=(1<<(BYTES_SENT-1));
      break;
    case RESULT:    /* Translate WebStar result codes */
      if (cin[0]=='O' && cin[1]=='K') *code=200;
      else if (cin[0]=='E' && cin[1]=='R' && cin[2]=='R' && cin[3]=='!')
	*code=404;
      else *code=299;
      flags|=(1<<(RESULT-1));
      break;
    case REFERER_K: /* read in refer */
      i = 0;
      for (cout = referer; *cin != '\0' && *cin != '\t' && *cin != '?' &&
	   i < MAXSTRINGLENGTH - 1; cin++) {
	*cout = *cin;
	cout++;
	i++;
      }
      *cout = '\0';                                  /* read in args */
      if (*cin == '?' && included(referer, UNSET, refexpandhead)) {
	for ( ; *cin != '\t' && *cin != '\0' && i < MAXSTRINGLENGTH - 1;
	     cin++) {
	  *cout = *cin;
	  cout++;
	  i++;
	}
	*cout = '\0';
      }
      if (*cin == '?') {
	while (*cin != '\t' && *cin != '\0') cin++;
      }
      flags|=(1<<(REFERER_K-1));
      break;
    case AGENT:             /* read in the user agent */
      i = 0;
      for (cout = agent; *cin != '\0' && *cin != '\t' &&
	   i < MAXSTRINGLENGTH - 1; cin++) {
	*cout = *cin;
	cout++;
	i++;
      }
      *cout = '\0';
      flags|=(1<<(AGENT-1));
      break;
    }
    while (*cin && *cin!='\t') ++cin;
    if (*cin) ++cin;
  }
  if ((flags&15)!=15) return(0);
  ++Mac_good_lines;
  return(11);
}
#endif

/* The function sscanf_netpresenz() and associated functions are due to 
   Nigel Perry (N.Perry@massey.ac.nz), September 1996 */

/* Parse NetPresenz 4.x format logs

   The format is:

       time\tdate\tIP\ttype\tsession\taction\tfile\t\r
       fullpath\r
       Referer: path\r

   Time may include am/pm. This code currently assumes date is dd/mm/yy,
   this is probably incorrect and Mac international routines should be used
   to parse both the time and date. 

   Type is one of HTTP, Gopher, NetPresenz ... (admin entries), or user id for
   FTP - the user id is an arbitrary string (users can type anything
   in!). Session is used only for FTP and gives the number of the current user.

   Action is one of get file, get dir, log in, log out, CGI, or error msg.
   The log in/out are for FTP.

   File is the short name, fullpath is the full (Mac) pathname, referer is the
   full URL as supplied. Lines two and three are optional.

   We will build an "agent" from the type & action fields, error from action
   field. Login, log out and NetPresenz admin lines will be ignored.

   There is no bytes sent field in the NetPresenz log, maybe this can be
   generated from the full path at some point...
*/

#ifdef NETPRESENZ

static const MatchPairs entryTypes[] =
{ { "HTTP", NP_HTTP },
    { "Gopher", NP_GOPHER },
    { NULL, NP_FTP }
};

static const MatchPairs actionTypes[] =
{ { "get file", NP_GETFILE },
    { "get dir", NP_GETDIR },
    { "log in", NP_LOGIN },
    { "log out", NP_LOGOUT },
    { "CGI", NP_CGI },
    { NULL, NP_ERROR }
};

NP_type checkValue(char *typeStr, const MatchPairs table[])
{
  while(table->str != NULL)
    { if(strcmp(table->str, typeStr) == 0) return table->value;
      table++;
    }
  return table->value; /* last field contains default value to return */
}

/* the NetPresenz fields have to be treated differently as they are
   matched by prefix only */

NP_type checkType(char *typeStr)
{ if(strncmp(typeStr, "NetPresenz", 10) == 0) return NP_ADMIN;
  return checkValue(typeStr, entryTypes);
}

/* make up an agent value from the type and action fields */

void build_agent(char agent[MAXSTRINGLENGTH], NP_type entryType,
		 char *entryTypeStr, NP_type actionType, char *actionTypeStr)
{ /* a simple algorithm for the moment */
  char *ans;

  switch(entryType)
    { case NP_HTTP:
	ans = actionType == NP_CGI ? "cgi" : "http";
	break;
      case NP_GOPHER:
	ans = "gopher";
	break;
      case NP_FTP:
	ans = "ftp";
	break;
      default:
	ans = "unknown";
	break;
      }
  strcpy(agent, ans);
}

/* the speed of sscanf is of concern, but I'm going to use a routine to read
   in positive numbers - the cost is small.
   The routine returns -1 on error, otherwise the value read in.
   The argument pointer is updated.
   */
int readDigits(char **from)
{
  int ans;
  char *cin = *from;

  if(!isdigit(*cin)) return -1;
  ans = *cin++ - '0';
  
  while(isdigit(*cin))
    { ans *= 10;
      ans += *cin++ - '0';
    }
  
  *from = cin;
  return ans;
}

/* scan a file name, obeying global flags
   return 0 on error, 1 if ok
   */
int scan_path(char *inputline, char filename[MAXSTRINGLENGTH], int maxlen,
	      struct include *expandhead)
{
  char *cin = inputline;
  char *cout = filename;
  int ix;

  extern flag case_insensitive;

  ix = 0;
  while(*cin && *cin != '\n' &&
        *cin != '?' && ix < maxlen)
    { *cout++ = *cin++;
      ix++;
    }
  *cout = '\0';
  if (*cin == '?')
    { if (case_insensitive)
	strtolower(filename);  /* if no question mark, strtolower later */
      if (!included(filename, UNSET, expandhead))
	{ while (*cin && *cin != '\n' && ix < maxlen) /* read in args */
	    { *cout++ = *cin++;
	      ix++;
	    }
	  *cout = '\0';
	}
    }
  if (*cin != '\n' && *cin != '?') return 0;    /* fouled up somewhere */

  return 1;
}

int sscanf_netpresenz(FILE *logfile, char *inputline,
		      char hostn[MAXSTRINGLENGTH], int *date,
		      int *monthno, int *year, int *hr, int *min,
		      char filename[MAXSTRINGLENGTH],
		      char referer[MAXSTRINGLENGTH],
		      char agent[MAXSTRINGLENGTH],
		      int *code, char bytestr[16], size_t preflength)
{
  char *cin = inputline;
  char *cout;
  int peek;
  char *entryTypeStr, *actionTypeStr;
  char *entryEnd, *actionEnd;
  NP_type entryType, actionType;
  int ix;

  extern struct include *noexpandhead, *refexpandhead;
#ifdef MAC
  Intl0Rec **intl_format;
  int tmp;
  static int dateOrder = -1;
  
  if (dateOrder<0) {
    intl_format=(Intl0Rec **)IUGetIntl(0);
    if (intl_format && *intl_format) dateOrder=(**intl_format).dateOrder;
    else dateOrder=0;
  }
#endif
  /* read time */
  /* hour */
  if((*hr = readDigits(&cin)) < 0) return 0;

  if(*cin++ != ':') return 0;

  /* min */
  if((*min = readDigits(&cin)) < 0) return 0;

  /* check for am/pm - we only check first character */
  while(*cin == ' ') cin++; /* skip spaces */
  switch(*cin)
    { case 'a': case 'A':
	if(*hr == 12) *hr = 0; /* 12am = 00:00 hours */
	break;
      case 'p': case 'P':
	if(*hr != 12) *hr += 12; /* 12pm = 12:00 hours */
	break;
      case '\t':
	break; /* no am/pm */
      }

  /* skip to next field */
  while(*cin && (*cin != '\t')) cin++;
  if(*cin++ != '\t') return 0;

  /* date */
  if((*monthno = readDigits(&cin)) < 0) return 0;
  if(*cin++ != '/') return 0;
  if((*date = readDigits(&cin)) < 0) return 0;
  if(*cin++ != '/') return 0;
  if((*year = readDigits(&cin)) < 0) return 0;
#ifdef MAC
  switch (dateOrder) {                    /* 0 and default - mdy */

  case 1:                                                 /* 1 - dmy */
    tmp = *monthno;
    *monthno = *date;
    *date=tmp;
    break;
  case 2:                                                 /* 2 - ymd */
    tmp = *monthno;
    *monthno = *date;
    *date = *year;
    *year=tmp;
    break;
  case 3:                                                 /* 3 - myd */
    tmp = *year;
    *year = *date;
    *date=tmp;
    break;
  case 4:                                                 /* 4 - dym */
    tmp = *monthno;
    *monthno = *year;
    *year = *date;
    *date=tmp;
    break;
  case 5:                                                 /* 5 - ydm */
    tmp = *monthno;
    *monthno = *year;
    *year=tmp;
    break;
  }
#endif

  /* adjust year */
  *year += *year < 70 ? 2000 : 1900;
  /* adjust month - this program starts at 0 */
  *monthno -= 1;

  /* skip to next field */
  while(*cin && (*cin != '\t')) cin++;
  if(*cin++ != '\t') return 0;

  /* read in hostname */
  ix = 0;
  cout = hostn;
  while(*cin != '\t' && *cin != '\0' && ix < (MAXSTRINGLENGTH - 1))
    { *cout++ = *cin++;
      ix++;
    }
  if (*cin++ != '\t') return(0);
  *cout = '\0';
  
  /* now entry type - tab terminated string */
  entryTypeStr = cin;
  while(*cin && (*cin != '\t')) cin++;
  if(*cin != '\t') return 0;

  entryEnd = cin++;
  /* so we can restore the tab on error */
  *entryEnd = '\0';  /* write eos over tab */
  entryType = checkType(entryTypeStr);

  /* now let's be smart, if the entry type is NP_ADMIN rather than return
     a bad line error, let's read a new line and start again. This stops
     lines which are not really bad from being counted as such.
     */
  if(entryType == NP_ADMIN)
    { int c;

      if((c = fgetc(logfile)) == EOF) return(0);
                   /* not much we can do in this case */
      ungetc(c, logfile);  /* you never know it might be a one char line... */
      fgets(inputline, MAXLINELENGTH, logfile);   /* won't fail -- >= 1 char */
      return sscanf_netpresenz(logfile, inputline, hostn, date, monthno, year,
			       hr, min, filename, referer, agent, code,
			       bytestr, preflength);
    }
  /* we have no use for the session, skip it */
  while(isdigit(*cin)) cin++;
  if(*cin++ != '\t')
    { *entryEnd = '\t';   /* return input to pristine state */
      return 0;
    }

  /* now the action type - tab terminated string */
  actionTypeStr = cin;
  while(*cin && (*cin != '\t')) cin++;
  if(*cin != '\t') return 0;

  actionEnd = cin++;          /* so we can restore the tab on error */
  *actionEnd = '\0';          /* write eos over tab */
  actionType = checkValue(actionTypeStr, actionTypes);

  /* similar to above, ignore log in/out */
  if(actionType == NP_LOGIN || actionType == NP_LOGOUT)
    { int c;

      if((c = fgetc(logfile)) == EOF) return(0);
      ungetc(c, logfile);
      fgets(inputline, MAXLINELENGTH, logfile);
      return sscanf_netpresenz(logfile, inputline, hostn, date, monthno, year,
			       hr, min, filename, referer, agent, code,
			       bytestr, preflength);
    }

  /* now the file - this is the short version which we might discard in
     a moment if there is a full path following
     */
  cout = filename;
  ix = 0;
  /* 04/Oct/96 JTL <start> - catch missing file name */
  if (!*cin || *cin=='\t') {
        *cout++ = '/';
        ix++;
      }
  else while(*cin && (*cin != '\t') &&
	     (ix < (MAXLINELENGTH - 1 - preflength)))  {
    if (*cin==':') {
      *cout++ = '/';
      ++cin;
    }
    else *cout++ = *cin++;    /* JTL <end> - and convert ':' to '/' */
    ix++;
  }
  *cout = '\0';
  /* add eos */

  if(*cin != '\t')
    { *entryEnd = *actionEnd = '\t';   /* return input to pristine state */
      return 0;
    }

  /* we now have a complete first line, finish off */
  build_agent(agent, entryType, entryTypeStr, actionType, actionTypeStr);
  /* set ok/error based on actionType */
  *code = actionType == NP_ERROR ? 404 : 200;
  /* set bytes to zero - as a string... */
  strcpy(bytestr, "0");
  /* no referer yet */
  *referer = '\0';

  /* now try for optional lines - first the full pathname */
  /* from this point on we always return 11 = referer & browser present,
     latter code handles there being no refer by an empty string check.
     This way we get a browser count which includes ftp & gopher
     */
  peek = fgetc(logfile);        /* peek at next char */
  if(peek == EOF) return 11;
  ungetc(peek, logfile);        /* put it back */
  if(isdigit(peek)) return 11;

  /* read in full path */
  fgets(inputline, MAXLINELENGTH, logfile);  /* can't fail - ungetc above */
  /* now copy it, obeying various flags - adapted from sscanf_common() */
  *filename = '/';
  /* leading / isn't there - add it */
  /* note filename+1 & -2 to account for last line */
  if(scan_path(inputline, filename+1, MAXLINELENGTH - 2 - preflength,
	       noexpandhead) == 0)
    return 0;
  /* fouled up somewhere, and trashed our file :-( */

  /* fix :'s in Mac path */
  cin = filename;
  while(*cin)
    { if(*cin == ':') *cin = '/';
      cin++;
    }

  /* ok, let's try for the referer... */
  peek = fgetc(logfile);             /* peek at next char */
  if(peek == EOF) return 11;
  ungetc(peek, logfile);             /* put it back */
  if(isdigit(peek)) return 11;

  /* read in full line */
  fgets(inputline, MAXLINELENGTH, logfile);

  /* skip past "Referer: " */
  cin = inputline;
  while(*cin && *cin != ' ') cin++;
  if(*cin++ != ' ') return 11;

  /* copy referer */
  if(scan_path(cin, referer, MAXLINELENGTH - 1, refexpandhead) == 0)
    *referer ='\0';

  return 11;
  /* we got the lot! */
}
#endif /* NETPRESENZ */
