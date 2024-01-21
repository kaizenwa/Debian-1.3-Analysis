    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "server_def.h"
#include "s_extern.h"
#include <ctype.h>
#include <netdb.h>

/****************************************************************************
* This file contains routines to maintain client database.
****************************************************************************/

#ifndef VMS
extern char *realloc(), *malloc(), *ctime();
#endif
extern int priv_mode;
extern int no_unnamed;

static HTAB     *htab;		/* client data base.			*/
static unsigned  hcnt;		/* number of clients.			*/
static unsigned  htot = 0;	/* available entries in the data base.	*/
static HTAB     hzero;

#define HALLOC_SIZE 30

IPrange **iptab = 0;
unsigned int ipcnt = 0, iptot = 0;

char *check_ip PROTO1(unsigned long, inet_num)
{
  int i, j;
  unsigned char val[4];
  
  val[0] = (inet_num & 0x000000ff)      ;
  val[1] = (inet_num & 0x0000ff00) >>  8;
  val[2] = (inet_num & 0x00ff0000) >> 16;
  val[3] = (inet_num & 0xff000000) >> 24;
  
  for (i = 0; i < ipcnt; i++) {
    for (j = 0; j < 4; j++)
      if (iptab[i]->lo[j] > val[j] || val[j] > iptab[i]->hi[j]) break;
    if (j == 4) break;
  }
  
  if (i >= ipcnt) return 0;
  
  return iptab[i]->text;
}

#define skip_whitespace(x) do {while (*(x)&&isspace(*(x))) (x)++;} while (0)

/* parse the text string as an integer and return a value between 0x00 and 0xff
   if there is any error in forming the value (e.g., the text string
   isn't an integer, or the integer value is too large) then make the
   text string NULL */
static unsigned char parse_ipcomponentnum PROTO1(char **, textp)
{
  unsigned long val = 0;
  
  if (!isdigit(**textp)) {
    *textp = 0;
  } else
    do {
      val = 10 * val + (**textp - '0');
      (*textp)++;
    } while (isdigit(**textp));
  
  if (val > 0xff) {
    val = 0;
    *textp = 0;
  }
  
  return val;
}

/* parse a whole field of a numerical IP address; it can be one of:
   integer		>> fixed value
   integer '-' integer	>> range of values
   '*'			>> same as 0-255
   */
static char *parse_ipcomponent PROTO3(char *, text, unsigned char *, lo,
				      unsigned char *, hi)
{
  if (*text == '*') {
    *lo = 0x00;
    *hi = 0xff;
    return (text + 1);
  }
  
  *lo = parse_ipcomponentnum(&text);
  if (!text) return 0;
  
  if (*text == '-') {
    text++;
    *hi = parse_ipcomponentnum(&text);
  } else *hi = *lo;
  
  return text;
}

static IPrange *parse_ipnumber PROTO1(char *, text)
{
  IPrange *reply;
  int i;
  
  reply = (IPrange *)malloc(sizeof(IPrange));
  
  for (i = 3; i >= 0 && !isspace(*text); i--) {
    if (i < 3) {
      if (*text != '.') return 0;
      else text++;
    }
    text = parse_ipcomponent(text, &reply->lo[i], &reply->hi[i]);
    if (!text) {
      free((char *)reply);
      return 0;
    }
  }
  
  /* fill in the gaps in the case that the loop terminated due to
     the occurrence of white-space */
  for (; i >= 0; i--) {
    reply->lo[i] = 0x00;
    reply->hi[i] = 0xff;
  }
  
  return reply;
}

static IPrange *parse_hostname PROTO2(char *, text, int, len)
{
  IPrange *reply;
  struct hostent *hostaddr;
  unsigned long inet_num;
  char *hostname;
  
  hostname = malloc(len + 1);
  strncpy(hostname, text, len);
  hostname[len] = 0;
  
  hostaddr = gethostbyname(hostname);
  free(hostname);
  
  if (!hostaddr) return 0;
  
  reply = (IPrange *)malloc(sizeof(IPrange));
  inet_num = ((struct in_addr *)(hostaddr->h_addr))->s_addr;
  reply->lo[0] = reply->hi[0] = (inet_num & 0x000000ff)      ;
  reply->lo[1] = reply->hi[1] = (inet_num & 0x0000ff00) >>  8;
  reply->lo[2] = reply->hi[2] = (inet_num & 0x00ff0000) >> 16;
  reply->lo[3] = reply->hi[3] = (inet_num & 0xff000000) >> 24;
  
  return reply;
}

/* parse a single line for the IP hosts file */
IPrange *parse_ipline PROTO1(char *, text)
{
  IPrange *reply;
  char type = 0;
  char *message = 0;
  int messlen = 0, addresslen;
  
  /* skip the leading white-space; if the line is commented or empty,
     ignore it */
  skip_whitespace(text); if (!*text || *text == '#') return 0;
  
  /* find the first non-space character after the address - this
     identifies the type of host this is */
  message = text;
  while (*message && !isspace(*message)) message++;
  addresslen = message - text;
  skip_whitespace(message);
  
  if (!*message || *message == '#') {
    /* if a host name is mentioned by itself, then treat it as ignored
       or normal depending on the value of priv_mode */
    type = priv_mode ? 'N': 'I';
    message = "";
    messlen = 0;
  } else {
    /* the first character after the host name is the type of host */
    type = *message;
    
    /* skip over the white space trailing the type - the start of the
       associated message */
    message++; /* remember skip_whitespace() is a macro... */
    skip_whitespace(message);
    
    /* `remove' the trailing white-space from the message */
    messlen = strlen(message);
    while (messlen > 0 && isspace(message[messlen-1])) messlen--;
  }
  
  /* if the first character of the address is numerical or '*' then parse
     as a numerical address, otherwise we do a host lookup on the name. */
  if (*text == '*' || isdigit(*text))
    reply = parse_ipnumber(text);
  else
    reply = parse_hostname(text, addresslen);
  
  if (!reply) {
    fprintf(stderr, "Badly formed address in config file:\n\t%s", text);
    return 0;
  }
  
  /* allocate a string to hold the message */
  reply->text = malloc(1 + messlen + 1); /* type + text + '\0' */
  reply->text[0] = type;
  strncpy(&reply->text[1], message, messlen);
  reply->text[1 + messlen] = '\0';
  
  return reply;
}

/****************************************************************************
 * Write out the IP table in the .IPTAB_DUMP file.
 ****************************************************************************/

int dump_iptab PROTO0((void))
{
  int i;
  FILE *fp;
  
  if (!(fp = fopen(".IPTAB_DUMP","w"))) return;
  
  for (i = 0; i < ipcnt; i++) {
    fprintf(fp, "%3d-%3d.%3d-%3d.%3d-%3d.%3d-%3d  %c  `%s'\n",
	    iptab[i]->lo[3], iptab[i]->hi[3],
	    iptab[i]->lo[2], iptab[i]->hi[2],
	    iptab[i]->lo[1], iptab[i]->hi[1],
	    iptab[i]->lo[0], iptab[i]->hi[0],
	    iptab[i]->text[0], &iptab[i]->text[1]);
  }
  
  fclose(fp);
}

/****************************************************************************
 * Look up the hostname for an inet number.  Return NULL if not found.
 ****************************************************************************/

static char *find_hostname PROTO1(unsigned long, inet_num)
{
  struct hostent *he;
  char *hostname;
  
  if ((he = gethostbyaddr((char*)&inet_num, sizeof(inet_num), AF_INET))) {
    hostname = malloc(strlen(he->h_name)+1);
    strcpy(hostname, he->h_name);
  } else
    hostname = 0;
  
  return hostname;
}

/****************************************************************************
 * Returns an entry from the database corresponding to to the inet number.
 * A new entry is created is it is not found.
 * The database is a linear array of sorted structures.
 * Entries are searched using binary search on the array.
 ****************************************************************************/

HTAB *find_host PROTO1(unsigned long, inet_num)
{
  unsigned l, h, m, i;
  unsigned long inum;
  HTAB *hs, *hd;
  
  for(l = 0, h = hcnt-1; (m = (l + h) >> 1) != l; ) {	/* binary search */
    inum = htab[m].inet_num;
    if(inum > inet_num) h = m;
    else if(inum < inet_num) l = m;
    else {
      /* if we *need* reverse naming and we haven't already got one
	 then try looking it up again. */
      if (no_unnamed && !htab[m].hostname)
	htab[m].hostname = find_hostname(inum);
      htab[m].acc_cnt++;
      return(htab+m);
    }
  }
  
  if(htab[m].inet_num < inet_num) m++;  /* locate first entry that is > */
  
  if((hcnt+1) > htot) { /* need more space */
    htot += HALLOC_SIZE;		/* add HALLOC_SIZE entries at a time */
    
    if(!(htab = (HTAB *) realloc(htab,sizeof(HTAB)*htot))) {
      perror("grow_htab realloc");
      exit(1);
    }
  }
  
  for(i = hcnt-m, hs = htab+hcnt, hd=htab+hcnt+1; i--; *--hd = *--hs);
  
  htab[m]=hzero;
  htab[m].inet_num = inet_num;
  htab[m].last_key = get_next_key();
  htab[m].next_key = get_next_key()+1;
  htab[m].hostname = find_hostname(inet_num);
  hcnt++;
  return(htab+m);
}

/****************************************************************************
 * Client database initialization routine.
 ****************************************************************************/

int init_htab PROTO0((void)) /* always have 2 entries -- 0, MAXINT */
{
  FILE *fp;
  HTAB *hp;
  char buf[1024];
  unsigned int i1,i2,i3,i4;
  unsigned long hnum;
  
  if(!(htab = (HTAB *) malloc(sizeof(HTAB)*HALLOC_SIZE))) {
    perror("grow_htab malloc");
    exit(1);
  }
  htab[0] = hzero;
  htab[1] = hzero;
  htab[1].inet_num = ~0;
  hcnt = 2;
  htot = HALLOC_SIZE;
}

/****************************************************************************
 * Write out the client table in the .HTAB_DUMP file.
 ****************************************************************************/

int dump_htab PROTO0((void))
{
  int i;
  FILE *fp;
  HTAB *hp;
  
  if(!(fp = fopen(".HTAB_DUMP","w"))) return;
  
  for(i = hcnt-2, hp = htab+1; i--; hp++) {
    fprintf(fp,"%d.%d.%d.%d\t%5d %s", ((unsigned char *)(&hp->inet_num))[0],
	    ((unsigned char *)(&hp->inet_num))[1],
	    ((unsigned char *)(&hp->inet_num))[2],
	    ((unsigned char *)(&hp->inet_num))[3],
	    hp->acc_cnt,
	    ctime((time_t *) &(hp->last_acc)));
  }
  
  fclose(fp);
}
