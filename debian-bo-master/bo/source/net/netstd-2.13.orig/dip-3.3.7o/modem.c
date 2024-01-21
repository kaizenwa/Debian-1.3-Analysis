/*
 * dip		A program for handling dialup IP connecions.
 *		Modem driving module.  On systems that support the
 *		dial(3) package, we (should) use that.  Otherwise,
 *		we use a very rudimentary HAYES-type dialer.
 *
 * Version:	@(#)modem.c	3.3.3	08/16/93
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal, <uri@watson.ibm.com>
 * 		Copyright 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"


/* Here are some HAYES modem commands. */
#define AT_ATTN		"+++"			/* "attention" string	*/
#define AT_RSET		"ATZ"			/* reset the modem	*/
#define AT_INIT		"ATE0 Q0 V1 X4"		/* setup for dialing	*/
#define AT_DIAL		"ATD"			/* start a dial		*/
#define AT_HANG		"ATH"			/* hang up the modem    */
#define AT_EOL		"\r"			/* AT "CRLF" mark	*/


static char	mdm_type[32];
static int	mdm_setup = 0;			/* can we do anything?	*/
static char	*mdm_init_s = AT_INIT;		/* current INIT string	*/
static int      timeout;

/* Reach this procedure at alarm clock. */
static void
TimeOut(int sig)
{
  (void) sig;
  timeout = 1;
}

/* Chat with modem, wait for one of specified responses. */
int
mdm_chatseq(char *send, struct response *expect, int out)
{
  struct response *resp;
  int  minlen;
  int  maxlen;
  char buff[128];
  char *s;
  int  len;
  void (*oldsig)(int);

  /* If you want to send nothing - the modem doesn't mind */
  if ((send == (char *)0) || (*send == '\0'))
    return 0;

#if 0
  /*
   * FIXME: this is ugly!!! Some modem's faults when its receiving
   * new command before finish of sending reply for previous command.
   */
  struct timeval   delay;
  
  delay.tv_sec = 0;
  delay.tv_usec = 500000;
  (void) select(0, (fd_set *)NULL, (fd_set *)NULL, (fd_set *)NULL, &delay);
#else
  (void) sleep(1);
#endif

  tty_flush();
  if((tty_puts(send) < 0) || (tty_puts(AT_EOL) < 0))	/* Modem fail */
    return(-1);
 
  if(expect == NULL)
    return (0);
  
  /* Calculating minimal and maximal length of response string */
  resp = expect;
  minlen = 9999;
  maxlen = 0;
  while(resp != NULL) {
    len = strlen(resp->string);
    if(len < minlen)
      minlen = len;
    if(len > maxlen)
      maxlen = len;
    resp = resp->next;
  }
  
  timeout = 0;
  oldsig = signal(SIGALRM, TimeOut);
  (void) alarm(out);
  s = buff;
  while(s - buff < minlen) {
    if(((len = tty_getc()) == -1) || timeout)	/* Terminal detached from modem */
    {
      (void) alarm(0);
      (void) signal (SIGALRM, oldsig);
      return((timeout)? 3 : -1);
    }
    if(len == -2)			/* No symbols in buffer */
      continue;
    *s++ = (char) len;
  }
  for(;;) {
    resp = expect;
    while(resp != NULL) {
      len = strlen(resp->string);

      /* Looking for wanted string in tail of buffer */
      if((s - buff >= len) && !strncmp(s - len, resp->string, len))
      {
        (void) signal(SIGALRM, oldsig);
        (void) alarm(0);
        return(resp->code);	/* We got that */
      }

      resp = resp->next;
    }
    if((s - buff) >= maxlen) {
      memcpy(buff, &buff[1], maxlen);	/* Scroll the string in buffer */
      --s;
    }
    len = -2;
    while(len == -2) {
      if(((len = tty_getc()) == -1) || timeout)
        break;
    }
    if((len == -1) || timeout)
      break;
    *s++ = (char) len;
  }
  (void) signal(SIGALRM, oldsig);
  (void) alarm(0);
  return((timeout) ? 3 : -1);
}



/* Set the desired type of modem. */
int
mdm_modem(char *modem)
{
  
  if (strcmp(modem, DEF_MODEM) != 0) {
	fprintf(stderr, "dip: modem: unknown modem type %s !\n", modem);
	return(-1);
  }
  memset(mdm_type, 0, sizeof(mdm_type));
  strncpy(mdm_type, modem, sizeof(mdm_type));
  mdm_setup = 1;
  if (opt_v == 1) printf("Modem set to \"%s\".\n", mdm_type);
  return(0);
}


/* Setup an INIT string for the current type of modem. */
int
mdm_init(char *text)
{
  char *s;
  int   t_len = strlen(text);

  if (t_len > 256) {
    fprintf(stderr, "Mdm init string too long (%d). Bye...\n", t_len);
  }
  
  if ((s = (char *) malloc(strlen(text) + 1)) == NULL) {
    fprintf(stderr, "Can't allocate memory for string\n");
    return(-1);
  }

  mdm_init_s = s;
  strncpy(mdm_init_s, text, t_len);
  if (opt_v == 1) printf("Modem INIT string set to \"%s\"\n", mdm_init_s);
  return(0);
}


/* Dial a phone number. */
int
mdm_dial(char *number, int out)
{
  char  buff[128];
  char *prefix = (char *)0;
  int   res    = 0;
  int   b_len = sizeof(buff);

/* R.H. is Ronald Holzloehner <ronald@marie.physik.TU-Berlin.DE> */
#define AT_DIAL_STORED "ATDS="

  /* R.H. */
  /* Numbers beginning with 'S' are treated as stored ones,
  for example
  dial S2
  will be translated in
  ATDS=2
  */

  if(*number == 'S')
        {
        number++;
        prefix=AT_DIAL_STORED;
        }
  else
        prefix=AT_DIAL;
/* R.H. End */


  if (mdm_setup == 1) {
	/* Setup to dial out.  Send the init command */
#ifdef NE_PAUL
	sprintf(buff, "%s%s", mdm_init_s, AT_EOL);
	tty_puts(buff);
	(void) sleep(1);
#else
	if ((res = mdm_chatseq(mdm_init_s, chat, 5)) != 0)
	  return(res);
#endif /* NE_PAUL */

	/* How's our parameter length? Trying to override stack? */
	if (strlen(number) + strlen(prefix) >= b_len) {
	  fprintf(stderr, "Sorry, Voltarian numbers not supported.\n");
	  syslog(LOG_INFO, "Attempt to dial Voltarian number! uid=%d", 
		 getuid());
	  return -1;
	}

	/* Dial the phone number.  The dialing itself */
#ifdef NE_PAUL
	sprintf(buff, "%s%s%s", prefix, number, AT_EOL);
	tty_puts(buff);
	return(0);
#else
	sprintf(buff, "%s%s", prefix, number);
	return(mdm_chatseq(buff, chat, out));
#endif /* NE_PAUL */
  }
  return(-1);
}


int
mdm_reset(void)
{
  if (mdm_setup == 1) {
	/* Make the modem listen to us. */
	(void) sleep(1);
	if (tty_puts(AT_ATTN) < 0)
		return (-1);
	(void) sleep(1);

	/* Reset the modem. */
	return(mdm_chatseq(AT_RSET, chat, 5));
  }
  return(-1);
}

int
mdm_hangup(void)
{
  if (mdm_setup == 1) {
	/* Make the modem listen to us. */
	(void) sleep(2);
	tty_puts(AT_ATTN);
	(void) sleep(2);

	/* Hang up the modem. */
	return(mdm_chatseq(AT_HANG, chat, 5));
  }
  return(-1);
}
