/*
 * dip		A program for handling dialup IP connecions.
 *		Script language processor.
 *
 * Version:	@(#)command.c	3.3.5	12/13/93
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal, <uri@watson.ibm.com>
 *		Copyright 1994, 1995, 1996 [and whatever :-]
 *
 *		Paul Cadach, <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *              Matthew Baker <M.Baker@qut.edu.au>
 *              Copyright 1995
 *              Added the `shell' command
 *
 *              Inaky Perez Gonzalez <inaky@peloncho.fis.ucm.es>
 *              (C) 1995 
 *              Offered code for "shell" command, added "onexit"
 *              command.
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"
#include <linux/types.h>
#include <linux/ip.h>
#include <linux/udp.h>
#include <sys/wait.h>

#define HASHSIZE 128

struct variable {
  struct variable *next;
  int              type;
  char            *name;
  char            *str_value;
  int              value;

};

struct keyword {
  char            *key;
  int              code;
};

struct label {
  struct label    *next;
  char            *name;
  off_t            offset;
};

struct commands {
  char	*name;
  int	(*func)(int, char **);
};


static int	timeout;		/* script "wait" timed out	*/
static int	errlevel;		/* script command return code	*/
static FILE	*scriptfp = (FILE *)NULL;	/* input script pointer	*/
static char	*var_modem = NULL;	/* name of modem we are using	*/
static char	var_modem_buff[PATH_MAX];
static char	*var_port = NULL;	/* terminal line used		*/
static char	var_port_buff[PATH_MAX];
static char	*var_speed = NULL;	/* terminal line speed		*/
static char	var_speed_buff[PATH_MAX];
static int	var_echo = 0;		/* enable line data echoing	*/
static int	var_dcount = 0;		/* number of times we've dialed */

static struct variable *hashtable[HASHSIZE];
static struct label    *labels;
static int       	alllabels = 0;		/* all labels collected */

#ifdef SECUREID
static char	var_securidfixed[9] = "";     /* fixed part of secureID */
#endif /* SECUREID */

static void
sig_exit(void)
{
  tty_close();
  exit(1);
}

static void
TimeOut(int sig)
{
  (void) sig;
  timeout = 1;
}


/* Convert a C-style backslash sequence to ASCII. */
static char
cvt_char(char c)
{
  if (c == '\0') return(c);

  switch(c) {
	case 'a':
		return('\a');

	case 'b':

		return('\b');

	case 'e':
		return(0x1b);

	case 'f':
		return('\f');

	case 'n':
		return('\n');

	case 'r':
		return('\r');

	case 's':
		return(' ');

	case 't':
		return('\t');

	case 'v':
		return('\v');

	case '\\':
		return('\\');

	case '\'':
		return('\'');

	case '"':
		return('\"');

	case '?':
		return('\?');

	case '$':
		return('$');

	default:
		return('?');
  }
  /* NOTREACHED */
  return('?');
}


static int
getkeyword(char *key, struct keyword *list)
{
  while(list->key != (char *)NULL) {
    if(strcmp(key, list->key) == 0)
      return(list->code);
    ++list;
  }
  return(-1);
}

static char *
lookkeyword(int code, struct keyword *list)
{
  while(list->key != (char *)NULL) {
    if(code == list->code)
      return(list->key);
    ++list;
  }
  return("--unknown--");
}

static int
add_label(char *label, off_t offset)
{
  struct label *l;

  if(labels == (struct label *)NULL)
    l = labels = malloc(sizeof(struct label));
  else
  {
    l = labels;
    while(l->next != NULL) {
      if(strcmp(l->name, label) == 0)			/* Duplicated labels */
        return(-1);
      l = l->next;
    }
    if(strcmp(l->name, label) == 0)
      return(-1);
    l->next = malloc(sizeof(struct label));
    l = l->next;
  }
  l->name = strdup(label);
  l->offset = offset;
  l->next = (struct label *)NULL;
  return(0);
}

static off_t
lookup_label(char *label)
{
  struct label *l;

  l = labels;
  while(l != (struct label *)NULL) {
    if(strcmp(l->name, label) == 0)
      return(l->offset);
    l = l->next;
  }
  return((off_t)-1);
}

static off_t
last_label(void)
{
  struct label *l;

  if((l = labels) != NULL) {
    while(l->next != NULL)
      l = l->next;
    return(l->offset);
  }
  return((off_t)-1);
}

inline static int
hashsum(char *name)
{
  int hash = 0x28101973;

  while(*name)
    hash = (hash << 2) ^ (*(unsigned char *)name++);

  return(hash & (HASHSIZE - 1));
}

static struct variable *
lookup_hash(char *varname)
{
  int sum = hashsum(++varname);		/* Skip leading '$' */
  struct variable *var;

  var = hashtable[sum];
  while(var != (struct variable *)NULL) {
    if(strcmp(var->name, varname) == 0)
      return(var);
    var = var->next;
  }
  return((struct variable *)NULL);
}

static struct variable *
add_hash(char *varname)
{
  int sum = hashsum(++varname);		/* Skip leading '$' */
  struct variable *var;
  
  var = hashtable[sum];
  while(var != (struct variable *)NULL) {
    if(strcmp(var->name, varname) == 0)
      return(var);
    var = var->next;
  }
  if((var = (struct variable *)malloc(sizeof(struct variable))) 
	== (struct variable *)NULL)
  {
    fprintf(stderr, "Not enough memory for allocate variable\n");
    return((struct variable *)NULL);
  }
  else {
    var->next = hashtable[sum];
    hashtable[sum] = var;
    if((var->name = strdup(varname)) == (char *)NULL)
    {
      fprintf(stderr, "Not enough memory for allocate variable\n");
      return((struct variable *)NULL);
    }
    var->value = 0;
  }
  return(var);
}

int is_special(char *name)
{
  if(!strcmp(name, "$locip") || !strcmp(name, "$local") ||
     !strcmp(name, "$rmtip") || !strcmp(name, "$remote") ||
     !strcmp(name, "$mtu") || !strcmp(name, "$modem") ||
     !strcmp(name, "$port") || !strcmp(name, "$speed"))
    return(1);
  return(0);
}


/* valudates a decimal number as value of variable */
inline static int is_a_num(char *n)
{
  while (*n) { /* check all the characters on the value */
    if ((*n < '0') || (*n > '9'))
      return 0; /* not a decimal number */
      n++;
  }
  return 1; /* yes, a digit */
}


inline static void get_value(int *var, char *name)
{
  struct variable *v;
                   
  *var = -1;  /* Just a precaution... */
 
  if(*name != '$') {
    if (is_a_num(name))
      *var = atoi(name);
    else {
      fprintf(stderr, "In this place must use integer variables!\n");
      return;
    }
  } else {
    if(is_special(name))  {
      fprintf(stderr, "Cannot use special variable %s in this place!\n",
              name);
      return;
    }  
    if((v = lookup_hash(name)) == (struct variable *)NULL) {
      fprintf(stderr, "Unknown variable %s!\n", name); 
      return;
    } 
    *var = v->value;
  }
}


/* Split the input string into multiple fields. */
static int
getargs(char *string, char *arguments[])
{
  char *sp;
  int argc;
  int i;

  sp = string; i = 0;
  arguments[i] = sp;
  while (*sp && i < 32) {
        while (*sp && (*sp == ' ' || *sp == '\t')) sp++;
        arguments[i++] = sp;
        while (*sp && *sp != ' ' && *sp != '\t') sp++;
        if (*sp != '\0') *sp++ = '\0';
  }
  argc = i;
  while (i < 32) arguments[i++] = (char *)NULL;
  return(argc);
}


/************************************************************************
 *									*
 *		Internal Scripting Commands				*
 *									*
 ************************************************************************/

static int do_beep(int argc, char *argv[])
{
  int  i,times;
  
  if (argc > 2 ) {
    fprintf(stderr, "Usage: beep [times]\n");
    return(-1);
  }
  if(argc==2)  times = atoi(argv[1]);
  else times=1;
  
  if (times > 30) times = 30; /* add some sanity */

  for(i=0;i<times;i++) {
    fprintf(stderr,"\aBEEP\n");
    if(i+1 < times)
      sleep(1);
  }
  return(0);
}


/* Enable/Disable echoing of data from terminal line. */
static int
do_echo(int argc, char *argv[])
{
  if (argc != 2) {
	fprintf(stderr, "Usage: echo on|off\n");
	return(-1);
  }
  
  if (strcmp(argv[1], "on") == 0) var_echo = 1;
    else if (strcmp(argv[1], "off") == 0) var_echo = 0;
    else {
	fprintf(stderr, "Usage: echo on|off\n");
	return(-1);
  }
	
  if (opt_v == 1) printf("Display modem output: %sabled\n",
				(var_echo == 0) ? "dis" : "en");
  return(0);
}
 

/* Go to some label in the script. */
static int
do_goto(int argc, char *argv[])
{
  char buff[1024];
  off_t oldpos, pos;
  char *label, *xp;
  char *sp;

  if (argc != 2) {
	fprintf(stderr, "Usage: goto label\n");
	return(-1);
  }
  if (scriptfp == stdin) {
	fprintf(stderr, "dip: GOTO not possible in TEST mode!\n");
	return(-1);
  }

#if 0 /* Cadach's change */
  label = argv[1];
  oldpos = ftell(scriptfp);
  rewind(scriptfp);
  (void) fflush(scriptfp);

  do {
        if (fgets(buff, 1024, scriptfp) == (char *)NULL) break;
	if ((sp = strchr(buff, '\n')) != (char *)NULL) *sp = '\0';
	sp = buff;
	while (*sp == ' ' || *sp == '\t') sp++;
	if (*sp == '#' || *sp == '\0') continue;
	if ((xp = strchr(sp, ':')) == (char *)NULL) continue;
	*xp = '\0';
	if (! strcmp(label, sp)) {
		oldpos = ftell(scriptfp);
		(void) fseek(scriptfp, oldpos, SEEK_SET);
		return(0);
	}
  } while(1);
  (void) fseek(scriptfp, oldpos, SEEK_SET);
  (void) fflush(scriptfp);
  return(-1);
#else
  if((pos = lookup_label(label = argv[1])) == (off_t)-1) {
    if(alllabels) {
BADLABEL:
      fprintf(stderr, "Label `%s' not found - skipped\n", label);
      return(-1);
    }
    oldpos = ftell(scriptfp);
    if(((pos = last_label()) != (off_t)-1) && (pos > oldpos))
      fseek(scriptfp, pos, SEEK_SET);		/* Continue search */
    do {
      if(fgets(buff, 1024, scriptfp) == (char *)NULL) {
        alllabels = 1;
        break;
      }
      if((sp = strchr(buff, '\n')) != (char *)NULL)
        *sp = '\0';
      sp = buff;
      while(*sp == ' ' || *sp == '\t')
        sp++;
      if(*sp == '#' || *sp == '\0')
        continue;
      if((xp = strchr(sp, ':')) == (char *)NULL)
        continue;
      *xp = '\0';
      if(add_label(sp, pos = ftell(scriptfp)) != 0)
        fprintf(stderr, "Duplicated label `%s' - ignored\n", sp);
      if(opt_v == 1)
        fprintf(stderr, "Got label `%s' at pos %08X\n", 
		sp, (unsigned int) pos);
      if(!strcmp(label, sp))
	return(0);
    } while(1);
    (void) fseek(scriptfp, oldpos, SEEK_SET);
    goto BADLABEL;
  }
  (void) fseek(scriptfp, pos, SEEK_SET);
  return(0);
#endif
}


/* Check some error (result) code. */
static int
do_if(int argc, char *argv[])
{
  char *cmd[3];
  char opcode;
  long val, var;
  int ret;
  struct variable *v;

  if (argc != 6) {
	fprintf(stderr, "Usage: if expr goto label\n");
	return(-1);
  };

  if (! strcmp(argv[2], "==")) opcode = '=';
    else if (! strcmp(argv[2], "!=")) opcode = '@';
    else if (! strcmp(argv[2], "<")) opcode = '<';
    else if (! strcmp(argv[2], ">")) opcode = '>';
    else if (! strcmp(argv[2], "<=")) opcode = 'L';
    else if (! strcmp(argv[2], ">=")) opcode = 'G';
    else {
	fprintf(stderr, "Syntax error: \"%s\" is not an opcode!\n", argv[2]);
	return(-1);
  }
  val = (long) atol(argv[3]);

  /* Now - do we really need this check? Here? */
  if (*argv[1] != '$') {
    fprintf(stderr, "Variable must lead with '$'!\n");
    return(-1);
  }

  if (! strcmp(argv[1], "$errlvl")) var = (long) errlevel;
    else if (! strcmp(argv[1], "$locip")) var = (long) mydip.loc_ip.s_addr;
    else if (! strcmp(argv[1], "$rmtip")) var = (long) mydip.rmt_ip.s_addr;
    else if ((v = lookup_hash(argv[1])) != (struct variable *)NULL)
      var = (long) v->value;
    else {
	fprintf(stderr, "Invalid variable \"%s\" !\n", argv[1]);
	return(-1);
  }

  ret = -1;
  switch(opcode) {
	case '=':	/* EQUAL */
		ret = (var == val);
		break;

	case '@':	/* NOT EQUAL */
		ret = (var != val);
		break;

	case '<':	/* LESS */
		ret = (var < val);
		break;

	case 'L':	/* LESS-EQ */
		ret = (var <= val);
		break;

	case '>':	/* GREATER */
		ret = (var > val);
		break;

	case 'G':	/* GREATER-EQ */
		ret = (var >= val);
		break;
  }

  if (strcmp(argv[4], "goto")) {
	fprintf(stderr, "Warning: keyword not \"goto\" !\n");
	argv[4] = "goto";
  }

  if (ret != 0) {
	cmd[1] = argv[4];
	cmd[1] = argv[5];
	cmd[2] = (char *)NULL;
	return(do_goto(2, cmd));
  }
  return(errlevel);
}

/* Set the shell command to be executed on program exit */

/* As things have evolved dramatically since the last time
 * I nosed around this code, I will do it in another fashion.
 * I'll just store the argv array and the argc count and we'll
 * pass'em over to do_shell upon the call of cleanup()
 */

/* I don't know which position will you prefer to put all these
 * functions, so I let them be together, so you find easier to put
 * them whereever you find they look prettier :)
 * The externs on main.c of onexit_argc, onexit_argv and
 * arg_clean() should be in main.c for use in cleanup()
 */

int onexit_argc = 0;
char **onexit_argv = NULL;    /* Warning! If not NULL, do_onexit will try to deallocate it!! */

/* Deallocate the given double array. argc is the number of sub-arrays */

void arg_clean (int argc, char **argv)
{
  int cnt;                          /* counter */
  if (argv != NULL && argc != 0)    /* previous data allocated? */
  {
    for (cnt = 0; cnt < argc; cnt++)    /* Ok, free old arg vectors */
      if (onexit_argv[cnt] != NULL)     /* if not NULL!! :*) */
        free (onexit_argv[cnt]);
  }
}

/* Allocate an string and print into there the argv's with spaces between
 * them. String's size is adjusted to the correct size!
 * free() the string after use!
 * Returns NULL on error
 * Remember! We have to skip argv[0], containing the function name
 */

char *argv_sprintf (int argc, char **argv)
{
  int  cnt;               /* counter */
  char *buf;              /* allocated string */
  int  size;              /* size of string */

  if (argv == NULL || argc == 0)  /* parameter error? */
  {
    buf = malloc (1);      /* allocate one char */
    if (buf)               /* ok? */
      *buf = 0;            /* set to the empty string */
    return buf;            /* return it */
  }

  /* Add up all the sizes to get the total string length
   * (mind one more for the space)
   */
  for (size = 0, cnt = 1; cnt < argc; cnt++)
    size += strlen (argv[cnt]) + 1;
  size++;                 /* this one is for the trailing NULL */

  buf = malloc (size);    /* allocate the string */
  if (buf == NULL)
  {
    fprintf (stderr, "error: Couldn't allocate %d bytes for"
                           " the onexit command string: `%s'\n",
             size, strerror (errno));
    return NULL;
  }

  /* Concatenate all the strings :) */

  *buf = 0;
  for (cnt = 1; cnt < argc; cnt++)
  {
     strcat (buf, argv[cnt]);   /* concatenate string #cnt */
     if (cnt != argc-1)         /* If this isn't the last one ... */
       strcat (buf, " ");       /* ... print an space to spare */
  }
  return buf;                  /* done ... return the pointer */
}

static int
do_onexit (int argc, char **argv)
{
  int  cnt;      /* counter */
  char **nargv;  /* temporary array */

  if (argc == 1 || argc == 0 || argv == NULL)   /* bad args? */
  {
    fprintf (stderr, "Usage: onexit command [parameters]\n");
    return -1;
  }

  arg_clean (onexit_argc, onexit_argv); /* remove old commands */
  onexit_argc = 0;                      /* and clean to ... */
  onexit_argv = NULL;                   /* ... avoid confusion */

  /* Ok, right now, the only thing we should do is to copy the argc and argv's
   * given into our array.
   */

  nargv = malloc (argc*sizeof(char*));   /* allocate the array of pointers */
  if (nargv == NULL)                     /* Ooops? */
  {
    fprintf (stderr, "error: Couldn't allocate %d bytes for"    /* Ooops! */
                           " onexit commands array: `%s'\n",
             argc*sizeof(char*), strerror (errno) );
    return -1;
  }

  for (cnt = 0; cnt < argc; cnt++)  /* clean (for arg_clean() to work ok) */
     nargv[cnt] = NULL;

  for (cnt = 0; cnt < argc; cnt++)  /* allocate and copy the argv[]'s */
  {
    nargv[cnt] = malloc (strlen(argv[cnt]) + 1);   /* allocate */
    if (nargv[cnt] == NULL)                        /* Oops? */
    {
      fprintf (stderr, "error: Couldn't allocate %d bytes for argument %d"
                             " of the onexit command: `%s'\n",
               strlen (argv[cnt]), cnt, strerror (errno) );
      arg_clean (argc, nargv);       /* clean */
      return -1;                     /* exit */
    }

    if (cnt == 0)
      strcpy (nargv[cnt], "shell");     /* copy the shell name */
	else
      strcpy (nargv[cnt], argv[cnt]);  /* copy the parameter name */
  }
  onexit_argv = nargv;    /* finished, copy the data */
  onexit_argc = argc;
  return 0;               /* and exit succesfully */
}

/* Print the contents of some variable. */
static int
do_print(int argc, char *argv[])
{
  char  *sp      = (char *)0;
  int    i       = 0;
  int    nonewln = 0;
  struct variable *var;
  char   *onexit_str;

  if (argc == 1) {
	printf("\n");
	return(0);
  }
  i = 0;
  if ((argv[1][0]=='-') &&
      (argv[1][1]=='n') &&
      (argv[1][2]==0))
    {
      i++;
      nonewln=1;
    }
  
  while (argv[++i] != (char *)NULL) {
    sp = argv[i];
    if (i != (1+nonewln)) printf(" ");
    if (*sp == '$') {
      if (! strcmp(++sp, "errlvl")) printf("%d", errlevel);
      else if (! strcmp(sp, "locip")) printf("%s",
					     inet_ntoa(mydip.loc_ip));
      else if (! strcmp(sp, "rmtip")) printf("%s",
					     inet_ntoa(mydip.rmt_ip));
      else if (! strcmp(sp, "local")) printf("%s", mydip.local);
      else if (! strcmp(sp, "remote")) printf("%s", mydip.remote);
      else if (! strcmp(sp, "mtu")) printf("%d", mydip.mtu);
      else if (! strcmp(sp, "modem")) printf("%s", var_modem);
      else if (! strcmp(sp, "onexit"))
      {
        onexit_str = argv_sprintf (onexit_argc, onexit_argv);
        if (onexit_str)         /* If no error, we print it; otherwise, */
        {                       /* arg_sprintf() has already complained */
          printf ("%s", onexit_str);
          free (onexit_str);
        }
      }
      else if (! strcmp(sp, "port")) printf("%s", var_port);
      else if (! strcmp(sp, "speed")) printf("%s", var_speed);
      else if (! strcmp(sp, "count")) printf("%d", var_dcount);
      else if ((var = lookup_hash(sp - 1)) == (struct variable *)NULL)
	fprintf(stderr, "Unknown variable %s\n", sp);
      else {
	if (var->type == 'i')
	  printf("%d", var->value);
	else
	  printf("%s", var->str_value);
      }
    } else printf("%s", sp);
  }
  if (!nonewln)
    printf("\n");
  return(0);
}


/* Send a string to the serial driver. */
static int
do_send(int argc, char *argv[])
{
  char *sp;
  char c;
  int i;
  
  if (argc < 2) {
	fprintf(stderr, "Usage: send words of text...\n");
	return(-1);
  }

  if (*argv[1] == '$') { /* We're dealing with variable! */
    struct variable *v;
    char t_buf[120];
    v = lookup_hash(argv[1]);
    if (v == (struct variable *)NULL) {
      fprintf(stderr, "Variable \"%s\" not found/defined.\n", argv[1]);
      return (-1);
    }
    memset(t_buf, '\0', sizeof(t_buf));
    switch (v->type) {
    case 'i': /* integer value */
      (void) sprintf (t_buf, "%i", v->value);
      break;
    case 's': /* ASCII string value */
      (void) strcpy (t_buf, v->str_value);
      break;
    default:
      /* wrong/unknown variable type! */
      return (-1);
    } /* end switch var-type */
    argv[1] = t_buf;
  } /* end of processing variable */
    

  for (i = 1; i < argc; i++) {
  	sp = argv[i];
  	while(*sp != '\0') {
		switch(*sp) {
			case '~':
				tty_putc('\r');
				tty_putc('\n');
				break;

			case '\\':
				sp++;
				if (isdigit(*sp)) {
				   c = atoi(sp);
				   while(isdigit(*sp))
				     sp++;
				} else
				 c = cvt_char(*sp);
				tty_putc((int) c);
				break;

			default:
				c = *sp;
				tty_putc((int) c);
		}
		sp++;
  	}
        if (i < (argc - 1)) tty_putc(' ');
  }
  tty_putc(-1);		/* flush the connection */
  return(0);
}


/* Send the output of a process to the serial driver */
static int
do_psend(int argc, char *argv[]) {
  pid_t pid;
  char *l, cmd[1024];
  int pipe_fd[2], i, status, ch;
  
  if (argc < 2) {
  	fprintf(stderr, "Usage: psend cmd args...\n");
  	return(-1);
  }

  l = cmd;
  for(i = 1; i < argc; ++i) {
  	*l++ = ' ';
  	l = stpcpy(l, argv[i]);
  }
  *l = '\0';

  /* We can't use popen because the process has to be executed without
   * special privileges
   */
  if (pipe(pipe_fd) < 0) {
  	perror("psend: pipe");
  	return(-1);
  }
  
  if (!(pid = fork())) {	/* This is the new child-process */
  	int fd;

  	dup2(pipe_fd[1], 1);
  	/* Close all open files, so that no unprivileged process can read/
  	 * write files we left open. Anyone using dip on a non-Posix-system?
  	 */
  	for (fd = sysconf(_SC_OPEN_MAX) - 1; fd >= 3; fd--) {
  		close(fd);
  	}
  	
  	setuid(getuid());
  	seteuid(getuid());

  	execl("/bin/sh", "sh", "-c", cmd, NULL);
  	_exit(127);
  }
  close(pipe_fd[1]);
  if (pid < 0) {	/* fork() failed */
  	close(pipe_fd[0]);
  	perror("psend: fork");
  	return(-1);
  }

  /* Now copy the output to the serial driver. This is obnoxiously
   * inefficient, but you just want to send short strings anyway, don't you?
   */
  while (read(pipe_fd[0], &ch, 1) == 1) {
  	tty_putc(ch);
  }
  tty_putc(-1);

  close(pipe_fd[0]);
  if (waitpid(pid, &status, 0) >= 0
  	&& WIFEXITED(status) && WEXITSTATUS(status)) {
  	fprintf(stderr, "dip:%s returned status %d\n", cmd,
  		WEXITSTATUS(status));
  	return(-1);
  }
  return(0);
}


/* Wait some time. */
static int
do_sleep(int argc, char *argv[])
{
  int secs;

  if (argc != 2) {
	fprintf(stderr, "Usage: sleep time_in_secs\n");
	return(-1);
  }

  get_value(&secs, argv[1]);
  (void) sleep(secs);
  return(0);
}

/* Execute a shell command using DIP variable substitution */
int
do_shell(int argc, char *argv[])
{
  char *usershell, *s;
  struct variable *var;
  char **subst_argv=0, *shell_argv[4];
  int i, status, ch_count=0;
  
  if (argc < 2) {
    fprintf(stderr, "Usage: shell command [parameters]\n");
    return(-1);
  }
  usershell = getenv("SHELL");
  if (!usershell)
  {
    fprintf(stderr,"Can't get SHELL environment variable\n");
    return(-1);
  }

  /* Parse the arguments, doing variable substitution and writing to a new
     array */
  subst_argv = (char **) malloc((argc-1)*sizeof(char *));
  if (!subst_argv) {
    fprintf(stderr,"Not enough memory for allocate variable\n");
    return(-1);
  }
  for(i=1;i<argc;i++) {
    if (*argv[i] == '$') {	/* DIP variable substitution */
      if (! strcmp(&argv[i][1], "errlvl")) {
	/* I guess 11 characters is long enough for ints. */
	if (!(subst_argv[i-1]=(char *)malloc(11*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	sprintf(subst_argv[i-1],"%d",errlevel);
      }
      else if (! strcmp(&argv[i][1], "locip")) {
	s = inet_ntoa(mydip.loc_ip);
	if (!(subst_argv[i-1]=(char *)malloc((strlen(s)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],s);
      }
      else if (! strcmp(&argv[i][1], "rmtip")) {
	s = inet_ntoa(mydip.rmt_ip);
	if (!(subst_argv[i-1]=(char *)malloc((strlen(s)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],s);
      }
      else if (! strcmp(&argv[i][1], "local")) {
	if (!(subst_argv[i-1]=
	      (char *)malloc((strlen(mydip.local)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],mydip.local);
      }
      else if (! strcmp(&argv[i][1], "remote")) {
	if (!(subst_argv[i-1]=
	      (char *)malloc((strlen(mydip.remote)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i],mydip.remote);
      }
      else if (! strcmp(&argv[i][1], "mtu")) {
	/* I guess 11 characters is long enough for ints. */
	if (!(subst_argv[i-1]=(char *)malloc(11*sizeof(char))))
	{
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	sprintf(subst_argv[i-1],"%d",mydip.mtu);
      }
      else if (! strcmp(&argv[i][1], "modem")) {
	if (!(subst_argv[i-1]=
	      (char *)malloc((strlen(var_modem)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],var_modem);
      }
      else if (! strcmp(&argv[i][1], "port")) {
	if (!(subst_argv[i-1]=
	      (char *)malloc((strlen(var_port)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],var_port);
      }
      else if (! strcmp(&argv[i][1], "speed")) {
	if (!(subst_argv[i-1]=
	      (char *)malloc((strlen(var_speed)+1)*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],var_speed);
      }
      else if (! strcmp(&argv[i][1], "onexit"))
      {
        subst_argv[i-1] = argv_sprintf (onexit_argc, onexit_argv);
        if (subst_argv[i-1] == NULL)  /* arg_sprintf() complains on error */
          return -1;
      }
      else if ((var = lookup_hash(&argv[i][1] - 1))
	       == (struct variable *)NULL) {
	fprintf(stderr, "Unknown variable %s - ignored\n", &argv[i][1]);
	if (!(subst_argv[i-1]=
	      (char *)malloc(strlen(argv[i])*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	strcpy(subst_argv[i-1],&argv[i][1]);
      }
      else {
	if (!(subst_argv[i-1]=(char *)malloc(11*sizeof(char)))) {
	  fprintf(stderr,"Not enough memory for allocate variable\n");
	  return(-1);
	}
	sprintf(subst_argv[i-1],"%d",var->value);
      }	
    }
    else if ((*argv[i]=='\\') && (argv[i][1]=='$')) {
      if (!(subst_argv[i-1]=
	    (char *)malloc(strlen(argv[i])*sizeof(char)))) {
	fprintf(stderr,"Not enough memory for allocate variable\n");
	return(-1);
      }
      strcpy(subst_argv[i-1],&argv[i][1]);
    }
    else { /* ordinary parameter */
      if (!(subst_argv[i-1]=
	    (char *)malloc((strlen(argv[i])+1)*sizeof(char))))
      {
	fprintf(stderr,"Not enough memory for allocate variable\n");
	return(-1);
      }
      strcpy(subst_argv[i-1],argv[i]);
    }
    ch_count += strlen(subst_argv[i-1]) + 1;
  }
  /* create a new argv array to pass to execv */
  shell_argv[0] = (char *) malloc((strlen(usershell)+1)*sizeof(char));
  if (!shell_argv[0]) {
    fprintf(stderr,"Not enough memory for allocate variable\n");
    return(-1);
  }
  strcpy(shell_argv[0],usershell);
  shell_argv[1] = (char *) malloc(3*sizeof(char));
  if (!shell_argv[1]) {
    fprintf(stderr,"Not enough memory for allocate variable\n");
    return(-1);
  }
  strcpy(shell_argv[1],"-c");
  shell_argv[2] = (char *) malloc((ch_count+1)*sizeof(char));
  if (!shell_argv[2]) {
    fprintf(stderr,"Not enough memory for allocate variable\n");
    return(-1);
  }
  shell_argv[2][0] = 0;
  for(i=0;i<argc-1;i++) {
    strcat(shell_argv[2], subst_argv[i]);
    if (i<argc-2) strcat(shell_argv[2], " ");
  }
  shell_argv[3] = NULL;
  /* now execute the command using execv */
  if (!fork()) { /* child process */
    seteuid(getuid());		/* don't execute as root! */
    execv(usershell, shell_argv);
    exit(0);
  }
  else { /* parent process */
    wait(&status);
  }
  /* free memory we just allocated */
  for(i=0;i<argc-1;i++)
     free(subst_argv[i]);
  free(subst_argv);
  free(shell_argv[0]);
  free(shell_argv[1]);
  free(shell_argv[2]);
  return(0);
}

/* Flush tty input */
static int
do_flush(int argc, char *argv[])
{
  if (argc != 1) {
	fprintf(stderr, "Usage: flush\n");
	return(-1);
  }

  tty_flush();
  return(0);
}


/* Wait for some string to arrive. */
static int
do_wait(int argc, char *argv[])
{
  char c, c2, *p;
  int howlong;
  void (*oldsig)(int);

  if (argc == 1 || argc > 3) {
	fprintf(stderr, "Usage: wait text [timeout_value | variable]\n");
	return(-1);
  }

  if (argc == 3)
	get_value (&howlong, argv[2]);
  else 
	howlong = 0;

  if (opt_v) {
    syslog(LOG_INFO, "waiting for \"%s\" for %d sec's.",
	   argv[1], howlong);
  }

  oldsig = signal(SIGALRM, TimeOut);
  (void) alarm(howlong);

  p = argv[1];
  timeout = 0;
  while((!timeout) && (*p != '\0')) {
#ifdef NE_PAUL /* Paul Cadach */
	c = (char) tty_getc();
#else
	while (((howlong = tty_getc()) == -2) && !timeout);
	if ((howlong == -1) || timeout) {
	  timeout = 1;
	  break;
	}
	c = (char) howlong;
#endif
	c &= 0177;
	if ((var_echo == 1) || (opt_v == 1)) {
		fputc(c, stdout);
		fflush(stdout);
	}
	if (timeout == 1) break;
	if (*p == '\\') c2 = cvt_char(*++p);
	  else c2 = *p;
	if (c2 != c) p = argv[1];
	  else p++;
  }
  (void) alarm(0);
  (void) signal(SIGALRM, oldsig);

  if (opt_v == 1) {
     if (timeout == 1)
	syslog(LOG_INFO, "do_wait: timeout...");
  }

#ifdef NE_PAUL
  return((timeout == 1) ? -1 : 0);
#else
  return((timeout == 1) ? 3 : 0);
#endif
}


/* Show some help. */
static int
do_help(int argc, char *argv[])
{
  extern struct commands commands[];
  int i, j;

  i = 0; j = 0;
  printf("DIP knows about the following commands:\n\n");
  while (commands[i].name != (char *)NULL) {
	if (j++ == 0) printf("\t");
	printf("%-8.8s ", commands[i].name);
	if (j == 5) {
		printf("\n");
		j = 0;
	}
	i++;
  }
  if (j != 0) printf("\n\n");
    else printf("\n");
  return(0);
}


/************************************************************************
 *									*
 *		Modem Handling and Dialing Commands			*
 *									*
 ************************************************************************/


/* Set the name of the terminal port to use. */
static int
do_port(int argc, char *argv[])
{
  if (argc != 2) {
	fprintf(stderr, "Usage: port tty_name\n");
	return(-1);
  }
  if (var_port != NULL) {
    if (opt_v)
	syslog(LOG_ERR, "PORT: terminal port already set to \"%s\".\n",
	       var_port);
    fprintf(stderr, "PORT: terminal port already set to \"%s\".\n",
            var_port);
    return(-1);
  }

  var_port = strcpy(var_port_buff, argv[1]);
  if (opt_v == 1) printf("PORT: terminal port set to \"%s\".\n", var_port);

  /* Initialize the terminal line. */
  if (tty_open(var_port) < 0) {
    bzero(var_port, sizeof(var_port));
    var_port = NULL;
#if 1
    exit(1);
#else
    return(-1);
#endif
  }

  return(0);
}


/* Set the correct DATABITS to use. */
static int
do_databits(int argc, char *argv[])
{
  if (argc != 2) {
        fprintf(stderr, "Usage: databits bits\n");
        return(-1);
  }
  if (var_port == NULL) { 
        fprintf(stderr, "Please set PORT first.\n");
        return(-1);
  }  
  return(tty_databits(argv[1]));
}


/* Define a modem "INIT" string. */
static int
do_init(int argc, char *argv[])
{
  if (argc != 2) {
	fprintf(stderr, "Usage: init <init string>\n");
	return(-1);
  }
  return(mdm_init(argv[1]));
}


static int
do_dial(int argc, char *argv[])
{
  int timeout;

  if ((argc != 2) && (argc != 3)) {
	fprintf(stderr, "Usage: dial telno [<timeout>]\n");
	return(-1);
  }

  if (var_modem == NULL) {
	fprintf(stderr, "Please set MODEM first.\n");
	return(-1);
  }

  if (argc == 3)
     get_value(&timeout, argv[2]);
  else
     timeout = 60;
  var_dcount++;
  return(mdm_dial(argv[1], timeout));
}


/* Set the name of the modem we want to use. */
static int
do_modem(int argc, char *argv[])
{
  if (argc != 2) {
	fprintf(stderr, "Usage: modem modem_name\n");
	return(-1);
  }
  if (var_modem != NULL) {
	fprintf(stderr, "MODEM: modem already set to \"%s\".\n", var_modem);
	return(-1);
  }
  var_modem = strncpy(var_modem_buff, argv[1], sizeof(var_modem_buff));
  if (opt_v == 1) printf("MODEM: modem set to \"%s\".\n", var_modem);

  /* Initialize this modem. */
  if (mdm_modem(var_modem) < 0) {
	var_modem = NULL;
	return(-1);
  }

  return(0);
}


static int
do_break(int argc, char *argv[])
{
    tty_sendbreak();
    return(0);
}

static int
do_parity(int argc, char *argv[])
{
  if (argc != 2) {
        fprintf(stderr, "Usage: parity E/O/N\n");
        return(-1);
  }

  if (var_port == NULL) {
        fprintf(stderr, "Please set PORT first.\n");
        return(-1);
  }
  return(tty_parity(argv[1]));
}


/* Reset the modem. */
static int
do_reset(int argc, char *argv[])
{
  if (argc != 1) {
	fprintf(stderr, "Usage: reset\n");
	return(-1);
  }

  /* Did we get a modem to work with? */
  if (var_modem == NULL) {
	fprintf(stderr, "Please set MODEM first.\n");
	return(-1);
  }

  /* Reset the modem. */
  (void) mdm_reset();
  return(0);
}


/* Set the correct SPEED to use. */
static int
do_speed(int argc, char *argv[])
{
  int ret_stat;

  if (argc != 2) {
	fprintf(stderr, "Usage: speed baudrate\n");
	return(-1);
  }
  if (var_port == NULL) {
	fprintf(stderr, "Please set PORT first.\n");
	return(-1);
  }
  var_speed = strcpy(var_speed_buff, argv[1]);
  ret_stat = tty_speed(var_speed);
  if ( ret_stat < 0 ) {
           fprintf(stderr,
                   "speed=%s not supported, reverting to default\n",
                   var_speed);
           var_speed = DEF_SPEED;
           ret_stat = tty_speed(var_speed);
   }
   return(ret_stat);
}


/* Set the correct STOPBITS to use. */
static int
do_stopbits(int argc, char *argv[])
{
  if (argc != 2) {
        fprintf(stderr, "Usage: stopbits bits\n");
        return(-1);
  }
  if (var_port == NULL) {
        fprintf(stderr, "Please set PORT first.\n");
        return(-1);
  }
  return(tty_stopbits(argv[1]));
} 


/* Enter a TERMINAL mode. */
static int
do_term(int argc, char *argv[])
{
  if (argc != 1) {
	fprintf(stderr, "Usage: term\n");
	return(-1);
  }
  if (var_port == NULL) {
	fprintf(stderr, "Please set PORT first.\n");
	return(-1);
  }
  do_terminal();
  return(0);
}


/************************************************************************
 *									*
 *		Connection Setup Commands				*
 *									*
 ************************************************************************/

/*
 * Get or ask for the value of a variable.
 * Get local IP#, Remote IP# and MTU
 */

#define VAR_IP		1
#define VAR_NUM		2
#define VAR_STR		3

static int
do_get(int argc, char *argv[])
{
  char str[128] = "";
  int var = 0;
  void *v1 = NULL, *v2 = NULL;
  void (*oldsig)(int);
  struct variable *v = NULL;
 
  if (argc <= 2 || argc > 4 || argv[1][0] != '$') {
    fprintf(stderr,
	    "Usage: get $variable [value | ask | remote [timeout_value | variable]]\n");
    return(-1);
  }
   
  /*
   * Check for special variables
   */
  if(*argv[1] != '$') {
    fprintf(stderr, "Variable must lead with '$'\n");
    return(-1);
  }

  if (! strcmp(argv[1], "$locip") || !strcmp(argv[1], "$local")) {
	v1 = (void *) mydip.local;
	v2 = (void *) &mydip.loc_ip;
	var = VAR_IP;
  } else if (! strcmp(argv[1], "$rmtip") || !strcmp(argv[1], "$remote")) {
	v1 = (void *) mydip.remote;
	v2 = (void *) &mydip.rmt_ip;
	var = VAR_IP;
  } else if (! strcmp(argv[1], "$mtu")) {
	v1 = (void *) &mydip.mtu;
	var = VAR_NUM;
  } else { /* a new variable */
     if((v = add_hash(argv[1])) == (struct variable *)NULL)
       return(-1);
     v->type = 'i'; /* jus in case, fill with integer */
     v1 = (void *) &v->value;
     var = VAR_NUM;
  }
 
  if (argc > 2) {
    if (! strcmp(argv[2], "ask")) {
      /* Ask the stdinput for the value of the variable. */
      register char *sp;
      fprintf(stderr, "Enter the value for %s: ", argv[1]);
      if (fgets(str, 128, stdin) == (char *)NULL) {
	return(1);
      }
      if ((sp = strchr(str, '\n')) != (char *)NULL) *sp = '\0';
      if (is_a_num(str) == 0) { /* if String value */
	var = VAR_STR;
	v->type  = 's';
	v->value = 0;
	v->str_value = calloc(1, strlen(str) + 2);
	if (v->str_value == NULL)
	  return (-1);
	strcpy(v->str_value, str);
	return 0;
      }
    } else if (! strcmp(argv[2],"remote")) {
      /* Get the variable string from the "remote" line */
      register char c, *p;
      int howlong = 0, state = 0;
      
      if (argc == 4) get_value (&howlong, argv[3]);
      oldsig = signal(SIGALRM, TimeOut);
      (void) alarm(howlong);
      
      p = str;
      timeout = 0;
      state = 0;
      if (var == VAR_IP || var == VAR_NUM) state = 1;
      while(!timeout && (state >= 0)) {
	while (((howlong = tty_getc()) == -2) && !timeout);
	if ((howlong == -1) || timeout) {
	  timeout = 1;
	  break;
	}
	c = (char) howlong;
	c &= 0177;
#ifdef NE_PAUL
	if (timeout == 1) break;
#endif
	switch (state) {
	case 0: /* throw away "white space" */
	  if (! strchr(" \n\t", c)) break;
	  if (var == VAR_STR) {
	    *(p++) = c;
	    state = 10;
	    break;
	  }
	  state = 1;
	  
	case 1:
	  if ( c >= '0' && c <= '9' ) {
	    *(p++) = c;
	    if (var == VAR_IP) {
	      state = 2;
	      break;
	    } else state = 5;
	    break;
	  }
	  break; 
	  
	case 2:
	case 3:
	case 4:
	  if ((c >= '0' && c <= '9') || c == '.') {
	    *(p++) = c;
	    if (c == '.') state++;
	    break;
	  } 
	  p = str;
	  state = 0;
	  break;
	case 5:
	  if (c >= '0' && c <= '9') {
	    *(p++) = c;
	    break;
	  } 
	  state = -1;
	  break;
	  
	case 10:
	  if (strchr(" \n\t", c)) state = -1;
	  else *(p++) = c;
	  break;
	  
	default:
	  break;
	}
      }
      *p = '\0';
      (void) alarm(0);
      (void) signal(SIGALRM, oldsig);
      if (timeout == 1) return (1);
    } else {
      /* The third argv is the value for the variable. */
      strncpy(str, argv[2], 128);
      if ((is_a_num(str) == 0) && (var != VAR_IP)) {
	var = VAR_STR;
	v->type = 's';
      }
    }
  }
  
  if (opt_v == 1) printf("About to set variable %s to %s\n", argv[1], str);
  
  if (*str) {
    switch (var) {
    case VAR_IP:
      {
	struct hostent *hp;
	hp = gethostbyname(str);
	if (hp == (struct hostent *)NULL) {
	  herror(str);
	  return(-1);
	}
	strncpy((char *) v1, hp->h_name, 128);
	memcpy((char *) v2, (char *) hp->h_addr_list[0],
	       hp->h_length);
      }
      break;
      
    case VAR_NUM:
      *(int *) v1 = atoi(str);
      break;
      
    case VAR_STR:
      v->type = 's'; /* var type is String */
      v->str_value = malloc(strlen(str) + 2);
      if (v->str_value == NULL)
	return(0);
      strcpy(v->str_value, str);
      break;
      
    default:
      break;
    }
  }
  
  return(0);
}


/* Enter a specific protocol. */
static int
do_mode(int argc, char *argv[])
{
  register int i=0;

  if (argc != 2) {
	fprintf(stderr, "Usage: mode protocol_name\n");
	return (-1);
  }
	
  if ((i = get_prot(argv[1])) == 0) {
	fprintf(stderr, 
		"protocol_name \"%s\" not found (rc=%d)!\n", 
		argv[1], i);
	return(-1);
  }

  if ((mydip.rmt_ip.s_addr == INADDR_ANY) &&
      (strcasecmp(argv[1], "PPP") != 0) )
    {
	fprintf(stderr, "Please set REMOTE first.\n");
	return(-1);
    }
  mydip.protonr = i;

  /* Enter BACKGROUND mode here! */
  (void) dip_daemon(&mydip);

  return(i);
}

/* Quit upon error - reset the tty to the proper ldisc */
static int
do_quit(int argc, char *argv[])
{
  (void) tty_close();
  exit(1);
}

/* Store the netmask in "mydip" structure */
static int
do_netmask(int argc, char *argv[])
{
   if (argc != 2) {
      fprintf(stderr, 
              "Usage of this command: netmask xxx.xxx.xxx.xxx\n");
      return(-1);
   }
   strcpy(mydip.netmask, argv[1]);
   return(0);
}

/* Set routing as default */
static int
do_default(int argc, char *argv[])
{
  if (argc != 1) {
	fprintf(stderr, "Usage: default\n");
	return(-1);
  }
  if (opt_v == 1) 
    printf("Destination net/address set to 'default'\n");

  mydip.rtdefault = 1;
  return(0);
}


/* Request Proxy ARP to be set */
static int do_proxyarp(int argc, char *argv[])
{
  if (argc != 1) {
        fprintf(stderr, "Usage: proxyarp\n");
        return(-1);
  }
  if (opt_v == 1) printf("Setting Proxy ARP entry...\n");

  mydip.proxyarp = 1;
  return(0);
}

/* Set timeout */
static int
do_timeout(int argc, char *argv[])
{
  if (argc != 2) {
    fprintf(stderr, "Usage: timeout <# of seconds>\n");
    return(-1);
  }
  
  mydip.timeout = atoi(argv[1]);

  if (mydip.timeout < 0) {
    printf("Silly! Use reasonable timeout, for your own sake!\n");
    mydip.timeout = 0;
  }

  if (opt_v == 1) 
    printf("Timeout set to %d seconds\n", mydip.timeout);
  
  return(0);
}


/************************************************************************
 *									*
 *		        Password Handling        			*
 *									*
 ************************************************************************/


/* Prompt for a password to be sent */
static int
do_password(int argc, char *argv[])
{
    char	*prompt = "Password:";
    char	*pass[3];
    
    pass[0] = "send";		/* in case anyone cares */
    pass[2] = NULL;		/* ditto */
    
    if (argc > 1)
	prompt = argv[1];

    pass[1] = getpass(prompt);
    do_send(2, &pass[0]);
    return(0);
}

#ifdef SECUREID
/* The SecureID system works as follows:

   - there is a fixed-part: a user-defined alphanumeric string
   that must be at least 4 characters long (max 8).  This value was
   set when you filled in the form and send it into the Network
   Admin people. The special variable '$secureidfixed' is
   used to hold this information.

   - there is a random-part: a 6 digit (max 8) string that
   changes every minute, generated by the ACE System SecureID
   card.  The user has the card, while the Network Admin people
   have the SecureID module installed on the Xyplex Annex
   terminal server - the two devices must be synchronized
   with each other.

   When the user dials in, the system requires the user
   to enter the 2 parts CONCATENATED together so that
   there is an ever-changing (minimum 10 character) password.

   Usage: secureid <random_part>
     This command will prompt the user for the current
     random-part, concatenate the two parts together,
     and send the resulting string to the terminal server.

   History:
   ========
   94/11/16 mwnorman@foobar.ocunix.on.ca: creation.

   */

static int
do_securidfixed(int argc, char *argv[])
{

    if (argc != 2) {
	fprintf(stderr, "Usage: securidfixed <fixed_part>\n");
	return(-1);
    }	
    memset(var_securidfixed,'\0',sizeof(var_securidfixed));
    if (opt_v == 1) {
        printf("About to set securidfixed to %s\n", argv[1]);
    }
    strcpy(var_securidfixed,argv[1]);
    return(0);
}

static int
do_securid(int argc, char *argv[])
{
    register char   *sp;
    char	    buf[18];

    if (argc > 1) {
	fprintf(stderr, "Usage: securid \n");
	return(-1);
    }
    /* clear and initialize buffers */
    memset(buf,'\0',sizeof(buf));
    if(strlen(var_securidfixed) == 0) {
        if (opt_v == 1) {
            printf("securidfixed not set. Hope it's OK...\n");
        }
        sp = buf;
    } else {
        if (opt_v == 1) {
            printf("found securidfixed = %s\n",var_securidfixed);
        }
        strcpy(buf,(const char *)var_securidfixed);
        sp = strchr(buf,'\0');
    }

    /* Ask from stdin for the value of the random-part of the secureid */
    fprintf(stderr, "Enter the value for securid (random part): ");
    if (fgets(sp, 8, stdin) == (char *)NULL) {
        return(2);
    }

    if (opt_v == 1) {
        printf("Sending securid = %s",buf);
    }
    strcat(buf,"\r");

    tty_puts(buf);
    tty_putc(-1);        /* flush the connection */
    return(0);

}
#endif /* SECUREID */

#ifdef SKEY
/* scan the input stream for an S/Key challenge, prompt
   the user for their secret password, generate the response
   and send it to the remote host - simple....

   Parts of this code are based on skey.c in the S/Key package */

static int
do_skey(int argc, char *argv[])
{
    char	buf[256];
    char	*p = buf;
    char	*sp;
    char	c;
    void	(*oldsig)(int);
    int		howlong = 0;
    int		challenge;
    char	*seed, passwd[256], key[8];
    char	response[33];
    char	*param[4];

    if (argc > 2) {
	fprintf(stderr, "Usage: skey [timeout | variable]\n");
	return(-1);
    }

    if (argc == 2) get_value (&howlong, argv[1]);
    oldsig = signal(SIGALRM, TimeOut);
    (void) alarm(howlong);
    timeout = 0;

    while (!timeout) {
#ifdef NE_PAUL
	c = (char) tty_getc();
#else
	while(((howlong = tty_getc()) == -2) && !timeout);
        if((howlong == -1) || timeout) {
          timeout = 1;
          break;
        }
	c = (char) howlong;
#endif
	c &= 0177;
#ifdef NE_PAUL
	if (timeout) break;
#endif

	if (c == '\r') {
	    *p = '\0';
	    if (strncmp(buf, SKEY_CHALLENGE, strlen(SKEY_CHALLENGE)) == 0)
		break;
	    else
		p = buf;
	}
	else if (c != '\n') { /* just add the char to the buffer */
	    *p++ = c;
	}
    }

    (void) alarm(0);
    (void) signal(SIGALRM, oldsig);
    if (timeout == 1) return (1);

    if (opt_v == 1) printf("Challenge string <%s>\n", buf);

    p = buf + strlen(SKEY_CHALLENGE);
    while (*p && (!isdigit((int) *p))) p++;
    sp = p;
    while (*p && isdigit((int) *p)) p++;
    *p++ = '\0';
    challenge = atoi(sp);

    while (*p && isspace((int) *p)) p++;
    seed = p;
    while (*p && (!isspace((int) *p)) && (*p != ']')) p++;
    *p = '\0';

    printf ("Enter secret password: ");
    readpass (passwd, sizeof (passwd));

    /* Crunch seed and password into starting key */
    if (keycrunch (key, seed, passwd) != 0) {
	fprintf (stderr, "%s: key crunch failed\n", argv[0]);
	return(1);
    }

    while (challenge-- != 0)
	f (key);

    (void) btoe (response, key);

    /* build up our command for do_send */
    param[0] = "send";
    param[1] = response;
    param[2] = "\r";
    param[3] = NULL;

    return(do_send(3, &param[0]));
}
#endif /* SKEY */
 


/* Store interface configuration parameters */
static int
do_config(int argc, char *argv[])
{
  struct ifcfg c, *s;
  char buff[1024], *l;
  int i;

  static struct keyword types[] = {
    { "interface",  IFC_CONFIG },
    { "routing",    IFC_ROUTE  },
    { (char *)NULL, 0          }
  };

  static struct keyword whens[] = {
    { "pre",        IFC_PRE  },
    { "up",         IFC_UP   },
    { "down",       IFC_DOWN },
    { "post",       IFC_POST },
    { (char *)NULL, 0        }
  };
 
  if((argc < 3) || ((c.type = getkeyword(argv[1], types)) == -1) ||
     ((c.when = getkeyword(argv[2], whens)) == -1))
  {
    fprintf(stderr, 
    "Usage: config [interface|routing] [pre|up|down|post] {<arguments>...}\n");
    return(-1);
  }
  
  /* To enable "config" command - change the following to #if 0 */
#if 1 
  fprintf(stderr, "This command was disabled for security reasons.\n");
  fprintf(stderr, "If you wish to enable it - edit the source for \n");
  fprintf(stderr, "\"command.c\" and recompile the DIP. Realize,  \n");
  fprintf(stderr, "that possible security problems will be on \n");
  fprintf(stderr, "own head. Enjoy!\n");
  return (-1);
#endif

  l = buff;
  for(i = 3; i < argc; ++i) {
    *l++ = ' ';
    l = stpcpy(l, argv[i]);
  }
  *l = '\0';
  c.param = strdup(buff);

  if(cfg == (struct ifcfg *)NULL) {
    cfg = (struct ifcfg *)malloc(sizeof(struct ifcfg));
    c.next = c.prev = cfg;
    memcpy(cfg, &c, sizeof(struct ifcfg));
  } else {
    s = cfg->prev;
    c.next = cfg;
    c.prev = s;
    cfg->prev = s->next = (struct ifcfg *)malloc(sizeof(struct ifcfg));
    memcpy(s->next, &c, sizeof(struct ifcfg));
  }
  if(opt_v == 1)
    fprintf(stderr, 
            "Saving configuration information for %s(%s), arguments '%s'\n",
            lookkeyword(c.type, types), lookkeyword(c.when, whens), c.param + 1);
  return(0);
}

static int
do_inc(int argc, char *argv[])
{
  struct variable *v;
  int increment = 1;
  
  if((argc < 2) || (argc > 3)) {
    fprintf(stderr, "Usage: inc $variable [<increment value>|variable]\n"
                    "       default <increment value> is 1\n");
    return(-1);
  }
  
  if(*argv[1] != '$') {
    fprintf(stderr, "Variable must lead with '$'\n");
    return(-1);
  }
  
  /*
   * Check for special variables
   */
  if(is_special(argv[1])) {
    fprintf(stderr, "Cannot increment special variable %s!\n", argv[1]);
    return(-1);
  }
  
  if((v = lookup_hash(argv[1])) == (struct variable *)NULL) {
    fprintf(stderr, "Variable %s not defined!\n", argv[1]);
    return(-1);
  }
  
  if(argc == 3)
    get_value(&increment, argv[2]);
  
  if(opt_v == 1)
    printf("Incrementing variable %s for %d\n", argv[1], increment);
  
  v->value += increment;
  return(0);
}

static int
do_dec(int argc, char *argv[])
{
  struct variable *v;
  int decrement = 1;
  
  if((argc < 2) || (argc > 3)) {
    fprintf(stderr, "Usage: dec $variable [<decrement value>|variable]\n"
                    "       default <decrement value> is 1\n");
    return(-1);
  }
  
  if(*argv[1] != '$') {
    fprintf(stderr, "Variable must lead with '$'\n");
    return(-1);
  }
  
  /*
   * Check for special variables
   */
  if(is_special(argv[1])) {
    fprintf(stderr, "Cannot decrement special variable %s!\n", argv[1]);
    return(-1);
  }
  
  if((v = lookup_hash(argv[1])) == (struct variable *)NULL) {
    fprintf(stderr, "Variable %s not defined!\n", argv[1]);
    return(-1);
  }
  
  if(argc == 3)
    get_value(&decrement, argv[2]);
  
  if(opt_v == 1)
    fprintf(stderr, "Decrementing variable %s for %d\n", argv[1], decrement);
  
  v->value -= decrement;
  return(0);
}

static int
do_chatkey(int argc, char *argv[])
{
  char buf[128];
  char *s, *d;
  unsigned char *fu;
  int code = 0;
  int buf_len = sizeof(buf);
  
  if ((argc != 3) && (argc != 2)) {
	fprintf(stderr, "Usage: chatkey <keyword> [<code>]\n");
	return(-1);
  }
  
  s = argv[1];
  d = buf;
  
  do  {
    if((*s == '\\') && (*(s + 1) != '\0'))
      *d++ = cvt_char(*++s);
    else
      *d++ = *s;
  } while(*s++ && (--buf_len > 0));

  if (buf_len <= 0) { 
    /* Somebody clever tried to override our stack? */
    syslog(LOG_INFO, "Attempt to use Vader's chatkey! uid=%d",
	   getuid());
    return -1;
  }

  if(argc == 3) {
    fu = (unsigned char *) argv[2];
    do { 
      if (isdigit(*fu) == 0) { /* if not a digit */
	/* Somebody trying to fool us? */
	fprintf(stderr, "Vader's numbers not permitted\n");
	syslog(LOG_INFO, "Attempt to use Vader's numbers caught! uid=%d",
	       getuid());
	return -1;
      }
      fu++;
    } while ( *fu);
    code = atoi(argv[2]);
  } else {
   code = 2;
  }
    
  dip_addchat(&chat, buf, code);
    
  if (opt_v == 1)
    fprintf(stderr, 
           "Added chat response keyword '%s' with code '%d'\n",
           buf, code);
  return(0);
}
 
#define int32 unsigned long
#define int16 unsigned short

struct bootp {
	char	op;			/* packet opcode type */
	char	htype;			/* hardware addr type */
	char	hlen;			/* hardware addr length */
	char	hops;			/* gateway hops */
	int32	xid;			/* transaction ID */
	int16	secs;			/* seconds since boot began */
	int16	unused;
	int32	ciaddr;		/* client IP address */
	int32	yiaddr;		/* 'your' IP address */
	int32	siaddr;		/* server IP address */
	int32	giaddr;		/* gateway IP address */
	char	chaddr[16];		/* client hardware address */
	char	sname[64];		/* server host name */
	char	file[128];		/* boot file name */
	char	vend[64];		/* vendor-specific area */
};
#define BOOTREQUEST		1
#define BOOTREPLY		2
#define	IPPORT_BOOTPS		67
#define	IPPORT_BOOTPC		68

/* SLIP protocol characters. */
#define END             0300		/* indicates end of frame	*/
#define ESC             0333		/* indicates byte stuffing	*/
#define ESC_END         0334		/* ESC ESC_END means END 'data'	*/
#define ESC_ESC         0335		/* ESC ESC_ESC means ESC 'data'	*/

static unsigned char packet_buff[512];
static unsigned char *bufpt;
static unsigned char *bufend;

static void put_buf(unsigned char c)
{
  switch(c) {
  case END:
    *bufpt++ = ESC;
    *bufpt++ = ESC_END;
    break;
  case ESC:
    *bufpt++ = ESC;
    *bufpt++ = ESC_ESC;
    break;
  default:
    *bufpt++ = c;
  }
}

/* This is a version of ip_compute_csum() optimized for IP headers, which
   always checksum on 4 octet boundaries. */
static inline unsigned short
ip_fast_csum(unsigned char * buff, int wlen)
{
    unsigned long sum = 0;
    if (wlen) {
    	unsigned long bogus;
	 __asm__("clc\n"
		"1:\t"
		"lodsl\n\t"
		"adcl %3, %0\n\t"
		"decl %2\n\t"
		"jne 1b\n\t"
		"adcl $0, %0\n\t"
		"movl %0, %3\n\t"
		"shrl $16, %3\n\t"
		"addw %w3, %w0\n\t"
		"adcw $0, %w0"
	    : "=r" (sum), "=S" (buff), "=r" (wlen), "=a" (bogus)
	    : "0"  (sum),  "1" (buff),  "2" (wlen));
    }
    return (~sum) & 0xffff;
}

static unsigned short
udp_check(struct udphdr *uh, int len,
	  unsigned long saddr, unsigned long daddr)
{
  unsigned long sum;

  __asm__("\t addl %%ecx,%%ebx\n"
	  "\t adcl %%edx,%%ebx\n"
	  "\t adcl $0, %%ebx\n"
	  : "=b"(sum)
	  : "0"(daddr), "c"(saddr), "d"((ntohs(len) << 16) + IPPROTO_UDP*256)
	  : "cx","bx","dx" );

  if (len > 3) {
	__asm__("\tclc\n"
		"1:\n"
		"\t lodsl\n"
		"\t adcl %%eax, %%ebx\n"
		"\t loop 1b\n"
		"\t adcl $0, %%ebx\n"
		: "=b"(sum) , "=S"(uh)
		: "0"(sum), "c"(len/4) ,"1"(uh)
		: "ax", "cx", "bx", "si" );
  }

  /* Convert from 32 bits to 16 bits. */
  __asm__("\t movl %%ebx, %%ecx\n"
	  "\t shrl $16,%%ecx\n"
	  "\t addw %%cx, %%bx\n"
	  "\t adcw $0, %%bx\n"
	  : "=b"(sum)
	  : "0"(sum)
	  : "bx", "cx");

  /* Check for an extra word. */
  if ((len & 2) != 0) {
	__asm__("\t lodsw\n"
		"\t addw %%ax,%%bx\n"
		"\t adcw $0, %%bx\n"
		: "=b"(sum), "=S"(uh)
		: "0"(sum) ,"1"(uh)
		: "si", "ax", "bx");
  }

  /* Now check for the extra byte. */
  if ((len & 1) != 0) {
	__asm__("\t lodsb\n"
		"\t movb $0,%%ah\n"
		"\t addw %%ax,%%bx\n"
		"\t adcw $0, %%bx\n"
		: "=b"(sum)
		: "0"(sum) ,"S"(uh)
		: "si", "ax", "bx");
  }

  /* We only want the bottom 16 bits, but we never cleared the top 16. */
  return((~sum) & 0xffff);
}

static void put_bootreq(void)
{
  struct iphdr ip;
  struct udphdr udp;
  struct bootp bootp;
  unsigned char *cp;
  int i;

  bufpt = packet_buff;
  *bufpt++ = END;

/* IP header */
  ip.ihl = 5;
  ip.version = 4;
  ip.tos = 0;
  ip.tot_len = htons(sizeof(struct iphdr) + sizeof(struct udphdr) +
		     sizeof(struct bootp));
  ip.id = 1;
  ip.frag_off = 0;
  ip.ttl = 32;
  ip.protocol = 17;
  ip.check = 0;
  ip.saddr = 0;
  ip.daddr = INADDR_BROADCAST;
  ip.check = ip_fast_csum((unsigned char *)&ip, 5);
  for (cp = (unsigned char *)&ip, i = sizeof(struct iphdr); i > 0; i--)
    put_buf(*cp++);

/* UDP header */
  udp.source = 	htons(IPPORT_BOOTPC);
  udp.dest = htons(IPPORT_BOOTPS);
  udp.len = htons(sizeof(struct udphdr) + sizeof(struct bootp));
  udp.check = 0;
  for (cp = (unsigned char *)&udp, i = sizeof(struct udphdr); i > 0; i--)
    put_buf(*cp++);

/* bootp request */
  memset(&bootp, 0, sizeof(bootp));
  bootp.op = BOOTREQUEST;
  bootp.htype = 1;
  bootp.hlen = 6;
  for (cp = (unsigned char *)&bootp, i = sizeof(struct bootp); i > 0; i--)
   put_buf(*cp++);

  *bufpt++ = END;
  write(tty_askfd(), packet_buff, bufpt - packet_buff);
}

static void get_buf(void)
{
  unsigned char c;
  struct iphdr *ip;
  struct udphdr *udp;
  struct bootp *bootp;

  bufend = bufpt + sizeof(packet_buff);

  while (1) {
    bufpt = packet_buff;

    while (1) {
      if (timeout)
	return;
      c = tty_getc();
      if (timeout)
	return;
      if (c == ESC) {
	c = tty_getc();
	if (timeout)
	  return;
	switch (c) {
	case ESC_ESC: c = ESC; break;
	case ESC_END: c = END; break;
	default: /* bad escape */
	}
      }
      else if (c == END)
	break;
      if (bufpt < bufend)
	*bufpt++ = c;
    }

    ip = (struct iphdr *)packet_buff;
    udp = (struct udphdr *)(packet_buff + (ip->ihl * 4));
    bootp = (struct bootp *)(udp + 1);

    if ((bufpt - packet_buff) >=
	  (sizeof(struct iphdr) + sizeof(struct udphdr)) &&
	ip->version == 4 &&
	ip->ihl >= 5 &&
	ip->protocol == 17 /* udp */ &&
	ip_fast_csum((unsigned char *)ip, ip->ihl) == 0 &&
	(udp->check == 0 || udp_check(udp, ntohs(ip->tot_len) - (ip->ihl*4),
				      ip->saddr, ip->daddr) == 0) &&
	ntohs(udp->source) == IPPORT_BOOTPS &&
	ntohs(udp->dest) == IPPORT_BOOTPC &&
	bootp->op == BOOTREPLY)
      break;
  }
}

static int
do_bootp(int argc, char *argv[])
{
  struct iphdr *ip;
  struct udphdr *udp;
  struct bootp *bootp;
  int howlong, howmany;
  char buffer[128];
  void (*oldsig)(int);

  if (argc > 1)
    howmany = atoi(argv[1]);
  else
    howmany = 3;

  if (argc > 2)
    howlong = atoi(argv[2]);
  else
    howlong = 3;

  while (1) {
    put_bootreq();

    oldsig = signal(SIGALRM, TimeOut);
    (void) alarm(howlong);
    timeout = 0;

    get_buf();

    alarm(0);
    signal(SIGALRM, oldsig);
    if (timeout) {
      if (--howmany <= 0) {
	if (opt_v) printf("Bootp timed out\n");
	return -1;
      }
    } else
      break;
  }

  ip = (struct iphdr *)packet_buff;
  udp = (struct udphdr *)(packet_buff + (ip->ihl * 4));
  bootp = (struct bootp *)(udp + 1);

  mydip.loc_ip.s_addr = bootp->yiaddr;
  mydip.rmt_ip.s_addr = bootp->siaddr;

  if (opt_v) {
    /*
     * inet_ntoa returns pointer into a single area.  need to
     * copy first return so second one doesn't overwrite it
     */
     strcpy(buffer, inet_ntoa(mydip.loc_ip));
     printf("Address set to %s by bootp from %s\n",
		    buffer, inet_ntoa(mydip.rmt_ip));
  }
  return(0);
}


struct commands commands[] = {
  { "beep",             do_beep         },
  { "bootp",		do_bootp	},
  { "break",		do_break 	},
  { "chatkey",		do_chatkey	},
  { "config",		do_config	},
  { "databits",         do_databits     },
  { "dec",		do_dec		},
  { "default",		do_default	},
  { "dial",		do_dial		},
  { "echo",		do_echo		},
  { "flush",		do_flush	},
  { "get",		do_get   	},
  { "goto",		do_goto		},
  { "help",		do_help		},
  { "if",		do_if		},
  { "inc",		do_inc		},
  { "init",		do_init		},
  { "mode",		do_mode		},
  { "modem",		do_modem	},
  { "netmask",          do_netmask      },
  { "onexit",           do_onexit       },
  { "parity",           do_parity       },
  { "password",		do_password	},
  { "proxyarp",		do_proxyarp	},
  { "print",		do_print	},
  { "psend",		do_psend	},
  { "port",		do_port		},
  { "quit",             do_quit         },
  { "reset",		do_reset	},
#ifdef SECUREID
  { "securidfixed",	do_securidfixed	},
  { "securid",		do_securid	},
#endif /* SECUREID */
  { "send",		do_send		},
  { "shell",            do_shell        },
#ifdef SKEY
  { "skey",		do_skey		},
#endif /* SKEY */
  { "sleep",		do_sleep	},
  { "speed",		do_speed	},
  { "stopbits",         do_stopbits     },
  { "term",		do_term		},
  { "timeout",		do_timeout	},
  { "wait",		do_wait		},
  { (char *)NULL,	NULL		}
};


void do_command(fp)
FILE *fp;
{
  char cline[1024];
  char *argv[32];
  int argc, i;
  int running;
  register char *sp, *xp;
  register off_t offs;
  int errlevel_flag = 1;
  int errlevel_temp = 0;

  /* Initialize the command level module. */
  var_modem = DEF_MODEM;
  var_speed = DEF_SPEED;
  scriptfp  = fp;
  running   = 1;
  timeout   = 0;

  signal(SIGINT, (void *)sig_exit);
  
  /* Tell the MODEM module about the current modem. */
  mdm_modem(var_modem);

  do {
	if (scriptfp == stdin) {
		if (opt_v == 1) printf("DIP [%-4d]> ", errlevel);
		  else printf("DIP> ");
		fflush(stdout);
	}
        if (fgets(cline, 1024, scriptfp) == (char *)NULL) {
          if (alllabels != -1)
            alllabels = 1;
          break;
        }
	if ((sp = strchr(cline, '\n')) != (char *)NULL) *sp = '\0';
	sp = cline;
	while (*sp == ' ' || *sp == '\t') sp++;
	if (*sp == '#' || *sp == '\0') continue;
	if (opt_v) fprintf(stderr, ">> %s\n", sp);
	if ((argc = getargs(sp, argv)) == 0) continue;

	/* If this is a label, skip it. */
#ifdef NE_PAUL
	if (strchr(argv[0], ':') != (char *)NULL) continue;
#else
	if ((xp = strchr(argv[0], ':')) != (char *)NULL) {
	  if (!alllabels) {
	    *xp = '\0';
	    if ((offs = ftell(scriptfp)) == (off_t) -1)
	      alllabels = -1;
	    else {
	      if ((offs > last_label()) && (add_label(sp, offs) != 0))
	        fprintf(stderr, "Duplicated label `%s' - ignored\n", sp);
	      if (opt_v)
	        fprintf(stderr, "Got label `%s' at offset %08X\n", sp, 
			(unsigned int) offs);
	    }
	  }
	  continue;
	}
#endif

	if (
	    (strcmp(argv[0], "if")    == 0)  && 
	    (strcmp(argv[0], "goto")  == 0)  &&
	    (strcmp(argv[0], "print") == 0) 
	    ) 
	  {
	    errlevel_flag=0; /* don't modify errlevel value */
	    errlevel  = 0; /* allow print of $errlvl after error detected */
	  }
	else {
	  errlevel_flag=1; /* allow to modify errlevel with the new rc */
	}

	/* Now, check which command it is. */
	if (strcmp(argv[0], "exit") != 0) {
	  i = 0;
	  while (commands[i].name != (char *)NULL) {
	    if (!strcmp(commands[i].name, argv[0])) break;
	    i++;
	  }
	  if (commands[i].name != (char *)NULL) {
	    errlevel_temp = (*commands[i].func)(argc, argv);
	    if (errlevel_flag) 
	      errlevel = errlevel_temp;
	  } else printf("? Unknown command (%s).\n", argv[0]);
	} else {
	  if (argc>1 && (errlevel=atoi(argv[1]))==-1) errlevel = 0;
	  running = 0;
	}
  } while(running);
  
#ifdef NE_PAUL
  tty_close();
#else
  cleanup();
#endif

  exit(errlevel);
}

