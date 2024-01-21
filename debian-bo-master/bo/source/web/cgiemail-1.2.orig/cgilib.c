/**********************************************************************
 *  cgi.c -- libcgi
 *
 * Copyright 1994, 1996 by the Massachusetts Institute of Technology
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/
#include "mit-copyright.h"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "cgi.h"

char *cgi_query=(char*)0;

int cgi_alloc_form(formp)
     cgi_form *formp;
{
  char *env_var;

  /* zero out everything */
  memset(formp, 0, sizeof(cgi_form));

  /* get query string from environment */
  if (!cgi_query)
    {
      env_var=getenv("REQUEST_METHOD");
      if (!env_var)
	{
	  formp->errcond=1;
	  strcpy(formp->errmsg, "400 REQUEST_METHOD not set.");
	  return(1);
	}
      if (!strcmp(env_var, "GET"))
	{
	  cgi_query= getenv("QUERY_STRING");
	  if (!cgi_query)
	    {
	      formp->errcond=1;
	      strcpy(formp->errmsg,
		     "400 REQUEST_METHOD is GET, but QUERY_STRING is not set.");
	      return(1);
	    }
	}
      else
	{
	  int content_length, total=0, bytes_read;

	  content_length= atoi(getenv("CONTENT_LENGTH"));
	  cgi_query=malloc(content_length+1);
	  if (!cgi_query)
	    {
	      formp->errcond=1;
	      sprintf(formp->errmsg,
		      "503 Couldn't allocate %d bytes of memory.",
		      content_length+1);
	      return(1);
	    }
	  while (total < content_length)
	    {
	      bytes_read = read(0, cgi_query+total, content_length);

	      /* If the client is gone, just exit */
	      if (bytes_read <= 0)
		{
		  char *progname;

		  progname=getenv("SCRIPT_NAME");
		  if (!progname) progname="cgiemail";
		  if (bytes_read < 0) perror(progname);
		  else fprintf(stderr, "%s: read() returned 0.\n", progname);
		  return(1);
		}
	      total += bytes_read;
	    }

	  cgi_query[content_length]='\0';
	}
    }

  formp->source = cgi_query;
  if (! formp->source)
    {
      formp->errcond=1;
      strcpy(formp->errmsg, "400 Could not read form input.");
      return(1);
    }

  /* decide how much to allocate */
  formp->maxstorage = strlen(formp->source)+1;
  formp->maxfields = formp->maxstorage / 4 + 1;

  /* allocate */
  formp->storage = (char *) malloc(formp->maxstorage);
  if (!formp->storage)
    {
      formp->errcond=1;
      sprintf(formp->errmsg, "503 Couldn't allocate %d bytes of memory.",
	      formp->maxstorage);
      return(1);
    }
  formp->fields = (cgi_field *) malloc(formp->maxfields * sizeof(cgi_field));
  if (!formp->fields)
    {
      free(formp->storage);
      formp->errcond=1;
      sprintf(formp->errmsg, "503 Couldn't allocate %d bytes of memory.",
	      formp->maxfields * sizeof(cgi_field));
      return(1);
    }

  return(0);
}

void
cgi_free_form(formp)
     cgi_form *formp;
{
  if (formp)
    {
      if (formp->storage) free(formp->storage);
      if (formp->fields) free(formp->fields);
    }
  return;
}

/* converts cr/lf pairs to lf, which Unix will handle better */
void
cgi_fix_crlf(string)
     char *string;
{
  char *ptr;

  while ((ptr=strchr(string, '\r')) != NULL)
    {
      if (*(ptr+1) == '\n') strcpy(ptr, ptr+1);
      else *ptr = '\n';
    }
  return;
}

/* returns 1 if string is filled-in filed, 0 if left blank */
int
cgi_nonblank(string)
     char *string;
{
  char *ptr;

  for(ptr=string; *ptr; ptr++)
    if (isgraph((int)(*ptr))) return(1);

  return(0);
}

/*
 * parsing has 4 states:
 * 0: beginning of name
 * 1: after beginning of name
 * 2: beginning of value
 * 3: after beginning of value
 */

int
cgi_parse_form(formp)
     cgi_form *formp;
{
  char *from_ptr, *to_ptr;
  cgi_field *field_ptr;
  char hex_string[3];
  int parse_state=0;
  int hex_int, i;

  /* initialize variables */
  field_ptr=formp->fields;
  formp->nfields=0;
  hex_string[2]='\0';

  /* iterate through all characters in query string */
  for (from_ptr=formp->source, to_ptr=formp->storage;
       *from_ptr != '\0' && to_ptr - formp->storage < formp->maxstorage;
       from_ptr++)
    {
      if (parse_state==0)
	{
	  /* we're at the 'n' in "name=value&name2=value2..." */
	  field_ptr->name = to_ptr;
	  formp->nfields++;
	  parse_state++;
	}
      if (parse_state==2)
	{
	  /* we're at the 'v' in "name=value&name2=value2..." */
	  field_ptr->value = to_ptr;
	  parse_state++;

	  if (from_ptr[0] == '&')
	    {
	      /* we're at the '&' in "name=&..." - value is "" */
	      if (cgi_required(field_ptr->name))
		{
		  formp->errcond=1;
		  sprintf(formp->errmsg,
			  "400 Required field left blank: %-.512s",
			  field_ptr->name);
		  return(1);
		}
	    }
	}
      if (from_ptr[0] == '%' && cgi_ishex((int)from_ptr[1]))
	{
	  /* hex representation like "%2B" in "four=2%2B2" */
	  hex_string[0] = from_ptr[1];
	  hex_string[1] = from_ptr[2];
	  sscanf(hex_string, "%x", &hex_int);
	  *to_ptr++ = (char)hex_int;
	  from_ptr += 1 + cgi_ishex((int)from_ptr[2]);
	  continue;
	}
      if (from_ptr[0] == '+')
	{
	  *to_ptr++ = ' ';
	  continue;
	}
      if (parse_state==1 && from_ptr[0] == '=')
	{
	  /* we're at the '=' in "name=value&name2=value2..." */
	  *to_ptr++ = '\0';
	  parse_state++;
	  continue;
	}	  
      if (from_ptr[0] == '&')
	{
	  *to_ptr++ = '\0';
	  field_ptr++;
	  if (field_ptr > formp->fields + formp->maxfields)
	    {
	      formp->errcond=1;
	      sprintf(formp->errmsg, "400 Exceeded %d fields.",
		      formp->maxfields);
	      return(1);
	    }
	  parse_state=0;
	  continue;
	}
      /* default */
      *to_ptr++ = *from_ptr;
    }

  /* success */
  *to_ptr='\0';
  if (parse_state==2) field_ptr->value = to_ptr;

  /* fix CR/LF; check required fields */
  for (i=0; i<formp->nfields; i++)
    {
      cgi_fix_crlf(formp->fields[i].value);

      if (cgi_required(formp->fields[i].name)
	  && !cgi_nonblank(formp->fields[i].value))
	{
	  formp->errcond=1;
	  sprintf(formp->errmsg,
		  "400 Required field left blank: %-.512s",
		  formp->fields[i].name);
	  return(1);
	}
    }

  return(0);
}

void
cgi_print_form(formp, outstream)
     cgi_form *formp;
     FILE *outstream;
{
  int i;

  fprintf(outstream,
	  "source=\t%s\nmaxstorage=\t%d\nmaxfields=\t%d\nnfields=\t%d\n",
	  formp->source, formp->maxstorage, formp->maxfields, formp->nfields);
  for (i=0; i<formp->nfields; i++)
    printf("Field %s=\t%s\n", formp->fields[i].name, formp->fields[i].value);
  return;
}

char *
cgi_value(formp, name)
     cgi_form *formp;
     char *name;
{
  int i;

  for (i=0; i<formp->nfields; i++)
    {
      if (!strcmp(formp->fields[i].name, name))
	return(formp->fields[i].value);
    }
  return("");
}

/* Output the value(s) of a CGI field name; return number found */
int
cgi_output_value(formp, name, formatstr, outstream)
     cgi_form *formp;
     char *name;
     char *formatstr;
     FILE *outstream;
{
  int i, nfound=0;

  for (i=0; i<formp->nfields; i++)
    {
      if (!strcmp(formp->fields[i].name, name))
	{
	  if (nfound) fputc(' ', outstream);
	  if (formatstr)
	    fprintf(outstream, formatstr, formp->fields[i].value);
	  else
	    fputs(formp->fields[i].value, outstream);
	  nfound++;
	}	  
    }
  return(nfound);
}

void
cgi_concat_errno(string)
     char *string;
{
#if HAVE_STRERROR
  strcat(strcat(string, " - "), strerror(errno));
#else
  char errstring[16];

  sprintf(errstring, " (errno: %d)", errno);
  strcat(string, errstring);
#endif
  return;
}

int
cgi_template_fill(formp, templatefile)
     cgi_form *formp;
     char *templatefile;
{
  FILE *tfp;
  char varname[CGI_VARNAME_MAX];
  char formatstr[CGI_VARNAME_MAX];
  int varnamelen=0, formatlen=0, nfound=0, substitutions=0;
  int inchar, parse_state=0;

#if ENABLE_CGIENV
  int cgienv=0;
  char *envval;
#endif /* ENABLE_CGIENV */

  /* open template file */
  tfp = fopen(templatefile, "r");
  if (!tfp)
    {
      formp->errcond=1;
      sprintf(formp->errmsg, "500 Could not open %-.512s", templatefile);
      cgi_concat_errno(formp->errmsg);
      return(1);
    }

  /* open temporary file for filled-in template */
  if (formp->tmpf) rewind(formp->tmpf);
  else formp->tmpf = tmpfile();
  if (!formp->tmpf)
    {
      formp->errcond=1;
      strcpy(formp->errmsg, "503 Could not open temporary file.");
      cgi_concat_errno(formp->errmsg);
      return(1);
    }

  /*
   * parsing states:
   * 0: echo
   * 1: quote
   * 2: variable
   * 3: format
   */
  
  while ((inchar=fgetc(tfp)) != EOF)
    {
      if (parse_state==1)	/* quote */
	{
	  fputc(inchar, formp->tmpf);
	  parse_state=0;
	  continue;
	}
      if (parse_state==0)	/* echo */
	{
	  if (inchar == (int)'\\')
	    {
	      parse_state=1;
	      continue;
	    }
	  if (inchar == (int)'[')
	    {
#if ENABLE_CGIENV
	      cgienv=0;
#endif
	      parse_state=2;
	      varnamelen=0;
	      formatlen=0;
	      continue;
	    }
	  fputc(inchar, formp->tmpf);
	  continue;
	}
      if (parse_state==2)	/* variable name */
	{
	  if (inchar == (int)'%')
	    {
	       formatlen=1;
	       strcpy(formatstr, "%");
	       parse_state=3;
	       continue;
	    }

#if ENABLE_CGIENV
	  if (inchar == (int)'$' && varnamelen==0)
	    {
	      cgienv=1;
	      continue;
	    }
#endif /* ENABLE_CGIENV */

	  if (inchar == (int)']')
	    {
	      varname[varnamelen]='\0';
#if ENABLE_CGIENV
	      if (cgienv)
		{
		  cgienv=0;
		  envval=getenv(varname);
		  if (!envval) envval="";
		  else substitutions++;
		  if (formatlen > 0)
		    fprintf(formp->tmpf, formatstr, envval);
		  else fputs(envval, formp->tmpf);
		}
	      else
#endif /* ENABLE_CGIENV */
		{
		  if (formatlen > 0)
		    nfound = cgi_output_value(formp,
					      varname,
					      formatstr,
					      formp->tmpf);
		  else nfound = cgi_output_value(formp,
						 varname,
						 NULL,
						 formp->tmpf);
	          if (!nfound && cgi_required(varname))
		    {
		      formp->errcond=1;
		      sprintf(formp->errmsg,
			      "400 Required field left blank: %-.512s",
			      varname);
		      return(1);
		    }
		  substitutions += nfound;
		}
	      parse_state=0;
	      continue;
	    }
	  varname[varnamelen++]=(char) inchar;
	  if (varnamelen > CGI_VARNAME_MAX)
	    {
	      fclose(tfp);
	      formp->errcond=1;
	      sprintf(formp->errmsg, "403 Variable Name too long (%-.32s...)",
		      varname);
	      return(1);
	    }
	  continue;
	}
      if (parse_state==3)	/* format */
        {
	  if (inchar == (int)',')
	    {
	      formatstr[formatlen]='\0';
	      parse_state=2;
	      continue;
	    }
	  formatstr[formatlen++]=(char) inchar;
	  if (formatlen > CGI_VARNAME_MAX)
	    {
	      fclose(tfp);
	      formp->errcond=1;
	      sprintf(formp->errmsg, "403 Format String too long (%-.32s...)",
		      formatstr);
	      return(1);
	    }
	  continue;
	}
    }
  fclose(tfp);

  /* If there were no variable substitutions in this template,
     someone may be trying to use cgiemail to get at restricted pages */
  if (!substitutions)
    {
      formp->errcond=1;
      sprintf(formp->errmsg, "403 No variable substitutions in \"%-.512s\"",
		      templatefile);
      return(1);
    }

  return 0;
}

/*
 * Make sure pclose() can get the exit status of the child process.
 * Some HTTP servers block SIGCHLD, causing pclose() to always return -1.
 * 
 * Thank you Guido van Rossum (guido@CNRI.Reston.VA.US)
 http://www-db.stanford.edu/~hassan/hymail/pythonlist/python_1995_q4/1317.html
 */

void
cgi_pclose_fix()
{
#ifdef HAVE_SIGPROCMASK
  /* POSIX signal mask */
  sigset_t mysigmask;

  sigemptyset(&mysigmask);
  sigaddset(&mysigmask, SIGCHLD);
  sigprocmask(SIG_UNBLOCK, &mysigmask, 0);

  /* Under Irix 5.3 we can be killed if sendmail died prematurely -brlewis */
  sigemptyset(&mysigmask);
  sigaddset(&mysigmask, SIGPIPE);
  sigprocmask(SIG_BLOCK, &mysigmask, 0);
#else
  /* Hopefully this works for old versions of SunOS.  We'll see. */
  signal(SIGCHLD, SIG_DFL);
#endif
}

int
cgi_mail_template(formp, templatefile)
     cgi_form *formp;
     char *templatefile;
{
  FILE *mypipe, *errorfp;
  int retval;
  char buf[BUFSIZ], command[BUFSIZ];
  int nbytes;
  char *fromhost, *servername, datebuf[80], errorfile[L_tmpnam];
  int old_stdout, old_stderr, errorfd;
  time_t rightnow;
  struct tm *timeptr;

  /* fill in template */
  retval=cgi_template_fill(formp, templatefile);
  if (retval) return(retval);

  /* Get a temporary file for error messages */
  tmpnam(errorfile);
  errorfd=open(errorfile, O_WRONLY|O_CREAT|O_EXCL, 0644);
  if (errorfd != -1)
    {
      old_stdout=dup(1);
      dup2(errorfd, 1);
      old_stderr=dup(2);
      dup2(errorfd, 2);
    }      

  /* open pipe to sendmail */
  cgi_pclose_fix();
  strcat(strcpy(command, PATH_SENDMAIL), " -oi -t");
  if (strstr(cgi_value(formp, "cgiemail-mailopt"), "sync"))
    strcat(command, " -odi");	/* synchronous delivery */
  mypipe = popen(command, "w");

  /* Restore stdout/stderr */
  if (errorfd != -1)
    {
      dup2(old_stdout, 1);
      close(old_stdout);
      dup2(old_stderr, 2);
      close(old_stderr);
      close(errorfd);
    }

  /* Check for failed popen */
  if (!mypipe)
    {
      formp->errcond=1;
      strcpy(formp->errmsg, "500 Could not open sendmail pipe.");
      cgi_concat_errno(formp->errmsg);
      return(1);
    }

  /* put "Received: from HOST with HTTP;\n\tdate\n" */
  /* See RFC 822 appendix D. */

  fromhost = getenv("REMOTE_HOST");
  if (!fromhost || !(*fromhost)) fromhost = getenv("REMOTE_ADDR");
  if (fromhost && *fromhost)
    {
      fputs("Received: from ", mypipe);
      fputs(fromhost, mypipe);
      servername=getenv("SERVER_NAME");
      if (servername && *servername)
	{
	  fputs(" by ", mypipe);
	  fputs(servername, mypipe);
	}
      fputs(" with HTTP;\n", mypipe);

      /* Put date-time (RFC 822 sec. 5, RFC 1123 sec. 5.2.14) */
      /* XXX not compliant if lang is not English or TZ unknown */
      time(&rightnow);
      timeptr=localtime(&rightnow);
      strftime(datebuf, 79, "\t%a, %d %b %Y %H:%M:%S %Z\n", timeptr);
      fputs(datebuf, mypipe);
    }

  /* send filled-in template */
  rewind(formp->tmpf);
  do {
    if ((nbytes = fread(buf, sizeof(char), BUFSIZ, formp->tmpf)) > 0)
      fwrite(buf, sizeof(char), nbytes, mypipe);
  } while (nbytes == BUFSIZ);

  /* close */
  retval=pclose(mypipe);

  /*
   * The return value of pclose is not always meaningful.
   * However, sendmail does not write to stdout/stderr if successful.
   */

  /* If we can't open the errorfile, something went wrong. */
  errorfp = fopen(errorfile, "r");
  if (!errorfp)
    {
      formp->errcond=1;
      sprintf(formp->errmsg,
	     "500 sendmail exit %d - check httpd error logs", retval);
      strcpy(formp->errinfo, command);
    }
  else
    {
      /* See if there was any error message */
      fread(formp->errinfo, sizeof(char), CGI_ERRMSG_MAX, errorfp);
      if (cgi_nonblank(formp->errinfo))
	{
	  formp->errcond=1;
	  sprintf(formp->errmsg, "500 sendmail exit %d with error message",
		 retval);
	}
    }

  unlink(errorfile);
  return(formp->errcond);
}

void
cgi_output_failure(formp, msg)
     cgi_form *formp;
     char *msg;
{
  puts("Content-Type: text/html\r");
  printf("Status: %s\r\n\r\n", formp->errmsg);
  puts("<HEAD><TITLE>Error</TITLE></HEAD>");
  printf("<BODY><H1>Error</H1>%s<P>", msg);
  puts("<BLOCKQUOTE><STRONG><SAMP>");
  puts(formp->errmsg);
  puts("</SAMP></STRONG><P>");
  if (*(formp->errinfo))
    {
      puts("<PRE>");
      puts(formp->errinfo);
      puts("</PRE>");
    }
  puts("</BLOCKQUOTE>");
  (void) cgi_output_value(formp, CGI_ADDENDUM, NULL, stdout);
  puts("<P><EM>cgiemail ");
  puts(CGIEMAIL_RELEASE);
  puts("</EM></BODY>");
  return;
}

/* Function to translate an integer character into an HTML entity string */
char *
cgi_char2entity(c)
     int c;
{
  static char retval[2];

  switch (c)
    {
    case (int)'&':
      return("&amp;");
    case (int)'<':
      return("&lt;");
    case (int)'>':
      return("&gt;");
    default:
      sprintf(retval, "%c", c);
      return(retval);
    }
}
  
void
cgi_ascii2html(instream, outstream)
     FILE *instream, *outstream;
{
  int c;

  while ((c=fgetc(instream)) != EOF)
    fputs(cgi_char2entity(c), outstream);
  return;
}

void
cgi_redirect(url)
     char *url;
{
  printf("Location: %s\r\n\r\n", url);
  return;
}

void
cgi_output_success(formp, msg)
     cgi_form *formp;
     char *msg;
{
  char *success;

  /* maybe use customized success message */
  success=cgi_value(formp, CGI_SUCCESS);
  if (success && *success)
    {
      cgi_redirect(success);
      return;
    }

  /* use generic success message */
  puts("Content-Type: text/html\r\n\r");
  puts("<HEAD><TITLE>Success</TITLE></HEAD>");
  printf("<BODY>%s<P><HR>", msg);
  puts("<PRE>");
  rewind(formp->tmpf);
  cgi_ascii2html(formp->tmpf, stdout);
  if (*cgi_value(formp, "debug-source"))
    {
      puts("<HR>");
      puts(formp->source);
      puts("<HR>");
    }
  puts("</PRE><P>");
  (void) cgi_output_value(formp, CGI_ADDENDUM, NULL, stdout);
  puts("<P><EM>cgiemail ");
  puts(CGIEMAIL_RELEASE);
  puts("</EM></BODY>");

  return;
}

int
cgi_standard_email()
{
  cgi_form form;
  char *template_filename;

  template_filename = getenv("PATH_TRANSLATED");
  if (!template_filename || !(*template_filename))
    {
      /* FIXME: shouldn't hard-code this URL */
      cgi_redirect("http://web.mit.edu/wwwdev/cgiemail/nopath.html");
      return(1);
    }

  if (cgi_alloc_form(&form) ||
      cgi_parse_form(&form) ||
      cgi_mail_template(&form, template_filename))
    {
      cgi_output_failure(&form, "No email was sent due to an error.");
      cgi_free_form(&form);
      return(1);
    }

  cgi_output_success(&form, "The following email message was sent.");
  cgi_free_form(&form);
  return(0);
}

int
cgi_standard_echo()
{
  cgi_form form;
  char *template_filename;

  template_filename = getenv("PATH_TRANSLATED");
  if (!template_filename || !(*template_filename))
    {
      /* FIXME: shouldn't hard-code this URL */
      cgi_redirect("http://web.mit.edu/wwwdev/cgiemail/nopath.html");
      return(1);
    }

  if (cgi_alloc_form(&form) ||
      cgi_parse_form(&form) ||
      cgi_template_fill(&form, template_filename))
    {
      cgi_output_failure(&form, "Form was not processed due to an error.");
      cgi_free_form(&form);
      return(1);
    }

  cgi_output_success(&form, "Processed form looks like this:");
  cgi_free_form(&form);
  return(0);
}
