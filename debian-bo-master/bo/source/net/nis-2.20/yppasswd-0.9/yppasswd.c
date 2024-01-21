/*
 * Copyright (c) 1992/3 Theo de Raadt <deraadt@fsa.ca>
 * Modifications (c) 1994 Olaf Kirch <okir@monad.swb.de>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/time.h>
#include <sys/types.h>
#include <limits.h>
#include <netdb.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#ifdef NO_GETOPT_H
#include <getopt.h>
#endif
#include <errno.h>
#include <pwd.h>
#ifdef __sgi
# include <crypt.h>
#endif

#include <rpc/rpc.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#include "yppasswd.h"

char	*prog_name;
char	*domainname;
char	*master;
uid_t	uid;

static unsigned char itoa64[] =		/* 0 ... 63 => ascii - 64 */
"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static struct timeval TIMEOUT = {25, 0};	/* total timeout */

void to64(char *, long int, int);
char *getnewpasswd(struct passwd *);

char *
val(char *s)
{
  return s ? s : "";
}

void
to64(char *s, long int v, int n)
{
  while (--n >= 0)
    {
      *s++ = itoa64[v&0x3f];
      v >>= 6;
    }
}

char *
getnewpasswd(register struct passwd *pw)
{
  char *buf;
  char salt[9], *p=NULL;
  int tries = 0;
  
  buf = (char *) malloc(30);
  
  printf("Changing YP password for %s on %s.\n", pw->pw_name, master);
  
  buf[0] = '\0';
  while(1) {
    p = getpass("Please enter new password:");
    if(*p == '\0') {
        printf("Password unchanged.\n");
        return NULL;
    }
#ifndef DEBUG
    if (strlen(p) <= 5 && (uid != 0 || ++tries < 6)) {
        printf("Please enter a longer password.\n");
        continue;
    }
#ifdef USE_OBVIOUS
    if (isobvious(p) && (uid != 0 || ++tries < 6)) {
	printf(
	"This password might be too easy for a program to guess. Please\n"
	"use a more complicated one, e.g. mixing upper- and lower-case\n"
	"letters, digits and special characters.\n");
	continue;
    }
#endif
#endif
    strcpy(buf, p);
    p = getpass("Please retype new password:");
    if( strcmp(buf, p) == 0) {
        break;
    } else {
        printf("Mismatch - password unchanged.\n");
        return NULL;
    }
  }
  
  /* grab a random printable character that isn't a colon */
  srandom((int)time((time_t *)NULL));
  to64(&salt[0], random(), 2);
  return strdup(crypt(buf, salt));
}

char *
getfield(char *gecos, char *field, int size)
{
    char *sp;

    for (sp = gecos; *sp != '\0' && *sp != ','; sp++);
    if (*sp != '\0') {
    	*sp++ = '\0';
    }
    strncpy (field, gecos, size-1);
    field[size-1] = '\0';
    return sp;
}

int
newfield(char *prompt, char *deflt, char *field, int size)
{
    char	*sp;

    if (deflt == NULL) {
        deflt = "none";
    }

    printf("%s [%s]: ", prompt, deflt);
    fflush(stdout);
    if (fgets(field, size, stdin) == NULL) {
    	return 1;
    }

    if ((sp = strchr(field, '\n')) != NULL) {
    	*sp = '\0';
    }

    if (!strcmp(field, "")) {
    	strcpy(field, deflt);
    }
    if (!strcmp(field, "none")) {
    	strcpy(field, "");
    }

    if (strchr(field, ':') != NULL) {
    	fprintf(stderr, "%s: no colons allowed in GECOS field... sorry.\n",
    				prog_name);
    	return 1;
    }
    return 0;
}

char *
getnewfullname(struct passwd *pw)
{
    char	gecos[1024], *sp, new_gecos[1024];
    char	name[254], location[254], office[254], phone[254];

    printf ("\nChanging full name for %s on %s.\n"
    	    "To accept the default, simply press return. To enter an empty\n"
    	    "field, type the word \"none\".\n",
    	    pw->pw_name, master);

    strncpy (gecos, pw->pw_gecos, sizeof(gecos));
    sp = getfield(gecos, name, sizeof(name));
    if (newfield("Name", strtok(gecos, ","), name, sizeof(name))) {
    	return NULL;
    }
    sp = getfield(sp, location, sizeof(location));
    if (newfield("Location", location, location, sizeof(location))) {
    	return NULL;
    }
    sp = getfield(sp, office, sizeof(office));
    if (newfield("Office Phone", office, office, sizeof(office))) {
    	return NULL;
    }
    sp = getfield(sp, phone, sizeof(phone));
    if (newfield("Home Phone", phone, phone, sizeof(phone))) {
    	return NULL;
    }
    sprintf (new_gecos, "%s,%s,%s,%s", name, location, office, phone);

    sp = new_gecos + strlen(new_gecos);
    while (*--sp == ',') *sp = '\0';

    return strdup(new_gecos);
}

char *
getnewshell(struct passwd *pw)
{
    char    new_shell[PATH_MAX];

    printf ("\nChanging login shell for %s on %s.\n"
    	    "To accept the default, simply press return. To use the\n"
    	    "system's default shell, type the word \"none\".\n",
    	    pw->pw_name, master);

    if (newfield("Login shell", pw->pw_shell, new_shell, sizeof(new_shell))) {
        return NULL;
    }
    return strdup(new_shell);
}

char *
getserver( void )
{
  char  *master;
  int 	port, err;

  if ((err = yp_get_default_domain(&domainname)) != 0) {
      fprintf(stderr, "%s: can't get local yp domain: %s\n",
              				prog_name, yperr_string(err));
      return NULL;
  }
  
  if ((err = yp_master(domainname, "passwd.byname", &master)) != 0) {
      fprintf(stderr, "%s: can't find the master ypserver: %s\n",
              				prog_name, yperr_string(err));
      return NULL;
  }
  port = getrpcport(master, YPPASSWDPROG, YPPASSWDPROC_UPDATE, IPPROTO_UDP);
  if (port==0) {
      fprintf (stderr, "%s: yppasswdd not running on NIS master host\n",
					prog_name);
      return NULL;
  }
  if (port >= IPPORT_RESERVED) {
      fprintf (stderr, "%s: yppasswd daemon running on illegal port.\n",
					prog_name);
      return NULL;
  }
  return master;
}

/* YP result codes. */
char *yp_strerror(int code)
{
  char *res;

  switch(code) {
	case  YP_NOMORE:  res = "No more"; break;
	case  YP_TRUE:    res = "True"; break;
	case  YP_FALSE:   res = "False"; break;
	case  YP_NOMAP:   res = "No such map"; break;
	case  YP_NODOM:   res = "No such domain"; break;
	case  YP_BADOP:   res = "Bad operation"; break;
	case  YP_BADDB:   res = "Database bad"; break;
	case  YP_BADARGS: res = "Bad arguments"; break;
	case  YP_VERS:    res = "Version Mismatch"; break;
	default:          res = "Unknown error"; break;
  }
  return(res);
}

struct passwd *ypgetpw(char *name, int uid)
{
  struct ypreq_key key;
  enum clnt_stat re;
  static struct ypresp_val resp;
  static char buffer[1024];
  static struct passwd pwd;
  char uidbuf[8];
  char *ptr, *keyval, *map;
  struct timeval timeout;
  CLIENT *clnt;

  clnt = clnt_create( master, YPPROG, YPVERS, "udp" );
  clnt->cl_auth = authunix_create_default();
  timeout.tv_sec = 25; timeout.tv_usec = 0;

  if (name == NULL) {
	sprintf(uidbuf, "%d", uid);
	keyval = uidbuf;
	map = "passwd.byuid";
  } else {
	keyval = name;
	map = "passwd.byname";
  }

  /* Fill in values. */
  resp.valdat.valdat_val = malloc(1024);
  key.domain = domainname;
  key.map = map;
  key.keydat.keydat_val = keyval;
  key.keydat.keydat_len = strlen(keyval);

  re = clnt_call(clnt, YPPROC_MATCH, xdr_ypreq_key, &key,
	       xdr_ypresp_val, &resp, TIMEOUT);

  if (re != RPC_SUCCESS) {
	clnt_perrno(re);
	fprintf(stderr, "\n");
	return NULL;
  }
  if (resp.status != 1) {
	fprintf(stderr, "%s\n", yp_strerror(resp.status));
	return NULL;
  }

  strncpy(buffer, resp.valdat.valdat_val, resp.valdat.valdat_len);
  buffer[resp.valdat.valdat_len] = (char) 0;
  free(resp.valdat.valdat_val);

  ptr = buffer;
  pwd.pw_name    = val(strsep(&ptr, ":"));
  pwd.pw_passwd  = val(strsep(&ptr, ":"));
  pwd.pw_uid     = atoi(val(strsep(&ptr, ":")));
  pwd.pw_gid     = atoi(val(strsep(&ptr, ":")));
  pwd.pw_gecos   = val(strsep(&ptr, ":"));
  pwd.pw_dir     = val(strsep(&ptr, ":"));
  pwd.pw_shell   = val(strsep(&ptr, ":"));

  auth_destroy( clnt->cl_auth );
  clnt_destroy( clnt );

  return &pwd;
}

void
usage(void)
{
     fprintf (stderr, "Usage: %s [-p] [-l] [-f] [user]\n", prog_name);
     exit (1);
}

int
main(int argc, char **argv)
{
  struct timeval timeout;
  struct yppasswd yppasswd;
  struct passwd *pw;
  CLIENT *clnt;
  int    opt_passwd = 0, opt_fullname = 0, opt_shell = 0;
  char   *user = NULL, *what;
  int    c, err, status;
  char   *s;
  
  if ((s = strrchr(argv[0], '/')) != NULL) {
      prog_name = s+1;
  } else {
      prog_name = argv[0];
  }

  while ((c = getopt(argc, argv, "plf")) >= 0) {
      switch (c) {
      case 'p':
      	  opt_passwd = 1;
      	  break;
      case 'l':
      	  opt_shell = 1;
      	  break;
      case 'f':
      	  opt_fullname = 1;
      	  break;
      default:
          fprintf (stderr, "%s: invalid option -%c.\n", prog_name, c);
      	  usage();
      }
  }
  if (optind < argc) {
      user = argv[optind++];
  }
  if (optind < argc) {
      usage();
  }

  if (!strcmp(prog_name, "ypchfn")) {
      opt_fullname = 1;
  } else if (!strcmp(prog_name, "ypchsh")) {
      opt_shell = 1;
  } else if (opt_passwd + opt_fullname + opt_shell == 0) {
      opt_passwd = 1;	/* default to yppasswd behavior */
  }

  if ((master = getserver()) == NULL) {
      exit(1);
  }
  
  /* Obtain the passwd struct for the user whose password is to be changed.
   */
  uid = getuid();
  if (user == NULL) {
      if ((pw = ypgetpw(user, uid)) == NULL) {
          fprintf(stderr, "%s: unknown user (uid=%d).\n", prog_name, (int) uid);
          exit(1);
      }
  } else {
      if ((pw = ypgetpw(user, uid)) == NULL) {
          fprintf(stderr, "%s: unknown user: %s.\n", prog_name, user);
          exit(1);
      }
      if (pw->pw_uid != uid && uid != 0) {
          fprintf(stderr, "%s: Only root may change account information "
          		    "for others\n", prog_name);
          exit(1);
      }
  }

  /* Initialize password information */
  yppasswd.newpw.pw_passwd = pw->pw_passwd;
  yppasswd.newpw.pw_name = pw->pw_name;
  yppasswd.newpw.pw_uid = pw->pw_uid;
  yppasswd.newpw.pw_gid = pw->pw_gid;
  yppasswd.newpw.pw_gecos = pw->pw_gecos;
  yppasswd.newpw.pw_dir = pw->pw_dir;
  yppasswd.newpw.pw_shell = pw->pw_shell;
  yppasswd.oldpass = NULL;
  
  switch(opt_passwd + (opt_fullname << 1) + (opt_shell << 2)) {
  case 1:
      what = "YP password";
      break;
  case 2:
      what = "fullname";
      break;
  case 4:
      what = "login shell";
      break;
  default:
      what = "account information";
  }

  /* Get old password */
  if (pw->pw_passwd[0]) {
    char prompt[40];

    printf("Changing %s for %s on %s.\n",  what, pw->pw_name, master);

    sprintf (prompt, "Please enter %spassword:", opt_passwd? "old " : "");
    s = getpass (prompt);

    /* We can't check the password with shadow passwords enabled. We
     * leave the checking to yppasswdd */
#if !defined(SHADOWPWD) && 0
    if(uid != 0 && strcmp(crypt(s, pw->pw_passwd), pw->pw_passwd)) {
        fprintf(stderr, "Sorry.\n");
        exit (1);
    }
#endif
    yppasswd.oldpass = strdup(s);
  }

  if (opt_passwd) {
      if ((s = getnewpasswd(pw)) == NULL) 
      	  exit (1);
      yppasswd.newpw.pw_passwd = s;
  }
  if (opt_fullname) {
      if ((s = getnewfullname(pw)) == NULL) 
      	  exit (1);
      yppasswd.newpw.pw_gecos = s;
  }
  if (opt_shell) {
      if ((s = getnewshell(pw)) == NULL) 
      	  exit (1);
      yppasswd.newpw.pw_shell = s;
  }
  
  /* The yppasswd.x file said `unix authentication required',
   * so I added it. This is the only reason it is in here.
   * My yppasswdd doesn't use it, but maybe some others out there
   * do. 					--okir
   */
  clnt = clnt_create( master, YPPASSWDPROG, YPPASSWDVERS, "udp" );
  clnt->cl_auth = authunix_create_default();
  bzero( (char*)&status, sizeof(status) );
  timeout.tv_sec = 25; timeout.tv_usec = 0;
  err = clnt_call( clnt, YPPASSWDPROC_UPDATE,
  		 (xdrproc_t) xdr_yppasswd, (char*) &yppasswd,
  		 (xdrproc_t) xdr_int,      (char*) &status,
  		 timeout );

  if (err) {
      clnt_perrno(err);
      fprintf( stderr, "\n" );
  } else if (status) {
      fprintf( stderr, "Error while changing %s.\n", what );
  }

  printf("\nThe %s has%s been changed on %s.\n", 
  		what, (err || status)? " not" : "", master);

  auth_destroy( clnt->cl_auth );
  clnt_destroy( clnt );
  exit ((err || status) != 0);
}
