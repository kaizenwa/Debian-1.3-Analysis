#include "tweak.h"
#include <stdio.h>
#include <ctype.h>
#include "server_def.h"
#include "s_extern.h"
#include "co_extern.h"

static int conf_read = 0;

int daemonize = 0;
int always_use_cache_dir = 0;
int read_only = 0;
int udp_port = 0;
int dbug = 0;
int run_uid = 0;
int priv_mode = 0;
int no_unnamed = 0;
int logging = 0;
int maxthcallowed = 0;

char *logname = NULL;
char *home_dir = NULL;
char *dir_cache_dir = NULL;
char *readme_file = NULL;

extern int inetd_mode;
extern IPrange **iptab;
extern unsigned int ipcnt, iptot;
extern char *config_file ;

extern char *strdup();
extern IPrange *parse_ipline();

static void log_set PROTO2(int, flag, int, neg)
{
  if(neg) logging &= ~flag;
  else logging |= flag;
}

void static read_configuration PROTO1(char *, name)
{
  FILE *fp;
  char buf[1024], *p, *q;

  fp = fopen(name,"r");
  if(!fp) {
    fprintf(stderr, "Unable to open configuration file: %s. Exiting.\n", name);
    exit(-1);
  }
  while(fgets(buf, sizeof(buf), fp)) {
    if(buf[0] == '\n' || buf[0] == '#') continue;
    buf[strlen(buf)-1] = '\0'; /* strip off the newline */
    p = buf; while(*p && isspace(*p)) p++;
    q = p; while(*q && !isspace(*q)) q++;
    if(!*p || !*q) {
      fprintf(stderr,"Bogus line in configuration file: %s. Exiting.\n",name);
      exit(-1);
    }
    *q = '\0'; q++;
    if(strcasecmp(p, "conf") == 0) {
      if(conf_read) {
	fprintf(stderr, "No recursion of conf commands allowed. Skipping.\n");
	continue;
      }
      conf_read = 1;
      fclose(fp);
      read_configuration(q);
      return;
    } else if(strcasecmp(p, "readme") == 0) {
      if(readme_file) free(readme_file);
      readme_file = strdup(q);
    } else if(strcasecmp(p, "homedir") == 0) {
      if(home_dir) free(home_dir);
      home_dir = strdup(q);
    } else if(strcasecmp(p, "logfile") == 0) {
      if(logname) free(logname);
      logname = strdup(q);
    } else if(strcasecmp(p, "cachedir") == 0) {
      if(dir_cache_dir) free(dir_cache_dir);
      dir_cache_dir = strdup(q);
    } else if(strcasecmp(p, "host") == 0) {
      IPrange *nl;
      nl = parse_ipline(q);
      if(nl) {
	if(!iptab) {
	  iptot = 1;
	  iptab = (IPrange **)malloc(sizeof(IPrange *) * iptot);
	} else if(ipcnt == iptot) {
	  iptot *= 2;
	  iptab = (IPrange **)realloc((char *)iptab,sizeof(IPrange *) * iptot);
	}
	iptab[ipcnt++] = nl;
      }
    } else if(strcasecmp(p, "log") == 0) {
      char *r;
      int neg;
      do {
	/* skip to next token */
	r = q; while(*r && !isspace(*r)) r++; 
        if (*r) { *r++ = 0 ; while(*r && isspace(*r)) r++; }
        if(strcasecmp(q, "none") == 0) {
	  logging = L_NONE;
	  break;
	} else if(strcasecmp(q, "all") == 0) {
	  logging = L_ALL;
	} else {
	  if(*q == '!') { neg = 1; q++;} else neg = 0;
          if(strcasecmp(q, "transfers") == 0) {
	    log_set(L_GETFILE, neg);
	    log_set(L_INSTALL, neg);
          } else if(strcasecmp(q, "version") == 0) log_set(L_VER, neg);
	  else if(strcasecmp(q, "errors") == 0) log_set(L_ERR, neg);
	  else if(strcasecmp(q, "getdir") == 0) log_set(L_GETDIR, neg);
	  else if(strcasecmp(q, "getfile") == 0) log_set(L_GETFILE, neg);
	  else if(strcasecmp(q, "upload") == 0) log_set(L_UPLOAD, neg);
	  else if(strcasecmp(q, "install") == 0) log_set(L_INSTALL, neg);
	  else if(strcasecmp(q, "delfile") == 0) log_set(L_DELFILE, neg);
	  else if(strcasecmp(q, "deldir") == 0) log_set(L_DELDIR, neg);
	  else if(strcasecmp(q, "setpro") == 0) log_set(L_SETPRO, neg);
	  else if(strcasecmp(q, "getpro") == 0) log_set(L_GETPRO, neg);
	  else if(strcasecmp(q, "makedir") == 0) log_set(L_MAKEDIR, neg);
	  else if(strcasecmp(q, "grabfile") == 0) log_set(L_GRABFILE, neg);
        }
	q = r;
      } while (*q);
    } else if(strcasecmp(p, "port") == 0)
      udp_port = atoi(q);
    else if(strcasecmp(p, "thruput") == 0) {
      if(strcasecmp(q, "off") == 0) maxthcallowed = 0;
      else maxthcallowed = atoi(q);
      if(maxthcallowed < 0) maxthcallowed = 0;
    } else if(strcasecmp(p, "setuid") == 0) {
      if(strcasecmp(q, "off") == 0) run_uid = 0;
      else run_uid = atoi(q);
    } else if(strcasecmp(p, "daemonize") == 0) {
      if(strcasecmp(q, "off") == 0) daemonize = 0;
      else daemonize = 1;
    } else if(strcasecmp(p, "debug") == 0) {
      if(strcasecmp(q, "off") == 0) dbug= 0;
      else dbug = 1;
    } else if(strcasecmp(p, "usecachedir") == 0) {
      if(strcasecmp(q, "off") == 0) always_use_cache_dir = 0;
      else always_use_cache_dir = 1;
    } else if(strcasecmp(p, "restricted") == 0) {
      if(strcasecmp(q, "off") == 0) priv_mode = 0;
      else priv_mode = 1;
    } else if(strcasecmp(p, "reverse_name") == 0) {
      if(strcasecmp(q, "off") == 0) no_unnamed = 0;
      else no_unnamed = 1;
    } else if(strcasecmp(p, "read_only") == 0) {
      if(strcasecmp(q, "off") == 0) read_only = 0;
      else read_only = 1;
    } else {
      fprintf(stderr, "Invalid command (%s) in config file, skipping.\n", p);
    }
  }
}

static void check_required_vars PROTO0((void))
{
  if(!inetd_mode && !udp_port) {
    fprintf(stderr, "No port set in config file. Exiting.\n");
    exit(-1);
  }
  if(!home_dir) {
    fprintf(stderr, "No home directory set in config file. Exiting.\n");
    exit(-1);
  }
  if(!readme_file) {
    readme_file = strdup(".README");
  }
  if(logging && !logname) {
    fprintf(stderr, "No log file set in config file. Exiting.\n");
    exit(-1);
  }
  if(always_use_cache_dir && !dir_cache_dir) {
    fprintf(stderr, "No cache directory set in config file. Exiting.\n");
    exit(-1);
  }
}

void load_configuration PROTO0((void))
{
  read_configuration(config_file ? config_file : CONF_FILE);
  check_required_vars();
}
