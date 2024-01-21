#include "sliplogin.h"


struct slip_modes {
	char *sm_name;
	int	sm_value;
}	 modes[] = {
	"normal",		0,	/* SL_MODE_SLIP */
	"compressed",	1,	/* SL_MODE_CSLIP */
	"6bit",			2,	/* SL_MODE_SLIP6, printable slip */
	"ax25",			4,	/* SL_MODE_AX25 */
	"auto",			8	/* SL_MODE_ADAPTIVE */
};

void scan_sliphosts(struct slipinfo *info)
{
	FILE *fp;
	char user[16], *nisdomain=NULL, *outval, *c;
    char loginargs[BUFSIZ], timeout[16];
    int status, outvallen, j;

	if ((fp = fopen(_PATH_ACCESS, "r")) == NULL)
	{
	 fprintf(stderr,"sliplogin: %s: %s\n", _PATH_ACCESS, strerror(errno));
	 syslog(LOG_ERR, "%s: %m\n", _PATH_ACCESS);
	 exit(1);
	}

/* MAIN SEARCH LOOP */
	while (fgets(loginargs, sizeof(loginargs) - 1, fp))
	{
	 if (ferror(fp)) break;
	 if (loginargs[0]=='#') continue; /* Comment */

     if (loginargs[0]=='+') /* Look at NIS map ? */
     {
      if(__yp_check(NULL)!=1)
      {
       syslog(LOG_ERR,"YP not running\n");
       continue;
      }
      yp_get_default_domain(&nisdomain);
      status=yp_match(nisdomain,"slip.hosts",
				info->loginname, strlen(info->loginname), &outval,&outvallen);
      if(status!=0)
      {
       fprintf(stderr,"%s not in NIS map\n",info->loginname);
       continue; /* Read next line */
      }
      sscanf(outval, "%15s%15s%15s%15s%15s%15s%15s%15s%15s",
			user, info->laddr, info->raddr, info->mask, info->sm_name,
			timeout, info->option[0], info->option[1], info->option[2]);
      if(strlen(loginargs+1)>1) /* Local address given */
      {
       if((c=strchr(loginargs,'\n'))!=NULL) *c='\0';
       strncpy(info->laddr,loginargs+1,15);
      }
     } else /* No NIS search */
     {
	  sscanf(loginargs, "%15s%15s%15s%15s%15s%15s%15s%15s%15s",
			user, info->laddr, info->raddr, info->mask, info->sm_name,
			timeout, info->option[0], info->option[1], info->option[2]);
     }
     if ( (strcmp(user, info->loginname) != 0) && strcmp(user, "*") != 0)
				 continue; /* Read next line */

	 /* Found user */
     info->timeout=atoi(timeout);
	 fclose(fp);
     /* Now check for dynamic IP assigning */
	 if (strcmp(info->raddr, "DYNAMIC")==0)
	 {
	  char *ttyn,sliptty[20],dynamic[80];
	  ttyn=ttyname(0);
      if ((fp = fopen(_PATH_SLIPTTY, "r")) == NULL)
	  {
		fprintf(stderr,"sliplogin: %s: %s\n", _PATH_SLIPTTY, strerror(errno));
		syslog(LOG_ERR, "%s: %m\n", _PATH_SLIPTTY);
		exit(1);
	  }
	  while (fgets(dynamic, 79, fp))
	  {
		if (ferror(fp)) break;
		if (dynamic[0]=='#') continue;
		sscanf(dynamic, "%15s%15s", sliptty,info->raddr);
		if (strcmp(ttyn, sliptty) != 0) continue;
		fclose(fp);
		fp=NULL;
		break;
	  }
	  if (fp!=NULL)
	  {
		fclose(fp);
		fprintf(stderr,"sliplogin: %s not found in %s\n", ttyn,_PATH_SLIPTTY);
		syslog(LOG_ERR,"%s not found in %s\n",ttyn,_PATH_SLIPTTY);
		syslog(LOG_ERR,"Couldn't start slip session for %s\n",info->loginname);
		exit(-1);
	  }
	 } /* end if DYNAMIC */

	 /* Prepare slip_mode */
	 info->sm_value = 0;
     for (j = 0; j < sizeof(modes)/sizeof(struct slip_modes); j++)
     {
      if (strcmp(modes[j].sm_name, info->sm_name) == 0)
      {
       info->sm_value |= modes[j].sm_value;
       break;
	  }
	 } /* end for */

	 /*
	  * see if there's a login file we can use.  First check for
	  * one specific to this host.  If none found, try for
	  * a generic one.
	  */
	 sprintf(info->loginfile, "%s.%s", _PATH_LOGIN, info->loginname);
	 if (access(info->loginfile, R_OK|X_OK) != 0)
	 {
	  strcpy(info->loginfile, _PATH_LOGIN);
	  if (access(info->loginfile, R_OK|X_OK))
	  {
		fputs("access denied - no login file\n", stderr);
		syslog(LOG_ERR,"access denied for %s - no %s\n",
				info->loginname, info->loginfile);
		exit(5);
	  }
	 }
	 return;
	} /* end while */
	fclose(fp);
	(void)fprintf(stderr, "SLIP access denied for %s\n", info->loginname);
	syslog(LOG_ERR, "SLIP access denied for %s\n", info->loginname);
	exit(4);
}
