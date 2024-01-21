/* 
 * Network accounting
 * config.c - configuration module
 * (C) 1994 Ulrich Callmeier
 */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <ctype.h>
#include "netacct.h"

char *rcs_revision_config_c = "$Revision: 1.9be $";

/* 
 * This routine reads all the configuration from the file fname.
 * On success it returns a non-NULL pointer to a struct config.
 * The parser is kind of a hack but it works.
 */

struct config *read_config(char *fname)
{
    char buff[1024];
    FILE *f;
    int line=0;
    struct config *cfg = malloc(sizeof(struct config));
    if(cfg == NULL) return cfg;

    cfg -> filename = NULL;
    cfg -> dumpname = NULL;
    cfg -> debugname = NULL;
    cfg -> flush = DEFAULT_FLUSH;
    cfg -> ignoremask = inet_addr(DEFAULT_IGNOREMASK);
    cfg -> err_delay = DEFAULT_ERR_DELAY;
    cfg -> ignorenet = NULL;
    cfg -> dontignore = NULL;
    cfg -> promisc = NULL;
    cfg -> notdev = NULL;
    cfg -> fdelay = DEFAULT_FDELAY;
    cfg -> disabled = 0;
    cfg -> dynamicip = NULL;
    cfg -> excludenamelookup = NULL;

    debug_level = 0;
    dev2line = NULL;

    f=fopen(fname,"r");
    if(f == NULL) return NULL;

    while(fgets(buff,sizeof(buff),f))
	{
	    /* remove trailing newline */
	    char *cmt = strchr(buff,'\n');
	    if(cmt) *cmt = '\0';

	    line++;
	    
	    /* remove comments */
	    cmt = strchr(buff,'#');
	    if(cmt) *cmt = '\0';
	    
	    /* remove leading whitespace */
	    while(isspace(*buff))
		{
		    memmove(buff,buff+1,strlen(buff));
		}

	    /* remove trailing whitespace */
	    cmt = strchr(buff,'\0');
	    cmt --;
	    while(isspace(*cmt))
		{
		    *cmt = '\0';
		    cmt --;
		}

	    /* process nonempty lines */
	    if(*buff)
		{
		    char *kwd = buff;
		    char *value = buff + strcspn(buff," \t");
		    *value++ = '\0';
		    while(isspace(*value)) value++;
		    
/*		    printf("key: \"%s\" value: \"%s\" \n",kwd, value);*/
		    
		    if(strcasecmp(kwd, "flush")==0)
			{
			    cfg->flush = atoi(value);
			    syslog(LOG_DEBUG,"config: set flushing to %d\n",cfg->flush);
			}
		    else if(strcasecmp(kwd, "fdelay")==0)
			{
			    cfg->fdelay = atoi(value);
			    syslog(LOG_DEBUG,"config: set fdelay to %d\n",cfg->fdelay);
			}
		    else if(strcasecmp(kwd, "file")==0)
			{
			    cfg->filename = strdup(value);
			    syslog(LOG_DEBUG,"config: set filename to %s\n",cfg->filename);
			}
		    else if(strcasecmp(kwd, "dumpfile")==0)
			{
			    cfg->dumpname = strdup(value);
			    syslog(LOG_DEBUG,"config: set dumpfile to %s\n",cfg->dumpname);
			}
		    else if(strcasecmp(kwd, "debugfile")==0)
			{
			    cfg->debugname = strdup(value);
			    syslog(LOG_DEBUG,"config: set debugfile to %s\n",cfg->debugname);
			}
		    else if(strcasecmp(kwd, "dynamicip")==0)
			{
			  if(value[strlen(value)-1]=='/') value[strlen(value)-1]='\0';
			  cfg->dynamicip = strdup(value);
			  syslog(LOG_DEBUG,"config: set dynamicip to %s\n",cfg->dynamicip);
			}
		    else if(strcasecmp(kwd, "ignoremask")==0)
			{
			    cfg->ignoremask = inet_addr(value);
			    syslog(LOG_DEBUG,"config: set ignoremask to %s\n",intoa(cfg->ignoremask));
			}
		    else if(strcasecmp(kwd, "debug")==0)
			{
			    debug_level = atoi(value);
			    syslog(LOG_DEBUG,"config: set debugging level to %d\n",debug_level);
			}
		    else if(strcasecmp(kwd, "disable")==0)
			{
			  int field;
			  field = atoi(value);
			  if((field < MIN_DISABLE) || (field > MAX_DISABLE))
			    {
			      syslog(LOG_ERR, "config file: invalid disable statement\n");
			      return NULL;
			    }
			  else
			    {
			      cfg->disabled |= BITMASK(field);
			      syslog(LOG_DEBUG,"config: disabled field %d\n",field);
			    }
			}
		    else if(strcasecmp(kwd, "ignorenet")==0)
			{
			    struct ipnetwork *tmp;
			    char *mask;

			    mask  = value + strcspn(value," \t");
			    *mask++ = '\0';
			    while(isspace(*mask)) mask++;

			    tmp = malloc(sizeof(struct ipnetwork));
			    
			    if(tmp != NULL)
				{
				    tmp -> netnumber = inet_addr(value);
				    tmp -> netmask = inet_addr(mask);
				    tmp -> next = cfg -> ignorenet;
				    cfg -> ignorenet = tmp;
				    syslog(LOG_DEBUG,
					   "config: added ignore network (netnumber %s)\n",
					   intoa(cfg -> ignorenet -> netnumber));
				    syslog(LOG_DEBUG,
					   "config: added ignore network (netmask %s)\n",
					   intoa(cfg -> ignorenet -> netmask));
				}
			}
		    else if(strcasecmp(kwd, "exclude-name-lookup")==0)
			{
			    struct ipnetwork *tmp;
			    char *mask;

			    mask  = value + strcspn(value," \t");
			    *mask++ = '\0';
			    while(isspace(*mask)) mask++;

			    tmp = malloc(sizeof(struct ipnetwork));
			    
			    if(tmp != NULL)
				{
				    tmp -> netnumber = inet_addr(value);
				    tmp -> netmask = inet_addr(mask);
				    tmp -> next = cfg -> excludenamelookup;
				    cfg -> excludenamelookup = tmp;
				    syslog(LOG_DEBUG,
					   "config: added exclude-name-lookup network (netnumber %s)\n",
					   intoa(cfg -> excludenamelookup -> netnumber));
				    syslog(LOG_DEBUG,
					   "config: added exclude-name-lookup network (netmask %s)\n",
					   intoa(cfg -> excludenamelookup -> netmask));
				}
			}
		    else if(strcasecmp(kwd, "dynamicnet")==0)
			{
			    char *mask;

			    mask  = value + strcspn(value," \t");
			    *mask++ = '\0';
			    while(isspace(*mask)) mask++;

			    cfg -> dynamicnet.netnumber = inet_addr(value);
			    cfg -> dynamicnet.netmask = inet_addr(mask);
			    syslog(LOG_DEBUG,
				   "config: set dynamic network (netnumber %s)\n",
				   intoa(cfg -> dynamicnet.netnumber));
			    syslog(LOG_DEBUG,
				   "config: set dynamic network (netmask %s)\n",
				   intoa(cfg -> dynamicnet.netmask));
			}
		    else if(strcasecmp(kwd, "dontignore")==0)
			{
			    struct ipnetwork *tmp;
			    char *mask;

			    mask  = value + strcspn(value," \t");
			    *mask++ = '\0';
			    while(isspace(*mask)) mask++;

			    tmp = malloc(sizeof(struct ipnetwork));
			    
			    if(tmp != NULL)
				{
				    tmp -> netnumber = inet_addr(value);
				    tmp -> netmask = inet_addr(mask);
				    tmp -> next = cfg -> dontignore;
				    cfg -> dontignore = tmp;
				    syslog(LOG_DEBUG,
					   "config: added dontignore network (netnumber %s)\n",
					   intoa(cfg -> dontignore -> netnumber));
				    syslog(LOG_DEBUG,
					   "config: added dontignore network (netmask %s)\n",
					   intoa(cfg -> dontignore -> netmask));
				}
			}
		    else if(strcasecmp(kwd, "device")==0)
			{
			    struct promisc_device *tmp;

			    tmp = malloc(sizeof(struct promisc_device));
			
			    if(tmp != NULL)
				{
				    tmp -> name  = strdup(value);
				    tmp -> reset = 0;
				    tmp -> next = cfg -> promisc;
				    cfg -> promisc = tmp;
				    syslog(LOG_DEBUG,"config: added promiscous device %s\n",
					   cfg->promisc->name);
				}
			}
		    else if(strcasecmp(kwd, "notdev")==0)
			{
			    struct promisc_device *tmp;

			    tmp = malloc(sizeof(struct promisc_device));
			
			    if(tmp != NULL)
				{
				    tmp -> name  = strdup(value);
				    tmp -> next = cfg -> notdev;
				    cfg -> notdev = tmp;
				    syslog(LOG_DEBUG,"config: added notdevice %s\n",
					   cfg->notdev->name);
				}
			}
		    else if(strcasecmp(kwd, "errdelay")==0)
			{
			    cfg->err_delay = atoi(value);
			    syslog(LOG_DEBUG,"config: set delay on error to %d\n",cfg->err_delay);
			}
		    else if(strcasecmp(kwd, "line")==0)
			{
			    struct dev2line *tmp;
			    char *line;

			    tmp = malloc(sizeof(struct dev2line));

			    line  = value + strcspn(value," \t");
			    *line++ = '\0';
			    while(isspace(*line)) line++;

			    tmp -> netinterface = strdup(value);
			    tmp -> line = strdup(line);
			    tmp -> next = dev2line;
			    dev2line = tmp;
			    
			    syslog(LOG_DEBUG,"config: [dev2line:] %s -> %s\n",dev2line->netinterface, dev2line->line);
			}
		    else
			{
		    	    syslog(LOG_ERR, "config file: unknown keyword %s in line %d\n",kwd,line);
			    return NULL;
			}
		}
	}

    if(cfg->filename == NULL)
	{
	    syslog(LOG_ERR, "config file: no filename given\n");
	    return NULL;
	}

    if(cfg->dumpname == NULL)
	{
	    syslog(LOG_ERR, "config file: no dumpfile given\n");
	    return NULL;
	}

    if(cfg->debugname == NULL)
      {
	syslog(LOG_INFO, "config file: no debugfile given, using /dev/null\n");
	cfg->debugname = strdup("/dev/null");
      }
    
    fclose(f);
    return cfg;
}
          
