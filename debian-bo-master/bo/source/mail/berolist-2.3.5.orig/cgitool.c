#include "tool.h"
#include "cgitool.h"

char *cgi_scan(char *setting) 
{
	char *par, *loc, *cont;
	char buf[2048];
	register int i;
	
	for(i=0;i<2048;i++) buf[i]='\0';
	par=getenv("QUERY_STRING");
	if(par==NULL) return NULL;
	strncpy(buf,par,slen(par));
	
	loc=strtok(buf,"&");
	while (loc) {
		cont=strstr(loc,setting);
		if(cont==loc) {
			loc=strchr(cont,'=')+1;
			loc=strdup(loc);
			return loc;
		} else
			loc=strtok(NULL,"&");
	}
	return NULL;
}
