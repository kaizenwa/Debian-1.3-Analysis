#include "list.h"
#include "tool.h"
#include "cgitool.h"
#include "cgipath.h"
#include <gdbm.h>

void errhead()
{
		puts("<HTML><HEAD><TITLE>Server configuration error</TITLE>");
		puts("<META NAME=Generator CONTENT=\"BeroList 2.3.0\">");
		puts("</TITLE><BODY>");
		puts("<H1>Server configuration error</H1><HR>");
}

int main(int argc, char *argv[])
{
	char *listname, *configfile, *config, *archivefile;
	char *d_key, *s;
	char *subject, *from;
	GDBM_FILE db;
	datum key,content;
	unsigned int num;
	register unsigned int i;
	
	printf("Content-Type: text/html\n\n");
	puts("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">");
	listname=cgi_scan("list");
	if(listname==NULL) {
		errhead();
		puts("The BeroList message list script is not configured correctly.<BR>");
		puts("You have to specify the parameter list=listname.");
		exit(1);
	}
	puts("<HTML><HEAD><TITLE>BeroList messages</TITLE>");
	puts("<META NAME=Generator CONTENT=\"BeroList 2.3.0\">");
	puts("</TITLE><BODY>");
	configfile=salloc(slen(LISTDIR)+slen(listname)+9);
	sprintf(configfile,"%s/%s.config",LISTDIR,listname);
	config=readfile(configfile);
	if(config==NULL) {
		errhead();
		printf("The specified list, %s, does not exist.",listname);
		exit(1);
	}

	archivefile=extract(config,"archive=",'\n');
	if(archivefile==NULL) {
		archivefile=salloc(slen(LISTDIR)+slen(listname)+10);
		sprintf(archivefile,"%s/%s.archive",LISTDIR,listname);
	}
	if(scasecmp(archivefile,"none")==0) {
		errhead();
		printf("The specified list, %s, does not archive messages.",listname);
		exit(1);
	}
	db=gdbm_open(archivefile,0,GDBM_READER,00664,0);
	if(db==NULL) {
		errhead();
		printf("Couldn't open archive file for list %s...<BR>Archive empty?",listname);
		exit(1);
	}
	printf("<H1>Messages on list %s:</H1><BR>\n",listname);
	puts("<TABLE BORDER=0 WIDTH=100%>");
	puts("<TR><TD WIDTH=80%><B>Subject</B><TD><B>From</B>");
	d_key=salloc(4);
	strcpy(d_key,"num");
	key.dptr=d_key;
	key.dsize=3;
	content=gdbm_fetch(db,key);
	free(d_key);
	num=atoi(content.dptr);
	s=salloc(12);
	for(i=0;i<num;i++) {
		sprintf(s,"s%u",i);
		key.dptr=s;
		key.dsize=strlen(s);
		content=gdbm_fetch(db,key);
		subject=salloc(content.dsize+1);
		strncpy(subject,content.dptr,content.dsize);
		sprintf(s,"f%u",i);
		key.dptr=s;
		key.dsize=strlen(s);
		content=gdbm_fetch(db,key);
		from=salloc(content.dsize+1);
		strncpy(from,content.dptr,content.dsize);
		printf("<TR><TD>");
		printf("<A HREF=\"%s?list=%s&msg=%u\">%s</A>",DISPLAY_CGI,listname,i,subject);
		if(from!=NULL) printf("<TD><A HREF=mailto:%s>%s</A>",email(from),realname(from));
		          else printf("<TD><I>(not specified)</I>");
	} 
	puts("</TABLE>");
	return 0;
}
