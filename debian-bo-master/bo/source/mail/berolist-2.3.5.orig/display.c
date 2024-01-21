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
	char *subject,*from,*message;
	GDBM_FILE db;
	datum key,content;
	unsigned int num,i;
	register int j;
	
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

	s=cgi_scan("msg");
	if(s==NULL)
		i=0;
	else
		i=atoi(s);
	
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

	d_key=salloc(4);
	strcpy(d_key,"num");
	key.dptr=d_key;
	key.dsize=3;
	content=gdbm_fetch(db,key);
	free(d_key);
	num=atoi(content.dptr);
	if(i<0) i=0;
        if(num<=i) i=num-1;
        s=salloc(12);
        sprintf(s,"s%u",i);
        key.dptr=s;
        key.dsize=strlen(s);
        content=gdbm_fetch(db,key);
        subject=salloc(content.dsize+1);
        strncpy(subject,content.dptr,content.dsize);
        free(s);
        s=salloc(12);
        sprintf(s,"f%u",i);
        key.dptr=s;
        key.dsize=strlen(s);
        content=gdbm_fetch(db,key);
        from=salloc(content.dsize+1);
        strncpy(from,content.dptr,content.dsize);
        free(s);
        printf("<CENTER><H1>%s</H1>From <A HREF=mailto:%s>%s</A></CENTER><HR>",subject,email(from),realname(from));
        free(from); free(s);
	s=salloc(12);
	sprintf(s,"%u",i);
	key.dptr=s;
	key.dsize=strlen(s);
	content=gdbm_fetch(db,key);
	message=salloc(content.dsize+1);
	strncpy(message,content.dptr,content.dsize);
	for(j=0;j<=slen(message);j++)
	{
		if(message[j]=='\n')
			puts("<BR>");
		else
			putchar(message[j]);
	}
	puts("<HR>");
	if(i>0)		printf("<A HREF=\"%s?list=%s&msg=%u\">previous message</A>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",DISPLAY_CGI,listname,i-1);
	printf("<A HREF=%s?list=%s>message list</A>&nbsp;&nbsp;&nbsp;&nbsp;",MESSAGES_CGI,listname);
	if(i<num-1)	printf("<A HREF=\"%s?list=%s&msg=%u\">next message</A>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",DISPLAY_CGI,listname,i+1);
	return 0;
}
