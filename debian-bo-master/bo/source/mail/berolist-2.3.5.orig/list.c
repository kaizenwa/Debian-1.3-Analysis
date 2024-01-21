/* BeroList 2.3.5                                                    */
/* An easy-to-use mailing list server                                */
/* (c) 1996/97 by Bernhard Rosenkraenzer <root@startrek.in-trier.de> */
/* Fixed by Jarno Paananen <jpaana@kalahari.ton.tut.fi>              */
/*          Stefano Torricella <stetor@ronchiato.it>                 */
/*          Christoph Lameter <clameter@waterf.org>                  */
/*          Andrus Kangro <andrusk@ml.ee>                            */

#define _GNU_SOURCE

#include "list.h"

/* Set default values for variables if they aren't set in list.h */
#if !defined(USE_SENDMAIL) && !defined(SMTP_SERVER)
	#define SMTP_SERVER "localhost"
#endif
#ifdef HAS_NNTP
	#ifndef DEFAULT_NNTP_SERVER
		#define DEFAULT_NNTP_SERVER "localhost"
	#endif
	#ifndef DEFAULT_DISTRIBUTION
		#define DEFAULT_DISTRIBUTION "local"
	#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "tool.h"
#ifdef HAVE_GETHOSTBYNAME
	#include <netdb.h>
#endif
#if defined(HAVE_GDBM) && defined(ARCHIVE)
	#include <gdbm.h>
#endif
#if defined(HAS_NNTP) || !defined(USE_SENDMAIL)
	#include "socket.h"
#endif
#ifndef HOST
	char *hostname;
	#define HOST hostname
	#define GET_HOST
#endif
#ifdef LOG
	FILE *log;
#endif
char *listaddress;

/* Function prototypes... */

void send_mail(char *list, char *to, char *from, char *subject, char *message, char *footfile);
void sendtolist(char *header, char *listname, char *from, char *subject, char *message);
void subscribe(char *listname, char *user, char *operator);
void unsubscribe(char *listname, char *user, char *operator);

/* Functions... */

void send_mail(char *list, char *to, char *from, char *subject, char *message, char *footfile)
{
	char *footer;
	#ifdef USE_SENDMAIL
		FILE *mailer;
		char *mailer_cmd;
	#else
		FILE *socket;
	#endif
	#ifdef DEBUG
		puts("proc send");
	#endif
	puts("Sending...");
	#ifdef USE_SENDMAIL     /* USE_SENDMAIL: Use the 1.x.x way */
		#ifdef DEBUG
			puts("using sendmail");
			#if defined(LOG) && defined(DEBUG)
				fputs("* Sending a message using sendmail",log);
			#endif
		#endif
		#ifdef TRUSTED
			#ifdef DEBUG
				puts("trusted");
			#endif
			from=email(from);
			mailer_cmd=salloc(slen(SENDMAIL)+slen(from)+slen(to)+7);
			sprintf(mailer_cmd,"%s -f%s '%s'",SENDMAIL,from,to);
			#ifdef LOG
				fprintf(log,"* %s\n",mailer_cmd);
			#endif
			mailer=popen(mailer_cmd,"w");
		#else
			mailer_cmd=salloc(slen(SENDMAIL)+slen(to)+4);
			sprintf(mailer_cmd,"%s '%s'",SENDMAIL,to);
			#ifdef LOG
				fprintf(log,"* %s\n",mailer_cmd);
			#endif
			mailer=popen(mailer_cmd,"w");
			fprintf(mailer,"From: %s\n",from);
		#endif
		if(subject==NULL && strncasecmp(message,"subject:",8)==0) {
			/* Subject: line @ beginning of msg body... */
			subject=salloc(slen(message)-6);
			strcpy(subject,message+8);
			while(strchr(subject,' ')==subject) subject++;
			subject[strchr(subject,'\n')-subject]='\0';
			message=strchr(message,'\n')+1;
		}
		if(subject!=NULL)	fprintf(mailer,"Subject: %s\n",subject);
		fprintf(mailer,"\n%s\n",message);
		footer=readfile(footfile);
		if(footer!=NULL)	fprintf(mailer,"%s\n",footer);
		fprintf(mailer,".\n");
		if(footer!=NULL)	free(footer);
		pclose(mailer);
	#else
		#ifdef DEBUG
			puts("using SMTP");
		#endif
		#if defined(LOG) && defined(DEBUG)
			fputs("* Sending a message using SMTP",log);
		#endif
		socket=SockOpen(SMTP_SERVER,25);
		if(socket==NULL) {
			printf("Can't connect to server %s.\n",SMTP_SERVER);
			exit(1);
		}
		printf("Connected to %s.\n",SMTP_SERVER);
		if(Reply(socket)!=220)
			abandon(9,"Server fails to initiate contact.");
		if(Send(socket,"HELO %s\r\n",HOST)!=250)
			abandon(9,"Server doesn't accept HELO.");
		if(Send(socket,"MAIL FROM: %s\r\n",email(from))!=250)
			abandon(9,"Server doesn't accept MAIL FROM.");
		if(Send(socket,"RCPT TO: %s\r\n",email(to))!=250)
			abandon(9,"Server doesn't accept RCPT TO.");
		if(Send(socket,"DATA\r\n")!=354)
			abandon(9,"Server doesn't accept DATA.");
		SockPrintf(socket,"From: %s\r\n",from);
		SockPrintf(socket,"To: %s\r\n",to);
		if(subject==NULL && strncasecmp(message,"subject:",8)==0) {
			/* Subject: line @ beginning of msg body... */
			subject=salloc(slen(message)-6);
			strcpy(subject,message+8);
			while(strchr(subject,' ')==subject) subject++;
			subject[strchr(subject,'\n')-subject]='\0';
			message=strchr(message,'\n')+1;
		}
		if(subject!=NULL)	SockPrintf(socket,"Subject: %s\r\n",subject);
		SockPrintf(socket,"\r\n");
		SockPuts(socket,message);
		footer=readfile(footfile);
		if(footer!=NULL)	{
			SockPuts(socket,footer);
			free(footer);
		}
		if(Send(socket,".\r\n")!=250)
			abandon(9,"Message rejected by server.");
		if(Send(socket,"QUIT\r\n")!=221)
			abandon(10,"Server doesn't accept QUIT, probably non-fatal error.");
		fclose(socket);
	#endif
}

void sendtolist(char *header, char *listname, char *from, char *subject, char *message)
{
	char *listfile, *footfile, *conffile, *member;
	char *oldmsg;
	char *config,*sender,*replyto;
	#ifdef KILLFILE
		char *killfile;
	#endif
	FILE *list;
	#if !defined(USE_SENDMAIL) || defined(HAS_NNTP)
		FILE *socket;
		char *footer;
	#endif
	#ifdef HAS_NNTP
		char *nntp_server;
		char *newsgroup;
		char *distribution;
		int post_nntp=1;
	#endif
	#if defined(HAVE_GDBM) && defined(ARCHIVE)
		char *archivefile;
		int use_archive=1;
		GDBM_FILE db;
		datum key,content;
		char *d_key,*d_content;
		unsigned long num;
	#endif

	puts("Sending to list...");

	#ifdef KILLFILE
		killfile=readfile(KILLFILE);
		if(killfile==NULL) {
			killfile=salloc(sizeof(char));
			killfile[0]='\0';
		}
		if(strcasestr(killfile,email(from))==NULL) {
	#endif
	
	listfile=salloc(slen(LISTDIR)+slen(listname)+10);
	sprintf(listfile,"%s/%s.members",LISTDIR,listname);
	footfile=salloc(slen(LISTDIR)+slen(listname)+9);
	sprintf(footfile,"%s/%s.footer",LISTDIR,listname);
	conffile=salloc(slen(LISTDIR)+slen(listname)+9);
	sprintf(conffile,"%s/%s.config",LISTDIR,listname);
	config=readfile(conffile);
	#if defined(HAVE_GDBM) && defined(ARCHIVE)
		archivefile=extract(config,"archive=",'\n');
		if(archivefile==NULL) {
			archivefile=salloc(slen(LISTDIR)+slen(listname)+10);
			sprintf(archivefile,"%s/%s.archive",LISTDIR,listname);
		}
		if(scasecmp(archivefile,"none")==0)
			use_archive=0;
	#endif

	sender=extract(config,"sender=",'\n');
	if(scasecmp(sender,"original")==0 || scasecmp(sender,"sender")==0 || sender==NULL)
		sender=from;
	if(scasecmp(sender,"list")==0)
		sender=listaddress;

	replyto=extract(config,"replyto=",'\n');
	if(scasecmp(replyto,"original")==0 || scasecmp(replyto,"sender")==0)
		replyto=from;
	if(scasecmp(replyto,"list")==0)
		replyto=listaddress;

	#ifndef NO_SENDER_ADDRESS
		if(scasecmp(email(sender),email(from))) { /* sender != original sender */
			oldmsg=message;
			message=salloc(slen(from)+slen(oldmsg)+15);
			sprintf(message,"* From: %s\n\n%s",from,oldmsg);
			free(oldmsg);
		}
	#endif
	#ifndef NO_LIST_ADDRESS
		if(scasecmp(listaddress,email(sender))) { /* sender != list */
			oldmsg=message;
			message=salloc(slen(listaddress)+slen(oldmsg)+15);
			sprintf(message,"* List: %s\n\n%s",listaddress,oldmsg);
			free(oldmsg);
		}
	#endif

	list=fopen(listfile,"r");

	#ifdef USE_SENDMAIL
		#if defined(LOG) && defined(DEBUG)
			fputs("* Sending to list using sendmail",log);
		#endif
		while(!feof(list)) {
			member=readline(list);
			if((slen(member)>2) && (member[0]!='#'))
				send_mail(listname,member,sender,subject,message,footfile);
			free(member);
		}
	#else
		#if defined(LOG) && defined(DEBUG)
			fputs("* Sending to list using SMTP",log);
		#endif
		socket=SockOpen(SMTP_SERVER,25);
		if(socket==NULL) {
			printf("Can't connect to server %s.\n",SMTP_SERVER);
			exit(1);
		}
		printf("Connected to %s.\n",SMTP_SERVER);
		if(Reply(socket)!=220)
			abandon(7,"Server fails to initiate contact.");
		if(Send(socket,"HELO %s\r\n",HOST)!=250)
			abandon(7,"Server doesn't accept HELO.");
		if(Send(socket,"MAIL FROM: %s\r\n",email(sender))!=250)
			abandon(7,"Server doesn't accept MAIL FROM.");
		while(!feof(list)) {
			member=readline(list);
			if((slen(member)>2) && (member[0]!='#')) {
				if(Send(socket,"RCPT TO: %s\r\n",email(member))!=250)
					printf("Server doesn't accept recipient %s.\n",email(member));
			}
		}
		if(Send(socket,"DATA\r\n")!=354)
			abandon(7,"Server doesn't accept DATA.");
		SockPrintf(socket,"From: %s\r\n",sender);
		if(replyto!=NULL) SockPrintf(socket,"Reply-To: %s\r\n",replyto);
		SockPrintf(socket,"To: %s\r\n",listaddress);
		SockPrintf(socket,"Message-ID: <%X.BeroList.2.3.5@%s>\r\n",(unsigned long) time(NULL),HOST);
		if(header!=NULL)	SockPrintf(socket,"%s",header);
		if(subject!=NULL)	SockPrintf(socket,"Subject: %s\r\n",subject);
		             else	SockPuts(socket,"Subject: (no subject)\r\n");
		SockPrintf(socket,"\r\n");
		SockPuts(socket,message);
		footer=readfile(footfile);
		if(footer!=NULL)	SockPrintf(socket,"%s\r\n",footer);
		if(Send(socket,".\r\n")!=250)
			abandon(7,"Message rejected by server.");
		if(Send(socket,"QUIT\r\n")!=221)
			abandon(8,"Server doesn't accept QUIT, probably non-fatal error.");
		fclose(socket);
		if(footer!=NULL)	free(footer);
	#endif /* !USE_SMTP */
	#ifdef HAS_NNTP
		newsgroup=extract(config,"newsgroup=",'\n');
		if(newsgroup==NULL)
		post_nntp=0;
		if(post_nntp!=0) {
			#ifdef LOG
				fprintf(log,"Posting to newsgroup %s.\n",newsgroup);
			#endif
			nntp_server=extract(config,"nntp-server=",'\n');
			if(nntp_server==NULL) {
				nntp_server=salloc(strlen(DEFAULT_NNTP_SERVER)+1);
				strcpy(nntp_server,DEFAULT_NNTP_SERVER);
			}
			distribution=extract(config,"distribution=",'\n');
			if(distribution==NULL) {
				distribution=salloc(strlen(DEFAULT_DISTRIBUTION)+1);
				strcpy(distribution,DEFAULT_DISTRIBUTION);
			}
			socket=SockOpen(nntp_server,119);
			if(socket==NULL) {
				printf("Can't connect to server %s.\n",nntp_server);
				exit(1);
			}
			printf("Connected to %s.\n",nntp_server);
			if(Reply(socket)!=200)
				abandon(11,"NNTP server fails to initiate contact.");
			if(Send(socket,"MODE READER\r\n")!=200)
				abandon(11,"NNTP Server doesn't accept MODE.");
			if(Send(socket,"POST\r\n")!=340)
				abandon(11,"NNTP Server doesn't accept POST.");
			SockPrintf(socket,"From: %s\n",sender);
			SockPrintf(socket,"Newsgroups: %s\n",newsgroup);
			SockPrintf(socket,"Distribution: %s\n",distribution);
			if(subject==NULL && strncasecmp(message,"subject:",8)==0) {
				/* Subject: line @ beginning of msg body... */
				subject=salloc(slen(message)-6);
				strcpy(subject,message+8);
				while(strchr(subject,' ')==subject) subject++;
				subject[strchr(subject,' ')-subject]='\0';
				message=strchr(message,'\n')+1;
			}
			if(subject!=NULL)
				SockPrintf(socket,"Subject: %s\n",subject);
			else
				SockPuts(socket,"Subject: (no subject)");

			/* The following line is required for MIME, but might crash the news server... */
			/* if(header!=NULL) SockPrintf(socket,"%s",header); */
			SockPrintf(socket,"\n");
			SockPuts(socket,message);
			footer=readfile(footfile);
			if(footer!=NULL)	SockPrintf(socket,"%s\r\n",footer);
			if(Send(socket,".\r\n")!=240)
				abandon(11,"Message rejected by NNTP server.");
			if(Send(socket,"QUIT\r\n")!=205)
				abandon(12,"NNTP Server doesn't accept QUIT, probably non-fatal error.");
			fclose(socket);
			free(footer);
		}
	#endif
	#if defined(HAVE_GDBM) && defined(ARCHIVE)
		if(use_archive!=0) {
			#if defined(LOG) && defined(DEBUG)
				fputs("* Storing to archive",log);
			#endif

			/* Store message to archive */
			/* Check for existing messages */
			db=gdbm_open(archivefile,0,GDBM_READER,00664,0);
			if(db==NULL)
				num=0;
			else {
				d_key=salloc(4);
				strcpy(d_key,"num");
				key.dptr=d_key;
				key.dsize=3;
				content=gdbm_fetch(db,key);
				free(d_key);
				num=atoi(content.dptr);
				gdbm_close(db);
			}
			db=gdbm_open(archivefile,0,GDBM_WRCREAT,00664,NULL);
			if(db==NULL) {
				puts("Error: Couldn't write to message archive.");
				puts("Your message has been sent to the list, anyway.");
				abandon(9,"Please contact the list operator.");
			}
			d_key=salloc(12);
			sprintf(d_key,"s%u",num);
			key.dptr=d_key;
			key.dsize=strlen(d_key);
			content.dptr=subject;
			content.dsize=strlen(subject);
			gdbm_store(db,key,content,GDBM_INSERT);
			sprintf(d_key,"f%u",num);
			key.dptr=d_key;
			key.dsize=strlen(d_key);
			content.dptr=from;
			content.dsize=strlen(from);
			gdbm_store(db,key,content,GDBM_INSERT);
			free(d_key);
			d_key=salloc(12);
			sprintf(d_key,"%u",num);
			key.dptr=d_key;
			key.dsize=strlen(d_key);
			content.dptr=message;
			content.dsize=strlen(message);
			gdbm_store(db,key,content,GDBM_INSERT);
			free(d_key);
			d_key=salloc(4);
			strcpy(d_key,"num");
			key.dptr=d_key;
			key.dsize=3;
			d_content=salloc(12);
			sprintf(d_content,"%u",++num);
			content.dptr=d_content;
			content.dsize=strlen(d_content);
			gdbm_store(db,key,content,GDBM_REPLACE);
			free(d_key);
			gdbm_close(db);
		}
	#endif
	#if defined(LOG) && defined(DEBUG)
		fputs("* done.",log);
	#endif

	fclose(list);
	free(config); free(listfile); free(footfile); free(conffile);
	#ifdef KILLFILE
		}
		free(killfile);
	#endif
}

void subscribe(char *listname, char *user, char *operator)
{
	FILE *list;
	char *mail,*from;
	char *listfile,*footfile,*welcfile;
	char *members;

	#ifdef LOG
		if(operator==NULL)
			fprintf(log,"* subscribing %s\n",user);
		else
			fprintf(log,"* %s subscribed by %s\n",user,operator);
	#endif

	listfile=salloc(slen(LISTDIR)+slen(listname)+10);
	sprintf(listfile,"%s/%s.members",LISTDIR,listname);
	welcfile=salloc(slen(LISTDIR)+slen(listname)+10);
	sprintf(welcfile,"%s/%s.welcome",LISTDIR,listname);
	footfile=salloc(slen(LISTDIR)+slen(listname)+9);
	sprintf(footfile,"%s/%s.footer",LISTDIR,listname);

	members=readfile(listfile);
	if(members==NULL) {
		members=salloc(sizeof(char));
		members[0]='\0';
	}

	if(strcasestr(members,email(user))!=NULL) { /* User is already subscribed */
		mail=salloc(2048);
		if(operator==NULL) { /* User subscribed himheritself */
			sprintf(mail,"You are already subscribed to this list.\n");
			send_mail(listname,user,listname,"List subscription...",mail,footfile);
		} else {
			sprintf(mail,"You tried to subscibe a user (%s) who is already subscribed to the list.\n",user);
			send_mail(listname,operator,listname,"List subscription...",mail,footfile);
		}
		free(mail);
	} else {
		list=fopen(listfile,"a");
		fprintf(list,"%s\n",user);
		fclose(list);
		mail=readfile(welcfile);
		if(mail!=NULL) {
			#ifdef LOG
				fprintf(log,"* sending welcome message\n");
			#endif
			if(operator!=NULL)
				send_mail(listname,user,operator,NULL,mail,footfile);
			else {
				from=salloc(1024);
				sprintf(from,"%s@%s",listname,HOST);
				send_mail(listname,user,from,NULL,mail,footfile);
				free(from);
			}
			free(mail);
		}
	}
	free(footfile);  free(welcfile);  free(listfile);
}

void unsubscribe(char *listname, char *user, char *operator)
{
	char *listfile;
	FILE *list;
	register int members,i;
	char *member[MAX_MEMBERS];

	listfile=salloc(slen(LISTDIR)+slen(listname)+10);
	sprintf(listfile,"%s/%s.members",LISTDIR,listname);

	#ifdef LOG
		if(operator==NULL)
			fprintf(log,"* %s unsubscribed\n",user);
		else
			fprintf(log,"* %s unsubscribed by %s\n",user,operator);
	#endif

	list=fopen(listfile,"r");
	members=0;
	while(!feof(list)) {
		member[members]=readline(list);
		if(scasecmp(email(member[members]),email(user))!=0) members++;
	}
	members--;
	fclose(list);
	list=fopen(listfile,"w");
	for(i=0;i<=members;i++) {
		if(slen(member[i])>2) fprintf(list,"%s\n",member[i]);
	}
	fclose(list);
	free(listfile);
}

int main(int argc, char *argv[])
{
	int pos,flen,wsize;
	char *mail,*header,*msg,*to,*from,*errors,*subject,*recipient;
	char *conffile,*listfile,*opfile,*footfile;
	char *header_copy,*additional_header,*header_line,*save_header;
	char *config;
	char *a;
	#ifdef LOG
		char *logfile;
		time_t utc;
		struct tm *servertime;
		static char *weekday[7]={"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	#endif

	#ifdef GET_HOST
		#ifdef HAVE_GETHOSTBYNAME
			struct hostent *x;
			hostname=salloc(1024);
			gethostname(hostname,1024);
			x=gethostbyname(hostname);
			if (x) strcpy(hostname,x->h_name); else strcpy(hostname,"localhost");
		#else   /* Try to determine hostname anyway */
			hostname=salloc(1024);
			gethostname(hostname,1024);
			hostname[slen(hostname)+1]='\0';
			hostname[slen(hostname)]='.';
			getdomainname(hostname+slen(hostname),1024-slen(hostname)-1);
		#endif
	#endif

	printf("%s ready...\n\n",VERSION);

	if(argc>2)
		abandon(1,"Error: BeroList must be called with one parameter (recommended),\nor without parameters.\n\nThe parameter must be the list name.\n\nContact the list operator.\n\n\n");

	#ifdef DEBUG
		puts("reading message...");
	#endif

	flen=0; wsize=1024;
	mail=salloc(sizeof(char)*(wsize+2));
	while(!feof(stdin)) {
		mail[flen]=(char) fgetc(stdin);
		if(flen==wsize) {
			mail[flen+1]='\0'; wsize+=1024;
			mail=(char *) realloc(mail,sizeof(char)*(wsize+2));
		}
		flen++;
	}
	flen--;
	mail[flen]='\0';

	#ifdef DEBUG
		puts("Extracting header...");
	#endif

	header=salloc(slen(mail) + 1);
	strcpy(header,mail);

	pos=strstr(header,"\n\n")-header;
	header[pos]='\0';

	msg=salloc(slen(mail+pos+2)+1);
	strcpy(msg,mail+pos+2);

	/* Get miscellaneous information from the message header... */
	if(argc==1) {
		/* If called without parameters, try to determine our address. */
		to=downcase(email(extract(header,"To: ",'\n')));
		if(strchr(to,'@')!=NULL) to[strchr(to,'@')-to]='\0';
		if(strncasecmp(to+slen(to)-8,"-request",8)==0)
			to[slen(to)-8]='\0';
		} else {
			to=salloc(strlen(argv[1])+1);
			strcpy(to,argv[1]);
		}
		from=extract(header,"From: ",'\n');
		subject=extract(header,"Subject: ",'\n');
		if(subject==NULL) {
			subject=salloc(13);
			strcpy(subject,"(no subject)\0");
		}
		if(to[0]=='"') to++;

	/* Get all other information from the message header...
	   Permit for MIME, etc. */

	#ifdef DEBUG
		puts("Extracting information...");
	#endif

	header_copy=salloc(slen(header)+2);
	strcpy(header_copy,header);
	header_copy[slen(header)]='\n';
	header_copy[slen(header)+1]='\0';
	save_header=header_copy;
	additional_header=salloc(slen(header)+1);
	header_line=salloc(slen(header)+1);
	while(strchr(header_copy,'\n')!=NULL) {
		strcpy(header_line,header_copy);
		header_copy=strchr(header_copy,'\n')+1;
		header_line[strchr(header_line,'\n')-header_line+1]='\0';
		if(strncasecmp(header_line,"From: ",6)!=0 &&
		   strncasecmp(header_line,"To: ",4)!=0 &&
		   strncasecmp(header_line,"Subject: ",9)!=0 &&
		   strncasecmp(header_line,"From ",5)!=0 &&
		   strncasecmp(header_line,"Return-Path",11)!=0 &&
		   strncasecmp(header_line,"Received ",9)!=0 &&
		   strncasecmp(header_line,"Received: ",10)!=0 &&
		   strncasecmp(header_line,"Message-Id",10)!=0 &&
		   strncasecmp(header_line,"Reply-To: ",10)!=0) {
			strcpy(additional_header+slen(additional_header),header_line);
		}
	}
	free(header_line);
	free(save_header);

	listaddress=salloc(slen(to)+slen(HOST)+2);
	sprintf(listaddress,"%s@%s",to,HOST);

	#ifdef LOG
		logfile=salloc(slen(LOGDIR)+slen(to)+6);
		sprintf(logfile,"%s/%s.log",LOGDIR,to);
		log=fopen(logfile,"a");
		if(log==NULL) {
			printf("can't access log file %s :(\nPlease contact the list operator.\n",logfile);
			printf("Make sure user %d (group %d) has read/write permissions\n",getuid(),getgid());
			puts("to the logfile (or the log directory, if logfile is to be created).\n");
			return(1);
		}
		utc=time(NULL);
		servertime=localtime(&utc);
		fprintf(log,"===================================================\n");
		fprintf(log,"%s %2u/%2u/%2u, %2u:%2u:%2u: Message '%s'\nfrom %s...\n",weekday[servertime->tm_wday],servertime->tm_year,servertime->tm_mon+1,servertime->tm_mday,servertime->tm_hour,servertime->tm_min,servertime->tm_sec,subject,from);
	#endif

	/* Find filenames for list... */

	listfile=salloc(slen(LISTDIR)+slen(to)+10);
	sprintf(listfile,"%s/%s.members",LISTDIR,to);
	opfile=salloc(slen(LISTDIR)+slen(to)+12);
	sprintf(opfile,"%s/%s.operators",LISTDIR,to);
	conffile=salloc(slen(LISTDIR)+slen(to)+9);
	sprintf(conffile,"%s/%s.config",LISTDIR,to);
	footfile=salloc(slen(LISTDIR)+slen(to)+9);
	sprintf(footfile,"%s/%s.footer",LISTDIR,to);

	/* read config file... */

	config=readfile(conffile);

	/* Permit "subscribe someone@somewhere" for the user - by converting it
	   to a reasonable form ("subscribe"). */

	if(strncasecmp(subject,"subscribe ",10)==0) {
		recipient=subject+10;
		if(scasecmp(email(from),email(recipient))==0)
			subject[9]='\0';
	}
	if(strncasecmp(subject,"unsubscribe ",12)==0) {
		recipient=subject+12;
		if(scasecmp(email(from),email(recipient))==0)
		subject[11]='\0';
	}

	if(strncasecmp(subject,"subscribe ",10)==0) {
		recipient=subject+10;
		a=readfile(opfile);
		if((char *) strcasestr(a,email(from))==NULL) { /* NO OPERATOR */
			#ifdef LOG
				fprintf(log,"* attempt to subscribe %s by %s\n",recipient,from);
			#endif
			free(mail);
			mail=salloc(1024);
			sprintf(mail,"You must be list operator in order to add new users to the list %s@%s.\n",to,HOST);
			send_mail(to,from,listaddress,"List subscription",mail,footfile);
		} else
			subscribe(to,recipient,from);
	} else if(strncasecmp(subject,"unsubscribe ",12)==0) {
		recipient=subject+12;
		a=readfile(opfile);
		if((char *) strcasestr(a,email(from))==NULL) { /* NO OPERATOR */
			#ifdef LOG
				fprintf(log,"* attempt to unsubscribe %s by %s\n",recipient,from);
			#endif
			free(mail);
			mail=salloc(1024);
			sprintf(mail,"You must be list operator in order to unsubscribe users.\n");
			send_mail(to,from,listaddress,"List unsubscription",mail,footfile);
		} else {
			unsubscribe(to,recipient,from);
		}
	} else if(scasecmp(subject,"subscribe")==0) {
		a=extract(config,"newusers=",'\n');
		if(scasecmp(a,"no")==0 && a!=NULL) {  /* Do not accept new users */
			a=extract(config,"contact=",'\n');
			if(a==NULL) {
				a=salloc(6+slen(HOST));
				sprintf(a,"root@%s",HOST);
			}
			#ifdef LOG
				fprintf(log,"* %s tried to subscribe\n",from);
			#endif
			free(mail);
			mail=salloc(1024);
			sprintf(mail,"%s is a closed mailing list. If you wish to subscribe, contact\nthe list operator at %s.\n",to,a);
			send_mail(to,from,listaddress,"List subscribtion",mail,footfile);
		} else {
			subscribe(to,from,NULL);
		}
	} else if(scasecmp(subject,"unsubscribe")==0) {
		unsubscribe(to,from,NULL);
	} else {  /* Mail for list */
		errors=extract(config,"errors-to=",'\n');
		if(errors==NULL) {
			errors=extract(config,"contact=",'\n');
			if(errors==NULL) {
				errors=salloc(6+slen(HOST));
				sprintf(errors,"root@%s",HOST);
			}
		}
		if(scasecmp(username(from),"mailer-daemon")==0) {
			#ifdef LOG
				fprintf(log,"* Error message - forwarding to %s.\n",errors);
			#endif
			a=salloc(slen(VERSION)+slen(msg)+slen(to)+31);
			sprintf(a,"%s\nError from mailing list %s:\n\n%s\n",VERSION,to,msg);
			free(mail);
			mail=a;
			send_mail(to,errors,from,subject,mail,NULL);
		} else {
			a=extract(config,"closed=",'\n');
			if(scasecmp(a,"yes")==0) {
				a=readfile(listfile);
			if((char *) strcasestr(a,email(from))==NULL) { /* NOT member of list */
				#ifdef LOG
					fprintf(log,"* Message rejected - %s not member of list.\n",from);
				#endif
				a=extract(config,"newusers=",'\n');
				free(mail); mail=salloc(2048);
				if(scasecmp(a,"no")==0) {
					a=extract(config,"contact=",'\n');
					if(a==NULL) {
						a=salloc(6+slen(HOST));
						sprintf(a,"root@%s",HOST);
					}
					sprintf(mail,"You may send mail to %s@%s only if you are\nsubscribed to the list.\nContact the list operator, %s.\n",to,HOST,a);
					send_mail(to,from,listaddress,"Sending mail to this list",mail,footfile);
				} else {
					sprintf(mail,"You may send mail to %s@%s only if you are\nsubscribed to the list.\nTo subscribe, send a message to %s@%s\nwith the Subject set to 'subscribe'.\n",to,HOST,to,HOST);
					send_mail(to,from,listaddress,"Sending mail to this list",mail,footfile);
				}
			} else
				sendtolist(additional_header,to,from,subject,msg);
			} else if(scasecmp(a,"operators")==0) {
				a=readfile(opfile);
				if((char *) strcasestr(a,email(from))==NULL) { /* NOT list operator */
					#ifdef LOG
						fprintf(log,"* Message rejected - %s not list operator.\n",from);
					#endif
					free(mail); mail=salloc(2048);
					sprintf(mail,"This is an announcements list.\nOnly list operators can send messages to it.\n");
					send_mail(to,from,listaddress,"This mailing list",mail,footfile);
				} else
					sendtolist(additional_header,to,from,subject,msg);
			} else
				sendtolist(additional_header,to,from,subject,msg);
		}
	}

	#ifdef LOG
		fclose(log);
		free(logfile);
	#endif

	free(mail); free(header);
	free(to); free(from); free(subject);
	free(listfile); free(opfile);
	free(config); free(listaddress);
	if(additional_header!=NULL) free(additional_header);
	#ifdef GET_HOST
		free(hostname);
	#endif
	#ifdef DEBUGONLY
		abandon(99,"Error message sent for debugging purposes only.");
	#endif
	return 0;
}
