/****************************************************************
 *
 * KOMMENTIEREN SIE IHREN KOT!!!!!!!!!!!
 *
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <fnmatch.h>
#include <netinet/in.h>

#include <Xm/XmAll.h>
#include <Xmt/Xmt.h>
#include <Xmt/Dialog.h>
#include <Xmt/Dialogs.h>
#include <Xmt/Create.h>
#include <Xmt/Converters.h>
#include <Xmt/Procedures.h>
#include <Xmt/Layout.h>
#include <Xmt/SetValue.h>
#include <Xmt/WidgetType.h>

#undef	True
#undef	False

#include "../../src/voice.h"
#include "../../src/settings.h"

#include "mam.h"

/****************************************************************
 ****************************************************************/

static char *fallb_rsrcs[]= {
	"MAnswerMashine.xmtChildren:	XmtLayout layout;",
	"MAnswerMashine*layout.xmtChildren: \
		XmLabel caller; \
		XmLabel name; \
		XmLabel size; \
		XmLabel time; \
		XmLabel date; \
		XmLabel flags; \
		XmLabel numot; \
		XmLabel nummsg; \
		XmScale loudness; \
		XmPushButton plus, minus, play, stop, \
			playall, stopall, delete, delall, quit; \
		XmToggleButton park, toggle;",
	"*plus.activateCallback: next_msg();",
	"*minus.activateCallback: prev_msg();",
	"*play.activateCallback: play_msg();",
	"*playall.activateCallback: play_all();",
	"*stop.activateCallback: stop_msg();",
	"*stopall.activateCallback: stop_all();",
	"*delete.activateCallback: del_msg();",
	"*delall.activateCallback: del_all();",
	"*quit.activateCallback: do_quit();",
	"*park.valueChangedCallback: do_park();",
	"*toggle.valueChangedCallback: do_toggle();",
	"*loudness.valueChangedCallback: do_loudness();",

#include "mam.ad.h"

	NULL
};

static Widget toplevel;
static XtInputId xinid;
static XtInputId xtmid;
static char *numberfield=NUMBERFIELD;
static char *flagfield=FLAGFIELD;
static char *namefield=NAMEFIELD;
static char *callerfield=CALLERFIELD;
static char *sizefield=SIZEFIELD;
static char *datefield=DATEFIELD;
static char *timefield=TIMEFIELD;
static char *msgsfield=MSGSFIELD;
static char *onofffield=ONOFFFIELD;
static char *parkfield=PARKFIELD;
static char *searchdir;
static char *homedir;
static char *vboxrc;
static char *vboxpark;
static char *vboxstop;
static int loudness;
static uid_t curusr;
static int triggerf;
static int whichone=1;
static int lastmsg=0;
static int kidspid=0;
static int allflag=0;

/****************************************************************
 ****************************************************************/

extern void GetNameFromCallerID(uid_t, char *, char *, char *, int);

/****************************************************************
 ****************************************************************/

static int  check_onoff(char *);
static void child_died();
static void de_activate_buttons(int);
static void del_all();
static void do_del_msg(char *, char *);
static void do_loudness();
static void del_msg();
static void do_park();
static void do_play_msg(char *, char *);
static void do_quit();
static void do_start(XtAppContext);
static void do_toggle();
static void empty_fields();
static void execute2(char **);
static int  get_num_msgs(char *);
static char *get_msg(char *, int );
static void get_date(char *);
static void get_time(char *);
static void get_name(char *);
static void get_phone(char *);
static int  get_comp(char *);
static void get_finfo(char *, char *);
static void make_fnames(char *);
static void next_msg();
static void play_all();
static void play_msg();
static void play_next_msg();
static void prev_msg();
static void set_active(char *, int);
static void set_bg_color(char *, char *);
static void set_w_text(char *, char *);
static void show_num();
static void show_info(char *);
static void stop_all();
static void stop_msg();
static void update_cb(XtAppContext);
static void update_info(XtAppContext);
static void usage(char *);

/****************************************************************
 *
 * Aufbau der ganzen Dateinamen mit HOMEDIR usw.
 *
 ****************************************************************/
static void
make_fnames(char *username)
{
	char dummy[1024];
	struct passwd *pwd;

	if (username) {
		pwd=getpwnam(username);
		if ((getuid()!=0) && (getuid()!= pwd->pw_uid)) {
			fprintf(stderr,"Only root may run as different user!\n");
			exit(EPERM);
		}
		if (setuid(pwd->pw_uid)!=0) exit(EPERM);
	} else {
		pwd=getpwuid(getuid());
	}

	if (!pwd) exit(EPERM);

	sprintf(dummy,"%s/%s/%s",SPOOLDIR,pwd->pw_name,INDIR);
	if (access(dummy,F_OK|R_OK|W_OK|X_OK)) {
			fprintf(stderr,"spooldir \"%s\" not accessable\n",dummy);
			exit(ENOENT);
	}
	searchdir=strdup(dummy);

	curusr=pwd->pw_uid;
    homedir=strdup(pwd->pw_dir);

	chdir(homedir);

	sprintf(dummy,"%s/%s",homedir,VBOXRC);
	vboxrc=strdup(dummy);
	
	sprintf(dummy,"%s/%s",homedir,VBOXSTOP);
	vboxstop=strdup(dummy);
	
	sprintf(dummy,"%s/%s",homedir,VBOXPARK);
	vboxpark=strdup(dummy);
	
}

/****************************************************************
 *
 * Warten auf sterbende Kinder (wird beim ersten fork() gesetzt) 
 *
 ****************************************************************/
static void
child_died()
{
    int status;
    int exstat;

    signal(SIGCHLD, SIG_DFL);
    waitpid(-1, &status, 0);
    exstat=WEXITSTATUS(status);
    kidspid=0;
	play_next_msg();
}

/****************************************************************
 *                                                              
 * Dr. Frankensteins Cloning-Maschine                          
 *                                                            
 ****************************************************************/
static void
execute2(char *argv[])
{
    int pid;

    pid=(fork());
        switch (pid) {
        case -1:
                perror("Cannot create new process");
                return;
        case 0:
			setsid();
            signal(SIGINT, SIG_DFL);
            execvp(argv[0],argv);
            perror(argv[0]);
            return;
        default:
            kidspid=pid;
			de_activate_buttons(0);
            signal(SIGCHLD, child_died);
        }
}


/****************************************************************
 *
 * Ist angegebene Datei vorhanden?
 *
 ****************************************************************/
static int
check_onoff(char *filename)
{
	return(!access(filename,F_OK));
}

/****************************************************************
 *
 * Nachricht loeschen
 *
 ****************************************************************/
static void
do_del_msg(char *dir, char *filename)
{
	char dummy[1024];

	sprintf(dummy,"%s/%s",dir,filename);
	unlink(dummy);
}

/****************************************************************
 *
 * VBOXPLAY aufrufen mit Uebergabe des Pfad/Dateinamens
 *
 ****************************************************************/
static void
do_play_msg(char *dir, char *filename)
{
	char *args[4];
	char sndfile[10240];
	char loud[10];
	int n=0;

	sprintf(sndfile,"%s/%s",dir,filename);
	chmod(sndfile,S_IRUSR | S_IWUSR | S_IXUSR);
	sprintf(loud,"%d", loudness);

	args[n]=VBOXPLAYCMD; n++;
	args[n]=sndfile; n++;
	args[n]=loud; n++;
	args[n]=NULL;

	execute2(args);
}

/****************************************************************
 *
 * Farbe des benannten Feldes setzen
 *
 ****************************************************************/
static void
set_bg_color(char *w_name, char *color)
{
	Widget w;

	if ((w=XmtNameToWidget(toplevel,w_name)))
		XmtSetValue(w, "background", color);
}

/****************************************************************
 *
 * Text des benannten Feldes setzen
 *
 ****************************************************************/
static void
set_w_text(char *w_name, char *s)
{
	Widget w;

	if ((w=XmtNameToWidget(toplevel,w_name)))
		XmtSetValue(w, "labelString", s);
}

/****************************************************************
 *
 * De-/Aktivieren des benannten Buttons
 *
 ****************************************************************/
static void
set_active(char *wname, int sense)
{
	Widget w;

	if ((w=XmtNameToWidget(toplevel,wname))) {
		XmtSetValue(w, "sensitive", (sense) ? "True" : "False" );
	}
	XmUpdateDisplay(toplevel);
}

/****************************************************************
 *
 * De-/Aktivieren der Buttons waehren/nach Abspielen von Nachrichten
 *
 ****************************************************************/
static void
de_activate_buttons(int on)
{
	set_active("*stop", !on);
	set_active("*playall",on);
	set_active("*play",on);
	set_active("*delete",on);
	if (allflag) {
		set_active("*stopall", !on);
		set_active("*plus", on);
		set_active("*minus", on);
	}

#ifdef 0
	set_active("*delall",on);
#endif
}

/****************************************************************
 *
 * Alle Felder leermachen 
 *
 ****************************************************************/
static void
empty_fields()
{
			set_w_text(numberfield,	"-");
			set_w_text(flagfield,	"-");
			set_w_text(namefield,	"-----------");
			set_w_text(callerfield,	"---------");
			set_w_text(sizefield,	"-----");
			set_w_text(datefield,	"-----");
			set_w_text(timefield,	"-----");
}

/****************************************************************
 *
 * Anzahl vorhandener Nachrichten holen
 *
 ****************************************************************/
static int
get_num_msgs(char *dir)
{
	DIR *dirp;
	struct dirent *d;
	struct stat statbuff;
	char *thefile;
	int	ret=0;
	
	dirp=opendir(dir);
	if (!dirp) return(0);
	while ((d=readdir(dirp))) {
		if (d->d_name[0]!='.') {
			if (((thefile=malloc(strlen(dir)+strlen(d->d_name)+2))!=NULL)) {
				strcpy(thefile,dir);
				strcat(thefile,"/");
				strcat(thefile,d->d_name);
				if ((stat(thefile,&statbuff)==0)) {
					if (((statbuff.st_mode & S_IRUSR)==S_IRUSR))
						ret++;
				}
				free(thefile);
			}
		}
	}
	closedir(dirp);
	lastmsg=ret;
	return(ret);
}

/****************************************************************
 *
 * Den Dateinamen der Xten Nachricht holen
 *
 ****************************************************************/
static char *
get_msg(char *dir, int num)
{
	DIR *dirp;
	struct dirent *d;
	struct stat statbuff;
	char *thefile;
	char *ret;
	int	found=0;
	
	dirp=opendir(dir);
	if (!dirp) return(0);
	while ((d=readdir(dirp))) {
		if (d->d_name[0]!='.') {
			if (((thefile=malloc(strlen(dir)+strlen(d->d_name)+2))!=NULL)) {
				strcpy(thefile,dir);
				strcat(thefile,"/");
				strcat(thefile,d->d_name);
				if ((stat(thefile,&statbuff)==0)) {
					if (((statbuff.st_mode & S_IRUSR)==S_IRUSR))
						found++;
				}
				free(thefile);
			}
		}
		if (found == num) {
			ret=strdup(d->d_name);
			closedir(dirp);
			return(ret);
		}
	}
	closedir(dirp);

	return(NULL);
}

/****************************************************************
 *
 * Datumsfeld fuellen
 *
 ****************************************************************/
static void
get_date(char *filename)
{
	char dummy[20];

	sprintf(dummy,"%2.2s.%2.2s.%2.2s",&filename[4],&filename[2],&filename[0]);
	set_w_text(datefield, dummy);
}

/****************************************************************
 *
 * Zeitfeld fuellen
 *
 ****************************************************************/
static void
get_time(char *filename)
{
	char dummy[20];

	sprintf(dummy,"%2.2s:%2.2s:%2.2s",&filename[6],&filename[8],&filename[10]);
	set_w_text(timefield, dummy);
}

/****************************************************************
 *
 * Namensfeld fuellen
 *
 ****************************************************************/
static void
get_name(char *phone)
{
	char buffer1[MAXCALLERIDLEN];
	char buffer2[MAXCALLERIDLEN];

	GetNameFromCallerID(curusr, phone, buffer1, buffer2, MAXCALLERIDLEN);
	set_w_text(namefield, buffer2);
}

/****************************************************************
 *
 * Nummernfeld fuellen
 *
 ****************************************************************/
static void
get_phone(char *filename)
{
	char dummy[100];

	sprintf(dummy,"%s",&filename[13]);
	if (!strcmp(dummy,"0")) strcpy(dummy,NONDIGITAL);
	set_w_text(callerfield, dummy);
	get_name(dummy);
}

/****************************************************************
 *
 * Compressionsverfahren aus Datei holen
 *
 ****************************************************************/
static int
get_comp(char *filename)
{
	struct VoiceHeader Header;
	short  Compression;
	int FD;
	int ret;

	if ((FD = open(filename, O_RDONLY)) != -1) {
		if (read(FD, &Header, sizeof(struct VoiceHeader)) ==
			 sizeof(struct VoiceHeader)) {
			Compression = ntohs(Header.Compression);
			if ((Compression < MIN_COMPRESSION_NUM) || 
				(Compression > MAX_COMPRESSION_NUM)) ret=255;	
			else ret=(int)Compression;
		}
		close(FD);
	}
	return(ret);
}

/****************************************************************
 *
 * Info ueber Datei holen und Felder ausfuellen
 *
 ****************************************************************/
static void
get_finfo(char *dir, char *filename)
{
	char dummy[1024];
	struct stat buffer;
	float sex;
	int ret;

	sprintf(dummy,"%s/%s",dir,filename);
	ret=stat(dummy,&buffer);

	if (!ret) {
		switch (get_comp(dummy)) {
			case 0x02:
				sex=buffer.st_size/(2 * VOICE_COMPRESSION_BASE);
				break;
			case 0x03:
				sex=buffer.st_size/(3 * VOICE_COMPRESSION_BASE);
				break;
			case 0x04:
				sex=buffer.st_size/(4 * VOICE_COMPRESSION_BASE);
				break;
			default:
				sex=buffer.st_size/(2 * VOICE_COMPRESSION_RATE);
				break;
		}
		sprintf(dummy,"%d %s",
			(int)sex,
			SECONDSSTR);
		set_w_text(sizefield, dummy);
	
		if (buffer.st_mode & S_IXUSR) {
			set_w_text(flagfield, OLDMESSAGE);
			set_bg_color(flagfield,	OLDCOLOR);
		} else {
			set_w_text(flagfield, NEWMESSAGE);
			set_bg_color(flagfield,	NEWCOLOR);
		}
	} else {
		set_w_text(sizefield, "-----");
		set_w_text(flagfield, "-");
	}
}

/****************************************************************
 *
 * Nummer der angezeigten Msg ausgeben
 *
 ****************************************************************/
static void
show_num()
{
	char number[6];

	sprintf(number,"%d",whichone);
	set_w_text(numberfield, number);
}

/****************************************************************
 *
 * Fenster mit ausgewaehlter Message neu aufbauen
 *
 ****************************************************************/
static void
show_info(char *dir)
{
	char *found;   
	char msgs[6];  
	int anzahl=0;

	anzahl=get_num_msgs(dir);
	sprintf(msgs,"%d",anzahl);
	set_w_text(msgsfield, msgs);

	if ( whichone > anzahl ) whichone=anzahl;
	if ( whichone == 0 ) whichone=1;
	if (anzahl) {
		found=get_msg(dir,whichone);
		if (found) {
			show_num();
			get_finfo(dir,found);
			get_phone(found);
			get_date(found);
			get_time(found);
			free(found);
		} else {
			empty_fields();
		}
	} else {
		empty_fields();
	}
	XmUpdateDisplay(toplevel);
}

/****************************************************************
 *
 * Lautstaerke aendern
 *
 ****************************************************************/
static void
do_loudness()
{
	Widget w;

	if ((w=XmtNameToWidget(toplevel,"*loudness"))) {
		XtVaGetValues(w, XmNvalue, &loudness, NULL);
	}
}
/****************************************************************
 *
 * Gespraech parken fuer Uebernahme 
 *
 ****************************************************************/
static void
do_park()
{
	FILE *fp;

	if (check_onoff(vboxpark)) {
		unlink(vboxpark);
	} else {
		if ((fp=fopen(vboxpark,"w")))
			fclose(fp);
	}
	show_info(searchdir);
}

/****************************************************************
 *
 * AB Ein-/Ausschalten  
 *
 ****************************************************************/
static void
do_toggle()
{
	FILE *fp;

	if (check_onoff(vboxstop)) {
		unlink(vboxstop);
	} else {
		if ((fp=fopen(vboxstop,"w")))
			fclose(fp);
	}
	show_info(searchdir);
}

/****************************************************************
 *
 * Alle Msgs loeschen 
 *
 ****************************************************************/
static void
del_all()
{
	/* Abfrage ob wirklich */
	/* Hinweis auf nicht abgehoerte Msgs nicht vergessen */
}

/****************************************************************
 *
 * Msg finden und loeschen  
 *
 ****************************************************************/
static void
del_msg()
{
	char *found;   

	if (whichone<=lastmsg) {
		found=get_msg(searchdir,whichone);
		if (found) {
			if (whichone == lastmsg)
				whichone--;
			do_del_msg(searchdir,found);
		}
	}
	show_info(searchdir);
}

/****************************************************************
 *
 * Sollen wir die naechste Msg auch spielen?
 *
 ****************************************************************/
static void
play_next_msg()
{
	de_activate_buttons(1);
	if (allflag==255) allflag=0;
	if ((whichone < lastmsg)) {
		whichone++;
		show_info(searchdir);
		if (allflag) play_msg();
	} 
}

/****************************************************************
 *
 * Msg finden und abspielen
 *
 ****************************************************************/
static void
play_msg()
{
	char *found;   
	if (whichone<=lastmsg) {
		found=get_msg(searchdir,whichone);
		if (found) {
			do_play_msg(searchdir,found);
		}
	}
}

/****************************************************************
 *
 * Alle Msgs abspielen
 *
 ****************************************************************/
static void
play_all()
{
	int i;
	
	allflag=1;
	play_msg();
}

/****************************************************************
 *
 * Abspielen aller Nachrichten stoppen
 *
 ****************************************************************/
static void
stop_all()
{
	allflag=255;
	stop_msg();
}

/****************************************************************
 *
 * Abspielen stoppen
 *
 ****************************************************************/
static void
stop_msg()
{
	int ret;

	if (kidspid) {
		ret=kill((-1)*kidspid,SIGINT);
	}
}

/****************************************************************
 *
 * Vorige Msg anzeigen
 *
 ****************************************************************/
static void
prev_msg()
{
	if (whichone > 1)
		whichone--;

	show_info(searchdir);
}

/****************************************************************
 *
 * Naechste Msg anzeigen 
 *
 ****************************************************************/
static void
next_msg()
{
	if (whichone < lastmsg)
		whichone++;

	show_info(searchdir);
}

/****************************************************************
 *
 * Fenster updaten
 * nicht schoen, aber funktioniert!
 *
 ****************************************************************/
static void
update_info(XtAppContext app)
{
	xtmid=0;
	show_info(searchdir);
}

/****************************************************************
 *
 * Zeitverzoegertes Update nach Auflegen erzwingen
 * nicht schoen, aber funktioniert!
 *
 ****************************************************************/
static void
update_cb(XtAppContext app)
{
	char buffer[4096];

	if (xtmid) XtRemoveTimeOut(xtmid);
	read(triggerf,buffer,4095);
	update_info(app);
	xtmid=XtAppAddTimeOut(app, 5000,
		(XtTimerCallbackProc) update_info, (XtPointer) app);
}

/****************************************************************
 *
 * /dev/isdnctrl ueberwachen. Wenn Aenderung dann Fenster updaten
 * nicht schoen, aber funktioniert!
 *
 ****************************************************************/
static void
do_start(XtAppContext app)
{
	char buffer[4096];

	do_loudness();
	update_info(app);
	
	if (((triggerf=open(TRIGGERFILE,O_RDONLY))== -1)) {
		perror(TRIGGERFILE);
		exit(1);
	}
	read(triggerf,buffer,4095);

	xinid=XtAppAddInput(app, triggerf,
		(XtPointer) XtInputReadMask,
		(XtInputCallbackProc) update_cb, (XtPointer) app);
}

/****************************************************************
 *
 * Alles sauber verlassen
 *
 ****************************************************************/
static void
do_quit()
{
	if (xtmid) XtRemoveTimeOut(xtmid);
	XtRemoveInput(xinid);
	close(triggerf);
	free(searchdir);
	free(homedir);
	free(vboxrc);
	free(vboxpark);
	free(vboxstop);
	exit(0);
}

/****************************************************************
 *
 * no comment 
 *
 ****************************************************************/
static void
usage(char *prog)
{
	fprintf(stderr,"usage:\t%s [-u username] [-v]\n"
				"\t-u option only applicable as superuser\n",prog);
	fprintf(stderr,"\tVersion: %s\n",MAMVERSION);
	exit(EINVAL);
}

/****************************************************************
 *
 * MAIN
 *
 ****************************************************************/
int
main(int argc, char **argv)
{
    XtAppContext app;
	char *username=NULL;
	extern char *optarg;
	extern int optind;
	int c;

    
    toplevel = XtAppInitialize (&app, "MAnswerMashine", NULL, 0,
                    &argc, argv, fallb_rsrcs, NULL, 0);

	while ((c=getopt(argc,argv,"u:")) != EOF)
		switch(c) {
			case 'u':
				username=optarg;
				break;
			default:
				usage(argv[0]);
		}
	
	XmtRegisterLayoutParser();

	XmtRegisterWidgetConverter();
	XmtRegisterCallbackConverter();
	XmtRegisterXmStringConverter();
	XmtRegisterPixmapConverter();
	XmtRegisterBitmapConverter();
	XmtRegisterMotifWidgets();
	XmtRegisterXmtWidgets();
	XmtRegisterXtProcedures();
	XmtRegisterXmtProcedures();

	XmtCreateChildren(toplevel);
	XtRealizeWidget (toplevel);

	XmtVaRegisterCallbackProcedures(
		"do_quit", do_quit, XmtRCallbackUnused,
		"next_msg", next_msg, XmtRCallbackUnused,
		"prev_msg", prev_msg, XmtRCallbackUnused,
		"play_msg", play_msg, XmtRCallbackUnused,
		"play_all", play_all, XmtRCallbackUnused,
		"stop_msg", stop_msg, XmtRCallbackUnused,
		"stop_all", stop_all, XmtRCallbackUnused,
		"del_msg", del_msg, XmtRCallbackUnused,
		"del_all", del_all, XmtRCallbackUnused,
		"do_toggle", do_toggle, XmtRCallbackUnused,
		"do_park", do_park, XmtRCallbackUnused,
		"do_loudness", do_loudness, XmtRCallbackUnused,
		NULL );

	set_active("*delall",0);
	set_active("*stopall",0);
	set_active("*list",0);
	set_active("*stop",0);
	set_active("*park",0);

	make_fnames(username);
	do_start(app);

    XtAppMainLoop(app);
	return(0);
}
