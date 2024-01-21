#include "gk.h"
#include <time.h>
#include <pwd.h>
#include <sys/types.h>
#include <utmp.h>
#include <sys/stat.h>

extern Tcl_KeyldelCmd();
extern Tcl_KeylgetCmd();
extern Tcl_KeylkeysCmd();
extern Tcl_KeylsetCmd();
     
extern Tcl_RandomCmd();

extern Gk_NewenvCmd();

int Gk_InfoCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, int argc, char **argv));
static char *getuserid _ANSI_ARGS_((void));
static int idletime _ANSI_ARGS_((Tcl_Interp *interp));

/*** GK stuff */
char gkInitCmd[] = "lappend auto_path $gk_library";

int Tgk_Init(interp)
  Tcl_Interp *interp;
{
    char* gklibdir;
    gklibdir = (char *)getenv("GK_LIBRARY");
    if (gklibdir == NULL) {
      gklibdir = GK_LIBRARY;
    }
    Tcl_SetVar(interp, "gk_library", gklibdir, TCL_GLOBAL_ONLY );


/** KEYED LIST */
     Tcl_CreateCommand(interp, "keyldel", Tcl_KeyldelCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(interp, "keylget", Tcl_KeylgetCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(interp, "keylkeys", Tcl_KeylkeysCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand(interp, "keylset", Tcl_KeylsetCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand (interp, "random", Tcl_RandomCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);
     Tcl_CreateCommand (interp, "gk_info", Gk_InfoCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);

     Tcl_CreateCommand (interp, "gk_env", Gk_NewenvCmd, (ClientData) NULL,
             (Tcl_CmdDeleteProc *) NULL);

    return Tcl_GlobalEval(interp, gkInitCmd); 
}

int Gk_InfoCmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char **argv;
{
   char buffer[TCL_RESULT_SIZE];
   time_t timeval;
   char *userid;
		
   if (argc!=2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " option\"", (char *) NULL);
	return TCL_ERROR;
   }	
   if (strcmp(argv[1], "host")==0) {
	   strcpy(interp->result, Tcl_GetHostName());
	   return TCL_OK;	
   } else if (strcmp(argv[1], "userid")==0) {
        userid = getuserid();
		strcpy(interp->result, ((userid!=NULL) ? userid : "unknown"));
	    return TCL_OK;
   } else if (strcmp(argv[1], "date")==0) {
        timeval = time(NULL);
        strcpy(buffer, ctime(&timeval));
	buffer[strlen(buffer)-1] = 0;
	strcpy(interp->result, buffer);
	return TCL_OK;
   } else if (strcmp(argv[1], "idletime")==0) {
	    return idletime( interp );
   } else {
	Tcl_AppendResult(interp, "bad argument to ", argv[0], ": should be one of host, userid, date, idletime", (char *) NULL);
	return TCL_ERROR;
   }
}

static char *getuserid() {
    struct passwd *pwentryPtr;
	pwentryPtr = getpwuid(getuid());
    return ( (pwentryPtr==NULL) ? NULL : pwentryPtr->pw_name);
}

static int idletime(interp) 
	Tcl_Interp *interp;
{
	struct utmp user;
	FILE *fp;
	char *userid;
	long minidle = 999999, idle;
    char ttypath[100];
	struct stat statbuf;
	time_t now;

	userid = getuserid();
	if (userid==NULL) {
		strcpy(interp->result, "could not get userid");
		return TCL_ERROR;
	}
    time(&now);
	fp = fopen("/etc/utmp", "r");
	if (fp==NULL) {
		strcpy(interp->result, "could not open /etc/utmp"); 
		return TCL_ERROR;
	}
	while (fread((char *)&user, sizeof(user), 1, fp)==1) {
		if (!user.ut_name[0])
			continue;
		if (strncmp(userid, user.ut_name, 8)==0) {
			/* found one */
			strcpy(ttypath, "/dev/");
			strcat(ttypath, user.ut_line);			
			if (stat(ttypath, &statbuf)==-1) {
				strcpy(interp->result, "error doing stat on tty");
				return TCL_ERROR;
			}		
			idle = (now < statbuf.st_atime ? 0 : now - statbuf.st_atime)/60;
			if (idle < minidle) 
				minidle = idle;			
		}
	}	
	fclose(fp);
	sprintf(interp->result, "%ld", minidle);
	return TCL_OK;
}
