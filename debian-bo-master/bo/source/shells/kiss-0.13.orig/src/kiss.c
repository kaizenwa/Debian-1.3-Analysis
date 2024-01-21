#include "kiss.h"

/* global variables */
Stringstack *alias;
int nalias;
char bufferedinput [FILENAMELEN];
Flags flags;
int lastfile;
int lastchildpid;
int laststatus;
char *progname;
int nhislist;
Stringstack *hislist;
int shlvl;
int orgargc;
char **orgargv;
int inputparsed;
char homedir [LINELEN];
char username [LINELEN];

Cmdtable cmdtable [] =
{
    /* cmd name	    function		0:run as child, 1:run immediately */
    /*--------------------------------------------------------------------*/
    
    { "!",	    dorecall,		0 },
    { "alias",	    doalias,		1 },
    { "cat",	    docat,		0 },
    { "cd",	    docd,		1 },
    { "chgrp",	    dochgrp,		0 },
    { "chmod",	    dochmod,		0 },
    { "chown",	    dochown,		0 },
    { "cp",	    docp,		0 },
    { "echo",	    doecho,		0 },
    { "exec",	    doexec,		1 },
    { "exit",	    doquit,		1 },
    { "grep",	    dogrep,		0 },
    { "help",	    dohelp,		0 },
    { "history",    dohistory,		0 },
    { "kill",	    dokill,		0 },
    { "ln",	    doln,		0 },
    { "ls",	    dols,		0 },
    { "mkdir",	    domkdir,		0 },
    { "mknod",	    domknod,		0 },
    { "mount",	    domount,		0 },
    { "more",	    domore,		0 },
    { "mv",	    domv,		0 },
    { "printenv",   doprintenv,		0 },
    { "pwd",	    dopwd,		0 },
    { "read",	    doread,		1 },
    { "rm",	    dorm,		0 },
    { "rmdir",	    dormdir,		0 },
    { "setenv",	    dosetenv,		1 },
    { "sleep",	    dosleep,		0 },
    { "source",	    dosource,		1 },
    { "touch",	    dotouch,		0 },
    { "umount",	    doumount,		0 },
    { "unsetenv",   dounsetenv,		1 },
    { "ver",	    dover,		0 },
    { "wc",	    dowc,		0 },
    { "where",	    dowhere,		0 },
    
    { NULL,	    NULL }
};

int main (int argc, char **argv)
{
    getprogname (argv [0]);

    return (dokiss (argc, argv));
}
