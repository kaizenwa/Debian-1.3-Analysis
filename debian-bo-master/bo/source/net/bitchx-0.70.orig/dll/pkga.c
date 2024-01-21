/* 
 * pkga.c --
 *
 *	This file contains a simple Tcl package "pkga" that is intended
 *	for testing the Tcl dynamic loading facilities.
 *
 *	CopyRight Colten Edwards aka panasync@efnet Jan 1997
 */
/* compile with 
 * gcc -o -I../include -fPIC -o pkga.o pkga.c
 * gcc -shared -o pkga.so pkga.o
 */

#include "irc.h"
#include "alias.h"
#include "ctcp.h"
#include "ircaux.h"
#include "list.h"
#include "struct.h"
#include "numbers.h"
#include "output.h"
#include "edit.h"
#include "vars.h"

/*
 * Prototypes for procedures defined later in this file:
 */
extern NumericFunction *numeric_dll;

static void	Pkga_EqCmd _(( IrcCommandDll *, char *, char *, char *));
static void	Pkga_QuoteCmd _((IrcCommandDll *, char *, char *, char *));
static char	*Pkga_newctcp _((CtcpEntryDll *, char *, char *, char *));
static char	*Pkga_ctcppage _((CtcpEntryDll *, char *, char *, char *));
static	char	*Pkga_alias _((char *));

static	int	Pkga_numeric _((char *, char *, char **));
/*
 *----------------------------------------------------------------------
 *
 * Pkga_EqCmd --
 *
 *	This procedure is invoked to process the "pkga_eq" Tcl command.
 *	It expects two arguments and returns 1 if they are the same,
 *	0 if they are different.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

void Pkga_EqCmd(interp, command, args, subargs)
    IrcCommandDll *interp;			/* Current interpreter. */
    char *command;
    char *args;				/* Number of arguments. */
    char *subargs;			/* Argument strings. */
{
char *arg1, *arg2;
	arg1 = next_arg(args, &args);
	arg2 = next_arg(args, &args);
	if (!arg1 || !arg2)
		return;
	put_it("arg1 %s arg2", !my_stricmp(arg1, arg2)?"eq":"!eq");
	return;
}

/*
 *----------------------------------------------------------------------
 *
 * Pkga_quoteCmd --
 *
 *	This procedure is invoked to process the "pkga_quote" Tcl command.
 *	It expects one argument, which it returns as result.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

void Pkga_QuoteCmd(interp, command, args, subargs)
    IrcCommandDll *interp;			/* Current interpreter. */
    char *command;
    char *args;			/* Argument strings. */
    char *subargs;
{
    return;
}

static char *Pkga_newctcp _((CtcpEntryDll *dll, char *from, char *to, char *args))
{
char putbuf[500];
	sprintf(putbuf, "%c%s %s%c", CTCP_DELIM_CHAR, dll->name, my_ctime(time(NULL)), CTCP_DELIM_CHAR);
	send_text(from, putbuf, "NOTICE", 0, 0);                              
	return NULL;
}

static char *Pkga_ctcppage _((CtcpEntryDll *dll, char *from, char *to, char *args))
{
char putbuf[500];
	sprintf(putbuf, "%c%s %s%c", CTCP_DELIM_CHAR, dll->name, my_ctime(time(NULL)), CTCP_DELIM_CHAR);
	send_text(from, putbuf, "NOTICE", 0, 0);                              
	put_it(" %s is paging you", from);
	return NULL;
}

static char *Pkga_alias _((char *word))
{
	if (!word || !*word)
		return m_strdup("no string passed");
	/* caller free's this string */
	return m_strdup(word);
}

static int Pkga_numeric _((char *from, char *user, char **args))
{
	put_it("Server numeric 1 being handled");
	return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * Pkga_Init --
 *
 *	This is a package initialization procedure, which is called
 *	by Tcl when this package is to be added to an interpreter.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Pkga_Init(interp)
    IrcCommandDll **interp;		/* Interpreter in which the package is
				 * to be made available. */
{
	int code;
	CtcpEntryDll *newc;
	IrcCommandDll *new;
	BuiltInDllFunctions *newa;
	NumericFunction	*newf;
	IrcVariableDll *newv;

		
	new = (IrcCommandDll *) new_malloc(sizeof(IrcCommandDll));
	new->name = m_strdup("pkga_eq");
	new->func = Pkga_EqCmd;
	add_to_list((List **)&dll_commands, (List *)new);
	put_it("Irc: New command has been added [%s]", new->name);


	newc = (CtcpEntryDll *) new_malloc(sizeof(CtcpEntryDll));
	newc->name = m_strdup("BLAH");
	newc->desc = m_strdup("New ctcp type");
	newc->id = -1;
	newc->flag = CTCP_SPECIAL | CTCP_TELLUSER;
	newc->func = Pkga_newctcp;
	add_to_list((List **)&dll_ctcp, (List *)newc);
	put_it("Irc: New ctcp has been added [%s]", newc->name);


	newc = (CtcpEntryDll *) new_malloc(sizeof(CtcpEntryDll));
	newc->name = m_strdup("PAGE");
	newc->desc = m_strdup("Paging user");
	newc->id = -1;
	newc->flag = CTCP_SPECIAL | CTCP_TELLUSER;
	newc->func = Pkga_ctcppage;
	add_to_list((List **)&dll_ctcp, (List *)newc);
	put_it("Irc: New ctcp has been added [%s]", newc->name);



	newa = (BuiltInDllFunctions *) new_malloc(sizeof(BuiltInDllFunctions));
	newa->name = m_strdup("BLAH");
	newa->func = Pkga_alias;
	add_to_list((List **)&dll_functions, (List *)newa);
	put_it("Irc: New alias has been added [%s]", newa->name);


	newf = (NumericFunction *) new_malloc(sizeof(NumericFunction));
	newf->number = 1;
	newf->name = m_sprintf("%3.3u", newf->number);
	newf->func = Pkga_numeric;
	add_to_list((List **)&numeric_dll, (List *)newf);
	put_it("New server numeric handler added [%d]", newf->number);



	newv = (IrcVariableDll *) new_malloc(sizeof(IrcVariableDll));
	newv->type = STR_TYPE_VAR;
	newv->name = m_sprintf("%s", "new_variable");
	newv->string = m_strdup("TEST VALUE");
	add_to_list((List **)&dll_variable, (List *)newv);
	put_it("New variable added [%s -> %s]", newv->name, newv->string);
	return 0;


}
