/* 
 * gkenv.c --
 *
 *	GroupKit environment support. 
 *
 */

#include <tcl.h>
#include <stdlib.h>
#include <gk.h>

#define MAXKEYSIZE 1000			/* maximum length of a key */
#define MAXKEYPART 100			/* maximum length of one component of a key */


/*
 * The structure below holds a single node in the environment's tree of data.
 * The "name" field holds only the last part of the node, e.g. "usernum"
 * for "local.3.usernum".  According to how environments are defined,
 * either value can be non-NULL, or children can be non-NULL, but not both.
 */

typedef struct EnvTree {
	char *name;					/* name of node */
	char *value;				/* value held by node */
	struct EnvTree *children;	/* children of this node */
	struct EnvTree *next;		/* next sibling to this node */
	struct EnvTree *parent;		/* parent of this node */
} EnvTree;


/*
 * The structure below represents a subcommand handler; subcommands are
 * either builtin procedures written in C, or are Tcl scripts.
 */

typedef struct SubCommand {
	int builtin;				/* 1=builtin (C)   0=external (Tcl) */
	union {
		Tcl_CmdProc *builtinSubcommand;	/* pointer to builtin command */
		char *tclSubcommand;	/* Tcl script for command */
	} v;
} SubCommand;


/*
 * The structure below is the ClientData for an environment, holding
 * all the information for a single instance.
 */

typedef struct Environment {
	Tcl_HashTable subcmds;		/* table of environment's subcommands */	
	EnvTree *tree;				/* data tree for this environment */
} Environment;


/*
 * Below are definitions for the builtin environment subcommands.
 */
 
static int Env_DebugSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_DestroySubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_OptionSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_SimpleGetSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_SimpleSetSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_KeysSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_DeleteSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_CommandSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_ErrUnknownSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_ImplicitUnknownSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static int Env_ExistsSubcmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));

static int EnvCmd _ANSI_ARGS_((ClientData clientData, 
		Tcl_Interp *interp, int argc, char *argv[]));
static void	Env_AddStdSubcommands _ANSI_ARGS_((Environment *env));
static void	Env_AddSubcommand _ANSI_ARGS_((Environment *env, char *cmdName, 
		Tcl_CmdProc *builtinProc, char *tclProc));
static EnvTree *Env_CreateNode _ANSI_ARGS_((Environment *env, char *prefix, 
		char *name, int *new));
static EnvTree *Env_FindNode _ANSI_ARGS_((Environment *env, char *prefix, 
		char *name));
static EnvTree *FindNode _ANSI_ARGS_((EnvTree *tree, char *name));
static EnvTree *CreateNode _ANSI_ARGS_((EnvTree *tree, char *name, int *new));
static void DeleteNodeAndKids _ANSI_ARGS_((EnvTree *node)); 
static int Env_DeleteNode _ANSI_ARGS_((Environment *env, char *prefix, char *name));
static char *createKeyedList _ANSI_ARGS_((EnvTree *node));

static int ignoreErrors _ANSI_ARGS_((Environment *env));
static int Env_UpdateEntrantSubcmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
								 int argc, char **argv));
static void Env_AuxUpdate _ANSI_ARGS_((Tcl_Interp *interp, EnvTree *tree, 
						  Tcl_DString *cmd, char *key));



/*
 *----------------------------------------------------------------------
 *
 * Gk_NewenvCmd --
 *
 *	This procedure is invoked to process the "gk_env" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

int
Gk_NewenvCmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];
{
	char *envName;
	Environment *env;
	int notifyFlag = 0;
	int new;

	if (argc != 2) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
				" name\"", (char *) NULL);
		return TCL_ERROR;
	}
	
	envName = argv[1];
	
	/* initialize environment data structure and create command */
	
	env = (Environment *) ckalloc(sizeof(Environment));
	Tcl_InitHashTable(&env->subcmds, TCL_STRING_KEYS);
	Env_AddStdSubcommands(env);

	env->tree = (EnvTree *) ckalloc (sizeof (EnvTree));
	env->tree->name = env->tree->value = (char *) NULL;
	env->tree->children = ( EnvTree *) NULL;
	env->tree->next = env->tree->parent = ( EnvTree *) NULL;
	
	Env_CreateNode(env, NULL, "data", &new);
	Env_CreateNode(env, NULL, "option", &new);
		
	Tcl_CreateCommand(interp, envName, EnvCmd, (ClientData) env,
			(Tcl_CmdDeleteProc *) NULL);
	return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * EnvCmd --
 *
 *	This procedure handles processing of commands for created
 *  environments.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

static int
EnvCmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];
{
	Environment *env = (Environment *)clientData;
	Tcl_HashEntry *cmdEntryPtr;
	SubCommand *subcommand;
	char *cmd, *cmdtail;
	int result;

	if (argc < 2) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			" option ?args?\"", (char *) NULL);
		return TCL_ERROR;
    }
    
    /* find requested subcommand */
    
    cmdEntryPtr = Tcl_FindHashEntry(&env->subcmds, argv[1]);
    if (cmdEntryPtr == NULL) {

		/* check for an unknown handler */
		cmdEntryPtr = Tcl_FindHashEntry(&env->subcmds, "unknown");
		if (cmdEntryPtr==NULL) {	
	    	Tcl_AppendResult(interp, argv[0], ": unknown subcommand", 
					(char *)NULL);
    		return TCL_ERROR;
		}
    }
    
    subcommand = (SubCommand *) Tcl_GetHashValue(cmdEntryPtr);
    
    /* if a C subcommand, invoke the builtin */
    
    if (subcommand->builtin) {
    	return subcommand->v.builtinSubcommand(clientData, interp, argc, argv);
    } 
    
    /* for Tcl subcommands, assemble a Tcl command and execute */
    
	cmdtail = Tcl_Merge(argc-2, argv+2);
	cmd = (char *)ckalloc(strlen(subcommand->v.tclSubcommand)+
				strlen(argv[0])+strlen(argv[1])+strlen(cmdtail)+4);
	sprintf(cmd, "%s %s %s %s", subcommand->v.tclSubcommand, argv[0], 
			argv[1], cmdtail);
	result = Tcl_RecordAndEval(interp, cmd, 0);
	ckfree(cmdtail);
	ckfree(cmd);
	return result;
}


/*
 *----------------------------------------------------------------------
 *
 * Env_AddStdSubcommands --
 *
 *	This procedure adds standard builtin subcommands to an environment.
 *
 * Results:
 *	none.
 *
 * Side effects:
 *	Adds builtin subcommands to the environment.
 *
 *----------------------------------------------------------------------
 */

static void
Env_AddStdSubcommands(env)
	Environment *env;			/* the environment to add subcommands to */
{
	Env_AddSubcommand(env, "debug", Env_DebugSubcmd, NULL);
	Env_AddSubcommand(env, "destroy", Env_DestroySubcmd, NULL);
	Env_AddSubcommand(env, "option", Env_OptionSubcmd, NULL);
	Env_AddSubcommand(env, "get", Env_SimpleGetSubcmd, NULL);
	Env_AddSubcommand(env, "set", Env_SimpleSetSubcmd, NULL);
	Env_AddSubcommand(env, "keys", Env_KeysSubcmd, NULL);
	Env_AddSubcommand(env, "delete", Env_DeleteSubcmd, NULL);
	Env_AddSubcommand(env, "command", Env_CommandSubcmd, NULL);
	Env_AddSubcommand(env, "unknown", Env_ErrUnknownSubcmd, NULL);
	Env_AddSubcommand(env, "exists", Env_ExistsSubcmd, NULL);
	Env_AddSubcommand(env, "_implicit", Env_ImplicitUnknownSubcmd, NULL);
	Env_AddSubcommand(env, "updateEntrant", Env_UpdateEntrantSubcmd, NULL);
}



/*
 *----------------------------------------------------------------------
 *
 * Env_AddSubcommand --
 *
 *	Add a single subcommand to an environment.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A subcommand is added.
 *
 *----------------------------------------------------------------------
 */
static void	Env_AddSubcommand(env, cmdName, builtinProc, tclProc)
	Environment *env;			/* the environment to add the command to */
	char *cmdName;				/* name of subcommand */
	Tcl_CmdProc *builtinProc;	/* C builtin proc or NULL */
	char *tclProc;				/* Tcl command or NULL */
{
	Tcl_HashEntry *entryPtr;
	SubCommand *subcommand;
	int new;

	/* validate arguments; just return if invalid */
	
	if (cmdName == NULL || ((builtinProc==NULL) && (tclProc==NULL)) ||
			((builtinProc!=NULL) && (tclProc!=NULL))) {
		return;
	}
	
	
	/* check for and remove any existing entry */

	entryPtr = Tcl_FindHashEntry(&env->subcmds, cmdName);
	if (entryPtr != NULL) {
		subcommand = (SubCommand *) Tcl_GetHashValue(entryPtr);
		Tcl_DeleteHashEntry(entryPtr);
		if (!subcommand->builtin && (subcommand->v.tclSubcommand != NULL)) {
			ckfree( subcommand->v.tclSubcommand );
		}
		ckfree( subcommand );
	}
		

	/* create the new entry */
	
	entryPtr = Tcl_CreateHashEntry(&env->subcmds, cmdName, &new);
	subcommand = (SubCommand *) ckalloc( sizeof(SubCommand) );
	if (builtinProc!=NULL) {
		subcommand->builtin = 1;
		subcommand->v.builtinSubcommand = builtinProc;
	} else {
		subcommand->builtin = 0;
		subcommand->v.tclSubcommand = (char *)ckalloc( strlen(tclProc)+1 );
		strcpy(subcommand->v.tclSubcommand, tclProc);
	}
	Tcl_SetHashValue(entryPtr, subcommand);
}


/*
 * the macro below concatenates prefix.name into fullname
 */
 
#define MAKENAME(fullname,prefix,name) \
	static char *fullname = NULL; \
	if (fullname==NULL) { \
		fullname = (char *) ckalloc( MAXKEYSIZE ); \
	} \
	if (prefix==NULL) { \
		strcpy(fullname, name); \
	} else { \
		strcpy(fullname, prefix); \
		strcat(fullname, "."); \
		strcat(fullname, name); \
	}



/*
 *----------------------------------------------------------------------
 *
 * Env_FindNode --
 *
 *	Find a node in the tree.
 *
 * Results:
 *	The node.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static EnvTree *
Env_FindNode(env, prefix, name)
	Environment *env;			/* environment to search in */
	char *prefix;				/* option prefix, e.g. "data" or NULL */
	char *name;					/* name of node to find */
{
	char *s;
	MAKENAME(fullname,prefix,name);
	return FindNode(env->tree, fullname);
}


static EnvTree *
FindNode(tree, name)
	EnvTree *tree;
	char *name;
{
	char word[MAXKEYPART];
	char *s, *p;
	EnvTree *cur;
	
	/* get up to "." or end of string of name */
	for (s=name, p=word; *s!='.' && *s!='\0'; s++, p++)
		*p = *s;
	*p = 0;
	
	/* search beneath current node for this word */
	for (cur = tree->children; cur!=NULL; cur=cur->next) {
		if (strcmp(cur->name, word)==0) {
			/* found it; recurse if we have to go deeper */
			if ((*s=='.') && (cur->children != NULL)) {
				return FindNode(cur, s+1);
			} else if (*s=='\0') {
				return cur;
			} else {
				return NULL;
			}
		}
	}
	return NULL;
}



/*
 *----------------------------------------------------------------------
 *
 * Env_CreateNode --
 *
 *	Create a new node in the tree.
 *
 * Results:
 *	Pointer to the new node.
 *
 * Side effects:
 *	A new node is created; old nodes may be destroyed.
 *
 *----------------------------------------------------------------------
 */

static EnvTree *
Env_CreateNode(env, prefix, name, new)
	Environment *env;			/* environment to create in */
	char *prefix;				/* option prefix, e.g. "data" or NULL */
	char *name;					/* name of node to create */
	int *new;					/* return if this is a brand new node */
	
{
	MAKENAME(fullname,prefix,name);
	*new = 1;	
	/* find/make place in tree */
	
	return CreateNode(env->tree, fullname, new);
}


static EnvTree *
CreateNode(tree, name, new)
	EnvTree *tree;
	char *name;
	int *new;
{
	char word[MAXKEYPART];
	char *s, *p;
	int needToGoDeeper;
	EnvTree *oldkids, *cur, *tmp, *next;
	
	/* get up to "." or end of string of name */
	for (s=name, p=word; *s!='.' && *s!='\0'; s++, p++)
		*p = *s;
	*p = 0;
	needToGoDeeper = (*s == '.');
	
	/* search beneath current node for this word */
	
	for (cur = tree->children; cur!=NULL; cur=cur->next) {
		if (strcmp(cur->name, word)==0) {
		
			if (cur->value != NULL) {
				ckfree(cur->value);
				cur->value = NULL;
			}
			if (needToGoDeeper) {
				return CreateNode(cur, s+1, new);
			} else {
				tmp = cur->children;  
				while (tmp!=NULL) {
					next = tmp->next;
					DeleteNodeAndKids(tmp);
					tmp = next;
				}
				*new = 0;
			    cur->children = NULL;
				return cur;
			}
			 
		}
	}
	if (cur==NULL)
		cur = tree;
	
	/* create a new node */
	
	oldkids = cur->children;
	tree->children = (EnvTree *) ckalloc(sizeof(EnvTree));
	tree->children->children = NULL;
	tree->children->value = NULL;
	tree->children->parent = tree;
	tree->children->next = oldkids;
	tree->children->name = (char *) ckalloc( strlen(word)+1 );
	strcpy(tree->children->name, word );
	
	if (needToGoDeeper) {
		return CreateNode(tree->children, s+1, new);
	} else {
		return tree->children;
	}
}



/*
 *----------------------------------------------------------------------
 *
 * Env_DeleteNode --
 *
 *	Delete a node in the tree.
 *
 * Results:
 *	1 = could delete node, 0 = node not found.
 *
 * Side effects:
 *	A node is deleted.
 *
 *----------------------------------------------------------------------
 */

static int
Env_DeleteNode(env, prefix, name)
	Environment *env;			/* environment to delete from */
	char *prefix;				/* option prefix, e.g. "data" or NULL */
	char *name;					/* name of node to delete */
{
	EnvTree *node, *kid;
	node = Env_FindNode(env, prefix, name);
	if (node!=NULL) {
		/* unhook from parent and then destroy */
		if (node->parent->children == node) {
			node->parent->children = node->next;
			/* if we leave the parent with no kids, fill in dummy value */
			if (node->next==NULL) {
				node->parent->value = (char *)ckalloc(1);
				node->parent->value[0] = '\0';
			} 
		} else {
			for (kid = node->parent->children; kid->next!=node && kid->next!=NULL; kid=kid->next) 
				;
			if (kid->next==node) {
				kid->next = node->next;
			}
		}
		DeleteNodeAndKids(node);
		return 1;
	}
	return 0;
}


static void
DeleteNodeAndKids(node) 
	EnvTree *node;
{
	EnvTree *kid, *next;
	
	if (node->name!=NULL)  ckfree( node->name );
	if (node->value!=NULL) ckfree ( node->value );
	for (kid = node->children; kid != NULL;  ) {
		next = kid->next;
		DeleteNodeAndKids(kid);
		kid = next;
	}
	ckfree (node);
}



/*
 *----------------------------------------------------------------------
 *
 * Env_DebugSubcmd --
 *
 *	Builtin handler for the debug subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Representation of the environment is dumped to stdout.
 *
 *----------------------------------------------------------------------
 */

static void debugNode _ANSI_ARGS_((EnvTree *node, int level));

static int
Env_DebugSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
    Environment *env = (Environment *)clientData;
    debugNode(env->tree, 0);
	return TCL_OK;
}

static void debugNode(node, level)
	EnvTree *node;
    int level;
{
	EnvTree *i;
	int j;

    for (j=0;j<level;j++) fprintf(stderr, "  ");
	if (node->name!=NULL) {fprintf(stderr, "%s", node->name);}
	if (node->value!=NULL) {fprintf(stderr, ": %s", node->value);}
    fprintf(stderr, "\n");
	for (i=node->children; i!=NULL; i=i->next) {
		debugNode(i,level+1);
    }
}



/*
 *----------------------------------------------------------------------
 *
 * Env_DestroySubcmd --
 *
 *	Builtin handler for the destroy subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The environment and all associated data is destroyed, and its
 *  Tcl command removed from the interpreter.
 *
 *----------------------------------------------------------------------
 */

static int
Env_DestroySubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Environment *env = (Environment *) clientData;
	Tcl_HashEntry *entryPtr;
	Tcl_HashSearch search;
	SubCommand *subcommand;
	
	if (argc != 2) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
				" ", argv[1], "\"", (char *) NULL);
		return TCL_ERROR;
	}
	
	DeleteNodeAndKids(env->tree);
	for (entryPtr = Tcl_FirstHashEntry(&env->subcmds, &search); entryPtr != NULL;
			entryPtr = Tcl_NextHashEntry(&search)) {
		subcommand = (SubCommand *) Tcl_GetHashValue(entryPtr);
		if (!subcommand->builtin && subcommand->v.tclSubcommand!=NULL) {
			ckfree (subcommand->v.tclSubcommand);
		}
		ckfree (subcommand);
	}
	Tcl_DeleteHashTable(&env->subcmds);

	Tcl_DeleteCommand(interp, argv[0]);
	return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * Env_OptionSubcmd --
 *
 *	Builtin handler for the option subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Information within the option data tree is set or retrieved.
 *
 *----------------------------------------------------------------------
 */

static int
Env_OptionSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	EnvTree *node, *kid;
	int new;
	char *result;

	Environment *env = (Environment *) clientData;

	if (argc<3) {
		Tcl_AppendResult(interp, "wrong # args: should be\"", argv[0],
			" ", argv[1], " subcommand ?options?\"", (char *) NULL);
		return TCL_ERROR;
	}
	
	if (strcmp(argv[2], "get")==0) {
		if (argc!=4) {
			Tcl_AppendResult(interp, "wrong # args: should be \"",
					argv[0], " ", argv[1], " get key\"", (char *) NULL);
			return TCL_ERROR;
		}
		node = Env_FindNode(env, "option", argv[3]);
		if (node==NULL) {
			/* non-existant option is fine */
			return TCL_OK;
		}
		result = createKeyedList(node);
		if (result!=NULL) Tcl_SetResult(interp, result, TCL_DYNAMIC);

	} else if (strcmp(argv[2], "set")==0) {
		if (argc!=5) {
			Tcl_AppendResult(interp, "wrong # args: should be \"",
					argv[0], " ", argv[1], " set key value\"", (char *) NULL);
			return TCL_ERROR;
		}
		node = Env_CreateNode(env, "option", argv[3], &new);
		if (node==NULL) {
			Tcl_AppendResult(interp, "could not create node", (char *) NULL);
			return TCL_ERROR;
		}
		node->value = (char *) ckalloc(strlen(argv[4])+1);
		strcpy(node->value, argv[4]);
	
	} else if (strcmp(argv[2], "delete")==0) { 
		if (argc!=4) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " delete key\"", (char *) NULL);
			return TCL_ERROR;
		}		
		if (Env_DeleteNode(env, "option", argv[3])==0) {
			Tcl_AppendResult(interp, argv[0], ": no such node", (char *)NULL);
			return TCL_ERROR;
		}

	} else if (strcmp(argv[2], "keys")==0) {
		if ((argc<3)||(argc>4)) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " keys ?key?\"", (char *) NULL);
			return TCL_ERROR;
		}
		if (argc==3) {
			node = Env_FindNode(env, NULL, "option");
		} else {
			node = Env_FindNode(env, "option", argv[3]);
		}
		if (node==NULL) {
			Tcl_AppendResult(interp, argv[0], ": no such node",
					(char *) NULL);
			return TCL_ERROR;
		}
		for (kid=node->children; kid!=NULL; kid=kid->next) {
			Tcl_AppendElement(interp, kid->name);
		}

	} else {
		Tcl_AppendResult(interp, argv[0], ": unknown ", argv[1], 
				" subcommand; should be one of get, set, delete, keys",
				(char *) NULL);
		return TCL_ERROR;
	}
	
	return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Env_SimpleGetSubcmd --
 *
 *	Simple builtin handler for the get subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
Env_SimpleGetSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	EnvTree *node;
	Environment *env = (Environment *) clientData;
	char *result;

	if (argc!=3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " ", argv[1], " key value\"", (char *) NULL);
		return TCL_ERROR;
	}
	if (strcmp(argv[2], "-root")==0) {
		node = Env_FindNode(env, NULL, "data");
	} else {
		node = Env_FindNode(env, "data", argv[2]);
	}
	if (node==NULL) {
		if (!ignoreErrors(env)) {
			Tcl_AppendResult(interp, "no such key", (char *)NULL);
			return TCL_ERROR;
		} else {
			return TCL_OK;
		}
	}	
	result = createKeyedList(node);
	if (result!=NULL) {
		Tcl_SetResult(interp, result, TCL_DYNAMIC);
	}
	return TCL_OK;
}


static char *createKeyedList(node)
	EnvTree *node;
{
	int kids, i;
	char **kidArray;
	char *copy, *result, *tmp;
	EnvTree *kidPtr;
	char *topLevel[3];

	if (node->children==NULL) {	
  		tmp = (node->value == NULL) ? "" : node->value;
		copy = (char *) ckalloc( strlen(tmp)+1 );
		strcpy(copy, tmp);
		return copy;
	} else {
		/* harder case.. walk kids.  first count them */
		for(kids=0,kidPtr=node->children; kidPtr!=NULL; kidPtr=kidPtr->next)
			kids++;
		kidArray = (char **) ckalloc (kids * sizeof(char *));

		/* now recurse for each kid */
		for(i=0,kidPtr=node->children; kidPtr!=NULL; kidPtr=kidPtr->next) {
			topLevel[0] = kidPtr->name;
			topLevel[1] = createKeyedList(kidPtr);
			kidArray[i++] = Tcl_Merge(2, topLevel);
			ckfree( topLevel[1] );
		}
		
		/* merge result */
		result = Tcl_Merge(kids, kidArray);

		/* free intermediate storage */
		for(i=0;i<kids;i++)
			ckfree( kidArray[i] );
		ckfree(kidArray);		

		return result;
	}

}


/*
 *----------------------------------------------------------------------
 *
 * Env_SimpleSetSubcmd --
 *
 *	Simple builtin handler for the set subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Data in the environment is modified.
 *
 *----------------------------------------------------------------------
 */

static int
Env_SimpleSetSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	int new;
	EnvTree *node;
	Environment *env = (Environment *) clientData;

	if (argc!=4) {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " ", argv[1], " key value\"", (char *) NULL);
		return TCL_ERROR;
	}

	node = Env_CreateNode(env, "data", argv[2], &new);
	if (node==NULL) {
		Tcl_AppendResult(interp, "could not create node", (char *)NULL);
		return TCL_ERROR;
	}	
	node->value = (char *)ckalloc(strlen(argv[3])+1);
	strcpy(node->value, argv[3]);
	return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * Env_KeysSubcmd --
 *
 *	Builtin handler for the keys subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
Env_KeysSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Environment *env = (Environment *) clientData;
	EnvTree *node, *kid;
	
	if (argc<2 || argc>3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0], " ", 
				argv[1], " ?key?\"", (char *) NULL);
		return TCL_ERROR;
	}
	if (argc == 2) {
		node = Env_FindNode(env, NULL, "data");
	} else {
		node = Env_FindNode(env, "data", argv[2]);
	}
	
	if (node == NULL) {
		if (!ignoreErrors(env)) {
			Tcl_AppendResult(interp, argv[0], ": no such node", (char *) NULL);
			return TCL_ERROR;
	 	} else {
			return TCL_OK;
		}
	}
	
	for (kid = node->children; kid!=NULL; kid = kid->next) {
		Tcl_AppendElement( interp, kid->name);
	}	
	return TCL_OK;	
}


/*
 *----------------------------------------------------------------------
 *
 * Env_DeleteSubcmd --
 *
 *	Builtin handler for the delete subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Information in the environment is deleted.
 *
 *----------------------------------------------------------------------
 */

static int
Env_DeleteSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Environment *env = (Environment *) clientData;
	
	if (argc!=3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0], " ",
				argv[1], " key\"", (char *) NULL);
		return TCL_ERROR;
	}
	
	if (Env_DeleteNode(env, "data", argv[2])==0) {
		if (!ignoreErrors(env)) {
			Tcl_AppendResult(interp, argv[0], ": no such node", (char *) NULL);
			return TCL_ERROR;
		}
	}
	return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * Env_ExistsSubcmd --
 *
 *	Builtin handler for the exists subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
Env_ExistsSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Environment *env = (Environment *) clientData;
	EnvTree *node;
	
	if (argc!=3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0], " ",
				argv[1], " key\"", (char *) NULL);
		return TCL_ERROR;
	}
	
	node = Env_FindNode(env, "data", argv[2]);
	if (node==NULL) {
		Tcl_SetResult(interp, "0", TCL_STATIC);
	} else {
		Tcl_SetResult(interp, "1", TCL_STATIC);
	}
	return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * Env_AddcmdSubcmd --
 *
 *	Builtin handler for the "command" subcommand.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

static int
Env_CommandSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Environment *env = (Environment *) clientData;
	Tcl_HashEntry *cmdEntryPtr;
	Tcl_HashSearch search;
	SubCommand *subcommand;
	char *s;
	
	if (argc<3) {
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
				" ", argv[1], " subcmd ?options?\"", (char *) NULL);
		return TCL_ERROR;
	}

	/*
	 * handle the "set" subcommand, by adding to the environment
	 */

	if (strcmp(argv[2], "set")==0) {
		if (argc!=5) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " set key value", (char *) NULL);
			return TCL_ERROR;
		} 
		s = (char *) ckalloc (strlen(argv[4])+1);
		strcpy(s, argv[4]);
		Env_AddSubcommand(env, argv[3], NULL, s);

	/*
	 * handle the "get" subcommand by retrieving a command string
 	 */
		
	} else if (strcmp(argv[2], "get")==0) {
		if (argc!=4) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " get key\"", (char *) NULL);
			return TCL_ERROR;
		} 
		cmdEntryPtr = Tcl_FindHashEntry(&env->subcmds, argv[3]);
		if (cmdEntryPtr==NULL) {
			Tcl_AppendResult(interp, argv[0], ": unknown subcommand",
					(char *) NULL);
			return TCL_ERROR;
		}
		subcommand = (SubCommand *) Tcl_GetHashValue(cmdEntryPtr);		
		if (subcommand->builtin) {
			Tcl_SetResult(interp, "<builtin>", TCL_STATIC);
		} else {
			Tcl_SetResult(interp, subcommand->v.tclSubcommand, TCL_VOLATILE);
		}

	/*
	 * handle the "delete" subcommand by removing a command
	 */

	} else if (strcmp(argv[2], "delete")==0) {
		if (argc!=4) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " delete key\"", (char *) NULL);
			return TCL_ERROR;
		}
		cmdEntryPtr = Tcl_FindHashEntry(&env->subcmds, argv[3]);	
		if (cmdEntryPtr==NULL) {
			Tcl_AppendResult(interp, argv[0], ": unknown subcommand",
					(char *) NULL);
			return TCL_ERROR;
		}
		Tcl_DeleteHashEntry(cmdEntryPtr);

	/*
	 * handle the "list" subcommand by showing all commands
	 */

	} else if (strcmp(argv[2], "list")==0) {
		if (argc!=3) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " list\"", (char *) NULL);
			return TCL_ERROR;
		}
		for(cmdEntryPtr = Tcl_FirstHashEntry(&env->subcmds, &search);
				cmdEntryPtr!=NULL; cmdEntryPtr = Tcl_NextHashEntry(&search)) {
			Tcl_AppendElement(interp, Tcl_GetHashKey(&env->subcmds, cmdEntryPtr));
		}

	/* 
	 * handle the "rename" command 
	 */

	} else if (strcmp(argv[2], "rename")==0) {
		if (argc!=5) {
			Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" ", argv[1], " rename oldname newname\"", 
					(char *) NULL);
			return TCL_ERROR;
		}
		cmdEntryPtr = Tcl_FindHashEntry(&env->subcmds, argv[3]);
		if (cmdEntryPtr==NULL) {
			Tcl_AppendResult(interp, argv[0], ": unknown subcommand",
					(char *) NULL);
			return TCL_ERROR;
		}
		subcommand = (SubCommand *) Tcl_GetHashValue(cmdEntryPtr);		
		if (subcommand->builtin) {
			Env_AddSubcommand(env, argv[4], subcommand->v.builtinSubcommand,
					NULL);
		} else {
			Env_AddSubcommand(env, argv[4], NULL, subcommand->v.tclSubcommand);
		}
		
	
	} else {
		Tcl_AppendResult(interp, "bad subcommand for ", argv[0], " ",
				argv[1], ": should be one of set, get, delete, list, rename",
				(char *) NULL);
		return TCL_ERROR;
	}

	return TCL_OK;	
}



/*
 *----------------------------------------------------------------------
 *
 * Env_ErrUnknownSubcmd --
 *
 *	Builtin handler for the "unknown" subcommand to generate errors.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

static int
Env_ErrUnknownSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	Tcl_SetResult(interp, "unknown subcommand", TCL_STATIC);
	return TCL_ERROR;
}



/*
 *----------------------------------------------------------------------
 *
 * Env_ImplicitUnknownSubcmd --
 *
 *	Builtin handler for the "unknown" subcommand to do implicit
 *  get/set commands.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

static int
Env_ImplicitUnknownSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char *argv[];	
{
	EnvTree *node;
	Environment *env = (Environment *)clientData;
	int new;
    char *newArgs[5];

	if (argc!=2 && argc!=3) {
		Tcl_SetResult(interp, "unknown subcommand", TCL_STATIC);
		return TCL_ERROR;
	}

	if (argc==2) {
		newArgs[0] = argv[0];
		newArgs[1] = "get";
		newArgs[2] = argv[1];
		newArgs[3] = NULL;
	} else {
		newArgs[0] = argv[0];
		newArgs[1] = "set";
		newArgs[2] = argv[1];
		newArgs[3] = argv[2];
		newArgs[4] = NULL;
	} 
	return EnvCmd(clientData, interp, (argc==2?3:4), newArgs);
}




/*
 *----------------------------------------------------------------------
 *
 * ignoreErrors
 *
 *	See if the option has been set to ignore certain errors.
 *
 * Results:
 *	1=ignore errors, 2=don't ignore
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

static int
ignoreErrors(env)
	Environment *env;
{
	EnvTree *node;
	node = FindNode(env->tree, "option.ignore_errors");
	if (node!=NULL) {
		if (node->value!=NULL) {
			if ((strcmp(node->value, "1")==0) ||
				(strcmp(node->value, "yes")==0) ||
				(strcmp(node->value, "on")==0)) {
				return 1;
			}
		}
	}
	return 0;
}




/* update entrant */
static int Env_UpdateEntrantSubcmd(clientData, interp, argc, argv)
	ClientData clientData;
	Tcl_Interp *interp;
	int argc;
	char **argv; 
{
	Environment *env = (Environment *) clientData;
	EnvTree *start;
	if (argc!=2) {
		Tcl_SetResult(interp, "update: wrong # args", TCL_STATIC);
		return TCL_ERROR;
	}
	start = Env_FindNode(env, NULL, "data");
	if (start!=NULL) {
		Tcl_DString cmd;
		Tcl_DStringInit(&cmd);
		Env_AuxUpdate(interp, start, &cmd, "");
		Tcl_SetResult(interp, Tcl_DStringValue(&cmd), TCL_VOLATILE);
		Tcl_DStringFree(&cmd);
	}
	return TCL_OK;
}

static void Env_AuxUpdate(interp, tree, cmd, key)
	Tcl_Interp *interp;
	EnvTree *tree;
	Tcl_DString *cmd;
	char *key;
{
	char buffer[100];
	EnvTree *nodePtr;
	for (nodePtr=tree->children; nodePtr!=NULL; nodePtr=nodePtr->next) {
		if (*key==0) {
			strcpy(buffer, nodePtr->name);
		} else {
			strcpy(buffer, key);
			strcat(buffer, ".");
			strcat(buffer, nodePtr->name);
		}
		if (nodePtr->value!=NULL) {
			Tcl_DStringAppendElement(cmd, buffer);
			Tcl_DStringAppendElement(cmd, nodePtr->value);
		} else {
			if (nodePtr->children!=NULL) {
				Env_AuxUpdate(interp, nodePtr, cmd, buffer);		
			}
		}
	}
}

