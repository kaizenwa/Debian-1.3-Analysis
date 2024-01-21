#include "kernel.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef MAX_STACK_SIZE
#  include <sys/time.h>
#  include <sys/resource.h>
#endif

#ifdef FIND_AOUT
#  ifdef INCLUDE_UNISTD_H
#    include <unistd.h>
#  else
#    include <sys/file.h>
#  endif
#endif

extern char *getenv();

char *stkbase;
int Stack_Grows_Down;
int Max_Stack;
int Interpreter_Initialized;
int GC_Debug = 0;
int Case_Insensitive;
int Verb_Load, Verb_Init;

char **Argv;
int Argc, First_Arg;

#ifdef FIND_AOUT
char *A_Out_Name;
char *Find_Executable();
#endif

#if defined(CAN_LOAD_OBJ) || defined(INIT_OBJECTS)
SYMTAB *The_Symbols;
#endif

void Exit_Handler () {
#if defined(CAN_LOAD_OBJ) || defined(INIT_OBJECTS)
    Call_Finalizers ();
#endif
#ifdef CAN_LOAD_OBJ
    Finit_Load ();
#endif
}

#ifndef ATEXIT
/* Hack: __GNUC_MINOR__ was introduced together with __attribute__ */
#ifdef __GNUC_MINOR__
extern void _exit() __attribute__ ((noreturn));
#endif
#ifndef PROFILING
void exit (n) {
    Exit_Handler ();
    _cleanup ();
    _exit (n);
}
#endif
#endif

#ifdef CAN_DUMP
int Was_Dumped;
char *Brk_On_Dump;
#endif


/* dump currently does not work for applications using Elk_Init().
 * The reason is that in this case the INITIAL_STK_OFFSET which
 * compensates for differences in argv[] in the original/dumped a.out
 * is not in effect (see comment below).
 * This cannot be fixed without changing Elk_Init() and its use in
 * an incompatible way.
 */
Check_If_Dump_Works () {
#ifdef NOMAIN
    Primitive_Error ("not yet supported for standalone applications");
#endif
}


#ifdef NOMAIN

void Elk_Init (ac, av, init_objects, toplevel) char **av, *toplevel; {

#else

main (ac, av) char **av; {

#endif

/* To avoid that the stack copying code overwrites argv if a dumped
 * copy of the interpreter is invoked with more arguments than the
 * original a.out, move the stack base INITIAL_STK_OFFSET bytes down.
 * The call to bzero() is there to prevent the optimizer from removing
 * the array.
 */
#ifdef CAN_DUMP
    char unused[INITIAL_STK_OFFSET];
#endif
    char *initfile, *loadfile = 0, *loadpath = 0;
    int debug = 0, heap = HEAP_SIZE;
    Object file;
    struct stat st;
    extern int errno;
    char foo;
#ifdef NOMAIN
#  define foo (av[0][0])
#endif

#ifdef CAN_DUMP
    bzero (unused, 1);  /* see comment above */
#endif
    if (ac == 0) {
	av[0] = "Elk"; ac = 1;
    }
    Get_Stack_Limit ();

#ifdef FIND_AOUT
    A_Out_Name = Find_Executable (av[0]);
#endif

    Argc = ac; Argv = av;
    First_Arg = 1;
#ifdef CAN_DUMP
    if (Was_Dumped) {
	/* Check if beginning of stack has moved by a large amount.
	 * This is the case, for instance, on a Sun-4m when the
	 * interpreter was dumped on a Sun-4c and vice versa.
	 */
	if (abs (stkbase - &foo) > INITIAL_STK_OFFSET) {
	    fprintf (stderr,
"Can't restart dumped interpreter from a different machine architecture\n");
	    fprintf (stderr,
"   (Stack delta = %d bytes).\n", stkbase - &foo);
	    exit (1);
	}
	/* Check if program break must be reset.
	*/
	if (Brk_On_Dump && (char *)brk (Brk_On_Dump) == (char *)-1) {
	    perror ("brk"); exit (1);
	}
#if defined(HP9K) && defined(CAN_DUMP) && defined(HPSHLIB)
	Restore_Shared_Data ();
#endif
#ifdef GENERATIONAL_GC
	Generational_GC_Reinitialize ();
#endif
	Loader_Input = 0;
	Install_Intr_Handler ();
	(void)Funcall_Control_Point (Dump_Control_Point, Arg_True, 0);
	/*NOTREACHED*/
    }
#endif

    for ( ; First_Arg < ac; First_Arg++) {
	if (strcmp (av[First_Arg], "-g") == 0) {
	    debug = 1;
	} else if (strcmp (av[First_Arg], "-i") == 0) {
	    Case_Insensitive = 1;
	} else if (strcmp (av[First_Arg], "-v") == 0) {
	    if (++First_Arg == ac)
		Usage ();
	    if (strcmp (av[First_Arg], "load") == 0)
		Verb_Load = 1;
	    else if (strcmp (av[First_Arg], "init") == 0)
		Verb_Init = 1;
	    else Usage ();
	} else if (strcmp (av[First_Arg], "-h") == 0) {
	    if (++First_Arg == ac)
		Usage ();
	    if ((heap = atoi (av[First_Arg])) <= 0) {
		fprintf (stderr, "Heap size must be a positive number.\n");
		exit (1);
	    }
	} else if (strcmp (av[First_Arg], "-l") == 0) {
	    if (++First_Arg == ac || loadfile)
		Usage ();
	    loadfile = av[First_Arg];
	} else if (strcmp (av[First_Arg], "-p") == 0) {
	    if (++First_Arg == ac || loadpath)
		Usage ();
	    loadpath = av[First_Arg];
	} else if (strcmp (av[First_Arg], "--") == 0) {
	    First_Arg++;
	    break;
	} else if (av[First_Arg][0] == '-') {
	    Usage ();
	} else {
	    break;
	}
    }

    stkbase = &foo;
    Stack_Grows_Down = Check_Stack_Grows_Down ();
    ALIGN(stkbase);
    Make_Heap (heap);
    Init_Everything ();
#ifdef ATEXIT
    if (atexit (Exit_Handler) != 0)
	Fatal_Error ("atexit returned non-zero value");
#endif
#ifdef INIT_OBJECTS
#ifdef NOMAIN
    if (init_objects) {
	Set_Error_Tag ("init-objects");
	The_Symbols = Open_File_And_Snarf_Symbols (A_Out_Name);
	Call_Initializers (The_Symbols, (char *)0, PR_EXTENSION);
    }
#else
    Set_Error_Tag ("init-objects");
    The_Symbols = Open_File_And_Snarf_Symbols (A_Out_Name);
    Call_Initializers (The_Symbols, (char *)0, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, (char *)0, PR_EXTENSION);
#endif
#endif
    if (loadpath || (loadpath = getenv (LOADPATH_ENV)))
	Init_Loadpath (loadpath);
    
    /* The following code is sort of a hack.  initscheme.scm should not
     * be resolved against load-path.  However, the .scm-files may not
     * have been installed yet (note that the interpreter is already
     * used in the "make" process).
     * Solution: if initscheme.scm hasn't been installed yet, do search
     * the load-path, so that -p can be used.
     */
    Set_Error_Tag ("scheme-init");
    initfile = Safe_Malloc (strlen (SCM_DIR) + 1 + sizeof (INITFILE) + 1);
    sprintf (initfile, "%s/%s", SCM_DIR, INITFILE);
    if (stat (initfile, &st) == -1 && errno == ENOENT)
	file = Make_String (INITFILE, sizeof(INITFILE)-1);
    else
	file = Make_String (initfile, strlen (initfile));
    free (initfile);
    (void)General_Load (file, The_Environment);

    Install_Intr_Handler ();

    Set_Error_Tag ("top-level");
#ifdef NOMAIN
    if ((loadfile = toplevel) == 0) {
	Interpreter_Initialized = 1;
	GC_Debug = debug;
	return;
    }
#endif
    if (loadfile == 0)
	loadfile = "toplevel.scm";
    file = Make_String (loadfile, strlen (loadfile));
    Interpreter_Initialized = 1;
    GC_Debug = debug;
    if (loadfile[0] == '-' && loadfile[1] == '\0')
	Load_Source_Port (Standard_Input_Port);
    else
	(void)General_Load (file, The_Environment);
#ifndef NOMAIN
    return 0;
#endif
}

static char *Usage_Msg[] = {
    "Options:",
    "   [-l filename]   Load file instead of standard toplevel",
    "   [-l -]          Load from standard input",
    "   [-h heapsize]   Heap size in KBytes",
    "   [-p loadpath]   Initialize load-path (colon-list of directories)",
    "   [-g]            Enable GC-debugging",
    "   [-i]            Case-insensitive symbols",
    "   [-v type]       Be verbose.  \"type\" controls what to print:",
    "                      load   linker command when loading object file",
    "                      init   names of extension [f]init functions when \
called",
    "   [--]            End options and begin arguments",
    0 };

Usage () {
    char **p;

    fprintf (stderr, "Usage: %s [options] [arguments]\n", Argv[0]);
    for (p = Usage_Msg; *p; p++)
	fprintf (stderr, "%s\n", *p);
    exit (1);
}

Init_Everything () {
    Init_Type ();
    Init_Cstring ();
    Init_String ();
    Init_Symbol ();
    Init_Env ();
    Init_Error ();
    Init_Exception ();
    Init_Io ();
    Init_Prim ();
    Init_Math ();
    Init_Print ();
    Init_Auto ();
    Init_Heap ();
    Init_Load ();
    Init_Proc ();
    Init_Special ();
    Init_Read ();
    Init_Features ();
    Init_Terminate ();
#ifdef CAN_DUMP
    Init_Dump ();
#endif
}

Get_Stack_Limit () {
#ifdef MAX_STACK_SIZE
    Max_Stack = MAX_STACK_SIZE;
#else
    struct rlimit rl;

    if (getrlimit (RLIMIT_STACK, &rl) == -1) {
	perror ("getrlimit");
	exit (1);
    }
    Max_Stack = rl.rlim_cur;
#endif
    Max_Stack -= STACK_MARGIN;
}

#ifdef FIND_AOUT
Executable (fn) char *fn; {
    struct stat s;

    return stat (fn, &s) != -1 && (s.st_mode & S_IFMT) == S_IFREG
	    && access (fn, X_OK) != -1;
}

char *Find_Executable (fn) char *fn; {
    char *path, *dir, *getenv();
    static char buf[1025];  /* Can't use Path_Max or Safe_Malloc here */
    register char *p;

    for (p = fn; *p; p++) {
	if (*p == '/') {
	    if (Executable (fn))
		return fn;
	    else
		Fatal_Error ("%s is not executable", fn);
	}
    }
    if ((path = getenv ("PATH")) == 0)
	path = ":/usr/ucb:/bin:/usr/bin";
    dir = path;
    do {
	p = buf;
	while (*dir && *dir != ':')
	    *p++ = *dir++;
	if (*dir)
	    ++dir;
	if (p > buf)
	    *p++ = '/';
	strcpy (p, fn);
	if (Executable (buf))
	    return buf;
    } while (*dir);
    if (dir > path && dir[-1] == ':' && Executable (fn))
	return fn;
    Fatal_Error ("cannot find pathname of %s", fn);
    /*NOTREACHED*/
}
#endif

Object P_Command_Line_Args () {
    Object ret, tail;
    register i;
    GC_Node2;

    ret = tail = P_Make_List (Make_Integer (Argc-First_Arg), Null);
    GC_Link2 (ret, tail);
    for (i = First_Arg; i < Argc; i++, tail = Cdr (tail)) {
	Object a;
	
	a = Make_String (Argv[i], strlen (Argv[i]));
	Car (tail) = a;
    }
    GC_Unlink;
    return ret;
}

Object P_Exit (argc, argv) Object *argv; {
    exit (argc == 0 ? 0 : Get_Unsigned (argv[0]));
    /*NOTREACHED*/
}
