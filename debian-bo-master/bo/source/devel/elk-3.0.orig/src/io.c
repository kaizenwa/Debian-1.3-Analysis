/* Ports and I/O primitives.
 */

#include "kernel.h"

#include <errno.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>

#ifdef PATHCONF_PATH_MAX
#  include <unistd.h>
#endif

extern int errno;
extern char *getenv();

Object Curr_Input_Port, Curr_Output_Port;
Object Standard_Input_Port, Standard_Output_Port;

Init_Io () {
    Standard_Input_Port = Make_Port (P_INPUT, stdin, Make_String ("stdin", 5));
    Standard_Output_Port = Make_Port (0, stdout, Make_String ("stdout", 6));
    Curr_Input_Port = Standard_Input_Port;
    Curr_Output_Port = Standard_Output_Port;
    Global_GC_Link (Standard_Input_Port);
    Global_GC_Link (Standard_Output_Port);
    Global_GC_Link (Curr_Input_Port);
    Global_GC_Link (Curr_Output_Port);
}

Reset_IO (destructive) {
    Discard_Input (Curr_Input_Port);
    if (destructive)
	Discard_Output (Curr_Output_Port);
    else
	Flush_Output (Curr_Output_Port);
    Curr_Input_Port = Standard_Input_Port;
    Curr_Output_Port = Standard_Output_Port;
}

Object Make_Port (flags, f, name) FILE *f; Object name; {
    Object port;
    extern fclose();
    GC_Node;

    GC_Link (name);
    port = Alloc_Object (sizeof (struct S_Port), T_Port, 0);
    PORT(port)->flags = flags|P_OPEN;
    PORT(port)->file = f;
    PORT(port)->name = name;
    PORT(port)->ptr = 0;
    PORT(port)->lno = 1;
    PORT(port)->closefun = fclose;
    GC_Unlink;
    return port;
}

Object P_Port_File_Name (p) Object p; {
    Check_Type (p, T_Port);
    return (PORT(p)->flags & P_STRING) ? False : PORT(p)->name;
}

Object P_Port_Line_Number (p) Object p; {
    Check_Type (p, T_Port);
    return Make_Unsigned (PORT(p)->lno);
}

Object P_Eof_Objectp (x) Object x; {
    return TYPE(x) == T_End_Of_File ? True : False;
}

Object P_Current_Input_Port () { return Curr_Input_Port; }

Object P_Current_Output_Port () { return Curr_Output_Port; }

Object P_Input_Portp (x) Object x; {
    return TYPE(x) == T_Port && IS_INPUT(x) ? True : False;
}

Object P_Output_Portp (x) Object x; {
    return TYPE(x) == T_Port && IS_OUTPUT(x) ? True : False;
}

int Path_Max () {
#ifdef PATH_MAX          /* POSIX */
    return PATH_MAX;
#else
#ifdef MAXPATHLEN        /* 4.3 BSD */
    return MAXPATHLEN;
#else
#ifdef PATHCONF_PATH_MAX
    static r; 
    if (r == 0) {
	if ((r = pathconf ("/", _PC_PATH_MAX)) == -1)
	    r = 1024;
	r++;
    }
    return r;
#else
    return 1024;
#endif
#endif
#endif
}

Object Get_File_Name (name) Object name; {
    register len;

    if (TYPE(name) == T_Symbol)
	name = SYMBOL(name)->name;
    else if (TYPE(name) != T_String)
	Wrong_Type_Combination (name, "string or symbol");
    if ((len = STRING(name)->size) > Path_Max () || len == 0)
	Primitive_Error ("invalid file name");
    return name;
}

char *Internal_Tilde_Expand (s, dirp) register char *s, **dirp; {
    register char *p; 
    struct passwd *pw, *getpwnam();

    if (*s++ != '~')
	return 0;
    for (p = s; *p && *p != '/'; p++)
	;
    if (*p == '/') *p++ = 0;
    if (*s == '\0') {
	if ((*dirp = getenv ("HOME")) == 0)
	    *dirp = "";
    } else {
	if ((pw = getpwnam (s)) == 0)
	    Primitive_Error ("unknown user: ~a", Make_String (s, strlen (s)));
	*dirp = pw->pw_dir;
    } 
    return p;
}

Object General_File_Operation (s, op) Object s; register op; {
    register char *r;
    Object ret, fn;
    Alloca_Begin;

    fn = Get_File_Name (s);
    Get_Strsym_Stack (fn, r);
    switch (op) {
    case 0: {
	char *p, *dir;
	if ((p = Internal_Tilde_Expand (r, &dir)) == 0) {
	    Alloca_End;
	    return s;
	}
	Alloca (r, char*, strlen (dir) + 1 + strlen (p) + 1);
	sprintf (r, "%s/%s", dir, p);
	ret = Make_String (r, strlen (r));
	Alloca_End;
	return ret;
    }
    case 1: {
	struct stat st;
	/* Doesn't make much sense to check for errno != ENOENT here:
	 */
	ret = stat (r, &st) == 0 ? True : False;
	Alloca_End;
	return ret;
    }}
    /*NOTREACHED*/
}

Object P_Tilde_Expand (s) Object s; {
    return General_File_Operation (s, 0);
}

Object P_File_Existsp (s) Object s; {
    return General_File_Operation (s, 1);
}

Close_All_Files () {
    Terminate_Type (T_Port);
}

Object Terminate_File (port) Object port; {
    (void)(PORT(port)->closefun) (PORT(port)->file);
    PORT(port)->flags &= ~P_OPEN;
    return Void;
}

Object Open_File (name, flags, err) char *name; {
    register FILE *f;
    char *dir, *p;
    Object fn, port;
    struct stat st;
    Alloca_Begin;

    if (p = Internal_Tilde_Expand (name, &dir)) {
	Alloca (name, char*, strlen (dir) + 1 + strlen (p) + 1);
	sprintf (name, "%s/%s", dir, p);
    }
    if (!err && stat (name, &st) == -1 &&
	    (errno == ENOENT || errno == ENOTDIR)) {
	Alloca_End;
	return Null;
    }
    switch (flags & (P_INPUT|P_BIDIR)) {
    case 0:               p = "w";  break;
    case P_INPUT:         p = "r";  break;
    default:              p = "r+"; break;
    }
    fn = Make_String (name, strlen (name));
    Disable_Interrupts;
    if ((f = fopen (name, p)) == NULL) {
	Saved_Errno = errno;  /* errno valid here? */
	Primitive_Error ("~s: ~E", fn);
    }
    port = Make_Port (flags, f, fn);
    Register_Object (port, (GENERIC)0, Terminate_File, 0);
    Enable_Interrupts;
    Alloca_End;
    return port;
}

Object General_Open_File (name, flags, path) Object name, path; {
    Object port, pref;
    char *buf = 0;
    register char *fn;
    register plen, len, blen = 0, gotpath = 0;
    Alloca_Begin;

    name = Get_File_Name (name);
    len = STRING(name)->size;
    fn = STRING(name)->data;
    if (fn[0] != '/' && fn[0] != '~') {
	for ( ; TYPE(path) == T_Pair; path = Cdr (path)) {
	    pref = Car (path);
	    if (TYPE(pref) == T_Symbol)
		pref = SYMBOL(pref)->name;
	    if (TYPE(pref) != T_String)
		continue;
	    gotpath = 1;
	    if ((plen = STRING(pref)->size) > Path_Max () || plen == 0)
		continue;
	    if (len + plen + 2 > blen) {
		blen = len + plen + 2;
		Alloca (buf, char*, blen);
	    }
	    bcopy (STRING(pref)->data, buf, plen);
	    if (buf[plen-1] != '/')
		buf[plen++] = '/';
	    bcopy (fn, buf+plen, len);
	    buf[len+plen] = '\0';
	    port = Open_File (buf, flags, 0);
	    /* No GC has been taken place in Open_File() if it returns Null.
	     */
	    if (!Nullp (port)) {
		Alloca_End;
		return port;
	    }
	}
    }
    if (gotpath)
	Primitive_Error ("file ~s not found", name);
    if (len + 1 > blen)
	Alloca (buf, char*, len + 1);
    bcopy (fn, buf, len);
    buf[len] = '\0';
    port = Open_File (buf, flags, 1);
    Alloca_End;
    return port;
}

Object P_Open_Input_File (name) Object name; {
    return General_Open_File (name, P_INPUT, Null);
}

Object P_Open_Output_File (name) Object name; {
    return General_Open_File (name, 0, Null);
}

Object P_Open_Input_Output_File (name) Object name; {
    return General_Open_File (name, P_BIDIR, Null);
}

Object General_Close_Port (port) Object port; {
    register flags, err = 0;
    FILE *f;

    Check_Type (port, T_Port);
    flags = PORT(port)->flags;
    if (!(flags & P_OPEN) || (flags & P_STRING))
	return Void;
    f = PORT(port)->file;
    if (f == stdin || f == stdout)
	return Void;
    if ((PORT(port)->closefun) (f) == EOF) {
	Saved_Errno = errno;   /* errno valid here? */
	err++;
    }
    PORT(port)->flags &= ~P_OPEN;
    Deregister_Object (port);
    if (err)
	Primitive_Error ("write error on ~s: ~E", port);
    return Void;
}

Object P_Close_Input_Port (port) Object port; {
    return General_Close_Port (port);
}

Object P_Close_Output_Port (port) Object port;{
    return General_Close_Port (port);
}

#define General_With(prim,curr,flags) Object prim (name, thunk)\
	Object name, thunk; {\
    Object old, ret;\
    GC_Node2;\
\
    Check_Procedure (thunk);\
    old = curr;\
    GC_Link2 (thunk, old);\
    curr = General_Open_File (name, flags, Null);\
    ret = Funcall (thunk, Null, 0);\
    (void)General_Close_Port (curr);\
    GC_Unlink;\
    curr = old;\
    return ret;\
}

General_With (P_With_Input_From_File, Curr_Input_Port, P_INPUT)
General_With (P_With_Output_To_File, Curr_Output_Port, 0)

Object General_Call_With (name, flags, proc) Object name, proc; {
    Object port, ret;
    GC_Node2;

    Check_Procedure (proc);
    GC_Link2 (proc, port);
    port = General_Open_File (name, flags, Null);
    port = Cons (port, Null);
    ret = Funcall (proc, port, 0);
    (void)General_Close_Port (Car (port));
    GC_Unlink;
    return ret;
}

Object P_Call_With_Input_File (name, proc) Object name, proc; {
    return General_Call_With (name, P_INPUT, proc);
}

Object P_Call_With_Output_File (name, proc) Object name, proc; {
    return General_Call_With (name, 0, proc);
}

Object P_Open_Input_String (string) Object string; {
    Check_Type (string, T_String);
    return Make_Port (P_STRING|P_INPUT, (FILE *)0, string);
}

Object P_Open_Output_String () {
    return Make_Port (P_STRING, (FILE *)0, Make_String ((char *)0, 0));
}
