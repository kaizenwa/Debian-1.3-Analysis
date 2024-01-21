#include AOUT_H
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>

#ifndef O_BINARY
#  define O_BINARY 0
#endif

#ifdef ECOFF
#  ifdef CACHECTL_H
#    include CACHECTL_H
#  endif

struct headers {
    struct filehdr fhdr;
    struct aouthdr aout;
    struct scnhdr section[3];    
};
#endif

extern void *sbrk();
extern char *getenv();

static char *Loader_Output;
static char *tmpdir;

Load_Object (names) Object names; {
#ifdef ECOFF
    struct headers hdr;
#else
    struct exec hdr;
#endif
    register char *brk, *obrk, *lp, *li;
    char *buf;
    register n, f, len, liblen;
    Object port, tail, fullnames, libs;
    FILE *fp;
    GC_Node3;
    Alloca_Begin;

    li = Loader_Input;
    if (!li)
	li = A_Out_Name;
    if (!Loader_Output) {
	if (!(tmpdir = getenv ("TMPDIR")))
	    tmpdir = "/tmp";
	Loader_Output = Safe_Malloc (strlen (tmpdir) + 20);
    }
    sprintf (Loader_Output, "%s/ldXXXXXX", tmpdir);
    (void)mktemp (Loader_Output);

    port = tail = fullnames = Null;
    GC_Link3 (port, tail, fullnames);
    for (len = 0, tail = names; !Nullp (tail); tail = Cdr (tail)) {
	port = General_Open_File (Car (tail), P_INPUT, Var_Get (V_Load_Path));
	fullnames = Cons (PORT(port)->name, fullnames);
	len += STRING(Car (fullnames))->size + 1;
	(void)P_Close_Input_Port (port);
    }
    GC_Unlink;

    libs = Var_Get (V_Load_Libraries);
    if (TYPE(libs) == T_String) {
        liblen = STRING(libs)->size;
	lp = STRING(libs)->data;
    } else {
	liblen = 3; lp = "-lc";
    }

    Alloca (buf, char*, strlen (A_Out_Name) + len + liblen + 100);

    obrk = brk = (char *)sbrk (0);
    brk = (char *)((int)brk + 7 & ~7);

#if defined(hp9000s300) || defined(__hp9000s300) || defined(__hp9000s300__)
    sprintf (buf, "%s -N %s -A %s -R %x -o %s ",
#else
    sprintf (buf, "%s -N %s -A %s -T %x -o %s ",
#endif
	LD_NAME, INC_LDFLAGS, li, (unsigned)brk, Loader_Output);

    for (tail = fullnames; !Nullp (tail); tail = Cdr (tail)) {
	register struct S_String *str = STRING(Car (tail));
	strncat (buf, str->data, str->size);
	strcat (buf, " ");
    }
    strncat (buf, lp, liblen);

    if (Verb_Load)
	printf ("[%s]\n", buf);
    if (system (buf) != 0) {
	(void)unlink (Loader_Output);
	Primitive_Error ("system linker failed");
    }
    Disable_Interrupts;               /* To ensure that f gets closed */
    if ((f = open (Loader_Output, O_RDONLY|O_BINARY)) == -1) {
	(void)unlink (Loader_Output);
	Primitive_Error ("cannot open tempfile");
    }
    if (Loader_Input)
	(void)unlink (Loader_Input);
    else
	Loader_Input = Safe_Malloc (strlen (tmpdir) + 20);
    strcpy (Loader_Input, Loader_Output);
    if (read (f, (char *)&hdr, sizeof (hdr)) != sizeof (hdr)) {
err:
	close (f);
	Primitive_Error ("corrupt tempfile (`ld' is broken)");
    }
#ifdef ECOFF
    n = hdr.aout.tsize + hdr.aout.dsize + hdr.aout.bsize;
#else
    n = hdr.a_text + hdr.a_data + hdr.a_bss;
#endif
    if ((char *)sbrk (n + brk-obrk) == (char *)-1) {
	close (f);
	Primitive_Error ("not enough memory to load object file");
    }
    bzero (brk, n);
#ifdef ECOFF
    n -= hdr.aout.bsize;
    (void)lseek (f, (off_t)hdr.section[0].s_scnptr, 0);
#else
    n -= hdr.a_bss;
#endif
    if (read (f, brk, n) != n)
	goto err;
    if ((fp = fdopen (f, O_BINARY ? "rb" : "r")) == NULL) {
	close (f);
	Primitive_Error ("cannot fdopen object file");
    }
    if (The_Symbols)
	Free_Symbols (The_Symbols);
    The_Symbols = Snarf_Symbols (fp, &hdr);
    (void)fclose (fp);
#if defined(ECOFF) && defined(CACHECTL_H)
    if (cacheflush (brk, n, BCACHE) == -1) {
	extern int errno;
	Saved_Errno = errno;
	Primitive_Error ("cacheflush failed: ~E");
    }
#endif
    Call_Initializers (The_Symbols, brk, PR_CONSTRUCTOR);
    Call_Initializers (The_Symbols, brk, PR_EXTENSION);
    Enable_Interrupts;
    Alloca_End;
}

void Finit_Load () {
    if (Loader_Input)
	(void)unlink (Loader_Input);
}

void Fork_Load () {
    char *newlink;

    if (Loader_Input) {
	Disable_Interrupts;
	newlink = Safe_Malloc (strlen (tmpdir) + 20);
	sprintf (newlink, "%s/ldXXXXXX", tmpdir);
	(void)mktemp (newlink);
	(void)link (Loader_Input, newlink);
	free (Loader_Input);
	Loader_Input = newlink;
	Enable_Interrupts;
    }
}
