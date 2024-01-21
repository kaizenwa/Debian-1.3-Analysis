
#ifdef MSDOS
#pragma comment (lib, "icmun")
#pragma comment (lib, "../rss/icrss")
#endif

#include "icmun.h"

void main (argc, argv)
int argc;
char **argv;
{
    register char
        *progname,
        *infname;
    static char
        bimext [] = ".bim";

    copyright ("ICMAKE Binary Make File Unassembler", version, release, 1);

    if (argc != 2)
    {
        progname = program_name (argv [0]);
        printf ("Usage: %s bimfile\n"
                "where: bimfile - binary makefile (default extension: %s)\n\n"
            , progname, bimext);
        exit (2);
    }

    infname = change_ext (argv [1], bimext);

    if (! (infile = fopen (infname, READBINARY)) )
        error ("cannot open %s for reading", infname);

    headerp = readheader (infile, version [0]);

    if ((INT16)(nvar = getvar (infile, headerp, &var)) == -1 )
        error ("invalid macro file, cannot read variable section");

    process ();

    exit (0);
}
