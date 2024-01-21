/*
                         I C M - C O M P . C
*/

#include "iccomp.h"

#pragma comment(lib, "icmcomp")
#pragma comment(lib, "../rss/icrss")

int main (argc, argv)            /* icm-comp source(txt) dest(bin) */
    int
        argc;
    char
        **argv;
{
    if (argc != 3)
    {
        copyright("Make Optimizing Compiler", version, release, 1);
        error("Usage: %s source dest\n"
              "where:\n"
              "\tsource: source file to compile\n"
              "\t        (normally output from ICM-PP)\n"
              "\tdest:   name of binary file to generate\n"
              ,
              program_name(argv[0]));
    }

    if
    (
        !(yyin = fopen(argv[1], "rt"))     /* open text file for parsing */
        ||
        !(s_bin = fopen(argv[2], "w+b"))   /* open binary file to write/read */
    )
        error("%s Can't read/write file(s)");

                                            /* malloc the dead-stack */
    dead = xrealloc(NULL, sizeof(unsigned));

    stringbuf = xstrdup(nullstring);        /* malloc initial stringbuf */

                                            /* go to first codebyte pos */
    fseek(s_bin, sizeof(BIN_HEADER_), SEEK_SET);

    yyparse();                              /* parse the source */

    if (!yynerr)                            /* backend if no errors */
        backend();
    else                                    /* informative message */
        printf("\n%d error(s) detected\n", errcount);

    return(yynerr != 0);                    /* returnvalue */
}
