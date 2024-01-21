/*
                         I C M A K E . C
*/

#include "icmake.h"

#ifdef MSDOS
#   pragma comment(lib, "icmake")
#   pragma comment(lib, "..\\rss\\icrss")
#endif

static char
    bim[]       = "bim",
    icm_comp[]  = ICMCOMP,
    icm_exec[]  = ICMEXEC,
    icm_pp[]    = ICMPP,
    pim[]       = "pim";

int main (argc, argv)              /* icmake source(txt) dest(bin) */
    int
        argc;
    char
        **argv;
{
    register int
        argc2,
        ret;
    char
        *prog;

    argc2 = options(argv, &argc);           /* process options */
                                            /* argc2 is index of args to
                                               icm-exec
                                            */
    if (!(flags & f_quiet))
        copyright("Make Utility", version, release, 1);

    prog = program_name(argv[0]);

    if (!(flags & f_icmake) && argc2 == 1)  /* argv[1]: already for icm-exec */
        error
        (
            "Icmake by Frank B. Brokken and Karel Kubat.\n"
            "\n"
            "Usage: %s [flags] source[.im] [dest[.bim]] [-- [args]]\n"
            "where:\n"
            "\tflags:  optional flags:\n"
            "\t\t-a     : information about %s\n"
            "\t\t-b     : blunt execution of the destinationfile\n"
            "\t\t-c     : the destination file is compiled\n"
            "\t\t-i file: 'file': name of source, argument processing stops\n"
#ifdef MSDOS
            "\t\t-o file: all icmake output is redirected to `file'\n"
#endif
            "\t\t-p     : only the preprocessor is activated\n"
            "\t\t-q     : quiet mode: copyright banner not displayed\n"
#ifndef MSDOS
            "\t\t-t file: 'file' is used as a temporary bim-file, to be removed\n"
            "\t\t         on exit. Argument processing stops.\n"
#endif
            "\tsource: make description source file (default extension: .im)\n"
            "\tdest:   binary make file             (default:    source.bim)\n"
            "\t        (not used with the -t option)\n"
            "\t-- :   optional icmake-file arguments separator\n"
            "\targs:  optional arguments following -- received by\n"
            "\t       the icmake file in its argv-list"
            , prog
            , prog
        );

#ifdef MSDOS
    if (redirect_nr != ~0)
    {
        if
        (
            !redirect_start(fileno(stderr), redirect_nr)
            ||
            !redirect_start(fileno(stdout), redirect_nr)
        )
            error("Output redirection to file fails");

        copyright("Make Utility", version, release, 1);
    }
#endif

    if (!(flags & f_icmake))                /* do not take source literally */
    {
        source_name =                       /* determine source */
            change_ext(argv[1], "im");      /* update the extension    */

        if (!(flags & f_tmpbim))            /* only if not a temp. bimfile */
            dest_name =
                argc2 >= 3 ?
                    argv[2]
                :
                    argv[1];
    }
    else if (!(flags & f_tmpbim))           /* only if not a temp. bimfile */
        dest_name = source_name;

    if (!(flags & f_tmpbim))                /* adapt extension of destination */
        dest_name = change_ext(dest_name, bim); /* if not tmp. bimfile */

    if
    (
        !(flags & f_blunt)                  /* no forced execution */
        &&
        compile_test()                      /* compilation needed */
    )
    {
                                            /* preprocessor filename */
        if (!(flags & f_tmpbim))            /* use .pim unless -t given */
            temporary = change_ext(source_name, pim);

        signal(SIGINT, abnormal_exit);      /* abnormal exits process */
                                            /* do the preprocessing */
        ret = _spawnlp(P_WAIT, icm_pp, icm_pp, source_name, temporary, NULL);
        if (ret)
        {
            if (ret == -1)
                spawn_err(icm_pp);
            cleanup();
            return (1);
        }

        if (flags & f_preprocessor)
            return (0);

                                            /* do the compilation */
        errors = _spawnlp(P_WAIT, icm_comp, icm_comp,
                                  temporary, dest_name, NULL);
        cleanup();

        if (errors)
        {
            if (errors == -1)
                spawn_err(icm_comp);
            return (1);
        }

        if (flags & f_compiler)
            return (0);
    }

    if (flags & f_tmpbim)                       /* -t used */
    {
        argc2 -= 3;                             /* set index to start of args */
        argv[argc2 + 1] = "-t";                 /* signal temporary */
        argv[argc2 + 2] = dest_name;            /* store dest-name */
    }
    else
    {
        argc2 -= 2;                             /* set index to start of args */
        argv[argc2 + 1] = dest_name;            /* store dest-name */
    }
    argv[argc2] = icm_exec;                     /* store executor's name */

#ifdef MSDOS
    quote_blanks(&argv[argc2]);             /* quote arguments with blanks */
#endif

                                            /* do the making of the file */
    errors = _execvp(icm_exec, &argv[argc2]);

    spawn_err(icm_exec);                    /* shouldn't get here ... */

    cleanup();                              /* remove tempfiles etc. */
    return (1);
}
