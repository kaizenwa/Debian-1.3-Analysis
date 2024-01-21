
/* check for whether caller is a specific uid (numeric) */

main(argc, argv)
int argc;
char *argv[];
{
    int uid = 0;

    if (argc > 1) uid = atoi(argv[1]);

    exit(getuid() == uid ? 0 : 1);
}
