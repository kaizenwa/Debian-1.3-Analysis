#define PASSWD          "/etc/passwd"
#define ETC_FILE        "/etc/pscolor"
#define PERSONAL_FILE   ".pscolor"
#define NEW_FILE_HEADER "#\n# this file was created with pscol\n#\n"

/* note, PERSONAL_FILE is .pscolor, not ~/.pscolor... I read $HOME to get
the base path... 
NEW_FILE_HEADER is written to the beginning of the output file when using
the -c option.

*/
