/* A few configuration options.  Most sites will not need to change these. */
/* 
 * This is the program to which inews postings are passed. It will be
 * invoked with popen().  It can be a relative path, in which case the C
 * News default path (NEWSPATH) will be searched.
 */
#define INEWS		"inews -h -W"

/*
 * Temporary kludge till we get some access control.  Define to 0 if you
 * allow posts from anywhere, define to 1 for no posting privileges to
 * anyone
 */
#define NO_POSTING	1

/*
 * Define this if have a syslog() command.  Note that you will still
 * have to turn on syslogging by command line options, since snntpd logs
 * errors to C News $NEWSCTL/errlog.
 */
#undef LOGLEVEL		LOG_NEWS

/*
 * Define this if you want fake syslogging to a file. If the pathname is
 * relative, then the file is assumed to relative to $NEWSCTL.
 */
#define LOGFILE		"snntpdlog"

/*
 * The size of article that can be cached incore.  Bigger articles are
 * spilled to disk.  Feel free to make this the same as ARTMAX.  My
 * present stats (Oct 1991) show that 2% of the articles are greater
 * than 8K.
 */
#define ARTBATCHSIZE	8192

/* 
 * By default, toss articles bigger than this.  Log message ids and size
 * in warning.  Feel free to make this much larger if you have scads of
 * disk space.  NOT IMPLEMENTED.
 */
#define ARTMAX		65535L
