extern unsigned int debug;
extern FILE *dfp;

/* dprintf is only a protocol trace excluding all data xfered. */
#define dprintf		if (debug == 0) ; else (void) fprintf
/* general tracing */
#define ddprintf	if (debug == 0 || debug < 2) ; else (void) fprintf
/* detailed debugging.  This will vanish once the code stabilizes */
#define dddprintf	if (debug == 0 || debug < 3) ; else (void) fprintf
