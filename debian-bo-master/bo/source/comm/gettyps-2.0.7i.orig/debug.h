/* define this if you want to be able to debug with gdb */
/* #define GDB_FRIENDLY */

/* define these if you're trying to track down signal()/alarm() invocations */
#define alarm(d) alarm(d); debug(D_RUN, "alarm set: %s:%u", __FILE__, __LINE__)
#define signal(d,e) signal(d,e); debug(D_RUN, "signal set: %s:%u", __FILE__, \
	__LINE__)

