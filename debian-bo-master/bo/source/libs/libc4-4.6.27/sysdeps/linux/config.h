#ifdef DEBUG
#  define LDSO_IMAGE "ld.so"
#  define LDSO_CONF  "ld.so.conf"
#else
#  define LDSO_IMAGE "/lib/ld.so"
#  define LDSO_IMAGE1 "/usr/"TARGET_MACHINE"/lib/ld.so"
#  define LDSO_CONF  "/etc/ld.so.conf"
#endif

#define VERSION      "1.3"
#define LDD_ARGV0    "__LDD_ARGV0"
#define DIR_SEP      ":, \t\n"
#define MAX_DIRS     32
#define DEFAULT_PATH "/usr/lib:/lib"

typedef void (*loadptr)(int func, ...);

#define FUNC_VERS    0
#define FUNC_LDD     1
#define FUNC_LINK    2
