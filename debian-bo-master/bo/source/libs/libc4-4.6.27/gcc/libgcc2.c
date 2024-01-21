#if __GNUC__ == 2
#if __GNUC_MINOR__ < 6
#include "libgcc2.c.2.5.8"
#else
#include "libgcc2.c.2.6.0"
#endif
#else
#error "Incompatible compiler. Gcc 2.5.x or above expexted."
#endif
