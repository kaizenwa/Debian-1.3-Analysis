#include <gnu-stabs.h>

/* Although we keep those functions in the jump table. But we
 * have removed the entry point to outside. So we need the 
 * alias to bypass the jump table.
 */
#if __GNUC__ == 2
#if __GNUC_MINOR__ < 6
symbol_alias (__set_new_handler__LOCAL__,__set_new_handler);
#else
symbol_alias (set_new_handler__LOCAL__,__set_new_handler);
symbol_alias (set_new_handler__LOCAL__,__set_new_handler__LOCAL__);
#endif
#else
#error "Incompatible compiler. Gcc 2.5.x or above expexted."
#endif
