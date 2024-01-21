#include <ansidecl.h>
#include <localeinfo.h>
#include <stddef.h>


extern CONST struct ctype_ctype_info __ctype_ctype_ISO_8859_1;
extern CONST struct ctype_mbchar_info __ctype_mbchar_ISO_8859_1;
CONST struct ctype_info __ctype_ISO_8859_1 =
  {
    (struct ctype_ctype_info*)&__ctype_ctype_ISO_8859_1,
    (struct ctype_mbchar_info*) &__ctype_mbchar_ISO_8859_1
  };

#if 0
CONST struct ctype_info *_ctype_info = &__ctype_ISO_8859_1;
#endif
