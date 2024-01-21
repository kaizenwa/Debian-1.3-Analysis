#include <ansidecl.h>
#include <localeinfo.h>
#include <stddef.h>


extern CONST struct ctype_ctype_info __ctype_ctype_KOI_8;
extern CONST struct ctype_mbchar_info __ctype_mbchar_KOI_8;
CONST struct ctype_info __ctype_KOI_8 =
  {
    (struct ctype_ctype_info*)&__ctype_ctype_KOI_8,
    (struct ctype_mbchar_info*) &__ctype_mbchar_KOI_8
  };

#if 0
CONST struct ctype_info *_ctype_info = &__ctype_KOI_8;
#endif
