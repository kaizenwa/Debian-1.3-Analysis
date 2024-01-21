#include "att.h"

#define ADDITIONAL_FEATURES \
		     ADD_FEATURE("ATT3B2");\
		     ADD_FEATURE("SYSTEM-V");


#define ATT3B2
#define	IEEEFLOAT
  
#define HOLEPAGE 32

#define INIT_ALLOC \
        FIXtemp=MAXPAGE; \
	if (brk(pagetochar(FIXtemp)) < 0) \
		{error("Can't allocate.  Good-bye!.");};

#undef IF_ALLOCATE_ERR
#define IF_ALLOCATE_ERR \
	if (PAGESIZE*(n - m) > pagetochar(MAXPAGE) - core_end)

/* smallest address for a page table entry */
#define DBEGIN 0x80800000
/* Begin for cmpinclude */


/* End for cmpinclude */
