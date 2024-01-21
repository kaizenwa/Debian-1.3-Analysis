#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>

struct utimbuf 
{
  time_t actime;
  time_t modtime; 
};

int utime(const char *filename, struct utimbuf *buf);



#ifdef __cplusplus
};
#endif
