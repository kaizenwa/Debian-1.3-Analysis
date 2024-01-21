#include <string.h>
#include <unistd.h>
#include <linux/utsname.h>
#include <errno.h>

int
__old__gethostname(char *name, size_t len)
{
  struct old_utsname uts;

  if (name == NULL) {
    errno = EINVAL;
    return -1;
  }

  if (__old__uname(&uts) == -1) return -1;

  if (strlen(uts.nodename)+1 > len) {
    errno = EINVAL;
    return -1;
  }
  strcpy(name, uts.nodename);
  return 0;
}
