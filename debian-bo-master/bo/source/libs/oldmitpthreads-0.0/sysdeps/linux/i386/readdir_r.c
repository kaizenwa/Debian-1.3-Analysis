#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <sys/syscall.h>

#include "../dirstream.h"

/*
 * readdir fills up the buffer with the readdir system call. it also
 * gives a third parameter (currently ignored, but should be 1) that
 * can with a future kernel be enhanced to be the number of entries
 * to be gotten.
 *
 * Right now the readdir system call return the number of characters
 * in the name - in the future it will probably return the number of
 * entries gotten. No matter - right now we just check for positive:
 * that will always work (as we know that it cannot be bigger than 1
 * in the future: we just asked for one entry).
 */
int old_readdir_r (DIR *, struct dirent *, struct dirent **);

int
old_readdir_r (DIR * dir, struct dirent * entry,
	struct dirent ** ret)
{
  int result;
  int count = NUMENT;

  if (dir->dd_size <= dir->dd_nextloc) {
    /* read count of directory entries. For now it should be one. */
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%esi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
	:"=a" (result)
	:"0" (SYS_readdir),"S" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (count));
#else
    __asm__("int $0x80"
	:"=a" (result)
	:"0" (SYS_readdir),"b" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (count));
#endif
    if (result <= 0)
    {
      result = -result;
      *ret = NULL;
      return result;
    }

    /*
     * Right now the readdir system call return the number of
     * characters in the name - in the future it will probably return
     * the number of entries gotten. No matter - right now we just
     * check for positive:
     */
#if 0
    dir->dd_size = result;
#else
    dir->dd_size = 1;
#endif

    dir->dd_nextloc = 0;
  }

  /* We copy the dirent entry to entry. */
  memcpy (entry, &(dir->dd_buf [(dir->dd_nextloc)++]),
	sizeof(struct dirent));
  *ret = entry;
  return 0;
}

#ifdef __ELF__
#pragma weak readdir_r = __libc_readdir_r
#endif

int
__libc_readdir_r (DIR * dir, struct dirent * entry, struct dirent ** ret)
{
  int result;

  if (!dir || !entry || !ret || !*ret) {
    return EBADF;
  }

  /* Are we running an old kernel? */
  if (dir->dd_getdents == no_getdents)
  {
    return old_readdir_r (dir, entry, ret);
  }

  if (dir->dd_size <= dir->dd_nextloc)
  {
    /* read dir->dd_max bytes of directory entries. */
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%esi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
	:"=a" (result)
	:"0" (SYS_getdents),"S" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (dir->dd_max));
#else
    __asm__("int $0x80"
	:"=a" (result)
	:"0" (SYS_getdents),"b" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (dir->dd_max));
#endif

    /* We assume we have getdents (). */
    dir->dd_getdents = have_getdents;
    if (result <= 0)
    {
      result = -result;

      if (result == ENOSYS)
      {
	dir->dd_getdents = no_getdents;
	return old_readdir_r (dir, entry, ret);
      }

      *ret = NULL;
      return result;
    }

    dir->dd_size = result;
    dir->dd_nextloc = 0;
  }

  /* We copy the dirent entry to entry. */
  memcpy (entry, ((char *)dir->dd_buf) + dir->dd_nextloc,
	sizeof(struct dirent));
  *ret = entry;

  /* Am I right? H.J. */
  dir->dd_nextloc += entry->d_reclen;

  /* We have to save the next offset here. */
  dir->dd_nextoff = entry->d_off;

  return 0;
}
