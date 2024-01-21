/* 
 * This file is hereby placed in the public domain.
 * David A. Holland, Sep. 1996
 */

#include <stdlib.h>
#include <unistd.h>
#include <gnu-stabs.h>

static int __saved_uid=-1;
static int __trust_environ=0;

static void __init_getenv(int, char **, char **);
char *__libc_secure_getenv(const char *);

static void
__init_getenv(int argc, char **argv, char **envp)
{
  /*
   * At program startup, euid==saved uid. (Later on, it may not be.)
   */
  __saved_uid = geteuid();

  /*
   * If we are not running setuid and not running setgid, permit libc
   * to trust the environment. (We should probably check the supplementary
   * groups list too.)
   */
  if (getuid() == __saved_uid && getgid()==getegid())
    __trust_environ = 1;
}

char *
__libc_secure_getenv(const char *name)
{
  if (__trust_environ)
    return getenv(name);
  return NULL;
}

/*
 * Call initializer.
 */
data_set_element (__libc_subinit, __init_getenv);
