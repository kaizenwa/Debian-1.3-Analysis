#include <errno.h>
#include <fcntl.h>

main()
{
  char proto[]="/tmp/mmXXXXXX";
  char *name;
  int fd;
  void *ptr;
  char *ptr1;
  int n;
  
  name = tmpnam(proto);
  fd = open(name, O_CREAT|O_TRUNC|O_RDWR, 0666);
  printf("fd: %d\n", fd);
  unlink(name);
  ptr = mmalloc_attach(fd, 0x02000000);
  n = errno;
  ptr1 = mmalloc(ptr, 10);
  strcpy(ptr1, "Hello!");
  mmalloc_detach(ptr);
  
  ptr = mmalloc_attach(fd, 0);
  printf("%s\n", ptr1);
  mfree(ptr,ptr1);
  mmalloc_detach(ptr);
  close(fd);
/*  unlink(name); */
}