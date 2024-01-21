#include <fcntl.h>

main()
{
 int fd=3;
 int b,c;
 close(fd);
 fd = open("/dev/tty", O_WRONLY|O_CREAT, 0666);
 write(fd,"Hello!\n",7);
 c = b;
 b = fd;
 write(fd,"Hello!\n",7);
}

 
 