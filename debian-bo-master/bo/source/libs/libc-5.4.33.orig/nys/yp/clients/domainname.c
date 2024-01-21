#include <stdio.h>
#include <unistd.h>
#include <string.h>


int main(int argc, char *argv[])
{
    char buf[1024];
    
    if (argc < 2)
    {
	if (getdomainname(buf, sizeof(buf)) < 0)
	{
	    perror("getdomainname()");
	    exit(1);
	}

	puts(buf);
	exit(0);
    }
    else
    {
	if (setdomainname(argv[1], strlen(argv[1])) < 0)
	{
	    perror("setdomainname()");
	    exit(1);
	}

	exit(0);
    }
}
