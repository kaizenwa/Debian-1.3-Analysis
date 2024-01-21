#include <string.h>

#ifndef LCC_PATH
#define LCC_PATH "/usr/local/lcc"
#endif

#ifndef CHECKER_PATH
#define CHECKER_PATH "/usr/local/lib/checker"
#endif

char *cpp[] = {
	"/lib/cpp", "-undef", "-nostdinc", "-lang-c", "-U__GNUC__",
	"-D_POSIX_SOURCE", "-D__STDC__", "-D__STRICT_ANSI__",
	"-Dunix", "-Di386", "-D__linux__", "-D__unix__", "-D__i386__",
	"-D__CHECKER__", "-DMALLOC_0_RETURNS_NULL", "$1", "$2", "$3", 0};
char *include[] = {"-I" LCC_PATH "/include", "-I/usr/include", 0};
char *com[] = { LCC_PATH "/rcc", "-g", "-target=x86-bsd", "$1", "$2", "$3", 0};
char *as[] = { CHECKER_PATH "/as", "-checker", "-o", "$3", "$1", "$2", 0};
char *ld[] = { CHECKER_PATH "/ld", "-checker", "-nostdlib",
	"-o", "$3", CHECKER_PATH "/crt0.o",
	"$1", "$2", CHECKER_PATH "/libchecker.o", 
	"-L" CHECKER_PATH, "-L" LCC_PATH, "-lgcc", "-lc", "-lgcc", 0};

int 
option(arg)
	char *arg;
{
#if 0
	if (strcmp(arg, "-g") == 0)
		as[3] = "-g";
	else if (strcmp(arg, "-p") == 0
		 && strcmp(ld[3], "/usr/lib/crt0.o") == 0) {
		ld[3] = "/usr/lib/mcrt0.o";
		ld[7] = "/usr/lib/libprof1.a";
	} else if (strcmp(arg, "-b") == 0
		   && access("/usr/local/lib/bbexit.o", 4) == 0)
		ld[6] = "/usr/local/lib/bbexit.o";
	else
		return 0;
#endif
	return 1;
}
