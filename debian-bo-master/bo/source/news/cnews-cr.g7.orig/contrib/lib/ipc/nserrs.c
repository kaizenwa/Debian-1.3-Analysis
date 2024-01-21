/* Convert nameserver errors (h_errno) to strings, /usr/include/netdb.h */

static char *herrlist[] = { "",
	"Authoritative Answer Host not found",
	"Non-Authoritative Host not found, or SERVERFAIL",
	"Non recoverable errors, FORMERR, REFUSED, NOTIMP",
	"Valid name, no data record of requested type",
};

char *
hstrerror(errnum)
int errnum;
{
	if (errnum <= 0 || errnum >= sizeof herrlist / sizeof herrlist[0])
		return "";
	return herrlist[errnum];
}

#ifdef TEST
int
main()
{
	int i;
	while (scanf(" %d", &i) == 1)
		printf("%d: %s\n", i, hstrerror(i));
	return 0;
}
#endif /* TEST */
