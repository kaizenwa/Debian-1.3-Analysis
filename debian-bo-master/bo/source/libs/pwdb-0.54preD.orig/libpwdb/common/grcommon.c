
#include "../_pwdb_internal.h"

char * __pwdb_fgetsx (char *buf, int cnt, FILE *f)
{
	char *cp = buf;
	char *ep;

	while (cnt > 0) {
		if (fgets (cp, cnt, f) == 0)
			if (cp == buf)
				return 0;
			else
				break;

		if ((ep = strrchr (cp, '\\')) && *(ep + 1) == '\n') {
			if ((cnt -= ep - cp) > 0)
				*(cp = ep) = '\0';
		} else
			break;
	}
	return buf;
}

int __pwdb_fputsx (const char *s, FILE *stream)
{
	int i;

	for (i = 0;*s;i++, s++) {
		if (putc (*s, stream) == EOF)
			return EOF;

		if (i > (BUFSIZ/2)) {
			if (putc ('\\', stream) == EOF ||
			    putc ('\n', stream) == EOF)
				return EOF;

			i = 0;
		}
	}
	return 0;
}
