/*
 * unicode.c - convert between utf strings and hex numbers.
 * implemented from the ascii/unicode manual page from plan 9.
 * this program is public domain. do what you want with it,
 * but don't blame me for anything.
 *	-Steve Kilbane 7 Oct 93.
 *	steve_kilbane@gec-epl.co.uk
 */

#include <u.h>
#include <string.h>
#include <ctype.h>
#include <libc.h>

int
main(int argc, char *argv[])
{
	Rune c1,c2;
	char s[UTFmax+1];
	int l;
	int optn = 0, optt = 0;

	if (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
			case 't':
				optt++;
				break;
			case 'n':
				optn++;
				break;
			default:
				printf("Unknown argument %s\n",argv[1]);
				exit(1);
		}
		argc--;
		argv++;
	}

	while (argc > 1) {
		char *dash;
		if (isxdigit(argv[1][0]) && !optn) {
			if (dash=strchr(argv[1],'-')) {
				*dash++ = 0;
				c1 = strtol(argv[1],(char **)0,16);
				c2 = strtol(dash,(char **)0,16);
			} else {
				c1 = c2 = strtol(argv[1],(char **)0,16);
			}
			while (c1 <= c2) {
				l = runetochar(s, &c1);
				s[l] = 0;
				if (optt)
					printf("%s", s);
				else {
					printf("%04x %s%c", c1, s,
							(c1+1)%8?'\t':'\n');
				}
				c1++;
			}
		} else {
			int l;
			char *s = argv[1];

			while (*s) {
				l = chartorune(&c1,s);
				s += l;
				printf("%x\n",c1);
			}
		}
		argv++;
		argc--;
	}
	if (optt)
		printf("\n");
	return 0;
}
