#include "internal.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

const char	more_usage[] = "more [file]\n"
"\n"
"\tDisplays a file, one page at a time.\n"
"\tIf there are no arguments, the standard input is displayed.\n";

extern int
more_fn(const struct FileInfo * i)
{
	FILE *	input = stdin;
	int	c;
	int	lines = 0;
	int	next_page = 0;
	
	if ( i && i->source && (input = fopen(i->source, "r")) < 0 )
		name_and_error(i->source);
	else {
		if ( i ) {
			static const char border[] = "::::::::::::::";
			printf("%s\n%s\n%s\n", border, i->source, border);
			lines += 3;
		}

		while ( (c = getc(input)) != EOF ) {
			if ( next_page ) {
				char	garbage;
				lines = 0;
				next_page = 0;
				if ( i && i->source )
					printf(
					 "--- press ENTER for more of file %s ---"
					,i->source);
				else
					printf("--- press ENTER for more ---");
					
				fflush(stdout);
				read(2, &garbage, 1);
			}
			putchar(c);
			if ( c == '\n' && ++lines == 24 )
				next_page = 1;
		}
		if ( input != stdin )
			fclose(input);
	}
	return 0;
}
