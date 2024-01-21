#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int stripcomments = 0;

static int copy(FILE *fout, char *s, int i) {
    FILE *fp;
    if (!(fp = fopen(s, "r")))
	return 0;
    if (i)
	fprintf(fout, ";LEVEL %d\n", i);
    while (fgets(s, 100, fp)) {
	if (stripcomments) {
	    if (!strncmp(s, "; ", 2))
		continue;
	    if (!strcmp(s, ";\n"))
		continue;
	}
	fprintf(fout, "%s", s);
    }
    fclose(fp);
    return 1;
}

int main(int argc, char *argv[]) {
    int c, i;
    for (c = 1; c < argc; ++c) {
	FILE *fout;
	char s[100];
	if (!strcmp(argv[c], "-s")) {
	    stripcomments = 1;
	    continue;
	}
	sprintf(s, "%s.def", argv[c]);
	if (!(fout = fopen(s, "w"))) {
	    fprintf(stderr, "Cannot open output file %s\n", s);
	    exit(1);
	}
	sprintf(s, "%s/definitions", argv[c]);
	if (!copy(fout, s, 0)) {
	    fprintf(stderr, "No file %s\n", s);
	    exit(1);
	}
	for (i = 1; i < 100; ++i) {
	    sprintf(s, "%s/screen.%02d", argv[c], i);
	    if (!copy(fout, s, i))
		break;
	}
    }
    return 0;
}
