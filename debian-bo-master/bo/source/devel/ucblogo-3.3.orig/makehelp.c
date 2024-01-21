#include <stdio.h>
#include <ctype.h>

char line[100], line2[100], line3[100];
char name[30] = "helpfiles/";
char name2[30] = "helpfiles/";
char tocname[20];

main() {
    FILE *in=fopen("usermanual", "r");
    FILE *fp, *fp2;
    FILE *toc=fopen("helpfiles/HELPCONTENTS", "w");
    char ch, *cp, *np, *tp;
    int intab, three, col=5;

    if (toc == NULL) {
	fprintf(stderr, "Can't open HELPCONTENTS.\n");
	exit(1);
    }

    fputs("Help is available on the following:\n\n", toc);

    fgets(line, 100, in);
    while (line[0] != '-') fgets(line, 100, in);

    while (!feof(in)) {
	for (cp = line, np = &name[10], tp = tocname;
	     (ch = *cp) == '.' || (ch >= 'A' && ch <= 'Z') || ch == '`';
	     cp++) {
	  *tp++ = tolower(ch);
	  if (ch == '.') ch='d';
	  *np++ = tolower(ch);
	}
	if (cp == line || (ch != ' ' && ch != '\t' && ch != '\n')) {
	    fgets(line, 100, in);
	    continue;
	}

	*tp = *np = '\0';

	if (name[11] == '\0' && name[10] != '`') {
	    fgets(line, 100, in);
	    continue;
	}

	fgets(line2, 100, in);
	if ((ch = line2[0]) == '-' || ch == '=') {
	    fgets(line, 100, in);
	    continue;
	}

	fp = fopen(name, "w");
	if (fp == NULL) {
	    fprintf(stderr, "Can't open %s\n", name);
	    exit(1);
	}

	if (--col == 0) {
	    col = 5;
	    fprintf(toc, "%s\n", tocname);
	} else {
	    fprintf(toc, "%-16s", tocname);
	}

	three = 0;
	if (ch == '.' || (ch >= 'A' && ch <= 'Z')) {
	    for (cp = line2, np = &name2[10], tp = tocname;
		 (ch = *cp) == '.' || (ch >= 'A' && ch <= 'Z') || ch == '?';
		 cp++) {
	       if (ch == '.') ch='d';
	       if (ch == '?') three++;
	       *np++ = tolower(ch);
	    }
	    *np = '\0';
	    if (three) {
		fp2 = NULL;
		fgets(line3, 100, in);
		if ((ch = line3[0]) == '.' || (ch >= 'A' && ch <= 'Z')) {
		    for (cp = line3, np = &name2[10];
			 (ch = *cp) == '.' || (ch >= 'A' && ch <= 'Z') || ch == '?';
			 cp++) {
		       if (ch == '.') ch='d';
		       *np++ = tolower(ch);
		    }
		    *np = '\0';
		} else name2[10] = '\0';
	    }
	    if (name2[10] != '\0') {
		fp2 = fopen(name2, "w");
		if (fp2 == NULL) {
		    fprintf(stderr, "Can't open %s\n", name2);
		    exit(1);
		}
	    } else fp2 = NULL;
	} else fp2 = NULL;

	fputs(line, fp);
	fputs(line2, fp);
	if (three) fputs(line3, fp);
	if (fp2) {
	    fputs(line, fp2);
	    fputs(line2, fp2);
	    if (three) fputs(line3, fp);
	}
	intab = 0;

	fgets(line, 100, in);
	while (!feof(in)) {
	    if (intab && line[0] != '\t' && line[0] != '\n') break;
	    if (!intab && line[0] == '\t') intab++;
	    fputs(line, fp);
	    if (fp2) fputs(line, fp2);
	    fgets(line, 100, in);
	}

	fclose(fp);
	if (fp2) fclose(fp2);
    }
    fprintf(toc, "\n");
    fclose(toc);
}
