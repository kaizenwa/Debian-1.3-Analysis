#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#define MAGICH	0x741d	/* magic of xsok version 1 highscore file */

static void portable_to_internal(long *args, unsigned char *p, int num) {
    do {
	int j;
	*args = 0;
	for (j = 0; j < 4; ++j)
	    *args += (long)p[3-j] << (j << 3);
	++args;
	p += 4;
    } while (--num);
}
int highscore[300];
static void ReadHighscores(const char *levelfile) {
    FILE *fp;
    char filename[256], base_name[32], *s;
    int i;

    memset(highscore, 0, sizeof(highscore));
    for (i = 100; i < 300; ++i)
	    highscore[i] = 0x7fffffff;

    strcpy(base_name, levelfile);
    if (!(s = strchr(base_name, '.')))
	return;
    strcpy(s+1, "score");
    sprintf(filename, "%s/%s", XSOKSAVE, base_name);
    if ((fp = fopen(filename, "rb"))) {
	long p[300];
	unsigned char ss[1200];
	fread(ss, 4, 300, fp);
	portable_to_internal(p, ss, 300);	
	for (i = 0; i < 300; ++i)
	    highscore[i] = p[i];
	fclose(fp);
    }
    highscore[0] = MAGICH;
}


static int brief = 1;
#define NARGS 16

static void checkfile(const char *filename) {
    FILE *fp;
    long p[NARGS];
    unsigned char s[256];
    char type[10], *z;
    int namelen, level;

    if (!(fp = fopen(filename, "rb")))
	return;
    fread(s, 4, NARGS, fp);
    portable_to_internal(p, s, NARGS);
    if (p[0] != 0x741b)
	return;
    fread(type, 1, 8, fp);
    level = p[6];
    type[8] = '\0';
    namelen = getc(fp);
    fread(s, 1, namelen, fp);
    s[namelen] = '\0';

    if (strrchr(filename, '/'))
	filename = strrchr(filename, '/') + 1;
    ReadHighscores(filename);

    z = ctime(&p[4]);
    if (strchr(z, '\n'))
	*strchr(z, '\n') = '\0';

    if (!brief) {
	printf("%-14s %-7s Level %2d:  Score: %5ld, Pushes: %4ld, Moves: %4ld\n",
	       filename, type, level, p[1], p[2], p[3]);
#if 0
	if (!p[5])	/* unsolved */
	    printf("\n");
	else
#endif
	    printf("saved %s by %s\n", z, s);
    } else {
	int c1, c2, c3;
	c1 = p[5] && highscore[level]     == p[1] ? '*' : ' ';
	c2 = p[5] && highscore[level+200] == p[2] ? '*' : ' ';
	c3 = p[5] && highscore[level+100] == p[3] ? '*' : ' ';
	printf("%2d %5ld%c %4ld%c %4ld%c  %s\n", level, p[1], c1, p[2], c2, p[3], c3, s);
    }
}

int main(int argc, char *argv[]) {
    int i;
    if (argc == 1) {
	fprintf(stderr, "usage: showscore [-b] (savefiles)\n");
	exit(1);
    }
    i = 1;
    if (!strcmp(argv[i], "-b")) {
	brief = 0;
	++i;
    } else
	printf("Lv Score Pushes Moves  Player\n");
    for (; i < argc; ++i)
	checkfile(argv[i]);
    return 0;
}
