#include <stdio.h>

char buf[80];
char *strrchr();

main(argc, argv)
char **argv;
int argc;
{	FILE *inf;
	int ver = 0;
	char *p;

	inf = fopen("version.h", "r");

	if( inf){
		if( fgets(buf,  79, inf)){
			buf[79] = '\0';
			p = strrchr(buf, ' ');
			if( p)
				ver = atoi(p);
			ver++;
		}
	}

	inf = fopen("version.h", "w");
	if( argc > 1){
		fprintf(inf, "#define VERSION \"%s / %d\"\n", argv[1], ver);
	}else {
		fprintf(inf, "#define VERSION \"Ver 1.0 / %d\"\n", ver);
	}
	fclose(inf);
	exit(0);
}
