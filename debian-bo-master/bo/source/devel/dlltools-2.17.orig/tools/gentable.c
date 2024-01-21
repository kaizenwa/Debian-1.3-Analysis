#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define progname(foo) (!strcmp(name,foo))
#define MAX_LINE_LEN 1024

#ifdef __svr4__
#include <stdlib.h>
#define bzero(x,y) memset(x,0,y)
#endif

char *strip(char *p)
{
char *i;

	if (!*p) return p;
	/* Strip all white space before string */
	for (;*p && isspace(*p);++p);

	if (!*p) return p;
	/* Strip all white space after string */
	for (i=strchr(p,'\n');*i && isspace(*i);*(i--)=0);
	return p;
}

int isblank_line(char *line)
{
	char *p;

	for (p=line;*p && isspace(*p); ++p);
	return !*p;
}

int main (int argc, char **argv)
{
FILE *f=stdin;
char *name;
char line[MAX_LINE_LEN];
char lib[MAX_LINE_LEN];
char maint[MAX_LINE_LEN];
char start[MAX_LINE_LEN];
char end[MAX_LINE_LEN];
char email[MAX_LINE_LEN];
char desc[MAX_LINE_LEN];
char last_maint[MAX_LINE_LEN];

bzero(line,MAX_LINE_LEN);

name = (name = strrchr (argv[0], '/')) ? ++name : argv[0];

if (argc>1 && ((f=fopen(argv[1],"r"))==NULL))
{
	fprintf(stderr,"%s: Can't open %s\n",name, argv[1]);
	return 1;
}

if progname("gentable")
{
	printf(".TS\ndoublebox, center;\nc fB | c fB | c fB | c fB .\n");
	printf("Library\tStart Addr.\tStop Addr.\tMaintainer\n.T&\n");
	printf("l fCR | c fCR | c fCR | l fCR .\n=\n");

} else {
	printf("struct tab {char name[50];unsigned int start_addr;} tb[]={\n");
}

while (fgets(line,MAX_LINE_LEN,f)!=NULL)
{

	bzero(lib,MAX_LINE_LEN);
	bzero(maint,MAX_LINE_LEN);
	bzero(start,MAX_LINE_LEN);
	bzero(end,MAX_LINE_LEN);
	bzero(email,MAX_LINE_LEN);
	bzero(desc,MAX_LINE_LEN);

	do {
		char *p;

		if (!strncmp(line,"Lib",strlen("Lib")) && (p=strchr(line,':')))
			strcpy(lib,strip(++p));
		else
		if (!strncmp(line,"Mai",strlen("Mai")) && (p=strchr(line,':')))
			strcpy(maint,strip(++p));
		else
		if (!strncmp(line,"St",strlen("St")) && (p=strchr(line,':')))
		{
			strcpy(start,strip(++p));
			if (strncmp(start,"0x",2))
				sprintf(start,"0x%s",++p);
		}
		else
		if (!strncmp(line,"End",strlen("End")) && (p=strchr(line,':')))
		{
			strcpy(end,strip(++p));
			if (strncmp(end,"0x",2))
				sprintf(end,"0x%s",++p);
		}
		else
		if (!strncmp(line,"Ema",strlen("Ema")) && (p=strchr(line,':')))
			strcpy(email,strip(++p));
		else
		if (!strncmp(line,"Des",strlen("Des")) && (p=strchr(line,':')))
			strcpy(desc,strip(++p));

	} while (fgets(line,MAX_LINE_LEN,f)!=NULL && !isblank_line(line));

	if (*lib) {
#ifdef DEBUG

			printf("Lib=(%s)\n",lib);
			printf("Maint=(%s)\n",maint);
			printf("Start=(%s)\n",start);
			printf("End=(%s)\n",end);
			printf("Email=(%s)\n",email);
			printf("Description=(%s)\n",desc);
#endif

	/* Perform some sanity checks */

	/* This is fatal as libinfo won't work */
	if (!*start || !*end)
	{
		fprintf(stderr,"%s: %s has no %s address. Bailing out...\n",
			name,
			lib,
			*start?"end":"start");
		return 1;
	}

	if (!*maint)
		fprintf(stderr,"%s: Warning: %s has no Maintainer\n",
			name, lib);
	if (!*email)
		fprintf(stderr,"%s: Warning: %s has no Email address\n",
			name, lib);
	if (!*desc)
		fprintf(stderr,"%s: Warning: %s has no Description\n",
			name, lib);

	if progname("gentable")
	{

		if (strcmp(maint,last_maint)) 
				printf("_\n");
#if 0
			{
				printf("_\n");
				printf("%s\t%s\t%s\t\\s-2%s\\s0\n",lib,start,end,email);
			} else
#endif
			printf("%s\t%s\t%s\t%s\n",lib,
			       start,
			       end,
			       *maint?maint:email);
		strcpy(last_maint,maint);
	} else {
		sprintf(line,"%s (%s)",lib,desc);
		printf("{\"%.50s\", %s},\n",line,start);
	}
	}

}
if progname("gentable")
{
	printf(".TE\n");

} else {

	printf("{\"\",%s}};\n",end);
}

return 0;
}
