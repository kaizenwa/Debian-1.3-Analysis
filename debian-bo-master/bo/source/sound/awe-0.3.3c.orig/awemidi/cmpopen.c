/*================================================================
 * cmpopen.c:
 *	open a compressed file
 *================================================================*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "util.h"

typedef struct _DecompList {
	char *ext;
	char *format;
} DecompList;

static DecompList declist[] = {
	{".gz", "gunzip -c \"%s\""},
	{".z", "gunzip -c \"%s\""},
	{".Z", "zcat \"%s\""},
	{".zip", "unzip -p \"%s\""},
	{".lha", "lha -pq \"%s\""},
	{".lzh", "lha -pq \"%s\""},
};

char *CmpExtension(int type)
{
	return declist[type].ext;
}

int CmpSearchFile(char *name)
{
	int i;
	char *ext;

	if ((ext = strrchr(name, '.')) == NULL)
		return -1;
	for (i = 0; i < sizeof(declist)/sizeof(declist[0]); i++) {
		if (strcmp(ext, declist[i].ext) == 0) {
			if (access(name, R_OK) == 0)
				return i;
		}
	}
	return -1;
}

FILE *CmpOpenFile(char *name, int *flag)
{
	FILE *fp;
	int i;
	char str[256];

	*flag = 0;
	if (strcmp(name, "-") == 0) {
		/* use standard input */
		*flag = 2;
		return stdin;
	}
	i = CmpSearchFile(name);
	if (i >= 0) {
		sprintf(str, declist[i].format, name);
		if ((fp = popen(str, "r")) != NULL) {
			*flag = 1;
			return fp;
		}
	}
	if ((fp = fopen(name, "r")) != NULL)
		return fp;

	return NULL;
}

void CmpCloseFile(FILE *fp, int flag)
{
	switch (flag) {
	case 0:
		fclose(fp); break;
	case 1:
		pclose(fp); break;
	}
}
