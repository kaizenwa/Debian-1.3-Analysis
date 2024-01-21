/*
 *  nwlsobj.c
 *
 *  List bindery objects
 *
 *  Copyright (C) 1996 by Volker Lendecke
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include "ncplib.h"

int
main(int argc, char **argv)
{
	struct ncp_conn *conn;
	struct ncp_bindery_object o;
	int found = 0;

	char default_pattern[] = "*";
	char *pattern = default_pattern;
	char *p;
	long err;

	if ((conn = ncp_initialize(&argc, argv, 1, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		return 1;
	}

	if (argc > 2)
	{
		fprintf(stderr, "usage: %s [options]\n", argv[0]);
		return 1;
	}

	if (argc == 2)
	{
		pattern = argv[1];
	}

	for (p = pattern; *p != '\0'; p++)
	{
		*p = toupper(*p);
	}

	o.object_id = 0xffffffff;

	while (ncp_scan_bindery_object(conn, o.object_id,
				       0xffff, pattern, &o) == 0)
	{
		found = 1;
		printf("%s %08X %04X\n",
		       o.object_name, (unsigned int)o.object_id,
		       (unsigned int)o.object_type);
	}

	ncp_close(conn);
	return 0;
}
