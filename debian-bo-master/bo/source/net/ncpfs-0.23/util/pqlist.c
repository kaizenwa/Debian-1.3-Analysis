/*
 *  pqlist.c
 *
 *  List all print queues on a server
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
	struct ncp_bindery_object q;
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
		fprintf(stderr, "usage: %s [options] [pattern]\n", argv[0]);
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

	if (isatty(1))
	{
		printf("\nServer: %s\n", conn->server);
		printf("%-52s%-10s\n"
		       "-----------------------------------------------"
		       "-------------\n",
		       "Print queue name",
		       "Queue ID");
	}

	q.object_id = 0xffffffff;

	while (ncp_scan_bindery_object(conn, q.object_id,
				       NCP_BINDERY_PQUEUE, pattern, &q) == 0)
	{
		found = 1;
		printf("%-52s", q.object_name);
		printf("%08X\n", (unsigned int)q.object_id);
	}

	if ((found == 0) && (isatty(1)))
	{
		printf("No queues found\n");
	}

	ncp_close(conn);
	return 0;
}
