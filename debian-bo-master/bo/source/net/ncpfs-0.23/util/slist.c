/*
 *  slist.c
 *
 *  List all file server that are known in the IPX network.
 *
 *  Copyright (C) 1995 by Volker Lendecke
 *
 */

#include "ncplib.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

void
main(int argc, char *argv[])
{
	struct ncp_conn *conn;
	struct ncp_bindery_object obj;
	int found = 0;
	char default_pattern[] = "*";
	char *pattern = default_pattern;
	char *p;
	long err;

	if (argc > 2)
	{
		printf("usage: %s [pattern]\n", argv[0]);
		exit(1);
	}

	if (argc == 2)
	{
		pattern = argv[1];
	}

	for (p = pattern; *p != '\0'; p++)
	{
		*p = toupper(*p);
	}

	if ((conn = ncp_initialize(&argc, argv, 0, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		exit(1);
	}

	if (isatty(1))
	{
		printf("\n%-52s%-10s%-12s\n"
		       "-----------------------------------------------"
		       "---------------------------\n",
		       "Known NetWare File Servers",
		       "Network",
		       "Node Address");
	}

	obj.object_id = 0xffffffff;

	while (ncp_scan_bindery_object(conn, obj.object_id,
				       NCP_BINDERY_FSERVER, pattern,
				       &obj) == 0)
	{
		struct nw_property prop;
		struct prop_net_address *naddr
			= (struct prop_net_address *)&prop;

		found = 1;

		printf("%-52s", obj.object_name);

		if (ncp_read_property_value(conn, NCP_BINDERY_FSERVER,
					    obj.object_name, 1, "NET_ADDRESS",
					    &prop) == 0)
		{
			ipx_print_network(naddr->network);
			printf("  ");
			ipx_print_node(naddr->node);
		}
		printf("\n");
	}

	if ((found == 0) && (isatty(1)))
	{
		printf("No servers found\n");
	}

	ncp_close(conn);
}

