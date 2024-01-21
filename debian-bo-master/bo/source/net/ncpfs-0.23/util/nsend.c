/*
 *  nsend.c
 *
 *  Send Messages to users
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
	__u8 conn_list[256] = {0,};
	int no_conn;

	char *message = NULL;
	char *user = NULL;
	long err;

	if ((conn = ncp_initialize(&argc, argv, 1, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		exit(1);
	}

	if (argc != 3)
	{
		fprintf(stderr, "usage: %s [options] user message\n", argv[0]);
		ncp_close(conn);
		exit(1);
	}

	user = argv[1];
	message = argv[2];

	if ((err = ncp_get_connlist(conn, NCP_BINDERY_USER, user, &no_conn,
				    conn_list)) != 0)
	{
		com_err(argv[0], err, "in get_connlist");
		ncp_close(conn);
		exit(1);
	}

	if (no_conn == 0)
	{
		fprintf(stderr, "No connection found for %s\n", user);
		ncp_close(conn);
		exit(1);
	}

	if ((err = ncp_send_broadcast(conn, no_conn, conn_list, message)) != 0)
	{
		com_err(argv[0], err, "in send_broadcast");
		ncp_close(conn);
		exit(1);
	}
	ncp_close(conn);
	return 0;
}
