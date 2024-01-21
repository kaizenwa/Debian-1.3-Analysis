/*
 *  ncptest.c
 *
 *  Copyright (C) 1995 by Volker Lendecke
 *
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
/* #include <sys/wait.h> */  /* generates a warning here */
extern pid_t waitpid(pid_t, int *, int);
#include <sys/errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <mntent.h>
#include <linux/ipx.h>

#include <linux/fs.h>
#include <linux/ncp.h>
#include <linux/ncp_fs.h>
#include <linux/ncp_mount.h>
#include "ncplib.h"


void
test_connlist(struct ncp_conn *conn)
{
	__u8 conn_list[256] = {0,};
	int no;

	ncp_get_connlist(conn, NCP_BINDERY_USER, "SUPERVISOR", &no,
			 conn_list);
	return;
}

void
test_send(struct ncp_conn *conn)
{
	__u8 conn_list[256] = {0,};
	int no;

	if (ncp_get_connlist(conn, NCP_BINDERY_USER, "ME", &no,
			     conn_list) != 0)
	{
		no = 0;
	}

	if (no > 0)
	{
		ncp_send_broadcast(conn, no, conn_list, "Hallo");
	}
	return;
}
void
test_create(struct ncp_conn *conn)
{
	struct nw_info_struct sys;
	struct nw_info_struct me;
	__u8 dir_handle;
	struct ncp_file_info new_file;

	if (ncp_do_lookup(conn, NULL, "SYS", &sys) != 0)
	{
		printf("lookup error\n");
		return;
	}
	if (ncp_do_lookup(conn, &sys, "ME", &me) != 0)
	{
		printf("lookup public error\n");
		return;
	}
	
	if (ncp_alloc_short_dir_handle(conn, &me, NCP_ALLOC_TEMPORARY,
				       &dir_handle) != 0)
	{
		printf("alloc_dir_handle error\n");
		return;
	}

	if (ncp_create_file(conn, dir_handle, "BLUB.TXT", 0,
			    &new_file) != 0)
	{
		printf("create error\n");
		return;
	}

	if (ncp_dealloc_dir_handle(conn, dir_handle) != 0)
	{
		printf("dealloc error\n");
		return;
	}
}
	

int
main(int argc, char *argv[])
{
	struct ncp_conn *conn;
	long err;

	if ((conn = ncp_initialize(&argc, argv, 1, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		return 1;
	}

	test_send(conn);
	ncp_close(conn);
	return 0;
}
