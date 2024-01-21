/*
 *  nwfsinfo.c
 *
 *  Print the info strings of a server, maybe sometime more.
 *
 *  Copyright (C) 1996 by Volker Lendecke
 *
 */

#include <stdio.h>
#include <unistd.h>
#include "ncplib.h"

int
main(int argc, char **argv)
{
	struct ncp_conn *conn;
	int opt;
	long err;

	if ((conn = ncp_initialize(&argc, argv, 0, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		return 1;
	}

	while ((opt = getopt(argc, argv, "dt")) != EOF)
	{
		switch(opt)
		{
		case 'd':
		{
			char strings[512];
			char *s;
			
			if (ncp_get_file_server_description_strings(conn,
								    strings)
			    != 0)
			{
				perror("could not get strings");
				ncp_close(conn);
				return 1;
			}

			s = strings;
			while (s < strings+512)
			{
				if (strlen(s) == 0)
				{
					break;
				}
				puts(s);
				s += strlen(s)+1;
			}
			break;
		}
		case 't':
		{
			time_t t;

			if (ncp_get_file_server_time(conn, &t) != 0)
			{
				perror("could not get server time");
				ncp_close(conn);
				return 1;
			}

			fputs(ctime(&t), stdout);
			break;
		}
		default:
			printf("unknown option: %c\n", opt);
			break;
		}
	}
		
	ncp_close(conn);
	return 0;
}
