/*
 *  nprint.c
 *
 *  Send data to a NetWare print queue.
 *
 *  Copyright (C) 1995 by Volker Lendecke
 *
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include "ncplib.h"

static char *progname;
static void
usage(void);
static void help(void);

void
main(int argc, char *argv[])
{
	struct ncp_conn *conn;

	char default_queue[] = "*";
	char *queue = default_queue;

	int opt;

	struct ncp_bindery_object q;
	struct queue_job j;
	struct print_job_record pj;

	int written, read_this_time;

	char buf[8192];

	char *file_name;
	int file;
	long err;

	progname = argv[0];

	memzero(j); memzero(pj); memzero(q);

	if (   (argc == 2)
	    && (strcmp(argv[1], "-h") == 0))
	{
		help();
		exit(0);
	}

	if ((conn = ncp_initialize(&argc, argv, 1, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		exit(1);
	}

	/*
	 * Fill in default values for print job
	 */
	j.j.TargetServerID = 0xffffffff; /* any server */
	/* at once */
	memset(&(j.j.TargetExecTime), 0xff, sizeof(j.j.TargetExecTime));
	j.j.JobType = htons(0);
	strcpy(j.j.JobTextDescription, "No Description");

	pj.Version = 0;
	pj.TabSize = 8;
	pj.Copies = htons(1);
	pj.CtrlFlags = 0;
	pj.Lines = htons(66);
	pj.Rows = htons(80);
	strcpy(pj.FnameHeader, "stdin");

	while ((opt = getopt(argc, argv, "hq:d:p:b:f:l:r:c:t:F:TN"))!=EOF)
	{
		switch (opt) {
		case 'h':
			help();
			ncp_close(conn);
			exit(1);
		case 'p':
			/* Path */
			pj.CtrlFlags |= PRINT_BANNER;
			if (strlen(optarg) >= sizeof(pj.Path))
			{
				strncpy(pj.Path, optarg,
					sizeof(pj.Path));
			}
			else
			{
				strcpy(pj.Path, optarg);
			}
			break;
		case 'b':
			/* Banner Name */
			pj.CtrlFlags |= PRINT_BANNER;
			if (strlen(optarg) >= sizeof(pj.BannerName))
			{
				strncpy(pj.BannerName, optarg,
					sizeof(pj.BannerName));
			}
			else
			{
				strcpy(pj.BannerName, optarg);
			}
			break;
		case 'f':
			/* File Name in Banner */
			pj.CtrlFlags |= PRINT_BANNER;
			if (strlen(optarg) >= sizeof(pj.FnameBanner))
			{
				strncpy(pj.FnameBanner, optarg,
					sizeof(pj.FnameBanner));
			}
			else
			{
				strcpy(pj.FnameBanner, optarg);
			}
			break;
		case 'l':
			/* lines, default: 66 */
			if ((atoi(optarg) < 0) || (atoi(optarg) > 65535))
			{
				fprintf(stderr,
					"invalid line number: %s\n", optarg);
				break;
			}
			pj.Lines = htons(atoi(optarg));
			pj.CtrlFlags |= EXPAND_TABS;
			break;
		case 'r':
			/* rows, default: 80 */
			if ((atoi(optarg) < 0) || (atoi(optarg) > 65535))
			{
				fprintf(stderr,
					"invalid row number: %s\n", optarg);
				break;
			}
			pj.Rows = htons(atoi(optarg));
			pj.CtrlFlags |= EXPAND_TABS;
			break;
		case 'c':
			/* copies, default: 1 */
			if ((atoi(optarg) < 0) || (atoi(optarg) > 65000))
			{
				fprintf(stderr,
					"invalid copies: %s\n", optarg);
				break;
			}
			pj.Copies = htons(atoi(optarg));
			pj.CtrlFlags |= EXPAND_TABS;
			break;
		case 't':
			/* tab size, default: 8 */
			if ((atoi(optarg) < 0) || (atoi(optarg) > 255))
			{
				fprintf(stderr,
					"invalid tab size: %s\n", optarg);
				break;
			}
			pj.TabSize = atoi(optarg);
			pj.CtrlFlags |= EXPAND_TABS;
			break;
		case 'T':
			/* expand tabs, default tabsize: 8 */
			pj.CtrlFlags |= EXPAND_TABS;
			break;
		case 'N':
			/* no form feed */
			pj.CtrlFlags |= NO_FORM_FEED;
			break;
		case 'F':
			/* Form number, default: 0 */
			if ((atoi(optarg) < 0) || (atoi(optarg) > 255))
			{
				fprintf(stderr,
					"invalid form number: %s\n", optarg);
				break;
			}
			j.j.JobType = htons(atoi(optarg));
			break;
		case 'q':
			/* Queue name to print on, default: '*' */
			if (strlen(optarg) >= NCP_BINDERY_NAME_LEN)
			{
				printf("queue name too long: %s\n",
				       optarg);
				ncp_close(conn);
				exit(1);
			}
			queue = optarg;
			break;
		case 'd':
			/* Job Description */
			pj.CtrlFlags |= PRINT_BANNER;
			if (strlen(optarg) >= sizeof(j.j.JobTextDescription))
			{
				strncpy(j.j.JobTextDescription, optarg,
					sizeof(j.j.JobTextDescription));
			}
			else
			{
				strcpy(j.j.JobTextDescription, optarg);
			}
			break;
			
		default:
			usage();
			ncp_close(conn);
			exit(1);
		}
	}

	if (optind != argc-1)
	{
		usage();
		ncp_close(conn);
		exit(1);
	}

	file_name = argv[optind];

	if (strcmp(file_name, "-") == 0)
	{
		file = 0;	/* stdin */
	}
	else
	{
		file = open(file_name, O_RDONLY, 0);
		if (file < 0)
		{
			perror("could not open file");
			ncp_close(conn);
			exit(1);
		}

		if (strlen(file_name) >= sizeof(pj.FnameHeader))
		{
			strncpy(pj.FnameHeader, file_name,
				sizeof(pj.FnameHeader));
		}
		else
		{
			strcpy(pj.FnameHeader, file_name);
		}

		if (strlen(pj.FnameBanner) == 0)
		{
			if (strlen(file_name) >= sizeof(pj.FnameBanner))
			{
				strncpy(pj.FnameBanner, file_name,
					sizeof(pj.FnameBanner));
			}
			else
			{
				strcpy(pj.FnameBanner, file_name);
			}
		}
	}

	memcpy(j.j.ClientRecordArea, &pj, sizeof(pj));

	str_upper(queue);

	if (ncp_scan_bindery_object(conn, 0xffffffff, NCP_BINDERY_PQUEUE,
				    queue, &q) != 0)
	{
		printf("could not find queue %s\n", queue);
		ncp_close(conn);
		exit(1);
	}
	
	if (ncp_create_queue_job_and_file(conn, q.object_id, &j) != 0)
	{
		printf("create error\n");
		ncp_close(conn);
		exit(1);
	}

	written = 0;
	do
	{
	        read_this_time = read(file, buf, sizeof(buf));
		if (read_this_time < 0)
		{
			break;
		}

		if (ncp_write(conn, j.file_handle,
			      written, read_this_time, buf) < read_this_time)
		{
			break;
		}

		written += read_this_time;
	} while (read_this_time > 0);

	close(file);

	if (ncp_close_file_and_start_job(conn, q.object_id, &j) != 0) {
		printf("close error\n");
		ncp_close(conn);
		return;
	}

	ncp_close(conn);
	return;
}

static void
usage(void)
{
	fprintf(stderr, "usage: %s [options] file\n", progname);
}

static void
help(void)
{
        printf("\n");
        printf("usage: %s [options] file\n", progname);
        printf("\n"
	       "-S server      Server name to be used\n"
               "-U username    Username sent to server\n"
               "-P password    Use this password\n"
               "-n             Do not use any password\n"
               "-C             Don't convert password to uppercase\n"
               "-q queue name  Name of the printing queue to use\n"
               "-d job desc    Job description\n"
               "-p path name   Path name to appear on banner\n"
               "-b bannername  Banner name (up to 12 chars)\n"
               "-f filename    Filename to appear on banner\n"
               "-l lines       Number of lines per page\n"
               "-r rows        Number of rows per page\n"
               "-t tab         Number of spaces per tab\n"
               "-T             Print server tab expantion\n"
               "-N             Surpress print server form feeds\n"
               "-F form #      Form number to print on\n"
               "-h             print this help text\n"
               "\n");
}
