/*
 *  pserver.c
 *
 *  Copyright (C) 1996 by Volker Lendecke
 *
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <signal.h>
#include "ncplib.h"

struct nw_queue {
	struct ncp_conn *conn;

	char queue_name[NCP_BINDERY_NAME_LEN];
	__u32 queue_id;
	__u16 job_type;

	char *command;
};

static struct nw_queue q;

static int term_request;
static char *progname;

static int
init_queue(struct ncp_conn *conn, char *queue_name,
	   char *command, struct nw_queue *q);

static int
poll_queue(struct nw_queue *q);

static void
usage(void)
{
	fprintf(stderr, "usage: %s [options] file\n", progname);
	exit(1);
}

static void
help(void)
{
        printf("\n");
        printf("usage: %s [options]\n", progname);
        printf("\n"
	       "-S server      Server name to be used\n"
               "-U username    Print Server name sent to server\n"
               "-P password    Use this password\n"
               "-n             Do not use any password\n"
               "-C             Don't convert password to uppercase\n"
               "-q queue name  Name of the printing queue to use\n"
	       "-c command     Name of print command, default: 'lpr'\n"
	       "-j job type    Type of job (Form number) to service\n"
	       "-t timeout     Polling interval, default: 30 sec\n"
	       "-d             Debug: don't daemonize\n"
               "-h             print this help text\n"
	       "\n");
}

#ifndef NCP_BINDERY_PSERVER
#define NCP_BINDERY_PSERVER (0x0007)
#endif

static void
terminate_handler()
{
	signal(SIGTERM,terminate_handler);
	signal(SIGINT, terminate_handler);
	term_request=1;
}

/* Daemon_init is taken from Stevens, Adv. Unix programming */
static int
daemon_init(void)
{
	pid_t pid;

	if ((pid = fork()) < 0)
	{
		return -1;
	}
	else if (pid != 0)
	{
		exit(0);	/* parent vanishes */
	}

	/* child process */
	setsid();
	chdir("/");
	umask(0);
	close(0);
	close(1);
	close(2);
	return 0;
}

int
main(int argc, char *argv[])
{
	struct ncp_conn *conn;
	int poll_timeout = 30;
	int opt;
	int job_type = 0xffff;
	int debug = 0;
	int i;
	long err;

	char *queue_name = NULL;

	char default_command[] = "lpr";
	char *command = default_command;

	progname = argv[0];

	for (i = 1; i < argc; i += 1)
	{
		if (   (strcmp(argv[i], "-h") == 0)
		    || (strcmp(argv[i], "-?") == 0))
		{
			help();
			exit(0);
		}
	}

	for (i = 1; i < argc; i += 1)
	{
		if (strcmp(argv[i], "-d") == 0)
		{
			debug = 1;
			break;
		}
	}

	if (debug == 0)
	{
		daemon_init();
		openlog("pserver", LOG_PID, LOG_LPR);
	}

	if ((conn = ncp_initialize_as(&argc, argv, 1,
				      NCP_BINDERY_PSERVER, &err)) == NULL)
	{
		com_err(argv[0], err, "in ncp_initialize");
		return 1;
	}

	while ((opt = getopt(argc, argv, "q:c:j:t:dh")) != EOF)
	{
		switch (opt)
		{
		case 'q':
			queue_name = optarg;
			break;
		case 'c':
			command = optarg;
			break;
		case 'j':
			job_type = atoi(optarg);
			break;
		case 't':
			poll_timeout = atoi(optarg);
			break;
		case 'd':
			debug = 1;
			break;
		case 'h':
			break;
		default:
			usage();
			return -1;
		}
	}

	if (argc != optind)
	{
		usage();
		return -1;
	}

	memzero(q);

	if (queue_name == NULL)
	{
		fprintf(stderr, "You must specify a queue\n");
		return 1;
	}

	if (init_queue(conn, queue_name, command, &q) != 0)
	{
		perror("Could not init queue");
		ncp_close(conn);
		return 1;
	}

	q.job_type = job_type;

	term_request = 0;
	signal(SIGTERM,terminate_handler);
	signal(SIGINT, terminate_handler);

	while (1)
	{
		if (   (poll_queue(&q) != 0)
		    && (term_request == 0))
		{
			continue;
		}

		if (term_request != 0)
		{
			break;
		}
		sleep(poll_timeout);
	}

	ncp_detach_from_queue(conn, q.queue_id);

	ncp_close(conn);
	return 0;
}

static int
init_queue(struct ncp_conn *conn, char *queue_name, char *command,
	   struct nw_queue *q)
{
	struct ncp_bindery_object obj;

	str_upper(queue_name);

	q->conn = conn;
	q->command = command;
	
	if (ncp_get_bindery_object_id(conn, NCP_BINDERY_PQUEUE,
				      queue_name, &obj) != 0)
	{
		fprintf(stderr, "Queue %s not found\n", queue_name);
		return -1;
	}

	q->queue_id = obj.object_id;
	memcpy(q->queue_name, obj.object_name, sizeof(q->queue_name));

	if (ncp_attach_to_queue(conn, q->queue_id) != 0)
	{
		fprintf(stderr, "Could not attach to queue %s\n",
			queue_name);
		return -1;
	}
	return 0;
}
	

static int
poll_queue(struct nw_queue *q)
{
	struct queue_job job;
	int fd[2];
	int pid;

	if (ncp_service_queue_job(q->conn, q->queue_id, q->job_type,
				  &job) != 0)
	{
		/* No job for us */
		return 0;
	}

	if (pipe(fd) < 0)
	{
		syslog(LOG_ERR, "pipe error: %m");
		goto fail;
	}

	if ((pid = fork()) < 0)
	{
		syslog(LOG_ERR, "fork error: %m");
		goto fail;
	}

	if (pid > 0)
	{
		/* parent */
		char buf[1024];
		size_t result;
		off_t offset = 0;

		close(fd[0]);	/* close read end */

		while ((result = ncp_read(q->conn, job.file_handle, offset,
					  sizeof(buf), buf)) > 0)
		{
			offset += result;
			if (write(fd[1], buf, result) != result)
			{
				goto fail;
			}
		}

		close(fd[1]);	/* and close write end */

		if (waitpid(pid, NULL, 0) < 0)
		{
			syslog(LOG_ERR, "waitpid: %m\n");
		}
	}
	else
	{
		/* child */

		close(fd[1]);	/* close write end */

		if (fd[0] != STDIN_FILENO)
		{
			if (dup2(fd[0], STDIN_FILENO) != STDIN_FILENO)
			{
				syslog(LOG_ERR, "dup2 error: %m\n");
				close(fd[0]);
				exit(1);
			}
			close(fd[0]);
		}

		execl("/bin/sh", "sh", "-c", q->command, NULL);
		syslog(LOG_ERR, "exec error: %m\n");
		close(fd[0]);
		exit(1);
	}

	ncp_finish_servicing_job(q->conn, q->queue_id, job.j.JobNumber,0);
	return 1;		

 fail:
	ncp_abort_servicing_job(q->conn, q->queue_id, job.j.JobNumber);
	/* We tell that we did not have a job to avoid overloading
	   when something's wrong */
	return 0;
}
