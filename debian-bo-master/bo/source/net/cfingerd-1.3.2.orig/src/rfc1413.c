/*
 * CFINGERD
 * RFC1413 implementation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "cfingerd.h"

void rfc1413_alarm(int signal)
{
    if (signal == SIGALRM)
	ident_user = "unknown@alarm.signal";
}

/* Self contained RFC1413 implementation.  Thanks to Joel Katz for parts of
 * the implementation.  Completely rewritten by yours truly to be self-
 * contained in a single program.  Simple, easy to use.
 */
char *get_rfc1413_data(void)
{
    int i, j;
    struct sockaddr_in sin;
    char buffer[1024], buf[256], uname[64], *bleah;

    bleah = (char *) malloc(256);
    bzero(bleah, 256);

    j = socket(AF_INET, SOCK_STREAM, 0);
    if (j < 2) {
	sprintf(bleah, "unknown@%s", remote_addr);
	syslog(LOG_ERR, "rfc1413-socket: %s", strerror(errno));
	return(bleah);
    }

    sin.sin_family = AF_INET;
    sin.sin_port = getservbyname("auth", "tcp")->s_port;
    sin.sin_addr.s_addr = inet_addr(ip_address);

    signal(SIGALRM, rfc1413_alarm);
    alarm(5);

    i = connect(j, (struct sockaddr *) &sin, sizeof(sin));
    if (i < 0) {
	syslog(LOG_ERR, "rfc1413-connect: %s", strerror(errno));
	close(j);
	sprintf(bleah, "unknown@%s", remote_addr);
	alarm(0);
	return(bleah);
    }

    sprintf(buffer, "%d,%d\n", remote_port, local_port);
    write(j, buffer, strlen(buffer));

    if (read(j, buf, 256) <= 0) {
	sprintf(bleah, "unknown@%s", remote_addr);
    } else {
	sscanf(buf, "%*d , %*d : %*[^ \t\n\r:] : %*[^\t\n\r:] : %[^\n\r]", uname);

	sprintf(bleah, "%s@%s", uname, remote_addr);
    }

    alarm(0);
    return(bleah);
}
