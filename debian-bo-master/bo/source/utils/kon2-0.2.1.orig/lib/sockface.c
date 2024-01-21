/*
 * KON2 - Kanji ON Console -
 * Copyright (C) 1992-1996 Takashi MANABE (manabe@papilio.tutics.tut.ac.jp)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Terrence R. Lambert.
 * 4. The name Terrence R. Lambert may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Julian R. Elischer ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE TERRENCE R. LAMBERT BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * 
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<unistd.h>
#include	<string.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/vt.h>
#include	<sys/ioctl.h>

#include	<interface.h>

u_int	wfontSize, sfontSize;
u_char	*sbFontBuff, *dbFontBuff;

/*
char	socketName[MAX_SOCKET_NAME+1];
*/

static u_char	clientNumber;

void	SocketKill(int sfd)
{
	close(sfd);
/*	unlink(socketName);*/
}

int	SocketRecCommand(int fd, struct messageHeader *mh)
{
	return(read(fd, mh, sizeof(struct messageHeader)));
}

int	SocketSendCommand(int fd, char cmd)
{
	struct messageHeader mh;
	
	mh.cmd = cmd;
	mh.cno = clientNumber;
	return(write(fd, &mh, sizeof(struct messageHeader)));
}

int	SocketSearchName(struct sockaddr *sa, int fd)
{
	struct	vt_stat vs;

	bzero(sa, sizeof(struct sockaddr));
	sa->sa_family = AF_UNIX;
	if (ioctl(fd, VT_GETSTATE, &vs) < 0) {
		return EOF;
	}
	sprintf(sa->sa_data, "%s%d", SOCKET_BASENAME, vs.v_active);
	return(0);
}

int	SocketClientOpen(void)
{
	int	s, len;
	struct	sockaddr sa;
	int	fd;

	if ((fd = open("/dev/console", O_WRONLY)) < 0)
	    fd = open("/dev/console", O_RDONLY);
	SocketSearchName(&sa, fd);
	s = socket(AF_UNIX, SOCK_STREAM, 0);

	len = sizeof(sa.sa_family) + strlen(sa.sa_data);
	if (connect(s, &sa, len) == -1) s = EOF;
	return(s);
}

int	SocketSendData(u_char *buff, int size, int fd)
{
	int	i;
	struct messageHeader mh;

	for (i = 0; i < size; i += BUFSIZ) {
		if ((size - i) < BUFSIZ)
			write(fd, (void *)buff, size - i);
		else
			write(fd, (void *)buff, BUFSIZ);
		SocketRecCommand(fd, &mh);
		if (mh.cmd != CHR_ACK) return(EOF);
		buff += BUFSIZ;
	}
	return(0);
}
