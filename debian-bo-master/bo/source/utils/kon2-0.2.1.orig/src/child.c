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
#include	<string.h>
#include	<unistd.h>

#include	<getcap.h>

#include	<defs.h>
#include	<version.h>
#include	<vc.h>

static char *startupStr, *execProg;

int	ConfigExecProg(const char *string)
{
	execProg = strdup(string);
	return SUCCESS;
}

static int	ConfigStartup(const char *string)
{
	startupStr = strdup(string);
	return SUCCESS;
}

static void	RunStartupCmd(void)
{
	char *p;

	p = strtok(startupStr, "\n");
	while(p) {
		system(p);
		p = strtok(NULL, "\n");
	}
}

static bool startupMessage;

static int	ConfigMessage(const char *confstr)
{
	startupMessage = BoolConf(confstr);
	return SUCCESS;
}

void	ChildInit(void)
{
	DefineCap("StartupMessage", ConfigMessage, "On");
	DefineCap("StartUp", ConfigStartup, NULL);
}

void	ChildCleanup(void)
{
	free(startupStr);
}

void	ChildStart(FILE *errfp)
{
	char	*shell, *tail, *tcap;
	char	buff[80];

	setgid(getgid());
	setuid(getuid());

	RunStartupCmd();

	sprintf(buff, "TERMCAP=:co#%d:li#%d:tc=console:",
		dInfo.txmax + 1, dInfo.tymax + 1);
	tcap = strdup(buff);
	putenv(tcap);

	if (startupMessage)
	    printf("\rKON2 Kanji On Console " VERSION
		   " using VT number %c\r\n"
		   "%*s\r\n"
		   "%*s\r\n", *(ttyname(fileno(errfp))+8),
		   dInfo.txmax,
		   "Copyright (C) "
		   "1993-1996  Takashi MANABE",
		   dInfo.txmax,
		   "1993, 1994 MAEDA Atusi   ");

/*
	printf("KON using VT number %c.\n\n",
	       *(ttyname(fileno(errfp))+8));
*/
	fflush(stdout);

	if (execProg)
	    execlp(execProg, execProg, 0);
	else {
	    if ((execProg = getenv("SHELL")) == NULL)
		execProg = "/bin/sh";
	    if ((tail = rindex(execProg, '/')) == NULL)
		tail = " sh";
	    sprintf(buff, "-%s", tail + 1);
	    execl(execProg, buff, 0);
	}
	fprintf(errfp, "KON> couldn't exec shell\r\n");
	fprintf(errfp, "%s: %s\r\n", execProg, strerror(errno));
	exit(EXIT_FAILURE);
}
