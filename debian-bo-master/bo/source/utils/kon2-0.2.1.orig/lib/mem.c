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

#include	<sys/types.h>
#include	<mem.h>
#include	<malloc.h>

u_char	PortInb(unsigned short port)
{
	u_char	_rval;

	__asm__	volatile ("inb %1,%0"
		:"=a" (_rval)
		:"d" ((u_short) port));

	return(_rval);
}

void	wzero(void *head, int n)
{
	__asm__	("cld\n\t"
		"rep\n\t"
		"stosw"
		::"a" (0),
		"c" (n>>1),
		"D" ((long)head)
		:"cx","di");
}

void	wmove(void *dst, void *src, int n)
{
	__asm__	("cld\n\t"
		"rep\n\t"
		"movsw\n\t"
		::"c" (n>>1),
		"D" ((long)dst),
		"S" ((long)src)
		:"cx","di","si");
}

void	lmove(void *dst, void *src, int n)
{
	__asm__	("cld\n\t"
		"rep\n\t"
		"movsl\n\t"
		::"c" (n>>2),
		"D" ((long)dst),
		"S" ((long)src)
		:"cx","di","si");
}

void	SafeFree(void **p)
{
	if (*p) {
		free(*p);
		*p = NULL;
	}
}
