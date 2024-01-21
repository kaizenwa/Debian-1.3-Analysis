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

#include	<config.h>

#include	<stdio.h>
#include	<stdlib.h>
#include	<sys/types.h>
#include	<sys/file.h>
#include	<string.h>
#include	<unistd.h>
#include	<sys/ipc.h>
#include	<sys/shm.h>

#include	<interface.h>
#include	<vt.h>
#include	<fnld.h>

struct fontRegs *dbFReg, *sbFReg;

u_char	*GetShmem();

void FontDetach(bool down)
{
    int i;

    i = 0;
    while (fSRegs[i].registry) {
	if (fSRegs[i].stat & FR_ATTACH)
	    shmdt(fSRegs[i].bitmap - sizeof(struct fontInfo));
	if (down) DownShmem(i|CHR_SFLD);
	fSRegs[i].width = fSRegs[i].high =
	    fSRegs[i].size = fSRegs[i].stat = 0;
	i ++;
    }
    i = 0;
    while (fDRegs[i].registry) {
	if (fDRegs[i].stat & FR_ATTACH)
	    shmdt(fDRegs[i].bitmap - sizeof(struct fontInfo));
	if (down) DownShmem(i|CHR_DFLD);
	fDRegs[i].width = fDRegs[i].high =
	    fDRegs[i].size = fDRegs[i].stat = 0;
	i ++;
    }
}

void FontAttach()
{
    int i;
    u_char *font;
    struct fontInfo *fi;

    i = 0;
    while (fSRegs[i].registry) {
	if ((font = GetShmem(i|CHR_SFLD)) != NULL) {
	    fi = (struct fontInfo*)font;
	    fSRegs[i].high = fi->high;
	    fSRegs[i].stat = FR_ATTACH;
	    fSRegs[i].size = fi->size;
	    fSRegs[i].bitmap = font + sizeof(struct fontInfo);
	    sbFReg = &fSRegs[i];
	} else fSRegs[i].stat = 0;
	i ++;
    }
    if (fSRegs[lInfo.sb].stat) sbFReg = &fSRegs[lInfo.sb];
#if 1
    i = 0;
    while (fSRegs[i].registry) {
	if (!fSRegs[i].stat) {
	    fSRegs[i].high = sbFReg->high;
	    fSRegs[i].size = sbFReg->size;
	    fSRegs[i].bitmap = sbFReg->bitmap;
	    fSRegs[i].stat = FR_PROXY;
	}
	i ++;
    }
#endif
    i = 0;
    while (fDRegs[i].registry) {
	if ((font = GetShmem(i|CHR_DFLD)) != NULL) {
	    fi = (struct fontInfo*)font;
	    fDRegs[i].high = fi->high;
	    fDRegs[i].stat = FR_ATTACH;
	    fDRegs[i].size = fi->size;
	    fDRegs[i].bitmap = font + sizeof(struct fontInfo);
	}
	i ++;
    }
    dbFReg = &fDRegs[lInfo.db];
}

void
VgaLoadRomFont(char *fontbuff)
{
    key_t shmkey;
    int	shmid, i;
    u_char *shmbuff, *buff;
    struct fontInfo fi;

    shmkey = ftok(CONFIG_NAME, CHR_SFLD);
/*
    if ((shmid = shmget(shmkey, sizeof(struct fontInfo), 0444)) < 0)
	return;
    if (shmat(shmid, 0, SHM_RDONLY)) return;
*/
    fi.size = 256 * 16;
    fi.high = 16;
    fi.width = 8;
    fi.type = CHR_SFLD;
    shmid = shmget(shmkey, fi.size+sizeof(struct fontInfo),
		   IPC_CREAT|0666);
    shmbuff = shmat(shmid, 0, 0);
    memcpy(shmbuff, &fi, sizeof(struct fontInfo));
    buff = shmbuff + sizeof(struct fontInfo);
/*
    memcpy(shmbuff + sizeof(struct fontInfo), font, fi.size);
*/

    for (i = 0; i < fi.size; i += fi.high) {
	memcpy(&(buff[i]), &(fontbuff[i*2]), fi.high);
    }
    shmdt(shmbuff);
/*

    if (!sbFontBuff) {
	if ((sbFontBuff = malloc(VGA_FONT_SIZE *
				 VGA_FONT_HEIGHT)) == NULL) return;
	sfontSize = VGA_FONT_SIZE * VGA_FONT_HEIGHT;
	con.sfonth = VGA_FONT_HEIGHT;
	for (i = 0; i < sfontSize; i += VGA_FONT_HEIGHT) {
	    memcpy(&(sbFontBuff[i]), &(fontBuff1[i*2+vgaFontOffset]),
		   VGA_FONT_HEIGHT);
	}
    }
*/
}
