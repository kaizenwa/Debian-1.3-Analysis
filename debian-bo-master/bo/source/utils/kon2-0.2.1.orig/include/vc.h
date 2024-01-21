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

/* vc -- high-level console driver */

#ifndef VC_H
#define VC_H

#define	ATTR_ULINE	0x80	/* under line */
#define	ATTR_REVERSE	0x40	/* reverse */
#define	ATTR_HIGH	0x20	/* high */

#define	LATCH_S		0x0 /* single byte char */
#define	LATCH_1		0x20 /* double byte char 1st byte */
#define	LATCH_2		0x40 /* double byte char 2nd byte */

#define	CLEAN_S		0x80
#define	CODEIS_1	LATCH_1
#define	CODEIS_2	LATCH_2
#define	LANG_CODE	0x0F
/*
#define	LANG_DCODE	LANG_CODE|CODEIS_1
#define	LANG_SCODE	LANG_CODE
*/

extern void	ConsoleInit(const char *video_type);
extern void	ConsoleStart(void);
extern void	ConsoleCleanup(void);
extern void	TextClearAll(void);
extern void	TextClearEol(u_char);
extern void	TextClearEos(u_char);
extern void	TextDeleteChar(int);
extern void	TextInsertChar(int);
extern void	TextMoveDown(int top, int btm, int line);
extern void	TextMoveUp(int top, int btm, int line);
extern void	TextMode(void);
extern void	GraphMode(void);
extern void	ScrollUp(int);
extern void	ScrollDown(int);
extern void	TextWput(u_char ch1, u_char ch2);
extern void	TextSput(u_char ch);
extern void	TextReverse(int fx, int fy, int tx, int ty);
extern void	TextRefresh(void);
extern void	TextInvalidate(void);
extern void	TextCopy(int fx, int fy, int tx, int ty);
extern void	TextPaste(void);
extern void	PollCursor(bool wakeup); /* Called to wakeup, or every 0.1 sec when idle */
extern void	Beep(void);

struct cursorInfo {
    short kanji;	 /* �����ξ�ˤ���� TRUE */
    u_int addr;		 /* VRAM ���ɥ쥹 */
    bool sw;		 /* FALSE �ʤ�ɽ���ػ� */
    int	interval;	 /* ���Ǵֳ� */
    int	count;		 /* �����ѥ������ */
    bool shown;		 /* ɽ����ե饰 */
};

/* video driver interface */
struct videoInfo {
    bool
	has_hard_scroll;	 /* �ϡ��ɥ������뤬�Ȥ��뤫�ɤ��� */
    void
	(*init)(void),		 /* ����� */
	(*text_mode)(void),	 /* �ƥ����ȥ⡼�ɤ����ؤ� */
	(*graph_mode)(void),	 /* ����ե��å��⡼�ɤ����ؤ� */
	(*wput)(u_char *code, u_char fc, u_char bc), /* �������� */
	(*sput)(u_char *code, u_char fc, u_char bc), /* ANK���� */
	(*set_cursor_address)(struct cursorInfo *c, u_int x, u_int y),
	/* �������� c �Υ��ɥ쥹�� (x,y) ������ */
	(*set_address)(u_int i),
	/* ʸ���񤭹��ߥ��ɥ쥹�� i ʸ���ܤ����� */
	(*cursor)(struct cursorInfo *),	/* ���������ȥ��� */
	(*clear_all)(void),	 /* ���̥��ꥢ */
	(*screen_saver)(bool),	 /* �����꡼��֥��/����֥�� */
	(*detatch)(void),	 /* �ɥ饤�в��� */
	/* �ϡ��ɥ������뤬�Ȥ��ʤ���аʲ���NULL */
	(*set_start_address)(void),	/* ɽ�����ϥ��ɥ쥹���� */
	(*hard_scroll_up)(int lines), 	/* �ϡ��ɥ������륢�å� */
	(*hard_scroll_down)(int lines);	/* �ϡ��ɥ������������ */
};

struct dispInfo {
    int
	gsize;
    short
	gxdim,
	gydim,
	txmax,
	tymax,
	glineChar,	/* text ����ʬ�� graph �Կ� */
	glineByte,	/* graph ����ʬ�ΥХ��ȿ� */
	tlineByte;	/* text ����ʬ�ΥХ��ȿ� */
};

extern struct dispInfo		dInfo;
extern struct cursorInfo	cInfo;
extern struct videoInfo		vInfo;

#endif
