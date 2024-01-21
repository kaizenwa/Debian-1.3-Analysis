/* first.S  -  LILO first stage boot loader */

/* Copyright 1992-1996 Werner Almesberger. See file COPYING for details. */


#include <linux/config.h>

#define LILO_ASM
#include "lilo.h"

#ifndef LCF_NO1STDIAG
#define CYL_CHECK
#endif


	.text

	.globl	_main

	.org	0

_main:	jmp	start

	.org	2

! Boot device parameters. They are set by the installer.

	.ascii	"LILO"
	.word	STAGE_FIRST
	.word	VERSION

timeout:.word	0		! input timeout
delay:	.word	0		! boot delay
port:	.byte	0		! COM port (0 = unused, 1 = COM1, etc.)
sparam:	.byte	0		! serial port parameters (0 = unused)

tstamp:	.long	0		! timestamp

d1_cx:	.word	0		! first descriptor sector address
d1_dx:	.word	0
d1_al:	.byte	0		! (unused)

d2_cx:	.word	0		! second descriptor sector address
d2_dx:	.word	0
d2_al:	.byte	0		! (unused)

dc_cx:	.word	0		! default command-line sector address
dc_dx:	.word	0
dc_al:	.byte	0		! (unused)

prompt:	.byte	0		! indicates whether to always enter prompt
				! (also used as alignment byte)

ms_len:	.word	0		! initial greeting message
ms_cx:	.word	0
ms_dx:	.word	0
ms_al:	.byte	0		! (unused)

d_addr:				! second stage sector addresses

	.org	CODE_START_1

ext_si:	.word	0		! external interface
ext_es:	.word	0
ext_bx:	.word	0
ext_dl:	.byte	0

start:	mov	ax,#BOOTSEG	! set DS
	mov	ds,ax
	mov	ext_es,es	! copy possible external parameters
	mov	ext_si,si
	mov	ext_bx,bx
	mov	ext_dl,dl
	mov	ax,#FIRSTSEG	! beam us up ...
	mov	es,ax
	mov	cx,#256
	sub	si,si
	sub	di,di
	cld
	rep
	movsw
	jmpi	go,FIRSTSEG

go:	cli			! no interrupts
	mov	ds,ax		! AX is already set
	mov	es,ax		! (ES may be wrong when restarting)
	mov	sp,#STACK	! set the stack
	mov	ax,#STACKSEG
	mov	ss,ax
	sti			! now it is safe

	mov	al,#0x0d	! gimme a CR ...
	call	display
	mov	al,#0x0a	! ... an LF ...
	call	display
	mov	al,#0x4c	! ... an 'L' ...
	call	display

lagain:	mov	si,#d_addr	! ready to load the second stage loader
	mov	bx,#SECOND
	cld
sload:	lodsw			! get CX
	mov	cx,ax
	lodsw			! get DX
	mov	dx,ax
	or	ax,cx		! at EOF ?
	jz	done		! yes -> start it
	inc	si		! skip the length byte
	call	cread
	jc	error		! error -> start over again
	add	bx,#512		! next sector
	jmp	sload
error:
#ifndef LCF_NO1STDIAG
	push	ax		! display a space
	mov	al,#32
	call	display
	pop	ax
	mov	al,ah		! display error code
	call	bout
#endif
	xor	ax,ax		! reset the FDC
	mov	dl,al
	int	0x13
	jmp	lagain		! redo from start
done:	mov	al,#0x49	! display an 'I'
	call	display
	jmpi	0,SECONDSEG	! start the second stage loader

#ifndef LCF_NO1STDIAG
bout:	push	ax		! display one byte
	shr	al,#4
	call	nout
	pop	ax
nout:	and	al,#15		! display one nibble
	add	al,#48
	cmp	al,#58
	jb	nokay
	add	al,#7
nokay:				! fall through
#endif

display:xor	bh,bh		! display on screen
	mov	ah,#14
	int	0x10
	ret

linerr:	pop	dx		! discard stack contents
	pop	cx
	pop	bx
	ret			! (carry is already set)

cread:	test	dl,#LINEAR_FLAG	! linear address ?
	jz	readsect	! no -> go on
	and	dl,#0xff-LINEAR_FLAG ! remove flag

!
! Translate the linear address into a sector/track/cylinder address
!
	push	bx		! BX is used as scratch
	push	cx		! LSW
	push	dx		! MSW with drive
	mov	ah,#8		! get drive geometry (do not clobber ES:DI)
	int	0x13
	jc	linerr		! error -> quit
	mov	bl,dh		! BL <- #heads
	pop	dx		! get MSW
	mov	t_drive,dl	! save drive
	mov	dl,dh		! linear address into DX:AX
	xor	dh,dh
	mov	bh,dh		! (clear BH too)
	pop	ax
#ifdef CYL_CHECK
	push	cx		! compute #cyls-1
	xchg	ch,cl
	rol	ch,1
	rol	ch,1
	and	ch,#3
	mov	n_cyl,cx	! save #cyls-1
	pop	cx
#endif
	and	cx,#0x3f	! CX <- #secs
	div	cx		! AX <- track, DX <- sector
	inc	dl
	mov	t_sector,dl
	xor	dx,dx		! divide by #heads
	inc	bx
	div	bx		! AX <- cylinder, DX <- head
	mov	dh,dl		! set up DX (head:drive)
	mov	dl,t_drive
#ifdef CYL_CHECK
	cmp	ax,n_cyl	! valid cylinder number ?
	ja	linerr3		! no -> error
#endif
	xchg	ah,al		! build cylinder number
	ror	al,1
	ror	al,1
	or	al,t_sector
	mov	cx,ax
	pop	bx		! restore BX
readsect:
	mov	ax,#0x201	! read one sector
	int	0x13
	ret			! quit, possibly with errors

#ifdef CYL_CHECK
linerr3:pop	bx		! pop BX and linear address
	xor	ax,ax		! zero indicates internal error
	stc			! error
	ret

n_cyl:	.word	0		! temporary space
#endif
t_drive:.byte	0
t_sector:.byte	0


/* Here are at least 66 bytes of free space. This is reserved for the
   partition table and the boot signature. */

theend:
