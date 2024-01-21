! mbr.S - a replacement master boot record
! Copyright (C) 1995 Neil Turton
!
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
#undef COMPAT
!
! We are loaded at 0:0x7c00 by the BIOS, but need to load boot sectors
! here, so we have got to move out of the way. We move to 0:DEST_ADDR
! 0x600 is as good a place as any.
#define DEST_ADDR 0x600
! The time when we started up.
stime	equ 0x800
.org DEST_ADDR
	cld
	xor ax,ax
	mov ds,ax
	mov es,ax
	mov si,#0x7c00
	mov di,#DEST_ADDR ! Here will do
	mov cx,#0x100
	repz
	movsw
	mov si,#table+0x30 ! Set bootable flags from partition table
	mov al,#8
	jmp 0:cont ! Switch to the newly made copy. From now on labels work.
cont:	cmpb (si),#0 ! Check for bootable flag
	jz l7
	or flags,al ! Set in flags
l7:	sub si,#0x10
	shr al,1
	jnc cont ! Repeat until bit gets shifted out of al
	cmpb delay,#255
	jz l3 ! Immediate interrupt
	int 0x1a ! Get time
	mov stime+2,cx
	mov stime,dx
l0:	testb def,#0x80 ! Key press interrupt allowed
	jz l1
	mov ah,#1
	int 0x16
	jnz l3 ! Interrupted
l1:	testb def,#0x40 ! Shift/Ctrl allowed
	jz l2
	test 0x417,#0xf ! Check shift status
	jnz l3 ! Interrupted
l2:	xor ax,ax ! Check timer.
	int 0x1a
	sub dx,stime ! This code gets confused at midnight
	sbb cx,stime+2 ! (FIXME)
	sub dl,delay
	sbb dh,#0
	sbb cx,#0
	jb l0 ! Time not passed
l4:	mov al,def ! Boot default
	and al,#7
	cmp al,#7
	jnz boot2 ! Boot number al
	mov si,#table ! Boot first partition marked bootable
	mov al,#0
	mov cx,#4
l5:	cmpb (si),#0
boot2:	jnz boot
	inc al
	add si,#0x10
	loop l5 ! Try next partition
l3:	mov cl,flags ! Interrupted (or no default)
	mov si,#chars ! List of chars to print
l9:	lodsb
	shr cl,#1 ! Shift next bit into carry
	jnb l8
	mov ah,#0xe ! Print char
	xor bx,bx
	int 0x10
l8:	or cl,cl ! Any more bits set?
	jnz l9
	mov ax,#0xe3a ! Print a colon
	xor bx,bx
	int 0x10
l11:	xor ax,ax ! Get user input
	int 0x16
	cmp al,#0
	jz l11 ! Skip extended chars
	cmp al,#13 ! RETURN pressed
	jz l4 ! Boot default
	cmp al,#'a ! CPP isn't quite compatible with as86
	jb l10
	cmp al,#'z ! ' This may make it a bit more obvious
	ja l10
	sub al,#0x20 ! Upper case input
l10:	mov bl,al
	mov si,#chars
	mov cl,flags
l12:	lodsb
	shr cl,#1
	jnb l13
	cmp bl,al
	jz l14
l13:	or cl,cl
	jnz l12
	jmp l11
l14:	mov ax,#0xe0d
	xor bx,bx
	int 0x10
	mov ax,si
	sub ax,#chars+1
	cmp al,#7 ! Check if advanced mode selected
	jnz boot
	mov al,#0x1f
	mov flags,al
il3:	jmp l3
	
boot:	xor dx,dx ! Boot partition al
	mov cx,#1
	cmp al,#4
	jz sread ! Boot from floppy
	jnb il3 ! Illegal - interrupt
	xor ah,ah
	mov cl,#4
	shl al,cl
	mov si,#table
	add si,ax
	mov dl,#0x80
	mov dh,(si+1)
	mov cx,(si+2)
sread:	mov di,#5
l6:	push cx
	push dx
	push di
	mov ax,#0x201
	mov bx,#0x7c00
	int 0x13
	pop di
	pop bx
	pop cx
	jnb lbit
	dec di
	jnz l6
	jmp il3
lbit:	cmp 0x7dfe,#0xaa55
	jnz il3
	jmp 0:0x7c00
chars:	db '1,'2,'3,'4,'F,0,0,'A
data:	.ascii "NDTmbr" ! Sig string
	db 0,0 ! From version 0.0
	db 0,0 ! to version 0.0
#ifndef COMPAT
! Setup the flags with cool settings. Have no delay, but allow
! interrupt. We mark everything bootable, but default to booting
! the active partition.
flags:	db 0x90 ! Floppy bootable. Advanced mode available.
def:	db 0xc7 ! Interrupt by anything.
delay:	db 18 ! About 1 second
drive:	db 0x80 ! /dev/hda1
#else
! Setup the flags to mimic standard MBRs unless interrupted
! flags:bits 0-4 marks partitions 1-4 and floppy bootable
!	bit 7 = unlocked
! def:	bits 0-2 default boot partition (0-4 or 7=active)
!	bits 6,7 interrupt on (shift/ctrl,key press)
flags:	db 0 ! No specially bootable partitions. Advanced mode disabled.
def:	db 7 ! Boot active partition
delay:	db 0 ! Delay in timer ticks. 0=>No delay. 255=>Interrupt.
drive:	db 0x80
#endif
.org DEST_ADDR+0x1bc
	dw data-DEST_ADDR ! Pointer to the data
! Format of partition table entry
!   00 byte Boot indicator
!   01 byte start head
!   02 byte start sector
!   03 byte start cylinder
!   04 byte type
!   05 byte end head
!   06 byte end sector
!   07 byte end cylinder
!   08 dw   sectors before partition
!   0c dw   sectors in partition
table:	.space 0x40 ! The partition table
	dw 0xaa55
