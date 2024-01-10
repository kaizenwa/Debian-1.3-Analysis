## Walkthrough of Linux 2.0.30's Bootstrap Code

### Documented Source Code

#### Boot Sector Code (linux/arch/i386/boot/bootsect.S)

```asm
!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x7F00 is 0x7F000 bytes = 508kB, more than enough for current
! versions of linux which compress the kernel
!
#include <linux/config.h>
SYSSIZE = DEF_SYSSIZE
!
!	bootsect.s		Copyright (C) 1991, 1992 Linus Torvalds
!	modified by Drew Eckhardt
!	modified by Bruce Evans (bde)
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! itself out of the way to address 0x90000, and jumps there.
!
! bde - should not jump blindly, there may be systems with only 512K low
! memory.  Use int 0x12 to get the top of memory, etc.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most (8*65536-4096) bytes long. This should 
! be no problem, even in the future. I want to keep it simple. This 508 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix (and especially now that the kernel is 
! compressed :-)
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole tracks at a time whenever possible.

.text

SETUPSECS = 4				! nr of setup-sectors
BOOTSEG   = 0x07C0			! original address of boot-sector
INITSEG   = DEF_INITSEG			! we move boot here - out of the way
SETUPSEG  = DEF_SETUPSEG		! setup starts here
SYSSEG    = DEF_SYSSEG			! system loaded at 0x10000 (65536).

! ROOT_DEV & SWAP_DEV are now written by "build".
ROOT_DEV = 0
SWAP_DEV = 0
#ifndef SVGA_MODE
#define SVGA_MODE ASK_VGA
#endif
#ifndef RAMDISK
#define RAMDISK 0
#endif 
#ifndef CONFIG_ROOT_RDONLY
#define CONFIG_ROOT_RDONLY 0
#endif

! ld86 requires an entry symbol. This may as well be the usual one.
.globl	_main
_main:
#if 0 /* hook for debugger, harmless unless BIOS is fussy (old HP) */
	int	3
#endif
	mov	ax,#BOOTSEG
	mov	ds,ax            ! %ds = 0x7c00
	mov	ax,#INITSEG
	mov	es,ax            ! %es = 0x9000
	mov	cx,#256
	sub	si,si
	sub	di,di
	cld
	rep
	movsw                ! Copies bootsector to 0x90000
	jmpi	go,INITSEG

go:	mov	ax,cs           ! %ax = 0x9000
	mov	dx,#0x4000-12   ! 0x4000 is arbitrary value >= length of
                        ! bootsect + length of setup + room for stack
                        ! 12 is disk parm size

! bde - changed 0xff00 to 0x4000 to use debugger at 0x6400 up (bde).  We
! wouldn't have to worry about this if we checked the top of memory.  Also
! my BIOS can be configured to put the wini drive tables in high memory
! instead of in the vector table.  The old stack might have clobbered the
! drive table.

	mov	ds,ax       ! %ds = %es = %ss = 0x9000
	mov	es,ax
	mov	ss,ax		! put stack at INITSEG:0x4000-12.
	mov	sp,dx       ! %esp = 9000:3fee
/*
 *	Many BIOS's default disk parameter tables will not 
 *	recognize multi-sector reads beyond the maximum sector number
 *	specified in the default diskette parameter tables - this may
 *	mean 7 sectors in some cases.
 *
 *	Since single sector reads are slow and out of the question,
 *	we must take care of this by creating new parameter tables
 *	(for the first disk) in RAM.  We will set the maximum sector
 *	count to 18 - the most we will encounter on an HD 1.44.  
 *
 *	High doesn't hurt.  Low does.
 *
 *	Segments are as follows: ds=es=ss=cs - INITSEG,
 *		fs = 0, gs = parameter table segment
 */

	push #0
	pop	fs
	mov	bx,#0x78		! fs:bx is parameter table address
	seg fs
	lgs	si,(bx)			! gs:si is source
                        ! gs:si = fs:bx = 0000:0078
	mov	di,dx			! es:di is destination
                        ! es:di = 9000:3fee = above the stack
	mov	cx,#6			! copy 12 bytes
	cld

	rep
	seg gs              ! seg gs modifies movs to copy from 
	movsw               !   gs:si to es:di instead of ds:si to es:di,
                        !   which corresponds to the bootstack here
	mov	di,dx
	movb 4(di),*18		! patch sector count
                        ! 9000:3ff2 = 18
	seg fs
	mov	(bx),di         ! Adjust the offset of the disk parameter
                        !   table's address to be 0x3fee
	seg fs
	mov	2(bx),es        ! Adjust the segment of the disk parameter
                        !   table's address to be 0x9000
                        ! Hence, the previous two code blocks update
                        !   the address of the disk parameter table
                        !   to the address of its copy on the stack

	mov	ax,cs           ! %ax = 0x9000
	mov	fs,ax
	mov	gs,ax           ! %fs = %gs = disk parameter table segment

! DISK - RESET DISK SYSTEM
!
!  AH = 00h
!  DL = drive (if bit 7 is set both hard disks and floppy disks reset)
!
! Return:
!  AH = status (see #00234)
!  CF clear if successful (returned AH=00h)
!  CF set on error
!
! Note: Forces controller to recalibrate drive heads (seek to track 0).
! For PS/2 35SX, 35LS, 40SX and L40SX, as well as many other systems,
! both the master drive and the slave drive respond to the Reset function
! that is issued to either drive

	xor	ah,ah			! reset FDC 
	xor	dl,dl
	int 	0x13	

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

! DISK - READ SECTOR(S) INTO MEMORY
!
!  AH = 02h
!  AL = number of sectors to read (must be nonzero)
!  CH = low eight bits of cylinder number
!  CL = sector number 1-63 (bits 0-5)
!       high two bits of cylinder (bits 6-7, hard disk only)
!  DH = head number
!  DL = drive number (bit 7 set for hard disk)
!  ES:BX -> data buffer
!
! Return:
!  CF set on error
!  if AH = 11h (corrected ECC error), AL = burst length
!  CF clear if successful
!  AH = status (see #00234)
!  AL = number of sectors transferred (only valid if CF set for some BIOSes)

load_setup:
	xor	dx, dx          ! drive 0, head 0
	mov	cx,#0x0002      ! sector 2, track 0
	mov	bx,#0x0200      ! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPSECS    ! service 2, nr of sectors
                                ! (assume all on head 0, track 0)
	int	0x13            ! read it
	jnc	ok_load_setup   ! ok - continue

	push	ax          ! dump error code
	call	print_nl
	mov	bp, sp
	call	print_hex
	pop	ax	
	
	xor	dl, dl          ! reset FDC
	xor	ah, ah
	int	0x13
	jmp	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

#if 0

! bde - the Phoenix BIOS manual says function 0x08 only works for fixed
! disks.  It doesn't work for one of my BIOS's (1987 Award).  It was
! fatal not to check the error code.

	xor	dl,dl
	mov	ah,#0x08		! AH=8 is get drive parameters
	int	0x13
	xor	ch,ch
#else

! It seems that there is no BIOS call to get the number of sectors.  Guess
! 18 sectors if sector 18 can be read, 15 if sector 15 can be read.
! Otherwise guess 9.

	xor	dx, dx          ! drive 0, head 0
	mov	cx,#0x0012      ! sector 18, track 0
	mov	bx,#0x0200+SETUPSECS*0x200  ! address after setup (es = cs)
	mov	ax,#0x0201      ! service 2, 1 sector
	int	0x13
	jnc	got_sectors
	mov	cl,#0x0f        ! sector 15
	mov	ax,#0x0201      ! service 2, 1 sector
	int	0x13
	jnc	got_sectors
	mov	cl,#0x09

#endif

got_sectors:
	seg cs
	mov	sectors,cx      ! set nb of sectors read
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03        ! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#9
	mov	bx,#0x0007      ! page 0, attribute 7 (normal)
	mov	bp,#msg1        ! "Loading"
	mov	ax,#0x1301      ! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax       ! segment of 0x010000
	call	read_it
	call	kill_motor
	call	print_nl

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	or	ax,ax
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208      ! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c      ! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
	mov	ax,#0x0200      ! /dev/fd0 - autodetect
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi 0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPSECS	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die         ! es must be at 64kB boundary
	xor bx,bx       ! bx is starting address within segment
rp_read:
	mov ax,es
	sub ax,#SYSSEG
	cmp ax,syssize      ! have we loaded all yet?
	jbe ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ah,#0x10
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	pusha
	pusha	
	mov	ax, #0xe2e  ! loading... message 2e = .
	mov	bx, #7
 	int	0x10
	popa		

	mov	dx,track
	mov	cx,sread
	inc	cx
	mov	ch,dl
	mov	dx,head
	mov	dh,dl
	and	dx,#0x0100
	mov	ah,#2
	
	push	dx              ! save for error dump
	push	cx
	push	bx
	push	ax

	int	0x13
	jc	bad_rt
	add	sp, #8
	popa
	ret

bad_rt:	push ax             ! save error code
	call print_all          ! ah = error, al = read
	
	
	xor ah,ah
	xor dl,dl
	int 0x13
	

	add	sp, #10
	popa	
	jmp read_track

/*
 *	print_all is for debugging purposes.  
 *	It will print out all of the registers.  The assumption is that this is
 *	called from a routine, with a stack frame like
 *	dx 
 *	cx
 *	bx
 *	ax
 *	error
 *	ret <- sp
 *
*/
 
print_all:
	mov	cx, #5      ! error code + 4 registers
	mov	bp, sp	

print_loop:
	push cx         ! save count left
	call print_nl   ! nl for readability

	cmp	cl, 5
	jae	no_reg      ! see if register name is needed
	
	mov	ax, #0xe05 + 'A - 1
	sub	al, cl
	int	0x10

	mov	al, #'X
	int	0x10

	mov	al, #':
	int	0x10

no_reg:
	add	bp, #2      ! next register
	call print_hex  ! print it
	pop	cx
	loop print_loop
	ret

print_nl:
	mov ax, #0xe0d  ! CR
	int	0x10
	mov	al, #0xa    ! LF
	int 0x10
	ret

/*
 *	print_hex is for debugging purposes, and prints the word
 *	pointed to by ss:bp in hexadecmial.
*/

print_hex:
	mov	cx, #4      ! 4 hex digits
	mov	dx, (bp)    ! load word into dx
print_digit:
	rol	dx, #4      ! rotate so that lowest 4 bits are used
	mov	ah, #0xe	
	mov	al, dl      ! mask off so we have only next nibble
	and	al, #0xf
	add	al, #'0     ! convert to 0-based digit
	cmp	al, #'9     ! check for overflow
	jbe	good_digit
	add	al, #'A - '0 - 10

good_digit:
	int	0x10
	loop print_digit
	ret


/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	xor al, al
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading"

.org 498
root_flags:
	.word CONFIG_ROOT_RDONLY
syssize:
	.word SYSSIZE
swap_dev:
	.word SWAP_DEV
ram_size:
	.word RAMDISK
vid_mode:
	.word SVGA_MODE
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55
```

#### Setup Routine (linux/arch/i386/boot/setup.S)

```asm
!
!	setup.S		Copyright (C) 1991, 1992 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!
! Move PS/2 aux init code to psaux.c
! (troyer@saifr00.cfsat.Honeywell.COM) 03Oct92
!
! some changes and additional features by Christoph Niemann, March 1993
! (niemann@rubdv15.ETDV.Ruhr-Uni-Bochum.De)
!

! NOTE! These had better be the same as in bootsect.s!
#include <linux/config.h>
#include <linux/segment.h>

#ifndef SVGA_MODE
#define SVGA_MODE ASK_VGA
#endif

INITSEG  = DEF_INITSEG	! we move boot here - out of the way
SYSSEG   = DEF_SYSSEG	! system loaded at 0x10000 (65536).
SETUPSEG = DEF_SETUPSEG	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG ! this is done in bootsect already, but...
	mov	ds,ax       ! %ds = 0x9000

! Get memory size (extended mem, kB)

! SYSTEM - GET EXTENDED MEMORY SIZE (286+)
!
!  AH = 88h
!
! Return:
!  CF clear if successful
!  AX = number of contiguous KB starting at absolute address 100000h
!
!  CF set on error
!  AH = status
!       80h invalid command (PC,PCjr)
!       86h unsupported function (XT,PS30)
!

	mov	ah,#0x88
	int	0x15
	mov	[2],ax      ! Assign memory size to 0x90002

! set the keyboard repeat rate to the max

! KEYBOARD - SET TYPEMATIC RATE AND DELAY
!
!  AH = 03h
!  AL = subfunction
!       00h set default delay and rate (PCjr and some PS/2)
!       01h increase delay before repeat (PCjr)
!       02h decrease repeat rate by factor of 2 (PCjr)
!       03h increase delay and decrease repeat rate (PCjr)
!       04h turn off typematic repeat (PCjr and some PS/2)
!       05h set repeat rate and delay (AT,PS)
!       06h get current typematic rate and delay (newer PS/2s)
!
!  BH = delay value (00h = 250ms to 03h = 1000ms)
!  BL = repeat rate (00h = 30/sec to 0ch = 10/sec [def] to 1Fh = 2/sec)
!
! Return:
!  AH destroyed by many BIOSes
!  BL = repeat rate (above)
!  BH = delay (above)
!

	mov	ax,#0x0305
	xor	bx,bx       ! clear bx; 250ms delay and 30/sec typematic rate
	int	0x16

! check for EGA/VGA and some config parameters

! VIDEO - ALTERNATE FUNCTION SELECT (PS,EGA,VGA,MCGA) - GET EGA INFO
!
!  AH = 12h
!  BL = 10h
!
! Return:
!  BH = video state
!       00h color mode in effect (I/O port 3Dxh)
!       01h mono mode in effect (I/O port 3Bxh)
!
!  BL = installed memory (00h=64k, 01h=128k, 02h=192k, 03h=256k)
!  CH = feature connector bits
!  CL = switch settings
!  AH destroyed (at least by Tseng ET4000 BIOS v8.00n)
!
! Bitfields for feature connector bits:
!  Bits(s)  Description
!  0       FEAT 1 line, state 2
!  1       FEAT 0 line, state 2
!  2       FEAT 1 line, state 1
!  3       FEAT 0 line, state 1
!  4-7     unused (0)
!  
! Bitfields for switch settings:
!  Bits(s)  Description
!  0       switch 1 OFF
!  1       switch 2 OFF
!  2       switch 3 OFF
!  3       switch 4 OFF
!  4-7     unused
!

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx     ! Assign installed memory & video state
	mov	[12],cx     ! Assign feature connector bits and switch settings
	mov	ax,#0x5019
	cmp	bl,#0x10

	je	novga       ! According to Ralph Brown's documents %bl will never
                    !  equal 0x10, so we never jump here

! VIDEO - GET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
!
!  AX = 1A00h
!
! Return:
!  AL = 1Ah if function was supported
!  BL = active display code
!  BH = alternate display code
!
! Values for display combination code:
!  00h   no display
!  01h   monochrome adapter w/ monochrome display
!  02h   CGA w/ color display
!  03h   reserved
!  04h   EGA w/ color display
!  05h   EGA w/ monochrome display
!  06h   PGA w/ color display
!  07h   VGA w/ monochrome analog display
!  08h   VGA w/ color analog display
!  09h   reserved
!  0Ah   MCGA w digital color display
!  0Bh   MCGA w/ monochrome analog display
!  0Ch   MCGA w/ color analog display
!  FFh   unknown display type
!

	mov	ax,#0x1a00  ! Added check for EGA/VGA discrimination
	int	0x10

	mov	bx,ax
	mov	ax,#0x5019
	cmp	bl,#0x1a    ! 1a means VGA, anything else EGA or lower
	jne	novga       ! There is a discrepancy between the API ref
                    !  and the code, so we default to the code

	call	chsvga
novga:	mov	[14],ax ! Assign EGA or VGA type

! VIDEO - GET CURSOR POSITION AND SIZE
!
!  AH = 03h
!  BH = page number
!       0-3 in modes 2&3
!       0-7 in modes 0&1
!       0 in graphics mode
!
! Return:
!  AX = 0000h (Phoenix BIOS)
!  CH = start scan line
!  CL = end scan line
!  DH = row (00h is top)
!  DL = column (00h is left)
!
! Notes: A separate cursor is maintained for each of up to 8 display
! pages. Many ROM BIOSes incorrectly return the default size for a
! color display (start 06h, end 07h) when a monochrome display is
! attached.
!

	mov	ah,#0x03    ! read cursor pos
	xor	bh,bh       ! clear bh; page 0
	int	0x10        ! save it in known place, con_init fetches
	mov	[0],dx      ! it from 0x90000.
	
! Get video-card data:

! VIDEO - GET CURRENT VIDEO MODE
!
!  AH = 0Fh
!
! Return:
!  AH = number of character columns
!  AL = display mode
!  BH = active page
!

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx      ! bh = display page
	mov	[6],ax      ! al = video mode, ah = window width

! Get hd0 data

	xor	ax,ax       ! clear ax
	mov	ds,ax
	lds	si,[4*0x41] ! ds:si = 0000:0104 (src addr)
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080  ! es:di = 9000:0080 (dest addr)
	mov	cx,#0x10
	cld
	rep
	movsb           ! Copies 16 bytes from 0x00104 to
                    !  0x90080 (boot block: hd0 data)

! Get hd1 data

	xor	ax,ax		! clear ax
	mov	ds,ax
	lds	si,[4*0x46] ! ds:si = 0000:0118
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090  ! es:di = 9000:0090
	mov	cx,#0x10
	cld
	rep
	movsb           ! Copies 16 bytes from 0x00118 to
                    !  0x90090 (boot block: hd1 data)

! Check that there IS a hd1 :-)

! DISK - GET DISK TYPE (XT 1986/1/10 or later,XT286,AT,PS)
!
!  AH = 15h
!  DL = drive number (bit 7 set for hard disk)
!  
! Return:
!  CF clear if successful
!  AH = type code
!       00h no such drive
!       01h floppy without change-line support
!       02h floppy (or other removable drive) with change-line support
!       03h hard disk
!
!  AL = 03h hard disk (SpeedStor)
!
!  CX:DX = number of 512-byte sectors
!  CF set on error
!

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	xor	ax,ax       ! clear ax
	cld
	rep
	stosb           ! Clears hd1's data in the boot block
                    !  if there is no hd1

is_disk1:

! check for PS/2 pointing device

! BIOS - GET EQUIPMENT LIST
!
! Return:
!  (E)AX = BIOS equipment list word
!
! Bitfields for BIOS equipment list:
!  Bit(s)  Description
!  0      floppy disk(s) installed (number specified by bits 7-6)
!  1      80x87 coprocessor installed
!  3-2    number of 16k banks of RAM on motherboard (PC only)
!         number of 64k banks of RAM on motherboard (XT only)
!  2      pointing device installed (PS)
!  3      unused (PS)
!  5-4    initial video mode
!         00 EGA, VGA< or PGA
!         01 40x25 color
!         10 80x25 color
!         11 80x25 monochrome
!
!  7-6    number of floppies installed less 1 (if bit 0 set)
!  8      DMA support installed (PCjr, Tandy 1400LT)
!         DMA support *not* installed (Tandy 1000's)
!
!  11-9   number of serial ports installed
!  12     game port installed
!  13     serial printer attached (PCjr)
!         Internal modem installed (PC/Convertible)
!
!  15-14  number of parallel ports installed
!  --- Compaq, Dell, and many other 386/486 machines ---
!  23     page tables set so that Weitek coprocessor addressable in
          real mode
!  24     Weitek math coprocessor present
!  --- Compaq Systempro ---
!  25     internal DMA parallel port available
!  26     IRQ for internal DMA parallel port (if bit 25 set)
!         0 = IRQ5
!         1 = IRQ7
!  28-27  parallel port DMA channel
!         00 DMA channel 0
!         01 DMA channel 0 ???
!         10 reserved
!         11 DMA channel 3
!

	mov	ax,#INITSEG
	mov	ds,ax       ! %ds = 0x9000
	mov	[0x1ff],#0  ! default is no pointing device
	int	0x11        ! int 0x11: equipment determination
	test	al,#0x04    ! check if pointing device installed
	jz	no_psmouse
	mov	[0x1ff],#0xaa   ! device present
no_psmouse:
! now we want to move to protected mode ...

	cli             ! no interrupts allowed !
	mov	al,#0x80    ! disable NMI for the bootup sequence
	out	#0x70,al

! first we move the system to its rightful place

	mov	ax,#0x100   ! start of destination segment
	mov	bx,#0x1000  ! start of source segment
	cld             ! 'direction'=0, movs moves forward
do_move:
	mov	es,ax       ! destination segment %es = 0x0100
	add	ax,#0x100   ! Increment dest segment by 0x100 (next page)
	cmp	ax,#0x9000  ! End of the kernel is at 0x90000
	jz	end_move
	mov	ds,bx       ! source segment %ds = 0x1000
	add	bx,#0x100   ! Increment src segment by 0x100 (next page)
	sub	di,di
	sub	si,si       ! reset index registers
	mov cx,#0x800   ! Copy 4096 bytes
	rep
	movsw           ! Moves 0x9000 - 0x100 pages of the kernel, which
                    !  is equal to 143MB
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG    ! right, forgot this at first. didn't work :-)
	mov	ds,ax           ! %ds = 0x9020
	lidt	idt_48      ! load idt with 0,0
	lgdt	gdt_48      ! load gdt with whatever appropriate

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1        ! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF        ! A20 on
	out	#0x60,al
	call	empty_8042

! make sure any possible coprocessor is properly reset..

	xor	ax,ax
	out	#0xf0,al
	call	delay
	out	#0xf1,al
	call	delay

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11        ! initialization sequence
	out	#0x20,al        ! send it to 8259A-1
	call	delay
	out	#0xA0,al        ! and to 8259A-2
	call	delay
	mov	al,#0x20        ! start of hardware int's (0x20)
	out	#0x21,al
	call	delay
	mov	al,#0x28        ! start of hardware int's 2 (0x28)
	out	#0xA1,al
	call	delay
	mov	al,#0x04        ! 8259-1 is master
	out	#0x21,al
	call	delay
	mov	al,#0x02        ! 8259-2 is slave
	out	#0xA1,al
	call	delay
	mov	al,#0x01        ! 8086 mode for both
	out	#0x21,al
	call	delay
	out	#0xA1,al
	call	delay
	mov	al,#0xFF        ! mask off all interrupts for now
	out	#0xA1,al
	call	delay
	mov	al,#0xFB        ! mask all irq's but irq2 which
	out	#0x21,al        ! is cascaded

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
!
! Note that the short jump isn't strictly needed, althought there are
! reasons why it might be a good idea. It won't hurt in any case.
!
	mov	ax,#0x0001  ! protected mode (PE) bit
	lmsw	ax      ! This is it!
	jmp	flush_instr
flush_instr:
	jmpi	0x1000,KERNEL_CS    ! jmp offset 1000 of segment 0x10 (cs)

! This routine checks that the keyboard command queue is empty
! (after emptying the output buffers)
!
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	call	delay
	in	al,#0x64    ! 8042 status port
	test	al,#1   ! output buffer?
	jz	no_output
	call	delay
	in	al,#0x60    ! read it
	jmp	empty_8042
no_output:
	test	al,#2   ! is input buffer full?
	jnz	empty_8042  ! yes - loop
	ret
!
! Read a key and return the (US-)ascii code in al, scan code in ah
!
getkey:
	xor	ah,ah
	int	0x16
	ret

!
! Read a key with a timeout of 30 seconds. The cmos clock is used to get
! the time.
!
getkt:
	call	gettime
	add	al,#30		! wait 30 seconds
	cmp	al,#60
	jl	lminute
	sub	al,#60
lminute:
	mov	cl,al
again:	mov	ah,#0x01
	int	0x16
	jnz	getkey		! key pressed, so get it
	call	gettime
	cmp	al,cl
	jne	again
	mov	al,#0x20	! timeout, return default char `space'
	ret

!
! Flush the keyboard buffer
!
flush:	mov	ah,#0x01
	int	0x16
	jz	empty
	xor	ah,ah
	int	0x16
	jmp	flush
empty:	ret

!
! Read the cmos clock. Return the seconds in al
!
gettime:
	push	cx
	mov	ah,#0x02
	int	0x1a
	mov	al,dh			! dh contains the seconds
	and	al,#0x0f
	mov	ah,dh
	mov	cl,#0x04
	shr	ah,cl
	aad
	pop	cx
	ret

!
! Delay is needed after doing i/o
!
delay:
	.word	0x00eb			! jmp $+2
	ret

! Routine trying to recognize type of SVGA-board present (if any)
! and if it recognize one gives the choices of resolution it offers.
! If one is found the resolution chosen is given by al,ah (rows,cols).

chsvga:	cld
	push	ds
	push	cs
	mov	ax,[0x01fa]
	pop	ds
	mov	modesave,ax
	mov 	ax,#0xc000
	mov	es,ax
	mov	ax,modesave
	cmp	ax,#NORMAL_VGA
	je	defvga
	cmp	ax,#EXTENDED_VGA
	je	vga50
	cmp	ax,#ASK_VGA
	jne	svga
	lea	si,msg1
	call	prtstr
	call	flush
nokey:	call	getkt
	cmp	al,#0x0d		! enter ?
	je	svga			! yes - svga selection
	cmp	al,#0x20		! space ?
	je	defvga			! no - repeat
	call 	beep
	jmp	nokey
defvga:	mov	ax,#0x5019
	pop	ds
	ret
/* extended vga mode: 80x50 */
vga50:
	mov	ax,#0x1112
	xor	bl,bl
	int	0x10		! use 8x8 font set (50 lines on VGA)
	mov	ax,#0x1200
	mov	bl,#0x20
	int	0x10		! use alternate print screen
	mov	ax,#0x1201
	mov	bl,#0x34
	int	0x10		! turn off cursor emulation
	mov	ah,#0x01
	mov	cx,#0x0607
	int	0x10		! turn on cursor (scan lines 6 to 7)
	pop	ds
	mov	ax,#0x5032	! return 80x50
	ret
/* extended vga mode: 80x28 */
vga28:
	pop	ax		! clean the stack
	mov	ax,#0x1111
	xor	bl,bl
	int	0x10		! use 9x14 fontset (28 lines on VGA)
	mov	ah, #0x01
	mov	cx,#0x0b0c
	int	0x10		! turn on cursor (scan lines 11 to 12)
	pop	ds
	mov	ax,#0x501c	! return 80x28
	ret
/* svga modes */
svga:   cld
        lea     si,id9GXE	! Check for the #9GXE (jyanowit@orixa.mtholyoke.edu,thanks dlm40629@uxa.cso.uiuc.edu)
        mov     di,#0x49	! id string is at c000:049
        mov     cx,#0x11	! length of "Graphics Power By"
        repe
        cmpsb
        jne     of1280
is9GXE:	lea 	si,dsc9GXE	! table of descriptions of video modes for BIOS
	lea	di,mo9GXE	! table of sizes of video modes for my BIOS
	br	selmod		! go ask for video mode
of1280:	cld	
	lea	si,idf1280	! Check for Orchid F1280 (dingbat@diku.dk)
	mov	di,#0x10a	! id string is at c000:010a
	mov	cx,#0x21	! length
	repe
	cmpsb
	jne	nf1280	
isVRAM:	lea	si,dscf1280
	lea	di,mof1280
	br	selmod
nf1280:	lea	si,idVRAM
	mov	di,#0x10a
	mov	cx,#0x0c
	repe
	cmpsb
	je	isVRAM
	cld
	lea 	si,idati		! Check ATI 'clues'
	mov	di,#0x31
	mov 	cx,#0x09
	repe
	cmpsb
	jne	noati
	lea	si,dscati
	lea	di,moati
	br	selmod
noati:	mov	ax,#0x200f		! Check Ahead 'clues'
	mov	dx,#0x3ce
	out	dx,ax
	inc	dx
	in	al,dx
	cmp	al,#0x20
	je	isahed
	cmp	al,#0x21
	jne	noahed
isahed:	lea	si,dscahead
	lea	di,moahead
	br	selmod
noahed:	mov	dx,#0x3c3		! Check Chips & Tech. 'clues'
	in	al,dx
	or	al,#0x10
	out	dx,al
	mov	dx,#0x104		
	in	al,dx
	mov	bl,al
	mov	dx,#0x3c3
	in	al,dx
	and	al,#0xef
	out	dx,al
	cmp	bl,[idcandt]
	jne	nocant
	lea	si,dsccandt
	lea	di,mocandt
	br	selmod
nocant:	mov	dx,#0x3d4		! Check Cirrus 'clues'
	mov	al,#0x0c
	out	dx,al
	inc	dx
	in	al,dx
	mov	bl,al
	xor	al,al
	out	dx,al
	dec	dx
	mov	al,#0x1f
	out	dx,al
	inc	dx
	in	al,dx
	mov	bh,al
	xor	ah,ah
	shl	al,#4
	mov	cx,ax
	mov	al,bh
	shr	al,#4
	add	cx,ax
	shl	cx,#8
	add	cx,#6
	mov	ax,cx
	mov	dx,#0x3c4
	out	dx,ax
	inc	dx
	in	al,dx
	and	al,al
	jnz	nocirr
	mov	al,bh
	out	dx,al
	in	al,dx
	cmp	al,#0x01
	jne	nocirr
	call	rst3d4	
	lea	si,dsccirrus
	lea	di,mocirrus
	br	selmod
rst3d4:	mov	dx,#0x3d4
	mov	al,bl
	xor	ah,ah
	shl	ax,#8
	add	ax,#0x0c
	out	dx,ax
	ret	
nocirr:	call	rst3d4			! Check Everex 'clues'
	mov	ax,#0x7000
	xor	bx,bx
	int	0x10
	cmp	al,#0x70
	jne	noevrx
	shr	dx,#4
	cmp	dx,#0x678
	je	istrid
	cmp	dx,#0x236
	je	istrid
	lea	si,dsceverex
	lea	di,moeverex
	br	selmod
istrid:	lea	cx,ev2tri
	jmp	cx
noevrx:	lea	si,idgenoa		! Check Genoa 'clues'
	xor 	ax,ax
	seg es
	mov	al,[0x37]
	mov	di,ax
	mov	cx,#0x04
	dec	si
	dec	di
l1:	inc	si
	inc	di
	mov	al,(si)
	test	al,al
	jz	l2
	seg es
	cmp	al,(di)
l2:	loope 	l1
	cmp	cx,#0x00
	jne	nogen
	lea	si,dscgenoa
	lea	di,mogenoa
	br	selmod
nogen:	cld
	lea	si,idoakvga
	mov	di,#0x08
	mov	cx,#0x08
	repe
	cmpsb
	jne	nooak
	lea	si,dscoakvga
	lea	di,mooakvga
	br	selmod
nooak:	cld
	lea	si,idparadise		! Check Paradise 'clues'
	mov	di,#0x7d
	mov	cx,#0x04
	repe
	cmpsb
	jne	nopara
	lea	si,dscparadise
	lea	di,moparadise
	br	selmod
nopara:	mov	dx,#0x3c4		! Check Trident 'clues'
	mov	al,#0x0e
	out	dx,al
	inc	dx
	in	al,dx
	xchg	ah,al
	xor	al,al
	out	dx,al
	in	al,dx
	xchg	al,ah
	mov	bl,al		! Strange thing ... in the book this wasn't
	and	bl,#0x02	! necessary but it worked on my card which
	jz	setb2		! is a trident. Without it the screen goes
	and	al,#0xfd	! blurred ...
	jmp	clrb2		!
setb2:	or	al,#0x02	!
clrb2:	out	dx,al
	and	ah,#0x0f
	cmp	ah,#0x02
	jne	notrid
ev2tri:	lea	si,dsctrident
	lea	di,motrident
	jmp	selmod
notrid:	mov	dx,#0x3cd		! Check Tseng 'clues'
	in	al,dx			! Could things be this simple ! :-)
	mov	bl,al
	mov	al,#0x55
	out	dx,al
	in	al,dx
	mov	ah,al
	mov	al,bl
	out	dx,al
	cmp	ah,#0x55
 	jne	notsen
	lea	si,dsctseng
	lea	di,motseng
	jmp	selmod
notsen:	mov	dx,#0x3cc		! Check Video7 'clues'
	in	al,dx
	mov	dx,#0x3b4
	and	al,#0x01
	jz	even7
	mov	dx,#0x3d4
even7:	mov	al,#0x0c
	out	dx,al
	inc	dx
	in	al,dx
	mov	bl,al
	mov	al,#0x55
	out	dx,al
	in	al,dx
	dec	dx
	mov	al,#0x1f
	out	dx,al
	inc	dx
	in	al,dx
	mov	bh,al
	dec	dx
	mov	al,#0x0c
	out	dx,al
	inc	dx
	mov	al,bl
	out	dx,al
	mov	al,#0x55
	xor	al,#0xea
	cmp	al,bh
	jne	novid7
	lea	si,dscvideo7
	lea	di,movideo7
	jmp	selmod
novid7:	lea	si,dsunknown
	lea	di,mounknown
selmod:	xor	cx,cx
	mov	cl,(di)
	mov	ax,modesave
	cmp	ax,#ASK_VGA
	je	askmod
	cmp	ax,#NORMAL_VGA
	je	askmod
	cmp	al,cl
	jl	gotmode
	push	si
	lea	si,msg4
	call	prtstr
	pop	si
askmod:	push	si
	lea	si,msg2
	call	prtstr
	pop	si
	push	si
	push	cx
tbl:	pop	bx
	push	bx
	mov	al,bl
	sub	al,cl
	call	modepr
	lodsw
	xchg	al,ah
	call	dprnt
	xchg	ah,al
	push	ax
	mov	al,#0x78
	call	prnt1
	pop	ax
	call	dprnt
	push	si
	lea	si,crlf		! print CR+LF
	call	prtstr
	pop	si
	loop	tbl
	pop	cx
	lea	si,msg3
	call	prtstr
	pop	si
	add	cl,#0x30
	jmp	nonum
nonumb:	call	beep
nonum:	call	getkey
	cmp	al,#0x30	! ascii `0'
	jb	nonumb
	cmp	al,#0x3a	! ascii `9'
	jbe	number
	cmp	al,#0x61	! ascii `a'
	jb	nonumb
	cmp	al,#0x7a	! ascii `z'
	ja	nonumb
	sub	al,#0x27
	cmp	al,cl
	jae	nonumb
	sub	al,#0x30
	jmp	gotmode
number: cmp	al,cl
	jae	nonumb
	sub	al,#0x30
gotmode:	xor	ah,ah
	or	al,al
	beq	vga50
	push	ax
	dec	ax
	beq	vga28
	add	di,ax
	mov	al,(di)
	int 	0x10
	pop	ax
	shl	ax,#1
	add	si,ax
	lodsw
	pop	ds
	ret

! Routine to print asciiz-string at DS:SI

prtstr:	lodsb
	and	al,al
	jz	fin
	call	prnt1
	jmp	prtstr
fin:	ret

! Routine to print a decimal value on screen, the value to be
! printed is put in al (i.e 0-255). 

dprnt:	push	ax
	push	cx
	xor	ah,ah		! Clear ah
	mov	cl,#0x0a
	idiv	cl
	cmp	al,#0x09
	jbe	lt100
	call	dprnt
	jmp	skip10
lt100:	add	al,#0x30
	call	prnt1
skip10:	mov	al,ah
	add	al,#0x30
	call	prnt1	
	pop	cx
	pop	ax
	ret

!
! Routine to print the mode number key on screen. Mode numbers
! 0-9 print the ascii values `0' to '9', 10-35 are represented by
! the letters `a' to `z'. This routine prints some spaces around the
! mode no.
!

modepr:	push	ax
	cmp	al,#0x0a
	jb	digit		! Here is no check for number > 35
	add	al,#0x27
digit:	add	al,#0x30
	mov	modenr, al
	push 	si
	lea	si, modestring
	call	prtstr
	pop	si
	pop	ax
	ret

! Part of above routine, this one just prints ascii al

prnt1:	push	ax
	push	cx
	xor	bh,bh
	mov	cx,#0x01
	mov	ah,#0x0e
	int	0x10
	pop	cx
	pop	ax
	ret

beep:	mov	al,#0x07
	jmp	prnt1
	
gdt:
	.word	0,0,0,0		! dummy

	.word	0,0,0,0		! unused

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx

msg1:		.ascii	"Press <RETURN> to see SVGA-modes available, <SPACE> to continue or wait 30 secs."
		db	0x0d, 0x0a, 0x0a, 0x00
msg2:		.ascii	"Mode:  COLSxROWS:"
		db	0x0d, 0x0a, 0x0a, 0x00
msg3:		db	0x0d, 0x0a
		.ascii	"Choose mode by pressing the corresponding number or letter."
crlf:		db	0x0d, 0x0a, 0x00
msg4:		.ascii	"You passed an undefined mode number to setup. Please choose a new mode."
		db	0x0d, 0x0a, 0x0a, 0x07, 0x00
modestring:	.ascii	"   "
modenr:		db	0x00	! mode number
		.ascii	":    "
		db	0x00
		
idati:		.ascii	"761295520"
idcandt:	.byte	0xa5
idgenoa:	.byte	0x77, 0x00, 0x99, 0x66
idparadise:	.ascii	"VGA="
idoakvga:	.ascii  "OAK VGA "
idf1280:	.ascii	"Orchid Technology Fahrenheit 1280"
id9GXE:		.ascii  "Graphics Power By"
idVRAM:		.ascii	"Stealth VRAM"

! Manufacturer:	  Numofmodes+2:	Mode:
! Number of modes is the number of chip-specific svga modes plus the extended
! modes available on any vga (currently 2)

moati:		.byte	0x04,	0x23, 0x33 
moahead:	.byte	0x07,	0x22, 0x23, 0x24, 0x2f, 0x34
mocandt:	.byte	0x04,	0x60, 0x61
mocirrus:	.byte	0x06,	0x1f, 0x20, 0x22, 0x31
moeverex:	.byte	0x0c,	0x03, 0x04, 0x07, 0x08, 0x0a, 0x0b, 0x16, 0x18, 0x21, 0x40
mogenoa:	.byte	0x0c,	0x58, 0x5a, 0x60, 0x61, 0x62, 0x63, 0x64, 0x72, 0x74, 0x78
moparadise:	.byte	0x04,	0x55, 0x54
motrident:	.byte	0x09,	0x50, 0x51, 0x52, 0x57, 0x58, 0x59, 0x5a
motseng:	.byte	0x07,	0x26, 0x2a, 0x23, 0x24, 0x22
movideo7:	.byte	0x08,	0x40, 0x43, 0x44, 0x41, 0x42, 0x45
mooakvga:	.byte   0x08,   0x00, 0x07, 0x4e, 0x4f, 0x50, 0x51
mo9GXE:		.byte	0x04,	0x54, 0x55
mof1280:	.byte	0x04,	0x54, 0x55
mounknown:	.byte	0x02

!			msb = Cols lsb = Rows:
! The first two modes are standard vga modes available on any vga.
! mode 0 is 80x50 and mode 1 is 80x28

dscati:		.word	0x5032, 0x501c, 0x8419, 0x842c
dscahead:	.word	0x5032, 0x501c, 0x842c, 0x8419, 0x841c, 0xa032, 0x5042
dsccandt:	.word	0x5032, 0x501c, 0x8419, 0x8432
dsccirrus:	.word	0x5032, 0x501c, 0x8419, 0x842c, 0x841e, 0x6425
dsceverex:	.word	0x5032, 0x501c, 0x5022, 0x503c, 0x642b, 0x644b, 0x8419, 0x842c, 0x501e, 0x641b, 0xa040, 0x841e
dscgenoa:	.word	0x5032, 0x501c, 0x5020, 0x642a, 0x8419, 0x841d, 0x8420, 0x842c, 0x843c, 0x503c, 0x5042, 0x644b
dscparadise:	.word	0x5032, 0x501c, 0x8419, 0x842b
dsctrident:	.word 	0x5032, 0x501c, 0x501e, 0x502b, 0x503c, 0x8419, 0x841e, 0x842b, 0x843c
dsctseng:	.word	0x5032, 0x501c, 0x503c, 0x6428, 0x8419, 0x841c, 0x842c
dscvideo7:	.word	0x5032, 0x501c, 0x502b, 0x503c, 0x643c, 0x8419, 0x842c, 0x841c
dscoakvga:	.word   0x5032, 0x501c, 0x2819, 0x5019, 0x503c, 0x843c, 0x8419, 0x842b
dscf1280:	.word	0x5032, 0x501c, 0x842b, 0x8419
dsc9GXE:	.word	0x5032, 0x501c, 0x842b, 0x8419
dsunknown:	.word	0x5032, 0x501c
modesave:	.word	SVGA_MODE

	
.text
endtext:
.data
enddata:
.bss
endbss:
```

#### 32-bit Startup Code (linux/boot/head.S)

```asm
/*
 *  linux/boot/head.S
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/*
 *  head.S contains the 32-bit startup code.
 */

.text
.globl _idt,_gdt,
.globl _swapper_pg_dir,_pg0
.globl _empty_bad_page
.globl _empty_bad_page_table
.globl _empty_zero_page
.globl _tmp_floppy_area,_floppy_track_buffer

#include <linux/tasks.h>
#include <linux/segment.h>

#define CL_MAGIC_ADDR	0x90020
#define CL_MAGIC	0xA33F
#define CL_BASE_ADDR	0x90000
#define CL_OFFSET	0x90022

/*
 * swapper_pg_dir is the main page directory, address 0x00001000 (or at
 * address 0x00101000 for a compressed boot).
 */
startup_32:
	cld
	movl $(KERNEL_DS),%eax    # Set data segments to 0x18
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp     # _stack_start = %esp
/*
 * Clear BSS first so that there are no surprises...
 */
	xorl %eax,%eax
	movl $__edata,%edi
	movl $__end,%ecx
	subl %edi,%ecx
	cld
	rep
	stosb
/*
 * start system 32-bit setup. We need to re-do some of the things done
 * in 16-bit mode for the "real" operations.
 */
	call setup_idt
	xorl %eax,%eax
1:	incl %eax           # check that A20 really IS enabled
	movl %eax,0x000000  # loop forever if it isn't
	cmpl %eax,0x100000
	je 1b
/*
 * Initialize eflags.  Some BIOS's leave bits like NT set.  This would
 * confuse the debugger if this code is traced.
 * XXX - best to initialize before switching to protected mode.
 */
	pushl $0
	popfl
/*
 * Copy bootup parameters out of the way. First 2kB of
 * _empty_zero_page is for boot parameters, second 2kB
 * is for the command line.
 */
	movl $0x90000,%esi             # %esi = 0x90000
	movl $_empty_zero_page,%edi    # %edi = 0x05000
	movl $512,%ecx
	cld
	rep
	movsl            # Writes 2kB of zeroes at 0x05000
	xorl %eax,%eax
	movl $512,%ecx
	rep
	stosl
	cmpw $(CL_MAGIC),CL_MAGIC_ADDR    # Is 0xA33F stored at 0x90020?
	jne 1f
	movl $_empty_zero_page+2048,%edi  # %edi = 0x05800
	movzwl CL_OFFSET,%esi             # %esi = 0x90022
	addl $(CL_BASE_ADDR),%esi         # %esi = cmd line address
	movl $2048,%ecx
	rep
	movsb            # Copies command line to 0x05800

1:
/* check if it is 486 or 386. */
/*
 * XXX - this does a lot of unnecessary setup.  Alignment checks don't
 * apply at our cpl of 0 and the stack ought to be aligned already, and
 * we don't need to preserve eflags.
 */
	movl %esp,%edi		# save stack pointer
	andl $0xfffffffc,%esp	# align stack to avoid AC fault
	movl $3,_x86
	pushfl			# push EFLAGS
	popl %eax		# get EFLAGS
	movl %eax,%ecx		# save original EFLAGS
	xorl $0x40000,%eax	# flip AC bit in EFLAGS
	pushl %eax		# copy to EFLAGS
	popfl			# set EFLAGS
	pushfl			# get new EFLAGS
	popl %eax		# put it in eax
	xorl %ecx,%eax		# change in flags
	andl $0x40000,%eax	# check if AC bit changed
	je is386
	movl $4,_x86
	movl %ecx,%eax
	xorl $0x200000,%eax	# check ID flag
	pushl %eax
	popfl			# if we are on a straight 486DX, SX, or
	pushfl			# 487SX we can't change it
	popl %eax
	xorl %ecx,%eax
	andl $0x200000,%eax
	je is486
isnew:	pushl %ecx		# restore original EFLAGS
	popfl
	movl $1, %eax		# Use the CPUID instruction to 
	.byte 0x0f, 0xa2	# check the processor type
	andl $0xf00, %eax	# Set _x86 with the family
	shrl $8, %eax		# returned.	
	movl %eax, _x86
	movl %edi,%esp		# restore esp
	movl %cr0,%eax		# 486+
	andl $0x80000011,%eax	# Save PG,PE,ET
	orl $0x50022,%eax	# set AM, WP, NE and MP
	jmp 2f
is486:	pushl %ecx		# restore original EFLAGS
	popfl
	movl %edi,%esp		# restore esp
	movl %cr0,%eax		# 486
	andl $0x80000011,%eax	# Save PG,PE,ET
	orl $0x50022,%eax	# set AM, WP, NE and MP
	jmp 2f
is386:	pushl %ecx		# restore original EFLAGS
	popfl
	movl %edi,%esp		# restore esp
	movl %cr0,%eax		# 386
	andl $0x80000011,%eax	# Save PG,PE,ET
	orl $2,%eax		# set MP
2:	movl %eax,%cr0
	call check_x87
	call setup_paging
	lgdt gdt_descr
	lidt idt_descr
	ljmp $(KERNEL_CS),$1f
1:	movl $(KERNEL_DS),%eax  # reload all the segment registers
	mov %ax,%ds             # after changing gdt.
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss _stack_start,%esp   # update _stack_start
	xorl %eax,%eax
	lldt %ax                # load proc0's LDT
	pushl %eax              # These are the parameters to main :-)
	pushl %eax              #  NOTE: comment above is outdated, start_kernel
	pushl %eax              #        has no parameters
	cld         # gcc2 wants the direction flag cleared at all times
	call _start_kernel
L6:
	jmp L6          # main should never return here, but
                    # just in case, we know what happens.

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	movl $0,_hard_math
	clts
	fninit
	fstsw %ax
	cmpb $0,%al
	je 1f
	movl %cr0,%eax		/* no coprocessor: have to set bits */
	xorl $4,%eax		/* set EM */
	movl %eax,%cr0
	ret
.align 2
1:	movl $1,_hard_math
	.byte 0xDB,0xE4		/* fsetpm for 287, ignored by 387 */
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It doesn't actually load
 *  idt - that can be done only after paging has been enabled
 *  and the kernel moved to 0xC0000000. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok.
 */
setup_idt:
	lea ignore_int,%edx
	movl $(KERNEL_CS << 16),%eax
	movw %dx,%ax		/* selector = 0x0010 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea _idt,%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	ret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 4MB.  The rest are initialized later.
 *
 * (ref: added support for up to 32mb, 17Apr92)  -- Rik Faith
 * (ref: update, 25Sept92)  -- croutons@crunchy.uucp 
 * (ref: 92.10.11 - Linus Torvalds. Corrected 16M limit - no upper memory limit)
 */
.align 2
setup_paging:
	movl $1024*2,%ecx       /* 2 pages - swapper_pg_dir+1 page table */
	xorl %eax,%eax
	movl $_swapper_pg_dir,%edi  /* swapper_pg_dir is at 0x1000 */
	cld;rep;stosl               /* bzero swapper_pg_dir */
/* Identity-map the kernel in low 4MB memory for ease of transition */
	movl $_pg0+7,_swapper_pg_dir        /* set present bit/user r/w */
                                        /* set first pde entry to point to 0x2000 */

/* But the real place is at 0xC0000000 */
	movl $_pg0+7,_swapper_pg_dir+3072   /* set present bit/user r/w */
                                        /* set KERNEL_OFFSET entry to point to 0x2000 */
	movl $_pg0+4092,%edi         /* %edi = end of pg table = 0x2FFC */
	movl $0x03ff007,%eax         /*  4Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl           /* fill the page backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	cld
	movl $_swapper_pg_dir,%eax
	movl %eax,%cr3          /* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0      /* set paging (PG) bit */
	ret         /* this also flushes the prefetch-queue */

/*
 * page 0 is made non-existent, so that kernel NULL pointer references get
 * caught. Thus the swapper page directory has been moved to 0x1000
 *
 * XXX Actually, the swapper page directory is at 0x1000 plus 1 megabyte,
 * with the introduction of the compressed boot code.  Theoretically,
 * the original design of overlaying the startup code with the swapper
 * page directory is still possible --- it would reduce the size of the kernel
 * by 2-3k.  This would be a good thing to do at some point.....
 */
.org 0x1000
_swapper_pg_dir:
/*
 * The page tables are initialized to only 4MB here - the final page
 * tables are set up later depending on memory size.
 */
.org 0x2000
_pg0:

.org 0x3000
_empty_bad_page:

.org 0x4000
_empty_bad_page_table:

.org 0x5000
_empty_zero_page:

.org 0x6000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0
/*
 * floppy_track_buffer is used to buffer one track of floppy data: it
 * has to be separate from the tmp_floppy area, as otherwise a single-
 * sector read/write can mess it up. It can contain one full track of
 * data (18*2*512 bytes).
 */
_floppy_track_buffer:
	.fill 512*2*18,1,0

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n"
.align 2
ignore_int:
	cld
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $(KERNEL_DS),%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret

/*
 * The interrupt descriptor table has room for 256 idt's
 */
.align 4
.word 0
idt_descr:
	.word 256*8-1       # idt contains 256 entries
	.long 0xc0000000+_idt

.align 4
_idt:
	.fill 256,8,0       # idt is uninitialized

.align 4
.word 0
gdt_descr:
	.word (8+2*NR_TASKS)*8-1    # 128 LDTs + 8 kernel descriptors
	.long 0xc0000000+_gdt

/*
 * This gdt setup gives the kernel a 1GB address space at virtual
 * address 0xC0000000 - space enough for expansion, I hope.
 */
.align 4
_gdt:
	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x0000000000000000	/* not used */
	.quad 0xc0c39a000000ffff	/* 0x10 kernel 1GB code at 0xC0000000 */
	.quad 0xc0c392000000ffff	/* 0x18 kernel 1GB data at 0xC0000000 */
	.quad 0x00cbfa000000ffff	/* 0x23 user   3GB code at 0x00000000 */
	.quad 0x00cbf2000000ffff	/* 0x2b user   3GB data at 0x00000000 */
	.quad 0x0000000000000000	/* not used */
	.quad 0x0000000000000000	/* not used */
	.fill 2*NR_TASKS,8,0		/* space for LDT's and TSS's etc */
```
