	.file "frexp.S"

#ifdef __ELF__
#define _frexp	frexp
#define _ldexp	ldexp
#ifdef __i486__
#define ALIGN_TEXT	16,0x90
#else
#define ALIGN_TEXT	4,0x90
#endif
#define ALIGN_DATA	4
#else
#ifdef __i486__
#define ALIGN_TEXT	4,0x90
#else
#define ALIGN_TEXT	2,0x90
#endif
#define ALIGN_DATA	2
#endif

#ifdef __ELF__
	.section	.rodata
#else
	.text
#endif

	.align ALIGN_TEXT

#ifdef __ELF__
.LC0:
#else
LC0:
#endif
	.double	1.7976931348623157e+308

	.align ALIGN_TEXT

#ifdef __ELF__
.LC1:
#else
LC1:
#endif
	.long	0x0,0x40000000

	.text
	.globl _frexp

	.align ALIGN_TEXT
_frexp:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$16,%esp
	pushl	%esi
	pushl	%ebx
/* y = x; */
	fldl	8(%ebp)
	movl	16(%ebp),%ebx
	fstl	-8(%ebp)
/* q = (short *)&y + 3; */
	leal	-2(%ebp),%ecx
/* i  = ( *q >> 4) & 0x7ff; */
	movw	-2(%ebp),%ax
	sarw	$4,%ax
	movl	%eax,%edx
	andl	$2047,%edx
/* if( i != 0 )       */
/*	goto ieeedon; */
	jne	L22
/* if( y == 0.0 ) */
	fldz
	fucompp
	fnstsw	%ax
	andb	$68,%ah
	xorb	$64,%ah
	jne	L18
/* *pw2 = 0; */
	movl	$0,(%ebx)
/* return( 0.0 ); */
	fldz
	jmp	L21
	.align	ALIGN_DATA,0x90
/* do { */
L18:
/* y *= 2.0; */
	fldl	-8(%ebp)
	fadd	%st(0),%st
	fstpl	-8(%ebp)
/* i -= 1; */
	decl	%edx
/* k  = ( *q >> 4) & 0x7ff; */
	movw	(%ecx),%ax
	sarw	$4,%ax
	movl	%eax,%esi
	andl	$2047,%esi
/* while( k == 0 ); */
	je	L18
/* i = i + k; */
	addl	%esi,%edx
	jmp	L16
	.align	ALIGN_DATA,0x90
/* ieeedon: */
L22:
	fstp	%st(0)
L16:
/* i -= 0x3fe; */
	addl	$-1022,%edx
/* *pw2 = i; */
	movl	%edx,(%ebx)
/* *q &= 0x800f; */
	andw	$32783,(%ecx)
/* *q |= 0x3fe0; */
	orw	$16352,(%ecx)
/* return( y ); */
	fldl	-8(%ebp)
L21:
	leal	-24(%ebp),%esp
	popl	%ebx
	popl	%esi
	leave
	ret

	.globl	_ldexp
	.align ALIGN_TEXT
_ldexp:
	pushl	%ebp
	movl	%esp,%ebp
	subl	$20,%esp
	pushl	%esi
	pushl	%ebx
/* power of 2 */
	movl	16(%ebp),%esi
/* y = x */
	movl	8(%ebp),%eax
	movl	12(%ebp),%edx
	movl	%eax,-8(%ebp)
	movl	%edx,-4(%ebp)
/* q = (short *)&y + 3; */
	leal	-2(%ebp),%ecx
/* while( (e = (*q & 0x7ff0) >> 4) == 0 ) { */
	movw	-2(%ebp),%dx
	andw	$32752,%dx
	sarw	$4,%dx
	movswl	%dx,%ebx
	testl	%ebx,%ebx
	jne	L25
	fldz
#ifdef __PIC__
	pushl	%ebx
	call	.L2
.L2:
	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-.L2],%ebx
	fldl	.LC1@GOTOFF(%ebx)
	popl	%ebx
#else
#ifdef __ELF__
	fldl	.LC1
#else
	fldl	LC1
#endif
#endif
	.align	ALIGN_DATA,0x90
L31:
/* if( y == 0.0 )  return( 0.0 ); */
	fldl	-8(%ebp)
	fucom	%st(2)
	fnstsw	%ax
	andb	$69,%ah
	cmpb	$64,%ah
	je	L45
/* if( pw2 > 0 ) { */
	testl	%esi,%esi
	jle	L46
/* y *= 2.0; */
	fadd	%st(0),%st
	fstpl	-8(%ebp)
/* pw2 -= 1; */
	decl	%esi
	jmp	L27
	.align	ALIGN_DATA,0x90
L46:
	fstp	%st(0)
L27:
/* if( pw2 < 0 ) { */
	testl	%esi,%esi
	jge	L28
/* if( pw2 < -53 )  return(0.0); */
	cmpl	$-53,%esi
	jl	L47
/* y /= 2.0; */
	fld	%st(0)
	fdivrl	-8(%ebp)
	fstpl	-8(%ebp)
/* pw2 += 1; */
	incl	%esi
L28:
/* if( pw2 == 0 ) return(y); */
	testl	%esi,%esi
	je	L48
/* } while exponent is zero */
	movw	(%ecx),%ax
	andw	$32752,%ax
	sarw	$4,%ax
	movswl	%ax,%ebx
	testl	%ebx,%ebx
	je	L31
	fstp	%st(0)
	fstp	%st(0)
L25:
/* e += pw2; */
	addl	%esi,%ebx
/* if( e > MEXP )  return( MAXNUM ); */
	cmpl	$2047,%ebx
	jle	L32
#ifdef __PIC__
	pushl	%ebx
	call	.L3
.L3:
	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-.L3],%ebx
	fldl	.LC0@GOTOFF(%ebx)
	popl	%ebx
#else
#ifdef __ELF__
	fldl	.LC0
#else
	fldl	LC0
#endif
#endif
	jmp	L39
	.align	ALIGN_DATA,0x90
L32:
/* if( e < 1 ) */
	testl	%ebx,%ebx
	jg	L33
/* if( e < -53 ) return(0.0); */
	cmpl	$-53,%ebx
	jge	L34
	jmp	L44
	.align	ALIGN_DATA,0x90
L45:
	fstp	%st(0)
L47:
	fstp	%st(0)
	fstp	%st(0)
L44:
	fldz
	jmp	L39
	.align	ALIGN_DATA,0x90
L34:
/* *q &= 0x800f; */
	andw	$32783,(%ecx)
/* *q |= 0x10; */
	orb	$16,(%ecx)
/* while( e < 1 ) */
	testl	%ebx,%ebx
	jg	L43
#ifdef __PIC__
	pushl	%ebx
	call	.L4
.L4:
	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-.L4],%ebx
	fldl	.LC1@GOTOFF(%ebx)
	popl	%ebx
#else
#ifdef __ELF__
	fldl	.LC1
#else
	fldl	LC1
#endif
#endif
	.align	ALIGN_DATA,0x90
L37:
/* y /= 2.0; */
	fld	%st(0)
	fdivrl	-8(%ebp)
	fstpl	-8(%ebp)
/* e += 1; */
	incl	%ebx
	testl	%ebx,%ebx
	jle	L37
/* return(y); */
	jmp	L49
	.align	ALIGN_DATA,0x90
L33:
/* *q &= 0x800f; */
	andw	$32783,(%ecx)
/* *q |= (e & 0x7ff) << 4; */
	movl	%ebx,%eax
	andb	$7,%ah
	salw	$4,%ax
	orw	%ax,(%ecx)
	jmp	L43
	.align	ALIGN_DATA,0x90
L48:
	fstp	%st(0)
L49:
	fstp	%st(0)
L43:
/* return(y); */
	fldl	-8(%ebp)
L39:
	leal	-28(%ebp),%esp
	popl	%ebx
	popl	%esi
	leave
	ret

#ifdef __ELF__
	.type	frexp,@function
	.type	ldexp,@function
#endif
