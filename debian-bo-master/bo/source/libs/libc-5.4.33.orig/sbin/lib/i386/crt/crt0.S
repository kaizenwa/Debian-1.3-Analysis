	.text
__entry:
	movl 8(%esp),%eax
	movl %eax,___environ
	call _main
/*
 *	We use int $0x80 for __exit().
 */
	movl %eax,%ebx
done:
	movl $1,%eax
	int $0x80
	jmp done
