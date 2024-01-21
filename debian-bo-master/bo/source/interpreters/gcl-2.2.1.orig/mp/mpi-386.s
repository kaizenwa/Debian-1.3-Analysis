	.file	"mpi.c"
gcc_compiled.:
.text
	.align 4
.globl mulsi
mulsi:
	pushl %ebp
	movl %esp,%ebp
	subl $28,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%eax
	movl %eax,-4(%ebp)
	movl 12(%ebp),%ebx
	movl 4(%ebx),%eax
	sarl $24,%eax
	movl %eax,-8(%ebp)
	movl 4(%ebx),%eax
	andl $65535,%eax
	movl %eax,-12(%ebp)
	cmpl $0,-4(%ebp)
	je .L3
	cmpl $0,-8(%ebp)
	jne .L2
.L3:
	movl gzero,%eax
	jmp .L1
.L2:
	cmpl $0,-4(%ebp)
	jge .L4
	negl -8(%ebp)
	negl -4(%ebp)
	jns .L4
	pushl %ebx
	pushl $-2147483648
	call stoi
	leal 4(%esp),%esp
	pushl %eax
	call mulii
	jmp .L1
.L4:
	movl -12(%ebp),%eax
	incl %eax
	pushl %eax
	call cgeti
	movl %eax,%edi
	xorl %ecx,%ecx
	movl -12(%ebp),%eax
	leal (,%eax,4),%eax
	movl %eax,-20(%ebp)
	leal (%eax,%ebx),%edx
	movl %edx,-16(%ebp)
	leal 4(%eax,%edi),%ebx
	movl -12(%ebp),%eax
	decl %eax
	movl %eax,-28(%ebp)
	jmp .L6
.L8:
	leal -4(%ebx),%ebx
	movl %ecx,-20(%ebp)
	movl -16(%ebp),%eax
	leal -4(%eax),%edx
	movl %edx,-16(%ebp)
	movl %edx,%esi
	movl -4(%ebp),%eax
/APP
	mull (%esi)
/NO_APP
	movl %edx,%ecx
	movl %eax,-24(%ebp)
	movl -20(%ebp),%eax
/APP
	addl -24(%ebp),%eax
	adcl $0,%ecx
/NO_APP
	movl %eax,(%ebx)
.L6:
	decl -28(%ebp)
	jne .L8
	testl %ecx,%ecx
	je .L9
	movl %ecx,-4(%ebx)
	movl 4(%edi),%eax
	andl $-65536,%eax
	movl -12(%ebp),%edx
	leal 1(%edx,%eax),%edx
	movl %edx,4(%edi)
	jmp .L10
.L9:
	addl $4,avma
	movl (%edi),%eax
	decl %eax
	movl %eax,4(%edi)
	leal 4(%edi),%edi
	movl 4(%edi),%eax
	andl $-65536,%eax
	addl -12(%ebp),%eax
	movl %eax,4(%edi)
.L10:
	movl 4(%edi),%eax
	andl $16777215,%eax
	movl -8(%ebp),%edx
	sall $24,%edx
	addl %edx,%eax
	movl %eax,4(%edi)
	movl %edi,%eax
.L1:
	leal -40(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl expi
expi:
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	movl 8(%ebp),%eax
	movl 4(%eax),%ebx
	andl $65535,%ebx
	cmpl $2,%ebx
	jne .L12
	movl $-8388608,%eax
	jmp .L13
.L12:
	pushl 8(%eax)
	call bfffo
	leal -2(%ebx),%edx
	sall $5,%edx
	subl %eax,%edx
	movl %edx,%eax
	decl %eax
.L13:
	leal -4(%ebp),%esp
	popl %ebx
	leave
	ret
	.align 4
.globl addsi
addsi:
	pushl %ebp
	movl %esp,%ebp
	subl $12,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	movl 12(%ebp),%edi
	testl %esi,%esi
	jne .L15
	pushl %edi
	call icopy
	jmp .L14
.L15:
	movl 4(%edi),%ebx
	sarl $24,%ebx
	movl %ebx,-8(%ebp)
	jne .L16
	pushl %esi
	call stoi
	jmp .L14
.L16:
	testl %esi,%esi
	jge .L17
	movl $-1,-4(%ebp)
	negl %esi
	jns .L19
	pushl %edi
	pushl $MOST_NEGS
	call addii
	jmp .L14
.L17:
	movl $1,-4(%ebp)
.L19:
	movl 4(%edi),%ebx
	andl $65535,%ebx
	movl %ebx,-12(%ebp)
	movl -8(%ebp),%ebx
	cmpl -4(%ebp),%ebx
	jne .L20
	movl %esi,%edx
	movl -12(%ebp),%ebx
	movl %edx,%ecx
	addl -4(%edi,%ebx,4),%ecx
	cmpl %ecx,%edx
	jbe .L21
	movl $1,%eax
	jmp .L22
.L21:
	xorl %eax,%eax
.L22:
	movl %ecx,%esi
	testl %eax,%eax
	je .L23
	movl -12(%ebp),%eax
	incl %eax
	pushl %eax
	call cgeti
	movl %eax,%ecx
	movl -12(%ebp),%ebx
	movl %esi,(%ecx,%ebx,4)
	movl %ebx,%eax
	jmp .L65
.L27:
	cmpl $-1,-4(%edi,%eax,4)
	jne .L25
	movl $0,(%ecx,%eax,4)
.L65:
	decl %eax
	cmpl $2,%eax
	jg .L27
.L25:
	cmpl $2,%eax
	jle .L28
	movl -4(%edi,%eax,4),%ebx
	incl %ebx
	jmp .L66
.L31:
	movl -4(%edi,%eax,4),%ebx
.L66:
	movl %ebx,(%ecx,%eax,4)
	decl %eax
	cmpl $2,%eax
	jg .L31
	movl (%ecx),%eax
	decl %eax
	movl %eax,4(%ecx)
	movl %eax,8(%ecx)
	leal 4(%ecx),%ecx
	addl $4,avma
	jmp .L33
.L28:
	movl $1,8(%ecx)
	movl (%ecx),%ebx
	movl %ebx,4(%ecx)
	jmp .L33
.L23:
	pushl -12(%ebp)
	call cgeti
	movl %eax,%ecx
	movl -12(%ebp),%ebx
	movl %esi,-4(%ecx,%ebx,4)
	movl $1,%eax
	movl %ebx,%edx
	decl %edx
	jmp .L34
.L37:
	movl (%edi,%eax,4),%ebx
	movl %ebx,(%ecx,%eax,4)
	incl %eax
.L34:
	cmpl %eax,%edx
	jg .L37
.L33:
	movl 4(%ecx),%eax
	andl $16777215,%eax
	movl -4(%ebp),%edx
	jmp .L67
.L20:
	cmpl $3,-12(%ebp)
	jne .L39
	cmpl 8(%edi),%esi
	jae .L40
	pushl $3
	call cgeti
	movl %eax,%ecx
	movl -8(%ebp),%eax
	sall $24,%eax
	addl $3,%eax
	movl %eax,4(%ecx)
	movl 8(%edi),%ebx
	subl %esi,%ebx
	movl %ebx,8(%ecx)
	jmp .L38
.L40:
	cmpl 8(%edi),%esi
	jne .L41
	movl gzero,%eax
	jmp .L14
.L41:
	pushl $3
	call cgeti
	movl %eax,%ecx
	movl -8(%ebp),%eax
	negl %eax
	sall $24,%eax
	addl $3,%eax
	movl %eax,4(%ecx)
	subl 8(%edi),%esi
	movl %esi,8(%ecx)
	jmp .L38
.L39:
	movl -12(%ebp),%ebx
	movl -4(%edi,%ebx,4),%edx
	movl %esi,%eax
	movl %edx,%ecx
	subl %eax,%ecx
	cmpl %edx,%eax
	ja .L42
	xorl %eax,%eax
	jmp .L43
.L42:
	movl $1,%eax
.L43:
	movl %ecx,%esi
	testl %eax,%eax
	je .L44
	pushl -12(%ebp)
	call cgeti
	movl %eax,%ecx
	movl -12(%ebp),%ebx
	movl %esi,-4(%ecx,%ebx,4)
	leal -2(%ebx),%eax
	cmpl $0,(%edi,%eax,4)
	jne .L62
.L48:
	movl $-1,(%ecx,%eax,4)
	decl %eax
	cmpl $0,(%edi,%eax,4)
	je .L48
.L62:
	movl (%edi,%eax,4),%edx
	decl %edx
	movl %edx,(%ecx,%eax,4)
	cmpl $2,%eax
	jg .L50
	testl %edx,%edx
	je .L49
.L50:
	decl %eax
	testl %eax,%eax
	jle .L38
.L54:
	movl (%edi,%eax,4),%ebx
	movl %ebx,(%ecx,%eax,4)
	decl %eax
	testl %eax,%eax
	jg .L54
	jmp .L38
.L49:
	movl (%ecx),%eax
	decl %eax
	movl %eax,4(%ecx)
	movl %eax,8(%ecx)
	leal 4(%ecx),%ecx
	addl $4,avma
	movl 4(%ecx),%eax
	andl $16777215,%eax
	movl -8(%ebp),%edx
.L67:
	sall $24,%edx
	addl %edx,%eax
	movl %eax,4(%ecx)
	jmp .L38
.L44:
	pushl -12(%ebp)
	call cgeti
	movl %eax,%ecx
	movl -12(%ebp),%ebx
	movl %esi,-4(%ecx,%ebx,4)
	movl $1,%eax
	movl %ebx,%edx
	decl %edx
	jmp .L57
.L60:
	movl (%edi,%eax,4),%ebx
	movl %ebx,(%ecx,%eax,4)
	incl %eax
.L57:
	cmpl %eax,%edx
	jg .L60
.L38:
	movl %ecx,%eax
.L14:
	leal -24(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl addii
addii:
	pushl %ebp
	movl %esp,%ebp
	subl $36,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl %edi,-4(%ebp)
	movl 12(%ebp),%edi
	movl %edi,-28(%ebp)
	movl -4(%ebp),%edi
	movl 4(%edi),%edx
	andl $65535,%edx
	movl %edx,-16(%ebp)
	movl -28(%ebp),%edi
	movl 4(%edi),%edx
	andl $65535,%edx
	movl %edx,-20(%ebp)
	cmpl -16(%ebp),%edx
	jle .L69
	movl -4(%ebp),%edi
	movl %edi,-24(%ebp)
	movl -28(%ebp),%edi
	movl %edi,-4(%ebp)
	movl -24(%ebp),%edi
	movl %edi,-28(%ebp)
	movl -16(%ebp),%edi
	movl %edi,-8(%ebp)
	movl %edx,-16(%ebp)
	movl %edi,-20(%ebp)
.L69:
	movl -28(%ebp),%edi
	movl 4(%edi),%edx
	sarl $24,%edx
	movl %edx,-12(%ebp)
	jne .L70
	pushl -4(%ebp)
	call icopy
	jmp .L68
.L70:
	movl -4(%ebp),%edi
	movl 4(%edi),%edx
	sarl $24,%edx
	movl %edx,-8(%ebp)
	movl -12(%ebp),%edi
	cmpl %edx,%edi
	jne .L71
	movl -16(%ebp),%eax
	incl %eax
	pushl %eax
	call cgeti
	movl %eax,-24(%ebp)
	movl $0,-32(%ebp)
	movl -16(%ebp),%eax
	leal (,%eax,4),%eax
	movl -24(%ebp),%edi
	leal 4(%eax,%edi),%ebx
	movl -4(%ebp),%edi
	leal (%eax,%edi),%esi
	movl -28(%ebp),%edx
	movl -20(%ebp),%edi
	leal (%edx,%edi,4),%edx
	movl %edx,-36(%ebp)
	leal -2(%edi),%edi
	movl %edi,-28(%ebp)
.L72:
	movl -28(%ebp),%eax
	decl %eax
	cmpl $15,%eax
	ja .L77
	jmp *.L109(,%eax,4)
.L109:
	.long .L107
	.long .L105
	.long .L103
	.long .L101
	.long .L99
	.long .L97
	.long .L95
	.long .L93
	.long .L91
	.long .L89
	.long .L87
	.long .L85
	.long .L83
	.long .L81
	.long .L79
	.long .L77
.L77:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L78
.L79:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L80
.L81:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L82
.L83:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L84
.L85:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L86
.L87:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L88
.L89:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L90
.L91:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L92
.L93:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L94
.L95:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L96
.L97:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L98
.L99:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L100
.L101:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L102
.L103:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L104
.L105:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L106
.L107:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L108
.L78:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L80:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L82:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L84:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L86:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L88:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L90:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L92:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L94:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L96:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L98:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L100:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L102:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L104:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L106:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L108:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	adcl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
/APP
	movl $0,-32(%ebp)
	adcl $0,-32(%ebp)
/NO_APP
	movl -28(%ebp),%edi
	leal -16(%edi),%edx
	movl %edx,-28(%ebp)
	testl %edx,%edx
	jg .L72
	cmpl $0,-32(%ebp)
	je .L110
	movl -4(%ebp),%edi
	leal 8(%edi),%edi
	movl %edi,-36(%ebp)
.L111:
	leal -4(%esi),%esi
	movl %esi,%eax
	cmpl -36(%ebp),%esi
	jb .L112
	cmpl $-1,(%esi)
	jne .L113
	leal -4(%ebx),%ebx
	movl $0,(%ebx)
	jmp .L111
.L113:
	leal -4(%ebx),%ebx
	movl (%eax),%edi
	incl %edi
	jmp .L190
.L117:
	leal -4(%ebx),%ebx
	movl (%eax),%edi
.L190:
	movl %edi,(%ebx)
	leal -4(%esi),%esi
	movl %esi,%eax
	cmpl -36(%ebp),%esi
	jae .L117
	jmp .L191
.L112:
	movl -24(%ebp),%edi
	movl $1,8(%edi)
	movl -4(%ebp),%edx
	movl 4(%edx),%ecx
	incl %ecx
	movl %ecx,4(%edi)
	jmp .L123
.L110:
	movl -16(%ebp),%edi
	subl -20(%ebp),%edi
	movl %edi,-32(%ebp)
	incl -32(%ebp)
	jmp .L120
.L122:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl (%esi),%edi
	movl %edi,(%ebx)
.L120:
	decl -32(%ebp)
	jne .L122
.L191:
	movl -24(%ebp),%edi
	movl (%edi),%edx
	decl %edx
	movl %edx,4(%edi)
	movl -4(%ebp),%edx
	movl 4(%edx),%ecx
	movl %ecx,8(%edi)
	leal 4(%edi),%edx
	movl %edx,-24(%ebp)
	addl $4,avma
	jmp .L123
.L71:
	movl -20(%ebp),%edi
	cmpl -16(%ebp),%edi
	jne .L124
	movl -16(%ebp),%edi
	decl %edi
	movl %edi,-32(%ebp)
	movl -4(%ebp),%edi
	leal 8(%edi),%esi
	movl -28(%ebp),%edi
	leal 8(%edi),%edi
	movl %edi,-36(%ebp)
	jmp .L125
.L131:
	movl (%esi),%ebx
	leal 4(%esi),%esi
	movl -36(%ebp),%edi
	movl (%edi),%eax
	leal 4(%edi),%edx
	movl %edx,-36(%ebp)
	cmpl %eax,%ebx
	jae .L127
	movl -4(%ebp),%edi
	movl %edi,-24(%ebp)
	movl -28(%ebp),%edi
	movl %edi,-4(%ebp)
	movl -24(%ebp),%edi
	movl %edi,-28(%ebp)
	movl -12(%ebp),%edi
	movl %edi,-8(%ebp)
	jmp .L124
.L127:
	cmpl %ebx,%eax
	jb .L124
.L125:
	decl -32(%ebp)
	jne .L131
	movl gzero,%eax
	jmp .L68
.L124:
	pushl -16(%ebp)
	call cgeti
	movl %eax,-24(%ebp)
	movl $0,-32(%ebp)
	movl -16(%ebp),%eax
	leal (,%eax,4),%eax
	movl -4(%ebp),%edi
	leal (%eax,%edi),%esi
	movl -28(%ebp),%edx
	movl -20(%ebp),%edi
	leal (%edx,%edi,4),%edx
	movl %edx,-36(%ebp)
	movl -24(%ebp),%edi
	leal (%eax,%edi),%ebx
	movl -20(%ebp),%edi
	leal -2(%edi),%edi
	movl %edi,-28(%ebp)
.L133:
	movl -28(%ebp),%eax
	decl %eax
	cmpl $15,%eax
	ja .L138
	jmp *.L170(,%eax,4)
.L170:
	.long .L168
	.long .L166
	.long .L164
	.long .L162
	.long .L160
	.long .L158
	.long .L156
	.long .L154
	.long .L152
	.long .L150
	.long .L148
	.long .L146
	.long .L144
	.long .L142
	.long .L140
	.long .L138
.L138:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L139
.L140:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L141
.L142:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L143
.L144:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L145
.L146:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L147
.L148:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L149
.L150:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L151
.L152:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L153
.L154:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L155
.L156:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L157
.L158:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L159
.L160:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L161
.L162:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L163
.L164:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L165
.L166:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L167
.L168:
	movl -32(%ebp),%edi
/APP
	addl $-1,%edi
/NO_APP
	movl %edi,-32(%ebp)
	jmp .L169
.L139:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L141:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L143:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L145:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L147:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L149:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L151:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L153:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L155:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L157:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L159:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L161:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L163:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L165:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L167:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
.L169:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl -36(%ebp),%edi
	leal -4(%edi),%edx
	movl %edx,-36(%ebp)
	movl (%esi),%edi
/APP
	sbbl (%edx),%edi
/NO_APP
	movl %edi,(%ebx)
/APP
	movl $0,-32(%ebp)
	adcl $0,-32(%ebp)
/NO_APP
	movl -28(%ebp),%edi
	leal -16(%edi),%edx
	movl %edx,-28(%ebp)
	testl %edx,%edx
	jg .L133
	cmpl $0,-32(%ebp)
	je .L171
	jmp .L172
.L174:
	leal -4(%ebx),%ebx
	movl $-1,(%ebx)
.L172:
	leal -4(%esi),%esi
	movl (%esi),%eax
	testl %eax,%eax
	je .L174
	movl -4(%ebp),%edi
	leal 8(%edi),%edi
	movl %edi,-36(%ebp)
	cmpl %esi,%edi
	ja .L179
	leal -4(%ebx),%ebx
	decl %eax
	movl %eax,(%ebx)
	jmp .L176
.L178:
	leal -4(%ebx),%ebx
	movl (%eax),%edi
	movl %edi,(%ebx)
.L176:
	leal -4(%esi),%esi
	movl %esi,%eax
	cmpl -36(%ebp),%esi
	jae .L178
	jmp .L179
.L171:
	movl -16(%ebp),%edi
	subl -20(%ebp),%edi
	movl %edi,-28(%ebp)
	incl -28(%ebp)
	jmp .L180
.L182:
	leal -4(%ebx),%ebx
	leal -4(%esi),%esi
	movl (%esi),%edi
	movl %edi,(%ebx)
.L180:
	decl -28(%ebp)
	jne .L182
.L179:
	movl -24(%ebp),%edi
	cmpl $0,8(%edi)
	je .L183
	movl -4(%ebp),%edx
	movl 4(%edx),%ecx
	movl %ecx,4(%edi)
	jmp .L123
.L183:
	movl -24(%ebp),%edi
	leal 12(%edi),%ebx
	cmpl $0,(%ebx)
	jne .L189
.L187:
	leal 4(%ebx),%ebx
	cmpl $0,(%ebx)
	je .L187
.L189:
	leal -8(%ebx),%ebx
	movl %ebx,%edi
	subl -24(%ebp),%edi
	movl %edi,-28(%ebp)
	jns .L188
	leal 3(%edi),%edx
	movl %edx,-28(%ebp)
.L188:
	sarl $2,-28(%ebp)
	movl -24(%ebp),%edi
	movl (%edi),%eax
	subl -28(%ebp),%eax
	movl %eax,(%ebx)
	movl %eax,4(%ebx)
	movl %ebx,-24(%ebp)
	movl 4(%ebx),%eax
	andl $16777215,%eax
	movl -8(%ebp),%edi
	sall $24,%edi
	movl %edi,-32(%ebp)
	addl %edi,%eax
	movl %eax,4(%ebx)
	movl -28(%ebp),%eax
	leal (,%eax,4),%eax
	addl %eax,avma
.L123:
	movl -24(%ebp),%eax
.L68:
	leal -48(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl mulss
mulss:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%eax
	movl 12(%ebp),%ebx
	testl %eax,%eax
	je .L194
	testl %ebx,%ebx
	jne .L193
.L194:
	movl gzero,%eax
	jmp .L192
.L193:
	movl $1,%esi
	testl %eax,%eax
	jge .L195
	movl $-1,%esi
	negl %eax
	jns .L195
	pushl %eax
	call stoi
	pushl %eax
	pushl %ebx
	call mulsi
	jmp .L192
.L195:
	testl %ebx,%ebx
	jge .L197
	negl %esi
	negl %ebx
	jns .L197
	pushl $ABS_MOST_NEGS
	testl %esi,%esi
	jg .L200
	negl %eax
.L200:
	pushl %eax
	call mulsi
	jmp .L192
.L197:
/APP
	mull %ebx
/NO_APP
	movl %edx,%ebx
	movl %eax,%edi
	testl %ebx,%ebx
	je .L201
	pushl $4
	call cgeti
	movl %eax,%ecx
	movl %ebx,8(%ecx)
	movl %edi,12(%ecx)
	jmp .L202
.L201:
	pushl $3
	call cgeti
	movl %eax,%ecx
	movl %edi,8(%ecx)
.L202:
	movl (%ecx),%edx
	movl %edx,4(%ecx)
	movl %edx,%eax
	andl $16777215,%eax
	sall $24,%esi
	movl %esi,-4(%ebp)
	addl -4(%ebp),%eax
	movl %eax,4(%ecx)
	movl %ecx,%eax
.L192:
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl mulii
mulii:
	pushl %ebp
	movl %esp,%ebp
	subl $44,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl 12(%ebp),%eax
	movl %eax,-4(%ebp)
	movl 4(%edi),%eax
	andl $65535,%eax
	movl %eax,-8(%ebp)
	movl -4(%ebp),%eax
	movl 4(%eax),%edx
	andl $65535,%edx
	movl %edx,-12(%ebp)
	movl 4(%edi),%esi
	sarl $24,%esi
	je .L219
	movl -4(%ebp),%eax
	movl 4(%eax),%ecx
	sarl $24,%ecx
	jne .L205
.L219:
	movl gzero,%eax
	jmp .L203
.L205:
	testl %ecx,%ecx
	jge .L206
	negl %esi
.L206:
	movl -12(%ebp),%eax
	cmpl -8(%ebp),%eax
	jge .L207
	movl %edi,-20(%ebp)
	movl -4(%ebp),%edi
	movl -20(%ebp),%eax
	movl %eax,-4(%ebp)
	movl -8(%ebp),%ebx
	movl -12(%ebp),%eax
	movl %eax,-8(%ebp)
	movl %ebx,-12(%ebp)
.L207:
	movl -8(%ebp),%eax
	movl -12(%ebp),%edx
	leal -2(%edx,%eax),%ebx
	cmpl $65535,%ebx
	jle .L208
	pushl $17
	call err
	leal 4(%esp),%esp
.L208:
	pushl %ebx
	call cgeti
	movl %eax,-20(%ebp)
	movl (%eax),%edx
	movl %edx,4(%eax)
	movl 4(%eax),%edx
	andl $16777215,%edx
	sall $24,%esi
	movl %esi,-44(%ebp)
	addl -44(%ebp),%edx
	movl %edx,4(%eax)
	movl -8(%ebp),%eax
	leal -4(%edi,%eax,4),%edx
	movl %edx,-28(%ebp)
	movl (%edx),%eax
	movl %eax,-16(%ebp)
	xorl %ecx,%ecx
	movl -12(%ebp),%edi
	decl %edi
	movl -4(%ebp),%edx
	movl -12(%ebp),%eax
	leal (%edx,%eax,4),%esi
	movl -20(%ebp),%eax
	leal (%eax,%ebx,4),%eax
	movl %eax,-24(%ebp)
	jmp .L209
.L211:
	movl -24(%ebp),%eax
	leal -4(%eax),%edx
	movl %edx,-24(%ebp)
	movl %ecx,-40(%ebp)
	leal -4(%esi),%esi
	movl -16(%ebp),%eax
/APP
	mull (%esi)
/NO_APP
	movl %edx,%ecx
	movl %eax,-44(%ebp)
	movl -40(%ebp),%eax
/APP
	addl -44(%ebp),%eax
	adcl $0,%ecx
/NO_APP
	movl %eax,-40(%ebp)
	movl -24(%ebp),%eax
	movl -40(%ebp),%edx
	movl %edx,(%eax)
.L209:
	decl %edi
	jne .L211
	movl -24(%ebp),%eax
	movl %ecx,-4(%eax)
	movl -20(%ebp),%eax
	leal (%eax,%ebx,4),%eax
	movl %eax,-24(%ebp)
	movl -4(%ebp),%edx
	movl -12(%ebp),%eax
	leal (%edx,%eax,4),%edx
	movl %edx,-32(%ebp)
	decl -12(%ebp)
	movl -8(%ebp),%eax
	leal -2(%eax),%edx
	movl %edx,-8(%ebp)
	jmp .L212
.L217:
	movl -28(%ebp),%eax
	leal -4(%eax),%edx
	movl %edx,-28(%ebp)
	movl (%edx),%eax
	movl %eax,-36(%ebp)
	movl -12(%ebp),%edi
	movl -32(%ebp),%esi
	movl -24(%ebp),%eax
	leal -4(%eax),%eax
	movl %eax,-44(%ebp)
	movl %eax,-24(%ebp)
	xorl %ebx,%ebx
	jmp .L214
.L216:
	leal -4(%esi),%esi
	movl (%esi),%eax
/APP
	mull -36(%ebp)
/NO_APP
	movl %edx,%ecx
	movl %eax,-40(%ebp)
	movl -44(%ebp),%eax
	leal -4(%eax),%edx
	movl %edx,-44(%ebp)
	movl -40(%ebp),%eax
/APP
	addl (%edx),%eax
	adcl $0,%ecx
/NO_APP
	movl %eax,-40(%ebp)
/APP
	addl %ebx,%eax
	adcl $0,%ecx
/NO_APP
	movl %eax,(%edx)
	movl %ecx,%ebx
.L214:
	decl %edi
	jne .L216
	movl -44(%ebp),%eax
	movl %ecx,-4(%eax)
.L212:
	decl -8(%ebp)
	cmpl $0,-8(%ebp)
	jg .L217
	movl -20(%ebp),%eax
	cmpl $0,8(%eax)
	jne .L218
	movl 4(%eax),%edx
	decl %edx
	movl %edx,8(%eax)
	movl (%eax),%edx
	decl %edx
	movl %edx,4(%eax)
	leal 4(%eax),%edx
	movl %edx,-20(%ebp)
	addl $4,avma
.L218:
	movl -20(%ebp),%eax
.L203:
	leal -56(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl confrac
confrac:
	pushl %ebp
	movl %esp,%ebp
	subl $52,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%ecx
	movl (%ecx),%edi
	andl $65535,%edi
	movl %edi,-4(%ebp)
	movl 4(%ecx),%edi
	andl $16777215,%edi
	movl %edi,-40(%ebp)
	leal -8388608(%edi),%ecx
	movl %ecx,-40(%ebp)
	movl $-1,%ecx
	subl -40(%ebp),%ecx
	movl %ecx,-40(%ebp)
	movl avma,%ecx
	movl %ecx,-8(%ebp)
	movl -4(%ebp),%ecx
	leal -2(%ecx),%ecx
	movl %ecx,-16(%ebp)
	sall $5,-16(%ebp)
	movl -16(%ebp),%ecx
	movl -40(%ebp),%edi
	leal (%edi,%ecx),%edi
	movl %edi,-16(%ebp)
	leal 63(%edi),%edi
	movl %edi,-12(%ebp)
	sarl $5,-12(%ebp)
	pushl -12(%ebp)
	call cgeti
	movl %eax,-24(%ebp)
	movl -40(%ebp),%eax
	sarl $5,%eax
	xorl %ebx,%ebx
	leal 4(%esp),%esp
	cmpl %ebx,%eax
	jle .L247
.L224:
	movl -24(%ebp),%ecx
	movl $0,(%ecx,%ebx,4)
	incl %ebx
	cmpl %ebx,%eax
	jg .L224
.L247:
	andl $31,-40(%ebp)
	jne .L225
	movl $2,%esi
	cmpl -4(%ebp),%esi
	jge .L230
.L229:
	movl -24(%ebp),%ecx
	movl 8(%ebp),%edi
	movl (%edi,%esi,4),%edx
	movl %edx,(%ecx,%ebx,4)
	incl %ebx
	incl %esi
	cmpl -4(%ebp),%esi
	jl .L229
	jmp .L230
.L225:
	movl $0,-20(%ebp)
	movl $2,%esi
	cmpl -4(%ebp),%esi
	jge .L245
	movl $32,%ecx
	subl -40(%ebp),%ecx
	movl %ecx,-32(%ebp)
.L234:
	movl %ebx,%eax
	movl 8(%ebp),%ecx
	movl (%ecx,%esi,4),%edi
	movl %edi,-36(%ebp)
	incl %ebx
	movl -40(%ebp),%ecx
	shrl %cl,%edi
	movl -24(%ebp),%ecx
	addl -20(%ebp),%edi
	movl %edi,(%ecx,%eax,4)
	movl -36(%ebp),%edi
	movl -32(%ebp),%ecx
	shll %cl,%edi
	movl %edi,-20(%ebp)
	incl %esi
	cmpl -4(%ebp),%esi
	jl .L234
.L245:
	movl -12(%ebp),%ecx
	movl -24(%ebp),%edi
	movl -20(%ebp),%edx
	movl %edx,-8(%edi,%ecx,4)
.L230:
	movl -12(%ebp),%ecx
	movl -24(%ebp),%edi
	movl $0,-4(%edi,%ecx,4)
	pushl -16(%ebp)
	call __floatsidf
	leal 4(%esp),%esp
	fstpl -52(%ebp)
	pushl $1070810131
	pushl $1430739506
	pushl -48(%ebp)
	pushl -52(%ebp)
	call __muldf3
	leal 16(%esp),%esp
	fstpl -52(%ebp)
	pushl $1072693248
	pushl $0
	pushl -48(%ebp)
	pushl -52(%ebp)
	call __adddf3
	leal 16(%esp),%esp
	fstpl -52(%ebp)
	pushl -48(%ebp)
	pushl -52(%ebp)
	call __fixdfsi
	leal 8(%esp),%esp
	movl %eax,%ebx
	leal 17(%ebx),%ecx
	movl %ecx,-40(%ebp)
	pushl $9
	pushl %ecx
	call __divsi3
	leal 8(%esp),%esp
	movl %eax,-40(%ebp)
	pushl %eax
	call cgeti
	movl %eax,-28(%ebp)
	movl %ebx,(%eax)
	movl $1,%esi
	cmpl -40(%ebp),%esi
	jge .L244
.L242:
	movl $0,-44(%ebp)
	movl -12(%ebp),%ebx
	jmp .L248
.L241:
	movl -44(%ebp),%ecx
	movl %ecx,-36(%ebp)
	movl -24(%ebp),%ecx
	movl (%ecx,%ebx,4),%eax
	movl $1000000000,%edi
/APP
	mull %edi
/NO_APP
	movl %edx,-44(%ebp)
	movl -36(%ebp),%ecx
/APP
	addl %eax,%ecx
	adcl $0,-44(%ebp)
/NO_APP
	movl %ecx,%eax
	movl -24(%ebp),%ecx
	movl %eax,(%ecx,%ebx,4)
.L248:
	decl %ebx
	jns .L241
	movl -28(%ebp),%ecx
	movl -44(%ebp),%edi
	movl %edi,(%ecx,%esi,4)
	incl %esi
	cmpl -40(%ebp),%esi
	jl .L242
.L244:
	movl -8(%ebp),%ecx
	movl %ecx,avma
	movl -28(%ebp),%eax
	leal -64(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl divss
divss:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl 12(%ebp),%esi
	testl %esi,%esi
	jne .L250
	pushl $23
	call err
	leal 4(%esp),%esp
.L250:
	cmpl $-2147483648,%edi
	jne .L251
	pushl %esi
	pushl %edi
	call stoi
	leal 4(%esp),%esp
	pushl %eax
	call divis
	jmp .L249
.L251:
	movl $0,hiremainder
	pushl %edi
	call abs
	movl %eax,%ebx
	pushl %esi
	call abs
	movl %eax,-4(%ebp)
	movl %ebx,%eax
	movl hiremainder,%edx
/APP
	divl -4(%ebp)
/NO_APP
	movl %edx,hiremainder
	movl %eax,%ebx
	leal 8(%esp),%esp
	movl %ebx,%ecx
	testl %esi,%esi
	jge .L252
	negl hiremainder
	negl %ecx
.L252:
	testl %edi,%edi
	jge .L253
	negl %ecx
.L253:
	pushl %ecx
	call stoi
.L249:
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl modss
modss:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%ebx
	movl 12(%ebp),%edi
	testl %edi,%edi
	jne .L255
	pushl $38
	call err
	leal 4(%esp),%esp
.L255:
	cmpl $-2147483648,%ebx
	jne .L256
	pushl %edi
	pushl %ebx
	call stoi
	leal 4(%esp),%esp
	pushl %eax
	call modis
	jmp .L254
.L256:
	xorl %esi,%esi
	pushl %ebx
	call abs
	movl %eax,%ebx
	pushl %edi
	call abs
	movl %eax,%ecx
	movl %ebx,%eax
	movl %esi,%edx
/APP
	divl %ecx
/NO_APP
	movl %edx,%esi
	movl %eax,%ebx
	leal 8(%esp),%esp
	testl %esi,%esi
	jne .L257
	movl gzero,%eax
	jmp .L254
.L257:
	testl %esi,%esi
	jge .L258
	movl %ecx,%eax
	subl %esi,%eax
	pushl %eax
	jmp .L260
.L258:
	pushl %esi
.L260:
	call stoi
	movl %eax,%ecx
	movl %ecx,%eax
.L254:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl resss
resss:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%edi
	testl %edi,%edi
	jne .L262
	pushl $40
	call err
	leal 4(%esp),%esp
.L262:
	xorl %esi,%esi
	pushl 8(%ebp)
	call abs
	movl %eax,%ebx
	pushl %edi
	call abs
	movl %eax,-4(%ebp)
	movl %ebx,%eax
	movl %esi,%edx
/APP
	divl -4(%ebp)
/NO_APP
	movl %edx,%esi
	movl %eax,%ebx
	leal 8(%esp),%esp
	testl %edi,%edi
	jge .L263
	movl %esi,%eax
	negl %eax
	pushl %eax
	jmp .L265
.L263:
	pushl %esi
.L265:
	call stoi
	movl %eax,%ecx
	movl %ecx,%eax
	leal -16(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl divsi
divsi:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edi
	movl 12(%ebp),%ebx
	movl 4(%ebx),%eax
	sarl $24,%eax
	movl 4(%ebx),%esi
	andl $65535,%esi
	testl %eax,%eax
	jne .L267
	pushl $24
	call err
	leal 4(%esp),%esp
.L267:
	testl %edi,%edi
	je .L269
	cmpl $3,%esi
	jg .L269
	cmpl $0,8(%ebx)
	jge .L268
.L269:
	movl %edi,hiremainder
	movl gzero,%eax
	jmp .L266
.L268:
	cmpl $-2147483648,%edi
	jne .L270
	pushl $0
	pushl %ebx
	pushl %edi
	call stoi
	leal 4(%esp),%esp
	pushl %eax
	call dvmdii
	jmp .L266
.L270:
	movl $0,hiremainder
	pushl %edi
	call abs
	movl hiremainder,%edx
/APP
	divl 8(%ebx)
/NO_APP
	movl %edx,hiremainder
	leal 4(%esp),%esp
	movl %eax,%ecx
	movl 4(%ebx),%eax
	sarl $24,%eax
	jns .L271
	negl hiremainder
	negl %ecx
.L271:
	testl %edi,%edi
	jge .L272
	negl %ecx
.L272:
	pushl %ecx
	call stoi
.L266:
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl divis
divis:
	pushl %ebp
	movl %esp,%ebp
	subl $20,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 12(%ebp),%ebx
	movl 8(%ebp),%edx
	movl 4(%edx),%edi
	sarl $24,%edi
	movl %edi,-4(%ebp)
	movl 4(%edx),%edi
	andl $65535,%edi
	movl %edi,-8(%ebp)
	testl %ebx,%ebx
	jne .L274
	pushl $26
	call err
	leal 4(%esp),%esp
.L274:
	cmpl $0,-4(%ebp)
	jne .L275
	movl $0,hiremainder
	movl gzero,%eax
	jmp .L273
.L275:
	testl %ebx,%ebx
	jge .L276
	negl -4(%ebp)
	negl %ebx
	jns .L276
	pushl $0
	pushl %ebx
	call stoi
	leal 4(%esp),%esp
	pushl %eax
	pushl 8(%ebp)
	call dvmdii
	jmp .L273
.L276:
	movl 8(%ebp),%edx
	cmpl 8(%edx),%ebx
	jbe .L278
	cmpl $3,-8(%ebp)
	jne .L279
	pushl 8(%ebp)
	call itos
	movl %eax,hiremainder
	movl gzero,%eax
	jmp .L273
.L279:
	movl -8(%ebp),%eax
	decl %eax
	pushl %eax
	call cgeti
	movl %eax,-20(%ebp)
	movl $1,-12(%ebp)
	movl 8(%ebp),%edx
	movl 8(%edx),%esi
	jmp .L281
.L278:
	pushl -8(%ebp)
	call cgeti
	movl %eax,-20(%ebp)
	movl $0,-12(%ebp)
	xorl %esi,%esi
.L281:
	movl -12(%ebp),%edx
	leal 2(%edx),%ecx
	cmpl -8(%ebp),%ecx
	jge .L288
.L285:
	movl %ecx,%edx
	subl -12(%ebp),%edx
	movl %edx,-16(%ebp)
	movl 8(%ebp),%edi
	movl (%edi,%ecx,4),%eax
	movl %esi,%edx
/APP
	divl %ebx
/NO_APP
	movl %edx,%esi
	movl -20(%ebp),%edi
	movl -16(%ebp),%edx
	movl %eax,(%edi,%edx,4)
	incl %ecx
	cmpl -8(%ebp),%ecx
	jl .L285
.L288:
	movl -20(%ebp),%edx
	movl (%edx),%edi
	movl %edi,4(%edx)
	movl 4(%edx),%eax
	andl $16777215,%eax
	movl -4(%ebp),%edx
	sall $24,%edx
	movl %edx,-16(%ebp)
	movl -20(%ebp),%edx
	addl -16(%ebp),%eax
	movl %eax,4(%edx)
	cmpl $0,-4(%ebp)
	jge .L286
	movl %esi,%eax
	negl %eax
	jmp .L287
.L286:
	movl %esi,%eax
.L287:
	movl %eax,hiremainder
	movl -20(%ebp),%eax
.L273:
	leal -32(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
	.align 4
.globl dvmdii
dvmdii:
	pushl %ebp
	movl %esp,%ebp
	subl $104,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%edx
	movl 4(%edx),%ecx
	sarl $24,%ecx
	movl %ecx,-32(%ebp)
	movl 12(%ebp),%edx
	movl 4(%edx),%ecx
	sarl $24,%ecx
	movl %ecx,-36(%ebp)
	jne .L290
	pushl $36
	call err
	leal 4(%esp),%esp
.L290:
	cmpl $0,-32(%ebp)
	jne .L291
	cmpl $-1,16(%ebp)
	je .L393
	cmpl $0,16(%ebp)
	je .L393
	movl 16(%ebp),%edx
	movl gzero,%ecx
	movl %ecx,(%edx)
.L393:
	movl gzero,%eax
	jmp .L289
.L291:
	movl 8(%ebp),%edx
	movl 4(%edx),%ecx
	andl $65535,%ecx
	movl %ecx,-8(%ebp)
	movl 12(%ebp),%edx
	movl 4(%edx),%ecx
	andl $65535,%ecx
	movl %ecx,-12(%ebp)
	movl -8(%ebp),%edx
	subl %ecx,%edx
	movl %edx,-16(%ebp)
	jns .L294
	cmpl $-1,16(%ebp)
	jne .L295
	pushl 8(%ebp)
	call icopy
	jmp .L289
.L295:
	cmpl $0,16(%ebp)
	je .L393
	pushl 8(%ebp)
	call icopy
	movl 16(%ebp),%edx
	movl %eax,(%edx)
	jmp .L393
.L294:
	movl avma,%edx
	movl %edx,-4(%ebp)
	cmpl $0,-32(%ebp)
	jge .L297
	negl -36(%ebp)
.L297:
	cmpl $3,-12(%ebp)
	jne .L298
	movl 12(%ebp),%edx
	movl 8(%edx),%ecx
	movl %ecx,-48(%ebp)
	movl 8(%ebp),%edx
	leal 8(%edx),%edx
	movl %edx,-104(%ebp)
	movl 8(%ebp),%edx
	movl 8(%edx),%ecx
	cmpl -48(%ebp),%ecx
	jae .L299
	movl -8(%ebp),%edx
	decl %edx
	movl %edx,-100(%ebp)
	movl -104(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,-96(%ebp)
	leal 4(%edx),%ecx
	movl %ecx,-104(%ebp)
	jmp .L300
.L299:
	movl -8(%ebp),%edx
	movl %edx,-100(%ebp)
	movl $0,-96(%ebp)
.L300:
	pushl -100(%ebp)
	call cgeti
	movl %eax,-56(%ebp)
	movl -100(%ebp),%edx
	decl %edx
	movl %edx,-20(%ebp)
	movl %eax,%edx
	leal 8(%edx),%edx
	movl %edx,-72(%ebp)
	leal 4(%esp),%esp
	jmp .L301
.L303:
	movl -104(%ebp),%edx
	movl (%edx),%eax
	leal 4(%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -96(%ebp),%edx
/APP
	divl -48(%ebp)
/NO_APP
	movl %edx,-96(%ebp)
	movl -72(%ebp),%edx
	movl %eax,(%edx)
	leal 4(%edx),%ecx
	movl %ecx,-72(%ebp)
.L301:
	decl -20(%ebp)
	jne .L303
	cmpl $-1,16(%ebp)
	jne .L304
	movl -4(%ebp),%edx
	movl %edx,avma
	cmpl $0,-96(%ebp)
	je .L393
	pushl $3
	call cgeti
	movl %eax,-60(%ebp)
	movl -36(%ebp),%eax
	sall $24,%eax
	movl -60(%ebp),%edx
	addl $3,%eax
	movl %eax,4(%edx)
	movl -96(%ebp),%ecx
	movl %ecx,8(%edx)
	movl -60(%ebp),%eax
	jmp .L289
.L304:
	cmpl $2,-100(%ebp)
	je .L306
	movl -56(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,4(%edx)
	movl 4(%edx),%eax
	andl $16777215,%eax
	movl -36(%ebp),%edx
	sall $24,%edx
	movl %edx,-88(%ebp)
	movl -56(%ebp),%edx
	addl -88(%ebp),%eax
	movl %eax,4(%edx)
	jmp .L307
.L306:
	movl -4(%ebp),%edx
	movl %edx,avma
	movl gzero,%edx
	movl %edx,-56(%ebp)
.L307:
	cmpl $0,16(%ebp)
	jne .L308
	movl -56(%ebp),%eax
	jmp .L289
.L308:
	cmpl $0,-96(%ebp)
	jne .L309
	movl 16(%ebp),%edx
	movl gzero,%ecx
	jmp .L389
.L309:
	pushl $3
	call cgeti
	movl %eax,-60(%ebp)
	movl -32(%ebp),%eax
	sall $24,%eax
	movl -60(%ebp),%edx
	addl $3,%eax
	movl %eax,4(%edx)
	movl -96(%ebp),%ecx
	movl %ecx,8(%edx)
	movl 16(%ebp),%edx
	movl -60(%ebp),%ecx
.L389:
	movl %ecx,(%edx)
	movl -56(%ebp),%eax
	jmp .L289
.L298:
	pushl -8(%ebp)
	call cgeti
	movl %eax,-56(%ebp)
	movl 12(%ebp),%edx
	pushl 8(%edx)
	call bfffo
	movl %eax,-24(%ebp)
	leal 8(%esp),%esp
	testl %eax,%eax
	je .L312
	movl 12(%ebp),%edx
	leal 8(%edx),%edx
	movl %edx,-104(%ebp)
	pushl -12(%ebp)
	call cgeti
	movl %eax,-60(%ebp)
	movl -104(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,-88(%ebp)
	leal 4(%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -88(%ebp),%edx
	movl -24(%ebp),%ecx
	shll %cl,%edx
	movl %edx,-28(%ebp)
	leal 8(%eax),%eax
	movl -12(%ebp),%edx
	leal -2(%edx),%edx
	movl %edx,-20(%ebp)
	leal 4(%esp),%esp
	movl $32,%edx
	subl %ecx,%edx
	movl %edx,-92(%ebp)
	jmp .L313
.L315:
	movl -104(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,-88(%ebp)
	leal 4(%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -88(%ebp),%edx
	movl -92(%ebp),%ecx
	shrl %cl,%edx
	movl %edx,-96(%ebp)
	addl -28(%ebp),%edx
	movl %edx,(%eax)
	leal 4(%eax),%eax
	movl -88(%ebp),%edx
	movl -24(%ebp),%ecx
	shll %cl,%edx
	movl %edx,-28(%ebp)
.L313:
	decl -20(%ebp)
	jne .L315
	movl -28(%ebp),%edx
	movl %edx,(%eax)
	movl $0,-28(%ebp)
	movl 8(%ebp),%edx
	leal 8(%edx),%edx
	movl %edx,-104(%ebp)
	movl -56(%ebp),%edx
	leal 4(%edx),%edx
	movl %edx,-72(%ebp)
	movl -8(%ebp),%edx
	decl %edx
	movl %edx,-20(%ebp)
	movl $32,%eax
	subl -24(%ebp),%eax
	jmp .L316
.L318:
	movl -104(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,-88(%ebp)
	leal 4(%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -88(%ebp),%edx
	movl %eax,%ecx
	shrl %cl,%edx
	movl %edx,-96(%ebp)
	movl -72(%ebp),%edx
	movl -96(%ebp),%ecx
	addl -28(%ebp),%ecx
	movl %ecx,(%edx)
	leal 4(%edx),%ecx
	movl %ecx,-72(%ebp)
	movl -88(%ebp),%edx
	movl -24(%ebp),%ecx
	shll %cl,%edx
	movl %edx,-28(%ebp)
.L316:
	decl -20(%ebp)
	jne .L318
	movl -72(%ebp),%edx
	movl -28(%ebp),%ecx
	movl %ecx,(%edx)
	jmp .L319
.L312:
	movl 8(%ebp),%edx
	leal 8(%edx),%edx
	movl %edx,-104(%ebp)
	movl -56(%ebp),%edx
	leal 4(%edx),%edx
	movl %edx,-72(%ebp)
	movl $0,(%edx)
	leal 4(%edx),%ecx
	movl %ecx,-72(%ebp)
	movl -8(%ebp),%edx
	decl %edx
	movl %edx,-100(%ebp)
	jmp .L320
.L322:
	movl -72(%ebp),%edx
	movl -104(%ebp),%ecx
	movl (%ecx),%esi
	movl %esi,(%edx)
	leal 4(%ecx),%edx
	movl %edx,-104(%ebp)
	movl -72(%ebp),%edx
	leal 4(%edx),%ecx
	movl %ecx,-72(%ebp)
.L320:
	decl -100(%ebp)
	jne .L322
	movl 12(%ebp),%edx
	movl %edx,-60(%ebp)
.L319:
	movl -60(%ebp),%edx
	movl 8(%edx),%ecx
	movl %ecx,-48(%ebp)
	movl 12(%edx),%ecx
	movl %ecx,-40(%ebp)
	movl -56(%ebp),%edx
	leal 4(%edx),%edx
	movl %edx,-72(%ebp)
	movl -16(%ebp),%edx
	leal 2(%edx),%edx
	movl %edx,-20(%ebp)
	movl -12(%ebp),%edx
	leal (,%edx,4),%edx
	movl %edx,-80(%ebp)
	jmp .L323
.L352:
	movl -72(%ebp),%edx
	movl (%edx),%eax
	leal 4(%edx),%ecx
	movl %ecx,-72(%ebp)
	cmpl -48(%ebp),%eax
	jne .L325
	movl $-1,-52(%ebp)
	movl -48(%ebp),%edx
	movl %edx,-88(%ebp)
	addl (%ecx),%edx
	movl %edx,-92(%ebp)
	movl -88(%ebp),%edx
	cmpl -92(%ebp),%edx
	jbe .L326
	movl $1,%eax
	jmp .L327
.L326:
	xorl %eax,%eax
.L327:
	movl -92(%ebp),%edx
	jmp .L390
.L325:
	movl -72(%ebp),%ecx
	movl (%ecx),%eax
	movl -4(%ecx),%edx
/APP
	divl -48(%ebp)
/NO_APP
	movl %edx,-96(%ebp)
	movl %eax,-52(%ebp)
	xorl %eax,%eax
.L390:
	movl %edx,-28(%ebp)
	testl %eax,%eax
	jne .L329
	movl -52(%ebp),%eax
/APP
	mull -40(%ebp)
/NO_APP
	movl %edx,-96(%ebp)
	movl %eax,-88(%ebp)
	movl -72(%ebp),%edx
	movl 4(%edx),%eax
	movl -88(%ebp),%edx
	subl %eax,%edx
	movl %edx,-92(%ebp)
	cmpl -88(%ebp),%eax
	ja .L330
	xorl %eax,%eax
	jmp .L331
.L330:
	movl $1,%eax
.L331:
	movl -92(%ebp),%edx
	movl %edx,-44(%ebp)
	movl -28(%ebp),%edx
	leal (%eax,%edx),%ecx
	movl %ecx,-28(%ebp)
	xorl %eax,%eax
	cmpl -96(%ebp),%ecx
	jae .L332
	movl $1,%eax
.L332:
	movl %eax,-100(%ebp)
	movl -96(%ebp),%edx
	subl -28(%ebp),%edx
	movl %edx,-92(%ebp)
	movl -28(%ebp),%edx
	jmp .L341
.L342:
	decl -52(%ebp)
	movl -44(%ebp),%edx
	movl %edx,-88(%ebp)
	movl -40(%ebp),%eax
	subl %eax,%edx
	movl %edx,-92(%ebp)
	cmpl -88(%ebp),%eax
	ja .L337
	xorl %eax,%eax
	jmp .L338
.L337:
	movl $1,%eax
.L338:
	movl -92(%ebp),%edx
	movl %edx,-44(%ebp)
	subl %eax,-104(%ebp)
	xorl %eax,%eax
	movl -48(%ebp),%edx
	cmpl -104(%ebp),%edx
	jae .L339
	movl $1,%eax
.L339:
	movl %eax,-100(%ebp)
	movl -104(%ebp),%edx
	movl %edx,-88(%ebp)
	movl -48(%ebp),%eax
	subl %eax,%edx
	movl %edx,-92(%ebp)
.L341:
	movl -92(%ebp),%edx
	movl %edx,-104(%ebp)
	cmpl $0,-100(%ebp)
	jne .L342
.L329:
	movl $0,-96(%ebp)
	movl -12(%ebp),%edx
	decl %edx
	movl %edx,-100(%ebp)
	movl -72(%ebp),%edx
	movl -80(%ebp),%ecx
	leal -8(%ecx,%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -60(%ebp),%edx
	movl -80(%ebp),%ecx
	leal (%ecx,%edx),%ecx
	movl %ecx,-76(%ebp)
	jmp .L343
.L347:
	movl -96(%ebp),%edx
	movl %edx,-88(%ebp)
	movl -76(%ebp),%edx
	leal -4(%edx),%ecx
	movl %ecx,-76(%ebp)
	movl -52(%ebp),%eax
/APP
	mull (%ecx)
/NO_APP
	movl %edx,-96(%ebp)
	movl %eax,-92(%ebp)
	movl -88(%ebp),%eax
/APP
	addl -92(%ebp),%eax
	adcl $0,-96(%ebp)
/NO_APP
	movl -104(%ebp),%edx
	leal -4(%edx),%ecx
	movl %ecx,-104(%ebp)
	movl %ecx,-84(%ebp)
	movl (%ecx),%edx
	movl %edx,-88(%ebp)
	subl %eax,%edx
	movl %edx,-92(%ebp)
	cmpl -88(%ebp),%eax
	ja .L345
	xorl %eax,%eax
	jmp .L346
.L345:
	movl $1,%eax
.L346:
	movl -84(%ebp),%edx
	movl -92(%ebp),%ecx
	movl %ecx,(%edx)
	movl -96(%ebp),%edx
	leal (%eax,%edx),%ecx
	movl %ecx,-96(%ebp)
.L343:
	decl -100(%ebp)
	jne .L347
	movl -72(%ebp),%edx
	movl -96(%ebp),%ecx
	cmpl -4(%edx),%ecx
	jbe .L348
	xorl %eax,%eax
	decl -52(%ebp)
	movl -12(%ebp),%edx
	decl %edx
	movl %edx,-100(%ebp)
	movl -72(%ebp),%edx
	movl -80(%ebp),%ecx
	leal -8(%ecx,%edx),%ecx
	movl %ecx,-104(%ebp)
	movl -60(%ebp),%edx
	movl -80(%ebp),%ecx
	leal (%ecx,%edx),%ecx
	movl %ecx,-76(%ebp)
	jmp .L349
.L351:
	movl -104(%ebp),%edx
	leal -4(%edx),%ecx
	movl %ecx,-104(%ebp)
/APP
	addl $-1,%eax
/NO_APP
	movl -76(%ebp),%edx
	leal -4(%edx),%ecx
	movl %ecx,-76(%ebp)
	movl -104(%ebp),%edx
	movl (%ecx),%esi
/APP
	adcl %esi,(%edx)
	movl $0,%eax
	adcl $0,%eax
/NO_APP
.L349:
	decl -100(%ebp)
	jne .L351
.L348:
	movl -72(%ebp),%edx
	movl -52(%ebp),%ecx
	movl %ecx,-4(%edx)
.L323:
	decl -20(%ebp)
	jne .L352
	movl avma,%edx
	movl %edx,-96(%ebp)
	cmpl $-1,16(%ebp)
	je .L353
	movl -16(%ebp),%edx
	leal 2(%edx),%edx
	movl %edx,-104(%ebp)
	movl -56(%ebp),%ecx
	leal (%ecx,%edx,4),%ecx
	movl %ecx,-72(%ebp)
	movl -56(%ebp),%edx
	cmpl $0,4(%edx)
	je .L354
	incl -104(%ebp)
	jmp .L355
.L354:
	cmpl $0,-16(%ebp)
	jne .L355
	movl $0,-36(%ebp)
.L355:
	pushl -104(%ebp)
	call cgeti
	movl %eax,-64(%ebp)
	movl -104(%ebp),%edx
	leal (%eax,%edx,4),%eax
	decl %edx
	movl %edx,-100(%ebp)
	leal 4(%esp),%esp
	jmp .L357
.L359:
	leal -4(%eax),%eax
	movl -72(%ebp),%edx
	leal -4(%edx),%ecx
	movl %ecx,-72(%ebp)
	movl (%ecx),%edx
	movl %edx,(%eax)
.L357:
	decl -100(%ebp)
	jne .L359
	cmpl $2,-104(%ebp)
	ja .L360
	movl -64(%ebp),%edx
	movl $2,4(%edx)
	jmp .L353
.L360:
	movl -64(%ebp),%edx
	movl (%edx),%ecx
	movl %ecx,4(%edx)
	movl 4(%edx),%eax
	andl $16777215,%eax
	movl -36(%ebp),%edx
	sall $24,%edx
	movl %edx,-88(%ebp)
	movl -64(%ebp),%edx
	addl -88(%ebp),%eax
	movl %eax,4(%edx)
.L353:
	cmpl $0,16(%ebp)
	je .L362
	movl -16(%ebp),%edx
	leal 2(%edx),%edx
	movl %edx,-100(%ebp)
	jmp .L391
.L366:
	movl -56(%ebp),%ecx
	movl -100(%ebp),%edx
	cmpl $0,(%ecx,%edx,4)
	jne .L364
	incl -100(%ebp)
.L391:
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jg .L366
.L364:
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jne .L367
	pushl gzero
	call icopy
	movl %eax,-68(%ebp)
	leal 4(%esp),%esp
	jmp .L362
.L367:
	movl -8(%ebp),%eax
	subl -100(%ebp),%eax
	leal 2(%eax),%eax
	pushl %eax
	call cgeti
	movl %eax,-68(%ebp)
	movl (%eax),%edx
	movl %edx,4(%eax)
	leal 4(%esp),%esp
	cmpl $0,-24(%ebp)
	jne .L369
	movl $2,-20(%ebp)
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jle .L374
.L373:
	movl -68(%ebp),%edi
	movl -20(%ebp),%edx
	movl -56(%ebp),%esi
	movl -100(%ebp),%ecx
	movl (%esi,%ecx,4),%ebx
	movl %ebx,(%edi,%edx,4)
	incl -100(%ebp)
	incl -20(%ebp)
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jg .L373
	jmp .L374
.L369:
	movl -56(%ebp),%ecx
	movl -100(%ebp),%edx
	movl (%ecx,%edx,4),%esi
	movl %esi,-88(%ebp)
	incl -100(%ebp)
	movl $32,%edx
	subl -24(%ebp),%edx
	movl %edx,-92(%ebp)
	movl %esi,%eax
	movl -24(%ebp),%ecx
	shrl %cl,%eax
	movl %esi,%edx
	movl -92(%ebp),%ecx
	shll %cl,%edx
	movl %edx,-28(%ebp)
	testl %eax,%eax
	je .L375
	movl -68(%ebp),%edx
	movl %eax,8(%edx)
	movl $1,%eax
	jmp .L376
.L375:
	movl -68(%ebp),%edx
	movl (%edx),%ecx
	decl %ecx
	movl %ecx,4(%edx)
	leal 4(%edx),%ecx
	movl %ecx,-68(%ebp)
	addl $4,avma
	movl (%ecx),%edx
	movl %edx,4(%ecx)
	xorl %eax,%eax
.L376:
	leal 2(%eax),%edx
	movl %edx,-20(%ebp)
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jle .L374
	movl $32,%edx
	subl -24(%ebp),%edx
	movl %edx,-104(%ebp)
.L380:
	movl -56(%ebp),%ecx
	movl -100(%ebp),%edx
	movl (%ecx,%edx,4),%esi
	movl %esi,-88(%ebp)
	movl %esi,%eax
	movl -24(%ebp),%ecx
	shrl %cl,%eax
	movl -68(%ebp),%ecx
	movl -20(%ebp),%edx
	addl -28(%ebp),%eax
	movl %eax,(%ecx,%edx,4)
	movl %esi,%edx
	movl -104(%ebp),%ecx
	shll %cl,%edx
	movl %edx,-28(%ebp)
	incl -100(%ebp)
	incl -20(%ebp)
	movl -8(%ebp),%edx
	cmpl -100(%ebp),%edx
	jg .L380
.L374:
	movl -68(%ebp),%edx
	movl 4(%edx),%eax
	andl $16777215,%eax
	movl -32(%ebp),%edx
	sall $24,%edx
	movl %edx,-88(%ebp)
	movl -68(%ebp),%edx
	addl -88(%ebp),%eax
	movl %eax,4(%edx)
.L362:
	cmpl $-1,16(%ebp)
	jne .L381
	pushl -68(%ebp)
	jmp .L392
.L381:
	cmpl $0,16(%ebp)
	jne .L382
	pushl -64(%ebp)
.L392:
	pushl -96(%ebp)
	pushl -4(%ebp)
	call gerepile
	jmp .L289
.L382:
	pushl $0
	pushl -96(%ebp)
	pushl -4(%ebp)
	call gerepile
	andl $-4,%eax
	movl 16(%ebp),%edx
	movl -68(%ebp),%ecx
	addl %eax,%ecx
	movl %ecx,(%edx)
	movl -64(%ebp),%edx
	leal (%eax,%edx),%eax
.L289:
	leal -116(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret
.comm in_saved_avma,4
