; Test simulator program						-*- asm -*-

$processor	84
$watchdog	on
$pwrup		on
$protect	off
$clock		rc, 400000
$idwords	1, 2, 3, 4

		RTCC	equ		1
		Opt		equ		1
		Pcl		equ		2
		Status	equ		3
		Fsr		equ		4
		TrisA	equ		5
		PortA	equ		5
		TrisB	equ		6
		PortB	equ		6
		Intcon	equ		0x0b
		SP		equ		0x0c
		FP		equ		0x0d
		GR0		equ		0x0e
		GR1		equ		0x0f

		C		equ		0				; STATUS bit numbers
		DC		equ		1
		Z		equ		2
		PD		equ		3
		TO		equ		4
		RP0		equ		5
		RP1		equ		6
		IRP		equ		7

		W		equ		0
		F		equ		1

;;;
;;;----------------------------------------------------------------------
;;; 
		org		0
		goto	Entry

		org		4
		goto	Interrupt
		
Entry:	btfss	Status,TO
		goto	WDTReset
		btfss	Status,PD
		goto	MCLRWakeup

PowerUp:movlw	0x30			; Set up SP
		movwf	SP
		movwf	FP

		bsf		Status,RP0
		clrw
		movwf	TrisA
		movwf	TrisB
		movlw	0b11001000
		movwf	Opt
		bcf		Status,RP0
		
		movlw	1
		call	push
		movlw	2
		call	push
		movlw	3
		call	push
		movlw	4
		call	push
		nop
		call	add16
		nop
		call	pop
		call	pop
		call	pop
		call	pop

		movlw	1
spin0:	movwf	PortA
		clrwdt
spin1:	rlf		PortA,F
		btfss	PortA,4
		goto	spin1
		clrf	PortA
		movwf	PortB
spin2:	rlf		PortB,F
		btfss	PortB,7
		goto	spin2
		clrf	PortB
		goto	spin0
		
		sleep

WDTReset:
		goto	WDTReset

MCLRWakeup:
		goto	MCLRWakeup

Interrupt:
		goto	Interrupt

add16:	clrw
		call	fentry

		movlw	4
		call	index
		movf	0,W
		movwf	GR0
		movlw	3
		call	index
		movf	0,W
		movwf	GR1
		movlw	2
		call	index
		movf	0,W
		addwf	GR0,F
		btfsc	STATUS,C
		incf	GR1,F
		movlw	1
		call	index
		movf	0,W
		addwf	GR1,F

		goto	fexit

push:	movwf	GR0				; *--SP = W
		decf	SP,F
		movf	SP,W
		addlw	-(GR1)
		btfsc	Status,Z
		goto	StackOverflow
		movf	SP,W
		movwf	Fsr
		movf	GR0,W
		movwf	0
		return
StackOverflow:
		goto	StackOverflow

pop:	movf	SP,W			; return *SP++
		movwf	Fsr
		movf	0,W
		incf	SP,F
		return

fentry:	movwf	GR1				; push FP, FP = SP, SP -= W
		movf	FP,W			; This depends on push not using GR1!
		call	push
		movf	SP,W
		movwf	FP
_fent:	movf	GR1,F		
		btfsc	Status,Z
		return
		call	push
		goto	_fent

fexit:	movf	FP,W
		movwf	Fsr
		movf	0,W
		movwf	FP
		movf	Fsr,W
		movwf	SP
		incf	SP,F
		return

index:	addwf	FP,W			; Set Fsr to point to FP[W]
		movwf	Fsr
		return

		end
