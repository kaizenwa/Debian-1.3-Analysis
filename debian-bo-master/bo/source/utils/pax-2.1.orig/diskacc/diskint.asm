;DISKINT.ASM

        .MODEL  SMALL,C
        .CODE

diskint PROC    service:BYTE, drive:BYTE, head:BYTE, track:BYTE, sector:BYTE, nsects:BYTE, buffer:FAR PTR BYTE

        mov     ah,service
        mov     al,nsects
        mov     ch,track
        mov     cl,sector
        mov     dh,head
        mov     dl,drive
        les     bx,buffer

        int     13h

	ret

diskint ENDP

        END
