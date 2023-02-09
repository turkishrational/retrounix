; ****************************************************************************
;
; CAT.ASM  (Retro Unix 8086 v1 - /bin/cat - concatenate files)
; ----------------------------------------------------------------------------
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; Retro UNIX 8086 v1 - /bin/cat file
;
; [ Last Modification: 16/07/2015 ]
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
;
; ****************************************************************************
;
; Derived from 'cat.s' file of original UNIX v1
;
; CAT2.ASM, 16/07/2015
; CAT1.ASM, 02/12/2013
;
; ****************************************************************************

.8086

; UNIX v1 system calls
_rele 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_stty 	equ 31
_gtty	equ 32
_ilgins	equ 33

;;;

sys macro syscallnumber, arg1, arg2, arg3

    ; Retro UNIX 8086 v1 system call.

    ifnb <arg3> 	
      mov dx, arg3
    endif
    
    ifnb <arg2> 	
      mov cx, arg2
    endif

    ifnb <arg1> 	
      mov bx, arg1
    endif
     			
    mov ax, syscallnumber	
    int 20h	
   
    endm

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

;ibuf	equ offset bss
;obuf	equ offset bss + 512
;fin	equ offset bss + 1024
; 16/07/2015
iobuf	equ offset bss
fin	equ offset bss + 512	

UNIX   	SEGMENT PUBLIC 'CODE'
        assume cs:UNIX,ds:UNIX,es:UNIX,ss:UNIX

START_CODE:
	;; / cat -- concatinate files

	sys	_write, 1, nl, 2

	pop	bp
	pop	dx
	mov	si, fin
        ;mov     di, obuf
        dec     bp
	jnz	short @f
	;AX = 2 (written byte count)
	xor	al, al
	jmp	short cat_3
		;;mov	(sp)+,r5
		;;tst	(sp)+
		;;mov	$obuf,r2
		;;cmp	r5,$1
		;;beq	3f
		;;
cat_1:	
	;;loop:
	dec	bp
	;jz	short cat_6
	; 16/07/2015
	jnz	short @f
	sys	_exit
@@:	
	pop	bx
	cmp	byte ptr [BX], '-'
	jne	short cat_2
	xor	ax, ax ; 0
	mov	word ptr [SI], ax ;0
	jmp	short cat_3
		;;dec	r5
		;;ble	done
		;;mov	(sp)+,r0
		;;cmpb	(r0),$'-
		;;bne	2f
		;;clr	fin
		;;br	3f
cat_2:
	;;2:
	; bx = file name offset
	xor 	cx, cx ; 0
	sys 	_open
	jc	short cat_1
	mov	word ptr [SI], ax
		;;mov	r0,0f
		;;sys	open; 0:..; 0
		;;bes	loop
		;;mov	r0,fin
cat_3:
	;;3:
        sys     _read, ax, iobuf, 512 ; 16/07/2015
	;sys 	_read, ax, ibuf, 512 
	jc	short cat_5
	; NOTE: If input file is a tty (keyboard)
	;	only 1 byte will be read, by ignoring
	;	byte count (512).
	;	Retro UNIX 8086 v1 kernel ('rtty')
	;	has been modified fot that.
	;       Erdogan Tan (16/07/2015)
	;
	and	ax, ax ; AX = 1 for tty (keyboard)
	jz	short cat_5
;	push	si
	;mov	si, ibuf
;	mov	si, cx ; offset ibuf
;	mov	cx, ax
		;;mov	fin,r0
		;;sys	read; ibuf; 512.
		;;bes	3f
		;;mov	r0,r4
		;;beq	3f
		;;mov	$ibuf,r3
	 ; 16/07/2015
;	mov	dx, ax
;	;add	dx, obuf
;cat_4:
;	;;4:
;	lodsb
	;call	putc
	; 16/07/2015
	sys 	_write, 1, iobuf, ax
	jc	short cat_5
	;
;	loop	cat_4
;	pop	si
@@:
	mov	ax, word ptr [SI]
	jmp	short cat_3
		;;movb	(r3)+,r0
		;;jsr	pc,putc
		;;dec	r4
		;;bne	4b
		;;br	3b
cat_5:
	;;3:
	mov	bx, word ptr [SI]
	or	bx, bx
	jz	short cat_1
	sys 	_close
	jmp	short cat_1
		;;mov	fin,r0
		;;beq	loop
		;;sys	close
		;;br	loop
	;;
;cat_6:
;	;;done:
;	sub	di, obuf
;	jz	short cat_7
;	sys	_write, 1, obuf, di 
		;;sub	$obuf,r2
		;;beq	1f
		;;mov	r2,0f
		;;mov	$1,r0
		;;sys	write; obuf; 0:..
;cat_7:	
;	;;1:
;	sys	_exit
		;;sys	exit
	;;
;putc:	
;	;;putc:
;	stosb
;	cmp	di, dx ; 16/07/2015
	;cmp	di, obuf + 512
;	jb	short @f
;	push	cx
	 ; 16/07/2015
;	mov	di, obuf
;	sub	dx, di ; byte (char) count
	; 
;	sys 	_write, 1, obuf
	;sys 	_write, 1, obuf, 512
	;mov	di, obuf
		;;movb	r0,(r2)+
		;;cmp	r2,$obuf+512.
		;;blo	1f
		;;mov	$1,r0
		;;sys	write; obuf; 512.
		;;mov	$obuf,r2
;	pop	cx
;@@:	
	;;1:
;	retn
		;;rts	pc

nl:	db 0Dh, 0Ah, 0

EVEN

bss:
;ibuf:	db 512 dup(0)
;obuf:	db 512 dup(0)
;fin:	dw 0

		;;.bss
		;;ibuf:	.=.+512.
		;;obuf:	.=.+512.
		;;fin:	.=.+2
		;;.text


UNIX     	ends

                end     START_CODE
