; ****************************************************************************
; rd8086.s (rmdir.asm) - by Erdogan Tan - 12/03/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - rmdir -- unlink directory
;
; [ Last Modification: 16/03/2022 ]
;
; Derived from (original) UNIX v5 'rmdir.s' (PDP-11 assembly) source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org/cgi-bin/utree.pl?file=V5)
; v5root.tar.gz
; ****************************************************************************
; [ v5root.tar - usr/source/s2/rmdir.s (directory archive date: 27-11-1974) ]
;
; ****************************************************************************
; Assembler: NASM 2.15
;	nasm rd8086.s -l rd8086.lst -o rd8086.bin -Z error.txt	

; Modified from: rmdir1.s (Retro UNIX 386 v1) source code by Erdogan Tan
;	         12/03/2022	

; 12/01/2022 (Retro UNIX 386 v1.2)
; 13/10/2015

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
_sleep	equ 34 ; Retro UNIX 8086 v1 feature only !
_msg    equ 35 ; Retro UNIX 386 v1 feature only !
_sleep	equ 34 ; Retro UNIX 8086 v1 feature only !
_msg	equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

; 16/03/2022 (sys macro, retro unix 8086, nasm version)

%macro sys 1-4
    ; Retro UNIX 8086 v1 system call.
    %if %0 >= 2   
        mov bx, %2
        %if %0 >= 3
            mov cx, %3
            %if %0 >= 4
               mov dx, %4
            %endif
        %endif
    %endif
    mov ax, %1
    int 20h
%endmacro

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 16] ; 16-bit (x86 real mode) intructions

[ORG 0]

START_CODE:
	;pop	ebp ; mov (sp)+,r5
	;pop	eax ; tst (sp)+
	; ebp = argument count
	; eax = pointer to argument 1 (executable file name)
	pop	bp
	pop	ax

	; temporary - 12/03/2022
	; print test message (if argument count = 1)
	;mov	eax, ebp
	;dec	eax
	mov	ax, bp
	dec	ax
	jnz	short _loop

	;sys	_msg, test_msg, 255, 0Fh

	mov	si, test_msg
	;call	strlen

strlen:
	; si = asciiz string address
	mov	di, si 
	dec	di
nextchr:
	inc	di
	cmp	byte [di], 0
	ja	short nextchr
	;cmp	[di], 0Dh
	;ja	short nextchr
	sub	di, si
	; di = asciiz string length
	;retn

	sys	_write, 1, si, di
	
_loop:	; loop:
	;dec	ebp		; dec r5
	dec	bp
	;;jle	done		; ble done
	;jg	short rm_1
	jnz	short rm_1
done:
	sys	_exit		; sys exit
;hang:
;	nop
;	nop
;	jmp	short hang

rm_1:
	;pop	esi		; mov (sp)+,r1
	; esi = ptr to (next) argument
	pop	si
	;mov	edi, name	; mov $name,r2
	; edi = dir name (path) addr
	mov	di, name
	;sub	ecx, ecx	; clr r0
	sub	cx, cx
rm_2:	; 1:
	;inc	ecx		; inc r0
	inc	cx
	lodsb			; movb (r1)+,(r2)+
	stosb
	or	al, al
	jnz	short rm_2	; bne 1b
	;dec	edi		; dec r2
	;dec	ecx		; dec r0
	dec	di
	dec	cx
	;mov	[size], ecx	; mov r0,size
	mov	[size], cx
	;cmp	edi, name	; cmp r2,$name
	cmp	di, name
	je	short error	; beq error
	sys	_stat, name, stbuf 
				; sys stat; name; stbuf
	jc	short error	; bes error

	; -- unix v5-v7 inode --
	; clear mode bits except bit13 & bit14 (unix v5 inode)
	;	bit15 = IALLOC : allocated flag
	; 	bit14 = IFDIR  : directory flag
	;  	bit13 = IFCHR  : (char) device/special flag
	;	bit14|bit13 = IFBLK : block device flag
	
		; bic $!60000,stbuf+4 
				; and word [stbuf+4], ~6000h 

	; -- unix v5-v7 inode --
	; check	if it is directory, jump to error if not  
		; cmp $40000,stbuf+4  
				; cmp word [stbuf+4], 4000h
		; bne error
				; jne short error		

	; -- Retro UNIX 8086/386 v1 & v1.1 inode --
	;
	; Note:	a regular file's inode number must be > 40
	;	((otherwise it is device file))
	;	mode/flag word:	
	;	bit 15 = allocated flag (must be 1)
	;	bit 14 = directory flag
	;
	;	Root directory inode number = 41
	
	cmp	word [stbuf], 41 ; is inode number > 41 ?	
	jna	short error ; (no, it can not be removed!)		
	;
	;mov	al, [stbuf+3] ; high byte of mode word
	;mov	ah, 0C0h ; allocated (80h) & directory (40h)
	;and	al, ah	 ; and al, 0C0h	
	;xor	ah, al		; cmp $40000,stbuf+4
	;jnz	short error	; bne error
	;
	test	byte [stbuf+3], 40h ; test directory flag
	jz	short error	; (not a directory!)

	; -- Retro UNIX 386 v1.2 --
	;
	;	bit 15 = regular file flag (must be 1)
	;	bit 14 = directory flag
	;	bit 13 = reserved flag (or char special)	
	;
	;	(bit14&bit15 must be 1)

	;mov	al, [stbuf+3] ; high byte of mode word
	;mov	ah, 0C0h ; regular file (80h) & dir (40h)
	;and	al, ah	 ; and al, 0C0h
	;			; bic $!60000,stbuf+4	
	;xor	ah, al		; cmp $40000,stbuf+4
	;jnz	short error	; bne error	
	
	; check if the last char of dir name is a dot
	;cmp	byte [edi-1], '.' ; cmpb -1(r2),$'.
	cmp	byte [di-1], '.'
	jne	short rm_3	  ; bne	1f

	; '.'  = current directory (can not be deleted)
	;cmp	edi, name+1	; cmp r2,$name+1
	cmp	di, name+1
	;je	short error 	; beq error
	jna	short error

	; '/.' = root directory (can not be deleted)
	;mov	al, [edi-2]
	mov	al, [di-2]
	cmp	al, '/'
	;cmp	byte [edi-2], '/' ; cmpb -2(r2),$'/
	je	short error	  ; beq	error

	; '..' = parent directory (can not be deleted)	
	cmp	al, '.'
	;cmp	byte [edi-2], '.' ; cmpb -2(r2),$'.
	jne	short rm_3	  ; bne 1f
	;cmp	edi, name+2	; cmp r2,$name+2
	cmp	di, name+2
	;je	short error	; beq error
	jna	short error

	; '/..' = parent directory (can not be deleted)	
	;cmp	byte [edi-3], '/' ; cmpb -3(r2),$'/
	cmp	byte [di-3], '/'
	;je	short error	  ; beq error
	jne	short rm_3

error:
	call	prname		; jsr pc,prname
	;mov	eax, 1		; mov $1,r0
	;sys	_write, eax, mes2, emes2-mes2
	sys	_write, 1, mes2, (emes2-mes2) - 1 
				; sys write; mes2; emes2-mes2
	jmp	_loop	; br loop

rm_3:	; 1:
	; open file -directory- for read
	sys	_open, name, 0	; sys open; name; 0
	jc	short error	; bes error
	; save file descriptor/number
	;;mov	esi, eax	; mov r0,r1
	;mov	[fd], eax
	mov	[fd], ax
rm_4:	; 1:
	;mov	eax, esi	; mov r1,r0
	; -- Retro UNIX 386 v1.1 & v1.2 --
	; read 16 bytes (read directory entry)
	;;sys	_read, eax, stbuf, 16
	;sys	_read, [fd], stbuf, 16
				; sys read; stbuf; 16.
	;jc	short rm_5	; bes 1f
	
	; -- Retro UNIX 8086/386 v1.0 --
	; read 10 bytes (read directory entry)
	;sys	_read, eax, stbuf, 10
	sys	_read, [fd], stbuf, 10
	jc	short rm_5	

	; check read count (eof)
	;or	eax, eax	; tst r0
	or	ax, ax
	jz	short rm_5	; beq 1f
	
	; check inode number and dir entry name
	;mov	eax, [stbuf]  ; 1st 4 byte of dir entry
	mov	ax, [stbuf]
	and	ax, ax ; empty (null/deleted) entry ?
	;cmp	word [stbuf], 0	 ; tst stbuf
	jz	short rm_4 ; yes ; beq 1b
	;je	short rm_4
	;
	; First 2 dir entries are '.' and '..'
	; If there is another directory entry,
	; that means the directory is not empty.
	;
	;shr	eax, 16
	mov	ax, [stbuf+2]
	cmp	al, '.'
	;cmp	byte [stbuf+2], '.' 
				; cmpb stbuf+2,$'.
	jne	short error1	; bne error1
	or	ah, ah
	;cmp	byte [stbuf+3], 0 ; tstb stbuf+3
	;je	short rm_4	  ; beq 1b
	jz	short rm_4
	cmp	ah, '.'
	cmp	byte [stbuf+3], '.' 
				; cmpb stbuf+3,$'.
	jne	short error1	; bne error1
	cmp	byte [stbuf+4], 0 ; tstb stbuf+4
	;je	short rm_4	; beq 1b
	jna	short rm_4

error1:
	call	prname		; jsr pc,prname

	;;mov	ebx, eax	; mov r1,r0
	;;sys	_close		; sys close
	;sys	_close, eax
	sys	_close, ax

	;mov	eax, 1		; mov $1,r0
	;sys	_write, eax, mes1, emes1-mes1
	sys	_write, 1, mes1, (emes1-mes1)-1
		; sys write; mes1; emes1-mes1
	;jmp	_loop
		; br loop
	jmp	short rm_6 ; jmp _loop

rm_5:	; 1:
	;mov	ebx, esi	; mov r1,r0
	; ebx = file number
	sys	_close		; sys close
	mov	al, '/'
	stosb			; movb $'/,(r2)+
	mov	al, '.'
	stosb			; movb $'.,(r2)+
	stosb			; movb $'.,(r2)+
	;sub	al, al ; 0 
	;;mov	[edi], al	; clrb (r2)
	;mov	byte [edi], 0
	mov	byte [di], 0

	; delete '..' (2nd, dotdot) entry of the directory
	sys	_unlink, name	; sys unlink; name
	;dec	edi
	dec	di
	;mov	byte [edi], 0	; clrb -(r2)
	mov	byte [di], 0

	; delete '.' (1st, dot) entry of the directory
	sys	_unlink, name	; sys unlink; name
	;dec	edi
	;dec	edi
	;mov	byte [edi], 0	; clrb -2(r2)
	dec	di
	dec	di
	mov	byte [di], 0

	; delete the directory's itself
	; (as a dir entry of it's parent diry)
	sys	_unlink, name	; sys unlink; name
	; ((cf=1 -> permission denied error or another error.))
	jc	short rm_7
rm_6:	
	jmp	_loop		; br loop
rm_7:
	jmp	error

;error:
;	call	prname		; jsr pc,prname
;	;mov	eax, 1		; mov $1,r0
;	;sys	_write, eax, mes2, emes2-mes2
;	sys	_write, 1, mes2, emes2-mes2 
;				; sys write; mes2; emes2-mes2
;	;jmp	_loop		; br loop
;	jmp	short rm_6	; jmp _loop

prname:
	sys	_write, 1, nl, 2 ; next line (CRLF)	

	;mov	eax, 1		; mov $1,r0
	;sys	_write, eax, name, [size]
	sys	_write, 1, name, [size]
				; sys write; name; size:..
	retn			; rts pc

;done:
;	sys	_exit		; sys exit

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;size:	dd 0 ; directory (path) name size (byte count)
;fd:	dd 0 ; file descriptor/num (of the dir to be deleted) 

; ----------------------------------------------------------------

test_msg:
	db  0Dh, 0Ah
	db  'RMDIR by Erdogan TAN - 16/03/2022'
nl:
	db  0Dh, 0Ah, 0
mes1:
	db  ' -- directory not empty', 0Dh, 0Ah, 0 
		;	< -- directory not empty\n>
emes1:
mes2:
	db  ' ?', 0Dh, 0Ah, 0 ;	< ?\n>
emes2:
	; .even

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 2

bss_start:

ABSOLUTE bss_start

;size:	resd 1 ; directory (path) name size (byte count)
size:	resw 1
;fd:	resd 1 ; file descriptor/num (of the dir to be deleted) 
fd:	resw 1

name:	resb 40 ; name:	 .=.+40.      ; dir (path) name buffer	
stbuf:	resb 40	; stbuf: .=.+40.      ; sysstat output buffer
;stbuf:	resb 66	; Retro UNIX 386 v1.2 ; (66 byte sysstat data)

; 12/03/2022
;-----------------------------------------------------------------
; Original UNIX v5 - rmdir (utility) source code (rmdir.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;/ rmdir -- unlink directory
;
;	mov	(sp)+,r5
;	tst	(sp)+
;
;loop:
;	dec	r5
;	ble	done
;	mov	(sp)+,r1
;	mov	$name,r2
;	clr	r0
;1:
;	inc	r0
;	movb	(r1)+,(r2)+
;	bne	1b
;	dec	r2
;	dec	r0
;	mov	r0,size
;	cmp	r2,$name
;	beq	error
;	sys	stat; name; stbuf
;	bes	error
;	bic	$!60000,stbuf+4
;	cmp	$40000,stbuf+4
;	bne	error
;	cmpb	-1(r2),$'.
;	bne	1f
;	cmp	r2,$name+1
;	beq	error
;	cmpb	-2(r2),$'/
;	beq	error
;	cmpb	-2(r2),$'.
;	bne	1f
;	cmp	r2,$name+2
;	beq	error
;	cmpb	-3(r2),$'/
;	beq	error
;1:
;	sys	open; name; 0
;	bes	error
;	mov	r0,r1
;1:
;	mov	r1,r0
;	sys	read; stbuf; 16.
;	bes	1f
;	tst	r0
;	beq	1f
;	tst	stbuf
;	beq	1b
;	cmpb	stbuf+2,$'.
;	bne	error1
;	tstb	stbuf+3
;	beq	1b
;	cmpb	stbuf+3,$'.
;	bne	error1
;	tstb	stbuf+4
;	beq	1b
;
;error1:
;	jsr	pc,prname
;	mov	r1,r0
;	sys	close
;	mov	$1,r0
;	sys	write; mes1; emes1-mes1
;	br	loop
;
;1:
;	mov	r1,r0
;	sys	close
;	movb	$'/,(r2)+
;	movb	$'.,(r2)+
;	movb	$'.,(r2)+
;	clrb	(r2)
;	sys	unlink; name
;	clrb	-(r2)
;	sys	unlink; name
;	clrb	-2(r2)
;	sys	unlink; name
;	br	loop
;
;error:
;	jsr	pc,prname
;	mov	$1,r0
;	sys	write; mes2; emes2-mes2
;	br	loop
;
;prname:
;	mov	$1,r0
;	sys	write; name; size:..
;	rts	pc
;
;done:
;	sys	exit
;
;mes1:
;	< -- directory not empty\n>
;emes1:
;mes2:
;	< ?\n>
;emes2:
;	.even
;
;.bss
;name:	.=.+40.
;stbuf:	.=.+40.
;