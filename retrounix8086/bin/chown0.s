; ****************************************************************************
; chown386.s (chown0.s) - by Erdogan Tan - 29/04/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - chown - change (file's) owner
;
; [ Last Modification: 30/04/2022 ]
;
; Derived from (original) UNIX v2 (v1) 'chown.s' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; svntree-20081216.tar.gz
; ****************************************************************************
; [ unix72/src/cmd/chown.s (archive date: 16-12-2008) ]

; Modified from:
; 	chown1.s - 30/04/2022 - (Retro UNIX 386 v1 & v1.1 % v1.2)

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

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

;%macro sys 1-4
;   ; 03/09/2015	
;   ; 13/04/2015
;   ; Retro UNIX 386 v1 system call.
;   %if %0 >= 2   
;       mov ebx, %2
;       %if %0 >= 3    
;           mov ecx, %3
;           ;%if %0 = 4
;           %if	%0 >= 4 ; 11/03/2022
;		mov edx, %4   
;           %endif
;       %endif
;   %endif
;   mov eax, %1
;   int 30h	   
;%endmacro

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

;; Retro UNIX 386 v1 system call format:
;; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

;; 11/03/2022
;; Note: Above 'sys' macro has limitation about register positions;
;;	ebx, ecx, edx registers must not be used after their
;;	positions in sys macro.
;; for example:
;;	'sys _write, 1, msg, ecx' is defective, because
;;	 ecx will be used/assigned before edx in 'sys' macro.
;; correct order may be:
;;	'sys _write, 1, msg, eax ; (eax = byte count)

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

; ----------------------------------------------------------------------------

[BITS 16] ; 16-bit intructions (8086/8088 - Real Mode)

[ORG 0] 

START_CODE:
	; 30/04/2022 - Retro UNIX 8086 v1
	;	32 bit to 16 bit conversion
	;	-----
	;	eax, edx, ecx, ebx -> ax, dx, cx, bx
	;	esi, edi, ebp, esp -> si, di, bp, sp
	;	register+4 -> register+2
	;	dword values on stack -> word values on stack
	;
	; 30/04/2022
	; 29/04/2022

	;mov	sp,r5
	;mov	(r5),r4
	;cmp	r4,$3
	;bge	1f
	;jsr	r5,mesg; <chown uid f1 ...\n\0>; .even

	pop	ax ; ax = number of arguments

	;cmp	ax, 3
	cmp	al, 3
	jnb	short chown_1

	mov	ax, usage_msg
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

chown_1:
;1:
	;add	$4,r5
	;mov	(r5),r3
	;cmpb	(r3),$'0
	;blt	1f
	;cmpb	(r3),$'9
	;bgt	1f
	;jsr	r5,cvnum; geta
	;br	do

	;mov	[argc], ax
	mov	[argc], al ; argument count

	;xor	cx, cx
	pop	ax ; argument 0 - (exec) file name
	pop	si ; argument 1 - (new) owner
	mov	al, [si]
	cmp	al, '0'
	jb	short chown_2 ; (new) owner name
	cmp	al, '9'
	ja	short chown_2 ; (new) owner name
	; (new) owner number
	mov	bx, geta ; get user num from user input
	call	cvnum
	jmp	short do

chown_2:
;1:
	;mov	$uids,r0
	;jsr	r5,fopen; ubuf
	;bec	1f
	;jsr	r5,mesg; <Can't open /etc/uids\n\0>; .even
	;sys	exit

	sys	_open, uids, 0
	jnc	short chown_3

	mov	ax, cant_open_msg
	jmp	short write_msg_and_exit

chown_3:
;1:
	;mov	r3,r2
	; 30/04/2022
	mov	[ubuf], ax  ; file descriptor
chown_4:
	; get user name 
	;  (it is 1st word of a row in '/etc/passwd' file,
	;   from 1st character in row to ':' character)

	mov	di, si
chown_5:
;2:
	;jsr	r5,getc; ubuf
	;bcc	3f

	;mov	bx, ubuf
	call	getc
	jnc	short chown_6
who:
	;jsr	r5,mesg; <Who?\n\0>; .even
	;sys	exit
	
	mov	ax, who_msg
write_msg_and_exit:
	call	print_msg
closefile_and_exit:
	mov	bx, [ubuf] ; 30/04/2022
	sys	_close
	sys	_exit

;hangemhigh:
;	nop
;	jmp	short hangemhigh

chown_6:
;3:
	;cmp	r0,$':
	;beq	3f
	;cmpb	(r2)+,r0
	;beq	2b
	
	cmp	al, ':'
	je	short chown_8
	scasb	; cmp al, [di] .. inc di
	je	short chown_5	

chown_7:
;2:
	;jsr	r5,getc; ubuf
	;bcs	who
	;cmp	r0,$'\n
	;bne	2b
	;br	1b

	;mov	bx, ubuf
	call	getc
	jc	short who

	; (there is CR character at the end of each row
	; in '/etc/passwd' file)

	cmp	al, EnterKey ; cmp al, 0Dh
	jne	short chown_7 ; continue/pass (until CR)
	;jmp	short chown_4 ; next row (in '/etc/passwd')
	; 30/04/2022
	; get linefeed
	call	getc
	;jc	short who
	;(line feed check is not necessary if 'etc/passwd')
	;cmp	al, 0Ah ; line feed (lf byte of crlf)
	;je	short chown_4
	;mov	di, si
	;jmp	short chown_6
	jmp	short chown_4
chown_8:
;3:
	;tstb	(r2)
	;bne	2b

	test	byte [di], 0FFh
	jnz	short chown_7
chown_9:
	; user name is ok (matches with chown input)
;3:
	;jsr	r5,getc; ubuf
	;cmpb	r0,$':
	;bne	3b
	;jsr	r5,cvnum; getc

	;mov	bx, ubuf
	call	getc
	cmp	al, ':'
	jne	short chown_9

	; get user number (which is in /'etc/passwd')
	mov	bx, getc ; get user num from '/etc/passwd'
	call	cvnum
do:
	;sub	$2,r4
	;mov	r1,0f+2
	;tst	(r5)+

	; [sp] = argument 2 = file name
	; cx = user number
	;	(<= 255 for current Retro UNIX version) 
	
	;sub	dword [argc], 2
	sub	byte [argc], 2
chown_10:
;1:
	;mov	(r5)+,0f
	;sys	chown; 0:..; 0
	;bec	2f
	;mov	0b,r0
	;mov	r0,0f
	;clr	0f+2

	pop	si ; argument 2 = file name
		   ; (and argument 3 .. argument 4)
	; cx = user number
	sys	_chown, si
	jnc	short chown_13

	xor	cx, cx ; 0
chown_11:	
;3:
	;tstb	(r0)+
	;beq	3f
	;inc	0f+2
	;br	3b

	; strlen (calculating file name length)
	dec	cl ; max. 255 bytes (may be 64 to 70)
	mov	di, si ; file name address
	sub	al, al ; 0
	repne	scasb
		; do:
		; and cx cx
		; jz short dont	
		; cmp [di], al
		; jne short dont
		; inc di
		; dec cx
		; jmp short do
		; dont:
	sub	di, si
	dec	di ; 30/04/2022
	; di = file name length
chown_12:
;3:
	;mov	$1,r0
	;sys	write; 0:..; ..
	;jsr	r5,mesg; <?\n\0>; .even

	; 30/04/2022
	mov	ax, nextline
	call	print_msg

	sys	_write, 1, si, di
	mov	ax, qu_msg
	call	print_msg

chown_13:
;2:
	;dec	r4
	;bgt	1b
	;sys	exit

	;dec	word [argc]
	dec	byte [argc]
	jg	short chown_10
	jmp	closefile_and_exit

print_msg:
	; 30/04/2022 
	; (32 -> 16 bit buffer and register size
	;	 modifications for Retro UNIX 8086 v1)
	; 29/04/2022 
	; Modified registers: ax, bx, cx, dx
strlen:
	; ax = asciiz string address
	mov	bx, ax
	dec	bx
nextchr:
	inc	bx
	cmp	byte [bx], 0
	ja	short nextchr
	;cmp	[bx], 0Dh
	;ja	short nextchr
	sub	bx, ax
	; bx = asciiz string length
	;retn
print_str:
	mov	dx, bx
	sys	_write, 1, ax
	;
	retn

cvnum:
	; 30/04/2022 
	; (32 -> 16 bit buffer and register size
	;	 modifications for Retro UNIX 8086 v1)
	; 30/04/2022
	; 29/04/2022

	;clr	r1

	; INPUT:
	;    bx = getc address (*)
	;      or geta address (**)
	; OUTPUT:
	;    cx = (user) number

	; Modified registers: cx, ax, dx, bx
	; 		and.. si (**) or bp (*)

	xor	cx, cx ; 0
	mov	[function], bx
cvn_1:
;1:
	;jsr	r5,*(r5); ubuf
	;bcs	1f
	;sub	$'0,r0
	;cmp	r0,$9.
	;bhi	1f
	;mpy	$10.,r1
	;add	r0,r1
	;br	1b

	call	word [function] ; 30/04/2022
	jc	short cvn_2
	; al = 0 
	sub	al, '0'  ; AL-30h
	;jb	short cvn_2
	; AL-30h > 9 for AL values less than 30h
	cmp	al, 9
	ja	short cvn_2
	push	ax
	mov	ax, 10
	mul	cx
	pop	cx
	add	cx, ax	
	jmp	short cvn_1
cvn_2:
;1:
	;tst	(r5)+
	;rts	r5

	retn
geta:
	;movb	(r3)+,r0
	;tst	(r5)+
	;rts	r5

	lodsb ; mov al, [si] .. inc si
	retn

getc:
	; 30/04/2022 
	; (32 -> 16 bit buffer and register size
	;	 modifications for Retro UNIX 8086 v1)
	; 30/04/2022
	; 29/04/2022
	
	; INPUT:
	;    ubuf = read buffer (header) address
	; OUTPUT:
	;    al = character (if cf=0)
	;    (if cf = 1 -> read error)

	; Modified registers: ax, bx, cx, dx, bp	

	mov	bp, ubuf
	mov	ax, [bp+2] ; char count
	;and	ax, ax
	and	ax, ax
	jnz	short gch1
gch0:
	mov	bx, [bp]
	mov	cx, ubuf+6 ; read buff. (data) addr.
	mov 	[bp+4], cx ; char offset
	;mov	[bp+2], ax ; 0
	sub	dx, dx
	mov	dh, 2 
	;mov 	dx, 512 
	sys	_read ; sys _read, bx, cx, dx
	jc	short gch2
	or	ax, ax
	;jz	short gch3
	jnz	short gch1
	;
	stc
	retn
gch1:
	dec	ax
	mov	[bp+2], ax
	mov	bx, [bp+4]
	xor	ah, ah
	mov	al, [bx]
	inc	bx
	mov	[bp+4], bx
	;retn 	
gch2:
	;xor	ax, ax
	retn
;gch3:
	;stc
	;retn
	
;
;uids:	</etc/passwd\0>
;
;	.bss
;ubuf:	.=.+518.

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;align 4

uids:	db "/etc/passwd", 0 ; password file (contains user IDs)

qu_msg:	; <?\n>

	db " ?"
nextline:
	db 0Dh, 0Ah, 0

usage_msg:
	db 0Dh, 0Ah 
	db "Usage: chown uid f1 ...", 0Dh, 0Ah, 0
cant_open_msg:
	db 0Dh, 0Ah
	db "Can't open /etc/uids", 0Dh, 0Ah, 0
who_msg:
	db " - Who ? ", 0Dh, 0Ah, 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

; 30/04/2022 (Size modifications for Retro UNIX 8086 v1)

function:
	resw 1	; (is used by 'cvnum')

ubuf:	resb 6	; Buffer header
		;  word - file descriptor
		;  word - character count
		;  word - character offset
	resb 512 ; Buffer data

;argc:	resw 1
argc:	resb 1	; argument count

; 29/04/2022

;-----------------------------------------------------------------
; Original UNIX v2 - /bin/chown source code (chown.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;
;/ chown -- change owner
;
;
;	.globl	fopen, getc, mesg
;
;	mov	sp,r5
;	mov	(r5),r4
;	cmp	r4,$3
;	bge	1f
;	jsr	r5,mesg; <chown uid f1 ...\n\0>; .even
;1:
;	add	$4,r5
;	mov	(r5),r3
;	cmpb	(r3),$'0
;	blt	1f
;	cmpb	(r3),$'9
;	bgt	1f
;	jsr	r5,cvnum; geta
;	br	do
;1:
;	mov	$uids,r0
;	jsr	r5,fopen; ubuf
;	bec	1f
;	jsr	r5,mesg; <Can't open /etc/uids\n\0>; .even
;	sys	exit
;1:
;	mov	r3,r2
;2:
;	jsr	r5,getc; ubuf
;	bcc	3f
;who:
;	jsr	r5,mesg; <Who?\n\0>; .even
;	sys	exit
;3:
;	cmp	r0,$':
;	beq	3f
;	cmpb	(r2)+,r0
;	beq	2b
;2:
;	jsr	r5,getc; ubuf
;	bcs	who
;	cmp	r0,$'\n
;	bne	2b
;	br	1b
;3:
;	tstb	(r2)
;	bne	2b
;3:
;	jsr	r5,getc; ubuf
;	cmpb	r0,$':
;	bne	3b
;	jsr	r5,cvnum; getc
;do:
;	sub	$2,r4
;	mov	r1,0f+2
;	tst	(r5)+
;1:
;	mov	(r5)+,0f
;	sys	chown; 0:..; 0
;	bec	2f
;	mov	0b,r0
;	mov	r0,0f
;	clr	0f+2
;3:
;	tstb	(r0)+
;	beq	3f
;	inc	0f+2
;	br	3b
;3:
;	mov	$1,r0
;	sys	write; 0:..; ..
;	jsr	r5,mesg; <?\n\0>; .even
;2:
;	dec	r4
;	bgt	1b
;	sys	exit
;
;cvnum:
;	clr	r1
;1:
;	jsr	r5,*(r5); ubuf
;	bcs	1f
;	sub	$'0,r0
;	cmp	r0,$9.
;	bhi	1f
;	mpy	$10.,r1
;	add	r0,r1
;	br	1b
;1:
;	tst	(r5)+
;	rts	r5
;
;geta:
;	movb	(r3)+,r0
;	tst	(r5)+
;	rts	r5
;
;uids:	</etc/passwd\0>
;
;	.bss
;ubuf:	.=.+518.
