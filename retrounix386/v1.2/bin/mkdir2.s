; ****************************************************************************
; mkdir386.s (mkdir2.s) - by Erdogan Tan - 10/03/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - makdir -- make a directory
;
; [ Last Modification: 11/03/2022 ]
;
; Derived from (original) UNIX v5 'mkdir.s' (PDP-11 assembly) source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org/cgi-bin/utree.pl?file=V5)
; v5root.tar.gz
; ****************************************************************************
; [ v5root.tar - usr/source/s2/mkdir.s (directory archive date: 27-11-1974) ]
;
; Assembler: NASM v2.15
; ((nasm mkdir2.s -l mkdir2.txt -o mkdir2 -Z error.txt))
;
; mkdir1.s - 11/03/2022 - Retro UNIX 386 v1 & v1.1 (unix v1 inode)
; mkdir2.s - 11/03/2022 - Retro UNIX 386 v1.2 (& v2) (modified unix v7 inode)

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
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_sysver	equ 39 ; (get) Retro Unix 386 version

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

%macro sys 1-4
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.		
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            ;%if %0 = 4
            %if	%0 >= 4 ; 11/03/2022
		mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    int 30h	   
%endmacro

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

; 11/03/2022
; Note: Above 'sys' macro has limitation about register positions;
;	ebx, ecx, edx registers must not be used after their
;	positions in sys macro.
; for example:
;	'sys _write, 1, msg, ecx' is defective, because
;	 ecx will be used/assigned before edx in 'sys' macro.
; correct order may be:
;	'sys _write, 1, msg, eax ; (eax = byte count)

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	sys	_getuid		; sys getuid ; get user id
	;mov	[uid], eax
	mov	[uid], al	; mov r0,uid

	mov	esi, esp	; mov sp,r5		
	lodsd	; number of arguments  ; tst (r5)+
	;mov	[args], al	; -- (sp) -- dword [esp] --

	; temporary - 11/03/2022
	; print test message (if argument count = 1)
	dec	eax
	jnz	short _loop
	sys	_msg, test_msg, 255, 0Fh

_loop:	; loop:
	lodsd	; (next) argument's addr in eax ; tst (r5)+
	;dec	byte [args]
	dec	dword [esp]	; dec (sp)
	;jg	short md_1	; bgt 1f
	jnz	short md_1
	sys	_exit		; sys exit
;hlt:
;	nop
;	nop
;	jmp	short hlt

md_1:	; 1:
	mov	ebx, [esi]	; mov (r5),r0		
	mov	ebp, ebx	; -- (r5) -- new sub dir name
	mov	edi, buf1	; mov $buf1,r1	
	mov	edx, buf2	; mov $buf2,r2
	sub	ecx, ecx	; clr r3
	mov	ah, '/'
md_2:	; 1:
      	mov	al, [ebx]	; movb (r0)+,r4
	or	al, al
	jz	short md_3	; beq 2f
	stosb		  	; movb r4,(r1)+	
	mov	[edx], al	; movb r4,(r2)+	
	inc	edx
	inc	ebx
	;cmp	al, '/'	 	; directory separator
	cmp	al, ah		; cmpb r4,$'/
	jne	short md_2	; bne 1b
		; save position of the last dir seperator 
	mov	ecx, edx	; mov r2,r3
	jmp	short md_2	; br 1b
md_3:	; 2:
	;mov	al, '/'
	mov	al, ah
	stosb		  	; movb $'/,(r1)+
	mov	al, '.'	  	; movb $'.,(r1)+ 
	stosb
	sub	al, al ; 0
	mov	[edi], al 	; clrb (r1) 	
	mov	dword [dir], dot ; mov $dot,dir
	and	ecx, ecx	; tst r3	
	jz	short md_4	; beq 1f
	mov	dword [dir], buf2 ; mov	$buf2,dir
	mov	[ecx], al ; 0	; clrb (r3)
	cmp	ecx, buf2+1	; cmp r3,$buf2+1
	je	short md_4	; beq 1f
	dec	ecx
	mov	[ecx], al ; 0 	; clrb -(r3) / ???
md_4:	; 1:
	;test	word [uid], 0FFFFh ; (Retro UNIX 386 v1.2)
	; (Retro UNIX 386 v1.0 & v1.1)
	test	byte [uid], 0FFh ; tstb uid ; (pdp-11 unix v5)
	jz	short md_6 	; beq 2f ; root (super user)
	sys	_stat, [dir], stbuf
		; sys stat; dir:..; stbuf / status of parent dir
	jc	short error	; bes error

	; inode mode/flags (from sysstat output)
	; (Retro UNIX 386 v1 & v1.1 &v1.2)
	mov	ax, [stbuf+2]	; mov stbuf+4,r0
				; (pdp-11 unix v5)
	; get owner's uid (from sysstat output)
	mov	bx, [stbuf+6]	; (Retro UNIX 386 v1.2)
	;mov	bl, [stbuf+5]	; (Retro UNIX 386 v1.0 & v1.1)
	;cmp	bx, [uid]	; (Retro UNIX 386 v1.2)
	; (Retro UNIX 386 v1.0 & v1.1)
	cmp	bl, [uid]	; cmpb uid,stbuf+7 ; (unix v5)
	jne	short md_5	; bne 1f
	; owner's r/w/e permissions
	;shr	ax, 6		; ash $-6,r0
				; ...
				; unix v5 inode mode r/w/e bits:
				; IEXEC = 40h (execute, owner)
				;  bit3 = 08h (execute, group)
				;  bit0 = 01h (execute, others)
				; IWRITE = 80h (write, owner)
				;  bit4 =  10h (write, group)
				;  bit1 =  02h (write, others)
				; IREAD = 100h (read, owner)
				;  bit5 =  20h (read, group)
				;  bit2 =  04h (read, others)
				;
	;shr	al, 2		; Retro UNIX 386 v1 & v1.1
				; unix v1 inode mode r/w/e bits:
				; bit 0 = write perm, others, 1
 	  			; bit 1 = read perm, others, 2
				; bit 2 = write perm, owner, 4
				; bit 3 = read perm, owner, 8
				; bit 4 = executable file, 16
				;
	shr	ax, 6		; Retro UNIX 386 v1.2 (& v2)
				; unix v7 inode mode r/w/e bits:
				; bit 0 = exec perm, others, 1
 	  			; bit 1 = write perm, others, 2
				; bit 2 = read perm, owner, 4
				; bit 3 = exec perm, group, 8
				; bit 4 = write perm, group, 16
				; bit 5 = read perm, group, 32
				; bit 6 = exec perm, owner, 64
				; bit 7 = write perm, owner, 128
				; bit 8 = read perm, owner, 256
				; ...
md_5:
	;ror	al, 1		; ror r0 ; (bit 6 to cf)
	;ror	al, 1		; ror r0 ; (bit 7 to cf)
	;;ror	al, 2		; bit 7 to cf (for unix v5/v7)				
	; (Retro UNIX 386 v1 & v1.1)
	;ror	al, 1		; bit 2 to cf (for unix v1)
	; (Retro UNIX 386 v1.2 & v2)
	ror	al, 2		; bit 7 to cf
	jnc	short error	; bcc error
				; / no write permission in parent
md_6:
	;mov	ebx, ebp	; mov (r5),0f
	sys	_mkdir, ebp, 0C1FFh ; Retro UNIX 386 v1.2
		; sys makdir; 0:..; 140777; 0
		; ...
		; 0C1FFh = 140777 (octal) = 1100000111111111b
		;	bit15  = allocated flag/bit
		;	bit14  = directory flag/bit
		;	bit8 to bit0 = r/w/e permissions

	;sys	_mkdir, ebp, 0C00Fh ; Retro UNIX 386 v1 & v1.1	
	
		; Retro UNIX 386 v1: (& v1.1)
		;	mkdir mode setting: 1100000000001111b 
		;	(unixcopy.com sett: 1100000000001110b)
		;	bit15 = allocated flag
		;	bit14 = directory flag
		;	bit13 = file modified flag (not used)
		;	bit12 = large file flag
		;	bit11 to bit6 are not used
		;	bit5  = setuid on execution flag 	
		;	bit4  = executable file flag
		;	bit3 to bit0 = r/w/e permissions
		;
		; Retro UNIX 386 v1.2: (& v2) 
		;	mkdir mode setting: 1100000111111111b 
		;	(unixcopy/unixhdcp: 1100000111101101b)
		;	bit15 = regular file (not device) flag
		;	bit14 = directory flag
		;	bit13 = reserved flag (or char special)
		;	bit12 = large file flag
		;	bit11 = setuid on execution flag
		;	bit10 = setgid on execution flag 
		;	bit9  = use extents flag (not used)
		;	bit8 to bit0 = r/w/e permissions	 

	jc	short error ; bes error	; / prob already exists
	
	;mov	ebx, ebp	 ; mov (r5),0f
	;movzx	ecx, byte [uid] 
	;sys	_chown, ebp
	sys	_chown, ebp, [uid] ; sys chown; 0:..; uid:..
	;mov	ebx, ebp	 ; mov (r5),0f
	sys	_link, ebp, buf1 ; sys link; 0:..; buf1
	jc	short error	 ; bes error
	mov	al, '.'
	stosb			 ; movb $'.,(r1)+
	sub	al, al ; 0
	mov	[edi], al	 ; clrb (r1)
	;mov	ebx, [dir]	 ; mov dir,0f
	sys	_link, [dir], buf1 ; sys link; 0:..; buf1
	jc	short error
	jmp	_loop ;jnc _loop ; bec loop

error:
	;sys	_msg, nl, 2, 07h
	sys	_write, 1, nl, 2
	
	mov	edx, ebp	; mov (r5),r0
				; mov r0,0f
				; clr 0f+2
err1:	;1:
	test	byte [edx], 0FFh ; tstb	(r0)+
	jz	short err2	 ; beq 1f
	inc	edx		; inc 0f+2
	jmp	short err1	; br 1b
err2:	; 1:
	sub	edx, ebp ; count
	jz	short err3
	;mov	ebx, 1		; mov $1,r0
	;;;sys	_write, ebx, ebp, edx
	;;sys	_write, 1, ebp, edx
				; sys write; 0:..; ..
	sys	_write, 1, ebp
	;sys	_msg, ebp, edx, 0Ch
err3:
	;mov	ebx, 1		; mov $1,r0
	sys	_write, 1, ques, 4 ; (SPACE+'?'+CR+LF)
	;sys	_write, 1, ques, 3 ; (' ?'+CR)
				; sys write; ques; 3
	;sys	_msg, ques, 4, 0Ch

	jmp	_loop		; br loop

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;args:	db 0
uid:	dd 0
;uid:	db 0
dir:	dd 0

; ----------------------------------------------------------------

test_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 386 v2 MKDIR by Erdogan TAN - 11/03/2022'
nl:
	db  0Dh, 0Ah, 0

dot:	db '.', 0
ques:	db ' ?', 0Dh, 0Ah

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 2

bss_start:

ABSOLUTE bss_start

buf1:	resb 100 ; new sub directory name/path buffer (make)
buf2:	resb 100 ; parent directory name/path buffer (perms)
;stbuf:	resb 40 ; sysstat output buffer
stbuf:	resb 66 ; for Retro UNIX 386 v1.2 (66 byte sysstat data)

; 10/03/2022
;-----------------------------------------------------------------
; Original UNIX v5 - mkdir (utility) source code (mkdir.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;/ makdir -- make a directory
;
;	sys	getuid
;	mov	r0,uid
;	mov	sp,r5
;	tst	(r5)+
;
;loop:
;	tst	(r5)+
;	dec	(sp)
;	bgt	1f
;	sys	exit
;1:
;	mov	(r5),r0
;	mov	$buf1,r1
;	mov	$buf2,r2
;	clr	r3
;1:
;	movb	(r0)+,r4
;	beq	2f
;	movb	r4,(r1)+
;	movb	r4,(r2)+
;	cmpb	r4,$'/
;	bne	1b
;	mov	r2,r3
;	br	1b
;2:
;	movb	$'/,(r1)+
;	movb	$'.,(r1)+
;	clrb	(r1)
;	mov	$dot,dir
;	tst	r3
;	beq	1f
;	mov	$buf2,dir
;	clrb	(r3)
;	cmp	r3,$buf2+1
;	beq	1f
;	clrb	-(r3)	/ ???
;1:
;	tstb	uid
;	beq	2f
;	sys	stat; dir:..; stbuf	/ status of parent dir
;	bes	error
;	mov	stbuf+4,r0
;	cmpb	uid,stbuf+7
;	bne	1f
;	ash	$-6,r0
;1:
;	ror	r0
;	ror	r0
;	bcc	error			/ no write permission in parent
;2:
;	mov	(r5),0f
;	sys	makdir; 0:..; 140777; 0
;	bes	error			/ prob already exists
;	mov	(r5),0f
;	sys	chown; 0:..; uid:..
;	mov	(r5),0f
;	sys	link; 0:..; buf1
;	bes	error
;	movb	$'.,(r1)+
;	clrb	(r1)
;	mov	dir,0f
;	sys	link; 0:..; buf1
;	bec	loop
;
;error:
;	mov	(r5),r0
;	mov	r0,0f
;	clr	0f+2
;1:
;	tstb	(r0)+
;	beq	1f
;	inc	0f+2
;	br	1b
;1:
;	mov	$1,r0
;	sys	write; 0:..; ..
;	mov	$1,r0
;	sys	write; ques; 3
;	br	loop
;
;dot:	<.\0>
;ques:	< ?\n>
;	.even
;
;.bss
;buf1:	.=.+100.
;buf2:	.=.+100.
;stbuf:	.=.+40.
;