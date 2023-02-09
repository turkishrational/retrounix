; ****************************************************************************
; login386.s (login2.s) - Retro Unix 386 v1 - /bin/login - enter new user
; ----------------------------------------------------------------------------
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; [ Last Modification: 27/02/2022 ]
;
; Derived from 'login03.asm' source code file of 'Retro UNIX 8086 v1'
; operating system project, /bin/login source code by Erdogan Tan
; (07/11/2013-27/06/2014)
;
; Derived from 'login.s' file of original UNIX operating system
; (v1.0 for PDP-11)
; ****************************************************************************
; LOGIN03.ASM (07/11/2013-27/06/2014, Retro UNIX 8086 v1, MASM 6.11, 16 bit)
; login386.s (13/10/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit)
; login0.s (17/11/2015) - Retro UNIX 386 v1
; login1.s (24/01/2022-27/02/2022) - Retro UNIX 386 v1.2
; login2.s (16/02/2022-27/02/2022) - Retro UNIX 386 v1
; login3.s (27/02/2022) - Retro UNIX 386 v1.1

; 13/10/2015

; Assembler: NASM 2.11
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
_msg	equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !

%macro sys 1-4
    ; Retro UNIX 386 v1 system call.		
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    int 30h	   
%endmacro

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; from 'sysexec' system calls
	;  (stack pointer -esp- points to 
	; to the head of arguments list which is 
	; on top the stack, backward from 'ecore'.) 
	; esp = offset argc (argument count)
	;

	sys 	_quit, 0
	sys 	_intr, 0

	; 18/02/2022 (bss section is cleaned by the kernel)
	;; Clear bss section (Clear uninitialized data area) 
	;; 14/10/2015 (Retro UNIX 386 v1)
	;xor	eax, eax ; 0
	;mov	ecx, (bss_end - bss_start)/4
	;mov	edi, bss_start	
	;rep	stosd  

	call 	ttyn
	;eax = 000000??h
	mov	[ttyx+8], al
	cmp	al, 'x' ; not found ?
	je	short S1
	sub	al, '0'
	jz	short S1
	;shl	ax, 4
	; 16/02/2022
	shl	eax, 4
	mov	[s_off], eax ; (offset for sysseek)
S1:
	pop	edx ; argument count
	pop	eax ; pointer to argument 0
		   ; executable file name	
	dec	edx ; dec dl
	jng	short login ; 16/11/2015
	pop	esi ; pointer to argument 1
		    ; user name
	mov	edi, uname
	mov	ebx, edi
	add	ebx, 8
S2:
	lodsb
	stosb
	and 	al, al
	jz 	short S3
	cmp	edi, ebx
	jb	short S2
S3:
	dec	edx
	jz	short login
	pop	esi
	mov	edi, passwd	
S4:
	lodsb
	stosb
	or 	al, al
	jz 	short login
	cmp	edi, passwd + 8
	jb	short S4	
login:
	mov	byte [ebx], 0 ;  uname + 8
	mov	eax, passwdf
	call	fopen
	jnc	short lg0
	mov	esi, msgNoPswdf
	call	mesg
	sys	_exit
lg0:
	call 	guname
lg1:
	mov	esi, uname
	call	compar
	je	short lg3 ; zf = 1 --> match
lg2:
	;mov	ebx, pbuf
	call	getc
        ;jc	sorry
	; 16/02/2022
	jc	short lg20 ; jmp sorry
	cmp	al, 0Dh ; \n
	jne	short lg2
	call	getc
	;jc	short sorry
	;cmp	al, 0Ah
	;jne	short sorry
	jmp	short lg1
lg3:
	call 	getc
        ;jc	sorry
	; 16/02/2022
	jnc	short lg21
lg20:
	jmp	sorry
lg21:
	cmp	al, ':'
	je	short lg4
	; 16/02/2022
	;push	ax	
	push	eax
	call	gpasswd
	;mov	esi, _word
	; 16/02/2022
	pop	eax
	;pop	ax
	mov	ah, [esi]
	cmp	al, ah
	jne	short sorry
	inc	esi
	; ESI = offset _word + 1 
	call	compar
	jne	short sorry
lg4:
	; get UID
	xor	ecx, ecx ; 0
	; 18/02/2022
	xor	edx, edx
lg5:
	;push	ecx
	push	edx ; 16/02/2022
	call	getc
	;jc	short sorry ; 16/02/2022
	cmp	al, ':'
	je	short lg6
	; 18/02/2022
	;mov	cl, al
	;sub 	cl, '0'
	sub	al, '0'
	;
	;xor	ch, ch
	;pop	edx
	;mov	ax, 10
	;mul	dx
	;add	cx, ax	
	; 16/02/2022
	;xor	eax, eax
	pop	edx
	; 18/02/2022
	or	edx, edx
	jnz	short lg23
	mov	dl, al
	jmp	short lg5	
lg23:
	mov	cl, al ; 18/02/2022
	mov	al, 10	
	mul	edx
	mov	dl, cl
	add	edx, eax
	jmp	short lg5
lg6:
	pop	ecx ; UID	
	sys	_chown, ttyx ; ecx = arg 2
	mov	[uid], cx
lg7:
	call	getc
	;jc	short sorry ; 16/02/2022
	cmp	al, ':'
	jne	short lg7    ; / skip ident field
	mov	edi, dirbuf
lg8:
	call	getc
	cmp	al, ':'
	je	short lg9
	stosb
	jmp	short lg8  	
lg9:
	xor	al, al
	stosb
	sys	_chdir, dirbuf
	jnc	short lg10
	mov	esi, msgNoDir
	call	mesg
        ;jmp	short sorry

sorry:
	mov	esi, msgIL
	call	mesg
	; 14/10/2015
	movzx 	ebx, word [pbuf]
	sys	_close
	; 16/02/2022
	;xor	al, al
	;mov	[uname], al
	;mov	[passwd], al
	; 14/10/2015
	mov 	ebx, uname
        ; 18/02/2022
	;mov 	byte [ebx], 0
	mov	byte [passwd], 0
	jmp     login

lg10:
	mov	ebx, uname + 7
lg11:
	cmp	byte [ebx], 0
	ja	short lg12
	mov	byte [ebx], 20h
	dec 	ebx
	jmp	short lg11
lg12:
	mov	esi, ttyx + 8
	cmp	byte [esi], 'x'
	;je	lg14
	; 16/02/2022
	jne	short lg22
	jmp	lg14
lg22:
	sys	_open, utmp, 1
	jc	short lg13
        mov     edi, eax
        mov     eax, [s_off]
        sys     _seek, edi, eax, 0
	mov	al, [esi]
	mov	[uname+8], al
	sys	_time
	mov 	[uname+10], eax
	sys	_write, edi, uname, 16
	sys	_close, edi  	
lg13:
	;cmp	byte [esi], 'x'
	;je	short lg14
	sys	_open, wtmp, 1
	jc	short lg14
	mov	edi, eax
	sys	_seek, edi, 0, 2
	sys	_write, edi, uname, 16
	sys	_close, edi 
lg14:
	call	getc
	cmp	al, 0Dh ; \n
	;je	short lg16
	; 25/02/2022
	jna	short lg16
	mov	edi, shell
lg15:
	; 25/02/2022 (BugFix)
	;mov	al, ah  ; (bug!)
	;
	stosb
	call	getc
	cmp	al, 0Dh	; \n
	;jne	short lg15
	; 25/02/2022
	ja	short lg15
	xor	al, al ; 0
	stosb
lg16:
	movzx	ebx, word [pbuf]
	sys	_close
	mov	eax, motd
	call	fopen
	jc	short lg18
lg17:
	call	getc
	jc	short lg18
	mov	[uname], al
	sys	_write, 1, uname, 1
	jmp	short lg17
lg18:
	movzx	ebx, word [pbuf]
	sys	_close
	sys	_stat, mailf, pbuf
	jc	short lg19
	mov	ax, [pbuf+6] ; file size 
	and	ax, ax
	jz	short lg19
	mov	esi, msgMail
	call 	mesg
lg19:
	movzx	ebx, word [uid]
	sys	_setuid
	sys     _exec, shell, shellp
	mov	esi, msgNoSh
	call 	mesg
        sys     _exit

gpasswd:
	mov	edi, passwd
	cmp	byte [edi], 1
	jnb	short gp2
	mov	esi, msgPswd
	call	mesg
gp1:
	call 	tgetc
	cmp	al, 08h
	je	short gp3
	cmp	al, 127
	je	short gp3
	stosb
	and	al, al
	jz	short gp2
	mov	byte [chr], '*'
	; 16/02/2022
	;call 	tputc
	cmp	edi, passwd + 9
	;jb	short gp1
	jb	short gp4 ; 16/02/2022
	dec	edi
	jmp	short gp1

gp2:
	mov	esi, passwd
	;call	crypt
	;;mov	esi, _word
	;retn
	; 16/02/2022
	jmp	crypt

gp3:	; Backspace 
	; (Retro UNIX 8086 v1 modification)
	cmp	edi, passwd
	jna	short gp1
	;mov 	byte [chr], 08h
	call	tputbs
	jmp	short gp1
gp4:
	; 16/02/2021
	call	tputc
	jmp	short gp1

guname:
	mov	edi, uname
	cmp	byte [edi], 1
	jnb	short gun2
	xor	eax, eax ; mov eax, 0
	stosd
	stosd
	mov	esi, msgName
	call	mesg
	mov	edi, uname
gun1:
	call	tgetc
	cmp	al, 08h
	je	short gun3
	cmp	al, 127
	je	short gun3
	stosb
	and 	al, al
	jz	short gun2
	; 16/02/2022
	;call 	tputc
	cmp	edi, uname + 9
	;jb	short gun1
	jb	short gun4 ; 16/02/2022
	dec	edi
	jmp	short gun1
gun2:
	retn

gun3:	; Backspace 
	; (Retro UNIX 8086 v1 modification)
	cmp	edi, uname
	jna	short gun1
	;mov 	byte [chr], 08h
	call	tputbs
	jmp	short gun1
gun4:
	; 16/02/2022
	call	tputc
	jmp	short gun1

compar:
	; ESI = uname or _word 
		; (encrypted passwd)
	;mov	ebx, pbuf
cmp_0:
	call	getc
	jnc	short cmp_1
	pop	eax
        jmp     sorry
cmp_1:
	mov	ah, al
	; AH = character
	lodsb
	cmp	al, ah
	je	short cmp_0
	and	al, al
	jnz	short cmp_2
	cmp	ah, ':'
cmp_2:
	;ZF = 1 --> match
	retn

tgetc:
	sys	_read, 0, chr, 1
	; 16/02/2022
	and	eax, eax
	;and	ax, ax
	jnz	short tgc1
	sys	_exit
tgc1:
	mov	al, [chr]
	cmp	al, 0Dh
	jne	short tgc2
	xor	al, al
tgc2:
	retn

tputbs:
	mov	byte [chr], 08h
	dec	edi
	; 16/02/2022
	call	tputc
	mov	byte [chr], 20h ; space/blank
	call	tputc
	mov	byte [chr], 08h ; backspace
tputc:
	sys	_write, 1, chr, 1
	retn	

mesg:
	mov	edx, esi
msg1:	
	lodsb
	and 	al, al
	jnz 	short msg1
	sub	esi, edx
	xchg	esi, edx
	; edx = string length
	sys	_write, 1, esi 
	retn

; return name of current tty
;
ttyn:
	push	edi
	push	esi
	push	edx
	mov	byte [ttyname], 'x'
	sys	_fstat, 1, buf ; get tty file status
			       ; file descriptor = 1 
			       ; (standard output) 
	jc	short er1
	sys	_open, dev, 0
	jc	short er1
	;
	mov	si, [buf]
	mov	edi, eax	
t1:
	sys	_read, edi, buf, 10
	jc	short er
	cmp	eax, 10
	jne	short er
	;mov	dx, [buf]	; inode number from sysfstat
	;cmp	dx, si 		; same inode number ?
	; 27/02/2022
	cmp	si, [buf]	; same inode number ?
	jne	short t1	; no, get next dir entry
	;mov	dx, [buf+2]	; check (8 byte) file name
	;cmp	dx, 'tt'	; 1st 2 chars
	; 27/02/2022
	cmp	word [buf+2], 'tt' ; First 2 characters
	jne	short er	; not 'tt'
	mov	dx, [buf+4]	; 2nd 2 chars
	cmp	dl, 'y'		; check the 3rd char
	jne	short er	; not 'y'
	;or	dh, dh
	;jz	short er
	cmp	dh, '0'		; < 'tty0'
	jb	short er	; yes
	cmp	dh, '9'		; > 'tty9'
	ja	short er	; yes
	cmp	byte [buf+6], 0
	jne	short er
	mov	byte [ttyname], dh
er:
	sys	_close, edi
er1:
	movzx	eax, byte [ttyname]
	pop	edx
	pop	esi
	pop	edi
	retn

; open a file for use by get(c|w)
;
fopen:
	; eax = file name ofset
	mov	edi, pbuf
	sys 	_open, eax, 0
	jc	short f1
	stosw
	xor	eax, eax ; 0
	stosw
	retn
f1:
	mov	ax, 0FFFFh
	stosw	
	retn

; get characters from input file
;
getc:
	push	esi
	mov	esi, pbuf
	; 16/02/2022
	;mov	ax, [esi+2]
	movzx	eax, word [esi+2] ; char count
	;and	ax, ax
	and	eax, eax
	jnz	short gch1
gch0:
	movzx	ebx, word [esi]
	mov	ecx, pbuf + 8 ; read buff. addr.
	mov 	[esi+4], ecx ; char offset
	;mov	[esi+2], ax ; 0
	; 16/02/2022
	sub	edx, edx
	mov	dh, 2 
	;mov 	edx, 512 
	sys	_read ; sys _read, ebx, ecx, edx
	jc	short gch2
	or	eax, eax
	jz	short gch3
gch1:
	;dec	ax
	; 16/02/2022
	dec	eax
	mov	[esi+2], ax
	mov	ebx, [esi+4]
	; 16/02/2022
	;xor	eax, eax
	xor	ah, ah
	mov	al, [ebx]
	inc	ebx
	mov	[esi+4], ebx
	;xor	ah, ah
	pop	esi
	retn 	
gch2:
	;xor	ax, ax
	; 16/02/2022
	xor	eax, eax
gch3:
	pop	esi
	stc
	retn

;/ crypt -- password incoding
;
;; Original Unix v5 (PDP-11) 'crypt'
;; code has been converted to 
;; Retro UNIX 8086 v1 'crypt' 
;; procedure in 'login.asm'
;; (by Erdogan Tan - 12/11/2013).
; 
;
;crypt:
;	mov	r1,-(sp)
;	mov	r2,-(sp)
;	mov	r3,-(sp)
;	mov	r4,-(sp)
;	mov	r5,-(sp)
;
;	mov	r0,r1
;	mov	$key,r0
;	movb	$004,(r0)+
;	movb	$034,(r0)+

; 14/10/2015 - 32 bit version (Retro UNIX 386 v1)

crypt:
	;mov	esi, passwd
	mov	edi, key
	mov	al, 4
	stosb
	mov	al, 28
	stosb

;1:
;	cmp	r0,$key+64.
;	bhis	1f
;	movb	(r1)+,(r0)+
;	bne	1b
;1:
;	dec	r0

cryp0:
	lodsb
	stosb
	and	al, al
	jz	short cryp1
	cmp	edi, key + 64
	jb	short cryp0
cryp1:
 	dec	edi
;/
;/
;/	fill out key space with clever junk
;/
;	mov	$key,r1
;1:
;	movb	-1(r0),r2
;	movb	(r1)+,r3
;	xor	r3,r2
;	movb	r2,(r0)+
;	cmp	r0,$key+128.
;	blo	1b


;/	fill out key space with clever junk

	mov	esi, key
cryp2:
	mov	bl, [edi-1]
	lodsb
	xor	al, bl
	stosb
	cmp	edi, key + 128
	jb	short cryp2
	;
;/
;/
;/	establish wheel codes and cage codes
;/
;	mov	$wheelcode,r4
;	mov	$cagecode,r5
;	mov	$256.,-(sp)
;2:
;	clr	r2
;	clr	(r4)
;	mov	$wheeldiv,r3
;3:
;	clr	r0
;	mov	(sp),r1
;	div	(r3)+,r0
;	add	r1,r2
;	bic	$40,r2
;	bis	shift(r2),(r4)
;	cmp	r3,$wheeldiv+6.
;	bhis	4f
;	bis	shift+4(r2),(r5)
;4:
;	cmp	r3,$wheeldiv+10.
;	blo	3b
;	sub	$2,(sp)
;	tst	(r4)+
;	tst	(r5)+
;	cmp	r4,$wheelcode+256.
;	blo	2b
;	tst	(sp)+
;/	

;/	establish wheel codes and cage codes

	mov	esi, wheelcode
	mov	edi, cagecode
	mov	ax, 256
	push	ax ; *
	mov	ebp, esp
cryp3:
	sub	dx, dx ; 0
	mov	[esi], dx ; 0
	mov	ebx, wheeldiv
cryp4:
	mov	ax, [ebp]	
	mov 	cl, [ebx]
	div	cl
	add	dl, ah
	inc	ebx
	and	dl, 01Fh
	push	ebx
	mov	ebx, shift
	add	ebx, edx
	mov	ax, [ebx] 
	or	[esi], ax
	pop	ecx
	cmp	ecx, wheeldiv + 3
	jnb	short cryp5
	add	ebx, 4
	mov	ax, [ebx]
	or	[edi], ax 	 
cryp5:
	mov	ebx, ecx
	cmp	ebx, wheeldiv + 5
	jb	short cryp4
	sub	word [ebp], 2
	lodsw
	inc	edi
	inc	edi
	cmp	esi, wheelcode + 256
	jb	short cryp3
	pop	ax ; *

;	.data
;shift:	1;2;4;10;20;40;100;200;400;1000;2000;4000;10000;20000;40000;100000
;	1;2
;wheeldiv: 32.; 18.; 10.; 6.; 4.
;	.bss
;cagecode: .=.+256.
;wheelcode: .=.+256.
;	.text
;/
;/
;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
;/
;	mov	$key,r0
;	mov	$cage,r2
;	mov	$wheel,r3
;1:
;	movb	(r0)+,r1
;	bic	$!177,r1
;	asl	r1
;	mov	cagecode(r1),(r2)+
;	mov	wheelcode(r1),(r3)+
;	cmp	r0,$key+128.
;	blo	1b

;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
cryp6:
        mov     ebx, key
        mov     esi, cage
        mov     edi, wheel
cryp7:
        mov     cl, [ebx]
        inc     ebx
        and     ecx, 7Fh
	shl	cl, 1
        xchg    ecx, ebx
        mov     ax, [ebx+cagecode]
        mov     [esi], ax
        inc     esi
        inc     esi
        mov     ax, [ebx+wheelcode]
	stosw
        mov     ebx, ecx
        cmp     ebx, key + 128
	jb	short cryp7

;/
;/
;/	now spin the cage against the wheel to produce output.
;/
;	mov	$word,r4
;	mov	$wheel+128.,r3
;3:
;	mov	-(r3),r2
;	mov	$cage,r0
;	clr	r5
;1:
;	bit	r2,(r0)+
;	beq	2f
;	incb	r5
;2:
;	cmp	r0,$cage+256.
;	blo	1b

;/
;/	now spin the cage against the wheel to produce output.
;/
cryp8:
        mov     edi, _word
        mov     ebx, wheel + 128
cryp9:
        dec     ebx
        dec     ebx
        mov     dx, [ebx]
        mov     esi, cage
	sub	cx, cx ; 0
cryp10:
	lodsw
	test	ax, dx
	jz	short cryp11
	inc	cl
cryp11:
        cmp     esi, cage + 256
	jb	short cryp10

;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
;	mov	r4,-(sp)
;	clr	r4
;	div	$26.+26.+10.,r4
;	add	$'0,r5
;	cmp	r5,$'9
;	blos	1f
;	add	$'A-'9-1,r5
;	cmp	r5,$'Z
;	blos	1f
;	add	$'a-'Z-1,r5
;1:
;	mov	(sp)+,r4
;	movb	r5,(r4)+
;	cmp	r4,$word+8.
;	blo	3b
;/
;
;	mov	(sp)+,r5
;	mov	(sp)+,r4
;	mov	(sp)+,r3
;	mov	(sp)+,r2
;	mov	(sp)+,r1
;	mov	$word,r0
;	rts	pc
;	.bss
;key:	.=.+128.
;word:	.=.+32.
;cage:	.=.+256.
;wheel:	.=.+256.

;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
	mov	ax, cx
	mov	dl, 26+26+10
	div	dl
	mov	al, ah
	add	al, '0'
	cmp	al, '9'
	jna	short cryp12
	add	al, 'A'-'9'-1
	cmp	al, 'Z'
	jna	short cryp12
	add	al, 'a'-'Z'-1
cryp12:
	stosb	
        cmp     edi, _word + 8
	jb	short cryp9
        mov     esi, _word
	retn


align 4
shellp:
	dd mshell
	dd 0
utmp:   db '/tmp/utmp'
        db 0
wtmp:   db '/tmp/wtmp'
        db 0
shell:	db '/bin/sh'
	db 0
shpl 	equ shell + 32 - shpad
shpad:  times shpl db 0

mshell: db '-'
	db 0
motd:   db '/etc/motd'
        db 0
mailf:  db 'mailbox'
        db 0
align 2
passwdf: db '/etc/passwd'
        db 0
ttyx:   db '/dev/tty' ; db '/dev/ttyx'
        db 0

dev:    db '/dev', 0

align 2
msgName:  db 0Dh, 0Ah, 'Name: ', 0
align 2
msgPswd:  db 0Dh, 0Ah, 'Password: ', 0
align 2
msgIL:	  db 0Dh, 0Ah, 'Login incorrect !', 0
;align 2
msgNoSh:  db 0Dh, 0Ah, 'No Shell !'
nextline: db 0Dh, 0Ah, 0
align 2
msgNoPswdf:
        db 0Dh, 0Ah, "Can't open password file !"
	db 0Dh, 0Ah, 0
align 2
msgNoDir:
	db 0Dh, 0Ah, 'No directory !'
	db 0Dh, 0Ah, 0
align 2
msgMail:
	db 0Dh, 0Ah, 'You have mail.'
	db 0Dh, 0Ah, 0

align 2
shift:	dw 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768
	dw 1, 2
wheeldiv: db 32, 18, 10, 6, 4

align 2	
	dw 417 ; 01A1h

align 4

bss_start:

ABSOLUTE bss_start

s_off:	resd 1

uname: 	resb 16
	resw 1
passwd: resb 8
	resw 1
dirbuf: resb 32
;shbuf: resb 32
;ttyb:  resb 6
uid:    resw 1
chr:	resw 1

buf:	resb 34
ttyname: resw 1

key:	resb 128
_word:	resb 10  ; resb 32
	resb 2
cage:	resb 256
wheel:	resb 256
cagecode:  resw 256
wheelcode: resw 256

alignb 4

pbuf:   resb 520
	
bss_end:

_end:  ; end of login386.s (NASM 2.11) source code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; login.s
;
;/  login --  enter new user
;
;.globl	ttyn
;.globl	crypt
;.globl	fopen
;.globl	getc
;.globl	mesg
;
;	sys	quit; 0
;	sys	intr; 0
;	jsr	pc,ttyn
;	movb	r0,ttyx+8.
;	sub	$'0,r0
;	cmp	r0,$'a-'0
;	blo	1f
;	sub	$'a-'0-10.,r0	/ map a-z into 10. on
;1:
;	asl	r0
;	asl	r0
;	asl	r0
;	asl	r0
;	mov	r0,offset
;	mov	(sp)+,r5
;	tst	(sp)+
;	dec	r5
;	ble	login
;	mov	(sp)+,r4
;	mov	$uname,r1
;2:
;	movb	(r4)+,(r1)+
;	bne	2b
;	dec	r5
;	ble	login
;	mov	(sp)+,r4
;	mov	$passwd,r1
;2:
;	movb	(r4)+,(r1)+
;	bne	2b
;login:
;	clrb	uname+8.
;	mov	$passwdf,r0
;	jsr	r5,fopen; pbuf
;	bec	1f
;	jsr	r5,mesg; <Can't open password file\n\0>; .even
;	sys	exit
;1:
;	jsr	pc,guname
;1:
;	jsr	r5,compar; uname
;		br .+4
;	br	2f
;3:
;	jsr	r5,getc; pbuf
;	bes	sorry
;	cmp	r0,$'\n
;	bne	3b
;	br	1b
;sorry:
;	jsr	r5,mesg; <Login incorrect\n\0>; .even
;	mov	pbuf,r0
;	sys	close
;	clr	uname
;	clr	passwd
;	br	login
;2:
;	jsr	r5,getc; pbuf
;	cmp	r0,$':
;	beq	2f
;	mov	r0,-(sp)
;	jsr	pc,gpasswd
;	cmpb	(r0)+,(sp)+
;	bne	sorry
;	mov	r0,0f
;	jsr	r5,compar; 0:..
;		br sorry
;2:
;	clr	r1
;2:
;	jsr	r5,getc; pbuf
;	cmp	r0,$':
;	beq	2f
;	mpy	$10.,r1
;	sub	$'0,r0
;	add	r0,r1
;	br	2b
;2:
;	mov	r1,0f
;	sys	chown; ttyx; 0:..
;	mov	r1,uid
;1:
;	jsr	r5,getc; pbuf
;	cmp	r0,$':
;	bne	1b			/ skip ident field
;	mov	$dirbuf,r1
;1:
;	jsr	r5,getc; pbuf
;	cmpb	r0,$':
;	beq	1f
;	movb	r0,(r1)+
;	br	1b
;1:
;	clrb	(r1)
;	sys	chdir; dirbuf
;	bec	1f
;	jsr	r5,mesg; <No directory\n\0>; .even
;	br	sorry
;1:
;	mov	$uname+8.,r1
;1:
;	tstb	-(r1)
;	bne	1f
;	movb	$' ,(r1)
;	br	1b
;1:
;	cmpb	ttyx+8.,$'x
;	beq	1f
;	sys	open; utmp; 1
;	bes	1f
;	mov	r0,r2
;	sys	seek; offset:..; 0
;	movb	ttyx+8.,uname+8.
;	sys	time
;	mov	r0,uname+10.
;	mov	r1,uname+12.
;	mov	r2,r0
;	sys	write; uname; 16.
;	mov	r2,r0
;	sys	close
;1:
;	cmpb	ttyx+8.,$'x
;	beq	1f
;	sys	open; wtmp; 1
;	bes	1f
;	mov	r0,r1
;	sys	seek; 0; 2
;	sys	write; uname; 16.
;	mov	r1,r0
;	sys	close
;1:
;	jsr	r5,getc; pbuf
;	cmp	r0,$'\n
;	beq	1f
;	mov	$shell,r1
;2:
;	movb	r0,(r1)+
;	jsr	r5,getc; pbuf
;	cmp	r0,$'\n
;	bne	2b
;	clrb	(r1)
;1:
;	mov	pbuf,r0
;	sys	close
;	mov	$motd,r0
;	jsr	r5,fopen; pbuf
;	bes	1f
;2:
;	jsr	r5,getc; pbuf
;	bes	1f
;	mov	r0,uname
;	mov	$1,r0
;	sys	write; uname; 1
;	br	2b
;1:
;	mov	pbuf,r0
;	sys	close
;	sys	stat; mailf; pbuf
;	bes	1f
;	tst	pbuf+6
;	beq	1f
;	jsr	r5,mesg; <You have mail\n\0>; .even
;1:
;	mov	uid,r0
;	sys	setuid
;	sys	exec; shell; shellp
;	jsr	r5,mesg; <No Shell\n\0>; .even
;	sys	exit
;
;gpasswd:
;	mov	$passwd,r1
;	tstb	(r1)
;	bne	3f
;	clr	r0
;	sys	gtty; ttyb
;	bic	$10,ttyb+4		/ turn off echo
;	clr	r0
;	sys	stty; ttyb
;	jsr	r5,mesg; <Password: \0>; .even
;2:
;	jsr	pc,tgetc
;	movb	r0,(r1)+
;	beq	1f
;	cmp	r1,$passwd+9.
;	blo	2b
;	dec	r1
;	br	2b
;1:
;	bis	$10,ttyb+4		/ turn on echo
;	clr	r0
;	sys	stty; ttyb
;	jsr	r5,mesg; <\n\0>; .even
;3:
;	mov	$passwd,r0
;	jsr	pc,crypt
;	clrb	8(r0)
;	rts	pc
;
;guname:
;	mov	$uname,r1
;	tstb	(r1)
;	bne	1f
;	clr	(r1)+
;	clr	(r1)+
;	clr	(r1)+
;	clr	(r1)+
;	mov	$uname,r1
;	jsr	r5,mesg; <Name: \0>; .even
;2:
;	jsr	pc,tgetc
;	movb	r0,(r1)+
;	beq	1f
;	cmp	r1,$uname+9.
;	blo	2b
;	dec	r1
;	br	2b
;1:
;	rts	pc
;
;compar:
;	mov	(r5)+,r4
;1:
;	jsr	r5,getc; pbuf
;	bes	2f
;	cmpb	r0,(r4)+
;	beq	1b
;	cmp	r0,$':
;	bne	1f
;	tstb	-(r4)
;	bne	1f
;	tst	(r5)+
;1:
;	rts	r5
;2:
;	tst	(sp)+
;	jmp	sorry
;
;tgetc:
;	clr	r0
;	sys	read; ch; 1
;	tst	r0
;	bne	1f
;	sys	exit
;1:
;	mov	ch,r0
;	cmp	r0,$'\n
;	bne	1f
;	clr	r0
;1:
;	rts	pc
;
;shellp:
;	mshell
;	0
;utmp:	</tmp/utmp\0>
;wtmp:	</tmp/wtmp\0>
;shell:	</bin/sh\0>; .=shell+32.
;mshell:<-\0>
;motd:	</etc/motd\0>
;mailf:	<mailbox\0>
;passwdf:</etc/passwd\0>
;ttyx:	</dev/ttyx\0>
;.even
;.bss
;uname: .=.+16.
;passwd:.=.+8.
;dirbuf:.=.+32.
;shbuf:	.=.+32.
;ttyb:	.=.+6
;uid:	.=.+2
;ch:	.=.+2
;pbuf:	.=.+518.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ttyn.s
;
;/ return name of current tty
;
;.globl	ttyn, _ttyn
;
;_ttyn:
;	mov	2(sp),r0
;	br	1f
;ttyn:
;	clr	r0
;1:
;	mov	$'x,name
;	tst	-(sp)
;	sys	fstat; buf
;	bes	er1
;	mov	buf+2,(sp)
;	sys	open; dev; 0
;	bes	er1
;	mov	r0,r1
;1:
;	mov	r1,r0
;	sys	read; buf; 16.
;	bes	er
;	cmp	r0,$16.
;	bne	er
;	mov	$buf,r0
;	cmp	(r0)+,(sp)
;	bne	1b
;	cmp	(r0)+,$"tt
;	bne	1b
;	cmpb	(r0)+,$'y
;	bne	1b
;	tstb	(r0)+
;	beq	1b
;	cmpb	(r0),$'\0
;	bne	1b
;	movb	-(r0),name
;
;er:
;	mov	r1,r0
;	sys	close
;
;er1:
;	tst	(sp)+
;	movb	name,r0
;	rts	pc
;
;.data
;dev:	</dev\0>
;.even
;.bss
;buf:	.=.+40.
;name:	.=.+2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get.s (unix v5)
;
; fopen -- open a file for use by get(c|w)
;
;fopen:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	mov	r0,0f
;	sys	0; 9f
;.data
;9:
;	sys	open; 0:..; 0
;.text
;	bes	1f
;	mov	r0,(r1)+
;	clr	(r1)+
;	mov	(sp)+,r1
;	rts	r5
;1:
;	mov	$-1,(r1)
;	mov	(sp)+,r1
;	sec
;	rts	r5
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get.s (unix v5)
;
; getc -- get characters from input file
;
;getc:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	dec	2(r1)
;	bge	1f
;	mov	r1,r0
;	add	$6,r0
;	mov	r0,0f
;	mov	r0,4(r1)
;	mov	(r1),r0
;	sys	0; 9f
;.data
;9:
;	sys	read; 0:..; 512.
;.text
;	bes	2f
;	tst	r0
;	bne	3f
;2:
;	mov	(sp)+,r1
;	sec
;	rts	r5
;3:
;	dec	r0
;	mov	r0,2(r1)
;1:
;	clr	r0
;	bisb	*4(r1),r0
;	inc	4(r1)
;	mov	(sp)+,r1
;	rts	r5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crypt.s (unix v5)
;
;/ crypt -- password incoding
;
;/	mov	$key,r0
;/	jsr	pc,crypt
;
;.globl	crypt, word
;
;crypt:
;	mov	r1,-(sp)
;	mov	r2,-(sp)
;	mov	r3,-(sp)
;	mov	r4,-(sp)
;	mov	r5,-(sp)
;
;	mov	r0,r1
;	mov	$key,r0
;	movb	$004,(r0)+
;	movb	$034,(r0)+
;1:
;	cmp	r0,$key+64.
;	bhis	1f
;	movb	(r1)+,(r0)+
;	bne	1b
;1:
;	dec	r0
;/
;/
;/	fill out key space with clever junk
;/
;	mov	$key,r1
;1:
;	movb	-1(r0),r2
;	movb	(r1)+,r3
;	xor	r3,r2
;	movb	r2,(r0)+
;	cmp	r0,$key+128.
;	blo	1b
;/
;/
;/	establish wheel codes and cage codes
;/
;	mov	$wheelcode,r4
;	mov	$cagecode,r5
;	mov	$256.,-(sp)
;2:
;	clr	r2
;	clr	(r4)
;	mov	$wheeldiv,r3
;3:
;	clr	r0
;	mov	(sp),r1
;	div	(r3)+,r0
;	add	r1,r2
;	bic	$40,r2
;	bis	shift(r2),(r4)
;	cmp	r3,$wheeldiv+6.
;	bhis	4f
;	bis	shift+4(r2),(r5)
;4:
;	cmp	r3,$wheeldiv+10.
;	blo	3b
;	sub	$2,(sp)
;	tst	(r4)+
;	tst	(r5)+
;	cmp	r4,$wheelcode+256.
;	blo	2b
;	tst	(sp)+
;/
;	.data
;shift:	1;2;4;10;20;40;100;200;400;1000;2000;4000;10000;20000;40000;100000
;	1;2
;wheeldiv: 32.; 18.; 10.; 6.; 4.
;	.bss
;cagecode: .=.+256.
;wheelcode: .=.+256.
;	.text
;/
;/
;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
;/
;	mov	$key,r0
;	mov	$cage,r2
;	mov	$wheel,r3
;1:
;	movb	(r0)+,r1
;	bic	$!177,r1
;	asl	r1
;	mov	cagecode(r1),(r2)+
;	mov	wheelcode(r1),(r3)+
;	cmp	r0,$key+128.
;	blo	1b
;/
;/
;/	now spin the cage against the wheel to produce output.
;/
;	mov	$word,r4
;	mov	$wheel+128.,r3
;3:
;	mov	-(r3),r2
;	mov	$cage,r0
;	clr	r5
;1:
;	bit	r2,(r0)+
;	beq	2f
;	incb	r5
;2:
;	cmp	r0,$cage+256.
;	blo	1b
;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
;	mov	r4,-(sp)
;	clr	r4
;	div	$26.+26.+10.,r4
;	add	$'0,r5
;	cmp	r5,$'9
;	blos	1f
;	add	$'A-'9-1,r5
;	cmp	r5,$'Z
;	blos	1f
;	add	$'a-'Z-1,r5
;1:
;	mov	(sp)+,r4
;	movb	r5,(r4)+
;	cmp	r4,$word+8.
;	blo	3b
;/
;
;	mov	(sp)+,r5
;	mov	(sp)+,r4
;	mov	(sp)+,r3
;	mov	(sp)+,r2
;	mov	(sp)+,r1
;	mov	$word,r0
;	rts	pc
;	.bss
;key:	.=.+128.
;word:	.=.+32.
;cage:	.=.+256.
;wheel:	.=.+256.