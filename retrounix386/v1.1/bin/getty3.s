; ****************************************************************************
; getty386.s (getty3.s) - Retro Unix 386 v1.2 - /etc/getty
; ----------------------------------------------------------------------------
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; [ Last Modification: 18/02/2022 ]
;
; Derived from 'getty07.asm' source code file of 'Retro UNIX 8086 v1'
; operating system project, /etc/getty source code by Erdogan Tan
; (06/11/2013-22/05/2014)
;
; Derived from 'getty.s' file of original UNIX operating system
; (v1.0 for PDP-11)
; ****************************************************************************

; getty3.s (05/02/2022, Retro UNIX 386 v1 & v1.1 & v1.2)
; getty2.s (17/11/2015, Retro UNIX 386 v1 & v1.1)
; getty1.s (11/11/2015, Retro UNIX 386 v1)
; getty0.s (13/10/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit version)
; GETTY07.ASM, 06/11/2013 - 22/05/2014 (Retro UNIX 8086 v1, MASM 6.11) 

; 12/01/2022 (Retro UNIX 386 v1.2)
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
ENTERKEY  equ 0Dh
NEXTLINE  equ 0Ah
BACKSPACE equ 08h
; 22/05/2014
EOT	equ 04h ; 'End Of Transfer' for serial ports	

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:

	sys _quit, 0
	sys _intr, 0

        sys _gtty, 0, 1  ; get status of console tty (w)
        ;jc  short terminate
	; 05/02/2022
	jc short dont
	;
	mov [console], al
	add al, '0'
	mov [ttynum], al
	;	
	sys _write, 1, msglogin, ml_size
	;jc short terminate
	jnc short do
dont:	; 05/02/2022
	jmp terminate		
do:
	;;mov word [cursorpos], 0FF00h
	;;mov byte [cposll], 0
	;
 	cmp byte [console], 7
	ja  short G0
	; 	
	sys _gtty, 0, 1  ; get status of console tty (w)	
	;jc short terminate
	; 05/02/2022
	jc short dont
	
	mov [cursorpos], bx
	mov [cposll], bl
	mov byte [chr], 07h ; bell/beep
G0:
	sys _write, 1, chr, 1
	jc  short terminate
	;
        mov edi, uname
	; 18/02/2022
	;mov esi, edi
getc:
	sys _read, 0, chr, 1
	jc short terminate
	;
	mov al, [chr]

	cmp al, 20h
	jb  short G2

	cmp al, 127
	je  short G3

        cmp edi, uname + 16
	jnb short G4
putc:
	stosb
	inc byte [cursorpos]
	; 18/02/2022
	;cmp esi, edi
	;jnb short G1
	;mov esi, edi
G1:
	sys _write, 1, chr, 1
	jnc  short getc

	; 05/02/2022
terminate:
        sys _exit
here:	
	hlt
	jmp short here

G2:
	cmp al, ENTERKEY  ; \r (carriage return)
        je  short G7

	;cmp al, NEXTLINE ; \n (next line)
        ;je  short G7

	cmp al, BACKSPACE ; \b (back space)
	jne short getc
G3:
	; Backspace
	mov dx, [cursorpos]
	;
	cmp dl, [cposll] ; left limit
	ja  short G5
G4:
	mov byte [chr], 07h
	jmp short G1
G5:
	dec edi ; 18/02/2022
	; 
	dec dl
	mov byte [cursorpos], dl
	mov cl, [console]
	cmp cl, 8
	jb  short G6
	; 18/02/2022
	;dec edi
	;;mov [chr], al
	mov byte [chr], BACKSPACE
	jmp short G1	
G6:
	mov ch, 20h ; ch < FFh & ch > 0 -> write 20h
		    ; (space) at requested cursor position	 		
	xor ebx, ebx ; 0 
	sys _stty  ; set cursor pos. for console tty
		   ; (back space)
	jc  short terminate
	; 18/02/2022
	;dec edi
        jmp getc
G7:
	;mov byte [esi], 0 ; ASCIIZ string
	;cmp byte [esi-1], 20h
	;jne short go
	;mov byte [esi-1], 0
;G8:
	;dec esi
	;; 18/02/2022
	;cmp byte [esi], 20h
	;jne short go
	;mov byte [esi], 0
	;jmp short G8 
	
	; 18/02/2022
	mov byte [edi], 0
G8:
	dec edi
	cmp byte [edi], 20h
	jne short go
	mov byte [edi], 0
	jmp short G8			
go:
        sys _exec, login, loginp
	; 05/02/2022
	jmp short terminate

align 4
loginp: 
	dd login
        dd uname
	dd 0
chr:	
	db 0
;align 2
console: 
	db 0 ; console tty
; cursor position
cposll: 
	db 0 ; left limit of cursor position
cursorpos: 
	db 0 ; row (for backspace)
	db 0FFh ; column 
           ; (FFh for serial ports, for sysstty) 
;(cursorpos will set by return of sysgtty for pseduo ttys)

;align 2
login:	
	db '/bin/login', 0

align 2
msglogin:
	db 0Dh, 0Ah
	db 'Retro Unix 386 v1 (tty'
ttynum: db 'x'
	db ')'
	db 0Dh, 0Ah
	db 'login : '
ml_size equ $ - msglogin
	db 0
align 2
uname:  
	times 16 db 0
	; 18/02/2022
	db 0
	db 18

;/ getty --  get name and tty mode
;/ for initialization
;
;/ cycle through speeds and "login:" messages
;/ summarized in itab
;
;stty = 31.
;
;	sys	quit; 0
;	sys	intr; 0
;0:
;	jsr	r5,nextspeed
;1:
;	mov	$name,r5
;2:
;	jsr	r5,getc
;	cmp	r0,$174
;	beq	5f
;	cmp	r0,$176
;	beq	5f
;	cmp	r0,$'\n
;	beq	1f
;	cmp	r0,$'\r
;	beq	4f
;	cmp	r0,$'@
;	beq	1b
;	cmp	r0,$'#
;	bne	3f
;	cmp	r5,$name
;	blos	2b
;	dec	r5
;	br	2b
;3:
;	movb	r0,(r5)+
;	br	2b
;4:
;	bis	$20,flags		/cr bit
;	mov	$1,r0
;	sys	write; nl; 1
;	br	2f
;5:
;	mov	$tab2741,itabp
;	inc	nowr
;	br	0b
;1:
;	mov	$1,r0
;	sys	write; cr; 1
;2:
;	clrb	(r5)+
;
;/ determine whether terminal is upper-case only
;
;	cmp	r5,$name+1
;	bhi	1f
;	bic	$4,flags	/no data-assume lc
;1:
;	mov	$name,r5
;1:
;	movb	(r5)+,r0
;	beq	1f
;	cmp	r0,$'A
;	blo	2f
;	cmp	r0,$'Z
;	bhi	2f
;	add	$40,r0		/ map to lc
;	movb	r0,-1(r5)
;	br	1b
;2:
;	cmp	r0,$'a
;	blo	1b
;	cmp	r0,$'z
;	bhi	1b
;	bic	$4,flags
;	br	1b
;1:
;	clr	r0
;	mov	fstate,r4
;	bis	flags,4(r4)
;	sys	stty; fstate: ..
;
;go:
;	sys	exec; login; loginp
;	sys	exit
;
;getc:
;	clr	r0
;	sys	read; ch; 1
;	tst	r0
;	beq	done
;	mov	ch,r2
;	beq	1f
;getc1:
;	cmp	r2,$174
;	bhis	3f
;	tst	nowr
;	bne	3f
;	mov	$1,r0
;	sys	write; ch; 1
;3:
;	mov	r2,r0
;	rts	r5
;1:
;	dec	$0		/ wait a while
;	bne	1b
;	mov	$name,(sp)
;	jsr	r5,nextspeed
;2:
;	clr	r0		/ flush nulls
;	sys	read; ch; 1
;	tst	r0
;	beq	done
;	movb	ch,r2
;	beq	2b
;	br	getc1
;
;done:
;	sys	exit
;
;nextspeed:
;	mov	itabp,r1
;	mov	(r1)+,0f
;	bne	1f
;	mov	$itab,itabp
;	br	nextspeed
;1:
;	clr	r0
;	sys	stty; 0:..
;	bes	go
;	mov	(r1)+,-(sp)
;	mov	(r1)+,fstate
;	mov	r1,itabp
;	mov	(sp)+,r1
;1:
;	movb	(r1)+,ch
;	beq	1f
;	mov	$1,r0
;	sys	write; ch; 1
;	br	1b
;1:
;	rts	r5
;
;itabp:	itab
;loginp:login
;	name
;	0
;
;itab:
;	itty37; ttymes; tty37
;	itn300; tnmes;  tn300
;tab2741:i2741; m2741; f2741
;	0
;
;itty37:511; 511; 340	/ any parity, raw, 150 baud
;tty37:	511; 511; 210	/ 37 parity, echo, 150 baud
;itn300:521; 521; 340	/ any parity, raw, cr, 300 baud
;tn300:	521; 521; 310	/ any parity, echo, 300 baud
;i2741:	1501; 501; 100540	/134 bits, 2741, raw, first time
;f2741:	1501; 501; 500	/134 bps, 2741
;
;	0
;m2741:	<\nlogin: \0>
;
;ttymes:
;	<\n\r\p:\alogin: \0>
;tnmes:
;	<\n\r\p;login: \0>
;
;login:	</bin/login\0>
;	.even
;
;nl:	<\n>
;cr:	<\r>
;
;flags:	004	/ upper case map
;
;	.bss
;ch:	.=.+2
;nowr:	.=.+2
;name:	.=.+32.