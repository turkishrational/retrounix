; ****************************************************************************
; cat386.s (Retro Unix 386 v1) - /bin/cat - concatenate files
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Retro UNIX 8086 v1 - '/bin/cat' file
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 05/03/2022 ] -cat1.s-
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************
; ((nasm cat1.s -l cat1.txt -o cat1 -Z error.txt))
; cat386.s - cat1.s (05/03/2022, Retro UNIX 386 v1 & v1.1 & v1.2)
; cat386.s - cat0.s (17/10/2015 - 28/12/2015, Retro UNIX 386 v1, NASM 2.11)
; CAT2.ASM (02/12/2013 - 16/07/2015, Retro UNIX 8086 v1, MASM 6.11) 

; 17/10/2015

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

%macro sys 1-4
    ; 03/09/2015	
    ; 13/04/2015
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
	;; / cat -- concatenate files

	sys	_write, 1, nl, 2

	pop	ebp
	pop	edx
	; esi = 0 ; ('sysexec' sets regs to zero)
	; 05/03/2022
	;mov	esi, fin 
	;mov    edi, obuf
	;EAX = 2 (written byte count)
	xor	al, al ; 0
        dec     ebp
	jz	short cat_3
		;;mov	(sp)+,r5
		;;tst	(sp)+
		;;mov	$obuf,r2
		;;cmp	r5,$1
		;;beq	3f
cat_0:	
	pop	ebx
	cmp	byte [ebx], '-'
	je	short cat_2
		;;dec	r5
		;;ble	done
		;;mov	(sp)+,r0
		;;cmpb	(r0),$'-
		;;bne	2f
		;;clr	fin
		;;br	3f
cat_1:
	;;2:
	; ebx = file name offset
	xor 	ecx, ecx ; 0
	sys 	_open
	;jc	short cat_7
	; 05/03/2022 ; (+)
	jnc	short cat_2
	jmp	cat_7
cat_2:
	; 05/03/2022
	mov	esi, eax
	;mov	[esi], eax ; 05/03/2022
	;mov	[esi], ax
		;;mov	r0,0f
		;;sys	open; 0:..; 0
		;;bes	loop
		;;mov	r0,fin

	; 05/03/2022 ; (+)
	; convert user's file number to inode number
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	sys	_fstat, esi, iobuf 
	;jc	short cat_7
	; 05/03/2022 ; (+)
	jnc	short cat_f
	jmp	cat_7
cat_f:
	; 05/03/2022 ; (+)
	;;movzx	edi, word [iobuf] ; inode number
	;movzx	edi, word [ecx] ; inode number
	; edi = 0 ; ('sysexec' sets regs to zero)
	mov	di, [ecx]	

	; 05/03/2022 ; (+)
	; check against '/dev/tty' inode number
	;sys	_stat, consoletty, iobuf
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	;mov	ecx, iobuf 
	sys	_stat, consoletty
	jc	short cat_x
	
	;movzx	eax, word [iobuf]
	mov	ax, [ecx]
	cmp	edi, eax ; is it /dev/tty inode ?
	je	short cat_y ; yes
	; no, it is not '/dev/tty'
cat_x:
	; 05/03/2022 ; (+)
	; get inode number of current tty (stdin)
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	;sys	_fstat, 0, iobuf 
	;;jc	short cat_z
	;mov	ecx, iobuf	
	sys	_fstat, 0  ; get stdin file inode details

	;movzx	eax, word [iobuf]
	mov	ax, [ecx] ; inode number
	cmp	edi, eax
	jne	short cat_z ; (ebx = 0)
	; (/dev/tyy0 .. /dev/tty9)
cat_y:
	; same with input file inode
	sub	esi, esi ; 0 
		; use console tty input (stdin)
cat_3:	;;3:
	; 05/03/2022
	;or	eax, eax  ; console tty input (stdin) ?	
	;jnz	short cat_r ; no..
	;
	; get keyboard status of console tty
	xor	ebx, ebx  ; 0	
cat_z:
	xor	ecx, ecx  ; 0
	sys	_gtty
	;;sub	eax, eax  ; clear ax
	;mov	eax, [esi] ; file descriptor/number
	and	ebx, ebx ; is there a waiting char ?
	;jz	short cat_3 ; no, wait for keystroke again
	jnz	short cat_c ; (a char is waiting in buffer)
	;mov	eax, [esi]
	;or	eax, eax ; 0 ; console tty input ?
	or	esi, esi ; 05/03/2022
	jnz	short cat_r ; no, continue (to read file)
	jmp	short cat_z ; yes, wait for keystroke again		
cat_c:
	; 05/03/2022
	; read from console tty (stdin)
	sys	_read, 0, iobuf, 1	
		; read one char into [iobuf]
	jc	short cat_6 ; (ebx = 0)
	;and	eax, eax
	;jz	short cat_6
	dec	eax ; 1 -> 0
	jnz	short cat_6
	inc	eax
	; eax = 1
	mov	bl, [iobuf] ; ascii code of the character
	cmp	bl, 1Bh ; 27 ; ESCape key ?
	je	short cat_exit ; yes, exit
	;mov	eax, [esi] ; file descriptor/number
	;or	eax, eax ; 0 ; console_tty input ?
	or	esi, esi ; 05/03/2022	
	jz	short cat_w ; yes, write char to screen
cat_r:
	; 05/03/2022
	;mov	eax, [esi] ; file descriptor/number
	;;mov	ax, [esi]
	;
	;sys	_read, eax, iobuf, 512 ; 16/07/2015
	;;sys 	_read, eax, ibuf, 512 
	;jc	short cat_6
	; 05/03/2022
	; esi = file descriptor/number
	sys	_read, esi, iobuf, 512	
		 ; read 512 chars into [iobuf]
	;jc	short cat_6
	jc	short cat_k ; 05/03/2022

	; NOTE: If input file is a tty (keyboard)
	;	only 1 byte will be read, by ignoring
	;	byte count (512).
	;	Retro UNIX 8086 v1 kernel ('rtty')
	;	has been modified fot that.
	;       Erdogan Tan (16/07/2015)
	;

	and	eax, eax ; EAX = 1 for tty (keyboard)
	;jz	short cat_6
	jz	short cat_k ; 05/03/2022

;	push	esi
	;mov	esi, ibuf
;	mov	esi, ecx ; offset ibuf
;	mov	ecx, eax
		;;mov	fin,r0
		;;sys	read; ibuf; 512.
		;;bes	3f
		;;mov	r0,r4
		;;beq	3f
		;;mov	$ibuf,r3
	 ; 16/07/2015
;	mov	edx, eax
;	;add	edx, obuf
;cat_4:
;	;;4:
;	lodsb
	;call	putc
cat_w:
	; 16/07/2015
	; write to console tty (stdout)
	sys 	_write, 1, iobuf, eax
	;jc	short cat_6
	; 05/03/2022
	jnc	short cat_3
	;
;	loop	cat_4
;	pop	esi
cat_5:
	;mov	eax, [esi] ; 05/03/2022
	;;mov	ax, [esi]
	;jmp	short cat_3
		;;movb	(r3)+,r0
		;;jsr	pc,putc
		;;dec	r4
		;;bne	4b
		;;br	3b
	;; 05/03/2022
	;; ebx = file descriptor/input
	;;and	ebx, ebx ; console input ?
	;;jnz	short cat_3 ; no, file input
	;;cmp	byte [quit], al ; 0
	;;ja	short cat_7 ; ebx = 0
	;;jmp	short cat_z
	; 05/03/2022
	;jmp	short cat_3
		
cat_6:	;;3:
	; 05/03/2022
	; ebx = file descriptor/number
	;movzx	ebx, word [esi]
	;
	or	esi, esi
	jz	short cat_7
cat_k:
	sys	_close, esi
	;
	;or	ebx, ebx
	;jz	short cat_7
	;sys 	_close
		;;mov	fin,r0
		;;beq	loop
		;;sys	close
		;;br	loop
cat_7:	
	;;loop:
	dec	ebp
	;;jz	short cat_8
	; 28/12/2015
	;jg	short cat_0
	; 05/03/2022
	jng	short cat_exit
	jmp	cat_0
cat_exit:
	sys	_exit
here:
	nop
	jmp short here
	;;
;cat_8:
;	;;done:
;	sub	di, obuf
;	jz	short cat_9
;	sys	_write, 1, obuf, di 
		;;sub	$obuf,r2
		;;beq	1f
		;;mov	r2,0f
		;;mov	$1,r0
		;;sys	write; obuf; 0:..
;cat_9:	
;	;;1:
;	sys	_exit
		;;sys	exit
	;;
;putc:	
;	;;putc:
;	stosb
;	cmp	di, dx ; 16/07/2015
	;cmp	di, obuf + 512
;	jb	short cat_10
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
;cat_10:	
	;;1:
;	retn
		;;rts	pc

nl:	db 0Dh, 0Ah ; , 0
; 05/03/2022
quit:	db 0
consoletty:
	db '/dev/tty', 0 ; (+)	

align 4

bss_start:

ABSOLUTE bss_start

iobuf:  resb 512
;ibuf:	resb 512
;obuf:	resb 512
;;fin:	resw 1
;fin:	resd 1 ; 05/03/2022	
		;;.bss
		;;ibuf:	.=.+512.
		;;obuf:	.=.+512.
		;;fin:	.=.+2
		;;.text
;bss_end: