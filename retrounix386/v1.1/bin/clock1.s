; ****************************************************************************
; clock386.s (Retro Unix 386 v1) - 'clock' prints current date & time
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Retro UNIX 8086 v1 - 'clock.asm' file
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 26/02/2022 ]
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************
; clock1.s (21/02/2022, Retro UNIX 386 v1&v1.1&v1.2)
; clock0.s (17/10/2015-18/11/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit)
; CLOCK.ASM, 12/12/2013 - 17/01/2014 (Retro UNIX 8086 v1, MASM 6.11) 
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
	sys	_write, 1, b_dt_txt, dt_size+1
	;jc	short terminate
	sys	_gtty, 0, 1 ; get console tty, cursor position
	;jc	short terminate
        mov     [ttynum], al
	cmp	al, 7 
	jna	short x1
	;mov	word [dt_txt], 0D07h
	mov	byte [dt_txt+1], 07h ; 21/02/2022
	jmp	short clk0
x1:
	mov 	[cursor_pos], bx ; cursor position
	xor	edx, edx ; 0
clk0:
	sys 	_time
	; EAX = Unix epoch time
	cmp	eax, edx
	;je	clk4
	; 21/02/2022
	jne	short clk1
clk4:
	nop
	nop
	nop
	nop
        ; 21/02/2022
	xor	eax, eax
	inc	eax
	dec	eax
	jmp	short clk0
clk1:
	push	eax ; current time
	call	ctime
	;sys	_write, 1, cbuf, 25
	; 25/02/2022
	sys	_write, 1, cbuf, 26
	sys	_gtty, 0, 0
	;or	bx, bx
	or	ebx, ebx ; 21/02/2022
	jz	short clk2
	sys	_read, 0, chr, 1
	cmp	byte [chr], 1Bh ; ESC key
	je	short clk3 ; exit
clk2:
	cmp	byte [ttynum], 8
	jb	short clk_pt
	sys	_write, 1, dt_txt, dt_size
	jmp	short x2
clk_pt:
	mov	dx, [cursor_pos]
	mov	cx, 0FFFFh ; set cursor position only
	; 26/02/2022 (ebx = 0)
	;;xor 	bx, bx ; set for console tty
	;xor	ebx, ebx ; 21/02/2022
	sys	_stty
x2:
	pop	edx ; current time -> previous time
        jmp     clk0 
clk3:
	pop	eax
terminate:
	sys	_exit
	; 21/02/2022
;clk4:
;	nop
;	nop
;	nop
;	nop
;	jmp     clk0
	; 21/02/2022
	jmp	clk4

;%include 'ctime386.inc'
%include 'ctime386.s' ; 21/02/2022

cursor_pos: dw 0

b_dt_txt:
	db 07h
dt_txt:
	db 0Dh
	db 0Ah
	db 'Current Date & Time : '
dt_size equ $ - dt_txt	
chr:	db 0
ttynum: db 0