; ****************************************************************************
; args8086.s (args.asm) - Retro Unix 8086 v1 - sys exec argument list test
; ----------------------------------------------------------------------------
; RETRO UNIX 8086/386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012)
;
; [ Last Modification: 16/03/2022 ]
;
; ****************************************************************************
; Assembler: NASM 2.15
;	nasm args8086.s -l args8086.lst -o args8086.bin -Z error.txt	

; Modified from: args386.s (Retro UNIX 386 v1) source code by Erdogan Tan
;	         11/10/2015	

; 09/10/2015
; 08/10/2015 

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

[BITS 16] ; 16-bit (x86 real mode) intructions

[ORG 0] 

START_CODE:
	;sys	_msg, version, 255, 0Ah
	sys	_write, 1, version, prg_msg_size 
	
	;mov	esi, esp
	;lodsd
	;and	eax, eax
	;jz	short terminate
	
	mov	si, sp
	lodsw
	and	ax, ax
	jz	short terminate		

	;mov	ebp, eax
	mov	bp, ax
nextarg:
	inc	byte [argn]

	;sys	_msg, args, 255, 0Eh
	sys	_write, 1, args, arg_lbl_size 	
	
	;lodsd
	lodsw	
	call	strlen ; 16/03/2022

	;sys	_msg, eax, 255, 0Fh
	sys	_write, 1, ax, di
		
	;dec	ebp
	dec	bp
	jnz	short nextarg

	;sys	_msg, nextline, 2, 07h
	sys	_write, 1, nextline, 2
terminate: 
	sys	_exit
halt:
	jmp	short halt

	; 16/03/2022
strlen:
	mov	di, ax
	dec	di
nextchr:
	inc	di
	cmp	byte [di], 0
	jnz	short nextchr
	;cmp	[di], 0Dh
	;ja	short nextchr
	sub	di, ax
	; di = asciiz string length
	retn

args:
	db 0Dh, 0Ah
	db 'Argument'
argn:
	db '0'
	db ': '
	db 0

arg_lbl_size equ ($ - args) - 1

version:
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Retro UNIX 8086 v1 - Argument List Test Program'
	db 0Dh, 0Ah
	db 'by Erdogan Tan - 16/03/2022'
nextline:
	db 0Dh, 0Ah, 0

prg_msg_size equ ($ - version) - 1
