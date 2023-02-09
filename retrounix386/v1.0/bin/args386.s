; ****************************************************************************
; args386.s (args.s) - Retro Unix 386 v1 - sys exec & shell argument list test
; ----------------------------------------------------------------------------
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; [ Last Modification: 11/10/2015 ]
;
; ****************************************************************************
; Assembler: NASM 2.11

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
	sys _msg, version, 255, 0Ah
	mov esi, esp
	lodsd
	and eax, eax
	jz  short terminate
	mov ebp, eax
nextarg:
	inc byte [argn]
	sys _msg, args, 255, 0Eh
	lodsd
	sys _msg, eax, 255, 0Fh
	dec ebp
	jnz short nextarg
	sys _msg, nexline, 2, 07h
terminate: 
	sys _exit
halt:
	jmp short halt
args:
	db 0Dh, 0Ah
	db 'Argument'
argn:
	db '0'
	db ': '
	db 0
version:
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Retro UNIX 386 v1 - Argument List Test Program'
	db 0Dh, 0Ah
	db 'by Erdogan Tan - 09/10/2015'
nexline:
	db 0Dh, 0Ah, 0
