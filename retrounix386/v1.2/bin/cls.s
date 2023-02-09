; ****************************************************************************
; CLS.ASM (cls.s)
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - cls (clear screen) file
;
; [ Last Modification: 02/02/2022 ]
;
; ****************************************************************************
; 02/02/2022
; Assembler: NASM 2.15
; 	nasm cls.s -l cls.txt -o cls -Z error.txt
;
; (Retro UNIX 386 v1 - cls.s, NASM 2.15)
; Derived from CLS.ASM (30/01/2022, Retro UNIX 8086 v1, MASM 6.14)  
;
; ****************************************************************************
; 02/02/2022
; ----------------------------------------------------------------------------

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

[BITS 32] ; 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; clear screen
	;
	; eax = _stty = 31
	; ebx = 0 (/dev/tty? name pointer will not be used)
	; ecx = 00FFh 
	;     ch = 0 -> clear screen condition 1 
	;			     (ch must be 0)
	;     cl = 0FFh -> clear console screen
	; edx = 0FFFFh
	;	clear screen condition 2
	;		     (dx must be 0FFFFh

	sys	_stty, 0, 00FFh, 0FFFFh

	sys	_exit

here:
	jmp	here
