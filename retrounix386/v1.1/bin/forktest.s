; ****************************************************************************
; forktest.s - Retro Unix 386 v1 Kernel - 'sysfork' test
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 17/09/2015 ]
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************

; forktest.s, 12/09/2015 (Retro UNIX 386 v1, NASM 2.11)
; SHELL01.ASM, 09/08/2013 (Retro UNIX 8086 v1, MASM 6.11) 

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
	mov 	esi, sysfork_test
	call 	print_msg

      	mov 	ebx, chldr
	sys 	_fork
	jc 	short error	

	; eAX = process id
	call 	bin_to_hex
	mov 	[cpid], ax

	mov 	esi, parent_return
	call 	print_msg

	sys	_read, 0, chr, 1 
pwait:
	sys 	_wait
	jc 	short error

	call 	bin_to_hex ; 17/09/2015
	cmp 	ax, [cpid]
	jne 	short pwait
f1b:
	mov 	esi, msg_ok
	call 	print_msg

	jmp 	short here	
chldr:
	; eAX = process id
	call 	bin_to_hex
	mov 	[ppid], ax

	mov 	esi, child_return
	call 	print_msg

	sys 	_exit

        jmp     short f1b
here:	
	sys	_exit
	; hlt
	; jmp 	short here

error:
	mov 	esi, msg_err
	call 	print_msg
        jmp 	short here
 
print_msg:
	sys 	_msg, esi, 255, 0Ah ; message with light green color (max. 255 chars)
	retn

bin_to_hex:
	db 	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
	or 	ax, '00'        ; Make it ZERO (ASCII) based

	xchg 	ah, al

	cmp 	al, '9'
        jna     short f1f
	add 	al, 7
f1f:
	cmp 	ah, '9'
        jna     short f2f
	add 	ah, 7
f2f:
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

sysfork_test:	db 0Dh, 0Ah
		db 'Retro Unix 386 v1 - <sysfork> test'
		db 0
parent_return:	db 0Dh, 0Ah
		db 'This is parent return !'
		db 0Dh, 0Ah
		db 'Child Process ID: '
cpid:           dw 3030h
		db 0Dh, 0Ah
sizeofparentr   equ $ - parent_return
		db 0
child_return:	db 0Dh, 0Ah
		db 'This is child return !'
		db 0Dh, 0Ah
		db 'Parent Process ID: '
ppid:           dw 3030h
		db 0Dh, 0Ah
sizeofchildr	equ $ - child_return
		db 0
msg_err:
		db 0Dh, 0Ah 
                db 'Error ! '
		db 0Dh, 0Ah, 0
msg_ok:
        	db 0Dh, 0Ah
        	db 'OK. '
        	db 0Dh, 0Ah
sizeof_ok 	equ $ - msg_ok 
        	db 0
chr:		db 0	