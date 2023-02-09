; ****************************************************************************
;
; CLS.ASM  (Retro Unix 8086 v1 - cls - clear video page)
; ----------------------------------------------------------------------------
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; Retro UNIX 8086 v1 - cls (clear screen) file
;
; [ Last Modification: 30/01/2022 ]
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
;
; ****************************************************************************
; 30/01/2022
; Assembler: MASM 6.14
; 	masm cls.asm, cls.obj, cls.lst
;	link /t cls.obj
;

.8086

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

;;;

sys macro syscallnumber, arg1, arg2, arg3

    ; Retro UNIX 8086 v1 system call.

    ifnb <arg1> 	
      mov bx, arg1
    endif
    
    ifnb <arg2> 	
      mov cx, arg2
    endif

    ifnb <arg3> 	
      mov dx, arg3
    endif
     			
    mov ax, syscallnumber	
    int 20h	
   
    endm

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>


UNIX   	SEGMENT PUBLIC 'CODE'
        assume cs:UNIX,ds:UNIX,es:UNIX,ss:UNIX

	; 30/01/2022
	; clear screen by using 
	; /// retro unix 8086 v1 kernel (30/01/2022) ///
	; 'sysstty' system call 
START_CODE:
	; clear screen
	;
	; ax = _stty = 31
	; bx = 0 (/dev/tty? name pointer will not be used)
	; cx = 00FFh 
	;     ch = 0 -> clear screen condition 1 
	;			     (ch must be 0)
	;     cl = 0FFh -> clear console screen
	; dx = 0FFFFh
	;	clear screen condition 2
	;		     (dx must be 0FFFFh

	sys	_stty, 0, 00FFh, 0FFFFh

	sys	_exit

UNIX	ends

	end     START_CODE