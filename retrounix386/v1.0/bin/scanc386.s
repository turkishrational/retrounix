; ****************************************************************************
; scanc386.s (Retro Unix 386 v1) - 'scancode', prints character codes
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Retro UNIX 8086 v1 - 'scanc.asm' file
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 23/11/2015 ]
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************
;
; scanc1.s (23/11/2015) 
; scanc0.s (17/10/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit version)
; scanc.asm (SCANCODE.ASM), 20/01/2014 (Retro UNIX 8086 v1, MASM 6.11) 
; (The first SCANCODE.ASM -  01/02/1998, DOS program)

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
		; (c) Erdogan TAN 1998
		; Scancode, prints character codes
		; 01/02/1998
		; NASM version: 01/12/2014

                mov     esi, BossMsg
                call    proc_printmsg
sc0:
		sys	_gtty, 0, 0
		jc	short exit
		and	bx, bx
		jz	short sc0
		cmp	al, 8  ; tty number 
		jb	short sc2 ; pseudo tty
		; serial port (tyy8 or tty9)
  		mov     word [Reg_ScanCode], 3030h ; '0'
sp_sc:
		call	write_line
		jc	short exit
		cmp	al, 0Dh ; ENTER (Carriage Return)		
		je	short exit
		jmp	short sp_sc
sc1:	
		sys	_gtty, 0, 0
		and	bx, bx
		jz	short sc1
sc2:    
  		cmp     bl, 0Dh
                je      short sc3
		mov	[Character], bl
sc3:    
                mov     al, bh
                call    proc_hex
                mov     [Reg_ScanCode], ax
		call	write_line
		cmp 	al, 0Dh
                jne 	short sc1
exit:
		sys	_exit
never_come_here:
		nop
                jmp     short never_come_here
write_line:
  		sys	_read, 0, Character, 1
		jc	short sc5
		mov	bl, [Character]
              	mov	al, bl
                call    proc_hex
                mov     [Reg_AsciiCode], ax
		cmp 	bl, 0Dh ; ENTER (CR) key
		jne	short sc4
                mov     byte [Character], 20h         
sc4:		
		push 	bx
                mov     esi,  ScancodeMsg
                call    proc_printmsg
		pop	ax
sc5:
		retn
proc_hex:
                mov     ah, al
                and     ah, 0Fh
                add     ah, 30h
                cmp     ah, 39h
                jna     short sc6
                add     ah, 07h
sc6:
                shr     al, 1
                shr     al, 1
                shr     al, 1
                shr     al, 1
                add     al, 30h
                cmp     al, 39h
                jna     short sc7
                add     al, 07h
sc7:
                retn
proc_printmsg:
		mov	ecx, esi
		xor	edx, edx
sc8:
		lodsb
		and	al, al
		jz	short sc9
		inc	edx
		jmp	short sc8
sc9:          
		mov	ebx, 1
		sys	_write
		retn

BossMsg:
                db 0Dh, 0Ah
                db '[ (c) Erdogan TAN  1998-2015 ]  Press a key to scan code...'
                db 0Dh, 0Ah
                db 0Dh, 0Ah, 0h
ScancodeMsg:
                db 'Character : '
Character:      db '?'
                db '     Scan Code : '
Reg_ScanCode:   dw '??'
                db 'h'
                db '     ASCII Code : '
Reg_AsciiCode:  dw '??'
                db 'h'
                db 0Dh, 0Ah, 0h
