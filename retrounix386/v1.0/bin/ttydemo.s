; ****************************************************************************
; ttydemo.s (args.s) - Retro Unix 386 v1 - tty output (digital) scroll demo
; ----------------------------------------------------------------------------
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; [ Last Modification: 11/11/2015 ]
;
; ****************************************************************************
; Assembler: NASM 2.11

; 11/11/2015

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

ESC equ 1Bh
CR  equ 0Dh

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 32-bit intructions (80386 protected mode)

[ORG 0] 

START_CODE:
	sys _write, 1, header, headersize
	mov [counter], ebx
	mov [shiftbit], bx
	mov byte [line], CR
	sys _time
	mov [timeword], ax
waitf1s:
	sys _time
	rol byte [shiftbit], 1
	cmp ax, [timeword]
        je short waitf1s
	mov [timeword], ax
	sys _time
waitf2s:
	sys _time
	cmp ax, [timeword]
	jne short delaycalc
	inc dword [counter]
	jmp short waitf2s
delaycalc:
	mov eax, [counter]
	xor dl, dl
	mov bl, 20
	div ebx
	mov [counter], eax
nextline:
	inc word [timeword]	
	;
	mov edi, text
	;
	mov dx, ax
nextstr:
	mov cl, [char]
	add al, cl
	add ah, [timeword]
	;
	and al, 1
	add al, '0'
	stosb ; +0
	sub al, al
	ror dx, 1
	adc al, '0'
	stosb ; +1
	mov al, dl
	xor al, ah
	mov cl, [shiftbit]
	and al, cl
	jz  short _0
	mov al, 1	
_0:
	add al, '0'
	stosb ; +2
	cmp edi, text+80
        jnb writeline 
	or  ah, dl
	and ah, cl
	jz  short _1
	mov ah, 1
_1:	
	mov al, ah
	add al, '0'
	stosb ; +3
	mov ax, dx
	xor al, ah
	and al, 1
	add al, '0'
	stosb ; +4
	xor al, al
	ror dl, 1
	adc al, '0'
	stosb ; +5
	sub al, al
	ror dl, 1
	adc al, '0'
	stosb ; +6
	xor al, al
	ror dh, 1
	adc al, '0'
	stosb ; +7
	mov dx, [timeword]
	dec byte [negative]
	js short _4
	neg dx
	mov [timeword], dx
_2:
	mov al, '0'
	shr ah, 1
	adc al, 0
	stosb ; +8
	and ah, 1
	mov al, 01010101b
	and al, cl
	jz short _3
	mov al, 1
_3:
	xor al, ah
        mov ah, '0'
	add al, ah
	stosb ; +9
	mov al, dl
	xor al, [edi]
	and al, 1
	add al, ah
	stosb ; +10
	rol cl, 1
	mov [shiftbit], cl		  	
_4:
	inc byte [negative]
	mov ax, dx
	jz short _2
	ror word [timeword], 1
	mov dx, [timeword]
        jmp nextstr
writeline:
	sys _gtty, 0, 0
	or bx, bx
	jz short _5
	sys _read, 0, char, 1
	cmp byte [char], ESC
        jne short _5
	sys _exit
_5:
	sys _write, 1, line, 81
delay:
	mov ecx, [counter]
_6:
        dec dword [counter]
	jns short _6
	mov [counter], ecx
_7:
	ror word [timeword], 1
	mov ax, [timeword]
	dec byte [negative]
	jns nextline
	inc byte [negative]
	not ax
	jmp nextline
	
align 2

header:
	db 0Dh
	db '*** Retro UNIX 386 v1 - TTY (DIGITS SCROLL) DEMO by Erdogan Tan - 11/11/2015 ***'
	db 0Dh
	db 0Ah

headersize equ    $ - header

	db 0

align 2

bss_start:

absolute bss_start

counter: resd 1
;	
timeword: resw 1
shiftbit: resb 1
negative: resb 1
;
char:   resb 1
line:  	resb 1
text:	resb 80

bss_end:
