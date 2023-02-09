; ****************************************************************************
; pfc.s (Retro Unix 386 v1.2) - get page fault count -Erdogan Tan- 27/12/2022
; ----------------------------------------------------------------------------
; [ Last Modification: 27/12/2022 ]

; ****************************************************************************
; Assembler: NASM v2.15
; ((nasm pfc0.s -l pfc0.txt -o pfc0 -Z error.txt))

; 27/12/2022 (40)
; 12/01/2022 (37,38,39)
; 21/09/2015 (36) 
; 01/07/2015 (35)
; 14/07/2013 (0-34)
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
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_ver	equ 39 ; (get) Retro Unix 386 version
; 27/12/2022 - Retro UNIX 386 v1.2
_mem	equ 40 ; get available memory

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

; 17/06/2022 - Retro UNIX 386 v1
;struc stat
;	; Retro UNIX v1 'sysstat' output !
;	; (34 bytes)
;	.inode:  resw 1	
;	.mode:	 resw 1
;	.nlinks: resb 1
;	.uid:	 resb 1
;	.size:	 resw 1
;	.dskptr: resw 8
;	.ctime:	 resd 1
;	.mtime:	 resd 1
;	.rsvd:   resw 1
;	.strucsize:
;endstruc 

ENTERKEY  equ 0Dh
NEXTLINE  equ 0Ah
BACKSPACE equ 08h 

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

; --------------------------------------------------------------

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; 27/12/2022
	; get my (current process's) page faults count
	xor 	ebx, ebx ; 0
	dec	ebx ; -1
	sys	_geterr
	mov	[my_pfc], eax
	; get total page faults count
	dec	ebx ; -2
	sys	_geterr
	mov	[total_pfc], eax

	call	page_fault_info

	sys	_write, 1, msg_pfc_info, size_of_pfcinfo

	sys	_exit
here:
	nop
	jmp	short here

; --------------------------------------------------------------

page_fault_info:	
	mov	eax, [total_pfc]
	mov	ebx, 10
	mov	cl, 7
	mov	esi, total_pfc_str	
	call	bintdstr
	mov	cl, 7
	mov	esi, my_pfc_str
	jmp	short bintdstr

bintdstr:
	; EAX = binary number
	; ESI = decimal/numeric string address
	; EBX = divisor (10)
	; ECX = string length (<=10)
	add	esi, ecx
btdstr0:
	dec	esi
	xor	edx, edx
	div	ebx
	add	dl, 30h
	mov	[esi], dl
	dec	cl
	jz	short btdstr2
	or	eax, eax
	jnz	short btdstr0
btdstr1:
	dec	esi
        mov     byte [esi], 20h ; blank space
	dec	cl
	jnz	short btdstr1
btdstr2:
	retn

msg_pfc_info:
	;db	07h
	db	0Dh, 0Ah
	db	0Dh, 0Ah
	db 	"MEMORY PAGE FAULT COUNTS", 0Dh, 0Ah, 0Dh, 0Ah
	db	"Total page faults : "
total_pfc_str:	; 7 digits
	db	"0000000 ", 0Dh, 0Ah
	db 	0Dh, 0Ah
	db	"My page faults    : "
my_pfc_str:	; 7 digits
	db	"??????? ", 0Dh, 0Ah
	db	0Dh, 0Ah, 0

size_of_pfcinfo equ ($ - msg_pfc_info) - 1

; --------------------------------------------------------------

align 4

bss_start:

ABSOLUTE bss_start

total_pfc: resd 1
my_pfc:	resd 1

; --------------------------------------------------------------
