; ****************************************************************************
; prntst86.s (/dev/lpr printer test) - by Erdogan Tan - 13/06/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - /dev/lpt -- printer test (LPT1) 
;
; [ Last Modification: 13/06/2022 ]
;
; ****************************************************************************

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
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_sysver	equ 39 ; (get) Retro Unix 386 version

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

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

[BITS 16] ; 16-bit intructions (for 8086 & 80386 real mode)

[ORG 0] 

START_CODE:
	; 13/06/2022
	pop	cx ; cx = number of arguments
	;
	pop	ax ; ax = argument 0 = executable file name
	;
	mov	bp, program_msg
	;
	;cmp	cx, 2
	cmp	cl, 2
	jb	short prntst1

	pop	di ; argument 1 = txt file name

	sys	_stat, di, stbuf
	jc	short prntst0
	
	mov	si, [stbuf+6] ; file size
	
	; (character count or buffer size must not over
	; available free space in current segment)
	mov	cx, sp ; top of stack
	sub	cx, fbuf+32 ; fbuf location +32 bytes for stack 
	cmp	cx, si ; cx contains max. available free space
	jnb	short prntst6
	; si (buffer size) must not be more/greater than cx
	mov	si, cx 
prntst6:
	sys	_open, di, 0 ; open for read
	jc	short prntst0

	;sys	_read, ax, fbuf, 1024

	sys	_read, ax, fbuf, si
	
	cmp	word [fbuf], 0
	jna	short prntst1

	mov	bp, fbuf
	jmp	short prntst1
prntst0:
	mov	ax, f_not_found
	call	print_msg
prntst1:
	xor	di, di
	sys	_open, lpt1, 1  ; open /dev/lpr for write
	jnc	short prntst2

prntst_err:
	mov	ax, err_msg
pr_ok_exit:
	call	print_msg
	and	di, di ; file descriptor
	jz	short hang
	sys	_close, di
hang:
	sys	_exit
	jmp	short hang
prntst2:
	mov	di, ax ; /dev/lpr file descriptor

	cmp	bp, program_msg
	jne	short prntst4

	mov	ax, p_p_msg
	call	print_msg

	; print program message on paper (on LPT1)
	sys	_write, di, program_msg, size_pmsg
	jc	short prntst_err

	sys	_write, di, usage_msg, size_umsg
	jc	short prntst_err
prntst3:
	mov	ax, ok_msg
	jmp	short pr_ok_exit

prntst4:
	mov	ax, p_f_msg
	call	print_msg

;	mov	cx, bss_end-fbuf ; 1024
;	mov	si, fbuf
;	sub	dx, dx
;prntst5:
;	lodsb
;	or	al, al
;	jz	short prntst6
;	inc	dx
;	loop	prntst5
;prntst6:
	;;dx  = character count to be printed
	;sys	_write, di, fbuf
	sys	_write, di, fbuf, si ; si = file size
	jnc	short prntst3
	jmp	prntst_err

;-----------------------------------------------------------------

print_msg:
	; ax = asciiz string address
	mov	bx, ax
	dec	bx
nextchr:
	inc	bx
	cmp	byte [bx], 0
	ja	short nextchr
	;cmp	[bx], 0Dh
	;ja	short nextchr
	sub	bx, ax
	mov	dx, bx
	; dx = asciiz string length
	;
	sys	_write, 1, ax
	;
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

lpt1:
	db	'/dev/lpr', 0

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 8086 v1 PRINTER Test by Erdogan TAN - 13/06/2022'
	db  0Dh, 0Ah, 0
usage_msg:
	db  0Dh, 0Ah
	db  'Usage: prntest textfilename'
nextline:
	db  0Dh, 0Ah, 0

size_pmsg equ usage_msg-(program_msg+1)
	
err_msg:
	db 0Dh, 0Ah	
	db "Printer Error !"	
 	db 0Dh, 0Ah, 0  		

size_umsg equ err_msg-(usage_msg+1)

p_f_msg:
	db 0Dh, 0Ah	
	db "Printing .. "
	db 0
ok_msg:
	;db 0Dh, 0Ah
	db 'OK. '
	db 0Dh, 0Ah, 0
f_not_found:
	db 0Dh, 0Ah	
	db "File not found !"
	db 0Dh, 0Ah, 0
p_p_msg:
	db 0Dh, 0Ah	
	db "Printing program message on LPT1 .. "
	db 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 2

bss_start:

ABSOLUTE bss_start

stbuf:	resb 34	 ; stat buffer
	resb 2	

fbuf:	resb 1024 ; file buffer

bss_end:

;-----------------------------------------------------------------