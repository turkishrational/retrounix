; ****************************************************************************
; df1.s (df386.s) - print free blocks - by Erdogan Tan - 30/06/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 (& v1.1) - file system (disk free blocks info) utility 
;
; [ Last Modification: 30/06/2022 ]

; Derived from unix v2 '/bin/df' (PDP-11 assembly) source code 
;
; ****************************************************************************
; [ svntree-20081216.tar.gz - bin/df (archive date: 21-11-1972) ]

; Assembler: NASM v2.15
; ((nasm df1.s -l df1.txt -o df1 -Z error.txt))

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

;-----------------------------------------------------------------
;  code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 30/06/2022
	pop	ecx ; ecx = number of arguments
	;
	pop	eax ; eax = argument 0 = executable file name
	;
	;dec	ecx
	dec	cl
	jng	short df_0

	pop	esi

	lodsw	

	cmp	ax, 'fd'
	jne	short df_2
	cmp	byte [esi], 0
	jna	short df_1
	lodsw
df_2:
	cmp	al, '0'
	jb	short df_1
	cmp	al, '1'
	ja	short df_1
	or	ah, ah
	jnz	short df_1
	mov	[fdname+7], al

	sys	_open, fdname, 0 ; open /dev/fd? for read
	jnc	short df_3

df_err:
	sys	_write, 1, error_msg, size_emsg
	jmp	short hang ; terminate
	
df_0:
	; print usage message on stdout
	;sys	_write, 1, program_msg, size_pmsg
	sys	_msg, program_msg, size_pmsg, 0Fh 
df_1:
	sys	_write, 1, usage_msg, size_umsg

hang:
	sys	_exit
	jmp	short hang

df_3:
	mov	[fdfnum], eax

	sys	_read, eax, bsbuffer, 512
	jc	short df_4

	; check Retro UNIX signature on boot sector
	cmp	dword [bsFSystemID], 'RUFS'
	jne	short df_4
	;cmp	word [bsfdsign], 'fd'	
	;jne	short df_4

	sys	_read, [fdfnum], sbbuffer
	jnc	short df_5

df_4:
	;sys	_close, [fdfnum]
	sys	_close
	jmp	df_err

df_5:
	; write volume serial number (as hex string)
	mov	esi, bsVolSerial
	mov	edi, volserialstr
	mov	cl, 2
	mov	ch, cl ; 2
df_6:
	lodsb
	mov	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	shr	al, 4
	mov	bl, al
	mov	al, [ebx+hexchrs]
	push	eax
	dec	cl
	jnz	short df_6
	dec	ch
	jz	short df_7
	mov	cl, 2
	jmp	short df_6
df_7:
	pop	eax
	stosw
	pop	eax
	stosw
	mov	al, '-'
	stosb
	pop	eax
	stosw
	pop	eax
	stosw
	
	mov	al, 'h'
	stosb

	mov	ax, 0A0Dh ; CRLF
	stosw
	
	;xor	al, al
	;stosb
	
	sys	_write, 1, nextline, 2

	;mov	ecx, fdname	
	;mov	dl, 8
	;sys	_write

	sys	_msg, fdname, 8, 0Fh

	;mov	ecx, volserialhdr
	;mov	dl, size_vsnhdr
	;sys	_write

	sys	_write, 1, volserialhdr, size_vsnhdr
	
	mov	ecx, volserialstr
	mov	dl, 12 ; xxxx-xxxxh, CR, LF
	sys	_write

df_8:	
	sys	_close, [fdfnum]
	;	

	mov	esi, freeb
	mov	bx, [nfree]
	sub	edi, edi
	xor	ecx, ecx
	;xor	edx, edx
	sub	dl, dl
	push	ebx
df_9:
	lodsb
	mov	cl, 8	
df_10:
	shr	al, 1
	jnc	short df_11
	inc	edx
	or	edi, edi
	jnz	short df_11
	; save first free block number
	mov	edi, esi
	sub	edi, freeb
	shl	edi, 3 ; * 8
	sub	edi, ecx
df_11:		
	loop	df_10
	dec	ebx
	jnz	short df_9

	pop	eax
	push	edi ; first free block
	push	edx ; free blocks

	shl	eax, 3  ; number of blocks (volume size)

	call	decimal_number
	; esi = start of decimal number string
	; edi = byte count of decimal number string

	sys	_write, 1, volszhdr, size_volsz

	;sys	_write, 1, esi, edi
		
	mov	ecx, esi
	mov	edx, edi
	sys	_write

	pop	eax ; free blocks
	
	call	decimal_number

	sys	_write, 1, fblkshdr, size_fblks

	;sys	_write, 1, esi, edi

	mov	ecx, esi
	mov	edx, edi
	sys	_write

	pop	eax ; first free block
	
	call	decimal_number

	sys	_write, 1, ffblkhdr, size_ffblk

	;sys	_write, 1, esi, edi

	mov	ecx, esi
	mov	edx, edi
	sys	_write

	;;sys	_write, 1, nextline, 2
	;mov	ecx, nextline
	;mov	dl, 2
	;sys	_write 	

	jmp	hang  ; terminate 

; ----------------------------------------------------------------

decimal_number:
	; eax = binary number
	
	sub	ecx, ecx

	mov	esi, num_str
	mov	edi, esi

	push	ecx ; 0

	mov	cl, 10
dn_1:
	sub	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	and	eax, eax
	jnz	short dn_1
dn_2:
	pop	eax
	and	eax, eax
	jz	short dn_3
	stosb
	jmp	short dn_2
dn_3:
	mov	ax, 0A0Dh  ; CRLF
	stosw
	
	sub	edi, esi
	retn		

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 386 v1 DF Utility by Erdogan TAN - 30/06/2022'
	db  0Dh, 0Ah, 0
usage_msg:
	db  0Dh, 0Ah
	db  'Usage: df <disk>'
	db  0Dh, 0Ah
	db  0Dh, 0Ah
	db  'Disk Names: fd0, fd1, 0 (fd0), 1 (fd1)'
	;db  0Dh, 0Ah
nextline:
	db  0Dh, 0Ah, 0

size_pmsg equ usage_msg-(program_msg+1)

fdname:
	db "/dev/fdx", 0

size_umsg equ fdname-(usage_msg+1)

volserialhdr:
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Volume Serial Number : " 
	db 0

size_vsnhdr equ volszhdr-(volserialhdr+1) 

volszhdr:
	;;db 0Dh, 0Ah
	;db 0Dh, 0Ah
	db "Volume Size          : "
	db 0

size_volsz equ fblkshdr-(volszhdr+1) 
  
fblkshdr:
	;;db 0Dh, 0Ah
	;db 0Dh, 0Ah
	db "Free Blocks          : "
	db 0

size_fblks equ ffblkhdr-(fblkshdr+1) 
  
ffblkhdr:
	;;db 0Dh, 0Ah
	;db 0Dh, 0Ah
	db "First Free Block     : "
	db 0

size_ffblk equ error_msg-(ffblkhdr+1) 

error_msg:
	db 0Dh, 0Ah	
	db "Error !"
 	db 0Dh, 0Ah, 0  

hexchrs:
	db '0123456789ABCDEF', 0

size_emsg equ hexchrs-(error_msg+1)

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

fdfnum:
	resd 1

bsbuffer:
	resb 512

bsFSystemID equ bsbuffer+2
bsVolSerial equ bsbuffer+6
;bsfdsign   equ bsbuffer+10

sbbuffer:
	resb 512

nfree equ sbbuffer
freeb equ sbbuffer+2

volserialstr:
	resb 16  ; xxxx-xxxxh, CRLF, 0

num_str:
	resb 16

bss_end:

;-----------------------------------------------------------------
; 30/06/2022
;-----------------------------------------------------------------
; Original UNIX v2 - df (disk free blocks info) source code
;-----------------------------------------------------------------
; [ svntree-20081216.tar.gz - bin/df (archive date: 21-11-1972) ]

;/ df -- find free space
;
;	cmp	(sp)+,$1
;	bgt	1f
;	mov	$rf0,0f
;	jsr	pc,df
;	mov	$1,r0
;	sys	write; plus; 1
;	mov	$rk1,0f
;	jsr	pc,df
;	mov	$1,r0
;	sys	write; plus; 1
;	mov	$rk2,0f
;	jsr	pc,df
;	mov	$1,r0
;	sys	write; plus; 1
;	mov	$rk3,0f
;	jsr	pc,df
;	mov	$1,r0
;2:
;	mov	$1,r0
;	sys	write; nl; 1
;	sys	exit
;
;1:
;	tst	(sp)+
;	mov	(sp)+,0f
;	jsr	pc,df
;	br	2b
;
;df:
;	clr	r3
;	sys	36.
;	sys	open; 0:..; 0
;	bes	9f
;	sys	read; nfree; 1024.
;	mov	$freeb,r1
;	mov	nfree,r2
;	asr	r2
;1:
;	mov	$16.,r4
;	mov	(r1)+,r5
;2:
;	rol	r5
;	adc	r3
;	dec	r4
;	bne	2b
;	dec	r2
;	bgt	1b
;9:
;	clr	r2
;	dvd	$10.,r2
;	mov	r3,-(sp)
;	mov	r2,r3
;	beq	2f
;	jsr	pc,9b
;2:
;	movb	(sp)+,ch
;	add	$'0,ch
;	mov	$1,r0
;	sys	write; ch; 1
;	rts	pc
;
;rf0:	</dev/rf0\0>
;rk0:	</dev/rk0\0>
;rk1:	</dev/rk1\0>
;rk2:	</dev/rk2\0>
;rk3:	</dev/rk3\0>
;plus:	<+>
;nl:	<\n>
;	.even
;
;	.bss
;ch:	.=.+2
;nfree:	.=.+2
;freeb:	.=.+1022.