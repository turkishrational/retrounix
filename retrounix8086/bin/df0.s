; ****************************************************************************
; df0.s (df8086.s) - print free blocks - by Erdogan Tan - 30/06/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - file system (disk free blocks info) utility 
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
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>kj

;-----------------------------------------------------------------
;  code
;-----------------------------------------------------------------

[BITS 16] ; 16-bit intructions (for x86 real mode)

[ORG 0] 

START_CODE:
	; 30/06/2022
	pop	cx ; cx = number of arguments
	;
	pop	ax ; ax = argument 0 = executable file name
	;
	;dec	cx
	dec	cl
	jng	short df_0

	pop	si

	lodsw	

	cmp	ax, 'fd'
	jne	short df_2
	cmp	byte [si], 0
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
	sys	_write, 1, program_msg, size_pmsg
	;sys	_msg, program_msg, size_pmsg, 0Fh 
df_1:
	sys	_write, 1, usage_msg, size_umsg

hang:
	sys	_exit
	jmp	short hang

df_3:
	mov	[fdfnum], ax

	sys	_read, ax, bsbuffer, 512
	jc	short df_4

	; check Retro UNIX signature on boot sector
	cmp	word [bsFSystemID], 'RU'
	jne	short df_4
	cmp	word [bsFSystemID+2], 'FS'
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
	mov	si, bsVolSerial
	mov	di, volserialstr
	mov	cl, 2
	mov	ch, cl ; 2
df_6:
	lodsb
	mov	bl, al
	and	bl, 0Fh
	mov	ah, [bx+hexchrs]
	shr	al, 4
	mov	bl, al
	mov	al, [bx+hexchrs]
	push	ax
	dec	cl
	jnz	short df_6
	dec	ch
	jz	short df_7
	mov	cl, 2
	jmp	short df_6
df_7:
	pop	ax
	stosw
	pop	ax
	stosw
	mov	al, '-'
	stosb
	pop	ax
	stosw
	pop	ax
	stosw
	
	mov	al, 'h'
	stosb

	mov	ax, 0A0Dh ; CRLF
	stosw
	
	;xor	al, al
	;stosb
	
	sys	_write, 1, nextline, 2

	mov	cx, fdname	
	mov	dl, 8
	sys	_write

	;sys	_msg, fdname, 8, 0Fh

	;mov	cx, volserialhdr
	;mov	dl, size_vsnhdr
	;sys	_write

	sys	_write, 1, volserialhdr, size_vsnhdr
	
	mov	cx, volserialstr
	mov	dl, 12 ; xxxx-xxxxh, CR, LF
	sys	_write

df_8:	
	sys	_close, [fdfnum]
	;	

	mov	si, freeb
	mov	bx, [nfree]
	sub	di, di
	xor	cx, cx
	;xor	dx, dx
	sub	dl, dl
	push	bx
df_9:
	lodsb
	mov	cl, 8	
df_10:
	shr	al, 1
	jnc	short df_11
	inc	dx
	or	di, di
	jnz	short df_11
	; save first free block number
	mov	di, si
	sub	di, freeb
	shl	di, 3 ; * 8
	sub	di, cx
df_11:		
	loop	df_10
	dec	bx
	jnz	short df_9

	pop	ax
	push	di ; first free block
	push	dx ; free blocks

	shl	ax, 3  ; number of blocks (volume size)

	call	decimal_number
	; si = start of decimal number string
	; di = byte count of decimal number string

	sys	_write, 1, volszhdr, size_volsz

	;sys	_write, 1, si, di
		
	mov	cx, si
	mov	dx, di
	sys	_write

	pop	ax ; free blocks
	
	call	decimal_number

	sys	_write, 1, fblkshdr, size_fblks

	;sys	_write, 1, si, di

	mov	cx, si
	mov	dx, di
	sys	_write

	pop	ax ; first free block
	
	call	decimal_number

	sys	_write, 1, ffblkhdr, size_ffblk

	;sys	_write, 1, si, di

	mov	cx, si
	mov	dx, di
	sys	_write

	;;sys	_write, 1, nextline, 2
	;mov	cx, nextline
	;mov	dl, 2
	;sys	_write 	

	jmp	hang  ; terminate 

; ----------------------------------------------------------------

decimal_number:
	; ax = binary number
	
	sub	cx, cx

	mov	si, num_str
	mov	di, si

	push	cx ; 0

	mov	cl, 10
dn_1:
	sub	dx, dx
	div	cx
	add	dl, '0'
	push	dx
	and	ax, ax
	jnz	short dn_1
dn_2:
	pop	ax
	and	ax, ax
	jz	short dn_3
	stosb
	jmp	short dn_2
dn_3:
	mov	ax, 0A0Dh  ; CRLF
	stosw
	
	sub	di, si
	retn		

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 8086 v1 DF Utility by Erdogan TAN - 30/06/2022'
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
	resw 1

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
	resb 14  ; xxxx-xxxxh, CRLF, 0

num_str:
	resb 12

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