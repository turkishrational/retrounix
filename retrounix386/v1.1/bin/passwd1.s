; ****************************************************************************
; passwd386.s (passwd1.s) - by Erdogan Tan - 30/04/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 (& v1.1) - passwd - change user's password
;
; [ Last Modification: 01/05/2022 ]
;
; Derived from (original) UNIX v5 'passwd.s' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; v5root.tar.gz
; ****************************************************************************
; [ usr/source/s2/passwd.s (archive date: 27-11-1974) ]

; passwd0.s - Retro UNIX 8086 v1 (16 bit version of 'passwd1.s')
; passwd1.s - Retro UNIX 386 v1 & v1.1 (unix v1 inode structure)
; passwd2.s - Retro UNIX 386 v1.2 (& v2) (modified unix v7 inode)

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

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 01/05/2022
	; 30/04/2022

	;cmp	(sp)+,$3
	;bge	1f
	;jsr	r5,mesg
	;	<Usage: passwd uid password\n\0>; .even
	;sys	exit

	pop	eax ; eax = number of arguments

	;cmp	eax, 3
	cmp	al, 3
	jnb	short pswd_1

	mov	eax, usage_msg
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

pswd_1:
;1:
	;tst	(sp)+
	;mov	(sp)+,uidp
	;mov	(sp)+,r0
	;tstb	(r0)
	;beq	1f
	;jsr	pc,crypt
	;clrb	8(r0)

	pop	eax ; argument 0 - binary file name

	pop	dword [uidp] ; argument 1 - user id
	pop	esi ; argument 2 - password

	; 01/05/2022
	call	strlen 
	cmp	al, 8
	ja	short max_8_chars

	call	crypt
	; esi = encyrpted password address
	mov	byte [esi+8], 0
pswd_2:
;1:
	;mov	r0,cryptp
	;mov	$passwf,r0
	;jsr	r5,fopen; ibuf
	;bec	1f
	;jsr	r5,mesg
	;	<cannot open password file\n\0>; .even
	;sys	exit

	mov	[cryptp], esi
	
	sys	_open, passwf, 0
	jnc	short pswd_3

	mov	eax, cant_open_msg
write_msg_and_exit:
	call	print_msg
	sys	_exit

;hangemhigh:
;	nop
;	jmp	short hangemhigh

max_8_chars:
	mov	eax, long_pswd_msg
	jmp	short write_msg_and_exit

pswd_3:
;1:
	; 30/04/2022
	;mov	[ibuf], ax  ; file descriptor
	mov	[ibuf], eax  ; file descriptor
pswd_4:
	;sys	stat; tempf; obuf+20.
	;bec	2f
	;sys	creat; tempf; 222
	;bec	1f

	sys	_stat, tempf, obuf+20
	jnc	short pswd_5

	; set write permission only
	sys	_creat, tempf, 101b ; unix v1 inode
	;sys	_creat, tempf, 10010010b ; runix v2 inode
	jnc	short pswd_6
pswd_5:
;2:
	;jsr	r5,mesg
	;	<temp file busy -- try again\n\0>; .even
	;sys	exit

	mov	eax, tmpf_bsy_msg
	jmp	short write_msg_and_exit

pswd_6:
;1:
	;mov	r0,obuf
	
	;mov	[obuf], ax ; file descriptor
	mov	[obuf], eax ; file descriptor

; / search for uid

comp:
	;mov	uidp,r1
	mov	esi, [uidp] ; 01/05/2022
cmp_1:
;1:
	;jsr	pc,pcop
	;cmp	r0,$':
	;beq	1f
	;cmpb	r0,(r1)+
	;beq	1b

	call	pcop
	cmp	al, ':'
	je	short cmp_3
	mov	ah, al
	lodsb
	cmp	al, ah
	je	short cmp_1
cmp_2:
;2:
	;jsr	pc,pcop
	;cmp	r0,$'\n
	;bne	2b
	;br	comp

	call	pcop
	; (skip remain bytes on row, get next line/row)
	; check cr byte of crlf (end of line chars)
	cmp	al, EnterKey  ; cmp al, 0Dh
	jne	short cmp_2
	; get lf byte of crlf out
	call	pcop
	jmp	short comp ; next line
	
cmp_3:
;1:
	;tstb	(r1)+
	;bne	2b

	; check end of uid input (match condition)
	lodsb	
	or	al, al
	jnz	short cmp_2 ; uid is not same
			; skip remain bytes on line/row

	; uid (input) matches with uid in passwd file 
	
; / skip over old password			

pswd_7:
;1:
	;jsr	pc,pget
	;cmp	r0,$':
	;bne	1b

	call	pget
	cmp	al, ':'
	jne	short pswd_7

; / copy in new password

	;mov	cryptp,r1	
	mov	esi, [cryptp] ; ptr to encyrpted passwd
pswd_8:
;1:	
	;movb	(r1)+,r0
	;beq	1f
	;jsr	pc,pput
	;br	1b

	lodsb
	and	al, al
	jz	short pswd_9
	call	pput
	jmp	short pswd_8
pswd_9:
;1:
	;mov	$':,r0
	;jsr	pc,pput

	mov	al, ':'
	call	pput

; / validate permission

	;clr	r1
	sub	ecx, ecx ; 0
	mov	edi, 10
pswd_10:	
;1:
	;jsr	pc,pcop
	;cmp	r0,$':
	;beq	1f
	;mpy	$10.,r1
	;sub	$'0,r0
	;add	r0,r1
	;br	1b

	push	ecx
	call	pcop
	pop	ecx
	; (eax <= 255)
	cmp	al, ':'
	je	short pswd_11
	xchg	eax, ecx
	mul	edi ; * 10
	sub	cl, '0'
	add	ecx, eax
	jmp	short pswd_10	

pswd_11:
;1:
	;sys	getuid
	;tst	r0
	;beq	1f

	; ecx = uid (as in passwd file)

	sys	_getuid

	;or	al, al
	;;or	ax, ax
	or	eax, eax
	jz	short pswd_12 ; root (superuser)	

	;cmp	r0,r1
	;beq	1f
	
	;cmp	cl, al
	;;cmp	cx, ax
	cmp	ecx, eax
	je	short pswd_12

	;jsr	r5,mesg
	;	<permission denied\n\0>; .even
	;br	done

	mov	eax, p_denied_msg
	call	print_msg
	jmp	short done

pswd_12:
;1:
	;inc	sflg

	; set 1st stage (cmpleted) flag
	inc	byte [sflg] ; 1st stage is ok
pswd_13:
;1:
	;jsr	pc,pcop
	;br	1b
	
	call	pcop
	
	; pcop will return/jump to 'done'
	; after the last byte of (old) passwd file
	; (call return address will be discarded)
	
	; (but if there is a next byte to read/write
	;  cpu will return here)
	
	jmp	short pswd_13 ; r/w next byte

; ---------------------------

done:
	;jsr	r5,flush; obuf
	;mov	obuf,r0
	;sys	close
	
	;mov	ebx, obuf
	call	flush 
		; (write buffer content to disk)
	movzx	ebx, word [obuf] 
	sys	_close ; (close output file)

	;mov	ibuf,r0
	;sys	close
	
	;movzx	ebx, word [ibuf] 
	mov	bx, [ibuf]
	sys	_close ; (close input file)

	;tst	sflg
	;beq	1f
	;tst	dflg
	;bne	1f
	;inc	dflg

	test	byte [sflg], 0FFh
	jz	short done_4 ; 1st stage failed 
			     ; unlink/remove tempf
	; 1st stage is ok
	test	byte [dflg], 0FFh
	jnz	short done_4 ; 2nd stage is ok (completed)
	
	; 2nd stage
	; (writing to tempf at 1st stage is ok)	
	inc	byte [dflg]  ; set 2nd stage flag 
			     ; (open tempf for read and
			     ;  write to new passwd file)
	;mov	$tempf,r0
	;jsr	r5,fopen; ibuf
	;bec	2f

	sys	_open, tempf, 0 ; open tempf for read
	jnc	short done_1		

	;jsr	r5,mesg
	;	<cannot reopen temp file\n\0>; .even
	;br	1f

	mov	eax, cnro_tmpf_msg
	call	print_msg
	jmp	short done_4
done_1:
;2:
	mov	[ibuf], eax
	xor	eax, eax ; 0
	mov	[ibuf+4], eax

	;mov	$passwf,r0
	;jsr	r5,fcreat; obuf
	;bec	2f

	; retro unix v1 inode
	sys	_creat, passwf, 1100b ; rw--
	; retro unix v2 inode
	;sys	_creat, passwf, 110000000b ; rw-------
	jnc	short done_2

	;jsr	r5,mesg
	;	<cannot reopen password file\n\0>; .even
	;br	1f

	mov	eax, cnro_pswdf_msg
	call	print_msg
	jmp	short done_4
done_2:
;2:
	;jsr	pc,pcop
	;br	2b

	mov	[obuf], eax
	sub	eax, eax ; 0
	mov	[obuf+4], eax
done_3:	; 01/05/2022
	call	pcop
	jmp	short done_3

done_4:
;1:
	;sys	unlink; tempf
	;sys	exit

	sys	_unlink, tempf
	sys	_exit

; ---------------------------

pput:
	;jsr	r5,putc; obuf
	;rts	pc

	;mov	ebx, obuf
	;call	putc
	;retn
	jmp	putc

; ---------------------------

pget:
	;jsr	r5,getc; ibuf
	;bes	1f
	;rts	pc

	;mov	ebx, ibuf
	call	getc
	jc	short pget_1
	retn
pget_1:
;1:
	;jsr	r5,mesg
	;	<format error on password file\n\0>; .even
	;br	done

	mov	eax, format_err_msg
	call	print_msg

	jmp	done

; ---------------------------

	; 30/04/2022
pcop:
	;jsr	r5,getc; ibuf
	;bes	1f
	;jsr	r5,putc; obuf
	;rts	pc

	;mov	ebx, ibuf
	call	getc
	jc	short pcop_1
	;mov	ebx, obuf
	;call	putc
	;retn
	jmp	putc
pcop_1:
;1:
	;tst	sflg
	;bne	1f
	;jsr	r5,mesg
	;	<uid not valid\n\0>; .even

	test	byte [sflg], 0FFh
	jnz	short pcop_2

	mov	eax, not_valid_msg
	call	print_msg
pcop_2:
;1:
	;br	done
	pop	eax ; discard call return addr
	jmp	done

; ---------------------------

print_msg:
	; 01/05/2022
	; 29/04/2022 
	; Modified registers: eax, ebx, ecx, edx

	call	_strlen ; 01/05/2022
print_str:
	mov	edx, ebx
	sys	_write, 1, eax
	;
	retn

	; 01/05/2022
	; 29/04/2022
_strlen:
	; eax = asciiz string address
	mov	ebx, eax
	dec	ebx
nextchr:
	inc	ebx
	cmp	byte [ebx], 0
	ja	short nextchr
	;cmp	[ebx], 0Dh
	;ja	short nextchr
	sub	ebx, eax
	; bx = asciiz string length
	retn

	; 01/05/2022
strlen:
	mov	eax, esi
	call	_strlen
	mov	eax, ebx
	retn

; ---------------------------------------------------
; 30/04/2022
; 'crypt' assembly source code
; copied from: 'login386.s' (Erdogan Tan, 27/02/2022)
; ---------------------------------------------------
 
;/ crypt -- password incoding
;
;; Original Unix v5 (PDP-11) 'crypt'
;; code has been converted to 
;; Retro UNIX 8086 v1 'crypt' 
;; procedure in 'login.asm'
;; (by Erdogan Tan - 12/11/2013).
; 
;
;crypt:
;	mov	r1,-(sp)
;	mov	r2,-(sp)
;	mov	r3,-(sp)
;	mov	r4,-(sp)
;	mov	r5,-(sp)
;
;	mov	r0,r1
;	mov	$key,r0
;	movb	$004,(r0)+
;	movb	$034,(r0)+

; 14/10/2015 - 32 bit version (Retro UNIX 386 v1)

crypt:
	; 30/04/2022
	; INPUT: 
	;	esi = password string address
	; OUTPUT:
	;	esi = encyrpted password str address
	;
	; Modified registers:
	;	eax, ebx, ecx, edx, esi, edi, ebp
 
	;mov	esi, passwd
	mov	edi, key
	mov	al, 4
	stosb
	mov	al, 28
	stosb

;1:
;	cmp	r0,$key+64.
;	bhis	1f
;	movb	(r1)+,(r0)+
;	bne	1b
;1:
;	dec	r0

cryp0:
	lodsb
	stosb
	and	al, al
	jz	short cryp1
	cmp	edi, key + 64
	jb	short cryp0
cryp1:
 	dec	edi
;/
;/
;/	fill out key space with clever junk
;/
;	mov	$key,r1
;1:
;	movb	-1(r0),r2
;	movb	(r1)+,r3
;	xor	r3,r2
;	movb	r2,(r0)+
;	cmp	r0,$key+128.
;	blo	1b


;/	fill out key space with clever junk

	mov	esi, key
cryp2:
	mov	bl, [edi-1]
	lodsb
	xor	al, bl
	stosb
	cmp	edi, key + 128
	jb	short cryp2
	;
;/
;/
;/	establish wheel codes and cage codes
;/
;	mov	$wheelcode,r4
;	mov	$cagecode,r5
;	mov	$256.,-(sp)
;2:
;	clr	r2
;	clr	(r4)
;	mov	$wheeldiv,r3
;3:
;	clr	r0
;	mov	(sp),r1
;	div	(r3)+,r0
;	add	r1,r2
;	bic	$40,r2
;	bis	shift(r2),(r4)
;	cmp	r3,$wheeldiv+6.
;	bhis	4f
;	bis	shift+4(r2),(r5)
;4:
;	cmp	r3,$wheeldiv+10.
;	blo	3b
;	sub	$2,(sp)
;	tst	(r4)+
;	tst	(r5)+
;	cmp	r4,$wheelcode+256.
;	blo	2b
;	tst	(sp)+
;/	

;/	establish wheel codes and cage codes

	mov	esi, wheelcode
	mov	edi, cagecode
	mov	ax, 256
	push	ax ; *
	mov	ebp, esp
cryp3:
	sub	dx, dx ; 0
	mov	[esi], dx ; 0
	mov	ebx, wheeldiv
cryp4:
	mov	ax, [ebp]	
	mov 	cl, [ebx]
	div	cl
	add	dl, ah
	inc	ebx
	and	dl, 01Fh
	push	ebx
	mov	ebx, shift
	add	ebx, edx
	mov	ax, [ebx] 
	or	[esi], ax
	pop	ecx
	cmp	ecx, wheeldiv + 3
	jnb	short cryp5
	add	ebx, 4
	mov	ax, [ebx]
	or	[edi], ax 	 
cryp5:
	mov	ebx, ecx
	cmp	ebx, wheeldiv + 5
	jb	short cryp4
	sub	word [ebp], 2
	lodsw
	inc	edi
	inc	edi
	cmp	esi, wheelcode + 256
	jb	short cryp3
	pop	ax ; *

;	.data
;shift:	1;2;4;10;20;40;100;200;400;1000;2000;4000;10000;20000;40000;100000
;	1;2
;wheeldiv: 32.; 18.; 10.; 6.; 4.
;	.bss
;cagecode: .=.+256.
;wheelcode: .=.+256.
;	.text
;/
;/
;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
;/
;	mov	$key,r0
;	mov	$cage,r2
;	mov	$wheel,r3
;1:
;	movb	(r0)+,r1
;	bic	$!177,r1
;	asl	r1
;	mov	cagecode(r1),(r2)+
;	mov	wheelcode(r1),(r3)+
;	cmp	r0,$key+128.
;	blo	1b

;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
cryp6:
        mov     ebx, key
        mov     esi, cage
        mov     edi, wheel
cryp7:
        mov     cl, [ebx]
        inc     ebx
        and     ecx, 7Fh
	shl	cl, 1
        xchg    ecx, ebx
        mov     ax, [ebx+cagecode]
        mov     [esi], ax
        inc     esi
        inc     esi
        mov     ax, [ebx+wheelcode]
	stosw
        mov     ebx, ecx
        cmp     ebx, key + 128
	jb	short cryp7
;/
;/
;/	now spin the cage against the wheel to produce output.
;/
;	mov	$word,r4
;	mov	$wheel+128.,r3
;3:
;	mov	-(r3),r2
;	mov	$cage,r0
;	clr	r5
;1:
;	bit	r2,(r0)+
;	beq	2f
;	incb	r5
;2:
;	cmp	r0,$cage+256.
;	blo	1b

;/
;/	now spin the cage against the wheel to produce output.
;/
cryp8:
        mov     edi, _word
        mov     ebx, wheel + 128
cryp9:
        dec     ebx
        dec     ebx
        mov     dx, [ebx]
        mov     esi, cage
	sub	cx, cx ; 0
cryp10:
	lodsw
	test	ax, dx
	jz	short cryp11
	inc	cl
cryp11:
        cmp     esi, cage + 256
	jb	short cryp10

;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
;	mov	r4,-(sp)
;	clr	r4
;	div	$26.+26.+10.,r4
;	add	$'0,r5
;	cmp	r5,$'9
;	blos	1f
;	add	$'A-'9-1,r5
;	cmp	r5,$'Z
;	blos	1f
;	add	$'a-'Z-1,r5
;1:
;	mov	(sp)+,r4
;	movb	r5,(r4)+
;	cmp	r4,$word+8.
;	blo	3b
;/
;
;	mov	(sp)+,r5
;	mov	(sp)+,r4
;	mov	(sp)+,r3
;	mov	(sp)+,r2
;	mov	(sp)+,r1
;	mov	$word,r0
;	rts	pc
;	.bss
;key:	.=.+128.
;word:	.=.+32.
;cage:	.=.+256.
;wheel:	.=.+256.

;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
	mov	ax, cx
	mov	dl, 26+26+10
	div	dl
	mov	al, ah
	add	al, '0'
	cmp	al, '9'
	jna	short cryp12
	add	al, 'A'-'9'-1
	cmp	al, 'Z'
	jna	short cryp12
	add	al, 'a'-'Z'-1
cryp12:
	stosb	
        cmp     edi, _word + 8
	jb	short cryp9
        mov     esi, _word
	retn

; ---------------------------------------------------
; 30/04/2022
; 'getc' assembly source code
; copied from: 'chown1.s' (Erdogan Tan, 30/04/2022)
;	(derived from unix v5 'get.s')
; ---------------------------------------------------

getc:
	; 30/04/2022
	; 29/04/2022

	; INPUT:
	;    ibuf = read buffer (header) address
	; OUTPUT:
	;    al = character (if cf=0)
	;    (if cf = 1 -> read error)

	; Modified registers: eax, ebx, ecx, edx, ebp	

	mov	ebp, ibuf
	;mov	ax, [ebp+2]
	movzx	eax, word [ebp+2] ; char count
	;and	ax, ax
	and	eax, eax
	jnz	short gch1
gch0:
	movzx	ebx, word [ebp]
	mov	ecx, ibuf+8 ; read buff. (data) addr.
	mov 	[ebp+4], ecx ; char offset
	;mov	[ebp+2], ax ; 0
	sub	edx, edx
	mov	dh, 2 
	;mov 	edx, 512 
	sys	_read ; sys _read, ebx, ecx, edx
	jc	short gch2
	or	eax, eax
	;jz	short gch3
	jnz	short gch1
	;
	stc
	retn
gch1:
	;dec	ax
	dec	eax
	mov	[ebp+2], ax
	mov	ebx, [ebp+4]
	;xor	eax, eax
	xor	ah, ah
	mov	al, [ebx]
	inc	ebx
	mov	[ebp+4], ebx
	;;xor	ah, ah
	;retn 	
gch2:
	;;xor	ax, ax
	;xor	eax, eax
	retn
;gch3:
	;stc
	;retn
	
; ---------------------------------------------------
; 30/04/2022 (Erdogan Tan)
; 'putc' assembly source code 
;	(derived from unix v5 'put.s')
; ---------------------------------------------------

putc:
	; 30/04/2022
	; 29/04/2022

	; INPUT:
	;      al = character (to be written)
	;    obuf = write buffer (header) address
	; OUTPUT:
	;    al = character (if cf=0)
	;    (if cf = 1 -> write error)

	; Modified registers: eax, ebx, ecx, edx, ebp	

	mov	ebp, obuf
pch0:
	dec	word [ebp+2] ; char count
	jge	short pch1
	push	eax
	call	_fl_
	pop	eax
	jmp	short pch0
pch1:
	mov	ebx, [ebp+4] ; char offset
	mov	[ebx], al
	;inc	ebx
	;mov	[ebp+4], ebx
	inc	dword [ebp+4]
	retn
flush:
	mov	ebp, obuf
_fl_:
	mov	edx, ebp ; buffer header address
	add	edx, 8 ; + 8
	; edx = buffer data address
	push	edx
	mov	eax, [ebp+4] ; char offset
	or 	eax, eax
	jz	short pch2 ; empty/new buffer
	sub	eax, edx ; char count
	; [ebp] = file descriptor
	movzx	ecx, word [ebp]
	;movzx	ecx, byte [ebp] ; < 256
	;		; (<=10 for retro unix v1)
	sys	_write, ecx, edx, eax
pch2:
	pop	dword [ebp+4]; character offset
	mov	word [ebp+2], 512 
			; available char count
			; to write in buffer
			; (before flushing)
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;align 4

; 30/04/2022

; cryprt.s

shift:	dw 1, 2, 4, 8, 16, 32, 64, 128, 256, 512
	dw 1024, 2048, 4096, 8192, 16384, 32768
	dw 1, 2
wheeldiv:
	db 32, 18, 10, 6, 4

; passwd.s

passwf:	db "/etc/passwd", 0 ; password file
tempf:	db "/tmp/ptmp", 0 ; temporary file	

usage_msg:
	db 0Dh, 0Ah 
	db "Usage: passwd uid password", 0Dh, 0Ah, 0
cant_open_msg:
	db 0Dh, 0Ah
	db "cannot open password file", 0Dh, 0Ah, 0
tmpf_bsy_msg:
	db 0Dh, 0Ah
	db "temp file busy -- try again", 0Dh, 0Ah, 0
p_denied_msg:
	db 0Dh, 0Ah
	db "permission denied", 0Dh, 0Ah, 0
cnro_tmpf_msg:
	db 0Dh, 0Ah
	db "cannot reopen temp file", 0Dh, 0Ah, 0
cnro_pswdf_msg:
	db 0Dh, 0Ah
	db "cannot reopen password file", 0Dh, 0Ah, 0
format_err_msg:
	db 0Dh, 0Ah
	db "format error on password file", 0Dh, 0Ah, 0
not_valid_msg:
	db 0Dh, 0Ah
	db "uid not valid", 0Dh, 0Ah, 0

	; 01/05/2022
long_pswd_msg:
	db 0Dh, 0Ah
	db "password length > 8 chars", 0Dh, 0Ah, 0	

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

; 30/04/2022

; crypt.s

key:	resb 128
;_word:	resb 32
_word:	resb 10
	resb 2
cage:	resb 256
wheel:	resb 256
cagecode:  resw 256 ; resb 256
wheelcode: resw 256 ; resb 256

; passwd.s

ibuf:	resb 520
obuf:	resb 520
cryptp:	resw 1
	resw 1 ; 32 bit pointer for Retro UNIX 386 v1
uidp:	resw 1
	resw 1 ; 32 bit pointer for Retro UNIX 386 v1
sflg:	resb 1 ; resw 1
dflg:	resb 1 ; resw 1

; 30/04/2022

;-----------------------------------------------------------------
; Original UNIX v5 - /bin/passwd source code (passwd.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;
;/ passwd -- change user's password
;
;.globl	mesg
;.globl	crypt
;.globl	getc
;.globl	flush
;.globl	fcreat
;.globl	putc
;.globl	fopen
;
;	cmp	(sp)+,$3
;	bge	1f
;	jsr	r5,mesg
;		<Usage: passwd uid password\n\0>; .even
;	sys	exit
;1:
;	tst	(sp)+
;	mov	(sp)+,uidp
;	mov	(sp)+,r0
;	tstb	(r0)
;	beq	1f
;	jsr	pc,crypt
;	clrb	8(r0)
;1:
;	mov	r0,cryptp
;	mov	$passwf,r0
;	jsr	r5,fopen; ibuf
;	bec	1f
;	jsr	r5,mesg
;		<cannot open password file\n\0>; .even
;	sys	exit
;1:
;	sys	stat; tempf; obuf+20.
;	bec	2f
;	sys	creat; tempf; 222
;	bec	1f
;2:
;	jsr	r5,mesg
;		<temp file busy -- try again\n\0>; .even
;	sys	exit
;1:
;	mov	r0,obuf
;
;/ search for uid
;
;comp:
;	mov	uidp,r1
;1:
;	jsr	pc,pcop
;	cmp	r0,$':
;	beq	1f
;	cmpb	r0,(r1)+
;	beq	1b
;2:
;	jsr	pc,pcop
;	cmp	r0,$'\n
;	bne	2b
;	br	comp
;1:
;	tstb	(r1)+
;	bne	2b
;
;/ skip over old password
;
;1:
;	jsr	pc,pget
;	cmp	r0,$':
;	bne	1b
;
;/ copy in new password
;
;	mov	cryptp,r1
;1:
;	movb	(r1)+,r0
;	beq	1f
;	jsr	pc,pput
;	br	1b
;1:
;	mov	$':,r0
;	jsr	pc,pput
;
;/ validate permission
;
;	clr	r1
;1:
;	jsr	pc,pcop
;	cmp	r0,$':
;	beq	1f
;	mpy	$10.,r1
;	sub	$'0,r0
;	add	r0,r1
;	br	1b
;1:
;	sys	getuid
;	tst	r0
;	beq	1f
;	cmp	r0,r1
;	beq	1f
;	jsr	r5,mesg
;		<permission denied\n\0>; .even
;	br	done
;1:
;	inc	sflg
;1:
;	jsr	pc,pcop
;	br	1b
;
;done:
;	jsr	r5,flush; obuf
;	mov	obuf,r0
;	sys	close
;	mov	ibuf,r0
;	sys	close
;	tst	sflg
;	beq	1f
;	tst	dflg
;	bne	1f
;	inc	dflg
;	mov	$tempf,r0
;	jsr	r5,fopen; ibuf
;	bec	2f
;	jsr	r5,mesg
;		<cannot reopen temp file\n\0>; .even
;	br	1f
;2:
;	mov	$passwf,r0
;	jsr	r5,fcreat; obuf
;	bec	2f
;	jsr	r5,mesg
;		<cannot reopen password file\n\0>; .even
;	br	1f
;2:
;	jsr	pc,pcop
;	br	2b
;1:
;	sys	unlink; tempf
;	sys	exit
;
;pput:
;	jsr	r5,putc; obuf
;	rts	pc
;
;pget:
;	jsr	r5,getc; ibuf
;	bes	1f
;	rts	pc
;1:
;	jsr	r5,mesg
;		<format error on password file\n\0>; .even
;	br	done
;
;pcop:
;	jsr	r5,getc; ibuf
;	bes	1f
;	jsr	r5,putc; obuf
;	rts	pc
;1:
;	tst	sflg
;	bne	1f
;	jsr	r5,mesg
;		<uid not valid\n\0>; .even
;1:
;	br	done
;
;.data
;passwf: </etc/passwd\0>
;tempf:	</tmp/ptmp\0>
;.even
;.bss
;ibuf:	.=.+520.
;obuf:	.=.+520.
;cryptp: .=.+2
;uidp:	.=.+2
;sflg:	.=.+2
;dflg:	.=.+2

; 30/04/2022

;-----------------------------------------------------------------
; Original UNIX v5 - 'crypt' source code (crypt.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;/usr/source/s3/crypt.s -- password incoding
;
;/ crypt -- password incoding
;
;/	mov	$key,r0
;/	jsr	pc,crypt
;
;.globl	crypt, word
;
;crypt:
;	mov	r1,-(sp)
;	mov	r2,-(sp)
;	mov	r3,-(sp)
;	mov	r4,-(sp)
;	mov	r5,-(sp)
;
;	mov	r0,r1
;	mov	$key,r0
;	movb	$004,(r0)+
;	movb	$034,(r0)+
;1:
;	cmp	r0,$key+64.
;	bhis	1f
;	movb	(r1)+,(r0)+
;	bne	1b
;1:
;	dec	r0
;/
;/
;/	fill out key space with clever junk
;/
;	mov	$key,r1
;1:
;	movb	-1(r0),r2
;	movb	(r1)+,r3
;	xor	r3,r2
;	movb	r2,(r0)+
;	cmp	r0,$key+128.
;	blo	1b
;/
;/
;/	establish wheel codes and cage codes
;/
;	mov	$wheelcode,r4
;	mov	$cagecode,r5
;	mov	$256.,-(sp)
;2:
;	clr	r2
;	clr	(r4)
;	mov	$wheeldiv,r3
;3:
;	clr	r0
;	mov	(sp),r1
;	div	(r3)+,r0
;	add	r1,r2
;	bic	$40,r2
;	bis	shift(r2),(r4)
;	cmp	r3,$wheeldiv+6.
;	bhis	4f
;	bis	shift+4(r2),(r5)
;4:
;	cmp	r3,$wheeldiv+10.
;	blo	3b
;	sub	$2,(sp)
;	tst	(r4)+
;	tst	(r5)+
;	cmp	r4,$wheelcode+256.
;	blo	2b
;	tst	(sp)+
;/
;	.data
;shift:	1;2;4;10;20;40;100;200;400;1000;2000;4000;10000;20000;40000;100000
;	1;2
;wheeldiv: 32.; 18.; 10.; 6.; 4.
;	.bss
;cagecode: .=.+256.
;wheelcode: .=.+256.
;	.text
;/
;/
;/	make the internal settings of the machine
;/	both the lugs on the 128 cage bars and the lugs
;/	on the 16 wheels are set from the expanded key
;/
;	mov	$key,r0
;	mov	$cage,r2
;	mov	$wheel,r3
;1:
;	movb	(r0)+,r1
;	bic	$!177,r1
;	asl	r1
;	mov	cagecode(r1),(r2)+
;	mov	wheelcode(r1),(r3)+
;	cmp	r0,$key+128.
;	blo	1b
;/
;/
;/	now spin the cage against the wheel to produce output.
;/
;	mov	$word,r4
;	mov	$wheel+128.,r3
;3:
;	mov	-(r3),r2
;	mov	$cage,r0
;	clr	r5
;1:
;	bit	r2,(r0)+
;	beq	2f
;	incb	r5
;2:
;	cmp	r0,$cage+256.
;	blo	1b
;/
;/	we have a piece of output from current wheel
;/	it needs to be folded to remove lingering hopes of
;/	inverting the function
;/
;	mov	r4,-(sp)
;	clr	r4
;	div	$26.+26.+10.,r4
;	add	$'0,r5
;	cmp	r5,$'9
;	blos	1f
;	add	$'A-'9-1,r5
;	cmp	r5,$'Z
;	blos	1f
;	add	$'a-'Z-1,r5
;1:
;	mov	(sp)+,r4
;	movb	r5,(r4)+
;	cmp	r4,$word+8.
;	blo	3b
;/
;
;	mov	(sp)+,r5
;	mov	(sp)+,r4
;	mov	(sp)+,r3
;	mov	(sp)+,r2
;	mov	(sp)+,r1
;	mov	$word,r0
;	rts	pc
;
;	.bss
;key:	.=.+128.
;word:	.=.+32.
;cage:	.=.+256.
;wheel:	.=.+256.

; 30/04/2022

;-----------------------------------------------------------------
; Original UNIX v5 - 'getc' & 'fopen' source code (get.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;/usr/source/s3/get.s
;--------------------
;/ getw/getc -- get words/characters from input file
;/ fopen -- open a file for use by get(c|w)
;/
;/ calling sequences --
;/
;/   mov $filename,r0
;/   jsr r5,fopen; ioptr
;/
;/  on return ioptr buffer is set up or error bit is set if
;/  file could not be opened.
;/
;/   jsr r5,get(c|w)1; ioptr
;/
;/  on return char/word is in r0; error bit is
;/  set on error or end of file.
;/
;/  ioptr is the address of a 518-byte buffer
;/  whose layout is as follows:
;/
;/  ioptr: .=.+2    / file descriptor
;/         .=.+2    / charact+2    / pointer to next character (reset if no. chars=0)
;/         .=.+512. / the buffer
;
;	.globl	getc,getw,fopen
;
;fopen:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	mov	r0,0f
;	sys	0; 9f
;.data
;9:
;	sys	open; 0:..; 0
;.text
;	bes	1f
;	mov	r0,(r1)+
;	clr	(r1)+
;	mov	(sp)+,r1
;	rts	r5
;1:
;	mov	$-1,(r1)
;	mov	(sp)+,r1
;	sec
;	rts	r5
;
;.data
;getw:
;	mov	(r5),9f
;	mov	(r5)+,8f
;	jsr	r5,getc; 8:..
;	bec	1f
;	rts	r5
;1:
;	mov	r0,-(sp)
;	jsr	r5,getc; 9:..
;	swab	r0
;	bis	(sp)+,r0
;	rts	r5
;.text
;
;getc:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	dec	2(r1)
;	bge	1f
;	mov	r1,r0
;	add	$6,r0
;	mov	r0,0f
;	mov	r0,4(r1)
;	mov	(r1),r0
;	sys	0; 9f
;.data
;9:
;	sys	read; 0:..; 512.
;.text
;	bes	2f
;	tst	r0
;	bne	3f
;2:
;	mov	(sp)+,r1
;	sec
;	rts	r5
;3:
;	dec	r0
;	mov	r0,2(r1)
;1:
;	clr	r0
;	bisb	*4(r1),r0
;	inc	4(r1)
;	mov	(sp)+,r1
;	rts	r5

; 30/04/2022

;-----------------------------------------------------------------
; Original UNIX v5 - 'putc' & 'flush' & 'fcreat' source code
;		     (put.s) in PDP-11 (unix) assembly language
;-----------------------------------------------------------------
;/usr/source/s3/put.s
;--------------------
;/ putw/putc -- write words/characters on output file
;/
;/ fcreat -- create an output file for use by put(w|c)
;/
;/ calling sequences --
;/
;/   mov $filename,r0
;/  jsr r5,fcreat; ioptr
;/
;/ on return ioptr is set up for use by put or error
;/ bit is set if file could not be created.
;/
;/   mov(b) thing,r0
;/   jsr r5,put(w|c)1; ioptr
;/
;/ the character or word is written out.
;/
;/   jsr r5,flush; ioptr
;/
;/ the buffer is fled.
;/
;
;	.globl	putc, putw, flush, fcreat
;
;fcreat:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	mov	r0,0f
;	sys	0; 9f
;.data
;9:
;	sys	creat; 0:..; 666
;.text
;	bes	1f
;	mov	r0,(r1)+
;2:
;	clr	(r1)+
;	clr	(r1)+
;	mov	(sp)+,r1
;	rts	r5
;1:
;	mov	$-1,(r1)+
;	mov	(sp)+,r1
;	sec
;	rts	r5
;
;.data
;putw:
;	mov	(r5),8f
;	mov	(r5)+,9f
;	mov	r0,-(sp)
;	jsr	r5,putc; 8:..
;	mov	(sp)+,r0
;	swab	r0
;	jsr	r5,putc; 9:..
;	rts	r5
;.text
;
;putc:
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;1:
;	dec	2(r1)
;	bge	1f
;	mov	r0,-(sp)
;	jsr	pc,fl
;	mov	(sp)+,r0
;	br	1b
;1:
;	movb	r0,*4(r1)
;	inc	4(r1)
;	mov	(sp)+,r1
;	rts	r5
;
;flush:
;	mov	r0,-(sp)
;	mov	r1,-(sp)
;	mov	(r5)+,r1
;	jsr	pc,fl
;	mov	(sp)+,r1
;	mov	(sp)+,r0
;	rts	r5
;
;fl:
;	mov	r1,r0
;	add	$6,r0
;	mov	r0,-(sp)
;	mov	r0,0f
;	mov	4(r1),0f+2
;	beq	1f
;	sub	(sp),0f+2
;	mov	(r1),r0
;	sys	0; 9f
;.data
;9:
;	sys	write; 0:..; ..
;.text
;1:
;	mov	(sp)+,4(r1)
;	mov	$512.,2(r1)
;	rts	pc