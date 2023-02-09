; ****************************************************************************
;
; CAT.ASM  (Retro Unix 8086 v1 - /bin/cat - concatenate files)
; ----------------------------------------------------------------------------
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; Retro UNIX 8086 v1 - /bin/cat file
;
; [ Last Modification: 18/07/2022 ]
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
;
; ****************************************************************************
;
; Derived from 'cat.s' file of original UNIX v1
;
; CAT3.ASM, 19/06/2022
; CAT2.ASM, 16/07/2015 
; CAT1.ASM, 02/12/2013
;
; ****************************************************************************

; CAT3.ASM (19/06/2022, Retro UNIX 8086 v1, MASM 6.14) -adapted from cat4.s-
; cat386.s - cat4.s (17/06/2022, Retro UNIX 386 v1 & v1.1, NASM 2.15)
; cat386.s - cat3.s (15/06/2022, Retro UNIX 386 v1.2, NASM 2.15)
; cat386.s - cat2.s (14/06/2022, Retro UNIX 386 v1 & v1.1, NASM 2.15)
; cat386.s - cat1.s (05/03/2022, Retro UNIX 386 v1 & v1.1 & v1.2, NASM 2.15)
; cat386.s - cat0.s (17/10/2015 - 28/12/2015, Retro UNIX 386 v1, NASM 2.11)
; CAT2.ASM (02/12/2013 - 16/07/2015, Retro UNIX 8086 v1, MASM 6.11) 

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

    ifnb <arg3> 	
	mov dx, arg3
    endif
    
    ifnb <arg2> 	
	mov cx, arg2
    endif

    ifnb <arg1> 	
	mov bx, arg1
    endif
     			
    mov	ax, syscallnumber	
    int	20h	
   
    endm

ENTERKEY  equ 0Dh
NEXTLINE  equ 0Ah
BACKSPACE equ 08h 

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

;;ibuf	equ offset bss
;;obuf	equ offset bss + 512
;;fin	equ offset bss + 1024
; 16/07/2015
;iobuf	equ offset bss
;fin	equ offset bss + 512	

; 19/06/2022 - bss (uninitialized data) section

stdout	equ bss	 ; 1 byte
chrin	equ bss+1 ; 1 byte
chr	equ bss+2 ; 1 byte
consol	equ bss+3 ; 1 byte
consol_r equ bss+4 ; 2 bytes
consol_w equ bss+6 ; 2 bytes
stdinrw	 equ bss+8 ; 2 bytes
filerwc	 equ bss+10 ; 2 bytes	
linebuf	 equ bss+12 ; (80 bytes)
iobuf	 equ bss+12 ; 512 bytes
;
bssend	 equ offset bss+524	

UNIX   	SEGMENT PUBLIC 'CODE'
        assume cs:UNIX,ds:UNIX,es:UNIX,ss:UNIX

START_CODE:
	;; / cat -- concatenate files

	;sys	_write, 1, nl, 2

	; 19/06/2022
	sys	_fstat, 1, iobuf 
	call	rwcount
	mov	byte ptr [stdout], dl
	and 	dl, dl
	jz	short cat_@ ; block device or regular file
	cmp	byte ptr [iobuf], 9  ; /dev/lpr
	je	short cat_@
	; /dev/tty0 .. /dev/tty9
	; next line
	sys	_write, 1, nl, 2 
	mov	cx, iobuf
cat_@:
	pop	bp
	pop	dx
	; 19/06/2022
	; si = 0 ; ('sysexec' sets regs to zero)
	;mov	si, fin
        ;mov	di, obuf
	;AX = 2 (written byte count)
	; 17/07/2022
	;xor	al, al
        dec     bp
	jz	short cat_10

		;;mov	(sp)+,r5
		;;tst	(sp)+
		;;mov	$obuf,r2
		;;cmp	r5,$1
		;;beq	3f
		;;
cat_0:	
	pop	bx
	cmp	byte ptr [BX], '-'
	;jne	short cat_1
	; 19/06/2022
	je	short cat_3

		;;dec	r5
		;;ble	done
		;;mov	(sp)+,r0
		;;cmpb	(r0),$'-
		;;bne	2f
		;;clr	fin
		;;br	3f
cat_1:
	;;2:
	; bx = file name offset
	xor 	cx, cx ; 0
	sys 	_open
	jnc	short cat_2
	; 19/06/2022
	jmp	cat_7

		;;mov	r0,0f
		;;sys	open; 0:..; 0
		;;bes	loop
		;;mov	r0,fin

cat_2:
	; 19/06/2022
	mov	si, ax

	; convert user's file number to inode number
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	sys	_fstat, si, iobuf 
	;jc	short cat_7
	jnc	short cat_f
	jmp	cat_k

rwcount:
	; 19/06/2022 - Retro UNIX 8086 v1
	xor	dx, dx
	mov	ax, word ptr [iobuf] ; inode number
	cmp	ax, 41 ; regular file (or directory) ?
	jnb	short rwc_2 ; yes

	cmp	al, 8 ; > hd3 inode number ?
	ja	short rwc_1 ; yes ; character device

	cmp	al, 3 ; >= fd0 inode number ?
	jnb	short rwc_2 ; yes ; block device
rwc_1:
	; no, character device
	; read/write count = 1
	inc	dl
	retn
rwc_2:
	; regular file or block device
	; read/write count = 512
	mov	dh, 2 ; dx = 512
	retn

cat_f:
	; 19/06/2022
	call	rwcount
	mov	word ptr [filerwc], dx

	; (check if input file is a tty)
	mov	byte ptr [chrin], 0
	and	dl, dl
	jz	short cat_3  ; not a character device
	;mov	ax, word ptr [iobuf]
	mov	al, byte ptr [iobuf] ; inode number
	cmp	al, 19 ; tty9
	ja	short cat_3
	cmp	al, 10 ; tty0
	jnb	short cat_g

	; [chrin] = 0
	cmp	al, 1  ; /dev/tty
	jne	short cat_3
cat_10:
	sys	_gtty, 0, 1  ; get status of console tty (w)
	inc	al  ; console tty number + 1
	jmp	short cat_h
cat_g:
	sub 	al, 9
cat_h:
	mov	byte ptr [chrin], al
	; [chrin] = tty number + 1

cat_3:
	; 19/06/2022
	;mov	cx, iobuf	
	sys	_fstat, 0  ; get stdin file inode details

	call	rwcount
	mov	word ptr [stdinrw], dx

cat_n:	; 19/06/2022
	; get keyboard status of console tty
	xor	bx, bx ; 0
	xor	cx, cx ; 0
	sys	_gtty
	mov	byte ptr [consol], al
	inc	al ; tty number + 1
	cmp	al, byte ptr [chrin]
	jne	short cat_b
	; input (source) file and stdin (console tty) is same
	;sub	si, si ; 0
;cat_l:
	test	byte ptr [stdout], 1
	jnz	short cat_b ; stdout is not a file		

	;call	writeline
	;cmp	al, 1Bh ; ESCape
	;jne	short cat_l
	;jmp	cat_exit
	
	jmp	writeline 
		; write tty/user input line(s) to file
cat_b:	
	; 19/06/2022
	and	bx, bx ; is there a waiting char ?
	jnz	short cat_x ; yes

	or	dh, dh  ; is stdin a character device (tty) ?
	jnz	short cat_p
			; no, stdin is a file or block device

	; is there a file to read ?
	or	si, si
	jnz	short cat_r ; yes

	; dx = [stdinrw]
	mov	cx, iobuf
	; bx = 0
	sys	_read
	;sys	_read, 0, iobuf
		; read one char into [iobuf]
	; 17/07/2022
	jc	short cat_s
	jmp	cat_6

cat_s:
	cmp	byte ptr [iobuf], 1Bh ; 27 ; ESCape key ?
	jne	short cat_c 

	jmp	cat_exit ; yes, exit

cat_x:
	; 19/06/2022
	cmp	bl, 1Bh ; 27 ; ESCape key ?
	jne	short cat_p
	jmp	cat_q
cat_p:
	; dx = [stdinrw]
	sys	_read, 0, iobuf
		; read one char into [iobuf]
	jc	short cat_6 ; 17/07/2022
	
	; 19/06/2022
	; ax = read count
	; read (redirected) stdin at first then other files 
	or	ax, ax
	jz	short cat_u
	and	dh, dh
	jz	short cat_c ; console tty (crlf check)
	jmp	short cat_w
cat_u:
	; 19/06/2022
	; trick to skip reading stdin file
	;xor	dx, dx
	; dx = 0
	;mov	word ptr [stdinrw], dx  ; end of stdin file
	; 18/06/2022
	mov	word ptr [stdinrw], ax ; 0 
				; end of stdin file
	;jmp	short cat_c ; read (input) file

	; is there a file to read ?
	and	si, si
	jz	short cat_7 ; 17/07/2022 
	
cat_r:
	; 19/06/2022
	mov	ch, byte ptr [chrin]
	and	ch, ch
	jz	short cat_m ; not a tty file
	xor	cl, cl ; 0
	sub	bx, bx ; 0	
	sys	_gtty	; get keyboard status of tty
	or	bx, bx
	; 17/07/2022
	jz	short cat_n ; check for console keystroke

cat_e:
	sys	_read, si, iobuf, 1
	cmp	byte ptr [iobuf], 1Bh ; 27 ; ESCape key ?
	je	short cat_k ; close file

cat_c:
	; is there a file to read ?
	;or	si, si
	;jnz	short cat_r ; yes

	;and	dx, dx
	;jz	short cat_exit ; end of stdin file

	;xor	ax, ax
	;inc	al
	mov	al, 1
	; ax = 1	
	cmp	byte ptr [iobuf], 0Dh ; carriage return ?
	jne	short cat_w
	mov	byte ptr [iobuf+1], 0Ah ; line feed
	inc	al
	; ax = 2
	jmp	short cat_w

cat_m:  ;;3:
	;mov	ax, si
	;sys    _read, ax, iobuf, 512 ; 16/07/2015
	;jc	short cat_5
	
	; 19/06/2022
	; si = file descriptor/number
	;sys	_read, si, iobuf, word ptr [filerwc]
	; 18/07/2022
	mov	dx, word ptr [filerwc]
	sys	_read, si, iobuf
		; read 512 chars into [iobuf]
	jc	short cat_k
	
	; NOTE: If input file is a tty (keyboard)
	;	only 1 byte will be read, by ignoring
	;	byte count (512).
	;	Retro UNIX 8086 v1 kernel ('rtty')
	;	has been modified fot that.
	;       Erdogan Tan (16/07/2015)
	;
	and	ax, ax ; AX = 1 for tty (keyboard)
	;jz	short cat_5
	jz	short cat_k ; 19/06/2022

;	push	si
	;mov	si, ibuf
;	mov	si, cx ; offset ibuf
;	mov	cx, ax
		;;mov	fin,r0
		;;sys	read; ibuf; 512.
		;;bes	3f
		;;mov	r0,r4
		;;beq	3f
		;;mov	$ibuf,r3
	 ; 16/07/2015
;	mov	dx, ax
;	;add	dx, obuf
;cat_4:
;	;;4:
;	lodsb
	;call	putc

cat_w:
	; 16/07/2015
	sys 	_write, 1, iobuf, ax
	;jc	short cat_5
	jnc	short cat_8 ; 19/06/2022
	;
;	loop	cat_4
;	pop	si

cat_5:
	; 19/06/2022
	;mov	ax, word ptr [SI]
	;jmp	short cat_3
	
		;;movb	(r3)+,r0
		;;jsr	pc,putc
		;;dec	r4
		;;bne	4b
		;;br	3b
;cat_5:	;;3:
cat_6:
	;mov	bx, word ptr [SI]
	;or	bx, bx
	;jz	short cat_1
	; 19/06/2022
	or	si, si
	jz	short cat_7
cat_k:
	;sys	_close
	; 19/06/2022
	sys 	_close, si
	;jmp	short cat_1

		;;mov	fin,r0
		;;beq	loop
		;;sys	close
		;;br	loop
	
	; 19/06/2022
cat_7:	
	;;loop:
	dec	bp
	jng	short cat_exit
	jmp	cat_0

	; 19/06/2022 (ESCape)
cat_q:
	; open console tty for read
	sys	_open, consoletty, 0
	jc	short cat_exit
	sys	_read, ax, iobuf
	sys	_close

	;jmp	short cat_exit	

	; 19/06/2022
cat_exit:
	sys	_exit
here:
	nop
	jmp	short here

cat_8:
	; 19/06/2022
	cmp	ax, dx
	jb	short cat_6
	mov	dx, word ptr [stdinrw]
	or 	dx, dx  ; end of stdin file
	jz	short cat_9
	jmp	cat_n

cat_9:
	; 19/06/2022
	and	si, si
	jz	short cat_k
	jmp	cat_r

;cat_6:
;	;;done:
;	sub	di, obuf
;	jz	short cat_7
;	sys	_write, 1, obuf, di
 
		;;sub	$obuf,r2
		;;beq	1f
		;;mov	r2,0f
		;;mov	$1,r0
		;;sys	write; obuf; 0:..
;cat_7:	
;	;;1:
;	sys	_exit
		;;sys	exit
	;;
;putc:	
;	;;putc:
;	stosb
;	cmp	di, dx ; 16/07/2015
	;cmp	di, obuf + 512
;	jb	short @f
;	push	cx
	 ; 16/07/2015
;	mov	di, obuf
;	sub	dx, di ; byte (char) count
	; 
;	sys 	_write, 1, obuf
	;sys 	_write, 1, obuf, 512
	;mov	di, obuf
		;;movb	r0,(r2)+
		;;cmp	r2,$obuf+512.
		;;blo	1f
		;;mov	$1,r0
		;;sys	write; obuf; 512.
		;;mov	$obuf,r2
;	pop	cx
;@@:	
	;;1:
;	retn
		;;rts	pc

writeline:
	; 19/06/2022

	; write line(s) to file (with echo)
	; stdin = console tty
	; stdout = file or another tty

	;mov	di, linebuf
 
	;sys	_gtty, 0, 1  ; get status of console tty (w)	
	;jc	short done3
	
	mov	al, byte ptr [consol]
	; al = console tty number
	add	al, '0'
	mov	byte ptr [consoletty+8], al

	mov	byte ptr [chr], 1Bh ; ESCape

	sys	_open, consoletty, 0 ; open for read
	;jc	short done3
	jnc	short G0
	jmp	short done3 
G0:
	mov	word ptr [consol_r], ax
	sys	_open, consoletty, 1 ; open for write
	jc	short done2
	mov	word ptr [consol_w], ax
	sys	_write, ax, crlf, 2
do:
        mov	di, linebuf
getc:
	mov	dx, 1
	mov	cx, offset chr
	mov	bx, word ptr [consol_r]
	sys	_read
	jc	short done1
	;and	ax, ax
	;jz	short done1
	;
	mov	al, byte ptr [chr]

	cmp	al, 1Bh ; ESCape key
	je	short done1

	cmp	al, 20h
	jb	short G1

	cmp	al, 127
	je	short G2

        cmp	di, linebuf+80
	jnb	short getc
putc:
	stosb
_echo:
	;sys	_write, word ptr [consol_w], chr, 1
	; cx = chr
	; dx = 1
	mov	bx, word ptr [consol_w] 
	sys	_write
	jmp	short getc

done1:
	mov	bx, word ptr [consol_w]
	sys	_close
done2:
	mov 	bx, word ptr [consol_r]
	sys	_close
done3:
	;mov	byte ptr [di], 0
	;mov	al, byte ptr [chr]
	;retn

	mov	dx, di
	sub	dx, linebuf	
	jz	short done4

	sys	_write, 1, linebuf
done4:
	;sys	_exit
	jmp	cat_exit

G1:
	cmp	al, ENTERKEY  ; \r (carriage return)
        je	short G3

	;cmp	al, NEXTLINE  ; \n (next line)
        ;je	short G3

	cmp	al, BACKSPACE ; \b (back space)
	je	short G2
getch:
	jmp	getc
G2:
	; Backspace
	cmp	di, linebuf
	jna	short getch
	dec	di
	jmp	short _echo

G3:
	;mov	al, ENTERKEY ; carriage return
	stosb
	mov	al, NEXTLINE ; line feed
	stosb
	mov	bx, word ptr [consol_w]
	mov	cx, offset crlf
	mov	dx, 2
	sys	_write
	mov	cx, offset linebuf 
	mov	dx, di
	sub	dx, cx ; sub dx, offset linebuf
	; cx = offset crlf
	sys	_write, 1
	jmp	do ; next line

; ;;;;

crlf:	; 19/06/2022
nl:	db 0Dh, 0Ah, 0

; 19/06/2022
consoletty:
	db '/dev/tty', 0, 0

EVEN

bss:
;ibuf:	db 512 dup(0)
;obuf:	db 512 dup(0)
;fin:	dw 0

		;;.bss
		;;ibuf:	.=.+512.
		;;obuf:	.=.+512.
		;;fin:	.=.+2
		;;.text


UNIX     	ends

                end     START_CODE
