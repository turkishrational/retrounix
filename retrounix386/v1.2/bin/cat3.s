; ****************************************************************************
; cat386.s (Retro Unix 386 v1.2) - /bin/cat - concatenate files
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Retro UNIX 8086 v1 - '/bin/cat' file
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 17/07/2022 ] -cat3.s-
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************
; ((nasm cat3.s -l cat3.txt -o cat3 -Z error.txt))
; cat386.s - cat4.s (17/06/2022, Retro UNIX 386 v1 & v1.1)
; cat386.s - cat3.s (15/06/2022, Retro UNIX 386 v1.2)
; cat386.s - cat2.s (14/06/2022, Retro UNIX 386 v1 & v1.1)
; cat386.s - cat1.s (05/03/2022, Retro UNIX 386 v1 & v1.1 & v1.2)
; cat386.s - cat0.s (17/10/2015 - 28/12/2015, Retro UNIX 386 v1, NASM 2.11)
; CAT2.ASM (02/12/2013 - 16/07/2015, Retro UNIX 8086 v1, MASM 6.11) 

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
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_sysver	equ 39 ; (get) Retro Unix 386 version

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

; 15/06/2022 - Retro UNIX 386 v1.2
struc stat
   ; Retro UNIX v1.2 'sysstat' output !
   ; (66 bytes)
   .inode:  resw 1	
   .mode:   resw 1
   .nlinks: resw 1 
   .uid:    resw 1
   .gid:    resb 1
   .size_h: resb 1
   .size:   resd 1
   .dskptr: resd 10
   .atime:  resd 1
   .mtime:  resd 1
   .ctime:  resd 1
   .strucsize:
endstruc

ENTERKEY  equ 0Dh
NEXTLINE  equ 0Ah
BACKSPACE equ 08h

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	;; / cat -- concatenate files

	;sys	_write, 1, nl, 2
	
	; 18/06/2022
	sys	_fstat, 1, iobuf 
	call	rwcount
	mov	[stdout], dl ; 19/06/2022
	and 	dl, dl
	jz	short cat_@ ; block device or regular file
	cmp	byte [iobuf], 16  ; /dev/lpr
	je	short cat_@
	; /dev/tty0 .. /dev/tty9
	; next line
	sys	_write, 1, nl, 2 
	mov	ecx, iobuf
cat_@:
	pop	ebp
	pop	edx
	; esi = 0 ; ('sysexec' sets regs to zero)
	; 05/03/2022
	;mov	esi, fin 
	;mov    edi, obuf
	;EAX = 2 (written byte count)
	; 17/07/2022
	;xor	al, al ; 0
        ; 18/06/2022  
	;mov	ecx, iobuf ; 17/06/2022
	dec     ebp
	;jz	short cat_3	
		;;mov	(sp)+,r5
		;;tst	(sp)+
		;;mov	$obuf,r2
		;;cmp	r5,$1
		;;beq	3f
	
	; 18/06/2022
	;jnz	short cat_0
	;jmp	cat_3
	; 19/06/2022
	jz	short cat_10
cat_0:	
	pop	ebx
	cmp	byte [ebx], '-'
	je	short cat_3 ; 16/06/2022
		;;dec	r5
		;;ble	done
		;;mov	(sp)+,r0
		;;cmpb	(r0),$'-
		;;bne	2f
		;;clr	fin
		;;br	3f
cat_1:
	;;2:
	; ebx = file name offset
	xor 	ecx, ecx ; 0
	sys 	_open
	;jc	short cat_7
	; 05/03/2022 ; (+)
	jnc	short cat_2
	jmp	cat_7
cat_2:
	; 05/03/2022
	mov	esi, eax
	;mov	[esi], eax ; 05/03/2022
	;mov	[esi], ax
		;;mov	r0,0f
		;;sys	open; 0:..; 0
		;;bes	loop
		;;mov	r0,fin

	; 05/03/2022 ; (+)
	; convert user's file number to inode number
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	sys	_fstat, esi, iobuf 
	;jc	short cat_7
	; 05/03/2022 ; (+)
	jnc	short cat_f
	jmp	cat_k ; 15/06/2022

rwcount:
	; 15/06/2022
	xor	edx, edx
	mov	al, [iobuf+stat.mode+1]
	test	al, 80h
	jnz	short rwc_1
	test	al, 40h ; block device ?
	jnz	short rwc_1 ; yes
	; no, character device
	; read/write count = 1
	inc	dl
	retn
rwc_1:
	; regular file or block device
	; read/write count = 512
	mov	dh, 2 ; edx = 512
	retn
	
cat_f:
	; 15/06/2022
	call	rwcount
	mov	[filerwc], edx

	; 18/06/2022
	; (check if input file is a tty)
	mov	byte [chrin], 0
	and	dl, dl
	jz	short cat_3 ; not a character device
	;mov	ax, [iobuf]
	mov	al, [iobuf] ; inode number
	cmp	al, 26 ; tty9
	ja	short cat_3
	cmp	al, 17 ; tty0
	jnb	short cat_g

	; [chrin] = 0
	cmp	al, 8  ; /dev/tty
	jne	short cat_3
cat_10:
	sys	_gtty, 0, 1 ; get status of console tty (w)
	inc	al  ; console tty number + 1
	;jmp	short cat_h
cat_g:
	;sub 	al, 16
	and	al, 0Fh ; 15
;cat_h:
	mov	[chrin], al
	; [chrin] = tty number + 1

cat_3:	
	; 05/03/2022 ; (+)
	; get inode number of current tty (stdin)
	; (get 34 byte inode details, inode num + inode)
	; (get 66 byte inode details for runix 386 v1.2)
	;sys	_fstat, 0, iobuf 
	;;jc	short cat_n
	;mov	ecx, iobuf	
	sys	_fstat, 0  ; get stdin file inode details

	; 15/06/2022
	call	rwcount
	mov	[stdinrw], edx

cat_n:	;;3:
	; get keyboard status of console tty
	xor	ebx, ebx ; 0
	xor	ecx, ecx ; 0
	sys	_gtty
	; 19/06/2022
	mov	[consol], al
	; 18/06/2022
	inc	al ; tty number + 1
	cmp	al, [chrin]
	jne	short cat_b
	; input (source) file and stdin (console tty) is same
	;sub	esi, esi ; 0
;cat_l:
	; 19/06/2022
	test	byte [stdout], 1
	jnz	short cat_b ; stdout is not a file		

	;call	writeline
	;cmp	al, 1Bh ; ESCape
	;jne	short cat_l
	;jmp	cat_exit
	; 19/06/2022
	jmp	writeline 
		; write tty/user input line(s) to file
cat_b:	
	; 15/06/2022
	and	ebx, ebx ; is there a waiting char ?
	jnz	short cat_x ; yes

	or	dh, dh  ; is stdin a character device (tty) ?
	jnz	short cat_p
			; no, stdin is a file or block device

	; is there a file to read ?
	or	esi, esi ; 05/03/2022
	jnz	short cat_r ; yes

	; edx = [stdinrw]
	mov	ecx, iobuf
	; ebx = 0
	sys	_read	; 18/06/2022
	;sys	_read, 0, iobuf
		; read one char into [iobuf]
	;jc	short cat_6 ; (ebx = 0)
	; 18/06/2022
	jc	short cat_d

	cmp	byte [iobuf], 1Bh ; 27 ; ESCape key ?
	jne	short cat_c 
	jmp	cat_exit ; yes, exit

cat_x:
	cmp	bl, 1Bh ; 27 ; ESCape key ?
	;je	short cat_exit ; yes, exit
	; 18/06/2022
	jne	short cat_p
	jmp	cat_q
cat_p:
	; edx = [stdinrw]
	sys	_read, 0, iobuf
		; read one char into [iobuf]
	;jc	short cat_6 ; (ebx = 0)
	; 18/06/2022
	jnc	short cat_s
cat_d:
	jmp	cat_6
cat_s:	
	; eax = read count
	; read (redirected) stdin at first then other files 
	or	eax, eax
	;jnz	short cat_w ; write bytes in buffer
	; 18/06/2022
	jz	short cat_u
	and	dh, dh
	jz	short cat_c ; console tty (crlf check)
	jmp	short cat_w
cat_u:
	; 16/06/2022
	; trick to skip reading stdin file
	;xor	edx, edx
	; edx = 0
	;mov	[stdinrw], edx  ; end of stdin file
	; 18/06/2022
	mov	[stdinrw], eax  ; 0 ; end of stdin file
	;jmp	short cat_c ; read (input) file

	; is there a file to read ?
	and	esi, esi
	;jz	short cat_7 ; no
	; 18/06/2022
	jnz	short cat_r
	jmp	cat_7 

cat_r:
	; 18/06/2022
	mov	ch, [chrin]
	and	ch, ch
	jz	short cat_m ; not a tty file
	xor	cl, cl ; 0
	sub	ebx, ebx ; 0	
	sys	_gtty	; get keyboard status of tty
	or	ebx, ebx
	;jz	short cat_n ; check for console keystroke
	; 18/06/2022
	jnz	short cat_e
	jmp	cat_n

cat_e:
	sys	_read, esi, iobuf, 1
	cmp	byte [iobuf], 1Bh ; 27 ; ESCape key ?
	je	short cat_k ; close file

cat_c:
	; is there a file to read ?
	;or	esi, esi ; 05/03/2022
	;jnz	short cat_r ; yes

	; 16/06/2022
	;and	edx, edx
	;jz	short cat_exit ; end of stdin file

	;xor	eax, eax
	;inc	al
	mov	al, 1
	; eax = 1	
	cmp	byte [iobuf], 0Dh ; carriage return ?
	jne	short cat_w
	mov	byte [iobuf+1], 0Ah ; line feed
	inc	al
	; eax = 2
	jmp	short cat_w

cat_m:
	; 05/03/2022
	;mov	eax, [esi] ; file descriptor/number
	;;mov	ax, [esi]
	;
	;sys	_read, eax, iobuf, 512 ; 16/07/2015
	;;sys 	_read, eax, ibuf, 512 
	;jc	short cat_6
	; 15/06/2022
	; 05/03/2022
	; esi = file descriptor/number
	sys	_read, esi, iobuf, [filerwc]
		; read 512 chars into [iobuf]
	;jc	short cat_6
	jc	short cat_k ; 05/03/2022

	; NOTE: If input file is a tty (keyboard)
	;	only 1 byte will be read, by ignoring
	;	byte count (512).
	;	Retro UNIX 8086 v1 kernel ('rtty')
	;	has been modified fot that.
	;       Erdogan Tan (16/07/2015)
	;

	; 15/06/2022
	and	eax, eax ; EAX = 1 for tty (keyboard)
	;jz	short cat_6
	jz	short cat_k ; 05/03/2022

;	push	esi
	;mov	esi, ibuf
;	mov	esi, ecx ; offset ibuf
;	mov	ecx, eax
		;;mov	fin,r0
		;;sys	read; ibuf; 512.
		;;bes	3f
		;;mov	r0,r4
		;;beq	3f
		;;mov	$ibuf,r3
	 ; 16/07/2015
;	mov	edx, eax
;	;add	edx, obuf
;cat_4:
;	;;4:
;	lodsb
	;call	putc
cat_w:
	; 16/07/2015
	; write to console tty (stdout)
	sys 	_write, 1, iobuf, eax
	;jc	short cat_6
	; 05/03/2022
	;jnc	short cat_3
	; 15/06/2022
	jnc	short cat_8

;	loop	cat_4
;	pop	esi
cat_5:
	;mov	eax, [esi] ; 05/03/2022
	;;mov	ax, [esi]
	;jmp	short cat_3
		;;movb	(r3)+,r0
		;;jsr	pc,putc
		;;dec	r4
		;;bne	4b
		;;br	3b
	;; 05/03/2022
	;; ebx = file descriptor/input
	;;and	ebx, ebx ; console input ?
	;;jnz	short cat_3 ; no, file input
	;;cmp	byte [quit], al ; 0
	;;ja	short cat_7 ; ebx = 0
	;;jmp	short cat_z
	; 05/03/2022
	;jmp	short cat_3
		
cat_6:	;;3:
	; 05/03/2022
	; ebx = file descriptor/number
	;movzx	ebx, word [esi]
	;
	or	esi, esi
	jz	short cat_7
cat_k:
	sys	_close, esi
	;
	;or	ebx, ebx
	;jz	short cat_7
	;sys 	_close
		;;mov	fin,r0
		;;beq	loop
		;;sys	close
		;;br	loop
cat_7:	
	;;loop:
	dec	ebp
	;;jz	short cat_8
	; 28/12/2015
	;jg	short cat_0
	; 05/03/2022
	jng	short cat_exit
	jmp	cat_0

	; 18/06/2022 (ESCape)
cat_q:
	; open console tty for read
	sys	_open, consoletty, 0
	jc	short cat_exit
	sys	_read, eax, iobuf
	sys	_close

	;jmp	short cat_exit	

cat_exit:
	sys	_exit
here:
	nop
	jmp	short here

cat_8:
	; 15/06/2022
	cmp	eax, edx
	jb	short cat_6
	mov	edx, [stdinrw]
	or 	edx, edx  ; end of stdin file
	jz	short cat_9
	jmp	cat_n

cat_9:
	; 16/06/2022
	and	esi, esi
	jz	short cat_k
	jmp	cat_r ; 18/06/2022

;cat_8:
;	;;done:
;	sub	di, obuf
;	jz	short cat_9
;	sys	_write, 1, obuf, di 
		;;sub	$obuf,r2
		;;beq	1f
		;;mov	r2,0f
		;;mov	$1,r0
		;;sys	write; obuf; 0:..
;cat_9:	
;	;;1:
;	sys	_exit
		;;sys	exit
	;;
;putc:	
;	;;putc:
;	stosb
;	cmp	di, dx ; 16/07/2015
	;cmp	di, obuf + 512
;	jb	short cat_10
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
;cat_10:	
	;;1:
;	retn
		;;rts	pc

writeline:
	; 19/06/2022

	; write line(s) to file (with echo)
	; stdin = console tty
	; stdout = file or another tty

	;mov	edi, linebuf
 
	;sys	_gtty, 0, 1  ; get status of console tty (w)	
	;jc	short done3
	
	mov	al, [consol]
	; al = console tty number
	add	al, '0'
	mov	[consoletty+8], al

	mov	byte [chr], 1Bh ; ESCape

	sys	_open, consoletty, 0 ; open for read
	;jc	short done3
	jnc	short G0
	jmp	done3 
G0:
	mov	[consol_r], eax
	sys	_open, consoletty, 1 ; open for write
	jc	short done2
	mov	[consol_w], eax
	sys	_write, [consol_w], crlf, 2
do:
        mov	edi, linebuf
getc:
	sys	_read, [consol_r], chr, 1
	jc	short done1
	;and	eax, eax
	;jz	short done1
	;
	mov	al, [chr]

	cmp	al, 1Bh ; ESCape key
	je	short done1

	cmp	al, 20h
	jb	short G1

	cmp	al, 127
	je	short G2

        cmp	edi, linebuf+80
	jnb	short getc
putc:
	stosb
echo:
	;sys	_write, [consol_w], chr, 1
	; ecx = chr
	; edx = 1
	sys	_write, [consol_w]
	jmp	short getc

done1:
	sys	_close, [consol_w]
done2:
	sys	_close, [consol_r]
done3:
	;mov	byte [edi], 0
	;mov	al, [chr]
	;retn

	mov	edx, edi
	sub	edx, linebuf	
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
	cmp	edi, linebuf
	jna	short getch
	dec	edi
	jmp	short echo

G3:
	;mov	al, ENTERKEY ; carriage return
	stosb
	mov	al, NEXTLINE ; line feed
	stosb
	sys	_write, [consol_w], crlf, 2
	mov	ecx, linebuf 
	mov	edx, edi
	sub	edx, ecx ; sub edx, linebuf
	; ecx = offset crlf
	sys	_write, 1
	jmp	do ; next line

; ;;;;

crlf:	; 19/06/2022
nl:	db 0Dh, 0Ah, 0

;; 05/03/2022
;quit:	db 0
; 18/06/2022
consoletty:
	db '/dev/tty', 0, 0 ; ; 19/06/2022

align 4

bss_start:

ABSOLUTE bss_start

; 19/06/2022
stdout:	resb 1
; 18/06/2022
chrin:	resb 1
; 19/06/2022
chr:	resb 1
consol:	resb 1
consol_r: resd 1
consol_w: resd 1

; 15/06/2022
stdinrw: resd 1
filerwc: resd 1

linebuf: ; 19/06/2022	
iobuf:  resb 512
;ibuf:	resb 512
;obuf:	resb 512
;;fin:	resw 1
;fin:	resd 1 ; 05/03/2022	
		;;.bss
		;;ibuf:	.=.+512.
		;;obuf:	.=.+512.
		;;fin:	.=.+2
		;;.text
;bss_end: