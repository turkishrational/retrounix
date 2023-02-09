; ****************************************************************************
; pr3.s (pr386.s) - print file - by Erdogan Tan - 25/06/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 (& v1.1) - file print utility 
;
; [ Last Modification: 10/07/2022 ]

; Derived from disassembled source code of unix v2 '/bin/pr' 
;
; ****************************************************************************
; [ s2-bits.gz - bin/pr (archive date: 17-01-1972) ]

; Assembler: NASM v2.15
; ((nasm pr3.s -l pr3.txt -o pr3 -Z error.txt))

; pr1.s (26/06/2022, Retro UNIX 386 v1 & v1.1)
; pr2.s (26/06/2022, Retro UNIX 386 v1.2)
; pr3.s (10/07/2022, Retro UNIX 386 v1 & v1.1) ; (LF -> CRLF)
; pr4.s (10/07/2022, Retro UNIX 386 v1.2) ; (LF -> CRLF)

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

; 25/06/2022
struc stat
	; Note: This is for Retro UNIX v1 'sysstat' output !!!
	; (34 bytes)
	.inode:  resw 1	
	.mode:	 resw 1
	.nlinks: resb 1
	.uid:	 resb 1
	.size:	 resw 1
	.dskptr: resw 8
	.ctime:	 resd 1
	.mtime:	 resd 1
	.rsvd:   resw 1
	.strucsize:
endstruc 

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

;-----------------------------------------------------------------
;  code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 25/06/2022
	pop	ecx ; ecx = number of arguments
	;
	pop	eax ; eax = argument 0 = executable file name
	;
	mov	ebp, program_msg
	;
	;dec	ecx
	dec	cl
	jng	short prn_0

	mov	[argc], cl

	sys	_open, lpt1, 1  ; open /dev/lpr for write
	jnc	short prn_1

	mov	eax, printer_err_msg
	call	print_msg
	jmp	hang	
prn_1:
	mov	[prnfnum], eax

prn_2:
	pop	edi

	mov	al, [edi]	

	cmp	al, '-' ; option ?	
	;jne	short prn_4
	je	short case_l
	jmp	prn_4
case_l:
	inc	edi
	mov	al, [edi]

	cmp	al, 'l'
	jne	short case_r 
	mov	byte [rows], 78
	jmp	short prn_3
case_r:
	cmp	al, 'r'
	jne	short case_m
	mov	byte [rows], 66
	jmp	short prn_3
case_m:
	cmp	al, 'm'
	jne	short case_c
	mov	byte [cdatef], 0
	jmp	short prn_3
case_c:
	cmp	al, 'c'
	jne	short case_c
	;mov	byte [cdatef], 1
	inc	byte [cdatef]

prn_3:
	dec	byte [argc]
	jg	short prn_2

	; print usage message on /dev/tty
	cmp	byte [files], 0
	ja	short fclose_exit
prn_0:
	; print usage message on stdout
	;sys	_write, 1, program_msg, size_pmsg
	sys	_msg, program_msg, size_pmsg, 0Fh 

	sys	_write, 1, usage_msg, size_umsg
	jmp	short hang

fclose_exit:
	sys	_write, 1, nextline, 2

	cmp	byte [txtfnum], 0
	jna	short pclose_exit
p_err_exit:
	sys	_close, [txtfnum]
	;jmp	short pclose_exit

pclose_exit:
	sys	_close, [prnfnum]
hang:
	sys	_exit
	jmp	short hang

prn_4:
	inc	byte [files]

	; edi = txt file name address

	sys	_open, edi, 0 ; open for read
	jnc	short prn_5

f_err_exit:
	mov	eax, file_err_msg
	call	print_msg
	jmp	prn_3

prn_5:
	mov	[txtfnum], eax

	test	byte [cdatef], 0FFh
	jz	short prn_6
	sys	_time
	jmp	short prn_7
prn_6:
	sys	_fstat, eax, stbuf
	mov	eax, [stbuf+stat.mtime]
prn_7:
	mov	[mtime], eax

write_pages:
	xor	eax, eax ; 0
	mov	[pgnumber], ax
	mov	[eof], al
	mov	[cchar], al	
write_next_page:	
	call	readf
	jnc	short prn_8

	sys	_close, [txtfnum]

	dec	byte [argc]
	;jng	short pclose_exit
	jg	short p_next_file
	jmp	pclose_exit

p_next_file:
	sub	eax, eax

	mov	[txtfnum], al ; 0 ; open file number
	;mov	[fbuf], eax ; 0	  ; open file number
	mov	[fbuf+4], eax ; 0 ; character count

	jmp	prn_2

prn_8:
	sys	_write, [prnfnum], EmptyLines, 4 ;  2 rows
	jc	short prn_err

	mov	eax, [mtime]
	push	edi
	call	ctime
	pop	edi
	; cbuf = date&time string (26 bytes)
	
	sys	_write, [prnfnum], cbuf, 26
	jc	short prn_err
	
	sys	_write, [prnfnum], PageHeader-2, 2
	jc	short prn_err

	inc	word [pgnumber]

	mov	esi, edi ; file name address (on stack)

	xor	edx, edx
prn_9:	
	lodsb
	or	al, al
	jz	short prn_10
	inc	dl
	jmp	short prn_9

prn_10:
	mov	ecx, edi
	;sys	_write, [prnfnum], edi, edx
	sys	_write
	jc	short prn_err

	mov	ecx, PageHeader
	mov	dl, 6
	;sys	_write, [prnfnum], PageHeader, edx
	sys	_write
	;jc	short prn_err
	; 10/07/2022
	jnc	short prn_14

prn_err:
	mov	eax, printer_err_msg
	call	print_msg	
	jmp	p_err_exit

prn_14:
	mov	ax, [pgnumber] 
	call	write_pg_num
	jc	short prn_err	

	;sys	_write, [prnfnum], EmptyLines, 8 ; 4 rows
	mov	ecx, EmptyLines
	mov	dl, 8
	sys	_write
	jc	short prn_err

	mov	dl, [rows]
	sub	dl, 11
	mov	ebp, edx

write_next_char:
prn_11:
	call	getchar

	; 10/07/2022
	mov	ah, al
	xchg	ah, [pchr] ; previous character
	cmp	al, 0Ah
	jne	short prn_13
	cmp	ah, 0Dh
	je	short prn_13
	mov	al, 0Dh
	call	writechar
	mov	al, 10 ; 0Ah
	;
prn_13:
	call	writechar
	cmp	al, 10 ; lf character, nextline
	jne	short prn_11
	dec	ebp
	jnz	short prn_11
	
	sys	_write, [prnfnum], EmptyLines, 10 ; 5 rows
	jc	short prn_err
prn_12:
	jmp	write_next_page

;-----------------------------------------------------------------

print_msg:
	; eax = asciiz string address
	mov	edx, eax
	dec	edx
nextchr:
	inc	edx
	cmp	byte [edx], 0
	ja	short nextchr
	;cmp	[edx], 0Dh
	;ja	short nextchr
	sub	edx, eax
	; edx = asciiz string length
	;
	sys	_write, 1, eax
	retn

; 25/06/2022
;-----------------------------------------------------------------

getchar:
	mov	al, [cchar]
	or	al, al
	jz	short getchar_1
	mov	byte [cchar], 0
	retn
getchar_1:
	test	byte [eof], 0FFh
	jz	short getchar_2
	mov	al, 10 ; lf, nextline
	retn
getchar_2:
	call	getc
	jnc	short getchar_ok
	inc	byte [eof]
	jmp	short getchar
getchar_ok:
readf_ok:
	retn

; 25/06/2022
;-----------------------------------------------------------------

readf:
	call	getchar
	mov	[cchar], al
	test	byte [eof], 0FFh
	jz	short readf_ok
readf_err:
	stc
	retn

; 25/06/2022
;-----------------------------------------------------------------

write_pg_num:
	; eax = page number (<= 65535)
	sub	ecx, ecx
	mov	cl, 10
	xor	edx, edx
w_pnum_1:
	div	ecx
	push	edx
	and	eax, eax
	jz	short w_pnum_2
	call	write_pg_num
w_pnum_2:	
	pop	eax
	add	al, '0'
writechar:
	mov	[chr], al
	sys	_write, [prnfnum], chr, 1
	mov	al, [chr]
	retn		
	

; 25/06/2022
;-----------------------------------------------------------------
; get characters from input file

getc:
	mov	eax, [fbuf+4] ; character count
	and	eax, eax
	jnz	short gch1
gch0:
	mov	ebx, [fbuf] ; file descriptor/number
	mov	ecx, fbuf + 12 ; read buffer address
	mov 	[fbuf+8], ecx ; character offset
	sub	edx, edx
	mov	dl, 128
	sys	_read ; sys _read, ebx, ecx, edx
	jc	short gch2
	or	eax, eax
	jz	short gch3
gch1:
	dec	eax
	mov	[fbuf+4], eax ; character count in buffer
	mov	ebx, [fbuf+8] ; current character offset
	;xor	eax, eax
	xor	ah, ah
	mov	al, [ebx]
	inc	ebx
	mov	[fbuf+8], ebx ; next character offset
	retn 	
gch2:
	xor	eax, eax
gch3:
	stc
	retn

; 25/06/2022
;=================================================================
%include 'ctime386.s'
;=================================================================

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

lpt1:
	db '/dev/lpr', 0

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 386 v1 PRINT Utility by Erdogan TAN - 26/06/2022'
	db  0Dh, 0Ah, 0
usage_msg:
	db  0Dh, 0Ah
	db  'Usage: pr [ -rlcm ] filename1 ... '
	db  0Dh, 0Ah
	db  '	    Options: '
	db  0Dh, 0Ah
	db  '             -r : 66 lines per page (default option)'
	db  0Dh, 0Ah	
	db  '             -l : 78 lines per page'
	db  0Dh, 0Ah
	db  '             -m : modification date (default option)'
	db  0Dh, 0Ah
	db  '             -c : current date'
	db  0Dh, 0Ah
nextline:
	db  0Dh, 0Ah, 0

size_pmsg equ usage_msg-(program_msg+1)

printer_err_msg:
	db 0Dh, 0Ah	
	db "Printer Error !"
 	db 0Dh, 0Ah, 0  		

size_umsg equ printer_err_msg-(usage_msg+1)

file_err_msg:
	db 0Dh, 0Ah	
	db "File Error !"
 	db 0Dh, 0Ah, 0  

EmptyLines:
	db 0Dh, 0Ah, 0Dh, 0Ah, 0Dh, 0Ah, 0Dh, 0Ah, 0Dh, 0Ah
	db 20h, 20h
PageHeader:
	db " Page "

rows:	db 66
cdatef:	db 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

argc:	 resb 1
files:	 resb 1
pgnumber: resw 1
eof:	 resb 1
cchar:	 resb 1
chr:	 resb 1
pchr:	 resb 1 ; 10/07/2022
mtime:	 resd 1	
prnfnum: resd 1

fbuf:
txtfnum: 
	resb 128+12 ; 26/06/2022
stbuf:
	resb 34

bss_end:

;-----------------------------------------------------------------
; 25/06/2022
;-----------------------------------------------------------------
; Original UNIX v2 - pr (utility) disassembled source code
;-----------------------------------------------------------------

;
; +-------------------------------------------------------------------------+
; |   This file	has been generated by The Interactive Disassembler (IDA)    |
; |	      Copyright	(c) 2013 Hex-Rays, <support@hex-rays.com>	    |
; +-------------------------------------------------------------------------+
;
; File Name   :	C:\Users\Erdoðan\Desktop\s2.tar\bin\pr
; Format      :	Binary file
; Base Address:	0000h Range: 4000h - 448Eh Loaded length: 0000048Eh

; Processor	  : PDP11
; Target assembler: Macro-11 Assembler

;.macro .array of,type,cnt,val
;.rept  cnt
; type  val
;.endr
;.endm .array

; ===========================================================================

;; Segment type:	Regular
;.PSECT RAM
;byte_0:		.blkb  40000
;; ---------------------------------------------------------------------------
;
;loc_40000:
;		br	loc_40014
;; ---------------------------------------------------------------------------
;		.word 2216
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;; ---------------------------------------------------------------------------
;
;loc_40014:				
;		trap	33
;; ---------------------------------------------------------------------------
;		.word intr_loc
;; ---------------------------------------------------------------------------
;		mov	(SP)+, argc
;		tst	(SP)+
;		clr	R0
;		trap	34
;; ---------------------------------------------------------------------------
;		.word fnum
;; ---------------------------------------------------------------------------
;		bit	#1, statbuf
;		beq	chmod_err
;		trap	22
;; ---------------------------------------------------------------------------
;		.word ttyx		; "/dev/tty0"
;		.word statbuf
;; ---------------------------------------------------------------------------
;		mov	fnum, R0
;		sub	statbuf, R0
;		add	#60, R0	; '0'
;		movb	R0, ttyx+10
;		trap	17
;; ---------------------------------------------------------------------------
;		.word ttyx		; "/dev/tty0"
;		.word 14
;; ---------------------------------------------------------------------------
;		bcs	chmod_err
;		inc	chmodflg
;
;chmod_err:				
;		clr	fnum
;
;err_cf:					
;		mov	fnum, R0
;		beq	err_cf_next
;		trap	6
;		clr	fnum
;
;err_cf_next:				
;		dec	argc
;		bgt	next_arg
;		jmp	intr_loc
;; ---------------------------------------------------------------------------
;
;next_arg:				
;		mov	(SP)+, R0
;		mov	R0, fname_ptr
;		cmpb	(R0)+, #55 ; '-'
;		bne	open_txt_file
;		cmpb	@R0, #154 ; 'l'
;		bne	case_r
;		mov	#78., rows
;		br	err_cf
;; ---------------------------------------------------------------------------
;
;case_r:					
;		cmpb	@R0, #162 ; 'r'
;		bne	case_m
;		mov	#66., rows
;		br	err_cf
;; ---------------------------------------------------------------------------
;
;case_m:					
;		cmpb	@R0, #155 ; 'm'
;		bne	case_c
;		clr	cdateflg
;		br	err_cf
;; ---------------------------------------------------------------------------
;
;case_c:					
;		cmpb	@R0, #143 ; 'c'
;		bne	open_txt_file
;		inc	cdateflg
;		br	err_cf
;; ---------------------------------------------------------------------------
;
;open_txt_file:				
;		mov	fname_ptr, R0
;		jsr	R5, fopen
;; ---------------------------------------------------------------------------
;		.word fnum
;; ---------------------------------------------------------------------------
;		bcs	err_cf
;		tst	cdateflg
;		beq	get_fmod_time
;		trap	15
;		mov	@#word_177304, mtime_l
;		mov	@#word_177302, mtime_h
;		br	write_pages
;; ---------------------------------------------------------------------------
;
;get_fmod_time:				
;		mov	fnum, R0
;		trap	34
;; ---------------------------------------------------------------------------
;		.word fstatbuf
;; ---------------------------------------------------------------------------
;		mov	i_mtime_l, mtime_l
;		mov	i_mtime_h, mtime_h
;
;write_pages:				
;		clr	pagenumber
;		clr	eof
;		clr	cchar
;
;write_next_page:			
;		call	readf
;		br	err_cf
;; ---------------------------------------------------------------------------
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;		.word EmptyLines	; "\n\n\n\n\n"
;		.word 2
;; ---------------------------------------------------------------------------
;		mov	#1, R0
;		mov	mtime_l, @#word_177304
;		mov	mtime_h, @#word_177302
;		call	write_hdr_date
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;		.word PageHeader	; "   Page "
;		.word 2
;; ---------------------------------------------------------------------------
;		inc	pagenumber
;		mov	fname_ptr, R1
;		mov	R1, f_name_ptr
;		mov	#-1, R0
;
;strlen:					
;		inc	R0
;		tstb	(R1)+
;		bne	strlen
;		mov	R0, charcount
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;f_name_ptr:	.word 40000		
;charcount:	.word 40000		
;; ---------------------------------------------------------------------------
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;		.word PageHeader+2
;		.word 6
;; ---------------------------------------------------------------------------
;		mov	pagenumber, @#word_177304
;		call	write_pg_num
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;		.word EmptyLines	; "\n\n\n\n\n"
;		.word 4
;; ---------------------------------------------------------------------------
;		mov	rows, R1
;		sub	#11., R1
;
;write_next_char:			
;		call	get_char
;		call	write_char
;		cmp	R0, #10.
;		bne	write_next_char
;		dec	R1
;		bne	write_next_char
;		mov	#1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;		.word EmptyLines	; "\n\n\n\n\n"
;		.word 5
;; ---------------------------------------------------------------------------
;		br	write_next_page
;; ---------------------------------------------------------------------------
;
;intr_loc:				
;					
;		tst	chmodflg
;		beq	_exit
;		trap	17
;; ---------------------------------------------------------------------------
;		.word ttyx		; "/dev/tty0"
;		.word 15
;; ---------------------------------------------------------------------------
;
;_exit:					
;		trap	1
;
;; =============== SUBROUTINE ================================================
;
;
;write_pg_num:				
;					
;		clr	@#word_177302
;		mov	#10., @#word_177300
;		mov	@#word_177302, -(SP)
;		tst	@#word_177304
;		beq	wrt_pg_num_1
;		call	write_pg_num
;
;wrt_pg_num_1:				
;		mov	(SP)+, R0
;		add	#60, R0	; '0'
;
;write_char:				
;		mov	R0, pn_chr
;		mov	#1, R0
;		trap	4
;; End of function write_pg_num
;
;; ---------------------------------------------------------------------------
;		.word pn_chr
;		.word 1
;; ---------------------------------------------------------------------------
;		mov	pn_chr, R0
;		return
;
;; =============== SUBROUTINE ================================================
;
;
;get_char:				
;					
;		mov	cchar, R0
;		beq	get_char_1
;		clr	cchar
;		return
;; ---------------------------------------------------------------------------
;
;get_char_1:				
;		tst	eof
;		beq	get_next_char
;		mov	#10., R0
;		return
;; ---------------------------------------------------------------------------
;
;get_next_char:				
;		jsr	R5, getc
;; End of function get_char
;
;; ---------------------------------------------------------------------------
;		.word fnum
;; ---------------------------------------------------------------------------
;		bcc	getc_ok
;		mov	PC, eof
;		br	get_char
;; ---------------------------------------------------------------------------
;
;getc_ok:				
;		return
;
;; =============== SUBROUTINE ================================================
;
;
;readf:				
;		call	get_char
;		mov	R0, cchar
;		tst	eof
;		bne	readf_err
;		add	#2, @SP
;
;readf_err:				
;		return
;; End of function readf
;
;; ---------------------------------------------------------------------------
;EmptyLines:	.ascii <12>		
;		.ascii <12>
;		.ascii <12>
;		.ascii <12><12>
;PageHeader:	.ascii \   Page \       
;ttyx:		.ascii \/dev/tty0\<0>   
;		.byte	 0 ;
;rows:		.word 102		
;mtime_l:	.word 0			
;mtime_h:	.word 0			
;argc:		.word 0			
;					
;eof:		.word 0			
;					
;cchar:		.word 0			
;pn_chr:	.word 0			
;					
;pagenumber:	.word 0			
;					
;fnum:		.word 0			
;statbuf:	.word 0			
;		.word 0
;		.word 0
;		.word 0
;fstatbuf:	.word 0			
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;		.word 0
;i_mtime_h:	.word 0			
;i_mtime_l:	.word 0			
;i_rsvd:	.word 0
;		.array of .bytes cnt=132 val=0
;chmodflg:	.word 0			
;fname_ptr:	.word 0			
;					
;cdateflg:	.word 0			
;; ---------------------------------------------------------------------------
;
;write_hdr_date:				
;		mov	R1, -(SP)
;		sub	#20, SP
;		mov	R0, R1
;		mov	SP, R0
;		call	calc_date
;		mov	R0, datebufptr
;		mov	R1, R0
;		trap	4
;; ---------------------------------------------------------------------------
;datebufptr:	.word 40000		
;		.word 15.
;; ---------------------------------------------------------------------------
;		add	#16., SP
;		mov	(SP)+, R1
;		return
;; ---------------------------------------------------------------------------
;
;calc_date:				
;		mov	R2, -(SP)
;		mov	R3, -(SP)
;		cmp	@#word_177302, word_41700
;		bcs	loc_41372
;		bhi	loc_41340
;		cmp	@#word_177304, word_41702
;		bcs	loc_41372
;
;loc_41340:				
;		mov	@#word_177302, -(SP)
;		sub	word_41702, @#word_177304
;		sbc	@SP
;		sub	word_41700, @SP
;		mov	(SP)+, @#word_177302
;		mov	#29., february
;
;loc_41372:				
;		mov	#-4, @#word_177314
;		mov	#32274,	@#word_177300
;		mov	@#word_177304, R3
;		mov	@#word_177302, @#word_177304
;		mov	#2, @#word_177314
;		mov	#15., @#word_177300
;		add	#15., R0
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 10.
;; ---------------------------------------------------------------------------
;		jsr	R5, divide
;		.word 6
;; ---------------------------------------------------------------------------
;		movb	#72, -(R0) ; ':'
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 10.
;; ---------------------------------------------------------------------------
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 6
;; ---------------------------------------------------------------------------
;		movb	#72, -(R0) ; ':'
;		mov	R3, @#word_177304
;		mov	#30, @#word_177300
;		mov	@#word_177304, R3
;		mov	@#word_177302, @#word_177304
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 10.
;; ---------------------------------------------------------------------------
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 10.
;; ---------------------------------------------------------------------------
;		mov	#mdays,	R2
;
;loc_41544:
;		cmp	@R2, R3
;		bgt	loc_41554
;		sub	(R2)+, R3
;		br	loc_41544
;; ---------------------------------------------------------------------------
;
;loc_41554:
;		movb	#40, -(R0) ; ' '
;		sub	#mdays,	R2
;		asl	R2
;		add	#byte_41740, R2
;		inc	R3
;		mov	R3, @#word_177304
;		jsr	R5, divide
;; ---------------------------------------------------------------------------
;		.word 10.
;; ---------------------------------------------------------------------------
;		tst	@#word_177304
;		beq	loc_41630
;		add	#60, @#word_177304 ; '0'
;		movb	@#word_177304, -(R0)
;		br	loc_41634
;; ---------------------------------------------------------------------------
;
;loc_41630:				
;		movb	#40, -(R0) ; ' '
;
;loc_41634:				
;		movb	#40, -(R0) ; ' '
;		movb	-(R2), -(R0)
;		movb	-(R2), -(R0)
;		movb	-(R2), -(R0)
;		mov	(SP)+, R3
;		mov	(SP)+, R2
;		return
;
;; =============== SUBROUTINE ================================================
;
;
;divide:					
;		clr	@#word_177302
;		mov	(R5)+, @#word_177300
;		add	#60, @#word_177302 ; '0'
;		movb	@#word_177302, -(R0)
;		rts	R5
;; End of function divide
;
;; ---------------------------------------------------------------------------
;word_41700:	.word 70310		
;word_41702:	.word 11000		
;mdays:		.word 31.
;february:	.word 28.		
;		.word 31.
;		.word 30.
;		.word 31.
;		.word 30.
;		.word 31.
;		.word 31.
;		.word 30.
;		.word 31.
;		.word 30.
;		.word 999.
;		.byte 0
;Jan:		.ascii \Jan\
;byte_41740:	.byte 0
;Feb:		.ascii \Feb\<0>
;Mar:		.ascii \Mar\<0>
;Apr:		.ascii \Apr\<0>
;May:		.ascii \May\<0>
;Jun:		.ascii \Jun\<0>
;Jul:		.ascii \Jul\<0>
;Aug:		.ascii \Aug\<0>
;Sep:		.ascii \Sep\<0>
;Oct:		.ascii \Oct\<0>
;Nov:		.ascii \Nov\<0>
;Dec:		.ascii \Dec\
;; ---------------------------------------------------------------------------
;
;fopen:					
;		mov	R1, -(SP)
;		mov	(R5)+, R1
;		mov	R0, fnameptr
;		trap	5
;; ---------------------------------------------------------------------------
;fnameptr:	.word 40000		
;		.word 0
;; ---------------------------------------------------------------------------
;		bcs	fopen_err
;		mov	R0, (R1)+
;		clr	(R1)+
;		mov	(SP)+, R1
;		rts	R5
;; ---------------------------------------------------------------------------
;
;fopen_err:				
;		mov	#-1, @R1
;		mov	(SP)+, R1
;		sec
;		rts	R5
;; ---------------------------------------------------------------------------
;		mov	@R5, fbuf
;		mov	(R5)+, word_42072
;		jsr	R5, getc
;; ---------------------------------------------------------------------------
;word_42072:	.word 40000		
;; ---------------------------------------------------------------------------
;		bcc	getw
;		rts	R5
;; ---------------------------------------------------------------------------
;
;getw:					
;		mov	R0, -(SP)
;		jsr	R5, getc
;; ---------------------------------------------------------------------------
;fbuf:		.word 40000		
;; ---------------------------------------------------------------------------
;		swab	R0
;		bis	(SP)+, R0
;		rts	R5
;; ---------------------------------------------------------------------------
;
;getc:					
;					
;		mov	R1, -(SP)
;		mov	(R5)+, R1
;		dec	2(R1)
;		bge	getcbuf
;		mov	R1, R0
;		add	#6, R0
;		mov	R0, bufoff
;		mov	R0, 4(R1)
;		mov	@R1, R0
;		trap	3
;; ---------------------------------------------------------------------------
;bufoff:	.word 40000		
;		.word 128.
;; ---------------------------------------------------------------------------
;		bcs	getw_err
;		tst	R0
;		bne	getw_ok
;
;getw_err:				
;		mov	(SP)+, R1
;		sec
;		rts	R5
;; ---------------------------------------------------------------------------
;
;getw_ok:				
;		dec	R0
;		mov	R0, 2(R1)
;
;getcbuf:				
;		clr	R0
;		bisb	@4(R1),	R0
;		inc	4(R1)
;		mov	(SP)+, R1
;		rts	R5
;; ---------------------------------------------------------------------------
;
;; end of 'RAM'
;
;		.END
