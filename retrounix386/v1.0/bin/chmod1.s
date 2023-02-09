; ****************************************************************************
; chmod386.s (chmod1.s) - by Erdogan Tan - 29/04/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 (v1.1 & v1.2) - chmod - change (file) mode
;
; [ Last Modification: 29/04/2022 ]
;
; Derived from (original) UNIX v2 (v1) 'chmod.s' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; svntree-20081216.tar.gz
; ****************************************************************************
; [ unix72/src/cmd/chmod.s (archive date: 16-12-2008) ]

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; 29/04/2022

	;mov	sp,r5
	;mov	(r5),r4
	;cmp	r4,$3
	;blt	chmerr
	;add	$4,r5
	;mov	(r5)+,r1
	;clr	0f

	pop	eax ; eax = number of arguments

	mov	edx, eax ; *
	
	;cmp	eax, 3
	cmp	al, 3
	jb	short chmerr

	;xor	ecx, ecx

	pop	eax
		; eax = argument 0 (file name: 'chmod')
	pop	esi
		; esi = argument 1 = mode (octal)
	mov	bl, 4 ; '7','7','7',0
;1:
chm_1:
	;movb	(r1)+,r0
	;beq	1f
	;asl	0f
	;asl	0f
	;asl	0f
	;bic	$!7,r0
	;bis	r0,0f
	;br	1b

	lodsb
	or	al, al
	jz	short chm_2
	;;; 29/04/2022
	dec	bl
	jz	short chmerr
	;;; 29/04/2022
	cmp	al, '7'
	ja	short chmerr	
	cmp	al, '0'
	jb	short chmerr
	;;;
	;shl	word [mode], 3 ; * 8
	shl	ecx, 3
	and	al, 7
	;or	byte [mode], al
	or	cl, al
	jmp	short chm_1
;1:
chm_2:		
	;mov	(r5)+,0f-2
	;sys	chmod; ..; 0:..
	;bes	chmerr
	;dec	r4
	;cmp	r4,$3
	;bge	1b
	;sys	exit

	pop	esi ; file name address
	;sys	_chmod, esi, [mode] 
	sys	_chmod, esi
	jc	short chmerr

	dec	edx ; *
	;cmp	edx, 3
	cmp	dl, 3
	jnb	short chm_2
exit:
	sys	_exit

chmerr:
	;mov	$1,r0
	;sys	write; 1f; 2
	;sys	exit

	sys	_write, 1, _1f, 4  ; ' ?' + CRLF
	;sys	_exit
	jmp	short exit 

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;align 4

;1:
_1f:	; <?\n>

	db ' ?', 0Dh, 0Ah, 0	

;mode:	dd 0


; 29/04/2022

;-----------------------------------------------------------------
; Original UNIX v2 - /bin/chmod source code (chmod.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------

;/ chmod - change mode
;
;chmode:
;	mov	sp,r5
;	mov	(r5),r4
;	cmp	r4,$3
;	blt	chmerr
;	add	$4,r5
;	mov	(r5)+,r1
;	clr	0f
;1:
;	movb	(r1)+,r0
;	beq	1f
;	asl	0f
;	asl	0f
;	asl	0f
;	bic	$!7,r0
;	bis	r0,0f
;	br	1b
;1:
;	mov	(r5)+,0f-2
;	sys	chmod; ..; 0:..
;	bes	chmerr
;	dec	r4
;	cmp	r4,$3
;	bge	1b
;	sys	exit
;
;chmerr:
;	mov	$1,r0
;	sys	write; 1f; 2
;	sys	exit
;
;1:	<?\n>
