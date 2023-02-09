; ****************************************************************************
; init386.s (init7.s) - Retro Unix 386 v1.2 - /etc/init - sys initialization 
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - /etc/init - process control initialization
;
; [ Last Modification: 19/03/2022 ]
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document> (Section E.12)
; ****************************************************************************
;
; init7.s (09/02/2022-02/03/2022)
;	('tmp/utmp' & '/tmp/wtmp' file writing code has been stripped out)	
; init4.s (17/11/2015) - Retro UNIX 386 v1
;
; (Retro UNIX 386 v1 - init386.s, NASM 2.11)
; Derived from INIT09.ASM (17/01/2014, Retro UNIX 8086 v1, MASM 6.11)  
; Derived from 'init.s' file of original UNIX v1
; INIT09.ASM, 17/01/2014 

; 19/03/2022
; 02/03/2022
; 27/02/2022
; 09/02/2022 (init7.s) - Retro UNIX 386 v1.2 & v1.1 (& v1.0)
; 24/01/2022 (init6.s) - Retro UNIX 386 v1.1
; 24/01/2022 (init5.s) - Retro UNIX 386 v1.2
; 17/11/2015 (init4.s)
; 23/10/2015 (init3.s)
; 14/10/2015 (init2.s)
; 17/09/2015
; 03/09/2015
; 27/08/2015
; 13/08/2015
; 07/08/2015
; 30/06/2015
; 14/07/2013

; 12/01/2022 (Retro UNIX 386 v1.2)
;
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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	sys	_intr, 0  ; disable time-out function 
	sys	_quit, 0  ; disable quit (ctrl+brk) signal

        sys	_open, ctty, 0 ; open tty0
	jc	short error

        sys	_open, ctty, 1 ; for read and write
	jc	short error

	sys	_write, 1, msg_te, sizeof_mte
        jc	short error
i0:
	sys	_read, 0, tchar, 1
	jc	short error

	;sys	_close, 0 ; close input file/tty
	;jc	short error
        ;sys	_close, 1 ; close output file/tty
	;jc	short error

	mov	al, [tchar]

	; 27/02/2022
	;cmp	al, EnterKey
	;je	short multiuser

	cmp	al, ESCKey
	je	short singleuser

	;jmp	short i0

	; 27/02/2022
	; (ALT+127 will ensure multiuser login without tty8 and tty9)
	cmp	al, 7Fh ; ALT+127
	je	short multiuser_x

	; 27/02/2022
	cmp	al, EnterKey
	je	short multiuser

	jmp	short i0
		
error:
	sys	_msg, error_msg, 255, 0Ch 
		; error message with red color (max. 255 chars)
exit:
	sys	_exit

haltsys:
	;hlt
	; 17/09/2015
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	jmp	short haltsys

singleuser:
i1:
help:
	sys	_close, 0 ; close input file/tty
	;jc	short error
	sys	_close, 1 ; close output file/tty
	;jc	short error
	sys	_open, ctty, 0 ; open control tty
	;jc	short error
        sys	_open, ctty, 1 ; for read an write
	;jc	short error
	;
	sys	_exec, shell, shellp
	;
 	jmp	short i1

multiuser_x:	; 27/02/2022
	mov	byte [itabs], 0 ; (disable serial port login)

multiuser:
	sys	_close, 0 ; close input file/tty
	;jc	short error
        sys	_close, 1 ; close output file/tty
	;jc	short error

; 09/02/2022

;	sys	_mount, fd1, usr
;			; root directory on mounted fd1
;			; disk is /usr
;	; 23/01/2022
;	; truncate /tmp/utmp 
;	sys	_creat, utmp, 1A4h ; Retro UNIX 386 1.2 
;				   ; (runix v2 fs inode flags)
;	;sys	_creat, utmp, 14 ; Retro UNIX 386 v1.1
;	;jc	short error	 ; (unix v1 fs inode flags)
;
;	sys	_close, eax	; close it
;	
;	; 23/01/2022
;	mov	byte [zero+8], 'x'
;	;mov	byte [zero+8], 0 ; put identifier
;				 ; in output buffer
;
;	call	wtmprec	     ; go to (call) write acting info

	; 23/10/2015
	;
	;; 10/12/2013
	;; 'Enable Multi Tasking' (Time-Out)
	;; system call (Retro UNIX 8086 v1 feature only !)
	sys	_emt, 1
	;;

	mov	esi, itab    ; address of table to ESI

; create shell processes
i2:
	lodsw		     ; 'x', x=0, 1... to AX
	and	ax, ax
	jz	short pwait  ; branch if table end
	
	mov	[ttyx+8], al ; put symbol in ttyx
	; 02/03/2022
	;mov	ecx, eax ; 27/02/2022
	mov	edi, esi
	call	dfork        ; go to make new init for this ttyx
	;mov	eax, ecx ; 27/02/2022
	stosw		     ; save child id in word offer
	 		     ;	'0', '1',...etc.
	mov	esi, edi
	jmp	short i2     ; set up next child

; wait for process to die
pwait:
	;sys	_write, 1, beep, 1 ; 10/12/2013
	;
        sys	_wait       ; wait for user to terminate process
	; 02/03/2022
	jnc	short i6    ; (eax = process ID of the child)
	; ERROR ! there is not a child process to wait ! 
	; (This may be only viable option for now!) - 02/03/2022 - 
	jmp	error	; (print error message then terminate) 	
i6:
	mov	esi, itab   ; initialize for search
	; 02/03/2022 
	;mov	dx, ax
	; 24/01/2022
	mov	edx, eax    ; (process ID of the child)

; search for process id
i3:
	lodsw		    ; bump ESI to child id location
	or	ax, ax
	jz	short pwait ; ? something silly

	lodsw
	; 24/01/2022
	cmp	edx, eax
	;cmp	dx, ax      ; which process has terminated
	jne	short i3    ; not this one

; take name out of utmp

; 09/02/2022
;	;mov	ecx, 4
;	; 24/01/2022
;	xor 	ecx, ecx
;	mov	cl, 4
;	sub	esi, 4	  ; process is found, point x to 'x'
;		          ; for it
;	sub	esi, ecx  ; 4
;	;push	esi	  ; save address on stack
;	mov	ax, [esi] ; move 'x' to AX
;	; 24/01/2022
;	sub	al, '0'
;	;sub	ax, '0'   ; remove zone bits from character
;	;;shl	ax, 4	  ; generate proper offset for seek
;	shl	ax, cl	  ; 4
;	;movzx	edx, ax	
;	; 24/01/2022
;	mov	edx, eax
;	mov	edi, zero
;	xor	eax, eax  ; 0 ; clear	
;	;mov	ecx, 4	  ; output buffer
;	rep	stosd	 
;	sys	_open, utmp, 1
;			  ; open file for writing
;	jc	short i4  ; if can't open, create user anyway
;	;movzx	edi, ax	  ; save file desc
;	; 24/01/2022
;	mov	edi, eax  ; save file descriptor	
;	sys	_seek, eax, edx, 0
;			  ; move to proper pointer position
;	sys	_write, edi, zero, 16
;			  ; zero this position in
;	sys	_close, edi
;			  ; close file

; re-create user process

	; 02/03/2022
	sub	esi, 4
i4:
	;pop	esi	  ; restore 'x' to ESI
	lodsw		  ; move it to AX
	mov	edi, esi 
	mov	[ttyx+8], al ; get correct ttyx

; 09/02/2022
;	mov	[zero+8], al
;	 		  ; move identifier to output buffer
;	call	wtmprec   ; go to write accting into

	call	dfork     ; fork
	stosw		  ; save id of child
	; 02/03/2022 
	;; 27/02/2022
	;mov	edx, eax  ; save id of child
	;	
	jmp	pwait	  ; go to wait for next process end

dfork:
	mov	ebx, i5 ; return address for new process
	sys	_fork
	jc	short dfork ; try again
	; 25/02/2022
	; eax = process number of the child
	; Note: the child will return (from sysfork)
	;	with process number of its parent
	retn

i5: ; to new copy of init
	;sys	_quit, 0  ; disable quit (ctrl+brk) signal
	;sys	_intr, 0  ; disable time-out function 
	;sys	_chown, ttyx, 0
	;;sys	_chmod; ttyx, 15
	;sys	_chmod; ttyx, 13 ; 23/01/2022
o0:	
	xor	ebx, ebx
	;xor	ch, ch
	;mov	cl, [ttyx+8]
	movzx	ecx, byte [ttyx+8]
	sub	cl, '0'
	; 17/01/2014
	; 07/12/2013
	; set console tty for current process
	;mov	dx, 0FF00h
	;mov	dh, 0FFh ; do not set cursor position
		     ; do not set serial port parameters
	;mov	edx, 0FF00h
	; 18/02/2022
	sub	edx, edx
	dec	dh
	; edx = 0FF00h
	sys	_stty
	;jc	short terminate
	; 19/03/2022
	jc	short o4
o1:
	; 16/11/2015	
	sys	_open, ttyx, 0 ; open this ttyx for reading
	; 19/03/2022
	jnc	short o2
	call	wait_for_terminal
	jmp	short o1
	; 17/11/2015
	;;jc	short terminate
	; 19/03/2022
	;jc	short o4
	; 18/02/2022
	;;;jc	short help1	
o2:
	; 16/11/2015
	sys	_open, ttyx, 1 ; open this ttyx for writing
	; 19/03/2022
	jnc	short o3
	call	wait_for_terminal
	jmp	short o2
	; 17/11/2015
	;;jc	short terminate
	; 19/03/2022
	;jc	short o4	
	; 18/02/2022
	;;;jc	short help1		
o3:	
	sys	_exec, getty, gettyp ; getty types <login> and
				; executes login which logs user
				; in and executes sh-
	; 14/10/2015
	sys	_msg, getty_error_msg, 255, 0Ch 
				; error msg with red color
o4:
	; 19/03/2022
	;movzx	ecx, byte [ttyx+8]
	mov	cl, [ttyx+8]
o5:
	sub	cl, '0'
	shl	cl, 2 ; * 4
	mov	byte [ecx], 0

terminate:
	sys	_exit  ; HELP!
;help1:
	jmp	help

wait_for_terminal:
	; 19/03/2022
	; 16/11/2015
	; 24/10/2015
	mov	cl, [ttyx+8]
	;sub	cl, '0'
	;cmp	cl, 8
	cmp	cl, '8'
	jb	short sysexit
	sys	_sleep
	retn
sysexit:
	pop	eax ; return address
	;jmp	short terminate
	; 19/03/2022
	jmp	short o5

; 09/02/2022
;wtmprec:
;	; 24/01/2022
;	; 23/10/2015 (wtmp_err)
;	;cmp	byte [wtmp_err], 0
;	;ja	short i6 	
;
;	sys	_time  ; get time
;	mov	[zero+10], eax ; move to output buffer
;
;	sys	_open, wtmp, 1 ; open accounting file
;	;jc	short i7
;	; 24/01/2022
;	jc	short i6
;
;	mov	esi, eax ; save file descriptor
;
;	sys	_seek, eax, 0, 2 ; move pointer to end of file
;	;;push	esi    ; save file descriptor
;	;jc	short i7
;
;	sys	_write, esi, zero, 16 ; write accting info
;	;;pop	ebx	  ; restore file descriptor
;	;jc	short i7
;
;	sys	_close, esi ; close file
;i6:
;	retn
;;i7:
;	;inc	byte [wtmp_err] ; 23/10/2015
;	;retn

; 23/01/2022
;here:
;	hlt	; General Protection Fault ! 17/09/2015
;	;jmp	short here
;	jmp	haltsys	

align 2
tchar:  db 0
align 2
ctty:   db "/dev/tty", 0
align 2
shell:  db "/bin/sh", 0
shellm: db "-", 0
;align 2
; 09/02/2022
;usr:	db "/usr",0
;align 2
; 09/02/2022	
;fd1:	db "/dev/fd1", 0
;
align 2
; 09/02/2022
;utmp:   db "/tmp/utmp", 0
;wtmp:   db "/tmp/wtmp", 0
ttyx:   db "/dev/ttyx", 0
getty:  db "/etc/getty",0

align 2
; 27/08/2015 (dw -> dd)
shellp: dd shellm
        dd 0
gettyp: dd getty
	dd 0
itab:
	db '0',0, 0,0
	db '1',0, 0,0
	db '2',0, 0,0
	db '3',0, 0,0
	db '4',0, 0,0
	db '5',0, 0,0
	db '6',0, 0,0
	db '7',0, 0,0
itabs:		; 27/02/2022
	; serial ports (COM1, COM2)
	db '8',0, 0,0
	db '9',0, 0,0
	dw 0

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_te:
        db 0Dh, 0Ah
        db 'Type ENTER to start in multi user mode', 0Dh, 0Ah
        db 'or type ESC to start in single user mode.'
        db 0Dh, 0Ah
sizeof_mte equ $ - msg_te 
        db 0
copy_right_msg:
	db  '(c) Erdogan TAN - 19/03/2022'
	db  0Dh, 0Ah, 0
error_msg:
	db 0Dh, 0Ah, 07h
	db '/etc/init error ! '
	db 0

getty_error_msg:
	; 14/10/2015
	db 0Dh, 0Ah, 07h
	db '/etc/getty error ! '
	db 0

; 23/10/2015
align 2
; 09/02/2022
;zero:
;	times 8 db 0
;	times 6 db 0
;	times 2 db 0	 
;
;wtmp_err:
;	db 0

; 23/01/2022
	
;-----------------------------------------------------------------
; Original UNIX v1 - /etc/init source code (init.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------

;/ init -- process control initialization
;
;mount = 21.
;
;	sys	intr; 0 / turn off interrupts
;	sys 	quit; 0
;	cmp	csw,$73700 / single user?
;	bne	1f / no
;help:
;	clr	r0 / yes
;	sys	close / close current read
;	mov	$1,r0 / and write
;	sys	close / files
;	sys	open; ctty; 0 / open control tty
;	sys	open; ctty; 1 / for read and write
;	sys	exec; shell; shellp / execute shell
;	br	help / keep trying
;1:
;	mov	$'0,r1 / prepare to change
;1 :
;	movb	r1,tapx+8 / mode of dec tape drive x, where
;	sys	chmod; tapx; 17 / x=0 to 7, to read/write by owner or
;	inc	r1 / non-owner mode
;	cmp	r1,$'8 / finished?
;	blo	1b / no
;	sys	mount; rk0; usr / yes, root file on mounted rko5
;				/ disk ls /usr
;	sys	creat; utmp; 16 / truncate /tmp/utmp
;	sys	close / close it
;	movb	$'x,zero+8. / put identifier in output buffer
;	jsr	pc,wtmprec / go to write accting info
;	mov	$itab,r1 / address of table to r1
;
;/ create shell processes
;
;1:
;	mov	(r1)+,r0 / 'x, x=0, 1... to r0
;	beq	1f / branch if table end
;	movb	r0,ttyx+8 / put symbol in ttyx
;	jsr	pc,dfork / go to make new init for this ttyx
;	mov	r0,(r1)+ / save child id in word offer '0, '1,...etc.
;	br	1b / set up next child
;
;/ wait for process to die
;
;1:
;	sys	wait / wait for user to terminate process
;	mov	$itab,r1 / initialize for search
;
;/ search for process id
;
;2:
;	tst	(r1)+ / bump r1 to child id location
;	beq	1b / ? something silly
;	cmp	r0,(r1)+ / which process has terminated
;	bne	2b / not this one
;
;/ take name out of utmp
;
;	sub	$4, r1 / process is found, point x' to 'x
;		       / for it
;	mov	r1,-(sp) / save address on stack
;	mov	(r1),r1 / move 'x to r1
;	sub	$'0,r1 / remove zone bits from character
;	asl	r1 / generate proper
;	asl	r1 / offset
;	asl	r1 / for
;	asl	r1 / seek
;	mov	r1,0f / move it to offset loc for seek
;	mov	$zero,r1
;2:
;	clr	(r1)+ / ccear-
;	cmp	r1,$zero+16. / output buffer
;	blo	2b / area
;	sys	open; utmp; 1 / open file for writing
;	bes	2f / if can't open, create user anyway
;	mov	r0,r1 / save file desc
;	sys	seek; 0:..; 0 / move to proper pointer position
;	mov	r1,r0 / not required
;	sys	write; zero; 16. / zero this position in
;	mov	r1,r0 / restore file descriptor
;	sys	close / close file
;
;/ re-create user process
;
;2:
;	mov	(sp)+,r1 / restore 'x to r1
;	mov	(r1)+,r0 / move it to r0
;	movb	r0,ttyx+8 / get correct ttyx
;	movb	r0,zero+8 / move identifier to output buffer
;	jsr	pc,wtmprec / go to write accting into
;	jsr	pc,dfork / fork
;	mov	r0,(r1)+ / save id of child
;	br	1b / go to wait for next process end
;
;dfork:
;	mov	r1,r2
;	sub	$itab+2,r2 / left over
;	asl	r2 / from previous
;	asl	r2 / version of code
;	mov	r2,offset
;	sys	fork
;		br 1f / to new copy of init
;	bes	dfork / try again
;	rts	pc / return
;1 :
;	sys	quit; 0 / new init turns off
;	sys	intr; 0 / interrupts
;	sys	chown; ttyx; 0 / change owner to super user
;	sys	chmod; ttyx; 15 / changemode to read/write owner,
;				/ write non-owner
;	sys	open; ttyx; 0 / open this ttyx for reading
;			      / and wait until someone calls
;	bes	help1 / branch if trouble
;	sys	open; ttyx; 1 / open this ttyx for writing after
;			      / user call
;	bes	help1 / branch if trouble
;	sys	exec; getty; gettyp / getty types <login> and
;				    / executes login which logs user
;				    / in and executes sh-
;	sys	exit / HELP!
;
;help1:
;	jmp	help / trouble
;
;wtmprec:
;	sys	time / get time
;	mov	ac,zero+10. / more to output
;	mov	mq,zero+12. / buffer
;	sys	open; wtmp; 1 / open accounting file
;	bes	2f
;	mov	r0,r2 / save file descriptor
;	sys	seek; 0; 2 / move pointer to end of file
;	mov	r2,r0 / not required
;	sys	write; zero; 16. / write accting info
;	mov	r2,r0 / restore file descriptor
;	sys	close / close file
;2:
;	rts	pc
;
;ctty:	</dev/tty\0>
;shell:	</bin/sh\0>
;shellm: <-\0>
;tapx:	</dev/tapx\0>
;rk0:	</dev/rk0\0>
;utmp:	</tmp/utmp\0>
;wtmp:	</tmp/wtmp\0>
;ttyx:	</dev/ttyx\0>
;getty:	</etc/getty\0>
;usr:	</usr\0>
;	.even
;
;shellp: shellm
;	0
;gettyp: getty
;	0
;itab:
;	'0; ..
;	'1; ..
;	'2; ..
;	'3; ..
;	'4; ..
;	'5; ..
;	'6; ..
;	'7; ..
;	0
;
;offset: .=.+2
;zero:	 .=.+8; .=.+6; .=.+2

; 23/01/2022

;-----------------------------------------------------------------
; Original UNIX v2 - /etc/init source code (init.s)
;		     in PDP-11 (unix) assembly language
;-----------------------------------------------------------------

; / init -- process control initialization
;
;	sys	intr; 0
;	sys	quit; 0
;	sys	38. / get console switches
;	cmp	r0,$173030
;	bne	1f
;help:
;	clr	r0
;	sys	close
;	mov	$1,r0
;	sys	close
;	sys	open; ctty; 0
;	sys	open; ctty; 1
;	sys	exec; shell; shellp
;	br	help
;1:
;	sys	mount; rk1; usr
;	sys	mount; rk2; ssys
;	sys	mount; rk3; crp
;	mov	$'0,r1
;1:
;	movb	r1,tapx+8
;	sys	chmod; tapx; 17
;	inc	r1
;	cmp	r1,$'8
;	blo	1b
;	sys	creat; utmp; 16
;	sys	close
;	sys	unlink; dpdlock
;	sys	fork
;		br daemon
;	sys	fork
;		br dirass
;	sys	fork
;		br dds
;	movb	$'x,zero+8.
;	jsr	pc,wtmprec
;	mov	$itab,r1
;	br	1f
;
;daemon:
;	sys	exec; etcdpd; etcdpdp
;	sys	exit
;
;dirass:
;	sys	chdir; usrmel
;	sys	exec; melda; meldap
;	sys	exit
;
;dds:
;	sys	exec; usrdd; usrddp
;	sys	exit
;
;/ create shell processes
;
;1:
;	mov	(r1)+,r0
;	beq	pwait
;	movb	r0,ttyx+8
;	jsr	pc,dfork
;	mov	r0,(r1)+
;	br	1b
;
;/ wait for process to die
;
;pwait:
;	sys	wait
;	mov	$itab,r1
;
;/ search for process id
;
;2:
;	tst	(r1)+
;	beq	pwait
;	cmp	r0,(r1)+
;	bne	2b
;
;/ take name out of utmp
;
;	sub	$4,r1
;	mov	r1,-(sp)
;	mov	(r1),r1
;	sub	$'0,r1
;	cmp	r1,$'a-'0
;	blo	2f
;	sub	$'a-'0-10.,r1	/ map a-z into 10. on
;2:
;	asl	r1
;	asl	r1
;	asl	r1
;	asl	r1
;	mov	r1,0f
;	mov	$zero,r1
;2:
;	clr	(r1)+
;	cmp	r1,$zero+16.
;	blo	2b
;	sys	open; utmp; 1
;	bes	2f
;	mov	r0,r1
;	sys	seek; 0:..; 0
;	mov	r1,r0
;	sys	write; zero; 16.
;	mov	r1,r0
;	sys	close
;
;/ re-create user process
;
;2:
;	mov	(sp)+,r1
;	mov	(r1)+,r0
;	movb	r0,ttyx+8
;	movb	r0,zero+8.
;	jsr	pc,wtmprec
;	jsr	pc,dfork
;	mov	r0,(r1)+
;	br	pwait
;
;dfork:
;	sys	fork
;		br 1f
;	bes	dfork
;	rts	pc
;1:
;	sys	quit; 0
;	sys	intr; 0
;	sys	chown; ttyx; 0
;	sys	chmod; ttyx; 15
;	sys	open; ttyx; 0
;	bes	help1
;	sys	open; ttyx; 1
;	bes	help1
;	sys	exec; getty; gettyp
;	sys	exit			/ HELP!
;
;help1:
;	jmp	help
;
;wtmprec:
;	mov	r1,-(sp)
;	sys	time
;	mov	r0,zero+10.
;	mov	r1,zero+12.
;	sys	open; wtmp; 1
;	bes	2f
;	mov	r0,r2
;	sys	seek; 0; 2
;	mov	r2,r0
;	sys	write; zero; 16.
;	mov	r2,r0
;	sys	close
;2:
;	mov	(sp)+,r1
;	rts	pc
;
;etcdpdp:
;	etcdpd; 0
;meldap:
;	melda; 0
;usrddp:
;	usrdd; 0
;usrdd:	</usr/demo/dds\0>
;melda:	</usr/mel/da\0>
;usrmel:</usr/mel\0>
;rk1:	</dev/rk1\0>
;rk2:	</dev/rk2\0>
;rk3:	</dev/rk3\0>
;usr:	</usr\0>
;ssys:	</sys\0>
;crp:	</crp\0>
;ctty:	</dev/tty\0>
;shell:	</bin/sh\0>
;shellm:<-\0>
;dpdlock:
;	</usr/dpd/lock\0>
;etcdpd:
;	</etc/dpd\0>
;tapx:	</dev/tapx\0>
;utmp:	</tmp/utmp\0>
;wtmp:	</tmp/wtmp\0>
;ttyx:	</dev/ttyx\0>
;getty:	</etc/getty\0>
;	.even
;
;shellp:shellm
;	0
;gettyp:getty
;	0
;itab:
;	'0; ..
;	'1; ..
;	'2; ..
;	'3; ..
;	'4; ..
;	'5; ..
;	'6; ..
;	'7; ..
;	'8; ..
;	'a; ..
;	'b; ..
;	 0
;
;	.bss
;offset:.=.+2
;zero:	.=.+8.; .=.+6; .=.+2.