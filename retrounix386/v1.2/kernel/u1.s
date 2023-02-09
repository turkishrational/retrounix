; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2) - SYS1.INC
; Last Modification: 27/12/2022 (Retro UNIX 386 v1.2, Kernel v0.2.2.4)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U1.ASM (12/07/2014) //// UNIX v1 -> u1.s
;
; ****************************************************************************

unkni: ; / used for all system calls
sysent: ; < enter to system call >
	; 27/12/2022
	; 25/12/2021 (Retro UNIX 386 v1.2)
	; 19/10/2015
	; 21/09/2015
	; 01/07/2015
	; 19/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/04/2013 - 18/01/2014 (Retro UNIX 8086 v1)
	;
	; 'unkni' or 'sysent' is sytem entry from various traps. 
	; The trap type is determined and an indirect jump is made to 
	; the appropriate system call handler. If there is a trap inside
	; the system a jump to panic is made. All user registers are saved 
	; and u.sp points to the end of the users stack. The sys (trap)
	; instructor is decoded to get the the system code part (see
	; trap instruction in the PDP-11 handbook) and from this 
	; the indirect jump address is calculated. If a bad system call is
	; made, i.e., the limits of the jump table are exceeded, 'badsys'
	; is called. If the call is legitimate control passes to the
	; appropriate system routine.
	;
	; Calling sequence:
	;	Through a trap caused by any sys call outside the system.
	; Arguments:
	;	Arguments of particular system call.	
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       System call number is in EAX register.
	;
	;       Other parameters are in EDX, EBX, ECX, ESI, EDI, EBP
	;	registers depending of function details.
  	;
	; 16/04/2015
        mov     [ss:u.sp], esp ; Kernel stack points to return address
	; save user registers
	push	ds
	push	es
	push	fs
	push	gs
	pushad  ; eax, ecx, edx, ebx, esp -before pushad-, ebp, esi, edi
	;
	; ESPACE = esp - [ss:u.sp] ; 4*12 = 48 ; 17/09/2015
	; 	(ESPACE is size of space in kernel stack 
	;	for saving/restoring user registers.)
	;
	push	eax ; 01/07/2015
	mov     ax, KDATA
        mov     ds, ax
        mov     es, ax
        mov     fs, ax
        mov     gs, ax
	mov	eax, [k_page_dir]
	mov	cr3, eax
	pop	eax ; 01/07/2015
	; 19/10/2015
	cld
	;
	inc	byte [sysflg]
		; incb sysflg / indicate a system routine is in progress
        sti 	; 18/01/2014
	;jnz	panic ; 24/05/2013
	; 04/12/2021
	jz	short _1
	jmp	panic
		; beq 1f
		; jmp panic ; / called if trap inside system
;1:
_1:	; 04/12/2021
	; 16/04/2015
	mov	[u.r0], eax
	mov	[u.usp], esp ; kernel stack points to user's registers
	;
		; mov $s.syst+2,clockp
		; mov r0,-(sp) / save user registers 
		; mov sp,u.r0 / pointer to bottom of users stack 
			   ; / in u.r0
		; mov r1,-(sp)
		; mov r2,-(sp)
		; mov r3,-(sp)
		; mov r4,-(sp)
		; mov r5,-(sp)
		; mov ac,-(sp) / "accumulator" register for extended
		             ; / arithmetic unit
		; mov mq,-(sp) / "multiplier quotient" register for the
		             ; / extended arithmetic unit
		; mov sc,-(sp) / "step count" register for the extended
		             ; / arithmetic unit
		; mov sp,u.sp / u.sp points to top of users stack
		; mov 18.(sp),r0 / store pc in r0
		; mov -(r0),r0 / sys inst in r0 10400xxx
		; sub $sys,r0 / get xxx code
	shl	eax, 2
		; asl r0 / multiply by 2 to jump indirect in bytes
	cmp	eax, end_of_syscalls - syscalls
		; cmp r0,$2f-1f / limit of table (35) exceeded
	;jnb	short badsys
		; bhis badsys / yes, bad system call
	; 25/12/2021
	jb	short _2
	jmp	badsys
_2:
	; 25/12/2021
	;cmc
	;pushf	
	;push	eax
 	mov 	ebp, [u.sp] ; Kernel stack at the beginning of sys call
	;mov	al, 0FEh ; 11111110b
	;;adc	al, 0 ; al = al + cf
	;and	[ebp+8], al ; flags (reset carry flag)
	and	byte [ebp+8], 0FEh ; 11111110b ; 25/12/2021
		; bic $341,20.(sp) / set users processor priority to 0 
				 ; / and clear carry bit
	;pop	ebp ; eax
	mov	ebp, eax ; 25/12/2021
	;popf
        ;;jc	badsys
	;; 04/12/2021
	;jnc	short _3
	;jmp	badsys
;_3:
	mov	eax, [u.r0]
	; system call registers: EAX, EDX, ECX, EBX, ESI, EDI
	jmp	dword [ebp+syscalls]
		; jmp *1f(r0) / jump indirect thru table of addresses
		            ; / to proper system routine.

	; 27/12/2022 (sysmemory)
	; 09/01/2022
	; 01/01/2022 (/etc/init error, BugFix efforts, test)
syscalls: ; 1:
	; 21/09/2015
	; 01/07/2015
	; 16/04/2015 (32 bit address modification) 
	dd sysrele	; / 0
	dd sysexit 	; / 1
	dd sysfork 	; / 2
	dd sysread 	; / 3
	dd syswrite 	; / 4
	dd sysopen 	; / 5
	dd sysclose 	; / 6
	dd syswait 	; / 7
	dd syscreat 	; / 8
	dd syslink 	; / 9
	dd sysunlink 	; / 10
	dd sysexec 	; / 11
	dd syschdir 	; / 12
	dd systime 	; / 13
	dd sysmkdir 	; / 14
	dd syschmod 	; / 15
	dd syschown 	; / 16
	dd sysbreak 	; / 17
	dd sysstat 	; / 18
	dd sysseek 	; / 19
	dd systell 	; / 20
	dd sysmount 	; / 21
	dd sysumount 	; / 22
	dd syssetuid 	; / 23
	dd sysgetuid 	; / 24
	dd sysstime 	; / 25
	dd sysquit 	; / 26
	dd sysintr 	; / 27
	dd sysfstat 	; / 28
	dd sysemt 	; / 29
	dd sysmdate 	; / 30
	dd sysstty 	; / 31
	dd sysgtty 	; / 32
	dd sysilgins 	; / 33
	dd syssleep 	; 34 ; Retro UNIX 8086 v1 feature only !
			     ; 11/06/2014
	dd sysmsg	; 35 ; Retro UNIX 386 v1 feature only !
			     ; 01/07/2015
	dd sysgeterr	; 36 ; Retro UNIX 386 v1 feature only !
			     ; 21/09/2015 - get last error number
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 27/03/2021 - Retro UNIX 386 v2
	dd syssetgid	; 37
	dd sysgetgid	; 38
	; 18/06/2021 - Retro UNIX 386 v2 (ref: TRDOS 386 v2)
	dd sysver	; 39 ; (get) Retro Unix 386 version
	; 27/12/2022 - Retro UNIX 386 v1.2
	dd sysmemory	; 40 ; get available (total&free) memory 	

end_of_syscalls:

error:
	; 11/12/2021 (Retro UNIX 386 v1.2)
	; 17/09/2015
	; 03/09/2015
	; 01/09/2015
	; 09/06/2015
	; 13/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/04/2013 - 07/08/2013 (Retro UNIX 8086 v1)
	;
	; 'error' merely sets the error bit off the processor status (c-bit)
	; then falls right into the 'sysret', 'sysrele' return sequence.
	;
	; INPUTS -> none
	; OUTPUTS ->
	;	processor status - carry (c) bit is set (means error)
	;
	; 26/05/2013 (Stack pointer must be reset here! 
	; 	      Because, jumps to error procedure
	;	      disrupts push-pop nesting balance)
	;
	mov	ebp, [u.sp] ; interrupt (system call) return (iretd) address
	or	byte [ebp+8], 1  ; set carry bit of flags register
				 ; (system call will return with cf = 1)
		; bis $1,20.(r1) / set c bit in processor status word below
		               ; / users stack
	; 17/09/2015
	sub	ebp, ESPACE ; 48 ; total size of stack frame ('sysdefs.inc')
				 ; for saving/restoring user registers	
	;cmp	ebp, [u.usp]
	;je	short err0	
	mov	[u.usp], ebp
;err0:
	; 01/09/2015
	mov	esp, [u.usp]	; Retro Unix 8086 v1 modification!
				; 10/04/2013
				; (If an I/O error occurs during disk I/O,
				; related procedures will jump to 'error'
				; procedure directly without returning to 
				; the caller procedure. So, stack pointer
				; must be restored here.)
	; 13/05/2015
	; NOTE: (The last) error code is in 'u.error', it can be retrieved by
	;	'get last error' system call later. 	

	; 03/09/2015 - 09/06/2015 - 07/08/2013
	mov 	byte [u.kcall], 0 ; namei_r, mkdir_w reset

sysret: ; < return from system call>
	; 11/12/2021
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 30/11/2021
	; 10/09/2015
	; 29/07/2015
	; 25/06/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/04/2013 - 23/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysret' first checks to see if process is about to be 
	; terminated (u.bsys). If it is, 'sysexit' is called.
	; If not, following happens:	 
	; 	1) The user's stack pointer is restored.
	;	2) r1=0 and 'iget' is called to see if last mentioned
	;	   i-node has been modified. If it has, it is written out
	;	   via 'ppoke'.
	;	3) If the super block has been modified, it is written out
	;	   via 'ppoke'.				
	;	4) If the dismountable file system's super block has been
	;	   modified, it is written out to the specified device
	;	   via 'ppoke'.
	;	5) A check is made if user's time quantum (uquant) ran out
	;	   during his execution. If so, 'tswap' is called to give
	;	   another user a chance to run.
	;	6) 'sysret' now goes into 'sysrele'. 
	;	    (See 'sysrele' for conclusion.)		
	;
	; Calling sequence:
	;	jump table or 'br sysret'
	; Arguments: 
	;	-	
	; ...............................................................
	;	
	; ((AX=r1 for 'iget' input))
	;	
	;xor	ax, ax ; 04/05/2013
	; 30/11/2021
	xor	eax, eax
sysret0: ; 29/07/2015 (eax = 0, jump from sysexec)
	inc	al ; 04/05/2013
	cmp	[u.bsys], al ; 1
		; tstb u.bsys / is a process about to be terminated because
	;jnb	sysexit ; 04/05/2013
	;	; bne sysexit / of an error? yes, go to sysexit
	; 04/12/2021
	jb	short _3
	; 11/12/2021
	mov	dword [u.error], ERR_INV_FUNC ; 1 ; 'invalid system call !'
	jmp	sysexit
_3:
	;mov	esp, [u.usp] ; 24/05/2013 (that is not needed here)
		; mov u.sp,sp / no point stack to users stack
	dec 	al ; mov ax, 0
		; clr r1 / zero r1 to check last mentioned i-node
	call	iget
		; jsr r0,iget / if last mentioned i-node has been modified
		            ; / it is written out
	;xor 	ax, ax ; 0
	; 30/11/2021
	xor	eax, eax
	cmp	[smod], al ; 0
		; tstb	smod / has the super block been modified
	jna	short sysret1
		; beq	1f / no, 1f
	mov	[smod], al ; 0
		; clrb smod / yes, clear smod
	mov	ebx, sb0 ;; 07/08//2013
   	or	word [ebx], 200h ;;
	;or	word [sb0], 200h ; write bit, bit 9
		; bis $1000,sb0 / set write bit in I/O queue for super block
		      	      ; / output
	; AX = 0
	call 	poke ; 07/08/2013
	;call	ppoke
	; AX = 0
		; jsr r0,ppoke / write out modified super block to disk
sysret1: ;1:
	cmp	[mmod], al ; 0
		; tstb	mmod / has the super block for the dismountable file
		           ; / system
	jna	short sysrel0
		; beq 1f / been modified?  no, 1f
	mov	[mmod], al ; 0	
		; clrb	mmod / yes, clear mmod
        ;mov    ax, [mntd]
        ;;mov   al, [mdev] ; 26/04/2013
	mov	ebx, sb1 ;; 07/08//2013
        ;;mov	[ebx], al
	;mov    [sb1], al
		; movb	mntd,sb1 / set the I/O queue
	or	word [ebx], 200h
	;or	word [sb1], 200h ; write bit, bit 9
		; bis $1000,sb1 / set write bit in I/O queue for detached sb
	call	poke ; 07/08/2013
	;call	ppoke 
		; jsr r0,ppoke / write it out to its device
        ;xor    al, al ; 26/04/2013       
;1:
		; tstb uquant / is the time quantum 0?
		; bne 1f / no, don't swap it out

sysrele: ; < release >
	; 14/10/2015
	; 01/09/2015
	; 24/07/2015
	; 14/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/04/2013 - 07/03/2014 (Retro UNIX 8086 v1)
	;
	; 'sysrele' first calls 'tswap' if the time quantum for a user is
	;  zero (see 'sysret'). It then restores the user's registers and
	; turns off the system flag. It then checked to see if there is
	; an interrupt from the user by calling 'isintr'. If there is, 
	; the output gets flashed (see isintr) and interrupt action is
	; taken by a branch to 'intract'. If there is no interrupt from
	; the user, a rti is made.
	;
	; Calling sequence:
	;	Fall through a 'bne' in 'sysret' & ?
	; Arguments:
	;	-	
	; ...............................................................
	;	
	; 23/02/2014 (swapret)
	; 22/09/2013
sysrel0: ;1:
	cmp	byte [u.quant], 0 ; 16/05/2013
		; tstb uquant / is the time quantum 0?
        ja      short swapret
		; bne 1f / no, don't swap it out
sysrelease: ; 07/12/2013 (jump from 'clock')
	call	tswap
		; jsr r0,tswap / yes, swap it out
;
; Retro Unix 8086 v1 feature: return from 'swap' to 'swapret' address.
swapret: ;1:
	; 10/09/2015
	; 01/09/2015
	; 14/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - 32 bit, pm modifications)
	; 26/05/2013 (Retro UNIX 8086 v1)
	; cli
	; 24/07/2015
	;
	;; 'esp' must be already equal to '[u.usp]' here ! 
	;; mov	esp, [u.usp]

	; 22/09/2013
	call	isintr
	; 20/10/2013
	jz	short sysrel1
	call	intract
		; jsr r0,isintr / is there an interrupt from the user
		;     br intract / yes, output gets flushed, take interrupt
		               ; / action
sysrel1:
	cli ; 14/10/2015
	dec	byte [sysflg]
		; decb sysflg / turn system flag off
	mov     eax, [u.pgdir]
	mov	cr3, eax  ; 1st PDE points to Kernel Page Table 0 (1st 4 MB)
			  ; (others are different than kernel page tables) 
	; 10/09/2015
	popad ; edi, esi, ebp, temp (increment esp by 4), ebx, edx, ecx, eax
		; mov (sp)+,sc / restore user registers
		; mov (sp)+,mq
		; mov (sp)+,ac
		; mov (sp)+,r5
		; mov (sp)+,r4
		; mov (sp)+,r3
		; mov (sp)+,r2
	;
	mov	eax, [u.r0]  ; ((return value in EAX))
	pop	gs
	pop	fs
	pop	es
	pop	ds
	iretd	
		; rti / no, return from interrupt

badsys:
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; (Major Modification: 'core' dumping procedure in
        ;       original UNIX v1 and Retro UNIX 8086 v1
	;	has been changed to print 'Invalid System Call !'
	;	message on the user's console tty.)
	; (EIP, EAX values will be shown on screen with error message)
	; (EIP = Return address just after the system call -INT 30h-)
	; (EAX = Function number)  
	;
	inc	byte [u.bsys]
	;
	mov	ebx, [u.sp] ; esp at the beginning of 'sysent'
	mov	eax, [ebx] ; EIP (return address, not 'INT 30h' address)
	call	dwordtohex
	mov	[bsys_msg_eip], edx
	mov	[bsys_msg_eip+4], eax
	mov	eax, [u.r0]
	call	dwordtohex
	mov	[bsys_msg_eax], edx
	mov	[bsys_msg_eax+4], eax
	; 11/12/2021
	;xor	eax, eax
	;mov	dword [u.base], badsys_msg ; "Invalid System Call !"
	;mov	ebx, [u.fofp]
	;mov	[ebx], eax
	;;mov	eax, 1 ; inode number of console tty (for user)	
	;inc	eax
	; 11/12/2021 - Retro UNIX 386 v2 fs compatibility 
	; (Retro UNIX 386 v1.2)
	;mov	al, 8 ; /dev/tty inode number (runix v2 fs)
	;; eax = 8 ; inode number of console tty (for user)	
	;mov	dword [u.count], BSYS_M_SIZE
		; writei
		; INPUTS ->
		;    r1 - inode number
		;    u.count - byte count to be written
		;    u.base - points to user buffer
		;    u.fofp - points to word with current file offset
		; OUTPUTS ->
		;    u.count - cleared
		;    u.nread - accumulates total bytes passed back	
		;
		; ((Modified registers: EDX, EBX, ECX, ESI, EDI, EBP)) 	
	;call	writei
	;;mov	eax, 1
	;jmp	sysexit

	; 11/12/2021 - Retro UNIX 386 v1.2
	mov	esi, badsys_msg ; "Invalid System Call !"
	movzx	ebx, byte [u.uno] ; process number
	mov	al, [ebx+p.ttyc-1] ; current/console tty
	mov	byte [ccolor], 0Fh ; white (message) color
	mov 	[u.ttyn], al ; current (active) tty (for user)
	call	print_cmsg
	;mov	dword [u.error], ERR_INV_FUNC ; 1 ; 'invalid system call !'
	jmp	error

		; incb u.bsys / turn on the user's bad-system flag
		; mov $3f,u.namep / point u.namep to "core\0\0"
		; jsr r0,namei / get the i-number for the core image file
		; br 1f / error
		; neg r1 / negate the i-number to open the core image file
		       ; / for writing
		; jsr r0,iopen / open the core image file
		; jsr r0,itrunc / free all associated blocks
		; br 2f
;1:
		; mov $17,r1 / put i-node mode (17) in r1
		; jsr r0,maknod / make an i-node
		; mov u.dirbuf,r1 / put i-node number in r1
;2:
		; mov $core,u.base / move address core to u.base
		; mov $ecore-core,u.count / put the byte count in u.count
		; mov $u.off,u.fofp / more user offset to u.fofp
		; clr u.off / clear user offset
		; jsr r0,writei / write out the core image to the user
		; mov $user,u.base / pt. u.base to user
		; mov $64.,u.count / u.count = 64
		; jsr r0,writei / write out all the user parameters
		; neg r1 / make i-number positive
		; jsr r0,iclose / close the core image file
		; br sysexit /
;3:
		; <core\0\0>

intract: ; / interrupt action
	; 14/10/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/05/2013 - 07/12/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; (Process/task switching and quit routine by using
	; Retro UNIX 8086 v1 keyboard interrupt output.))
	;
	; input -> 'u.quit'  (also value of 'u.intr' > 0)
	; output -> If value of 'u.quit' = FFFFh ('ctrl+brk' sign)
	;		'intract' will jump to 'sysexit'.
	;	    Intract will return to the caller 
	;		if value of 'u.quit' <> FFFFh. 	 
	; 14/10/2015
	sti
	; 07/12/2013	
	inc 	word [u.quit]
	jz	short intrct0 ; FFFFh -> 0
	dec	word [u.quit]
	; 16/04/2015
	retn
intrct0:	
	pop	eax ; call intract -> retn
	;
	xor 	eax, eax
	inc	al  ; mov ax, 1
;;;
	; UNIX v1 original 'intract' routine... 
	; / interrupt action
		;cmp *(sp),$rti / are you in a clock interrupt?
		; bne 1f / no, 1f
		; cmp (sp)+,(sp)+ / pop clock pointer
	; 1: / now in user area
		; mov r1,-(sp) / save r1
		; mov u.ttyp,r1 
			; / pointer to tty buffer in control-to r1
		; cmpb 6(r1),$177
			; / is the interrupt char equal to "del"
		; beq 1f / yes, 1f
		; clrb 6(r1) 
		        ; / no, clear the byte 
			; / (must be a quit character)
		; mov (sp)+,r1 / restore r1
		; clr u.quit / clear quit flag
		; bis $20,2(sp) 
		    	; / set trace for quit (sets t bit of 
			; / ps-trace trap)
		; rti   ;  / return from interrupt
	; 1: / interrupt char = del
		; clrb 6(r1) / clear the interrupt byte 
			   ; / in the buffer
		; mov (sp)+,r1 / restore r1
		; cmp u.intr,$core / should control be 
				; / transferred to loc core?
		; blo 1f
		; jmp *u.intr / user to do rti yes, 
				; / transfer to loc core
	; 1:
		; sys 1 / exit

sysexit: ; <terminate process>
	; 17/07/2022
	; 30/11/2021 - Retro UNIX 386 v1.2
	; 01/09/2015
	; 31/08/2015
	; 14/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/04/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysexit' terminates a process. First each file that
	; the process has opened is closed by 'flose'. The process
	; status is then set to unused. The 'p.pid' table is then
	; searched to find children of the dying process. If any of
	; children are zombies (died by not waited for), they are
	; set free. The 'p.pid' table is then searched to find the
	; dying process's parent. When the parent is found, it is
	; checked to see if it is free or it is a zombie. If it is
	; one of these, the dying process just dies. If it is waiting
	; for a child process to die, it notified that it doesn't 
	; have to wait anymore by setting it's status from 2 to 1
	; (waiting to active). It is awakened and put on runq by
	; 'putlu'. The dying process enters a zombie state in which
	; it will never be run again but stays around until a 'wait'
	; is completed by it's parent process. If the parent is not
	; found, process just dies. This means 'swap' is called with
	; 'u.uno=0'. What this does is the 'wswap' is not called
	; to write out the process and 'rswap' reads the new process
	; over the one that dies..i.e., the dying process is 
	; overwritten and destroyed.	
 	;
	; Calling sequence:
	;	sysexit or conditional branch.
	; Arguments:
	;	-	
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       System call number (=1) is in EAX register.
	;
	;       Other parameters are in EDX, EBX, ECX, ESI, EDI, EBP
	;       registers depending of function details.
	;
	; ('swap' procedure is mostly different than original UNIX v1.)
	;
; / terminate process
	; AX = 1
	;dec 	ax ; 0
	; 30/11/2021
	dec	eax ; 0
	mov	[u.intr], ax ; 0
		; clr u.intr / clear interrupt control word
		; clr r1 / clear r1
	; AX = 0
sysexit_1: ; 1:
	; AX = File descriptor
		; / r1 has file descriptor (index to u.fp list)
		; / Search the whole list
	call	fclose
		; jsr r0,fclose / close all files the process opened
	;; ignore error return
		; br .+2 / ignore error return
	;inc	ax
	inc	al
		; inc r1 / increment file descriptor
	;cmp	ax, 10
	cmp	al, 10
		; cmp r1,$10. / end of u.fp list?
	jb	short sysexit_1
		; blt 1b / no, go back
	movzx	ebx, byte [u.uno] ; 01/09/2015
		; movb	u.uno,r1 / yes, move dying process's number to r1
	mov	[ebx+p.stat-1], ah ; 0, SFREE, 05/02/2014
		; clrb p.stat-1(r1) / free the process
	;shl	bx, 1
	shl	bl, 1
		; asl r1 / use r1 for index into the below tables
	mov	cx, [ebx+p.pid-2]
		; mov p.pid-2(r1),r3 / move dying process's name to r3
	mov	dx, [ebx+p.ppid-2]
		; mov p.ppid-2(r1),r4 / move its parents name to r4
	; xor 	bx, bx ; 0
	xor	bl, bl ; 0
		; clr r2
	xor	esi, esi ; 0
		; clr r5 / initialize reg
sysexit_2: ; 1:
	        ; / find children of this dying process, 
		; / if they are zombies, free them
	;add	bx, 2
	add	bl, 2
		; add $2,r2 / search parent process table 
		          ; / for dying process's name
	cmp	[ebx+p.ppid-2], cx
		; cmp p.ppid-2(r2),r3 / found it?
	jne	short sysexit_4
		; bne 3f / no
	;shr	bx, 1
	shr	bl, 1
		; asr r2 / yes, it is a parent
	cmp	byte [ebx+p.stat-1], 3 ; SZOMB, 05/02/2014
		; cmpb p.stat-1(r2),$3 / is the child of this 
				     ; / dying process a zombie
	jne	short sysexit_3 
		; bne 2f / no
	mov	[ebx+p.stat-1], ah ; 0, SFREE, 05/02/2014
		; clrb p.stat-1(r2) / yes, free the child process
sysexit_3: ; 2:
	;shr	bx, 1
	shl	bl, 1
		; asl r2
sysexit_4: ; 3:
		; / search the process name table 
		; / for the dying process's parent
	cmp	[ebx+p.pid-2], dx ; 17/09/2013	
		; cmp p.pid-2(r2),r4 / found it?
	jne	short sysexit_5
		; bne 3f / no
	mov	esi, ebx
		; mov r2,r5 / yes, put index to p.pid table (parents
		          ; / process # x2) in r5
sysexit_5: ; 3:
	;cmp	bx, nproc + nproc
	cmp	bl, nproc + nproc
		; cmp r2,$nproc+nproc / has whole table been searched?
	jb	short sysexit_2
		; blt 1b / no, go back
		; mov r5,r1 / yes, r1 now has parents process # x2
	and	esi, esi ; r5=r1
	jz	short sysexit_6
		; beq 2f / no parent has been found. 
		       ; / The process just dies
	;shr	si, 1
	; 17/07/2022
	shr	esi, 1
		; asr r1 / set up index to p.stat
	mov	al, [esi+p.stat-1]
		; movb p.stat-1(r1),r2 / move status of parent to r2
	and	al, al
	jz	short sysexit_6
		; beq 2f / if its been freed, 2f
	cmp	al, 3
		; cmp r2,$3 / is parent a zombie?
	je	short sysexit_6
		; beq 2f / yes, 2f
	; BH = 0
	mov	bl, [u.uno]
		; movb u.uno,r3 / move dying process's number to r3
	mov	byte [ebx+p.stat-1], 3  ; SZOMB, 05/02/2014
		; movb $3,p.stat-1(r3) / make the process a zombie
	; 05/02/2014
	cmp	al, 1 ; SRUN
	je	short sysexit_6
	;cmp	al, 2
		; cmp r2,$2 / is the parent waiting for 
			  ; / this child to die
	;jne	short sysexit_6	
		; bne 2f / yes, notify parent not to wait any more
	; 05/02/2014
	; p.stat = 2 --> waiting
	; p.stat = 4 --> sleeping
	mov	byte [esi+p.stat-1], 1 ; SRUN ; 05/02/2014
	;dec	byte [esi+p.stat-1]
		; decb	p.stat-1(r1) / awaken it by putting it (parent)
	mov	ax, si ; r1  (process number in AL)
	; 
	;mov	ebx, runq + 4
		; mov $runq+4,r2 / on the runq
	call	putlu
		; jsr r0, putlu
sysexit_6: ; 2:
	; 31/08/2015
		; / the process dies
	mov	byte [u.uno], 0
		; clrb u.uno / put zero as the process number, 
	           ; / so "swap" will
	call	swap
		; jsr r0,swap / overwrite process with another process
hlt_sys:
	;sti ; 18/01/2014
hlts0:
	hlt
	jmp	short hlts0
		; 0 / and thereby kill it; halt?

syswait: ; < wait for a processs to die >
	; 09/02/2022
	; 08/01/2022 (Retro UNIX 386 v1.2)
	; 17/09/2015
	; 02/09/2015
	; 01/09/2015
	; 16/04/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/05/2013 - 05/02/2014 (Retro UNIX 8086 v1)
	;
	; 'syswait' waits for a process die. 
	; It works in following way:
	;    1) From the parent process number, the parent's 
	; 	process name is found. The p.ppid table of parent
	;	names is then searched for this process name.
	;	If a match occurs, r2 contains child's process
	;	number. The child status is checked to see if it is
	;	a zombie, i.e; dead but not waited for (p.stat=3)
	;	If it is, the child process is freed and it's name
	;	is put in (u.r0). A return is then made via 'sysret'.
	;	If the child is not a zombie, nothing happens and
	;	the search goes on through the p.ppid table until
	;	all processes are checked or a zombie is found.
	;    2) If no zombies are found, a check is made to see if
	;	there are any children at all. If there are none,
	;	an error return is made. If there are, the parent's
	;	status is set to 2 (waiting for child to die),
	;	the parent is swapped out, and a branch to 'syswait'
	;	is made to wait on the next process.
	;
	; Calling sequence:
	;	?
	; Arguments:
	;	-
	; Inputs: - 
	; Outputs: if zombie found, it's name put in u.r0.	
	; ...............................................................
	;				
	
; / wait for a process to die

syswait_0:
	movzx	ebx, byte [u.uno] ; 01/09/2015
		; movb u.uno,r1 / put parents process number in r1
	shl	bl, 1
	;shl	bx, 1
		; asl r1 / x2 to get index into p.pid table
	mov	ax, [ebx+p.pid-2]
		; mov p.pid-2(r1),r1 / get the name of this process
	xor	esi, esi
		; clr r2
	xor	ecx, ecx ; 30/10/2013
	;xor 	cl, cl
		; clr r3 / initialize reg 3
syswait_1: ; 1:
	; 09/02/2022
	inc	esi
	inc	esi
	;add	si, 2
		; add $2,r2 / use r2 for index into p.ppid table
			  ; / search table of parent processes 
			  ; / for this process name
	cmp	ax, [esi+p.ppid-2]
		; cmp p.ppid-2(r2),r1 / r2 will contain the childs 
			            ; / process number
	jne	short syswait_3
		; bne 3f / branch if no match of parent process name
	;inc	cx
	inc	cl
		; inc r3 / yes, a match, r3 indicates number of children
	; 09/02/2022
	shr	esi, 1
	;shr	si, 1
		; asr r2 / r2/2 to get index to p.stat table
	; The possible states ('p.stat' values) of a process are:
	;	0 = free or unused
	;	1 = active
	;	2 = waiting for a child process to die
	;	3 = terminated, but not yet waited for (zombie).	
	cmp	byte [esi+p.stat-1], 3 ; SZOMB, 05/02/2014
		; cmpb p.stat-1(r2),$3 / is the child process a zombie?
	jne	short syswait_2
		; bne 2f / no, skip it
	mov	[esi+p.stat-1], bh ; 0
		; clrb p.stat-1(r2) / yes, free it
	; 09/02/2022
	shl	esi, 1
	;shl	si, 1
		; asl r2 / r2x2 to get index into p.pid table
	movzx	eax, word [esi+p.pid-2]
	mov	[u.r0], eax
		; mov p.pid-2(r2),*u.r0 
			      ; / put childs process name in (u.r0)
	;
	; Retro UNIX 386 v1 modification ! (17/09/2015)
	;
	; Parent process ID -p.ppid- field (of the child process)
	; must be cleared in order to prevent infinitive 'syswait'
	; system call loop from the application/program if it calls
	; 'syswait' again (mistakenly) while there is not a zombie
	; or running child process to wait. ('forktest.s', 17/09/2015)
	;
	; Note: syswait will return with error if there is not a
	;       zombie or running process to wait.	
	;
	;sub	ax, ax
	; 08/01/2022
	sub	eax, eax
	mov 	[esi+p.ppid-2], ax ; 0 ; 17/09/2015
	jmp	sysret0 ; ax = 0
	;
	;jmp	sysret
		; br sysret1 / return cause child is dead
syswait_2: ; 2:
	; 09/02/2022
	shl	esi, 1
	;shl	si, 1
		; asl r2 / r2x2 to get index into p.ppid table
syswait_3: ; 3:
	cmp	si, nproc+nproc
		; cmp r2,$nproc+nproc / have all processes been checked?
	jb	short syswait_1
		; blt 1b / no, continue search
	;and	cx, cx
	and	cl, cl
		; tst r3 / one gets here if there are no children 
		       ; / or children that are still active
	; 30/10/2013
	jnz	short syswait_4
	;jz	error
		; beq error1 / there are no children, error
	mov	[u.r0], ecx ; 0
	; 09/02/2022
	mov	dword [u.error], ERR_MISC ; 27
			; miscellaneous/other errors
	jmp	error
syswait_4:
	mov	bl, [u.uno]
		; movb u.uno,r1 / there are children so put 
			      ; / parent process number in r1
	inc	byte [ebx+p.stat-1] ; 2, SWAIT, 05/02/2014
		; incb p.stat-1(r1) / it is waiting for 
				  ; / other children to die
	; 04/11/2013
	call	swap
		; jsr r0,swap / swap it out, because it's waiting
	jmp	syswait_0
		; br syswait / wait on next process

sysfork: ; < create a new process >
	; 27/02/2022
	; 08/01/2022 (Retro UNIX 386 v1.2)
	; 18/09/2015
	; 04/09/2015
	; 02/09/2015
	; 01/09/2015
	; 28/08/2015
	; 14/05/2015
	; 10/05/2015
	; 09/05/2015
	; 06/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/05/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	;
	; 'sysfork' creates a new process. This process is referred
	; to as the child process. This new process core image is
	; a copy of that of the caller of 'sysfork'. The only
	; distinction is the return location and the fact that (u.r0)
	; in the old process (parent) contains the process id (p.pid)
	; of the new process (child). This id is used by 'syswait'.
	; 'sysfork' works in the following manner: 	
	;    1) The process status table (p.stat) is searched to find
	;	a process number that is unused. If none are found
	;	an error occurs.
	;    2) when one is found, it becomes the child process number
	;	and it's status (p.stat) is set to active.
	;    3) If the parent had a control tty, the interrupt 
	;	character in that tty buffer is cleared.
	;    4) The child process is put on the lowest priority run 
	;	queue via 'putlu'.
	;    5) A new process name is gotten from 'mpid' (actually 
	;	it is a unique number) and is put in the child's unique
	;	identifier; process id (p.pid).
	;    6) The process name of the parent is then obtained and
	;	placed in the unique identifier of the parent process
	;	name is then put in 'u.r0'.	
	;    7) The child process is then written out on disk by
	;	'wswap',i.e., the parent process is copied onto disk
	;	and the child is born. (The child process is written 
	;	out on disk/drum with 'u.uno' being the child process
	;	number.)
	;    8) The parent process number is then restored to 'u.uno'.
	;    9) The child process name is put in 'u.r0'.
	;   10) The pc on the stack sp + 18 is incremented by 2 to
	;	create the return address for the parent process.
	;   11) The 'u.fp' list as then searched to see what files
	;	the parent has opened. For each file the parent has
	;	opened, the corresponding 'fsp' entry must be updated
	;	to indicate that the child process also has opened
	;	the file. A branch to 'sysret' is then made.	 			 				
	;
	; Calling sequence:
	;	from shell ?
	; Arguments:
	;	-
	; Inputs: -
	; Outputs: *u.r0 - child process name
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;	AX = r0 = PID (>0) (at the return of 'sysfork')
	;	= process id of child a parent process returns
	;	= process id of parent when a child process returns
	;
	;       In original UNIX v1, sysfork is called and returns as
	;	in following manner: (with an example: c library, fork)
	;	
	;	1:
	;		sys	fork
	;			br 1f  / child process returns here
	;		bes	2f     / parent process returns here
	;		/ pid of new process in r0
	;		rts	pc
	;	2: / parent process condionally branches here
	;		mov	$-1,r0 / pid = -1 means error return
	;		rts	pc
	;
	;	1: / child process brances here
	;		clr	r0   / pid = 0 in child process
	;		rts	pc
	;
	;	In UNIX v7x86 (386) by Robert Nordier (1999)
	;		// pid = fork();
	;		//
	;		// pid == 0 in child process; 
	;		// pid == -1 means error return
	;		// in child, 
	;		//	parents id is in par_uid if needed
	;		
	;		_fork:
	;			mov	$.fork,eax
	;			int	$0x30
	;			jmp	1f
	;			jnc	2f
	;			jmp	cerror
	;		1:
	;			mov	eax,_par_uid
	;			xor	eax,eax
	;		2:
	;			ret
	;
	;	In Retro UNIX 8086 v1,
	;	'sysfork' returns in following manner:
	;	
	;		mov	ax, sys_fork
	;		mov	bx, offset @f ; routine for child
	;		int	20h
	;		jc	error
	;		
	;	; Routine for parent process here (just after 'jc')
	;		mov	word ptr [pid_of_child], ax
	;		jmp	next_routine_for_parent	
	;
	;	@@: ; routine for child process here				
	;		....	
	;	NOTE: 'sysfork' returns to specified offset
	;	       for child process by using BX input.
	;	      (at first, parent process will return then 
	;	      child process will return -after swapped in-
	;	      'syswait' is needed in parent process
	;	      if return from child process will be waited for.)
	;	  				
	
; / create a new process
	; EBX = return address for child process 
	     ; (Retro UNIX 8086 v1 modification !)
	xor 	esi, esi
		; clr r1
sysfork_1: ; 1: / search p.stat table for unused process number
	inc	esi
		; inc r1
	cmp	byte [esi+p.stat-1], 0 ; SFREE, 05/02/2014
		; tstb p.stat-1(r1) / is process active, unused, dead
	jna	short sysfork_2	
		; beq 1f / it's unused so branch
	cmp	si, nproc
		; cmp r1,$nproc / all processes checked
	jb	short sysfork_1
		; blt 1b / no, branch back
	;
	; Retro UNIX 8086 v1. modification:
	;	Parent process returns from 'sysfork' to address 
	;	which is just after 'sysfork' system call in parent
	;	process. Child process returns to address which is put
	;	in BX register by parent process for 'sysfork'. 
	;
		; add $2,18.(sp) / add 2 to pc when trap occured, points
		             ; / to old process return
		; br error1 / no room for a new process
sysfork_0:
	jmp	error
sysfork_2: ; 1:
	call	allocate_page
	;jc	error
	; 08/01/2022
	jc	short sysfork_0

	push	eax   ; UPAGE (user structure page) address
	; Retro UNIX 386 v1 modification!
	call	duplicate_page_dir
		; EAX = New page directory 
	jnc	short sysfork_3
	pop	eax   ; UPAGE (user structure page) address
	call 	deallocate_page
	;jmp	error
	; 08/01/2022
	jmp	short sysfork_0  ; error
sysfork_3:
	; Retro UNIX 386 v1 modification !
	push	esi
	call	wswap ; save current user (u) structure, user registers
		      ; and interrupt return components (for IRET)
	xchg	eax, [u.pgdir] ; page directory of the child process
	mov	[u.ppgdir], eax ; page directory of the parent process
	pop	esi
	pop	eax   ; UPAGE (user structure page) address
		; [u.usp] = esp
	mov	edi, esi
	;shl	di, 2
	; 08/01/2022
	shl	edi, 2
	mov	[edi+p.upage-4], eax ; memory page for 'user' struct
	mov	[u.upage], eax ; memory page for 'user' struct (child)
	; 28/08/2015
	movzx	eax, byte [u.uno] ; parent process number
		; movb u.uno,-(sp) / save parent process number
	mov	edi, eax
        push	eax ; ** 
	mov     al, [edi+p.ttyc-1] ; console tty (parent)
	; 18/09/2015 (27/02/2022)
	mov	[esi+p.ttyc-1], al ; set child's console tty
	; 27/02/2022 (p.waitc is not used)
	;mov	[esi+p.waitc-1], ah ; 0 ; reset child's wait channel
	; 27/02/2022 (BugFix)
	;mov	[esi+p.ttyc-1], ax ; al - set child's console tty
	;			   ; ah - reset child's wait channel
	mov	eax, esi
	mov	[u.uno], al ; child process number
		;movb r1,u.uno / set child process number to r1
        inc     byte [esi+p.stat-1] ; 1, SRUN, 05/02/2014
		; incb p.stat-1(r1) / set p.stat entry for child 
				; / process to active status
		; mov u.ttyp,r2 / put pointer to parent process' 
			      ; / control tty buffer in r2
                ; beq 2f / branch, if no such tty assigned
		; clrb 6(r2) / clear interrupt character in tty buffer
	; 2:
	push	ebx  ; * return address for the child process
		     ; * Retro UNIX 8086 v1 feature only !	
	; (Retro UNIX 8086 v1 modification!)
		; mov $runq+4,r2
	call	putlu 
 		; jsr r0,putlu / put child process on lowest priority 
			   ; / run queue
	; 08/01/2022
	shl	esi, 1
	;shl	si, 1
		; asl r1 / multiply r1 by 2 to get index 
		       ; / into p.pid table
	inc	word [mpid]
		; inc mpid / increment m.pid; get a new process name
	mov	ax, [mpid]
	mov	[esi+p.pid-2], ax
		;mov mpid,p.pid-2(r1) / put new process name 
				    ; / in child process' name slot
	pop	edx  ; * return address for the child process
		     ; * Retro UNIX 8086 v1 feature only !	
  	pop	ebx  ; **
	;mov	ebx, [esp] ; ** parent process number
		; movb (sp),r2 / put parent process number in r2
	; 08/01/2022
	shl	ebx, 1
	;shl 	bx, 1
		; asl r2 / multiply by 2 to get index into below tables
	;movzx eax, word [ebx+p.pid-2]
	mov	ax, [ebx+p.pid-2]
		; mov p.pid-2(r2),r2 / get process name of parent
				   ; / process
	mov	[esi+p.ppid-2], ax
		; mov r2,p.ppid-2(r1) / put parent process name 
			  ; / in parent process slot for child
	mov	[u.r0], eax	
		; mov r2,*u.r0 / put parent process name on stack 
			     ; / at location where r0 was saved
	mov 	ebp, [u.sp] ; points to return address (EIP for IRET)
	mov	[ebp], edx ; *, CS:EIP -> EIP
			   ; * return address for the child process
		; mov $sysret1,-(sp) /
		; mov sp,u.usp / contents of sp at the time when 
			      ; / user is swapped out
		; mov $sstack,sp / point sp to swapping stack space
	; 04/09/2015 - 01/09/2015
	; [u.usp] = esp
	push	sysret ; ***
	mov	[u.usp], esp ; points to 'sysret' address (***)
			     ; (for child process)	
	xor 	eax, eax
	mov 	[u.ttyp], ax ; 0
	;
	call	wswap ; Retro UNIX 8086 v1 modification !
		;jsr r0,wswap / put child process out on drum
		;jsr r0,unpack / unpack user stack
		;mov u.usp,sp / restore user stack pointer
		; tst (sp)+ / bump stack pointer
	; Retro UNIX 386 v1 modification !
	pop	eax ; ***
	; 08/01/2022
	shl	ebx, 1
	;shl 	bx, 1
	mov     eax, [ebx+p.upage-4] ; UPAGE address ; 14/05/2015
	call	rswap ; restore parent process 'u' structure, 
		      ; registers and return address (for IRET)
		;movb (sp)+,u.uno / put parent process number in u.uno
        movzx   eax, word [mpid]
	mov	[u.r0], eax
		; mov mpid,*u.r0 / put child process name on stack 
			       ; / where r0 was saved
		; add $2,18.(sp) / add 2 to pc on stack; gives parent
			          ; / process return
	;xor	ebx, ebx
	xor     esi, esi
		;clr r1
sysfork_4: ; 1: / search u.fp list to find the files 
	      ; / opened by the parent process
	; 08/01/2022
	; 01/09/2015
	xor	bh, bh
	mov 	bl, [esi+u.fp]
	; 08/01/2022
	;mov 	al, [esi+u.fp]
		; movb u.fp(r1),r2 / get an open file for this process
	or	bl, bl
	; 08/01/2022
	;or	al, al
	jz	short sysfork_5	
		; beq 2f / file has not been opened by parent, 
		       ; / so branch
	;mov	ah, 10 ; Retro UNIX 386 v1 fsp structure size = 10 bytes
	; 08/01/2022
	;mov	ah, 16 ; Retro UNIX 386 v2 fsp structure size = 16 bytes
	;mul	ah
	;;movzx	ebx, ax
	;mov	bx, ax
	;mov	ebx, eax ; 08/01/2022
	;shl	bx, 3
		; asl r2 / multiply by 8
       		; asl r2 / to get index into fsp table
       		; asl r2
	; 08/01/2022
	shl	ebx, 4 ; multiply by 16
	; 08/01/2022
	inc	byte [ebx+fsp-10] ; Retro UNIX 386 v2 fs fsp structure
  	;inc	byte [ebx+fsp-2]
		; incb fsp-2(r2) / increment number of processes
			     ; / using file, because child will now be
			     ; / using this file
sysfork_5: ; 2:
        inc     esi
		; inc r1 / get next open file
        cmp     si, 10
		; cmp r1,$10. / 10. files is the maximum number which
			  ; / can be opened
	jb	short sysfork_4	
		; blt 1b / check next entry
	jmp	sysret
		; br sysret1

sysread: ; < read from file >
	; 24/12/2021
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 27/03/2020
	; 26/03/2020 - Retro UNIX 386 v2
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysread' is given a buffer to read into and the number of
	; characters to be read. If finds the file from the file
	; descriptor located in *u.r0 (r0). This file descriptor
	; is returned from a successful open call (sysopen).
	; The i-number of file is obtained via 'rw1' and the data
	; is read into core via 'readi'.
	;
	; Calling sequence:
	;	sysread; buffer; nchars
	; Arguments:
	;	buffer - location of contiguous bytes where 
	;		 input will be placed.
	;	nchars - number of bytes or characters to be read.
	; Inputs: *u.r0 - file descriptor (& arguments)
	; Outputs: *u.r0 - number of bytes read.	
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysread' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX register
	;	* 2nd argument, buffer address/offset in CX register
	;	* 3rd argument, number of bytes is in DX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with number of bytes read. 
	;

	call	rw1
	;jc	error ; 13/05/2015, ax < 1
	;	; jsr r0,rw1 / get i-number of file to be read into r1
       	; 04/12/2021
	jc	short sysread_err
	jnz	short rw3 ; error ! (permission denied)
	;		; read a file while it is open for write!

	; eax = inode number

	; 04/12/2021
	;((Retro Unix 386 v1 code)) (old code below)
	;test	ah, 80h
	;	; tst r1 / negative i-number?
	;;jnz	error
	;;	; ble error1 / yes, error 1 to read
	;;		   ; / it should be positive
	;; 04/12/2021
	;jnz	short sysread_err

	call	readi
		; jsr r0,readi / read data into core
	jmp	short rw0
		; br 1f
sysread_err:
syswrite_err:
	; 04/12/2021
	jmp	error

syswrite: ; < write to file >
	; 24/12/2021
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 27/03/2020
	; 26/03/2020 - Retro UNIX 386 v2
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 (Retro UNIX 8086 v1)
	;
	; 'syswrite' is given a buffer to write onto an output file
	; and the number of characters to write. If finds the file
	; from the file descriptor located in *u.r0 (r0). This file 
	; descriptor is returned from a successful open or create call
	; (sysopen or syscreat). The i-number of file is obtained via
	; 'rw1' and buffer is written on the output file via 'write'.
	;
	; Calling sequence:
	;	syswrite; buffer; nchars
	; Arguments:
	;	buffer - location of contiguous bytes to be writtten.
	;	nchars - number of characters to be written.
	; Inputs: *u.r0 - file descriptor (& arguments)
	; Outputs: *u.r0 - number of bytes written.	
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'syswrite' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX register
	;	* 2nd argument, buffer address/offset in CX register
	;	* 3rd argument, number of bytes is in DX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with number of bytes written. 
	;

	call	rw1
	;jc	error ; 13/05/2015, ax < 1
	;	; jsr r0,rw1 / get i-number in r1 of file to write
       	; 04/12/2021
	jc	short syswrite_err
	jz	short rw3 ; error ! (permission denied)
			; write to a file while it is open for read
	; eax = inode number
	
	; 04/12/2021
	;((Retro Unix 386 v1 code)) (old code below)
	;test	ah, 80h
	;	; tst r1 / positive i-number ?
        ;jz	short rw3 ; 13/05/2015
	;;jz	error
	;	; bge error1 / yes, error 1 
	;		   ; / negative i-number means write
        ;neg	ax
	;	; neg r1 / make it positive
	
	call	writei
        	; jsr r0,writei / write data
rw0: ; 1:
        mov	eax, [u.nread]
	mov	[u.r0], eax
		; mov u.nread,*u.r0 / put no. of bytes transferred
				  ; / into (u.r0)
	jmp	sysret
        	; br sysret1
rw3: 
	; 13/05/2015
	mov	dword [u.error], ERR_FILE_ACCESS ; permission denied !
	;stc
	;retn
	; 24/12/2021 - Retro UNIX 386 v1.2
	jmp	short syswrite_err

rw1:	
	; 04/12/2021 - Retro UNIX 386 v1.2
	; 27/03/2020
	; 26/03/2020 - Retro UNIX 386 v2
	; 14/05/2015
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/05/2013 - 24/05/2013 (Retro UNIX 8086 v1)
	; System call registers: bx, cx, dx (through 'sysenter')
	;
	;mov	[u.base], ecx 	; buffer address/offset 
				;(in the user's virtual memory space)
	;mov	[u.count], edx 
		; jsr r0,arg; u.base / get buffer pointer
        	; jsr r0,arg; u.count / get no. of characters
	;;mov	eax, ebx ; file descriptor
		; mov *u.r0,r1 / put file descriptor 
		             ; / (index to u.fp table) in r1
	; 13/05/2015
	mov	dword [u.r0], 0 ; r/w transfer count = 0 (reset)
	;
	;; call	getf
        ; ebx = File descriptor
	call	getf1 ; calling point in 'getf' from 'rw1'
		; jsr r0,getf / get i-number of the file in r1
	; AX = I-number of the file ; negative i-number means write

	; 04/12/2021 - Retro UNIX 386 v1.2
	; 26/03/2020 - Retro UNIX 386 v2
	; BL = open mode & status flag (0 = read, 1 = write)
	; eax = inode number (ax)
	; 27/03/2020
	; [cdev] = logical drive number 	

	; 13/05/2015
	cmp 	ax, 1
	jb	short rw2
	;
	mov	[u.base], ecx 	; buffer address/offset 
				;(in the user's virtual memory space)
	mov	[u.count], edx 
	; 14/05/2015
        mov     dword [u.error], 0 ; reset the last error code

	; 04/12/2021 - Retro UNIX 386 v1.2
	; 27/03/2020 - Retro UNIX 386 v2
	and	bl, 1
		; zf = 0 -> open for write	
		; zf = 1 -> open for read
	retn
        	; rts r0
rw2:
	; 13/05/2015
	mov	dword [u.error], ERR_FILE_NOT_OPEN ; file not open !
	retn

	; 18/04/2022
	; 08/01/2022
	; 02/01/2022
	; 01/01/2022
	; 27/12/2021
	; 22/11/2021 - Retro UNIX 386 v2 fs compatibility code
sysopen: ;<open file>
	; 04/12/2021
	; 07/08/2020 (Retro UNIX 386 v2)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 22/05/2013 - 27/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysopen' opens a file in following manner:
	;    1) The second argument in a sysopen says whether to
	;	open the file ro read (0) or write (>0).
	;    2) I-node of the particular file is obtained via 'namei'.
	;    3) The file is opened by 'iopen'.
	;    4) Next housekeeping is performed on the fsp table
	;	and the user's open file list - u.fp.
	;	a) u.fp and fsp are scanned for the next available slot.
	;	b) An entry for the file is created in the fsp table.
	;	c) The number of this entry is put on u.fp list.
	;	d) The file descriptor index to u.fp list is pointed
	;	   to by u.r0.
	;
	; Calling sequence:
	;	sysopen; name; mode
	; Arguments:
	;	name - file name or path name
	;	mode - 0 to open for reading
	;	       1 to open for writing
	; Inputs: (arguments)
	; Outputs: *u.r0 - index to u.fp list (the file descriptor)
	;		  is put into r0's location on the stack.	
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysopen' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with the file descriptor/number 
	;	(index to u.fp list).
	;
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * mode - sysopen argument 2 is in CX register 
	;          which is on top of stack.
	;
	; jsr r0,arg2 / get sys args into u.namep and on stack
	;
       	; system call registers: ebx, ecx (through 'sysenter')

	mov	[u.namep], ebx
	push	ecx ; * ; 04/12/2021 (cx -> ecx)
	call	namei
		; jsr r0,namei / i-number of file in r1
     	;and	ax, ax
	;jz	error ; File not found
	jc	short fnotfound ; 14/05/2015
	;jc	error ; 27/05/2013
		; br  error2 / file not found
   	;pop	edx ; * ; mode ; 04/12/2021
	;push	edx ; *
	;;or	dx, dx
	;or	dl, dl
	;	; tst (sp) / is mode = 0 (2nd arg of call;
	;	         ; / 0 means, open for read)
	;;jz	short sysopen_0
	;;	; beq 1f / yes, leave i-number positive
	
	mov	edx, [esp] ; *
	; edx = open mode (0 or 1)

;syscreat_0: ;op0: ; 27/12/2015
;	neg	ax
;        	; neg r1 / open for writing so make i-number negative

sysopen_0: ;1:
	call	iopen
		; jsr r0,iopen / open file whose i-number is in r1
	pop	edx ; * ; mode ; 04/12/2021
	;;and	dx, dx
	;and	dl, dl
        ;	; tst (sp)+ / pop the stack and test the mode
	;jz	short sysopen_2
        ;	; beq op1 / is open for read op1
;sysopen_1: ;op0:
	;neg	ax
        ;	; neg r1 
	;	     ;/ make i-number positive if open for writing [???]

	;; NOTE: iopen always make i-number positive.
	;; Here i-number becomes negative again. [22/05/2013]
sysopen_1: ; 04/12/2021
sysopen_2: ;op1:
        xor     esi, esi
        	; clr r2 / clear registers
        xor     ebx, ebx
		; clr r3
sysopen_3: ;1: / scan the list of entries in fsp table
        cmp     [esi+u.fp], bl ; 0
		; tstb u.fp(r2) / test the entry in the u.fp list
	jna	short sysopen_4
		; beq 1f / if byte in list is 0 branch
        inc     esi
		; inc r2 / bump r2 so next byte can be checked
	; 02/01/2022
	;cmp	si, OPENFILES ; 04/12/2021
	cmp	si, 10
		; cmp r2,$10. / reached end of list?
	jb	short sysopen_3
		; blt 1b / no, go back
toomanyf:
	; 14/05/2015
	mov	dword [u.error], ERR_TOO_MANY_FILES ; too many open files !
	jmp	error
        	; br error2 / yes, error (no files open)
fnotfound: 
	; 14/05/2015
	mov	dword [u.error], ERR_FILE_NOT_FOUND ; file not found !
	jmp	error

sysopen_4: ; 1:
        cmp     word [ebx+fsp], 0
		; tst fsp(r3) / scan fsp entries
        jna     short sysopen_5
		; beq 1f / if 0 branch
	;; 14/05/2015 - Retro UNIX 386 v1 modification !
	;add	bx, 10 ; fsp structure size = 10 bytes/entry
		; add $8.,r3 / add 8 to r3 
			; / to bump it to next entry mfsp table
	; 01/01/2022
	; 07/08/2020 - Retro UNIX 386 v2
	add	bx, 16 ; fsp structure size = 16 bytes/entry ; runix v2
	; 27/12/2021 - Retro UNIX 386 v1.2 (runix v2 fs)
	;add	bx, fp.size

	; 22/11/2021
	;cmp	bx, NFILES*fp.size ; NFILES*16
	; 02/01/2022
	cmp	bx, nfiles*16
	; 01/01/2022
	;cmp	bx, NFILES*16 ; NFILES*fp.size
	;;cmp	bx, nfiles*10
		; cmp r3,$[nfiles*8.] / done scanning
	jb	short sysopen_4
       		; blt 1b / no, back
	;jmp	error
        ;	; br error2 / yes, error
	; 04/12/2021
	; 07/08/2020
	jmp	short toomanyf

sysopen_5: ; 1: / r2 has index to u.fp list; r3, has index to fsp table
        mov     [ebx+fsp], ax
		; mov r1,fsp(r3) / put i-number of open file 
			; / into next available entry in fsp table,
	;mov	di, [cdev] ; word ? byte ?
        ;mov	[ebx+fsp+2], di ; device number
	;	; mov cdev,fsp+2(r3) / put # of device in next word

	; 18/04/2022
	; 04/12/2021        
	;mov	al, [cdev]
	;mov	ah, dl ; open mode, 0 = read, 1 = write
	;mov	[ebx+fsp+4], ax ; device number & open mode

	;xor	edi, edi
        ;mov	[ebx+fsp+4], edi ; offset pointer (0)
	;	; clr fsp+4(r3)
        ;mov	[ebx+fsp+8], di ; open count (0), deleted flag (0)
       	;	; clr fsp+6(r3) / clear the next two words

	; 04/12/2021 
	xor	eax, eax
  	mov	[ebx+fsp+8], eax ; offset pointer = 0

	;;inc	word [ebx+fsp+6]
	;inc	byte [ebx+fsp+6] ; open count = open count + 1	
	; 18/04/2022
	;mov	word [ebx+fsp+6], ax ; open count = 0 
				     ; (sysfork increases open count)
	; 18/04/2022		     ; reserved (mnt) flag = 0
	mov	al, [cdev]
	mov	ah, dl ; open mode, 0 = read, 1 = write
	mov	[ebx+fsp+4], eax ; device number (al)
				 ; & open mode (ah)
				 ; (& open count = 0)
  	;mov	eax, ebx
	;mov	bl, 10
	;div	bl 
	;	; asr r3
	;	; asr r3 / divide by 8 
	;	; asr r3 ; / to get number of the fsp entry-1
	;inc	al
        ;	; inc r3 / add 1 to get fsp entry number
        ;mov	[esi+u.fp], al
	;	; movb r3,u.fp(r2) / move entry number into 
			; / next available slot in u.fp list
	; 04/12/2021
	shr	ebx, 4	; / 16
	;shr	bx, 4	; bx = fsp entry number (index)	
			; bx <= 49 for current runix 386 version
	inc 	bl	; bl = 1 to 50
	mov	[esi+u.fp], bl 	

        mov     [u.r0], esi
		; mov r2,*u.r0 / move index to u.fp list 
			     ; / into r0 loc on stack
        jmp	sysret
		; br sysret2

; 27/03/2020 - Retro UNIX 386 v2 - FSP (OPEN FILES) TABLE 
;
;         15                    7                   0
;  1     |-------------------------------------------|
;        |   	     i-number of open file           |
;        |-------------------------------------------| 
;        |        high word of 32 bit i-number       |
;        |-------------------------------------------|
;        | open mode & status  |   device number     |
;        |-------------------------------------------|			
;        |    reserved byte    |     open count      |
;        |-------------------------------------------| 
;        | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 16-31)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 32-47)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 48-63)  | 
;        |-------------------------------------------|
;  2     |                                           |
;        |-------------------------------------------| 
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------| 
;        |                                           | 

	;
	; 'fsp' table (10 bytes/entry)
	; bit 15				   bit 0
	; ---|-------------------------------------------
	; r/w|		i-number of open file
	; ---|-------------------------------------------
	;		   device number
	; -----------------------------------------------
	; offset pointer, r/w pointer to file (bit 0-15)
	; -----------------------------------------------
	; offset pointer, r/w pointer to file (bit 16-31)
	; ----------------------|------------------------
	;  flag that says file 	| number of processes
	;   has been deleted	| that have file open 
	; ----------------------|------------------------
	;

	; 12/03/2022
	; 11/02/2022
	; 01/01/2022
	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
syscreat: ; < create file >
	; 12/03/2022
	; 11/02/2022
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 04/04/2021
	; 27/03/2021 (Retro UNIX 386 v2 - Beginning)
	; 27/12/2015 (Retro UNIX 386 v1.1)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 27/05/2013 (Retro UNIX 8086 v1)
	;
	; 'syscreat' called with two arguments; name and mode.
	; u.namep points to name of the file and mode is put
	; on the stack. 'namei' is called to get i-number of the file.		
	; If the file aready exists, it's mode and owner remain 
	; unchanged, but it is truncated to zero length. If the file
	; did not exist, an i-node is created with the new mode via
	; 'maknod' whether or not the file already existed, it is
	; open for writing. The fsp table is then searched for a free
	; entry. When a free entry is found, proper data is placed
	; in it and the number of this entry is put in the u.fp list.
	; The index to the u.fp (also know as the file descriptor)
	; is put in the user's r0. 			
	;
	; Calling sequence:
	;	syscreate; name; mode
	; Arguments:
	;	name - name of the file to be created
	;	mode - mode of the file to be created
	; Inputs: (arguments)
	; Outputs: *u.r0 - index to u.fp list 
	;		   (the file descriptor of new file)
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'syscreate' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	;	AX register (will be restored via 'u.r0') will return
	;	to the user with the file descriptor/number 
	;	(index to u.fp list).
	;
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * mode - sysopen argument 2 is in CX register 
	;          which is on top of stack.
	;
        	; jsr r0,arg2 / put file name in u.namep put mode 
			    ; / on stack
	mov	[u.namep], ebx ; file name address
	;push	cx ; mode
	; 04/12/2021
	push	ecx ; cx = mode (permission flags)
	call 	namei        	
		; jsr r0,namei / get the i-number
        ;and	ax, ax
	;jz	short syscreat_1	       	
	jc	short syscreat_1
		; br 2f / if file doesn't exist 2f
	; 27/12/2015
	;cmp	ax, 41 ; device inode ?
        ;jb	syscreat_0 ; yes
	;
	;neg 	ax
        ;	; neg r1 / if file already exists make i-number 
		       ; / negative (open for writing)
	; 11/02/2022
	; Truncate existing file
;syscreate_2:
	;; 27/03/2021
	;;xor	edx, edx ; 0
	;;inc	dl ; DL = 1 ; create/truncate (open for write)
	;mov	dl, 1
	; 11/02/2022
	mov	dl, 2 ; create file (call from syscreat)
	call	iopen
         	; jsr r0,iopen
	; 11/02/2022
	; cpu will return here if inode in eax is regular file inode
	; (if it is device or dir inode, cpu will jumpt to 'error')
	;
	; 12/03/2022
	push	eax ; * ; save inode number
	; 
	; truncate file to zero length
	;call	itrunc
        	; jsr r0,itrunc / truncate to 0 length
	; 11/02/2022
	; (iget and regular file check -in 'itrunc'- is not needed) 
	call	itrunc_1
	; 12/03/2022
	pop	eax ; * ; restore inode number in eax
	;
	;pop	cx ; pop mode (did not exist in original Unix v1 !?)
	; 04/12/2021
	pop	ecx
syscreat_2:
	mov	dl, 1 ; open for writing
        jmp     sysopen_1
        	; br op0
syscreat_1: ; 2: / file doesn't exist
	;pop	ax
        ;	; mov (sp)+,r1 / put the mode in r1
	; 27/03/2021
	pop	eax  ; ax = mode (permission flags)
	;xor	ah, ah	
        ;	; bic $!377,r1 / clear upper byte
	; 27/03/2021
	;(ref: Retro UNIX 386 v2 inode flags,'ux.s')
	; clear bits 15,14,13,12,9 of mode (input from user)
	and	ah, 0Dh ; ISUID (800h) & ISGID (400h) & IREAD (100h)
	; 11/02/2022
	or	ah, 80h	; IFREG (8000h) ; Regular file
	call 	maknod
        	; jsr r0,maknod / make an i-node for this file
	;sub	eax, eax ; 04/12/2021
	mov	ax, [u.dirbuf]
        	; mov u.dirbuf,r1 / put i-number 
			        ; / for this new file in r1
	; 04/04/2021
	;mov	dl, 1 ; open for writing
        ;
	;jmp	sysopen_1
        ;	; br op0 / open the file
	; 04/04/2021
	jmp	short syscreat_2

	; 11/02/2022
	; 04/12/2021
dir_access_err:	; 13/03/2022
;	; 14/05/2015
;	mov	dword [u.error], ERR_DIR_ACCESS ; permission denied !
;f_create_error:
	; 27/03/2021
	mov	dword [u.error], ERR_PERM_DENIED ; permission denied !	
	jmp	error

	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
sysmkdir: ; < make directory >
	; 13/03/2022
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 02/04/2021
	; 27/03/2021 (Retro UNIX 386 v2 - Beginning)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 27/05/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysmkdir' creates an empty directory whose name is
	; pointed to by arg 1. The mode of the directory is arg 2.	
	; The special entries '.' and '..' are not present.
	; Errors are indicated if the directory already exists or		
	; user is not the super user. 
	;
	; Calling sequence:
	;	sysmkdir; name; mode
	; Arguments:
	;	name - points to the name of the directory
	;	mode - mode of the directory
	; Inputs: (arguments)
	; Outputs: -
	;    (sets 'directory' flag to 1; 
	;    'set user id on execution' and 'executable' flags to 0)
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysmkdir' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
		
; / make a directory

	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * mode - sysopen argument 2 is in CX register 
	;          which is on top of stack.

		; jsr r0,arg2 / put file name in u.namep put mode 
			    ; / on stack
	mov	[u.namep], ebx
	;push	cx ; mode
	; 27/03/2021
	push	ecx ; cx = mode
	call	namei
        	; jsr r0,namei / get the i-number
        	;     br .+4 / if file not found branch around error
        ;xor 	ax, ax
	;jnz	error
	jnc	short dir_exists ; 14/05/2015
	;jnc	error	
		; br error2 / directory already exists (error)

	;cmp	byte [u.uid], 0 ; 02/08/2013
        ;	;tstb u.uid / is user the super user
	;jna	short dir_access_err ; 14/05/2015
	;;jna	error
        ;	;bne error2 / no, not allowed
	;pop	ax
        ;	;mov (sp)+,r1 / put the mode in r1
	;and	ax, 0FFCFh ; 1111111111001111b
        ;	;bic $!317,r1 / all but su and ex
	;;or	ax, 4000h ; 1011111111111111b
	;or	ah, 40h ; Set bit 14 to 1
        ; 	;bis $40000,r1 / directory flag

	; 02/04/2021
	;cmp	word [u.uid], 0
	;;ja	error
	;ja	short dir_access_err

	; 13/03/2022
	; NOTE:
	; Unix v5-v7 kernels do not allow (ordinary) users
	; (except root/superuser) --if [u.uid] > 0--
	; to make a sub directory. (ref: sys2.c, 'mknod')
	;
	; But, Retro UNIX 386 v1.2 will allow the owner of
	; the parent directory to make a sub directory, here.

	; 13/03/2022
	cmp	word [u.uid], 0
	jna	short sysmkdir1 ; root (superuser)

	; Here..
	; (current) inode buffer contains inode structure
	; of the parent directory (at the return of 'namei').

	mov	ax, [i.uid] ; owner ID of the parent dir
	cmp	ax, [u.uid] ; user ID of current user/process
	jne	short dir_access_err
	; 13/03/2022
	; additional checking (may or may not be necessary!?)
	cmp	ax, [u.ruid] 
			; real (login) user ID must be same
	jne	short dir_access_err

	; 02/04/2021
	; ('mkdir' procedure will be called from 'maknod'
	; and then 'mkdir' will check write access permission) 
	; ((so, 'access_w' call is not needed here.))

	;; 02/04/2021
	;; ('make directory' user permission check)
	;; ((current directory's write permission flags
	;;  will be checked against user's 'uid' & gid'))
	;mov	dx, 80h ; IWRITE
	;call	access_w  ; (in 'access', 'u5.s')
	;; (If cpu will return here, the user has write permission)

sysmkdir1:
	; 27/03/2021
	pop	eax  ; ax = mode
	; [ii] = current directory's inode number

	; 27/03/2021
	;(ref: Retro UNIX 386 v2 inode flags,'ux.s')
	; clear bits 13,12,11,10,9,6,3,0 of mode (input from user)
	;and	ax, 0C1B6h ; bits 15,14,8,7,5,4,2,1
	and	ax, 01B6h
	or	ah, 0C0h ; IFREG (8000h) + IFDIR (4000h) 
			 ; Directory
	call	maknod
        	;jsr r0,maknod / make the i-node for the directory
	jmp	sysret
        	;br sysret2 /
dir_exists:
	; 27/03/2021
	; (same error number for files and directories)
	; Error Number: 14 (ERR_DIR_EXISTS)
	mov	dword [u.error], ERR_FILE_EXISTS
				; 'file already exists !' error
	; 04/12/2021
	jmp	error
;dir_access_err:
;	; 14/05/2015
;	mov	dword [u.error], ERR_DIR_ACCESS ; permission denied !
;	jmp	error

	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
sysclose: ;<close file>
	; 02/03/2022
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 22/05/2013 - 26/05/2013 (Retro UNIX 8086 v1)
	;
	; 'sysclose', given a file descriptor in 'u.r0', closes the
	; associated file. The file descriptor (index to 'u.fp' list)
	; is put in r1 and 'fclose' is called.
	;
	; Calling sequence:
	;	sysclose
	; Arguments:
	;	-  
	; Inputs: *u.r0 - file descriptor
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification:
	;	 The user/application program puts file descriptor
	;        in BX register as 'sysclose' system call argument.
	; 	 (argument transfer method 1)

	; / close the file
	
	;mov 	eax, ebx
	; 04/12/2021
	; ebx = file descriptor/number
	;call 	fclose
	; 02/03/2022
	call	_fclose
		; mov *u.r0,r1 / move index to u.fp list into r1
		; jsr r0,fclose / close the file
               	; br error2 / unknown file descriptor
		; br sysret2
	;; 14/05/2015
	;jnc	sysret
	; 04/12/2021
	jc	short sysclose_err
	jmp	sysret
sysclose_err:
	mov	dword [u.error], ERR_FILE_NOT_OPEN ; file not open !
	jmp	error

	; 23/02/2022
	; 19/12/2021
	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
sysemt:
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/12/2013 - 20/04/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification: 
	;	'Enable Multi Tasking' system call instead 
	;	of 'Emulator Trap' in original UNIX v1 for PDP-11.
	;
	; Retro UNIX 8086 v1 feature only!
	;	Using purpose: Kernel will start without time-out
	;	(internal clock/timer) functionality.
	;	Then etc/init will enable clock/timer for
	;	multi tasking. (Then it will not be disabled again
	;	except hardware reset/restart.)
	;
	
	;cmp	byte [u.uid], 0 ; root ?
	;;ja	error
	;ja	badsys ; 14/05/2015
	; 04/12/2021
	cmp	word [u.uid], 0 ; root ?
	jna	short emt_0 
	jmp	badsys
emt_0:
	cli	; 23/02/2022
	and	ebx, ebx
	jz	short emt_2
	; Enable multi tasking -time sharing-
	mov	eax, clock ; enable multi tasking clock/timer
	; 23/02/2022
	mov	edx, rtci_default ; disable rtc (digital) printing
emt_1:
	mov	[x_timer], eax
	; 23/02/2022 (Temporary)
	mov	[x_rtci], edx
	mov	bl, 6  ; timer interrupt page, video page 6
	call	wttyc  ; clear video page
	mov	bl, 7  ; rtc interrupt page, video page 7
	call	wttyc  ; clear video page
	;
	sti	; 23/02/2022
	jmp	sysret
emt_2:
	; Disable multi tasking -time sharing-
	mov	eax, u_timer ; enable timer tick printing
	; 23/02/2022
	mov	edx, rtc_p   ; enable rtc (digital) printing
	;
	jmp	short emt_1

	; Original UNIX v1 'sysemt' routine
;sysemt:
        ;
	;jsr    r0,arg; 30 / put the argument of the sysemt call 
			 ; / in loc 30
        ;cmp    30,$core / was the argument a lower address 
			; / than core
        ;blo    1f / yes, rtssym
        ;cmp    30,$ecore / no, was it higher than "core" 
			; / and less than "ecore"
        ;blo    2f / yes, sysret2
;1:
        ;mov    $rtssym,30
;2:
        ;br     sysret2

sysilgins:
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 03/06/2013
	; Retro UNIX 8086 v1 modification: 
	;	not a valid system call ! (not in use)
	;
	jmp	badsys
	;jmp	error
	;;jmp 	sysret

	; Original UNIX v1 'sysemt' routine
;sysilgins: / calculate proper illegal instruction trap address
        ;jsr    r0,arg; 10 / take address from sysilgins call
			  ;/ put it in loc 8.,
        ;cmp    10,$core / making it the illegal instruction 
		       ; / trap address
        ;blo    1f / is the address a user core address?  
		; / yes, go to 2f
        ;cmp    10,$ecore
        ;blo    2f
;1:
        ;mov    $fpsym,10 / no, make 'fpsum' the illegal 
		    ; / instruction trap address for the system
;2:
        ;br     sysret2 / return to the caller via 'sysret'

	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
sysmdate: ; < change the modification time of a file >
	; 23/02/2022 (Retro UNIX 386 v1 feature/modification)
	;	(ECX input)
	; 24/12/2021
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 16/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 03/06/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysmdate' is given a file name. It gets inode of this 
	; file into core. The user is checked if he is the owner 
	; or super user. If he is neither an error occurs.
	; 'setimod' is then called to set the i-node modification
	; byte and the modification time, but the modification time
	; is overwritten by whatever get put on the stack during
	; a 'systime' system call. This calls are restricted to
	; the super user.		
	;
	; Calling sequence:
	;	sysmdate; name
	; Arguments:
	;	name - points to the name of file
	; Inputs: (arguments)
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;	 The user/application program puts address 
	;	 of the file name in BX register 
	;	 as 'sysmdate' system call argument.
	;
; / change the modification time of a file
		; jsr r0,arg; u.namep / point u.namep to the file name
        mov	[u.namep], ebx
	; 23/02/2022 - (Retro UNIX 386 v1 modification on unix v1 code)
	mov	[p_time], ecx ; save new modification time to be set 
	call	namei
		; jsr r0,namei / get its i-number
	;;jc	error       
	;	; br error2 / no, such file
	;jc	fnotfound ; file not found !
	; 24/12/2021
	jnc	short mdate_0
	jmp	fnotfound
mdate_0: 
	call	iget
		; jsr r0,iget / get i-node into core
	;mov	al, [u.uid]
	;cmp	al, [i.uid]
        ;	; cmpb u.uid,i.uid / is user same as owner
	;je	short mdate_1
        ;	; beq 1f / yes
	;and	al, al
	;	; tstb u.uid / no, is user the super user
	;;jnz	error
	;	; bne error2 / no, error
	;jz	short mdate_1
	; 04/12/2021
	mov	ax, [u.uid]
	cmp	ax, [i.uid]
	je	short mdate_1	
	and	ax, ax
	jz	short mdate_1

	mov	dword [u.error], ERR_FILE_ACCESS ; permission denied !
sysstty_err:	; 06/02/2022
	jmp	error
mdate_1: ;1:
	call	setimod
        	; jsr r0,setimod / fill in modification data,
		               ; / time etc.
	mov	esi, p_time
	mov	edi, i.mtim
	movsd
		; mov 4(sp),i.mtim / move present time to
        	; mov 2(sp),i.mtim+2 / modification time
        jmp	sysret
		; br sysret2

	; 06/02/2022
sysstty_err_s:
	mov	byte [u.r0], cl ; serial port's tty number
	jmp	short sysstty_err

sysstty: ; < set tty status and mode >
	; 22/02/2022
	; 21/02/2022
	; 06/02/2022 (Retro UNIX 286 v1.2)
	; 04/02/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 02/02/2022 (Retro UNIX 386 v1, Kernel v0.2.0.18)
	; 01/02/2022 (Retro UNIX 386 v1) -clear screen-
	; 24/12/2021 (Retro UNIX 386 v1.1)
	;	    ((32 bit reg push/pop))
	; 17/11/2015
	; 12/11/2015
	; 29/10/2015
	; 17/10/2015
	; 13/10/2015
	; 29/06/2015
	; 27/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 02/06/2013 - 12/07/2014 (Retro UNIX 8086 v1)
	;
	; 'sysstty' sets the status and mode of the typewriter 
	; whose file descriptor is in (u.r0).
	;
	; Calling sequence:
	;	sysstty; arg
	; Arguments:
	;	arg - address of 3 consequitive words that contain
	;	      the source of status data	
	; Inputs: ((*u.r0 - file descriptor & argument))
	; Outputs: ((status in address which is pointed to by arg))
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;	'sysstty' system call will set the tty
	;	(clear keyboard buffer and set cursor position)
	;	 in following manner:
	;   NOTE: All of tty setting functions are here (16/01/2014)
	;
	; Inputs:
	;	BX = 0 --> means
	;	   If CL = FFh (& DX <> 0FFFFh) ; 01/02/2022
	;	      set cursor position for console tty, only 
	;	      CH will be ignored (char. will not be written)
	;	   If CH = 0 (& DX <> 0FFFFh & CL < FFh) ; 01/02/20222
	;	      set console tty for (current) process
	;	      CL = tty number (0 to 9)
	;	      (If CH = 0, character will not be written)
	;          If CH > 0 (CL < FFh)	
	;             CL = tty number (0 to 9)
	;	      CH = character will be written
	;	        at requested cursor position (in DX)
	;	   DX = cursor position for tty number 0 to 7.
  	;		(only tty number 0 to 7) 
	;          DL = communication parameters (for serial ports)
	;	        (only for COM1 and COM2 serial ports)
	;	   DH < 0FFh -> DL is valid, initialize serial port
	;			or set cursor position	
	;	   DH = 0FFh -> DL is not valid
	;		do not set serial port parameters 
	;		or do not set cursor position
	;
	;	BX > 0 --> points to name of tty
	;    	   CH > 0 -->
	;		CH = character will be written in current 
	;            	cursor position (for tty number from 0 to 7)
	;	     	or character will be sent to serial port
	;	     	(for tty number 8 or 9)
	;		CL = color of the character if tty number < 8.
	;    	   CH = 0 --> Do not write a character, 
	;		set mode (tty 8 to 9) or 
	;		set current cursor positions (tty 0 to 7) only.
	;   	   DX = cursor position for tty number 0 to 7.
	;    	   DH = FFh --> Do not set cursor pos (or comm. params.)
	;		(DL is not valid)
	;	   DL = communication parameters 
	;		for tty number 8 or 9 (COM1 or COM2).
	;
	;	01/02/2022 - Retro UNIX 386 v1 - 2022 modification
	;	(30/01/2022 - Retro UNIX 8086 - 2022 modification)
	;	If CH = 0 & DX = 0FFFFh -> 
	;	   clear screen (video page) & set cursor pos to 0,0.
	;	   (for tty number 0 to 7, CL <= 7)
	;	   (if CL = 0FFh -> clear console tty)	
	;
	; Outputs:
	;	cf = 0 -> OK
	;	     AL = tty number (0 to 9)
	;	     AH = line status if tty number is 8 or 9
	;	     AH = process number (of the caller)
	;	cf = 1 means error (requested tty is not ready)
	;	     AH = FFh if the tty is locked 
	;		  (owned by another process)
	;	        = process number (of the caller) 
	;		  (if < FFh and tty number < 8)
	;	     AL = tty number (0FFh if it does not exist)
	;	     AH = line status if tty number is 8 or 9
	;	NOTE: Video page will be cleared if cf = 0.
	;	

	; 27/06/2015 (32 bit modifications)
	; 14/01/2014
	xor 	eax, eax
	dec	ax ; 17/10/2015
	mov	[u.r0], eax ; 0FFFFh
	;;;
	; 01/02/2022
	inc	cl  ; 0FFh -> 0, 7 -> 8
	cmp	edx, eax
	;cmp	dx, ax ; 0FFFFh
	jne	short sysstty_18
	; clear video page
	; (CH must be 0)
	or	ch, ch
	jnz	short sysstty_err ; invalid parameters
	cmp	cl, 8 ; > tty7 (serial port?)
	ja	short sysstty_err ; invalid parameters
	and	cl, cl
	jnz	short sysstty_18 ; actual tty (video page) num + 1
	movzx	esi, byte [u.uno]
	mov	cl, byte [esi+p.ttyc-1] ; current/console tty
	cmp	cl, 7
	ja	short sysstty_err_s ; serial port !	 
	; here CL contains (actual) tty number (tty0 to tty7) 
	inc	cl  ; 0 -> 1, 7 -> 8 
sysstty_18:
	dec	cl  ; 8 -> 7, 1 -> 0
	; cl = video page (tty) number
	;;;
	and	ebx, ebx
	;jnz	sysstty_6
	; 01/02/2022
	jz	short sysstty_19
	jmp	sysstty_6
sysstty_19:
	; set console tty
	; 29/10/2015
	; 17/01/2014 
	cmp	cl, 9
	jna	short sysstty_0
	; 17/11/2015
	cmp	cl, 0FFh
	jb	short sysstty_13
	mov	ch, cl ; force CH value to FFh 
sysstty_13:
	mov	bl, [u.uno] ; process number
	mov	cl, [ebx+p.ttyc-1] ; current/console tty
sysstty_0:
	; 29/06/2015
	push	edx ; 24/12/2021
	push	ecx
	xor 	dl, dl	; sysstty call sign
	mov	al, cl
	mov	[u.r0], al ; tty number (0 to 9)
	call	ottyp
	pop	ecx
	pop	edx
	;
	jc	short sysstty_pd_err
	;
	; 22/02/2022 (Bug!, BugFix)
	; (ebx = ?, modified in ottyp, it may be > 255)
	;
	cmp	cl, 8
	jb	short sysstty_2
	;
	cmp	dh, 0FFh
	je	short sysstty_2

; 01/02/2022
;	; 29/10/2015
;	mov	ah, dl ; communication parameters
;		; ah = 0E3h = 11100011b = 115200 baud,
;		;			 THRE int + RDA int 
;		; ah = 23h = 00100011b = 9600 baud,
;		;			 THRE int + RDA int 
;	sub	al, al ; 0
;	; 12/07/2014
;	cmp	cl, 9
;	jb	short sysstty_1
;	inc	al
;sysstty_1:
;	; 01/02/2022
;	push	ecx
;	; 29/06/2015	
;	call 	sp_setp ; Set serial port communication parameters
;	mov	[u.r0+1], cx ; Line status (ah)
;			     ; Modem status (EAX bits 16 to 23)
;	; 01/02/2022
;	pop	ecx	
;       jc      short sysstty_tmout_err ; 29/10/2015

	; 01/02/2022
	call	sysstty_scp
        jc      short sysstty_tmout_err ; 29/10/2015

sysstty_2:
	; 17/01/2014
	and	ch, ch 	; set cursor position 
			; or comm. parameters ONLY
	jnz	short sysstty_3
	; 01/02/2022
	cmp	dx, 0FFFFh
	jb	short sysstty_20
	; clear screen (video page)
	jmp	sysstty_14

sysstty_pd_err: ; 29/06/2015
	; 'permission denied !' error
	mov	dword [u.error], ERR_NOT_OWNER
	jmp	error

sysstty_20:
	movzx	ebx, byte [u.uno] ; process number
	mov	[ebx+p.ttyc-1], cl ; console tty
sysstty_3:
	; 16/01/2014
	mov	al, ch ; character  ; 0 to FFh
	; 17/11/2015
	mov 	ch, 7  ; Default color (light gray)
	cmp	cl, ch ; 7 (tty number)
        ;jna	sysstty_9
	; 24/12/2021
	ja	short sysstty_12
	jmp	sysstty_9

sysstty_12:
	;; BX = 0, CL = 8 or CL = 9
	; (Set specified serial port as console tty port)
	; CH = character to be written
	; 15/04/2014
	; CH = 0 --> initialization only
	; AL = character
	; 26/06/2014
	mov	[u.ttyn], cl
	; 12/07/2014
	mov	ah, cl ; tty number (8 or 9)
	; 02/02/2022
	inc	al  ; 0FFh -> 0, 0 -> 1
	jz	short sysstty_4 ; al = ch = 0
	dec	al  ; 1 -> 0	
	;and	al, al
	jz	short sysstty_4 ; al = ch = 0
 	; 04/07/2014
	call 	sndc
	; 12/07/2014
	jmp	short sysstty_5

sysstty_4:
	; 12/07/2014
	;xchg 	ah, al ; al = 0 -> al = ah, ah = 0
	mov	al, ah ; 29/06/2015
	sub	al, 8
	; 27/06/2015
	call	sp_status ; get serial port status
	; AL = Line status, AH = Modem status
	; 12/11/2015
	cmp	al, 80h
	cmc
sysstty_5:
	mov	[u.r0+1], ax ; ah = line status
		; EAX bits 16-23 = modem status	
	pushf
	xor	dl, dl ; sysstty call sign
	mov	al, [u.ttyn] ; 26/06/2014
	call	cttyp
	popf
	;jnc	sysret ; time out error 
	; 01/02/2022
	jc	short sysstty_tmout_err
	jmp	sysret

	; time out error 
sysstty_tmout_err:
	mov	dword [u.error], ERR_TIME_OUT
	jmp	error

sysstty_6:
	push	edx ; 24/12/2021
	push	ecx
	mov	[u.namep], ebx
	call	namei
	pop	ecx
	pop	edx
	jc	short sysstty_inv_dn
	;
	; 21/02/2022
	;cmp	ax, 19  ; inode number of /dev/COM2
	;ja	short sysstty_inv_dn ; 27/06/2015
	;;
	;cmp	al, 10 ; /dev/tty0 .. /dev/tty7
	;	       ; /dev/COM1, /dev/COM2
	;jb	short sysstty_7
	;sub	al, 10
	;jmp	short sysstty_8
	;
	; 21/02/2022
	; (Retro UNIX 386 v2 file system inode numbers)
	cmp	eax, 26	; /dev/tty9 (/dev/com2) inode number is 26
	ja	short sysstty_inv_dn
	;cmp	al, 8	; /dev/tty inode number is 8
	;jb	short sysstty_inv_dn
	sub	al, 8
	jb	short sysstty_inv_dn
	jz	short sysstty_7 ; /dev/tty inode number is 8
	; convert inode number to tty number (tty0 to tty9)
	sub	al, 9
	jb	short sysstty_inv_dn
	; al = 0 to 9
	sub	ebx, ebx ; 22/02/2022
	jmp	short sysstty_8 
sysstty_7:
	; 21/02/2022
	;cmp	al, 1 ; /dev/tty
	;jne	short sysstty_inv_dn ; 27/06/2015
	movzx	ebx, byte [u.uno] ; process number
	mov	al, [ebx+p.ttyc-1] ; console tty
sysstty_8:
	; 22/02/2022
	; (ebx < 256)	
	mov	[u.r0], al
	push	edx ; 24/12/2021
	push	eax
	push	ecx	
	call	ottyp
	pop	ecx
	pop	eax
	pop	edx
        ;jc	sysstty_pd_err ; 'permission denied !'
	; 06/02/2022
	jnc	short sysstty_21
	; 'permission denied !'
	jmp	sysstty_pd_err
sysstty_21:
	; 29/10/2015
	xchg 	ch, cl
		; cl = character, ch = color code
	xchg	al, cl
		; al = character, cl = tty number
	cmp	cl, 7
	;ja	sysstty_12
	; 06/02/2022
	jna	short sysstty_16
;;
	cmp	dh, 0FFh
	je	short sysstty_22 ; do not set comm. parameters

; 01/02/2022
;	; 29/10/2015
;	mov	ah, dl ; communication parameters
;		; ah = 0E3h = 11100011b = 115200 baud,
;		;			 THRE int + RDA int 
;		; ah = 23h = 00100011b = 9600 baud,
;		;			 THRE int + RDA int 
;	sub	al, al ; 0
;	; 12/07/2014
;	cmp	cl, 9
;	jb	short sysstty_1
;	inc	al
;sysstty_1:
;	; 01/02/2022
;	push	ecx
;	; 29/06/2015	
;	call 	sp_setp ; Set serial port communication parameters
;	mov	[u.r0+1], cx ; Line status (ah)
;			     ; Modem status (EAX bits 16 to 23)
;	; 01/02/2022
;	pop	ecx	
;       jc      short sysstty_tmout_err ; 29/10/2015

	; 02/02/2022
	mov	ch, al ; save char
	; 01/02/2022
	call	sysstty_scp
        jc      short sysstty_tmout_err ; 29/10/2015
	; 02/02/2022
	mov	al, ch ; restore char
sysstty_22:
	; 01/02/2022
	or	ch, ch
	jz	short sysstty_11 ; do not send char to terminal
	; send char to (serial port) terminal
	; al = character
	; cl = tty number (8 or 9)
	jmp	sysstty_12 ; (tty8 or tty9)

sysstty_inv_dn: 
	; 27/06/2015
	; Invalid device name (not a tty) ! error
	; (Device is not a tty or device name not found)
	mov	dword [u.error], ERR_INV_DEV_NAME
	jmp	error

sysstty_16:
	; 22/02/2022
	; 16/01/2014
	;xor	bh, bh
sysstty_9: 	; tty 0 to tty 7
	; al = character
	; ch = color/attribute ; 01/02/2022
	;
 	; 22/02/2022 (BugFix)
	; (ebx may be > 255 here!? due to 'ottyp')
	sub	ebx, ebx ; *
	mov	bl, cl ; (tty number = video page number)
	;
	cmp	dh, 0FFh ; Do not set cursor position
	je	short sysstty_10
	; 24/12/2021
	push	ecx
	push	eax
	; 22/02/2022	
	;;movzx	ebx, cl ; *
	;mov	bl, cl ; (tty number = video page number)
	call	set_cpos
	pop	eax
	pop	ecx
sysstty_10: 
	; 22/02/2022
	; bl = video page (tty) number 
	;
	; 29/10/2015
	or	al, al ; character
	jz      short sysstty_11 ; al = 0
	; 17/11/2015
	cmp	al, 0FFh
	jnb	short sysstty_11
		; ch > 0 and ch < FFh
	; write a character at current cursor position
	mov	ah, ch ; color/attribute
	; 12/07/2014
	push	ecx ; 24/12/2021
	call	write_c_current
	pop	ecx
sysstty_11:
	; 14/01/2014
	xor	dl, dl ; sysstty call sign
	; 18/01/2014
	;movzx	eax, cl ; 27/06/2015
	mov	al, cl
	call	cttyp
	jmp	sysret

	; 06/02/2022 (Retro UNIX 386 v1.2)
sysstty_scp:
	; 02/02/2022
	; set communication parameters (for COM1 or COM2)
	; 01/02/2022
	;
	; 29/10/2015
	mov	ah, dl ; communication parameters
		; ah = 0E3h = 11100011b = 115200 baud,
		;			 THRE int + RDA int 
		; ah = 23h = 00100011b = 9600 baud,
		;			 THRE int + RDA int 
	sub	al, al ; 0
	; 12/07/2014
	cmp	cl, 9
	jb	short sysstty_1
	inc	al
sysstty_1:
	; 02/02/2022
	push	edx
	; 01/02/2022
	push	ecx
	; 29/06/2015	
	call 	sp_setp ; Set serial port communication parameters
	mov	[u.r0+1], cx ; Line status (ah)
			     ; Modem status (EAX bits 16 to 23)
	; 01/02/2022
	pop	ecx	
	pop	edx ; 02/02/2022
	; 01/02/2022
	; if cf = 1 -> sysstty_tmout_err
	retn

	; 06/02/2022 (Retro UNIX 386 v1.2)
sysstty_14:
	; 23/02/2022
	; 02/02/2022
	; ch = 0
	; cl = video page
	;
	; dx = 0FFFFh
	; clear screen (video page)
	;

	; 02/02/2022
	; clear screen
	;
	; (modified registers: eax, ebx, ecx, edx, esi, edi)

	; 23/02/2022
	mov 	bl, cl ; CL = tty number (0 to 7)

	; clear video page
	call	wttyc ; 23/02/2022

	; 23/02/2022
	mov	al, bl
	mov	ah, [u.uno]
	mov	[u.r0], ax
	jmp	short sysstty_11

wttyc:
	; 23/02/2022
	; (clear video page)
	; INPUT:
	;  bl = video page (0 to 7)
	;
	; Modified registers: eax, ecx, edx, esi, edi

	;xor	dx, dx ; column 0, row 0
	;;inc	dx ; 0 ; 23/02/2022
	;
;	movzx	ebx, cl
;	mov 	bl, cl ; CL = tty number (0 to 7) ; 23/02/2022

;	shl 	bl, 1 
;	mov 	al, byte ptr [ebx+ttyl]
;		; AL = lock value (0 or process number)
;	or	al, al
;	jz	short @f
;	cmp	al, byte ptr [u.uno] ; process number
;	jne	short sysstty_15
;		; only the owner can clear its video page
;	xor	al, al ; 0
;@@:
;	;mov	bl, cl		
;	shr	bl, 1 

	xor	al, al	; 0
	mov 	ah, 07h	; attribute/color (default)

	; scroll_up input:
	;
	; al = line count (0 or 1) ((0 == clear video page))
	; 	((al = 1 for write_tty (putc) procedure))
	; ah = attribute to be used on blanked line
	; bl = video page number (0 to 7)

	call	scroll_up ; clear video page (al=0)

	; (modified registers: eax, ecx, edx, esi, edi)

	; bl = video page number (0 to 7)
	;xor	dx, dx ; column 0, row 0
	; 02/02/2022
	xor	edx, edx
	; 23/02/2022
	;call	set_cpos
	;retn
	jmp	set_cpos

	;mov	al, bl
	;mov	ah, [u.uno]
	;mov	[u.r0], ax
	;jmp	short sysstty_11

;sysstty_15:
;	; 30/01/2022
;	; permission (denied) error
;	;xor	dl, dl ; sysstty call sign
;	mov	al, cl
;	sub	ah, ah ; 0
;	call	cttyp
;	jmp	error

; Original UNIX v1 'sysstty' routine:
; gtty:
;sysstty: / set mode of typewriter; 3 consequtive word arguments
        ;jsr    r0,gtty / r1 will have offset to tty block, 
	; 		/ r2 has source
        ;mov    r2,-(sp)
        ;mov    r1,-(sp) / put r1 and r2 on the stack
;1: / flush the clist wait till typewriter is quiescent
        ;mov    (sp),r1 / restore r1 to tty block offset
        ;movb   tty+3(r1),0f / put cc offset into getc argument
        ;mov    $240,*$ps / set processor priority to 5
        ;jsr    r0,getc; 0:../ put character from clist in r1
        ;       br .+4 / list empty, skip branch
        ;br     1b / get another character until list is empty
        ;mov    0b,r1 / move cc offset to r1
        ;inc    r1 / bump it for output clist
        ;tstb   cc(r1) / is it 0
        ;beq    1f / yes, no characters to output
 	;mov    r1,0f / no, put offset in sleep arg
        ;jsr    r0,sleep; 0:.. / put tty output process to sleep
        ;br     1b / try to calm it down again
;1:
        ;mov    (sp)+,r1
        ;mov    (sp)+,r2 / restore registers
	;mov    (r2)+,r3 / put reader control status in r3
        ;beq    1f / if 0, 1f
        ;mov    r3,rcsr(r1) / move r.c. status to reader
        ;                   / control status register
;1:
        ;mov    (r2)+,r3 / move pointer control status to r3
        ;beq    1f / if 0 1f
        ;mov    r3,tcsr(r1) / move p.c. status to printer 
	;		    / control status reg
;1:
        ;mov    (r2)+,tty+4(r1) / move to flag byte of tty block
        ;jmp     sysret2 / return to user

sysgtty: ; < get tty status >
	; 22/02/2022
	; 21/02/2022 
	;	(Retro UNIX 386 v1.2, inode number modifications)
	; 24/12/2021 (Retro UNIX 386 v1.1)
	;	    ((32 bit reg push/pop))
	; 23/11/2015
	; 29/10/2015
	; 17/10/2015
	; 28/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 30/05/2013 - 12/07/2014 (Retro UNIX 8086 v1)
	;
	; 'sysgtty' gets the status of tty in question. 
	; It stores in the three words addressed by it's argument
	; the status of the typewriter whose file descriptor
	; in (u.r0).
	;
	; Calling sequence:
	;	sysgtty; arg
	; Arguments:
	;	arg - address of 3 words destination of the status
	; Inputs: ((*u.r0 - file descriptor))
	; Outputs: ((status in address which is pointed to by arg))
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;	'sysgtty' system call will return status of tty
	;	(keyboard, serial port and video page status)
	;	 in following manner:
	;
	; Inputs:
	;	BX = 0 --> means 
	;	     CH = 0 -->	'return status of the console tty' 
	;	                 for (current) process
	;	     CL = 0 --> return keyboard status (tty 0 to 9)
	;	     CL = 1 --> return video page status (tty 0 to 7)
	;	     CL = 1 --> return serial port status (tty 8 & 9)		
	;	     CH > 0 -->	tty number + 1
	;
	;	BX > 0 --> points to name of tty
	;	     CL = 0 --> return keyboard status
	;	     CL = 1 --> return video page status
	;	     CH = undefined		 
	;
	; Outputs:
	;	cf = 0 ->
	;
	;	     AL = tty number from 0 to 9
	;		  (0 to 7 is also the video page of the tty)	
	;	     AH = 0 if the tty is free/unused
	;	     AH = the process number of the caller 
 	;	     AH = FFh if the tty is locked by another process
	;
	;	  (if calling is for serial port status)
	;	     BX = serial port status if tty number is 8 or 9
	;		  (BH = modem status, BL = Line status)
	;	     CX = 0FFFFh (if data is ready)
	;	     CX = 0 (if data is not ready or undefined)		
	;
	;	  (if calling is for keyboard status)
	;	     BX = current character in tty/keyboard buffer
	;		  (BH = scan code, BL = ascii code)
	;		  (BX=0 if there is not a waiting character)
	;	     CX  is undefined
	;
	;	  (if calling is for video page status)	
	;	     BX = cursor position on the video page
	;		  if tty number < 8
	;		  (BH = row, BL = column)
	;	     CX = current character (in cursor position)
	;		  on the video page of the tty 
	;		  if tty number < 8
	;		  (CH = color, CL = character)
	;	
	;	cf = 1 means error (requested tty is not ready)
	;
	;	     AH = FFh if the caller is not owner of
	;		  specified tty or console tty
	;	     AL = tty number (0FFh if it does not exist)
	;	     BX, CX are undefined if cf = 1
	;
	;	  (If tty number is 8 or 9)
	;	     AL = tty number 
	;	     AH = the process number of the caller 
	;	     BX = serial port status
	;  		 (BH = modem status, BL = Line status)
	;	     CX = 0
	;
		
gtty:   ; get (requested) tty number
	; 22/02/2022
	; 21/02/2022 (Retro UNIX 386 v1.2, inode number modifications)
	; 17/10/2015
	; 28/06/2015 (Retro UNIX 386 v1 - 32 bit modifications)
	; 30/05/2013 - 12/07/2014
	; Retro UNIX 8086 v1 modification ! 
	;
	; ((Modified regs: eAX, eBX, eCX, eDX, eSI, eDI, eBP))
	;
	; 28/06/2015 (32 bit modifications)
	; 16/01/2014
	xor 	eax, eax
	dec	ax ; 17/10/2015
	mov 	[u.r0], eax ; 0FFFFh
	cmp	cl, 1
	jna	short sysgtty_0
sysgtty_invp:
	; 28/06/2015
        mov     dword [u.error], ERR_INV_PARAMETER ; 'invalid parameter !' 
	jmp	error
sysgtty_0:	
	and	ebx, ebx
	jz	short sysgtty_1
	;
	mov	[u.namep], ebx
	;push	cx ; 23/11/2015
	push	ecx ; 24/12/2021
	call	namei
	pop	ecx
	;pop	cx ; 23/11/2015
	jc 	short sysgtty_inv_dn ; 28/06/2015
	;
	; 21/02/2022
	;cmp	ax, 1
	;jna	short sysgtty_2
	;sub	ax, 10
	;cmp	ax, 9
	;;ja	short sysgtty_inv_dn
	;;mov	ch, al
	;;jmp	short sysgtty_4
	;; 23/11/2015
	;jna	short sysgtty_4
	;
	; 21/02/2022
	; (Retro UNIX 386 v2 file system inode numbers)
	cmp	eax, 26	; /dev/tty9 (/dev/com2) inode number is 26
	ja	short sysgtty_inv_dn
	;cmp	al, 8	; /dev/tty inode number is 8
	;jb	short sysgtty_inv_dn
	sub	al, 8
	jb	short sysgtty_inv_dn
	jz	short sysgtty_2 ; /dev/tty inode number is 8
	; convert inode number to tty number (tty0 to tty9)
	sub	al, 9
	jb	short sysgtty_inv_dn
	; al = 0 to 9
	sub	ebx, ebx ; 22/02/2022
	jmp	short sysgtty_4
	
sysgtty_inv_dn: 
	;; 28/06/2015
	;; Invalid device name (not a tty) ! error
	;; (Device is not a tty or device name not found)
	;mov	dword [u.error], ERR_INV_DEV_NAME
	;jmp	error
	; 21/02/2022
	jmp	sysstty_inv_dn

sysgtty_1:
	; 16/01/2014
	cmp	ch, 10
	ja	short sysgtty_invp ; 28/06/2015
	dec	ch ; 0 -> FFh (negative)
	jns	short sysgtty_3 ; not negative
sysgtty_2:
	; get tty number of console tty
	mov	ah, [u.uno]
 	; 28/06/2015
	movzx 	ebx, ah
	mov	ch, [ebx+p.ttyc-1]
sysgtty_3:
	mov	al, ch
sysgtty_4:
	mov	[u.r0], al
 	; 28/06/2015
	;cmp	al, 9
	;ja	short sysgtty_invp
	mov	ebp, [u.usp]
	; 23/11/2015
	and	cl, cl
	jz	short sysgtty_6 ; keyboard status
	cmp	al, 8 ; cmp ch, 8
	jb	short sysgtty_6 ; video page status
	; serial port status
	; 12/07/2014
	;mov	dx, 0
	;je	short sysgtty_5
	;inc	dl
;sysgtty_5:
	; 28/06/2015
	sub	al, 8
	call	sp_status ; serial (COM) port (line) status
	; AL = Line status, AH = Modem status
	mov	[ebp+16], ax ; serial port status (in EBX)
	mov	ah, [u.uno]
        mov     [u.r0+1], ah
	; 24/12/2021
	mov	word [ebp+24], 0 ; data status (0 = not ready)
				; (in ECX)
	test	al, 80h
	jnz	short sysgtty_dnr_err ; 29/06/2015
	test	al, 1
	;jz	sysret
	jz	short sysgtty_10
	dec	word [ebp+24] ; data status (FFFFh = ready)
sysgtty_10:
	jmp	sysret
sysgtty_6:
	mov	[u.ttyn], al ; tty number
	;movzx	ebx, al
	mov 	bl, al ; tty number (0 to 9)
	shl 	bl, 1  ; aligned to word
	; 22/04/2014 - 29/06/2015
        add     ebx, ttyl
 	mov	ah, [ebx]
	cmp	ah, [u.uno]
	je	short sysgtty_7
	and	ah, ah
	;jz	short sysgtty_7
	jnz	short sysgtty_8
	;mov	ah, 0FFh
sysgtty_7:
        mov     [u.r0+1], ah
sysgtty_8:
	or	cl, cl
	jnz	short sysgtty_9
	mov	al, 1  ; test a key is available
	call	getc
	mov	[ebp+16], ax ; bx, character
	jmp	sysret
sysgtty_9:
	mov	bl, [u.ttyn]
	; bl = video page number
	call 	get_cpos
	; dx = cursor position
	mov	[ebp+16], dx ; bx
	;mov	bl, [u.ttyn]
	; bl = video page number
	call	read_ac_current
	; ax = character and attribute/color
	mov	[ebp+24], ax ; cx
	jmp	sysret
sysgtty_dnr_err:
	; 'device not responding !' error	
	;mov 	dword [u.error], ERR_TIME_OUT ; 25
	mov 	dword [u.error], ERR_DEV_NOT_RESP ; 25
	jmp	error	

; Original UNIX v1 'sysgtty' routine:
; sysgtty:
        ;jsr    r0,gtty / r1 will have offset to tty block,
	;	       / r2 has destination
        ;mov    rcsr(r1),(r2)+ / put reader control status 
	;                     / in 1st word of dest
        ;mov    tcsr(r1),(r2)+ / put printer control status
	;                     / in 2nd word of dest
        ;mov    tty+4(r1),(r2)+ / put mode in 3rd word
        ;jmp    sysret2 / return to user
	
; Original UNIX v1 'gtty' routine:
; gtty:
        ;jsr    r0,arg; u.off / put first arg in u.off
        ;mov    *u.r0,r1 / put file descriptor in r1
        ;jsr    r0,getf / get the i-number of the file
        ;tst    r1 / is it open for reading
        ;bgt    1f / yes
        ;neg    r1 / no, i-number is negative, 
	;          / so make it positive
;1:
        ;sub    $14.,r1 / get i-number of tty0
        ;cmp    r1,$ntty-1 / is there such a typewriter
        ;bhis   error9 / no, error
        ;asl    r1 / 0%2
        ;asl    r1 / 0%4 / yes
        ;asl    r1 / 0%8 / multiply by 8 so r1 points to 
	;	       ; / tty block
        ;mov    u.off,r2 / put argument in r2
        ;rts    r0 / return