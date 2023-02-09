; Retro UNIX 386 v1 Kernel (v0.2.1.2) - SYS4.INC
; Last Modification: 26/02/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U4.ASM (04/07/2014) //// UNIX v1 -> u4.s
;
; ****************************************************************************

;setisp:
       ;mov     r1,-(sp)
       ;mov     r2,-(sp)
       ;mov     r3,-(sp)
       ;mov     clockp,-(sp)
       ;mov     $s.syst+2,clockp
       ;jmp     (r0)

clock: ; / interrupt from 60 cycle clock
	
	; 14/10/2015
	; 14/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/12/2013 - 10/04/2014 (Retro UNIX 8086 v1)

       ;mov     r0,-(sp) / save r0
       ;tst     *$lks / restart clock?
       ;mov     $s.time+2,r0 / increment the time of day
       ;inc     (r0)
       ;bne     1f
       ;inc     -(r0)
;1:
       ;mov     clockp,r0 / increment appropriate time category
       ;inc     (r0)
       ;bne     1f
       ;inc     -(r0)
;1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cmp	byte [u.quant], 0
	ja	short clk_1
	;
        cmp     byte [sysflg], 0FFh ; user or system space ?
	jne	short clk_2 ; system space (sysflg <> 0FFh)
	cmp     byte [u.uno], 1 ; /etc/init ?
	jna	short clk_1 ; yes, do not swap out
	cmp	word [u.intr], 0
	jna	short clk_2
clk_0:
	; 14/10/2015
	inc	byte [sysflg] 	; Now, we are in system space
	pop	eax ; return address to the timer interrupt
	;
	MOV	AL,EOI			; GET END OF INTERRUPT MASK
	;CLI				; DISABLE INTERRUPTS TILL STACK CLEARED
	OUT	INTA00,AL		; END OF INTERRUPT TO 8259 - 1	
	;
	jmp     sysrelease ; 'sys release' by clock/timer
clk_1:
	dec	byte [u.quant]
clk_2:
	retn   ; return to (hardware) timer interrupt routine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

       ;mov     $uquant,r0 / decrement user time quantum
       ;decb    (r0)
       ;bge     1f / if less than 0
       ;clrb    (r0) / make it 0
;1: / decrement time out counts return now if priority was not 0
       ;cmp     4(sp),$200 / ps greater than or equal to 200
       ;bge     2f / yes, check time outs
       ;tstb    (r0) / no, user timed out?
       ;bne     1f / no
       ;cmpb    sysflg,$-1 / yes, are we outside the system?
       ;bne     1f / no, 1f
       ;mov     (sp)+,r0 / yes, put users r0 in r0
       ;sys     0 / sysrele
       ;rti
;2: / priority is high so just decrement time out counts
       ;mov     $toutt,r0 / r0 points to beginning of time out table
;2:
       ;tstb    (r0) / is the time out?
       ;beq     3f / yes, 3f (get next entry)
       ;decb    (r0) / no, decrement the time
       ;bne     3f / isit zero now?
       ;incb    (r0) / yes, increment the time
;3:
       ;inc     r0 / next entry
       ;cmp     r0,$touts / end of toutt table?
       ;blo     2b / no, check this entry
       ;mov     (sp)+,r0 / yes, restore r0
       ;rti / return from interrupt
;1: / decrement time out counts; if 0 call subroutine
       ;mov     (sp)+,r0 / restore r0
       ;mov     $240,*$ps / set processor priority to 5
       ;jsr     r0,setisp / save registers
       ;mov     $touts-toutt-1,r0 / set up r0 as index to decrement thru
                               ;  / the table
;1:
       ;tstb    toutt(r0) / is the time out for this entry
       ;beq     2f / yes
       ;decb    toutt(r0) / no, decrement the time
       ;bne     2f / is the time 0, now
       ;asl     r0 / yes, 2 x r0 to get word index for tout entry
       ;jsr     r0,*touts(r0) / go to appropriate routine specified in this
       ;asr     r0 / touts entry; set r0 back to toutt index
;2:
       ;dec     r0 / set up r0 for next entry
       ;bge     1b / finished? , no, go back
       ;br      retisp / yes, restore registers and do a rti

;retisp:
       ;mov     (sp)+,clockp / pop values before interrupt off the stack
       ;mov     (sp)+,r3
       ;mov     (sp)+,r2
       ;mov     (sp)+,r1
       ;mov     (sp)+,r0
       ;rti     / return from interrupt


wakeup: ; / wakeup processes waiting for an event 
	; / by linking them to the queue
	;
	; 26/02/2022
	; 15/09/2015
	; 29/06/2015
	; 15/04/2015 (Retro UNIX 386 v1 - Beginning)
	;
	; 15/05/2013 - 02/06/2014
	; Retro UNIX 8086 v1 modification !
	; (Process/task switching routine by using
	; Retro UNIX 8086 v1 keyboard interrupt output.)
	;
	; In original UNIX v1, 'wakeup' is called to wake the process
	; sleeping in the specified wait channel by creating a link 
	; to it from the last user process on the run queue.
	; If there is no process to wake up, nothing happens.
	;
	; In Retro UNIX 8086 v1, Int 09h keyboard interrupt will set
	; 'switching' status of the current process (owns current tty)
	; (via alt + function keys) to a process which has highest
	; priority (on run queue) on the requested tty (0 to 7, except
	; 8 and 9 which are tty identifiers of COM1, COM2 serial ports)
	; as it's console tty. (NOTE: 'p.ttyc' is used to set console
	; tty for tty switching by keyboard.)	 
	; 
	; INPUT -> 
	;	   AL = wait channel (r3) ('tty number' for now)
	;	   ;;EBX = Run queue (r2) offset
	;
	; ((modified registers: EAX, EBX))
	;
	movzx	ebx, al ; 29/06/2015
	add	ebx, wlist
	mov	al, [ebx] ; waiting list (waiting process number)
	and	al, al
	jz	short wa0 ; nothing to wakeup
	;
	xor	ah, ah
	mov 	[u.quant], ah ; 0 ; time quantum = 0	
	mov	[ebx], ah ; 0 ; zero wait channel entry
	; 15/09/2015
	movzx	ebx, al
	; 26/02/2022 (p.waitc is not used)
	;mov	[ebx+p.waitc-1], ah ; 0
	inc	ah
	mov	byte [ebx+p.stat-1], ah ; 1 ; SRUN
	;
	push	edi
	push	edx
	call	putlu
	pop	edx
	pop	edi
wa0:
	retn

sleep: 
	; 26/02/2022
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 15/09/2015
	; 30/06/2015 (Retro UNIX 386 v1 - Beginning)
	;
	; 09/05/2013 - 20/03/2014
	;
	; Retro UNIX 8086 v1 modification !
	; (Process/task switching and quit routine by using
	; Retro UNIX 8086 v1 keyboard interrupt output.))
	;
	; In original UNIX v1, 'sleep' is called to wait for
	; tty and tape output or input becomes available
	; and process is put on waiting channel and swapped out,
	; then -when the tty or tape is ready to write or read-
	; 'wakeup' gets process back to active swapped-in status.)
	;
	; In Retro UNIX 8086 v1, Int 1Bh ctrl+brk interrupt and
	; Int 09h keyboard interrupt will set 'quit' or 'switching'		
	; status of the current process also INT 1Ch will count down
	; 'uquant' value and INT 09h will redirect scancode of keystroke
	; to tty buffer of the current process and kernel will get
	; user input by using tty buffer of the current process
	; (instead of standard INT 16h interrupt).
	; TTY output will be redirected to related video page of text mode
	; (INT 10h will be called with different video page depending
	; on tty assignment of the active process: 0 to 7 for
	; pseudo screens.)
	;
	; In Retro UNIX 8086 v1, 'sleep' will be called to wait for
	; a keystroke from keyboard or wait for reading or writing
	; characters/data on serial port(s).
	;
	; Character/Terminal input/output through COM1 and COM2 will be
	; performed by related routines in addition to pseudo TTY routines.
	; 
	; R1 = AH = wait channel (0-9 for TTYs) ; 05/10/2013 (22/09/2013)
	;
	;; 05/10/2013
        ;10/12/2013
	;cmp   byte [u.uno], 1
        ;ja    short sleep0
	;retn

	; 20/03/2014
	;mov	bx, [runq]
	;cmp	bl, bh
	;jne	short sleep0	
	; 25/02/2014
	;cmp word ptr [runq], 0
	;ja short sleep0	
	;retn
sleep0:
	;
	call	isintr
	;jnz	sysret
		; / wait for event
       		; jsr r0,isintr / check to see if interrupt 
			      ; / or quit from user
               		; br 2f / something happened
			      ; / yes, his interrupt so return
                     	      ;	/ to user
	; 24/12/2021
	jz	short sleep_2
sleep_3:
	jmp	sysret
sleep_2:
	; 30/06/2015
    	movzx	ebx, ah ; 30/06/2015
	add	ebx, wlist
	mov	al, [ebx]
	and	al, al
	jz	short sleep1
	push	ebx
	call	putlu
	pop	ebx
sleep1:
	mov	al, [u.uno]    
  	mov	[ebx], al 	; put the process number
				; in the wait channel
		; mov (r0)+,r1 / put number of wait channel in r1
		; movb wlist(r1),-(sp) / put old process number in there,
				     ; / on the stack
       		; movb u.uno,wlist(r1) / put process number of process
				     ; / to put to sleep in there
        ; 15/09/2015
	movzx	ebx, al
        mov     byte [ebx+p.stat-1], 4 ; SSLEEP
	; 26/02/2022 (p.waitc is not used)
	;inc	ah
	;mov	[ebx+p.waitc-1], ah ; wait channel + 1
	;
	push    word [cdev]
		; mov cdev,-(sp) / nothing happened in isintr so
	call	swap
       		; jsr r0,swap / swap out process that needs to sleep
        pop     word [cdev]
		; mov (sp)+,cdev / restore device
	call	isintr
	; 22/09/2013
	;jnz	sysret         
		; jsr r0,isintr / check for interrupt of new process
               		; br 2f / yes, return to new user
		; movb (sp)+,r1 / no, r1 = old process number that was 
				; / originally on the wait channel
       		; beq 1f / if 0 branch
  		; mov $runq+4,r2 / r2 points to lowest priority queue
       		; mov $300,*$ps / processor priority = 6
		; jsr r0,putlu / create link to old process number
       		; clr *$ps / clear the status; process priority = 0
	; 24/12/2021
	jnz	short sleep_3 ; jump to sysret
     ;1:
	retn
		; rts r0 / return
     ;2:
        ;;jmp	sysret
		; jmp sysret / return to user

isintr:
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 30/06/2015 (Retro UNIX 386 v1 - Beginning)
	;
	; 09/05/2013 - 30/05/2014
	;
	; Retro UNIX 8086 v1 modification !
	; (Process/task switching and quit routine by using
	; Retro UNIX 8086 v1 keyboard interrupt output.))
	;
	; Retro UNIX 8086 v1 modification:
	; 'isintr' checks if user interrupt request is enabled
	;  and there is a 'quit' request by user;
	;  otherwise, 'isintr' will return with zf=1 that means
	;  "nothing to do". (20/10/2013)
	;
	; 20/10/2013
	cmp 	word [u.ttyp], 0 ; has process got a tty ?
	jna	short isintr2 ; retn
	; 03/09/2013
	; (nothing to do)
	;retn
	; 22/09/2013
	cmp	word [u.intr], 0
	jna	short isintr2 ; retn
	; 30/05/2014
	;push	ax
	; 24/12/2021
	push	eax
	mov	ax, [u.quit]
	or	ax, ax ; 0 ?
	jz	short isintr1 ; zf = 1
	cmp	ax, 0FFFEh  ; 'ctrl + brk' check
	ja	short isintr1 ; 0FFFFh, zf = 0
	;xor	ax, ax ; zf = 1
	; 24/12/2021
	xor	eax, eax ; zf = 1
isintr1:
	;pop	ax
	; 24/12/2021
	pop	eax
isintr2: ; 22/09/2013
	; zf=1 -> nothing to do
	retn

	; UNIX v1 original 'isintr' routine... 
       	;mov     r1,-(sp) / put number of wait channel on the stack
       	;mov     r2,-(sp) / save r2
       	;mov     u.ttyp,r1 / r1 = pointer to buffer of process control
        ;                 / typewriter
       	;beq     1f / if 0, do nothing except skip return
       	;movb    6(r1),r1 / put interrupt char in the tty buffer in r1
       	;beq     1f / if its 0 do nothing except skip return
       	;cmp     r1,$177 / is interrupt char = delete?
       	;bne     3f / no, so it must be a quit (fs)
       	;tst     u.intr / yes, value of u.intr determines handling
        ;              / of interrupts
       	;bne     2f / if not 0, 2f. If zero do nothing.
     ;1:
       	;tst     (r0)+ / bump r0 past system return (skip)
     ;4:
       	;mov     (sp)+,r2 / restore r1 and r2
       	;mov     (sp)+,r1
       	;rts     r0
     ;3: / interrupt char = quit (fs)
       	;tst     u.quit / value of u.quit determines handling of quits
       	;beq     1b / u.quit = 0 means do nothing
     ;2: / get here because either u.intr <> 0 or u.qult <> O
       	;mov     $tty+6,r1 / move pointer to tty block into r1
     ;1: / find process control tty entry in tty block
       	;cmp     (r1),u.ttyp / is this the process control tty buffer?
       	;beq     1f / block found go to 1f
       	;add     $8,r1 / look at next tty block
       	;cmp     r1,$tty+[ntty*8]+6 / are we at end of tty blocks
       	;blo     1b / no
       	;br      4b / no process control tty found so go to 4b
     ;1:
       	;mov     $240,*$ps / set processor priority to 5
       	;movb    -3(r1),0f / load getc call argument; character llst
        ;                  / identifier
       	;inc     0f / increment
     ;1:
       	;jsr     r0,getc; 0:.. / erase output char list for control
        ;        br 4b / process tty. This prevents a line of stuff
        ;             / being typed out after you hit the interrupt
        ;             / key
       	;br      1b