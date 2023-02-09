; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2) - SYS3.INC
; Last Modification: 24/12/2021
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U3.ASM (08/03/2014) //// UNIX v1 -> u3.s
;
; ****************************************************************************

tswitch: ; Retro UNIX 386 v1
tswap:
	; 01/09/2015
	; 10/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/04/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	; time out swap, called when a user times out.
	; the user is put on the low priority queue.
	; This is done by making a link from the last user
	; on the low priority queue to him via a call to 'putlu'.
	; then he is swapped out.
	;
	; Retro UNIX 386 v1 modification ->
	;       swap (software task switch) is performed by changing
	;	user's page directory (u.pgdir) instead of segment change
	;	as in Retro UNIX 8086 v1.
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory 
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    u.uno - users process number
	;    runq+4 - lowest priority queue
	; OUTPUTS ->
	;    r0 - users process number
	;    r2 - lowest priority queue address
	;
	; ((AX = R0, BX = R2)) output
	; ((Modified registers: EDX, EBX, ECX, ESI, EDI))  	
	;
	mov 	al, [u.uno]
	       	; movb u.uno,r1 / move users process number to r1
		; mov  $runq+4,r2 
			; / move lowest priority queue address to r2
        call 	putlu
		; jsr r0,putlu / create link from last user on Q to 
		             ; / u.uno's user

switch: ; Retro UNIX 386 v1
swap:
	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 02/09/2015
	; 01/09/2015
	; 31/08/2015
	; 10/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/04/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	; 'swap' is routine that controls the swapping of processes
	; in and out of core.
	;
	; Retro UNIX 386 v1 modification ->
	;       swap (software task switch) is performed by changing
	;	user's page directory (u.pgdir) instead of segment change
	;	as in Retro UNIX 8086 v1.
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory 
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    runq table - contains processes to run.
	;    p.link - contains next process in line to be run.
	;    u.uno - process number of process in core	
	;    s.stack - swap stack used as an internal stack for swapping.	
	; OUTPUTS ->
	;    (original unix v1 -> present process to its disk block)
	;    (original unix v1 -> new process into core -> 
	;	   Retro Unix 8086 v1 -> segment registers changed 
	;	   for new process)
	;    u.quant = 3 (Time quantum for a process)
	; 	((INT 1Ch count down speed -> 18.2 times per second)	 	
	;    RETRO UNIX 8086 v1 will use INT 1Ch (18.2 times per second)
	;	 for now, it will swap the process if there is not
	;	 a keyboard event (keystroke) (Int 15h, function 4Fh)
	;	 or will count down from 3 to 0 even if there is a
	;        keyboard event locking due to repetitive key strokes.
	;	 u.quant will be reset to 3 for RETRO UNIX 8086 v1.
	;
	;    u.pri -points to highest priority run Q.
	;    r2 - points to the run queue.
	;    r1 - contains new process number
	;    r0 - points to place in routine or process that called
	;	  swap all user parameters
	;				
	; ((Modified registers: EAX, EDX, EBX, ECX, ESI, EDI))  	
	;
swap_0:
		;mov $300,*$ps / processor priority = 6
	mov	esi, runq
		; mov $runq,r2 / r2 points to runq table
swap_1: ; 1: / search runq table for highest priority process
	mov	ax, [esi]
	and 	ax, ax
       		; tst (r2)+ / are there any processes to run 
			  ; / in this Q entry
	jnz	short swap_2
       		; bne 1f / yes, process 1f
		; cmp r2,$runq+6 / if zero compare address 
			       ; / to end of table
		; bne 1b / if not at end, go back
	call	idle
		; jsr r0,idle; s.idlet+2 / wait for interrupt; 
				       ; / all queues are empty
	jmp	short swap_1
		; br swap
swap_2: ; 1:
	movzx	ebx, al ; 02/09/2015
		; tst -(r2) / restore pointer to right Q entry
 		; mov r2,u.pri / set present user to this run queue
	        ; movb (r2)+,r1 / move 1st process in queue to r1
	cmp	al, ah
		; cmpb r1,(r2)+ / is there only 1 process 
			      ; / in this Q to be run
	je	short swap_3
       		; beq 1f / yes
		; tst -(r2) / no, pt r2 back to this Q entry
	;movzx	ebx, al
	mov	ah, [ebx+p.link-1] 
       	mov	[esi], ah
		; movb p.link-1(r1),(r2) / move next process 
				       ; / in line into run queue
	jmp	short swap_4
       		; br 2f
swap_3: ; 1:
	;xor	dx, dx
	; 24/12/2021
	xor	edx, edx
	mov	[esi], dx
		; clr -(r2) / zero the entry; no processes on the Q
swap_4: ; / write out core to appropriate disk area and read 
      ; / in new process if required
       		; clr *$ps / clear processor status
	mov 	ah, [u.uno]
	cmp	ah, al
		; cmpb r1,u.uno / is this process the same as 
			      ; / the process in core?
       	je	short swap_8
       		; beq 2f / yes, don't have to swap
       		; mov r0,-(sp) / no, write out core; save r0 
			   ; / (address in routine that called swap)
		; mov r1,-(sp) / put r1 (new process #) on the stack
	; 01/09/2015
	;mov	[u.usp], esp
       		; mov sp,u.usp / save stack pointer
		; mov $sstack,sp / move swap stack pointer 
			       ; / to the stack pointer
	or	ah, ah
       		; tstb u.uno / is the process # = 0
       	jz	short swap_6 ; 'sysexit'
		; beq  1f / yes, kill process by overwriting
	; 02/09/2015
	mov	[u.usp], esp ; return  address for 'syswait' & 'sleep'
	;
	call	wswap
		;jsr r0,wswap / write out core to disk
	 ; 31/08/2015
	;movzx	ebx, al ; New (running) process number
	jmp 	short swap_7
swap_6:
	; 31/08/2015
	; Deallocate memory pages belong to the process
	; which is being terminated
	; 14/05/2015 ('sysexit')
 	; Deallocate memory pages of the process
	; (Retro UNIX 386 v1 modification !)
	;
	; movzx ebx, al
	push	ebx
	mov 	eax, [u.pgdir]  ; page directory of the process
	mov	ebx, [u.ppgdir] ; page directory of the parent process
	call	deallocate_page_dir
	mov	eax, [u.upage] ; 'user' structure page of the process
	call	deallocate_page
	pop	ebx
swap_7: ;1: 
	; 02/09/2015
	; 31/08/2015
	; 14/05/2015
	shl	bl, 2 ; * 4 
	mov	eax, [ebx+p.upage-4] ; the 'u' page of the new process
	;cli
	call	rswap
 		; mov (sp)+,r1 / restore r1 to new process number
		; jsr r0,rswap / read new process into core
       		; jsr r0,unpack / unpack the users stack from next
			      ; / to his program to its normal
	; 01/09/2015
	;mov	esp, [u.usp]	
		; mov u.usp,sp / location; restore stack pointer to
			     ; / new process stack
		; mov (sp)+,r0 / put address of where the process 
			     ; / that just got swapped in, left off.,
			     ; / i.e., transfer control to new process
	;sti
swap_8: ;2:
	; RETRO UNIX 8086 v1 modification !
	mov	byte [u.quant], time_count 
		; movb $30.,uquant / initialize process time quantum
	retn
		; rts r0 / return

wswap:  ; < swap out, swap to disk >
	; 09/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/05/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	; 'wswap' writes out the process that is in core onto its 
	; appropriate disk area.
	;
	; Retro UNIX 386 v1 modification ->
	;       User (u) structure content and the user's register content
	;	will be copied to the process's/user's UPAGE (a page for
	;	saving 'u' structure and user registers for task switching).
	;	u.usp - points to kernel stack address which contains
	;		user's registers while entering system call.  
	;	u.sp  - points to kernel stack address 
	;		to return from system call -for IRET-.
	;	[u.usp]+32+16 = [u.sp] 
	;	[u.usp] -> edi, esi, ebp, esp (= [u.usp]+32), ebx, 
	;		edx, ecx, eax, gs, fs, es, ds, -> [u.sp].
	;
	; Retro UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory 
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    u.break - points to end of program
	;    u.usp - stack pointer at the moment of swap
	;    core - beginning of process program		
	;    ecore - end of core 	
	;    user - start of user parameter area		
	;    u.uno - user process number	
	;    p.dska - holds block number of process	
	; OUTPUTS ->
	;    swp I/O queue
	;    p.break - negative word count of process 
	;    r1 - process disk address	
	;    r2 - negative word count
	;
	; RETRO UNIX 8086 v1 input/output:
	;
	; INPUTS ->
	;    u.uno - process number (to be swapped out)
	; OUTPUTS ->
	;    none
	;
	;   ((Modified registers: ECX, ESI, EDI))  
	;
	mov	edi, [u.upage] ; process's user (u) structure page addr
	mov	ecx, (U_SIZE + 3) / 4
	mov	esi, user ; active user (u) structure	
	rep	movsd
	;
	mov	esi, [u.usp] ; esp (system stack pointer, 
			     ;      points to user registers)
	mov	ecx, [u.sp]  ; return address from the system call
			     ; (for IRET)
			     ; [u.sp] -> EIP (user)
			     ; [u.sp+4]-> CS (user)
			     ; [u.sp+8] -> EFLAGS (user)
			     ; [u.sp+12] -> ESP (user)
			     ; [u.sp+16] -> SS (user)	
	sub	ecx, esi     ; required space for user registers
	add	ecx, 20	     ; +5 dwords to return from system call
			     ; (for IRET) 	
	shr	ecx, 2	     		
	rep	movsd
	retn

	; Original UNIX v1 'wswap' routine:
	; wswap:
		; mov *$30,u.emt / determines handling of emts
        	; mov *$10,u.ilgins / determines handling of 
				; / illegal instructions
		; mov u.break,r2 / put process program break address in r2
		; inc r2 / add 1 to it 
		; bic $1,r2 / make it even
		; mov r2,u.break / set break to an even location
		; mov u.usp,r3 / put users stack pointer 
			     ; / at moment of swap in r3
		; cmp r2,$core / is u.break less than $core
		; blos 2f / yes
		; cmp r2,r3 / no, is (u.break) greater than stack ptr.
       		; bhis 2f / yes
	; 1:
       		; mov (r3)+,(r2)+ / no, pack stack next to users program
		; cmp r3,$ecore / has stack reached end of core
		; bne 1b / no, keep packing
	 	; br 1f / yes
	; 2:
       		; mov $ecore,r2 / put end of core in r2 
	; 1:
       		; sub  $user,r2 / get number of bytes to write out 
			   ; / (user up to end of stack gets written out)
		; neg r2 / make it negative
		; asr r2 / change bytes to words (divide by 2)
		; mov r2,swp+4 / word count
		; movb u.uno,r1 / move user process number to r1
		; asl r1 / x2 for index
      		; mov r2,p.break-2(r1) / put negative of word count 
				     ; / into the p.break table
       		; mov p.dska-2(r1),r1 / move disk address of swap area 
				    ; /	for process to r1
       		; mov r1,swp+2 / put processes dska address in swp+2 
			     ; / (block number)
		; bis $1000,swp / set it up to write (set bit 9)
       		; jsr r0,ppoke / write process out on swap area of disk
	; 1:
       		; tstb swp+1 / is lt done writing?
       		; bne 1b / no, wait
		; rts r0 / yes, return to swap

rswap:  ; < swap in, swap from disk >
	; 15/09/2015
	; 28/08/2015
	; 14/05/2015
	; 09/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/05/2013 - 08/03/2014 (Retro UNIX 8086 v1)
	; 'rswap' reads a process whose number is in r1, 
	; from disk into core.
	;
	; Retro UNIX 386 v1 modification ->
	;       User (u) structure content and the user's register content
	;	will be restored from process's/user's UPAGE (a page for
	;	saving 'u' structure and user registers for task switching).
	;	u.usp - points to kernel stack address which contains
	;		user's registers while entering system call.  
	;	u.sp  - points to kernel stack address 
	;		to return from system call -for IRET-.
	;	[u.usp]+32+16 = [u.sp] 
	;	[u.usp] -> edi, esi, ebp, esp (= [u.usp]+32), ebx, 
	;		edx, ecx, eax, gs, fs, es, ds, -> [u.sp].
	;
	; RETRO UNIX 8086 v1 modification ->
	;       'swap to disk' is replaced with 'change running segment'
	;	according to 8086 cpu (x86 real mode) architecture.
	;	pdp-11 was using 64KB uniform memory while IBM PC
	;	compatibles was using 1MB segmented memory 
	;	in 8086/8088 times.
	;
	; INPUTS ->
	;    r1 - process number of process to be read in
	;    p.break - negative of word count of process 
	;    p.dska - disk address of the process		
	;    u.emt - determines handling of emt's 	
	;    u.ilgins - determines handling of illegal instructions		
	; OUTPUTS ->
	;    8 = (u.ilgins)
	;    24 = (u.emt)
	;    swp - bit 10 is set to indicate read 
	;		(bit 15=0 when reading is done)	
	;    swp+2 - disk block address
	;    swp+4 - negative word count 	
	;      ((swp+6 - address of user structure)) 
	;
	; RETRO UNIX 8086 v1 input/output:
	;
	; INPUTS ->
	;    AL	- new process number (to be swapped in)	 
	; OUTPUTS ->
	;    none
	;
	;   ((Modified registers: EAX, ECX, ESI, EDI, ESP)) 
	;
	; Retro UNIX 386 v1 - modification ! 14/05/2015
	mov	esi, eax  ; process's user (u) structure page addr
	mov	ecx, (U_SIZE + 3) / 4
	mov	edi, user ; active user (u) structure	
	rep	movsd
	pop	eax ; 15/09/2015, 'rswap' return address 
	mov	edi, [u.usp] ; esp (system stack pointer, 
			     ;      points to user registers)
	mov	ecx, [u.sp]  ; return address from the system call
			     ; (for IRET)
			     ; [u.sp] -> EIP (user)
			     ; [u.sp+4]-> CS (user)
			     ; [u.sp+8] -> EFLAGS (user)
			     ; [u.sp+12] -> ESP (user)
			     ; [u.sp+16] -> SS (user)		
	; 28/08/2015
	sub	ecx, edi     ; required space for user registers
	add	ecx, 20	     ; +5 dwords to return from system call
			     ; (for IRET) 	
	shr	ecx, 2	       		
	rep	movsd
	mov	esp, [u.usp] ; 15/09/2015
	push	eax ; 15/09/2015 'rswap' return address
	retn

	; Original UNIX v1 'rswap'  and 'unpack' routines:
	;rswap:
       		; asl r1 / process number x2 for index
       		; mov p.break-2(r1), swp+4 / word count
       		; mov p.dska-2(r1),swp+2 / disk address
       		; bis $2000,swp / read
       		; jsr r0,ppoke / read it in 
	; 1:
       		; tstb swp+1 / done
       		; bne 1b / no, wait for bit 15 to clear (inhibit bit)
       		; mov u.emt,*$30 / yes move these
       		; mov u.ilgins,*$10 / back
       		; rts r0 / return

	;unpack: ; / move stack back to its normal place
		; mov u.break,r2 / r2 points to end of user program
       		; cmp r2,$core / at beginning of user program yet?
		; blos 2f / yes, return
		; cmp r2,u.usp / is break_above the stack pointer 
			     ; / before swapping
		; bhis 2f / yes, return
		; mov $ecore,r3 / r3 points to end of core
		; add r3,r2
		; sub u.usp,r2 / end of users stack is in r2
	; 1:
		; mov -(r2),-(r3) / move stack back to its normal place
		; cmp r2,u.break / in core
		; bne 1b
	; 2:
       		; rts r0

putlu: 
	; 12/09/2015
	; 02/09/2015
	; 10/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/04/2013 - 23/02/2014 (Retro UNIX 8086 v1)
	; 'putlu' is called with a process number in r1 and a pointer
	; to lowest priority Q (runq+4) in r2. A link is created from
	; the last process on the queue to process in r1 by putting
	; the process number in r1 into the last process's link.
	;
	; INPUTS ->
	;    r1 - user process number
	;    r2 - points to lowest priority queue 
	;    p.dska - disk address of the process		
	;    u.emt - determines handling of emt's 	
	;    u.ilgins - determines handling of illegal instructions		
	; OUTPUTS ->
	;    r3 - process number of last process on the queue upon
	;	  entering putlu
	;    p.link-1 + r3 - process number in r1
	;    r2 - points to lowest priority queue
	;
	; ((Modified registers: EDX, EBX)) 
	;
	; / r1 = user process no.; r2 points to lowest priority queue

	; eBX = r2
	; eAX = r1 (AL=r1b)

	mov	ebx, runq
	movzx  	edx, byte [ebx]
	inc	ebx
	and	dl, dl
		; tstb (r2)+ / is queue empty?
       	jz	short putlu_1
		; beq 1f / yes, branch
	mov 	dl, [ebx] ; 12/09/2015
		; movb (r2),r3 / no, save the "last user" process number
			     ; / in r3
       	mov	[edx+p.link-1], al
		; movb r1,p.link-1(r3) / put pointer to user on 
			     ; / "last users" link
	jmp	short putlu_2
		; br 2f /
putlu_1: ; 1:
	mov	[ebx-1], al
       		; movb r1,-1(r2) / user is only user; 
			    ; / put process no. at beginning and at end
putlu_2: ; 2: 
	mov	[ebx], al
       		; movb r1,(r2) / user process in r1 is now the last entry
			     ; / on the queue
	mov	dl, al
        mov     [edx+p.link-1], dh ; 0
		; dec r2 / restore r2
        retn
		; rts r0

;copyz:
;       mov     r1,-(sp) / put r1 on stack
;       mov     r2,-(sp) / put r2 on stack
;       mov     (r0)+,r1
;       mov     (r0)+,r2
;1:
;       clr     (r1)+ / clear all locations between r1 and r2
;       cmp     r1,r2 
;       blo     1b
;       mov     (sp)+,r2 / restore r2
;       mov     (sp)+,r1 / restore r1
;       rts     r0 

idle:
	; 01/09/2015
	; 10/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 10/04/2013 - 23/10/2013 (Retro UNIX 8086 v1)
	; (idle & wait loop)
	; Retro Unix 8086 v1 modification on original UNIX v1
	; idle procedure!
      	;
  	; 01/09/2015
	sti
      	; 29/07/2013
      	hlt
      	nop ; 10/10/2013
      	nop
      	nop
      	; 23/10/2013
      	nop
      	nop
      	nop
      	nop
      	retn      

	;mov *$ps,-(sp) / save ps on stack
	;clr *$ps / clear ps
	;mov clockp,-(sp) / save clockp on stack
	;mov (r0)+,clockp / arg to idle in clockp
	;1 / wait for interrupt
	;mov (sp)+,clockp / restore clockp, ps
	;mov (sp)+,*$ps
	;rts r0

clear:
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 10/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/04/2013 - 03/08/2013 (Retro UNIX 8086 v1)
	; 'clear' zero's out of a block (whose block number is in r1)
	; on the current device (cdev)
	;	
	; INPUTS ->
	;    r1 - block number of block to be zeroed
	;    cdev - current device number 
	; OUTPUTS ->
	;    a zeroed I/O buffer onto the current device
	;    r1 - points to last entry in the I/O buffer
	;
	; ((AX = R1)) input/output
	;    (Retro UNIX Prototype : 18/11/2012 - 14/11/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: EDX, ECX, EBX, ESI, EDI, EBP))  

	call 	wslot
		; jsr r0,wslot / get an I/O buffer set bits 9 and 15 in first
                   ; / word of I/O queue r5 points to first data word in buffer
	mov	edi, ebx ; r5
	mov	edx, eax
	mov	ecx, 128
		; mov $256.,r3
	xor	eax, eax
	rep	stosd
	mov	eax, edx
; 1: 
       		; clr (r5)+ / zero data word in buffer
       		; dec r3
       		; bgt 1b / branch until all data words in buffer are zero
	;call	dskwr
		; jsr r0,dskwr / write zeroed buffer area out onto physical
                             ; / block specified in r1
	; eAX (r1) = block number
	;retn
		; rts r0
	; 24/12/2021
	jmp	dskwr