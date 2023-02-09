; Retro UNIX 386 v1 Kernel (v0.2.1.6) - SYS6.INC
; Last Modification: 19/07/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U6.ASM (23/07/2014) //// UNIX v1 -> u6.s
;
; ****************************************************************************

readi:
	; 11/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 20/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 11/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Reads from an inode whose number in R1
	; 
	; INPUTS ->
	;    r1 - inode number
	;    u.count - byte count user desires
	;    u.base - points to user buffer
	;    u.fofp - points to word with current file offset
	; OUTPUTS ->
	;    u.count - cleared
	;    u.nread - accumulates total bytes passed back
	;
	; ((AX = R1)) input/output
	;    (Retro UNIX Prototype : 01/03/2013 - 14/12/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: EDX, EBX, ECX, ESI, EDI)) -15/07/2022- 

	xor	edx, edx ; 0
	mov 	[u.nread], edx ; 0
		 ; clr u.nread / accumulates number of bytes transmitted
	mov	[u.pcount], dx ; 19/05/2015
	cmp 	[u.count], edx ; 0
	         ; tst u.count / is number of bytes to be read greater than 0
	ja 	short readi_1 ; 1f
		 ; bgt 1f / yes, branch
	retn
		 ; rts r0 / no, nothing to read; return to caller
readi_1: ; 1:
	         ; mov r1,-(sp) / save i-number on stack
	cmp	ax, 40
		 ; cmp r1,$40. / want to read a special file 
		 ;             / (i-nodes 1,...,40 are for special files)
        ;ja	dskr 
		 ; ble 1f / yes, branch
		 ; jmp dskr / no, jmp to dskr; 
		 ;         / read file with i-node number (r1)
		 ;    / starting at byte ((u.fofp)), read in u.count bytes
	; 24/12/2021
	jna	short readi_3
	jmp	dskr
readi_3:
	; (20/05/2015)
	push	eax ; because subroutines will jump to 'ret_'
	; 1:
	movzx	ebx, al
	; 11/01/2022
	shl	ebx, 2
	;shl	bx, 2
		 ; asl r1 / multiply inode number by 2
	add	ebx, readi_2 - 4
	jmp	dword [ebx]	
		 ; jmp *1f-2(r1)
readi_2: ; 1:
	dd	rtty ; tty, AX = 1 (runix)
		 ;rtty / tty; r1=2
		 ;rppt / ppt; r1=4
	dd	rmem ; mem, AX = 2 (runix)
		 ;rmem / mem; r1=6
		 ;rrf0 / rf0
		 ;rrk0 / rk0
		 ;rtap / tap0
		 ;rtap / tap1
		 ;rtap / tap2
		 ;rtap / tap3
		 ;rtap / tap4
		 ;rtap / tap5
		 ;rtap / tap6
		 ;rtap / tap7
	dd	rfd ; fd0, AX = 3 (runix only)
	dd	rfd ; fd1, AX = 4 (runix only)
	dd	rhd ; hd0, AX = 5 (runix only)
	dd	rhd ; hd1, AX = 6 (runix only)	
	dd	rhd ; hd2, AX = 7 (runix only)
	dd	rhd ; hd3, AX = 8 (runix only)	
	dd	rlpr ; lpr, AX = 9 (invalid, write only device !?)
	dd	rcvt ; tty0, AX = 10 (runix)	  
		 ;rcvt / tty0
	dd	rcvt ; tty1, AX = 11 (runix)	  
		 ;rcvt / tty1
	dd	rcvt ; tty2, AX = 12 (runix)	  
		 ;rcvt / tty2
	dd	rcvt ; tty3, AX = 13 (runix)	  
		 ;rcvt / tty3
	dd	rcvt ; tty4, AX = 14 (runix)	  
		 ;rcvt / tty4
	dd	rcvt ; tty5, AX = 15 (runix)	  
		 ;rcvt / tty5
	dd	rcvt ; tty6, AX = 16 (runix)	  
		 ;rcvt / tty6
	dd	rcvt ; tty7, AX = 17 (runix)	  
		 ;rcvt / tty7
	dd	rcvt ; COM1, AX = 18 (runix only)	  
		 ;rcrd / crd
	dd	rcvt ; COM2, AX = 19 (runix only)

rtty: ; / read from console tty
	; 11/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 17/10/2015 - 16/07/2015 (Retro UNIX 8086 v1)
	; 	     (Only 1 byte is read, by ignoring byte count!)
	;  	     WHAT FOR: Every character from Keyboard input 
	;	     must be written immediate on video page (screen)
	;	     when it is required.	
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 11/03/2013 - 19/06/2014 (Retro UNIX 8086 v1)
	;
	; Console tty buffer is PC keyboard buffer
	; and keyboard-keystroke handling is different than original
	; unix (PDP-11) here. TTY/Keyboard procedures here are changed
	; according to IBM PC compatible ROM BIOS keyboard functions. 
	;
	; 06/12/2013
	movzx	ebx, byte [u.uno] ; process number
	mov	al, [ebx+p.ttyc-1] ; current/console tty
rttys:
		; mov tty+[8*ntty]-8+6,r5 / r5 is the address of the 4th word of
	               ; / of the control and status block
		; tst 2(r5) / for the console tty; this word points to the console
		       ; / tty buffer
	; 28/07/2013
	mov 	[u.ttyn], al
	; 13/01/2014
	inc	al
	mov	[u.ttyp], al ; tty number + 1
rtty_nc: ; 01/02/2014
	; 29/09/2013
	;mov	ecx, 10
	; 11/01/2022
	sub	ecx, ecx
	mov	cl, 10
rtty_1: 	; 01/02/2014
	;push 	cx ; 29/09/2013
	; 24/12/2021
	push	ecx
	; byte [u.ttyn] = tty number (0 to 9) 
	mov 	al, 1
	call 	getc
	; 24/12/2021
	pop	ecx
	;pop 	cx ; 29/09/2013	
	jnz	short rtty_2
		; bne 1f / 2nd word of console tty buffer contains number
	               ; / of chars. Is this number non-zero?
	loop	rtty_idle ; 01/02/2014
	; 05/10/2013
	mov	ah, [u.ttyn]
	; 29/09/2013
	call	sleep
		; jsr r0,canon; ttych / if 0, call 'canon' to get a line
                ;           / (120 chars.)
	;byte [u.ttyn] = tty number (0 to 9) 
	jmp	short rtty_nc ; 01/02/2014

rtty_idle:
	; 29/07/2013
	call 	idle
	jmp	short rtty_1 ; 01/02/2014
	;1:
		; tst 2(r5) / is the number of characters zero
		; beq ret1 / yes, return to caller via 'ret1'
		; movb *4(r5),r1 / no, put character in r1
		; inc 4(r5) / 3rd word of console tty buffer points to byte which
		          ; / contains the next char.
		; dec 2(r5) / decrement the character count
rtty_2:
	xor 	al, al
	call 	getc
	call	passc
		; jsr r0,passc / move the character to core (user)
	;; 17/10/2015 - 16/07/2015
	; 19/06/2014
	;;jnz	short rtty_nc
	pop	eax  ; (20/05/2015)
	retn 
;ret1:
		; jmp ret / return to caller via 'ret'

rcvt:   ; < receive/read character from tty >
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/05/2013 - 06/12/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; In original UNIX v1, 'rcvt' routine 
	;		(exactly different than this one)
	;	was in 'u9.s' file.
	;
	sub 	al, 10
	; AL = tty number (0 to 9), (COM1=8, COM2=9)
	; 16/07/2013
	; 21/05/2013
        jmp     short rttys
      
;rppt: / read paper tape
;	jsr	r0,pptic / gets next character in clist for ppt input and
;			 / places
;		br ret / it in r1; if there 1s no problem with reader, it
;		       / also enables read bit in prs
;	jsr	r0,passc / place character in users buffer area
;	br	rppt

rmem: ; / transfer characters from memory to a user area of core
	; 17/10/2015
	; 11/06/2015
	; 24/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	;
	mov     esi, [u.fofp]
rmem_1:
        mov     ebx, [esi]        
	        ; mov *u.fofp,r1 / save file offset which points to the char
		               ; / to be transferred to user
        inc     dword [esi] ; 17/10/2015
		; inc *u.fofp / increment file offset to point to 'next' 
			    ; / char in memory file
	mov	al, [ebx]
		; movb (r1),r1 / get character from memory file, 
		             ; / put it in r1
	call	passc        ; jsr r0,passc / move this character to 
			     ;  / the next byte of the users core area
		; br rmem / continue
	jnz	short rmem_1
ret_:
	pop	eax ; 09/06/2015
	retn

rlpr:
;1:
;rcrd:
        mov     dword [u.error], ERR_DEV_NOT_RDY ; 19/05/2015
	jmp	error
		;jmp	error / see 'error' routine

dskr:
	; 19/07/2022
	; 12/10/2015
	; 21/08/2015
	; 25/07/2015
	; 10/07/2015
	; 16/06/2015
	; 31/05/2015
	; 24/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013 - 03/08/2013 (Retro UNIX 8086 v1)
dskr_0:
	push	eax
		; mov (sp),r1 / i-number in r1
	; AX = i-number
	call	iget
		; jsr r0,iget / get i-node (r1) into i-node section of core
        movzx   edx, word [i.size] ; 16/06/2015
		; mov i.size,r2 / file size in bytes in r2
	mov	ebx, [u.fofp]
	sub	edx, [ebx]
		; sub *u.fofp,r2 / subtract file offset
        ; 12/10/2015
	; jna     short ret_ 
		; blos ret
	ja	short dskr_1
	;
dskr_retn: ; 12/10/2015
	pop	eax
	mov	byte [u.kcall], 0
	retn	
dskr_1: 
	cmp     edx, [u.count] 
		; cmp r2,u.count / are enough bytes left in file 
			       ; / to carry out read
	jnb	short dskr_2
		; bhis 1f
	mov	[u.count], edx
		; mov r2,u.count / no, just read to end of file
dskr_2: ; 1:
	; AX = i-number
	call	mget
		; jsr r0,mget / returns physical block number of block 
			    ; / in file where offset points
	; EAX = physical block number
	call	dskrd
		; jsr r0,dskrd / read in block, r5 points to 
			     ; / 1st word of data in buffer
	; 09/06/2015
	cmp	byte [u.kcall], 0 ; the caller is 'namei' sign (=1)
	ja	short dskr_4	  ; zf=0 -> the caller is 'namei'
	cmp	word [u.pcount], 0
	ja	short dskr_4
dskr_3:
	; [u.base] = virtual address to transfer (as destination address)
	call	trans_addr_w ; translate virtual address to physical (w)
dskr_4:
	; EBX (r5) = system (I/O) buffer address -physical-
	call	sioreg
		; jsr r0,sioreg

	; 19/07/2022
	;xchg	esi, edi
	
	; 19/07/2022
	;  EDX = user data offset (previous value of [u.pbase])
	;  ESI = pointer to file offset 
	;  EDI = system (I/O) buffer offset
	;  ECX = byte count
	;  EBX = system buffer (data) address	
	;  EAX = remain bytes after byte count within page frame 

	add	[esi], ecx 
			; new file offset (old offset + byte count)
	;
	mov	esi, edi ; sector (I/O) buffer offset
	mov	edi, edx

	; EDI = file (user data) offset
	; ESI = sector (I/O) buffer offset
	; ECX = byte count
	rep	movsb
		; movb (r2)+,(r1)+ / move data from buffer into working core
		                 ; / starting at u.base
		; dec r3
		; bne 2b / branch until proper number of bytes are transferred
	; 25/07/2015
	; eax = remain bytes in buffer
        ;       (check if remain bytes in the buffer > [u.pcount])
	or	eax, eax
	jnz	short dskr_3 ; (page end before system buffer end!)		
	; 03/08/2013
	;pop	eax
	cmp	[u.count], ecx ; 0
		; tst u.count / all bytes read off disk
		; bne dskr
		; br ret
        ;ja	short dskr_0
	;mov	[u.kcall], cl ; 0 ; 09/06/2015
	;retn
	; 12/10/2015
	jna	short dskr_retn
	pop	eax  ; (i-node number)
	jmp	short dskr_0
	
passc:
	; 18/10/2015
	; 10/07/2015
	; 01/07/2015
	; 08/06/2015
	; 04/06/2015
	; 20/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	;
   	;(Retro UNIX 386 v1 - translation from user's virtual address
	;		      to physical address
	cmp	word [u.pcount], 0 ; byte count in page = 0 (initial value)
			     ; 1-4095 --> use previous physical base address
			     ; in [u.pbase]
	ja	short passc_3
	; 08/06/2015 - 10/07/2015
	call	trans_addr_w
passc_3:
	; 19/05/2015
	dec	word [u.pcount]
	;
	mov	ebx, [u.pbase]
	mov	[ebx], al
		; movb r1,*u.base / move a character to the next byte of the
		               ; / users buffer
	inc	dword [u.base]
		; inc u.base / increment the pointer to point to 
			  ; / the next byte in users buffer
	inc	dword [u.pbase] ; 04/06/2015
	inc	dword [u.nread]
		; inc u.nread / increment the number of bytes read
	dec	dword [u.count]
		; dec u.count / decrement the number of bytes to be read
		; bne 1f / any more bytes to read?; yes, branch
	retn
		; mov (sp)+,r0 / no, do a non-local return to the caller of
		             ; / 'readi' by:
		;/ (1) pop the return address off the stack into r0
		; mov (sp)+,r1 / (2) pop the i-number off the stack into r1
	;1:
		; clr	*$ps / clear processor status
		; rts r0 / return to address currently on top of stack

trans_addr_r:
	; Translate virtual address to physical address 
	; for reading from user's memory space
	; (Retro UNIX 386 v1 feature only !)
	; 18/10/2015
	; 10/07/2015
	; 09/06/2015
	; 08/06/2015 
	; 04/06/2015
	;
	; 18/10/2015
	xor	edx, edx ; 0 (read access sign)
	jmp 	short trans_addr_rw

	;push	eax
	;push	ebx
	;mov	ebx, [u.base]
	;call	get_physical_addr ; get physical address
	;;jnc	short cpass_0
	;jnc	short passc_1
	;mov	[u.error], eax
	;;pop	ebx
	;;pop	eax
	;jmp	error
;cpass_0:
	; 18/10/2015
	; 20/05/2015
	;mov 	[u.pbase], eax ; physical address	
	;mov	[u.pcount], cx ; remain byte count in page (1-4096)
	;pop	ebx
	;pop	eax
	;retn	; 08/06/2015

	; 24/12/2021 (Retro UNIX 386 v1.1)
trans_addr_w:
	; 31/12/2021
	; Translate virtual address to physical address 
	; for writing to user's memory space
	; (Retro UNIX 386 v1 feature only !)
	; 18/10/2015
	; 29/07/2015
	; 10/07/2015
	; 09/06/2015
	; 08/06/2015
	; 04/06/2015 (passc)
	;
	; 18/10/2015
	sub	edx, edx
	inc	dl ; 1 (write access sign)
trans_addr_rw:
	push	eax
	push	ebx
	; 18/10/2015
	push 	edx ; r/w sign (in DL)
	;
	mov	ebx, [u.base]
	call	get_physical_addr ; get physical address
	jnc	short passc_0
	mov	[u.error], eax
	;pop	edx
	;pop 	ebx
	;pop	eax
	jmp	error
passc_0:
	test	dl, PTE_A_WRITE ; writable page ; 18/10/2015
	pop	edx ; 18/10/2015
	jnz	short passc_1
	; 18/10/2015
	and 	dl, dl
	jz	short passc_1
	; 20/05/2015
	; read only (duplicated) page -must be copied to a new page-
	; EBX = linear address
	push 	ecx
	push	ebx ; * ; 31/12/2021 (BugFix)
	call 	copy_page
	pop	ebx ; * ; 31/12/2021 (BugFix)
	pop	ecx
	jc	short passc_2
	; 24/12/2021
	;push	eax ; physical address of the new/allocated page
	;call	add_to_swap_queue
	;pop	eax
	; 18/10/2015
	and 	ebx, PAGE_OFF ; 0FFFh
	;mov 	ecx, PAGE_SIZE
	;sub	ecx, ebx 
	add	eax, ebx  
passc_1: 
	; 18/10/2015
	; 20/05/2015
	mov 	[u.pbase], eax ; physical address	
	mov	[u.pcount], cx ; remain byte count in page (1-4096)
	pop	ebx
	pop	eax
	retn	; 08/06/2015
passc_2:
	mov	dword [u.error], ERR_MINOR_IM ; "Insufficient memory !" error
	;pop 	ebx
	;pop	eax
	jmp	error

writei:
	; 13/06/2022
	; 03/02/2022
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 20/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 12/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Write data to file with inode number in R1
	; 
	; INPUTS ->
	;    r1 - inode number
	;    u.count - byte count to be written
	;    u.base - points to user buffer
	;    u.fofp - points to word with current file offset
	; OUTPUTS ->
	;    u.count - cleared
	;    u.nread - accumulates total bytes passed back	
	; ((AX = R1))
	;    (Retro UNIX Prototype : 18/11/2012 - 11/11/2012, UNIXCOPY.ASM)
	;    ((Modified registers: EDX, EBX, ECX, ESI, EDI)) -15/07/2022-

	xor	ecx, ecx
	mov 	[u.nread], ecx  ; 0
		; clr u.nread / clear the number of bytes transmitted during
		            ; / read or write calls
	mov	[u.pcount], cx ; 19/05/2015
	cmp 	[u.count], ecx
	;	; tst u.count / test the byte count specified by the user
	ja 	short writei_1 ; 1f
		; bgt 1f / any bytes to output; yes, branch

	; 13/06/2022 - ('get/read LPT printer status' modification)
	cmp	eax, 16  ; LPR_INODE ; lpt (parallel port printer) ?
	jne	short writei_0
	jmp	lpr_stat	; get/read line status
writei_0:
	retn
	;	; rts r0 / no, return - no writing to do
writei_1: ;1:
		; mov r1 ,-(sp) / save the i-node number on the stack
	cmp 	ax, 40
		; cmp r1,$40.
		; / does the i-node number indicate a special file?
	;ja	dskw 
		; bgt dskw / no, branch to standard file output
	; 24/12/2021
	jna	short writei_3
	jmp	dskw
writei_3:
	; (20/05/2015)
	push	eax ; because subroutines will jump to 'wret'
	movzx	ebx, al
	; 03/02/2022
	shl	ebx, 2
	;shl	bx, 2
		; asl r1 / yes, calculate the index into the special file
	add	ebx, writei_2 - 4
	jmp	dword [ebx]	
		; jmp *1f-2(r1)
		; / jump table and jump to the appropriate routine
writei_2: ;1:
	dd	wtty ; tty, AX = 1 (runix)
		 ;wtty / tty; r1=2
		 ;wppt / ppt; r1=4
	dd	wmem ; mem, AX = 2 (runix)
		 ;wmem / mem; r1=6
		 ;wrf0 / rf0
		 ;wrk0 / rk0
		 ;wtap / tap0
		 ;wtap / tap1
		 ;wtap / tap2
		 ;wtap / tap3
		 ;wtap / tap4
		 ;wtap / tap5
		 ;wtap / tap6
		 ;wtap / tap7
	dd	wfd ; fd0, AX = 3 (runix only)
	dd	wfd ; fd1, AX = 4 (runix only)
	dd	whd ; hd0, AX = 5 (runix only)
	dd	whd ; hd1, AX = 6 (runix only)	
	dd	whd ; hd2, AX = 7 (runix only)
	dd	whd ; hd3, AX = 8 (runix only)	
	dd	wlpr ; lpr, AX = 9   (runix)
	dd	xmtt ; tty0, AX = 10 (runix)	  
		 ;xmtt / tty0
	dd	xmtt ; tty1, AX = 11 (runix)	  
		 ;xmtt / tty1
	dd	xmtt ; tty2, AX = 12 (runix)	  
		 ;xmtt / tty2
	dd	xmtt ; tty3, AX = 13 (runix)	  
		 ;xmtt / tty3
	dd	xmtt ; tty4, AX = 14 (runix)	  
		 ;xmtt / tty4
	dd	xmtt ; tty5, AX = 15 (runix)	  
		 ;xmtt / tty5
	dd	xmtt ; tty6, AX = 16 (runix)	  
		 ;xmtt / tty6
	dd	xmtt ; tty7, AX = 17 (runix)	  
		 ;xmtt / tty7
	dd	xmtt ; COM1, AX = 18 (runix only)	  
		; / wlpr / lpr
	dd	xmtt ; COM2, AX = 19 (runix only)	

wtty: ; write to console tty (write to screen)
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 18/11/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 12/03/2013 - 07/07/2014 (Retro UNIX 8086 v1)
	;
	; Console tty output is on current video page
	; Console tty character output procedure is changed here
	; acconding to IBM PC compatible ROM BIOS video (text mode) functions.
	;
	movzx	ebx, byte [u.uno] ; process number
	mov	ah, [ebx+p.ttyc-1] ; current/console tty
	mov	al, ah ; 07/07/2014
wttys:	
	; 10/10/2013
	mov 	[u.ttyn], ah
	; 13/01/2014
	inc	al
	mov	[u.ttyp+1], al ; tty number + 1
wtty_nc: ; 15/05/2013
	; AH = [u.ttyn] = tty number ; 28/07/2013
	call	cpass
		; jsr r0,cpass / get next character from user buffer area; if
		             ; / none go to return address in syswrite
		; tst r1 / is character = null
		; beq wtty / yes, get next character
	; 10/10/2013
	jz	short wret
	;1 : 
		;mov 	$240,*$ps / no, set processor priority to five
		;cmpb	cc+1,$20. / is character count for console tty greater
		;	          / than 20
		;bhis	2f / yes; branch to put process to sleep
	; 27/06/2014
wtty_1:
	; AH = tty number
	; AL = ASCII code of the character
	; 15/04/2014
	;push	ax
	; 24/12/2021
	push	eax
	call	putc ; 14/05/2013
	jnc	short wtty_2
	; 18/11/2015
	call	idle
	;mov	ax, [esp]
	; 24/12/2021
	mov	eax, [esp]
	call	putc
	jnc	short wtty_2 
	; 02/06/2014
	mov	ah, [u.ttyn]
	call	sleep
	;pop	ax
	; 24/12/2021
	pop	eax
	jmp 	short wtty_1
		; jc 	error ; 15/05/2013 (COM1 or COM2 serial port error)
		; jsr 	r0,putc; 1 / find place in freelist to assign to 
			      ; / console tty and
		; br 	2f / place character in list; if none available
		   	  ; / branch to put process to sleep
		; jsr	r0,startty / attempt to output character on tty
wtty_2:
	; 15/04/2014
	;pop	ax
	; 24/12/2021
	pop	eax
	jmp	short wtty_nc
		; br wtty
wret:	; 10/10/2013 (20/05/2015)
	pop	eax
	retn
	;2:
		;mov	r1,-(sp) / place character on stack
		;jsr	r0,sleep; 1 / put process to sleep
		;mov	(sp)+,r1 / remove character from stack
		;br	1b / try again to place character in clist and output

xmtt:   ; < send/write character to tty >
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/05/2013 - 06/12/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; In original UNIX v1, 'xmtt' routine 
	;		(exactly different than this one)
	;	was in 'u9.s' file.
	;
	sub 	al, 10
	; AL = tty number (0 to 9), (COM1=8, COM2=9)
	; 10/10/2013
	mov	ah, al
	; 28/07/2013
	jmp	short wttys

;wppt:
;	jsr	r0,cpass / get next character from user buffer area,
;		         / if none return to writei's calling routine
;	jsr	r0,pptoc / output character on ppt
;	br	wppt

wmem: ; / transfer characters from a user area of core to memory file
	; 17/10/2015
	; 11/06/2015
	; 24/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	;
	cmp	dword [x_timer], clock ; multi tasking clock/timer
        je      short wmem_acc_err
	;
        mov     esi, [u.fofp] 
wmem_1:
	call	cpass
		; jsr r0,cpass / get next character from users area of
			     ; / core and put it in r1
        	; mov r1,-(sp) / put character on the stack
	; 20/09/2013
	jz	short wret ; wmem_2  
        mov     ebx, [esi]
		; mov *u.fofp,r1 / save file offset in r1
        inc     dword [esi] ; 17/10/2015
		; inc *u.fofp / increment file offset to point to next
			    ; / available location in file
	mov	[ebx], al	
		; movb (sp)+,(r1) / pop char off stack, put in memory loc 
			        ; / assigned to it
	jmp	short wmem_1
		; br wmem / continue
	;1:
	;jmp	error / ?
;wmem_2:	
;	; 20/09/2013
;	pop	ax
;	retn

wmem_acc_err:
	mov	dword [u.error], ERR_FILE_ACCESS ; permission denied !
	jmp	error

;wlpr:
	; 13/06/2022
	;mov	dword [u.error], ERR_DEV_NOT_RDY ; 19/05/2015
	;jmp	error   ; ... Printing procedure will be located here ...
		;/	jsr	r0,cpass
		;/	cmp	r0,$'a
		;/	blo	1f
		;/	cmp	r1,$'z
		;/	bhi	1f
		;/	sub	$40,r1
		;/1:
		;/	jsr	r0,lptoc
		;/	br	wlpr
		; br rmem / continue

; 13/06/2022 - Retro UNIX 386 v1.1 - PRINTER BIOS (Functions)

;; Ref: MSDOS 3.3 (Retro DOS 3.2) Printer driver code (MSLPT.ASM)
;; MSLPT.ASM - MSDOS 3.3 - 24/07/1987
;; 23/03/2018 - Retro DOS v2.0
;; RETRODOS32.ASM - 03/08/2019 (Retro DOS v3.2)

; IBM ROMBIOS (INT 17h) STATUS BITS

NOTBUSYSTATUS	equ 10000000b	; NOT BUSY
ACKSTATUS	equ 01000000b	; ACKNOWLEDGE (FOR WHAT?)
NOPAPERSTATUS	equ 00100000b	; NO MORE PAPER
SELECTEDSTATUS	equ 00010000b	; THE PRINTER SAID IT WAS SELECTED
IOERRSTATUS	equ 00001000b	; SOME KIND ERROR
RESERVED	equ 00000110b	; NOPS
TIMEOUTSTATUS	equ 00000001b	; TIME OUT.

;----------------------------------------------------------------
;								:
;		WRITE TO PRINTER DEVICE 			:
;								:
;   CX has count of bytes to be printed 			:
;   ES:DI point to source buffer contains characters		:
;   AuxNum (in msbio.asm) has printer number			:
;								:
;----------------------------------------------------------------

wlpr:
	; 15/07/2022
	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
PRN$WRIT:
	; INPUT:
	;	[u.count] = count of characters to be printed
	;	[u.base] = buffer address in user's memory space
	;
	;	(if ECX = 0, printer status will be returned)
	
	;xor	ebx, ebx
PRN$LOOP:
	call	cpass		  ; Get a character into AL
	jz	short pr_exit
	;
	mov	bl, 2  ; retry count
PRN$OUT:
	; al = character which will be printed
	xor	ah, ah ; 0	  ; PRINT THE CHARACTER IN (AL)
	call	PRNOP
	jz	short PRN$LOOP 	  ; if error, try to print again
PrRetry:
	; al = character
	dec	bl
	jnz	short PRN$OUT
pr_err_exit:
	movzx	eax, al
	mov	[u.error], eax
	mov	[u.r0], eax ; error code in AL
	;mov 	ebp, [u.sp]
	mov	ebx, [u.sp] ; 15/07/2022
			; Kernel stack at the beginning of sys call
	mov	edx, [u.nread]
	dec	edx ; last char failed
	;mov	[ebp+20], edx ; count of printed characters in edx
	mov	[ebx+20], edx ; 15/07/2022
	jmp	error
pr_exit:
	pop	eax ; inode number

	;mov	eax, [u.nread]
	;mov	[u.r0], eax ; count of printed chacters
	;jmp	sysret
	retn	; return from writei to syswrite (rw0)

; 13/06/2022

;----------------------------------------------------------------
;								:
;		PRINTER STATUS ROUTINE				:
;								:
;----------------------------------------------------------------
;

lpr_stat:
	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
PRN$STAT:
	call	PRNSTAT		  ; get the status
	jnz	short prn_stat_retn
				  ; if error jump to error routine
	;mov	al, 9		  ; AGAIN, ASSUME OUT OF PAPER...
	mov	al, ERR_PRN_PAPER
	test	ah, NOPAPERSTATUS
	jnz	short prn_stat_retn
	test	ah, NOTBUSYSTATUS
	jnz	short prn_stat_ok ; if not busy return (with cf=0)
	mov	al, ERR_PRN_BUSY  ; else busy, return to busy exit
prn_stat_retn:
	; al = error code
	; ah = status flags
	mov	[u.r0], eax
	;movzx	eax, al
	;mov 	[u.error], eax
	pop	eax ; discard return address to syswrite
	jmp	sysret
prn_stat_ok:
	xor	al, al ; 0
	jmp	short prn_stat_retn

;
;   PRNSTAT	get printer status
;   PRNOP	print a character
;
; PRNSTAT and PRNOP are two routines which call on the ROM-BIOS
; printer routines.  The routines share code which calls on the bios and
; then determines which, if any, error occured. PRNSTAT and PRNOP differ
; only by the value put into AH before the ROM-BIOS call.
;
;   INPUT	if PRNOP then character in AL
;
;   OUTPUT	- AL holds error code
;		- AH status byte from printer
;		- flag NZ if error

PRNSTAT:
	mov	ah, 2		  ; set command for get status 

PRNOP:
	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
	;
	; Print character (on paper)

	; INPUT:
	;	al = character to be printed
	; OUTPUT:
	;	zf = 1 -> ok
	;	zf = 0 -> error code in AL

	call	int17h	 ; call lpt bios
	
	test	ah, IOERRSTATUS	  ; I/O ERROR?
	jz	short short prnop_chk_nrdy ; NO, TRY NOT READY

	; AT THIS POINT, WE KNOW WE HAVE AN ERROR.
	; THE CONVERSE IS NOT TRUE.

	;mov	al, 9		  ; FIRST, ASSUME OUT OF PAPER
	mov	al, ERR_PRN_PAPER
	test	ah, NOPAPERSTATUS ; OUT OF PAPER SET?
	jnz	short PRNOP1	  ; YES, ERROR IS SET
	;mov	al, ERR_PRN_IO
	inc	al		  ; INDICATE I/O ERROR
PRNOP1: 

; WE HAVE TRIAGED NOW FOR OUT OF PAPER AND IO ERR (IGNORING TIME-OUT)

	retn			  ; RETURN WITH ERROR
 
; THE BITS SAID NO ERROR.
; UNFORTUNATELY, THERE MAY BE OTHER THINGS AT WORK HERE.

prnop_chk_nrdy:
	;mov	al, 2		  ; ASSUME NOT-READY
	mov	al, ERR_PRN_TIMEOUT ; ''time out !' error
	
	test	ah, TIMEOUTSTATUS ; IS TIME-OUT SET?
				  ; IF NZ THEN ERROR, ELSE OK???
PRNOP2: 
	retn


dskw: ; / write routine for non-special files
	;
	; 19/07/2022
	;	(file offset bugfix for 'dskwr' error return situation)
	;
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 25/07/2015
	; 16/06/2015
	; 09/06/2015
	; 31/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013 - 20/09/2013 (Retro UNIX 8086 v1)
	;
	; 01/08/2013 (mkdir_w check)

	;push	ax ; 26/04/2013
		; mov (sp),r1 / get an i-node number from the stack into r1
	; 24/12/2021
	push	eax
	; AX = inode number
	call	iget
		; jsr r0,iget / write i-node out (if modified), 
		            ; / read i-node 'r1' into i-node area of core
        mov     ebx, [u.fofp] 
	mov 	edx, [ebx]
		; mov *u.fofp,r2 / put the file offset [(u.off) or the offset
			       ; / in the fsp entry for this file] in r2
	add 	edx, [u.count]	
		; add u.count,r2 / no. of bytes to be written
			       ; / + file offset is put in r2
	; 16/06/2015        
	cmp	edx, 65535 ; file size limit (for UNIX v1 file system)
	jna	short dskw_0
	mov	dword [u.error], ERR_FILE_SIZE ; 'file size error !'
	jmp	error
dskw_0:	
	cmp     dx, [i.size]
		; cmp r2,i.size / is this greater than the present size of
		              ; / the file?
	jna	short dskw_1
		; blos 1f / no, branch
        mov     [i.size], dx
	 	; mov r2,i.size / yes, increase the file size to 
			      ; / file offset + no. of data bytes
	call	setimod
	 	; jsr r0,setimod / set imod=1 (i.e., core inode has been
		          ; / modified), stuff time of modification into
	          	  ; / core image of i-node
dskw_1: ; 1:	
	call	mget
	; EAX = Block number
		; jsr r0,mget / get the block no. in which to write 
			    ; /	the next data byte
	; eax = block number
	mov     ebx, [u.fofp]
	mov	edx, [ebx]
	and	edx, 1FFh  
		; bit *u.fofp,$777 / test the lower 9 bits of the file offset
	jnz	short dskw_2
		; bne 2f / if its non-zero, branch; if zero, file offset = 0,
		       ; / 512, 1024,...(i.e., start of new block)
	cmp	dword [u.count], 512
		; cmp u.count,$512. / if zero, is there enough data to fill
				  ; / an entire block? (i.e., no. of
	jnb	short dskw_3
		; bhis 3f / bytes to be written greater than 512.? 
			; / Yes, branch. Don't have to read block
dskw_2: ; 2: / in as no past info. is to be saved (the entire block will be
   		; / overwritten).
	call	dskrd
		; jsr r0,dskrd / no, must retain old info.. 
			     ; / Hence, read block 'r1' into an I/O buffer
dskw_3: ; 3:
	; EAX (r1) = block/sector number
	call	wslot
		; jsr r0,wslot / set write and inhibit bits in I/O queue, 
			   ; / proc. status=0, r5 points to 1st word of data
	cmp	byte [u.kcall], 0
	ja	short dskw_5 ; zf=0 -> the caller is 'mkdir'
	;
	cmp	word [u.pcount], 0
	ja	short dskw_5
dskw_4:
	; [u.base] = virtual address to transfer (as source address)
	call	trans_addr_r ; translate virtual address to physical (r)
dskw_5:
	; EBX (r5) = system (I/O) buffer address
	call	sioreg
		; jsr r0,sioreg / r3 = no. of bytes of data, 
			     ; / r1 = address of data, r2 points to location
			     ; / in buffer in which to start writing data
	; 19/07/2022
	;  EDX = user data offset (previous value of [u.pbase])
	;  ESI = pointer to file offset 
	;  EDI = system (I/O) buffer offset
	;  ECX = byte count
	;  EBX = system buffer (data) address	
	;  EAX = remain bytes after byte count within page frame 

	; 19/07/2022 - Erdogan Tan
	; BugFix (Also original unix v1 kernel code has this bug!)
	; ((Against a possible disk write failure/error, 
	;   file offset must not be updated/increased before 'dskwr'
	;   but it was updated in 'sioreg'. I have modified 'sioreg'
	;   and 'dskw' procedures for that.))

	; 19/07/2022
	push	esi ; *	 ; save file offset (pointer)
	push	ecx ; ** ; save byte count
	mov	esi, edx

	; ESI = file (user data) offset
	; EDI = sector (I/O) buffer offset
	; ECX = byte count
	;
  	rep	movsb
		; movb (r1 )+,(r2)+ 
		         ; / transfer a byte of data to the I/O buffer
		; dec r3 / decrement no. of bytes to be written
		; bne 2b / have all bytes been transferred? No, branch
	; 25/07/2015
	; eax = remain bytes in buffer
        ;       (check if remain bytes in the buffer > [u.pcount])
	or	eax, eax
	jnz	short dskw_4 ; (page end before system buffer end!)
dskw_6:
	call	dskwr
		; jsr r0,dskwr / yes, write the block and the i-node

	; 19/07/2022
	; (there is not a disk write error, we can increase file offset)
	pop	eax ; ** ; byte count
	pop	edi ; *  ; file offset (pointer)
	;
	add	[edi], eax
			; new file offset (old offset + byte count)

        cmp     dword [u.count], 0
		; tst u.count / any more data to write?
	ja	short dskw_1
		; bne 1b / yes, branch
	; 03/08/2013
	mov	byte [u.kcall], 0
	; 20/09/2013 (;;)
	;pop	ax
	; 24/12/2021
	pop	eax
	retn
	;;jmp 	short dskw_ret 
	        ; jmp ret / no, return to the caller via 'ret'

cpass: ; / get next character from user area of core and put it in r1
	; 18/10/2015
	; 10/10/2015
	; 10/07/2015
	; 02/07/2015
	; 01/07/2015
	; 24/06/2015
	; 08/06/2015
	; 04/06/2015
	; 20/05/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	;
	; INPUTS -> 
	;     [u.base] = virtual address in user area
	;     [u.count] = byte count (max.)
	;     [u.pcount] = byte count in page (0 = reset)		
	; OUTPUTS -> 
	;     AL = the character which is pointed by [u.base]
	;     zf = 1 -> transfer count has been completed	
        ;
	; ((Modified registers:  EAX, EDX, ECX))
	;
	;
	cmp 	dword [u.count], 0  ; 14/08/2013
		; tst u.count / have all the characters been transferred
			    ; / (i.e., u.count, # of chars. left
	jna	short cpass_3
		; beq 1f / to be transferred = 0?) yes, branch
	dec	dword [u.count]
		; dec u.count / no, decrement u.count
        ; 19/05/2015 
	;(Retro UNIX 386 v1 - translation from user's virtual address
	;		      to physical address
	cmp	word [u.pcount], 0 ; byte count in page = 0 (initial value)
			     ; 1-4095 --> use previous physical base address
			     ; in [u.pbase]
	ja	short cpass_1
	; 02/07/2015
        cmp     dword [u.ppgdir], 0  ; is the caller os kernel
        je      short cpass_k       ; (sysexec, '/etc/init') ? 
	; 08/06/2015 - 10/07/2015
	call	trans_addr_r
cpass_1:
	; 02/07/2015
	; 24/06/2015
	dec	word [u.pcount]
cpass_2: 
	; 10/10/2015
	; 02/07/2015
	mov	edx, [u.pbase]
	mov	al, [edx] ; 10/10/2015
		; movb *u.base,r1 / take the character pointed to 
				; / by u.base and put it in r1
	inc	dword [u.nread]
		; inc u.nread / increment no. of bytes transferred
	inc	dword [u.base]
		; inc u.base / increment the buffer address to point to the
			   ; / next byte
	inc	dword [u.pbase] ; 04/06/2015
cpass_3:
	retn
		; rts	r0 / next byte
	; 1: 
		; mov (sp)+,r0 
		         ; / put return address of calling routine into r0
		; mov (sp)+,r1 / i-number in r1
		; rts r0 / non-local return
cpass_k:
	; 02/07/2015
	; The caller is os kernel 
	; (get sysexec arguments from kernel's memory space)
	;
	mov	ebx, [u.base]
        mov     word [u.pcount], PAGE_SIZE ; 4096
	mov	[u.pbase], ebx
	jmp	short cpass_2
	
sioreg:
	; 19/07/2022
	;	(file offset bugfix for 'dskwr' error return situation)
	; 25/07/2015
	; 18/07/2015
	; 02/07/2015
	; 17/06/2015
	; 09/06/2015
	; 19/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 12/03/2013 - 22/07/2013 (Retro UNIX 8086 v1)
	;
	; INPUTS -> 
	;     EBX = system buffer (data) address (r5)
	;     [u.fofp] = pointer to file offset pointer
	;     [u.base] = virtual address of the user buffer
	;     [u.pbase] = physical address of the user buffer
	;     [u.count] = byte count
	;     [u.pcount] = byte count within page frame 			
	; OUTPUTS -> 
	;     ESI = user data offset (r1)
	;     EDI = system (I/O) buffer offset (r2)
	;     ECX = byte count (r3)
	;     EAX = remain bytes after byte count within page frame
	;	(If EAX > 0, transfer will continue from the next page)
        ;
	; ((Modified registers:  EDX))

	; 19/07/2022
	; OUTPUTS -> 
	;	EDX = user data offset (previous value of [u.pbase])
	;	ESI = pointer to file offset 
	; 	EDI = system (I/O) buffer offset
	;	ECX = byte count
	;	EBX = system buffer (data) address	
	;	EAX = remain bytes after byte count within page frame
 
        mov     esi, [u.fofp]
        mov     edi, [esi]
		; mov *u.fofp,r2 / file offset (in bytes) is moved to r2
	mov	ecx, edi
		; mov r2,r3 / and also to r3
	or	ecx, 0FFFFFE00h
		; bis $177000,r3 / set bits 9,...,15 of file offset in r3
	and	edi, 1FFh
		; bic $!777,r2 / calculate file offset mod 512.
	add	edi, ebx ; EBX = system buffer (data) address
		; add r5,r2 / r2 now points to 1st byte in system buffer
			  ; / where data is to be placed
                ; mov u.base,r1 / address of data is in r1
	neg	ecx
		; neg r3 / 512 - file offset (mod512.) in r3 
		       ; / (i.e., the no. of free bytes in the file block)
	cmp	ecx, [u.count]
		; cmp r3,u.count / compare this with the no. of data bytes
			       ; / to be written to the file
	jna	short sioreg_0
		; blos	2f / if less than branch. Use the no. of free bytes
			 ; / in the file block as the number to be written
	mov	ecx, [u.count]
		; mov u.count,r3 / if greater than, use the no. of data 
			       ; / bytes as the number to be written
sioreg_0:
	; 17/06/2015
	cmp	byte [u.kcall], 0 
	jna	short sioreg_1
	; 25/07/2015
	; the caller is 'mkdir' or 'namei'
	mov	eax, [u.base] ; 25/07/2015
	mov 	[u.pbase], eax ; physical address = virtual address
	mov	word [u.pcount], cx ; remain bytes in buffer (1 sector)
	jmp	short sioreg_2
sioreg_1:
	; 25/07/2015
	; 18/07/2015
	; 09/06/2015 
	movzx	edx, word [u.pcount]
		; ecx and [u.pcount] are always > 0, here
	cmp	ecx, edx	
	ja	short sioreg_4 ; transfer count > [u.pcount]
sioreg_2: ; 2:
	xor 	eax, eax ; 25/07/2015
sioreg_3:
	add 	[u.nread], ecx
		; add r3,u.nread / r3 + number of bytes xmitted 
			         ; / during write is put into u.nread
	sub 	[u.count], ecx
		; sub r3,u.count / u.count = no. of bytes that still 
			       ; / must be written or read
	add 	[u.base], ecx
		; add r3,u.base / u.base points to the 1st of the remaining
			      ; / data bytes
        ; 19/07/2022
	;add 	[esi], ecx 
	;	; add r3,*u.fofp / new file offset = number of bytes done
			       ; / + old file offset
	; 25/07/2015
	;mov	esi, [u.pbase]
	; 19/07/2022
	mov	edx, [u.pbase]

	sub	[u.pcount], cx
	add	[u.pbase], ecx
        retn
		; rts r0
		; transfer count > [u.pcount]
sioreg_4:
	; 25/07/2015
	; transfer count > [u.pcount] 
	; (ecx > edx)
	mov	eax, ecx
	sub	eax, edx ; remain bytes for 1 sector (block) transfer 
	mov	ecx, edx ; current transfer count = [u.pcount]
	jmp	short sioreg_3