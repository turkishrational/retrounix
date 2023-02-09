; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2.2.3) - SYS0.INC
; Last Modification: 17/07/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U0.ASM (28/07/2014) //// UNIX v1 -> u0.s
;
; ****************************************************************************

sys_init:
	; 23/02/2022
	; 08/01/2022
	; 01/01/2022
	; 26/12/2021
	; 27/11/2021
	; 22/11/2021 - Retro UNIX 386 v2 (test) compatibility modification
	; 18/10/2015
	; 28/08/2015
	; 24/08/2015
	; 14/08/2015
	; 24/07/2015 
	; 02/07/2015
	; 01/07/2015
	; 23/06/2015
	; 15/04/2015
	; 13/04/2015
	; 11/03/2015 (Retro UNIX 386 v1 - Beginning)
	; 28/07/2014 (Retro UNIX 8086 v1)
	;
	;call	ldrv_init ; Logical drive description tables initialization
	;
	;; 14/02/2014
	;; 14/07/2013
	;mov	ax, 41
	;mov	[rootdir], ax
	;mov	[u.cdir], ax
	;and	al, 1 ; 15/04/2015
	
	; 27/11/2021
	; 18/04/2021 - Retro UNIX 386 v2
	xor	eax, eax
	inc	al	; eax = 1 (root directory inode) ; runix v2 fs
	mov	[rootdir], eax ; = 1 ; 28/10/2021 (32 bit)
	mov	[u.cdir], eax ; 28/10/2021 (32 bit)

	mov	[u.uno], al ; = 1
	;mov	[mpid], ax ; 1 
	;mov	[p.pid], ax ; 1
	; 01/01/2022
	mov	[mpid], al
	mov	[p.pid], al
	mov	[p.stat], al ; SRUN, 05/02/2014
	;
	mov	al, time_count ; 30/08/2013
	mov	[u.quant], al ; 14/07/2013
	; 02/07/2015
	mov	eax, [k_page_dir]
	mov	[u.pgdir], eax ; reset
	; 18/10/2015
	;sub	eax, eax
	;mov	[u.ppgdir], eax ; 0
        ;
 	; 23/02/2022
 	;call	epoch
	;mov	[s.time], eax ; 13/03/2015
	; 17/07/2013
	call 	bf_init ; buffer initialization
	; 23/02/2022
	; (save sysinit time on sb0)
	call	epoch
	mov	[s.time], eax ; 13/03/2015
	; 23/06/2015
	call	allocate_page
	;;jc	error
	;jc	panic   ; jc short panic (01/07/2015)
	; 05/12/2021
	jnc	short sysinit_1
	jmp	panic
sysinit_1:
	mov	[u.upage], eax ; user structure page	
	mov	[p.upage], eax
	;
	call	clear_page
	;
	; 14/08/2015
	cli
	; 14/03/2015
	; 17/01/2014
	call	sp_init ; serial port initialization
	; 14/08/2015
	sti
	;
	; 30/06/2015
	;mov	esi, kernel_init_ok_msg
	;call 	print_msg
	;
	xor	bl, bl ; video page 0
vp_clr_nxt:  ; clear video pages (reset cursor positions)
	call 	vp_clr  ; 17/07/2013
	inc	bl
	cmp	bl, 8
	jb	short vp_clr_nxt
	;
	; 24/07/2015
	;push	KDATA
        ;push	esp
	;mov	[tss.esp0], esp
        ;mov	word [tss.ss0], KDATA
	;
	; 08/01/2022
	; 24/08/2015
	;; temporary (01/07/2015)
	;mov	byte [u.quant], time_count ; 4 
	;		       ; it is not needed here !
	;;inc	byte [u.kcall] ; 'the caller is kernel' sign
	dec 	byte [sysflg] ; FFh = ready for system call
			      ; 0 = executing a system call
	;;sys 	_msg, kernel_init_ok_msg, 255, 0
	;
	;;; 06/08/2015
	;;;call	getch ; wait for a key stroke
	;;mov 	ecx, 0FFFFFFFh	
;;sys_init_msg_wait:
;;	push 	ecx
;;	mov	al, 1
;;	mov 	ah, [ptty] ; active (current) video page
;;	call	getc_n
;;	pop	ecx
;;	jnz	short sys_init_msg_ok
;;	loop	sys_init_msg_wait
	;
;;sys_init_msg_ok:
	; 28/08/2015 (initial settings for the 1st 'rswap')
	push	KDATA ; ss
	push	esp
	pushfd
	push	KCODE ; cs
	push	init_exec ; eip
	mov	[u.sp], esp
	push	ds
	push	es
	push	fs
	push	gs	
	pushad
	mov	[u.usp], esp
	call	wswap ; save current user (u) structure, user registers
		      ; and interrupt return components (for IRET)
	popad
	pop	ax ; gs
	pop	ax ; fs
	pop	ax ; es
	pop	ax ; ds	
	pop	eax ; eip (init_exec)
	pop	ax ; cs (KCODE)
	pop	eax ; E-FLAGS
	pop	eax ; esp
	pop	ax ; ss (KDATA)
	;
	; 26/12/2021 ([u.ppgdir] is zero already)
	;xor	eax, eax ; 0
	;mov	[u.ppgdir], eax ; reset (to zero) for '/etc/init'
	;
	; 02/07/2015
	; [u.pgdir ] = [k_page_dir]
	; [u.ppgdir] = 0 (page dir of the parent process)
	;     (The caller is os kernel sign for 'sysexec')
init_exec:
	; 13/03/2013
	; 24/07/2013
	mov	ebx, init_file
	mov	ecx, init_argp
	; EBX contains 'etc/init' asciiz file name address  
	; ECX contains address of argument list pointer
	;
	;dec 	byte [sysflg] ; FFh = ready for system call
			      ; 0 = executing a system call
	sys	_exec  ; execute file
	jnc	short panic
	;
	mov	esi, etc_init_err_msg
	; 22/11/2021
	;call 	print_msg
	jmp	short key_to_reboot

;align 4
init_argp:
	dd 	init_file, 0  ; 23/06/2015 (dw -> dd)
init_file:
	; 24/08/2015
	db 	'/etc/init', 0
panic:
	; 13/03/2015 (Retro UNIX 386 v1)
	; 07/03/2014 (Retro UNIX 8086 v1)
	mov 	esi, panic_msg
key_to_reboot: ; 22/11/2021
	call 	print_msg
;key_to_reboot:
	; 15/11/2015
	call 	getch 
		; wait for a character from the current tty
	;
	mov	al, 0Ah
	mov	bl, [ptty] ; [active_page]
	mov	ah, 07h ; Black background, 
			; light gray forecolor
	call 	write_tty
	jmp	cpu_reset 

print_msg:
	; 01/07/2015
	; 13/03/2015 (Retro UNIX 386 v1)
	; 07/03/2014 (Retro UNIX 8086 v1)
	; (Modified registers: EAX, EBX, ECX, EDX, ESI, EDI)
	;
	;
	lodsb
pmsg1:
	push 	esi
	movzx	ebx, byte [ptty]
	mov	ah, 07h ; Black background, light gray forecolor
	call 	write_tty
	pop	esi
	lodsb
	and 	al, al
	jnz 	short pmsg1
	retn
	
ctrlbrk:
	; 06/02/2022
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 12/11/2015
	; 13/03/2015 (Retro UNIX 386 v1)
	; 06/12/2013 (Retro UNIX 8086 v1)
	;
	; INT 1Bh (control+break) handler		
	;
      	; Retro Unix 8086 v1 feature only!
      	;
	cmp 	word [u.intr], 0
	jna 	short cbrk4
cbrk0:
	; 12/11/2015
	; 06/12/2013
	cmp 	word [u.quit], 0
	jz	short cbrk4
	;
	; 20/09/2013	
	;push 	ax
	; 04/12/2021
	push	eax

	; 06/02/2022
	; (repetitive ctrl+brk check) 
	mov	ax, [u.quit]
	inc	ax ; 0FFFFh -> 0
	jz	short cbrk3

	; 06/12/2013
	mov	al, [ptty]
	;
	; 12/11/2015
	;
	; ctrl+break (EOT, CTRL+D) from serial port
	; or ctrl+break from console (pseudo) tty
	; (!redirection!)
	;
	cmp	al, 8 ; serial port tty nums > 7
        jb      short cbrk1 ; console (pseudo) tty
	;	
	; Serial port interrupt handler sets [ptty]
	; to the port's tty number (as temporary).
	;
	; If active process is using a stdin or 
	; stdout redirection (by the shell),
        ; console tty keyboard must be available
	; to terminate running process,
	; in order to prevent a deadlock. 
	;
	push	edx
	movzx	edx, byte [u.uno]
	cmp     al, [edx+p.ttyc-1] ; console tty (rw)
	pop	edx
	je	short cbrk2
cbrk1:
	inc 	al  ; [u.ttyp] : 1 based tty number
	; 06/12/2013
	cmp	al, [u.ttyp] ; recent open tty (r)
	je	short cbrk2	
        cmp     al, [u.ttyp+1] ; recent open tty (w)
	jne	short cbrk3	
cbrk2:
	;; 06/12/2013
	;mov	ax, [u.quit]
	;and	ax, ax
	;jz	short cbrk3
	;
	;xor	ax, ax ; 0
	;dec	ax
	; 04/12/2021
	xor	eax, eax ; 0
	dec	eax ; 0FFFFFFFFh
	; 0FFFFh = 'ctrl+brk' keystroke
	mov	[u.quit], ax
cbrk3:
	;pop	ax
	; 04/12/2021
	pop	eax
cbrk4:
	retn

com2_int:
	; 08/01/2022 (Retro UNIX 386 v1.2)
	; 07/11/2015 
	; 24/10/2015
	; 23/10/2015
	; 14/03/2015 (Retro UNIX 386 v1 - Beginning)
	; 28/07/2014 (Retro UNIX 8086 v1)
	; < serial port 2 interrupt handler >
	;
	mov 	[esp], eax ; overwrite call return address
	;;push	eax
	; 08/01/2022
	sub	eax, eax
	mov	al, 9
	;mov	ax, 9
	jmp	short comm_int
com1_int:
	; 07/11/2015
	; 24/10/2015
	mov 	[esp], eax ; overwrite call return address
	; 23/10/2015
	;push	eax
	; 08/01/2022
	sub	eax, eax
	mov	al, 8
	;mov	ax, 8
comm_int:
	; 08/01/2022
	; 20/11/2015
	; 18/11/2015
	; 17/11/2015
	; 16/11/2015
	; 09/11/2015
	; 08/11/2015
	; 07/11/2015
	; 06/11/2015 (serial4.asm, 'serial')	
	; 01/11/2015
	; 26/10/2015
	; 23/10/2015
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	; 18/11/2015
	mov	ebx, cr3
	push	ebx ; ****
	;
	push	ecx ; ***
	push	edx ; **
	;
	mov	ebx, KDATA
	mov	ds, bx
	mov	es, bx
	;
	mov	ecx, [k_page_dir]
	mov	cr3, ecx
	; 20/11/2015
	; Interrupt identification register
	mov	dx, 2FAh ; COM2
	;
	cmp 	al, 8 
	ja 	short com_i0
	;
	; 08/01/2022 (Retro UNIX 386 v1.1)
	; 20/11/2015
	; 17/11/2015
	; 16/11/2015
	; 15/11/2015
	; 24/10/2015
	; 14/03/2015 (Retro UNIX 386 v1 - Beginning)
	; 28/07/2014 (Retro UNIX 8086 v1)
	; < serial port 1 interrupt handler >
	;
	inc	dh ; 3FAh ; COM1 Interrupt id. register
com_i0:
	;push	eax ; *
	; 07/11/2015
	mov 	byte [ccomport], al
	; 09/11/2015
	;movzx	ebx, ax ; 8 or 9
	; 08/01/2022
	mov	ebx, eax ; 8 or 9
	; 17/11/2015
 	; reset request for response status
	mov	[ebx+req_resp-8], ah ; 0
	;
	; 20/11/2015
	in	al, dx		; read interrupt id. register
	JMP	$+2	   	; I/O DELAY
	and	al, 4		; received data available?	
	jz	short com_eoi	; (transmit. holding reg. empty)
	;
	; 20/11/2015
	sub	dl, 3FAh-3F8h	; data register (3F8h, 2F8h)
	in	al, dx     	; read character
	;JMP	$+2	   	; I/O DELAY
	; 08/11/2015
	; 07/11/2015
	mov	esi, ebx 
	mov	edi, ebx
	add 	esi, rchar - 8 ; points to last received char
	add	edi, schar - 8 ; points to last sent char
	mov	[esi], al ; received char (current char)
	; query
	and	al, al
	jnz	short com_i2
   	; response
	; 17/11/2015
	; set request for response status
        inc     byte [ebx+req_resp-8] ; 1 
	;
	add	dx, 3FDh-3F8h	; (3FDh, 2FDh)
	in	al, dx	   	; read line status register 
	JMP	$+2	   	; I/O DELAY
	and	al, 20h	   	; transmitter holding reg. empty?
	jz	short com_eoi 	; no
	mov 	al, 0FFh   	; response			
	sub	dx, 3FDh-3F8h 	; data port (3F8h, 2F8h)
	out	dx, al	   	; send on serial port
	; 17/11/2015
	cmp 	byte [edi], 0   ; query ? (schar)
	jne 	short com_i1    ; no
	mov	[edi], al 	; 0FFh (responded)
com_i1:
	; 17/11/2015
	; reset request for response status (again)
        dec     byte [ebx+req_resp-8] ; 0 
	jmp	short com_eoi
com_i2:	
	; 08/11/2015
	cmp 	al, 0FFh	; (response ?)
	je	short com_i3	; (check for response signal)
	; 07/11/2015
	cmp	al, 04h	; EOT
	jne	short com_i4	
	; EOT = 04h (End of Transmit) - 'CTRL + D'
	;(an EOT char is supposed as a ctrl+brk from the terminal)
	; 08/11/2015
		; ptty -> tty 0 to 7 (pseudo screens)
	xchg	bl, [ptty]  ; tty number (8 or 9)
	call 	ctrlbrk
	xchg	[ptty], bl ; (restore ptty value and BL value)
	;mov	al, 04h ; EOT
	; 08/11/2015
	jmp	short com_i4	
com_i3:
	; 08/11/2015
	; If 0FFh has been received just after a query
	; (schar, ZERO), it is a response signal.
	; 17/11/2015
        cmp     byte [edi], 0 ; query ? (schar)
	ja	short com_i4 ; no
	; reset query status (schar)
	mov	[edi], al ; 0FFh
	inc	al ; 0
com_i4:
	; 27/07/2014
	; 09/07/2014
	shl	bl, 1	
	add	ebx, ttychr
	; 23/07/2014 (always overwrite)
	;;cmp	word [ebx], 0
	;;ja	short com_eoi
	;
	mov	[ebx], ax   ; Save ascii code
			    ; scan code = 0
com_eoi:
	;mov	al, 20h
	;out	20h, al	   ; end of interrupt
	;
	; 07/11/2015
      	;pop	eax ; *
	mov	al, byte [ccomport] ; current COM port
	; al = tty number (8 or 9)
        call	wakeup
com_iret:
	; 23/10/2015
	pop	edx ; **
	pop	ecx ; ***
	; 18/11/2015
	;pop	eax ; ****
	;mov	cr3, eax
	;jmp	iiret
	jmp	iiretp

;iiretp: ; 01/09/2015
;	; 28/08/2015
;	pop	eax ; (*) page directory
;	mov	cr3, eax
;iiret:
;	; 22/08/2014
;	mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
;	out	20h, al	; 8259 PORT
;	;
;	pop	es
;	pop	ds
;	pop	edi
;	pop	esi
;	pop	ebx ; 29/08/2014
;	pop 	eax
;	iretd

sp_init:
	; 08/01/2022 (Retro UNIX 386 v1.2)
	; 07/11/2015
	; 29/10/2015
	; 26/10/2015
	; 23/10/2015
	; 29/06/2015
	; 14/03/2015 (Retro UNIX 386 v1 - 115200 baud)
	; 28/07/2014 (Retro UNIX 8086 v1 - 9600 baud)
	; Initialization of Serial Port Communication Parameters
	; (COM1 base port address = 3F8h, COM1 Interrupt = IRQ 4)
	; (COM2 base port address = 2F8h, COM1 Interrupt = IRQ 3)
	;
	; ((Modified registers: EAX, ECX, EDX, EBX))
	;
	; INPUT:  (29/06/2015)
	;	AL = 0 for COM1
	;	     1 for COM2
	;	AH = Communication parameters	
	;
	;  (*) Communication parameters (except BAUD RATE):
	;	Bit	4	3	2	1	0
	;		-PARITY--   STOP BIT  -WORD LENGTH-	 		 
	;  this one -->	00 = none    0 = 1 bit  11 = 8 bits
	;		01 = odd     1 = 2 bits	10 = 7 bits
	;		11 = even
	;  Baud rate setting bits: (29/06/2015)
	;		Retro UNIX 386 v1 feature only !
	;	Bit	7    6    5  | Baud rate
	;		------------------------
	;	value	0    0    0  | Default (Divisor = 1)
	;		0    0    1  | 9600 (12)
	;		0    1    0  | 19200 (6) 
	;		0    1	  1  | 38400 (3) 
	;		1    0	  0  | 14400 (8)
	;		1    0	  1  | 28800 (4)
	;		1    1    0  | 57600 (2)
	;		1    1    1  | 115200 (1) 	
	
	; References:	
	; (1) IBM PC-XT Model 286 BIOS Source Code
	;     RS232.ASM --- 10/06/1985 COMMUNICATIONS BIOS (RS232)
	; (2) Award BIOS 1999 - ATORGS.ASM
	; (3) http://wiki.osdev.org/Serial_Ports
	;
	; Set communication parameters for COM1 (= 03h)	
	;
	mov	ebx, com1p		; COM1 parameters  
	mov	dx, 3F8h		; COM1
	; 29/10/2015
	mov	cx, 301h  ; divisor = 1 (115200 baud)
	call	sp_i3	; call A4	
	test	al, 80h
	jz	short sp_i0 ; OK..
		; Error !
	;mov	dx, 3F8h
	sub	dl, 5 ; 3FDh -> 3F8h
	;mov	cx, 30Eh  ; divisor = 12 (9600 baud)
	; 08/01/2022
	mov	cl, 0Eh ; cx = 30Eh, divisor = 12 (9600 baud)
	call	sp_i3	; call A4	
	test	al, 80h
	jnz	short sp_i1
sp_i0:
        ; (Note: Serial port interrupts will be disabled here...)	
        ; (INT 14h initialization code disables interrupts.)
	;
	mov	byte [ebx], 0E3h ; 11100011b
	call	sp_i5 ; 29/06/2015
sp_i1:
	inc	ebx
	mov	dx, 2F8h		; COM2
	; 29/10/2015
	;mov	cx, 301h  ; divisor = 1 (115200 baud)
	; 08/01/2022
	mov	cl, 01h ; cx = 301h, divisor = 1 (115200 baud)
	call	sp_i3	; call A4	
	test	al, 80h
	jz	short sp_i2 ; OK..
		; Error !
	;mov	dx, 2F8h
	sub	dl, 5 ; 2FDh -> 2F8h
	;mov	cx, 30Eh  ; divisor = 12 (9600 baud)
	; 08/01/2022
	mov	cl, 0Eh ; cx = 30Eh, divisor = 12 (9600 baud)
	call	sp_i3	; call A4	
	test	al, 80h
	jnz	short sp_i7
sp_i2:
	mov	byte [ebx], 0E3h ; 11100011b
sp_i6:
	;; COM2 - enabling IRQ 3
	; 08/01/2022
	mov	ah, 0F7h ; enable IRQ 3 (COM2)
	; 07/11/2015
	; 26/10/2015
	;pushf
	;cli
	;;
	;;mov	dx, 2FCh   		; modem control register
	;mov	dl, 0FCh ; 08/01/2022
	;in	al, dx 	   		; read register
	;JMP	$+2	   		; I/O DELAY
	;or	al, 8      		; enable bit 3 (OUT2)
	;out	dx, al     		; write back to register
	;JMP	$+2	   		; I/O DELAY
	;;mov	dx, 2F9h   		; interrupt enable register
	;mov	dl, 0F9h ; 08/01/2022
	;in	al, dx     		; read register
	;JMP	$+2	   		; I/O DELAY
	;;or	al, 1      		; receiver data interrupt enable and
	;or	al, 3	   		; transmitter empty interrupt enable
	;out	dx, al 	   		; write back to register
	;JMP	$+2        		; I/O DELAY
	;in	al, 21h    		; read interrupt mask register
	;JMP	$+2	   		; I/O DELAY
	;and	al, 0F7h   		; enable IRQ 3 (COM2)
	;out	21h, al    		; write back to register
	;
	; 08/01/2022
	pushf
	call	sp_i8
	; 23/10/2015
	mov 	eax, com2_int
	mov	[com2_irq3], eax
	; 26/10/2015
	popf	
sp_i7:
	retn

sp_i3:
;A4:  	;-----	INITIALIZE THE COMMUNICATIONS PORT
	; 28/10/2015
	inc	dl	; 3F9h (2F9h)	; 3F9h, COM1 Interrupt enable register 
	mov	al, 0
	out	dx, al			; disable serial port interrupt
	JMP	$+2			; I/O DELAY
	add	dl, 2 	; 3FBh (2FBh)	; COM1 Line control register (3FBh)
	mov	al, 80h			
	out	dx, al			; SET DLAB=1 ; divisor latch access bit
	;-----	SET BAUD RATE DIVISOR
	; 26/10/2015
	sub 	dl, 3   ; 3F8h (2F8h)	; register for least significant byte
					; of the divisor value
	mov	al, cl	; 1
	out	dx, al			; 1 = 115200 baud (Retro UNIX 386 v1)
					; 2 = 57600 baud
					; 3 = 38400 baud
					; 6 = 19200 baud
					; 12 = 9600 baud (Retro UNIX 8086 v1)
	JMP	$+2			; I/O DELAY
	sub	al, al
	inc	dl      ; 3F9h (2F9h)	; register for most significant byte
					; of the divisor value
	out	dx, al ; 0
	JMP	$+2			; I/O DELAY
	;	
	mov	al, ch ; 3		; 8 data bits, 1 stop bit, no parity
	;and	al, 1Fh ; Bits 0,1,2,3,4	
	add	dl, 2	; 3FBh (2FBh)	; Line control register
	out	dx, al			
	JMP	$+2			; I/O DELAY
	; 29/10/2015
	dec 	dl 	; 3FAh (2FAh)	; FIFO Control register (16550/16750)
	xor	al, al			; 0
	out	dx, al			; Disable FIFOs (reset to 8250 mode)
	JMP	$+2	
sp_i4:
;A18:	;-----	COMM PORT STATUS ROUTINE
	; 29/06/2015 (line status after modem status)
	add	dl, 4	; 3FEh (2FEh)	; Modem status register
sp_i4s:
	in	al, dx			; GET MODEM CONTROL STATUS
	JMP	$+2			; I/O DELAY
	mov	ah, al			; PUT IN (AH) FOR RETURN
	dec	dl	; 3FDh (2FDh)	; POINT TO LINE STATUS REGISTER
					; dx = 3FDh for COM1, 2FDh for COM2
	in	al, dx			; GET LINE CONTROL STATUS
	; AL = Line status, AH = Modem status
	retn

sp_status:
	; 29/06/2015
	; 27/06/2015 (Retro UNIX 386 v1)
	; Get serial port status
	mov	dx, 3FEh		; Modem status register (COM1)
	sub	dh, al			; dh = 2 for COM2 (al = 1)
					; dx = 2FEh for COM2
	jmp	short sp_i4s

sp_setp: ; Set serial port communication parameters
	; 08/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 07/11/2015
	; 29/10/2015
	; 29/06/2015
	; Retro UNIX 386 v1 feature only !	
	;
	; INPUT:
	;	AL = 0 for COM1
	;	     1 for COM2
	;	AH = Communication parameters (*)
	; OUTPUT:
	;	CL = Line status
	;	CH = Modem status
	;   If cf = 1 -> Error code in [u.error]
	;		 'invalid parameter !' 
	;		 	 or
	;		 'device not ready !' error
	;	
	;  (*) Communication parameters (except BAUD RATE):
	;	Bit	4	3	2	1	0
	;		-PARITY--   STOP BIT  -WORD LENGTH-	 		 
	;  this one -->	00 = none    0 = 1 bit  11 = 8 bits
	;		01 = odd     1 = 2 bits	10 = 7 bits
	;		11 = even
	;  Baud rate setting bits: (29/06/2015)
	;		Retro UNIX 386 v1 feature only !
	;	Bit	7    6    5  | Baud rate
	;		------------------------
	;	value	0    0    0  | Default (Divisor = 1)
	;		0    0    1  | 9600 (12)
	;		0    1    0  | 19200 (6) 
	;		0    1	  1  | 38400 (3) 
	;		1    0	  0  | 14400 (8)
	;		1    0	  1  | 28800 (4)
	;		1    1    0  | 57600 (2)
	;		1    1    1  | 115200 (1) 
	;
	; (COM1 base port address = 3F8h, COM1 Interrupt = IRQ 4)
	; (COM2 base port address = 2F8h, COM1 Interrupt = IRQ 3)
	;
	; ((Modified registers: EAX, ECX, EDX, EBX))
	;
	mov	dx, 3F8h
	mov	ebx, com1p ; COM1 control byte offset
	cmp	al, 1
	ja 	short sp_invp_err
	jb	short sp_setp1 ;  COM1 (AL = 0)
	dec	dh ; 2F8h
	inc	ebx ; COM2 control byte offset
sp_setp1:
	; 29/10/2015
	mov	[ebx], ah
	movzx 	ecx, ah
	shr	cl, 5 ; -> baud rate index
	and	ah, 1Fh ; communication parameters except baud rate
	mov	al, [ecx+b_div_tbl]
	mov	cx, ax
	call	sp_i3
	mov	cx, ax ; CL = Line status, CH = Modem status
	test	al, 80h
	jz	short sp_setp2
        mov     byte [ebx], 0E3h ; Reset to initial value (11100011b)
stp_dnr_err:
	mov	dword [u.error], ERR_DEV_NOT_RDY ; 'device not ready !'
	; CL = Line status, CH = Modem status
	stc
	retn
sp_setp2:
	cmp	dh, 2 ; COM2 (2F?h)
        ;jna	sp_i6
		      ; COM1 (3F?h)
	; 24/12/2021
	ja	short sp_i5
	jmp	sp_i6
sp_i5: 
	; 08/01/2022
	mov	ah, 0EFh ; enable IRQ 4 (COM1)
	; 07/11/2015
	; 26/10/2015
	; 29/06/2015
	;
	;; COM1 - enabling IRQ 4
	;pushf
	;cli
	;;mov	dx, 3FCh   		; modem control register
	;mov	dl, 0FCh ; 08/01/2022
	;in	al, dx 	   		; read register
	;JMP	$+2			; I/O DELAY
	;or	al, 8      		; enable bit 3 (OUT2)
	;out	dx, al     		; write back to register
	;JMP	$+2			; I/O DELAY
	;;mov	dx, 3F9h   		; interrupt enable register
	;mov	dl, 0F9h ; 08/01/2022
	;in	al, dx     		; read register
	;JMP	$+2			; I/O DELAY
	;;or	al, 1      		; receiver data interrupt enable and
	;or	al, 3	   		; transmitter empty interrupt enable
	;out	dx, al 	   		; write back to register
	;JMP	$+2        		; I/O DELAY
	;in	al, 21h    		; read interrupt mask register
	;JMP	$+2			; I/O DELAY
	;and	al, 0EFh   		; enable IRQ 4 (COM1)
	;out	21h, al    		; write back to register
	;
	; 08/01/2022
	pushf
	call	sp_i8
	; 23/10/2015
	mov 	eax, com1_int
	mov	[com1_irq4], eax
	; 26/10/2015
	popf
	retn

sp_i8:
	; 08/01/2022
	;pushf
	cli
	;
	;mov	dx, 2FCh  ; 3FCh	; modem control register
	mov	dl, 0FCh
	in	al, dx 	   		; read register
	JMP	$+2	   		; I/O DELAY
	or	al, 8      		; enable bit 3 (OUT2)
	out	dx, al     		; write back to register
	JMP	$+2	   		; I/O DELAY
	;mov	dx, 2F9h  ; 3F9h 	; interrupt enable register
	mov	dl, 0F9h
	in	al, dx     		; read register
	JMP	$+2	   		; I/O DELAY
	;or	al, 1      		; receiver data interrupt enable and
	or	al, 3	   		; transmitter empty interrupt enable
	out	dx, al 	   		; write back to register
	JMP	$+2        		; I/O DELAY
	in	al, 21h    		; read interrupt mask register
	JMP	$+2	   		; I/O DELAY
	;and	al, 0F7h  ; 0EFh	; enable IRQ 3 (COM2)
	and	al, ah	; 0F7h or 0EFh 
	out	21h, al    		; write back to register
	;
	;popf	
	retn

sp_invp_err:
	mov	dword [u.error], ERR_INV_PARAMETER ; 'invalid parameter !' 
	xor	ecx, ecx
	dec	ecx ; 0FFFFh
	stc
	retn

; 29/10/2015
b_div_tbl: ; Baud rate divisor table (115200/divisor)
	db	1, 12, 6, 3, 8, 4, 1

; Retro UNIX 8086 v1 - UNIX.ASM (01/09/2014) 
epoch:
	; 04/12/2021 (Retro UNIX 386 v1.2)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 09/04/2013 (Retro UNIX 8086 v1 - UNIX.ASM)
	; 'epoch' procedure prototype: 
	; 	            UNIXCOPY.ASM, 10/03/2013
	; 14/11/2012
	; unixboot.asm (boot file configuration)
	; version of "epoch" procedure in "unixproc.asm"
	; 21/7/2012
	; 15/7/2012
	; 14/7/2012		
	; Erdogan Tan - RETRO UNIX v0.1
	; compute current date and time as UNIX Epoch/Time
	; UNIX Epoch: seconds since 1/1/1970 00:00:00
	;
        ;  ((Modified registers: EAX, EDX, ECX, EBX))  
	;
	call 	get_rtc_time		; Return Current Time
        xchg 	ch,cl
        mov 	[hour], cx
        xchg 	dh,dl
        mov 	[second], dx
	;
        call 	get_rtc_date		; Return Current Date
        xchg 	ch,cl
        mov 	[year], cx
        xchg 	dh,dl
        mov 	[month], dx
	;
	mov 	cx, 3030h
	;
	mov 	al, [hour] ; Hour
        	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[hour], al
	mov 	al, [hour+1] ; Minute
        	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[minute], al
	mov 	al, [second] ; Second
        	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[second], al
	mov 	ax, [year] ; Year (century)
	;push 	ax
	; 04/12/2021
	push	eax
	   	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	ah, 100
	mul 	ah
	mov 	[year], ax
	;pop	ax
	; 04/12/2021
	pop	eax
	mov	al, ah
        	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	add 	[year], ax
	mov 	al, [month] ; Month
           	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[month], al	
        mov     al, [month+1]      	; Day
           	; AL <= BCD number)
        db 	0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
        mov     [day], al
	
_epoch:
	; 17/07/2022
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit modification)
	; 09/04/2013 (Retro UNIX 8086 v1)
	;
	; ((Modified registers: EAX, EDX, EBX)) 
	;
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	sub 	eax, eax
	mov 	ax, [year]
	sub 	ax, 1970
	mov 	edx, 365
	mul 	edx
	xor 	ebx, ebx
	mov 	bl, [month]
	dec 	bl
	shl 	bl, 1
	;sub	edx, edx
	mov 	dx, [EBX+DMonth]
        mov     bl, [day]
	dec 	bl
	add 	eax, edx
	add 	eax, ebx
			; EAX = days since 1/1/1970
	mov 	dx, [year]
	sub 	dx, 1969
	;shr 	dx, 1
	;shr 	dx, 1		
	; 17/07/2022
	shr	edx, 2
		; (year-1969)/4
	add 	eax, edx
			; + leap days since 1/1/1970
	cmp 	byte [month], 2	; if past february
	jna 	short cte1
	mov 	dx, [year]
	and 	dx, 3 ; year mod 4
	jnz 	short cte1		
			; and if leap year
	add 	eax, 1 	; add this year's leap day (february 29)
cte1: 			; compute seconds since 1/1/1970
	mov 	edx, 24
	mul	edx
	mov 	dl, [hour]
	add 	eax, edx
		; EAX = hours since 1/1/1970 00:00:00
	;mov	ebx, 60
	mov	bl, 60
	mul	ebx
	mov 	dl, [minute]
	add 	eax, edx
		; EAX = minutes since 1/1/1970 00:00:00
	;mov 	ebx, 60
	mul	ebx
	mov 	dl, [second]
	add 	eax, edx
 		; EAX -> seconds since 1/1/1970 00:00:00
	retn

get_rtc_time:
	; 15/03/2015
	; Derived from IBM PC-XT Model 286 BIOS Source Code
	; BIOS2.ASM ---- 10/06/1985 BIOS INTERRUPT ROUTINES
	; INT 1Ah
	; (AH) = 02H  READ THE REAL TIME CLOCK AND RETURN WITH,	:
	;       (CH) = HOURS IN BCD (00-23)			:
	;       (CL) = MINUTES IN BCD (00-59)			:
	;       (DH) = SECONDS IN BCD (00-59)			:
	;       (DL) = DAYLIGHT SAVINGS ENABLE (00-01).		:
	;								
RTC_20: 				; GET RTC TIME
	cli
	CALL	UPD_IPR 		; CHECK FOR UPDATE IN PROCESS
	JC	short RTC_29		; EXIT IF ERROR (CY= 1)

	MOV	AL, CMOS_SECONDS 	; SET ADDRESS OF SECONDS
	CALL	CMOS_READ		; GET SECONDS
	MOV	DH, AL			; SAVE
	MOV	AL, CMOS_REG_B		; ADDRESS ALARM REGISTER
	CALL	CMOS_READ		; READ CURRENT VALUE OF DSE BIT
	AND	AL, 00000001B		; MASK FOR VALID DSE BIT
	MOV	DL, AL			; SET [DL] TO ZERO FOR NO DSE BIT
	MOV	AL, CMOS_MINUTES 	; SET ADDRESS OF MINUTES
	CALL	CMOS_READ		; GET MINUTES
	MOV	CL, AL			; SAVE
	MOV	AL, CMOS_HOURS		; SET ADDRESS OF HOURS
	CALL	CMOS_READ		; GET HOURS
	MOV	CH, AL			; SAVE
	CLC				; SET CY= 0
RTC_29:
	sti
	RETn				; RETURN WITH RESULT IN CARRY FLAG

get_rtc_date:
	; 15/03/2015
	; Derived from IBM PC-XT Model 286 BIOS Source Code
	; BIOS2.ASM ---- 10/06/1985 BIOS INTERRUPT ROUTINES
	; INT 1Ah
	; (AH) = 04H  READ THE DATE FROM THE REAL TIME CLOCK AND RETURN WITH,:
	;      (CH) = CENTURY IN BCD (19 OR 20) 		       :
	;      (CL) = YEAR IN BCD (00-99)			       :
	;      (DH) = MONTH IN BCD (01-12)			       :
	;      (DL) = DAY IN BCD (01-31).		
	;
RTC_40: 				; GET RTC DATE
	cli
	CALL	UPD_IPR 		; CHECK FOR UPDATE IN PROCESS
	JC	short RTC_49		; EXIT IF ERROR (CY= 1)

	MOV	AL, CMOS_DAY_MONTH	; ADDRESS DAY OF MONTH
	CALL	CMOS_READ		; READ DAY OF MONTH
	MOV	DL, AL			; SAVE
	MOV	AL, CMOS_MONTH		; ADDRESS MONTH
	CALL	CMOS_READ		; READ MONTH
	MOV	DH, AL			; SAVE
	MOV	AL, CMOS_YEAR		; ADDRESS YEAR
	CALL	CMOS_READ		; READ YEAR
	MOV	CL, AL			; SAVE
	MOV	AL, CMOS_CENTURY 	; ADDRESS CENTURY LOCATION
	CALL	CMOS_READ		; GET CENTURY BYTE
	MOV	CH, AL			; SAVE
	CLC				; SET CY=0
RTC_49:
	sti
	RETn				; RETURN WITH RESULTS IN CARRY FLAG

set_date_time:
convert_from_epoch:
	; 02/06/2022 (BugFix)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 20/06/2013 (Retro UNIX 8086 v1)
	; 'convert_from_epoch' procedure prototype: 
	; 	            UNIXCOPY.ASM, 10/03/2013
	;
	; ((Modified registers: EAX, EDX, ECX, EBX))	
	;
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	;
	; INPUT:
	; EAX = Unix (Epoch) Time
	;
	xor 	edx, edx
	; 02/06/2022
	xor	ecx, ecx
	xor	ebx, ebx
	;mov 	ecx, 60
	mov	cl, 60
	div	ecx
	;mov 	[imin], eax  ; whole minutes
			     ; since 1/1/1970
	;mov 	[second], dx ; leftover seconds
	mov	[second], dl ; 02/06/2022
	sub 	edx, edx
	div	ecx
	;mov 	[ihrs], eax  ; whole hours
	;		     ; since 1/1/1970
	;mov 	[minute], dx ; leftover minutes
	mov	[minute], dl ; 02/06/2022
	xor	edx, edx
	;mov 	cx, 24
	mov 	cl, 24
	div	ecx
	;mov 	[iday], ax   ; whole days
			     ; since 1/1/1970
	;mov 	[hour], dx   ; leftover hours
	mov	[hour], dl   ; 02/06/2022

	add 	eax, 365+366 ; whole day since
			     ; 1/1/1968 	
		;mov 	[iday], ax
	push 	eax
	sub	edx, edx
	mov 	ecx, (4*365)+1 ; 4 years = 1461 days
	div	ecx
	pop 	ecx
	;mov 	[lday], ax   ; count of quadyrs (4 years)
	;push	dx
	; 02/06/2022
	push 	edx
	;mov 	[qday], dx   ; days since quadyr began
	cmp 	dx, 31+29    ; if past feb 29 then
	cmc		     ; add this quadyr's leap day
	adc 	eax, 0	     ; to # of qadyrs (leap days)
	;mov 	[lday], ax   ; since 1968			  
	;mov 	cx, [iday]
	xchg 	ecx, eax     ; ECX = lday, EAX = iday		  
	sub 	eax, ecx     ; iday - lday
	mov 	ecx, 365
	xor	edx, edx
	; EAX = iday-lday, EDX = 0
	div	ecx
	;mov 	[iyrs], ax   ; whole years since 1968
	;jday = iday - (iyrs*365) - lday
	;mov	[jday], dx   ; days since 1/1 of current year
	;add	eax, 1968
	add 	ax, 1968     ; compute year
	mov 	[year], ax
	;mov 	cx, dx
	; 02/06/2022
	mov	ecx, edx
	;mov 	dx, [qday]
	;pop	dx
	; 02/06/2022
	pop 	edx
	cmp 	dx, 365	     ; if qday <= 365 and qday >= 60	
	ja 	short cfe1   ; jday = jday +1
	cmp 	dx, 60       ; if past 2/29 and leap year then
        cmc		     ; add a leap day to the # of whole
	adc 	cx, 0        ; days since 1/1 of current year
cfe1:			
	;mov 	[jday], cx
	;mov 	bx, 12       ; estimate month
	;sub	ebx, ebx
	; 02/06/2022
	mov	bl, 12
	mov 	dx, 366      ; mday, max. days since 1/1 is 365
	and 	ax, 11b      ; year mod 4 (and dx, 3) 
cfe2:	; Month calculation  ; 0 to 11  (11 to 0)	
	;cmp 	cx, dx       ; mday = # of days passed from 1/1
	; 02/06/2022
	cmp	ecx, edx 	 		
	jnb 	short cfe3
	;dec 	bx           ; month = month - 1
	dec	bl			
	;shl 	bx, 1
	shl	bl, 1
	mov 	dx, [ebx+DMonth] ; # elapsed days at 1st of month
	;shr 	bx, 1        ; bx = month - 1 (0 to 11)
	; 02/06/2022
	shr	bl, 1
	cmp	bl, 1
	;cmp	bx, 1        ; if month > 2 and year mod 4  = 0	
	jna 	short cfe2   ; then mday = mday + 1
	or 	al, al       ; if past 2/29 and leap year then
	jnz 	short cfe2   ; add leap day (to mday)
	;inc 	dx           ; mday = mday + 1
	inc	edx
	jmp 	short cfe2
cfe3:
	;inc 	bx	     ; -> bx = month, 1 to 12
	; 02/06/2022
	inc	bl
	;mov 	[month], bx
	mov	[month], bl
	;sub 	cx, dx	     ; day = jday - mday + 1	
	sub	ecx, edx
	;inc 	cx 			  
	inc	cl
	;mov 	[day], cx
	mov	[day], cl    ; 02/06/2022

	; eax, ebx, ecx, edx is changed at return
	; output ->
	; [year], [month], [day], [hour], [minute], [second]
	
	; 02/06/2022 (BugFix)	
_set_date:
	mov	ax, [year]
	mov	ch, 20h ; century (bcd)
	sub	ax, 2000
	jnc	short set_date
	mov	ch, 19h ; century (bcd) 
	add	ax, 100	
	; 02/06/2022
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 20/06/2013 (Retro UNIX 8086 v1)
set_date:
        ;mov	al, [year+1]
	;aam 	; ah = al / 10, al = al mod 10
	;db 	0D5h, 10h    ; Undocumented inst. AAD
	;		     ; AL = AH * 10h + AL
	;mov 	ch, al ; century (BCD)
	;mov 	al, [year]
	; al = year (0-99) ; 01/06/2022
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	cl, al ; year (BCD)
        mov 	al, [month]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	dh, al ; month (BCD)
	mov 	al, [day]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	; 02/06/2022 (BugFix)
	mov 	dl, al ; day (BCD)

	; Set real-time clock date
	call	set_rtc_date
set_time:
        ; Read real-time clock time 
	; (get day light saving time bit status)
 	cli
	CALL	UPD_IPR 	; CHECK FOR UPDATE IN PROCESS
	; cf = 1 -> al = 0
        jc      short stime1
	MOV	AL, CMOS_REG_B	; ADDRESS ALARM REGISTER
	CALL	CMOS_READ	; READ CURRENT VALUE OF DSE BIT
stime1:
	sti
	AND	AL, 00000001B	; MASK FOR VALID DSE BIT
	MOV	DL, AL		; SET [DL] TO ZERO FOR NO DSE BIT
	; DL = 1 or 0 (day light saving time)
	;	
	mov 	al, [hour]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	ch, al ; hour (BCD)
        mov     al, [minute]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	cl, al       ; minute (BCD)
        mov     al, [second]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	dh, al	     ; second (BCD)

	; Set real-time clock time
 	; call	set_rtc_time
set_rtc_time:
	; 15/04/2015 (257, POSTEQU.INC -> H EQU 256, X EQU H+1)
	; 15/03/2015
	; Derived from IBM PC-XT Model 286 BIOS Source Code
	; BIOS2.ASM ---- 10/06/1985 BIOS INTERRUPT ROUTINES
	; INT 1Ah
	; (AH) = 03H  SET THE REAL TIME CLOCK USING,			:
	;      (CH) = HOURS IN BCD (00-23)			       	:
	;      (CL) = MINUTES IN BCD (00-59)			       	:
	;      (DH) = SECONDS IN BCD (00-59)			       	:
	;      (DL) = 01 IF DAYLIGHT SAVINGS ENABLE OPTION, ELSE 00.    :
	;								:
	;  NOTE: (DL)= 00 IF DAYLIGHT SAVINGS TIME ENABLE IS NOT ENABLED. :
	;        (DL)= 01 ENABLES TWO SPECIAL UPDATES THE LAST SUNDAY IN  :
	;         APRIL   (1:59:59 --> 3:00:00 AM) AND THE LAST SUNDAY IN :
	;         OCTOBER (1:59:59 --> 1:00:00 AM) THE FIRST TIME.	  :
	;
RTC_30: 				; SET RTC TIME
	cli
	CALL	UPD_IPR 		; CHECK FOR UPDATE IN PROCESS
	JNC	short RTC_35		; GO AROUND IF CLOCK OPERATING
	CALL	RTC_STA 		; ELSE TRY INITIALIZING CLOCK
RTC_35:
	MOV	AH, DH			; GET TIME BYTE - SECONDS
	MOV	AL, CMOS_SECONDS 	; ADDRESS SECONDS
	CALL	CMOS_WRITE		; UPDATE SECONDS
	MOV	AH, CL			; GET TIME BYTE - MINUTES
	MOV	AL, CMOS_MINUTES 	; ADDRESS MINUTES
	CALL	CMOS_WRITE		; UPDATE MINUTES
	MOV	AH, CH			; GET TIME BYTE - HOURS
	MOV	AL, CMOS_HOURS		; ADDRESS HOURS
	CALL	CMOS_WRITE		; UPDATE ADDRESS
	;MOV	AX, X*CMOS_REG_B 	; ADDRESS ALARM REGISTER
	MOV	AX, 257*CMOS_REG_B 	; 
	CALL	CMOS_READ		; READ CURRENT TIME
	AND	AL, 01100010B		; MASK FOR VALID BIT POSITIONS
	OR	AL, 00000010B		; TURN ON 24 HOUR MODE
	AND	DL, 00000001B		; USE ONLY THE DSE BIT
	OR	AL, DL			; GET DAY LIGHT SAVINGS TIME BIT (OSE)
	XCHG	AH, AL			; PLACE IN WORK REGISTER AND GET ADDRESS
	CALL	CMOS_WRITE		; SET NEW ALARM BITS
	CLC				; SET CY= 0
	sti
	RETn				; RETURN WITH CY= 0

set_rtc_date:
	; 15/04/2015 (257, POSTEQU.INC -> H EQU 256, X EQU H+1)
	; 15/03/2015
	; Derived from IBM PC-XT Model 286 BIOS Source Code
	; BIOS2.ASM ---- 10/06/1985 BIOS INTERRUPT ROUTINES
	; INT 1Ah
	; (AH) = 05H  SET THE DATE INTO THE REAL TIME CLOCK USING, :
	;     (CH) = CENTURY IN BCD (19 OR 20)			   :
	;     (CL) = YEAR IN BCD (00-99)			   :
	;     (DH) = MONTH IN BCD (01-12)			   :
	;     (DL) = DAY IN BCD (01-31).
	;
RTC_50: 				; SET RTC DATE
	cli
	CALL	UPD_IPR 		; CHECK FOR UPDATE IN PROCESS
	JNC	short RTC_55		; GO AROUND IF NO ERROR
	CALL	RTC_STA 		; ELSE INITIALIZE CLOCK
RTC_55:
	MOV	AX, CMOS_DAY_WEEK	; ADDRESS OF DAY OF WEEK BYTE
	CALL	CMOS_WRITE		; LOAD ZEROS TO DAY OF WEEK
	MOV	AH, DL			; GET DAY OF MONTH BYTE
	MOV	AL, CMOS_DAY_MONTH	; ADDRESS DAY OF MONTH BYTE
	CALL	CMOS_WRITE		; WRITE OF DAY OF MONTH REGISTER
	MOV	AH, DH			; GET MONTH
	MOV	AL, CMOS_MONTH		; ADDRESS MONTH BYTE
	CALL	CMOS_WRITE		; WRITE MONTH REGISTER
	MOV	AH, CL			; GET YEAR BYTE
	MOV	AL, CMOS_YEAR		; ADDRESS YEAR REGISTER
	CALL	CMOS_WRITE		; WRITE YEAR REGISTER
	MOV	AH, CH			; GET CENTURY BYTE
	MOV	AL, CMOS_CENTURY 	; ADDRESS CENTURY BYTE
	CALL	CMOS_WRITE		; WRITE CENTURY LOCATION
	;MOV	AX, X*CMOS_REG_B 	; ADDRESS ALARM REGISTER
	MOV	AX, 257*CMOS_REG_B 	; 
	CALL	CMOS_READ		; READ CURRENT SETTINGS
	AND	AL, 07FH 		; CLEAR 'SET BIT'
	XCHG	AH, AL			; MOVE TO WORK REGISTER
	CALL	CMOS_WRITE		; AND START CLOCK UPDATING
	CLC				; SET CY= 0
	sti
	RETn				; RETURN CY=0

	; 15/03/2015
RTC_STA:				; INITIALIZE REAL TIME CLOCK
	mov	ah, 26h
	mov	al, CMOS_REG_A		; ADDRESS REGISTER A AND LOAD DATA MASK
	CALL	CMOS_WRITE		; INITIALIZE STATUS REGISTER A
	mov	ah, 82h
	mov 	al, CMOS_REG_B		; SET "SET BIT" FOR CLOCK INITIALIZATION
	CALL	CMOS_WRITE		; AND 24 HOUR MODE TO REGISTER B
	MOV	AL, CMOS_REG_C		; ADDRESS REGISTER C
	CALL	CMOS_READ		; READ REGISTER C TO INITIALIZE
	MOV	AL, CMOS_REG_D		; ADDRESS REGISTER D
	;CALL	CMOS_READ		; READ REGISTER D TO INITIALIZE
	;RETn
	; 06/02/2022
	jmp	CMOS_READ

	; 15/03/2015
	; IBM PC/XT Model 286 BIOS source code ----- 10/06/85 (test4.asm)
CMOS_WRITE:			; WRITE (AH) TO LOCATION (AL)
	pushf			; SAVE INTERRUPT ENABLE STATUS AND FLAGS
	;push	ax		; SAVE WORK REGISTER VALUES
	rol	al, 1		; MOVE NMI BIT TO LOW POSITION
	stc			; FORCE NMI BIT ON IN CARRY FLAG
	rcr	al, 1		; HIGH BIT ON TO DISABLE NMI - OLD IN CY
	cli			; DISABLE INTERRUPTS
	out	CMOS_PORT, al	; ADDRESS LOCATION AND DISABLE NMI
	mov	al, ah		; GET THE DATA BYTE TO WRITE
	out	CMOS_DATA, al	; PLACE IN REQUESTED CMOS LOCATION
	mov	al, CMOS_SHUT_DOWN*2 ; GET ADDRESS OF DEFAULT LOCATION
	rcr	al, 1		; PUT ORIGINAL NMI MASK BIT INTO ADDRESS
	out	CMOS_PORT, al	; SET DEFAULT TO READ ONLY REGISTER
	nop			; I/O DELAY
	in	al, CMOS_DATA	; OPEN STANDBY LATCH
	;pop	ax		; RESTORE WORK REGISTERS
	popf	
	RETn

bf_init:
	; 21/03/2022 (Retro UNIX 386 v1.2)
	; 28/11/2021
	; 14/08/2015
	; 02/07/2015
	; 01/07/2015
	; 15/04/2015 (Retro UNIX 386 v1 - Beginning)
	; Buffer (pointer) initialization !
	; 
	; 17/07/2013 - 24/07/2013
	; Retro UNIX 8086 v1 (U9.ASM)
	; (Retro UNIX 8086 v1 feature only !)
	;
	mov	edi, bufp 
	mov	eax, buffer + (nbuf*520) 
	sub	edx, edx
	dec	dl
	xor	ecx, ecx
	dec	ecx
bi0:
	sub	eax, 520 ; 8 header + 512 data
	stosd
	mov	esi, eax
	mov	[esi], edx ; 000000FFh
			    ; Not a valid device sign
	mov	[esi+4], ecx ; 0FFFFFFFFh
		      ; Not a valid block number sign 	 	
	cmp	eax, buffer
	ja	short bi0
	mov	eax, sb0
	stosd
	mov	eax, sb1
	stosd
	mov	esi, eax ; offset sb1
	mov	[esi], edx ; 000000FFh
			   ; Not a valid device sign
	mov	[esi+4], ecx ; 0FFFFFFFFh
		      ; Not a valid block number sign 	 
	; 14/08/2015
	;call 	rdev_init
	;retn

; ----- Root file system initialization
	
	; 21/03/2022 - Retro UNIX 386 v1.2
	;	(Retro UNIX -runix- v2 file system) 
	; 28/11/2021
rdev_init: ; root device, super block buffer initialization
	; 14/08/2015
	; Retro UNIX 386 v1 feature only !
	;
	; NOTE: Disk partitions (file systems), logical
	; drive initialization, partition's start sector etc.
	; will be coded here, later in 'ldrv_init'	

	movzx	eax, byte [boot_drv]
rdi_0:
	cmp	al, 80h
	jb	short rdi_1
	sub	al, 7Eh ; 80h = 2 (hd0), 81h = 3 (hd1)
rdi_1:
	mov	[rdev], al
	; 21/03/2022
	;cmp	al, 2
	;jnb	short rdi_2
	or	al, al
	jnz	short rdi_2 ; hard disk boot
	; floppy disk boot
	xor	edx, edx ; device number = 0
			 ; (& fglags = 0)
	; eax = 0 ; boot sector address = 0
	;jmp	short rdi_5
	jmp	short rdi_4
rdi_2:
	; load masterboot sector
	; to get runix v2 partition's boot sector address
	;	
	mov	ebx, mbrbuf ; masterboot buffer header addr
	;mov	[ebx], eax
	mov	[ebx], al
	;sub	al, al
	;; eax = 0
	;mov	[ebx+4], eax ; masterboot sector address
	call	diskio
	jnc	short rdi_3
rdi_err:
	mov	esi, disk_read_err_msg
	jmp	key_to_reboot

rdi_3:
	; [ebx+8] = masterboot buffer (data) address 
	;cmp	word [ebx+8+510], 0AA55h
	;jc	short rdi_err

	lea	esi, [ebx+8] 
	call	runix_p_bs    ; return start sector address of
			      ; retro unix v2 partition in esi
	;;jc	short rdi_err ; 'retn'
	;jnc	short rdi_4 ; Runix v2 partition not found !?
			    ; 21/03/2022	
			    ; ((Here, if cf is 1, that means,
			    ; runix v2 partition not found
			    ; in the MBR partition table.
			    ; But, at least, there is a valid
			    ; runix v2 boot sector on the disk
			    ; which started the kernel.
			    ; So, runix v2 boot sector code
			    ; may -must- be on physical sector 0
			    ; and then the superblock may be
			    ; -must be- on physical sector 1.)) 
	;sub	eax, eax
	; eax = 0
;rdi_4:
	mov	edx, [ebx] ; restore device number in dl
;rdi_5:
rdi_4:
        mov	ebx, sb0 ; super block buffer (header)

	inc	eax	; default sector address of the sb
			; (boot sector address + 1)

	;mov 	[ebx], eax
	;mov	al, 1 ; eax = 1
	;mov	[ebx+4], eax ; super block address on disk
	; 21/03/2022
	mov	[ebx], edx ; device number in DL
			   ; (other bytes -flags- are zero)
	mov	[ebx+4], eax ; superblock address on disk

	;call 	diskio
	;retn
	; 28/11/2021
	;jmp	diskio

	; 21/03/2022
	call	diskio
	jc	short rdi_err

	; 21/03/2022
	; Note: If superblock is defective or SB sector
	;	address is wrong, "ERROR: /etc/init !?"
	;	message will appear after here.
	;	..because /etc/init inode will not be found..
	;	So, i am not writing SB validation
	;	(check) code here.
	;
	; ((Also, if we are here, everything should be normal!))

	retn

disk_read_err_msg:
	; 21/03/2022 - Retro UNIX 386 v1.2
	db 07h, 0Dh, 0Ah
	db "Disk read error ! "
	db 0Dh, 0Ah, 0

	; 21/03/2022 - Retro UNIX 386 v1.2
runix_p_bs:
	; get retro unix v2 partition's boot sector address
	
	; 09/05/2021
	; 19/04/2021 - Retro UNIX 386 v2
	; INPUT:
	;	;;Masterboot buffer at offset mbrbuf
	;	; 21/03/2022
	;	esi = Masterboot buffer (data) address
	; OUTPUT:
	;	;;esi = start sector addr of retro unix v2 fs
	;	; 21/03/2022
	;	eax = start sector address of retro unix v2 fs
	;
	;	cf = 1 -> error, retro unix fs not found
	;		(eax = 0)	
	;
	; Modified registers: esi, eax

	ptBootable       equ 0
	ptBeginHead      equ 1
	ptBeginSector    equ 2
	ptBeginCylinder  equ 3
	ptFileSystemID   equ 4
	ptEndHead        equ 5
	ptEndSector      equ 6
	ptEndCylinder    equ 7
	ptStartSector    equ 8
	ptSectors        equ 12

	FS_RETROUNIX	 equ 71h ; runix v2 partition ID

	;cmp	word [mbrbuf+510], 0AA55h
	cmp	word [esi+510], 0AA55h
	jne	short runix_p_nf

	; partition table entries are at offset 1BEh
	;mov	esi, mbrbuf+1BEh+ptFileSystemID
	add	esi, 1BEh+ptFileSystemID
	lea	eax, [esi+(4*16)] 
runix_p_bs_0:
	cmp	byte [esi], FS_RETROUNIX ; 71h
	je	short runix_p_f ; it is retro unix partition
	add	esi, 16
	;cmp	esi, mbrbuf+1BEh+ptFileSystemID+(4*16) 
	cmp	esi, eax
	jb	short runix_p_bs_0 	  
runix_p_nf:
	sub	eax, eax
	; eax = 0
	stc
	retn
runix_p_f:
	;mov	esi, [esi+ptStartSector-ptFileSystemID]
	mov	eax, [esi+ptStartSector-ptFileSystemID]
	retn

; 23/10/2015
com1_irq4:
	dd dummy_retn
com2_irq3:
	dd dummy_retn

dummy_retn:
	retn