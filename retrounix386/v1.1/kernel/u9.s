; Retro UNIX 386 v1 Kernel (v0.2.1.4) - SYS9.INC
; Last Modification: 13/06/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U9.ASM (01/09/2014) //// UNIX v1 -> u9.s
;
; ****************************************************************************

getch:
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 30/06/2015
	; 18/02/2015 - Retro UNIX 386 v1 - feature only!
	sub	al, al ; 0
getch_q: ; 06/08/2015
	mov 	ah, [ptty] ; active (current) video page
        jmp     short getc_n

getc: 
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 12/11/2015
	; 15/09/2015
	; 01/07/2015
	; 30/06/2015
	; 18/02/2015 (Retro UNIX 386 v1 - Beginning)
	; 13/05/2013 - 04/07/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; 'getc' gets (next) character 
	;	 from requested TTY (keyboard) buffer 
	; INPUTS ->
	;     [u.ttyn] = tty number (0 to 7) (8 is COM1, 9 is COM2)	
	;     AL=0 -> Get (next) character from requested TTY buffer
	;	(Keyboard buffer will point to 
	;			next character at next call)
	;     AL=1 -> Test a key is available in requested TTY buffer
	;	(Keyboard buffer will point to 
	;			current character at next call)
	; OUTPUTS ->
	;     (If AL input is 1) ZF=1 -> 'empty buffer' (no chars)
	;     			 ZF=0 -> AX has (current) character
	;      AL = ascii code
	;      AH = scan code	(AH = line status for COM1 or COM2)	 			
	; 		        (cf=1 -> error code/flags in AH)
	; Original UNIX V1 'getc': 
	;		get a character off character list
	;
	; ((Modified registers: eAX, eBX, eCX, eDX, eSI, eDI))	
	;
	; 30/06/2015 (32 bit modifications)
	; 16/07/2013
	; mov 	[getctty], ah
	;

	mov	ah, [u.ttyn] 	; 28/07/2013
getc_n:
	; 30/06/2015
	or	ah, ah
	jz	short getc0 
	shl	ah, 1
	movzx	ebx, ah
	add	ebx, ttychr
	jmp	short getc1
getc0:
	mov	ebx, ttychr
getc1:
	mov	cx, [ebx] 	; ascii & scan code
				; (by kb_int)	
	or	cx, cx
	jnz	short getc2
	and 	al, al
	jz	short getc_s
	;xor	ax, ax
	; 24/12/2021
	xor	eax, eax
	retn
getc2:	
	and	al, al
	mov	ax, cx
	mov	cx, 0
	jnz	short getc3
getc_sn:
	mov	[ebx], cx ; 0, reset
	cmp	ax, cx  ; zf = 0
getc3:
	retn
getc_s:
	; 12/11/2015
	; 15/09/2015
	; 01/07/2015
	; 30/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 16/07/2013 - 14/02/2014 (Retro UNIX 8086 v1)
	;
	; tty  of the current process is not 
	; current tty (ptty); so, current process only 
	; can use keyboard input when its tty becomes 
	; current tty (ptty).
	; 'sleep' is for preventing an endless lock
	; during this tty input request.
	; (Because, the user is not looking at the video page
	; of the process to undersand there is a keyboard
	; input request.)
	;
	;((Modified registers: eAX, eBX, eCX, eDX, eSI, eDI))
	;
	; 05/10/2013
	; ah = byte ptr [u.ttyn] ; (tty number)
	;
	; 10/10/2013
gcw0:
	mov	cl, 10 ; ch = 0
gcw1:	
	; 12/11/2015
	call intract ; jumps to 'sysexit' if [u.quit] = FFFFh
	; 10/10/2013
	call	idle
	mov	ax, [ebx] 	; ascii & scan code
				; (by kb_int)
	or	ax, ax
;	jnz	short gcw3
	jnz	short gcw2 ; 15/09/2015
	; 30/06/2015
	dec	cl
	jnz	short gcw1
	;
	mov	ah, [u.ttyn] 	; 20/10/2013
;	; 10/12/2013
;	cmp 	ah, [ptty]
;	jne	short gcw2
;	; 14/02/2014
;	cmp	byte [u.uno], 1
;	jna	short gcw0		
;gcw2:
	call	sleep
	;
	; 20/09/2013
	mov	ah, [u.ttyn]
	xor 	al, al
	jmp	short getc_n
;gcw3:
gcw2: 	; 15/09/2015
	; 10/10/2013
	xor	cl, cl
	jmp	short getc_sn

putc:	
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 13/08/2015
	; 30/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/05/2013 - 27/07/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; 'putc' puts a character 
	;	 onto requested (tty) video page or
	;	 serial port
	; INPUTS ->
	;     AL = ascii code of the character
	;     AH = video page (tty) number (0 to 7)
	;			  (8 is COM1, 9 is COM2)	
	; OUTPUTS ->
	;    (If AL input is 1) ZF=1 -> 'empty buffer' (no chars)
	;      			ZF=0 -> AX has (current) character
	;     cf=0 and AH = 0 -> no error
	;     cf=1 and AH > 0 -> error (only for COM1 and COM2)		 			
	; 
	; Original UNIX V1 'putc': 
	;     put a character at the end of character list
	;
	; ((Modified registers: eAX, eBX, eCX, eDX, eSI, eDI))
	;
	cmp	ah, 7
        ;ja	sndc
        ja      short sndc ; 24/12/2024
	; 30/06/2015
	movzx	ebx, ah
	; 13/08/2015
	mov	ah, 07h ; black background, light gray character color
	jmp	write_tty ; 'video.inc'

sndc:   ; <Send character>
	;
	; 12/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 17/11/2015
	; 16/11/2015
	; 11/11/2015
	; 10/11/2015
	; 09/11/2015
	; 08/11/2015
	; 07/11/2015
	; 06/11/2015 (serial4.asm, 'sendchr')	
	; 29/10/2015
	; 30/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/05/2013 - 28/07/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 feature only !
	;
	; ah = [u.ttyn]
	;
	; 30/06/2015
	sub	ah, 8 ; ; 0 = tty8 or 1 = tty9
	; 07/11/2015
	movzx	ebx, ah ; serial port index (0 or 1)
sndc0:
	; 07/11/2015
	call	isintr ; quit (ctrl+break) check
	jz	short sndc1
	call	intract ; quit (ctrl+break) check
	; CPU will jump to 'sysexit' if 'u.quit' = 0FFFFh (yes)
sndc1:
	; 16/11/2015
	;mov	cx, ax	; *** al = character (to be sent)
	; 24/12/2021
	mov	ecx, eax ; *** al = character (to be sent)
sndcx:
	mov	al, [ebx+schar] ; last sent character
	mov	ah, [ebx+rchar] ; last received character
	;
	; 17/11/2015
	; check 'request for response' status
	cmp	byte [ebx+req_resp], 0
	jz	short query
response:
	inc 	byte [comqr] ; query or response status
	mov	al, 0FFh	 
	jmp	short sndc3
query:
	or 	al, al  ; 0 = query (also end of text)
	jnz 	short sndc2 ; normal character
	;cmp 	ah, 0FFh     ; is it responded by terminal ?
	;je	short sndc2  ; yes, already responded
	; 16/11/2015
	; query: request for response (again)
	mov	[ebx+rchar], al ; 0 ; reset
	inc 	byte [comqr] ; query or response status
	jmp	short sndc3
sndc2:
	mov	al, cl 	; *** character (to be sent)
sndc3:
	mov	[ebx+schar], al ; current character (to be sent)
	mov	al, bl ; 0 or 1 (serial port index)
	; 30/06/2015
	call	sp_status ; get serial port status
	; AL = Line status, AH = Modem status
	; 07/11/2015
	test	al, 80h
	jnz	short sndc4
	test	al, 20h	; Transmitter holding register empty ?
	jnz	short sndc5
sndc4: 	; Check line status again
	; 16/11/2015
	;push	cx
	; 24/12/2021
	push	ecx
	;mov	ecx, 6 ; 6*30 micro seconds (~5556 chars/second)
	; 12/01/2022
	xor	ecx, ecx
	mov	cl, 6
	call	WAITF
	;pop	cx
	; 24/12/1021
	pop	ecx
	;
	mov	al, bl ; 0 or 1 (serial port index)
	call	sp_status ; get serial port status
	; 16/11/2015
	; 09/11/2015
	; 08/11/2015
	test	al, 80h	; time out error
        jnz     short sndc7
	test	al, 20h	; Transmitter holding register empty ?
        jz	short sndc7
sndc5:  
	mov	al, [ebx+schar] ; character (to be sent)
	mov	dx, 3F8h   ; data port (COM2)
	sub	dh, bl
	out	dx, al	   ; send on serial port
	; 10/11/2015
	; delay for 3*30 (3*(15..80)) micro seconds
	; (to improve text flow to the terminal)
	; ('diskette.inc': 'WAITF')
	; Uses port 61h, bit 4 to have CPU speed independent waiting.
	; (refresh periods = 1 per 30 microseconds on most machines)
	;push	cx
	; 24/12/2021
	push	ecx
	;mov	ecx, 6 ; 6*30 micro seconds (~5556 chars/second)
	; 12/01/2022
	sub	ecx, ecx
	mov	cl, 6
	call	WAITF
	;pop	cx
	; 24/12/1021
	pop	ecx
    	;
	; 07/11/2015
	mov	al, bl ; al = 0 (tty8) or 1 (tty9)
	;
	call	sp_status ; get serial port status
	; AL = Line status, AH = Modem status
	;
	call	isintr ; quit (ctrl+break) check
	jz	short sndc6
	call	intract ; quit (ctrl+break) check
	; CPU will jump to 'sysexit' if 'u.quit' = 0FFFFh (yes)
sndc6:
	cmp	al, 80h
	jnb	short sndc7		
	;
	cmp	byte [comqr], 1 ; 'query or response' ?
	jb	short sndc8 	; no, normal character
	mov 	byte [comqr], bh ; 0 ; reset
	; 17/11/2015
	call	idle
	;
	cmp	[ebx+schar], bh ; 0 ; query ?
        ;ja	sndc2       ; response (will be followed by
			    ; a normal character)
	; 24/12/2021
	jna	short sndc_10
	jmp	sndc2
sndc_10:
	; Query request must be responded by the terminal
	; before sending a normal character !
	push	ebx
	;push	cx ; *** cl = character (to be sent)
	; 24/12/2021
	push	ecx ; *** cl = character (to be sent)
	mov	ah, [u.ttyn]
	call	sleep ; this process will be awakened by
		      ; received data available interrupt
	;pop	cx ; *** cl = character (to be sent)
	; 24/12/2021
	pop	ecx ; *** cl = character (to be sent) 
	pop	ebx
        jmp	sndcx
sndc7:
	 ; 16/11/2015
	cmp	byte [comqr], 1 ; 'query or response' ?
	jb	short sndc9 	; no
	;
	mov	[ebx+rchar], bh ; 0 ; reset
	mov	[ebx+schar], bh ; 0 ; reset
	;
	mov	byte [comqr], bh ; 0 ; reset  
sndc8:
	cmc  ; jnc -> jc, jb -> jnb
sndc9:
	; AL = Line status, AH = Modem status
	retn

get_cpos:
	; 29/06/2015 (Retro UNIX 386 v1)
	; 04/12/2013 (Retro UNIX 8086 v1 - 'sysgtty')
	;
	; INPUT -> bl = video page number
	; RETURN -> dx = cursor position

	push	ebx
	and	ebx, 0Fh ; 07h ; tty0 to tty7
	shl	bl, 1
	add	ebx, cursor_posn
	mov	dx, [ebx]
	pop	ebx
	retn

read_ac_current:
	; 29/06/2015 (Retro UNIX 386 v1)
	; 04/12/2013 (Retro UNIX 8086 v1 - 'sysgtty')
	;
	; INPUT -> bl = video page number
	; RETURN -> ax = character (al) and attribute (ah)

	call 	find_position ; 'video.inc'
	; dx = status port
	; esi = cursor location/address
	add	esi, 0B8000h	; 30/08/2014 (Retro UNIX 386 v1)
	mov 	ax, [esi]	; get the character and attribute
	retn

syssleep:
	; 29/06/2015 - (Retro UNIX 386 v1)
	; 11/06/2014 - (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 feature only
	; (INPUT -> none)
	;
	movzx	ebx, byte [u.uno] ; process number
	mov	ah, [ebx+p.ttyc-1] ; current/console tty
	call	sleep
	jmp	sysret

	; 27/02/2022
	; (u1.s, 'wttyc' & 'wpc_clr')
%if 0

vp_clr:
	; Reset/Clear Video Page
	;
	; 24/12/2021 - (Retro UNIX 386 v1.1)
	; 30/06/2015 - (Retro UNIX 386 v1)
	; 21/05/2013 - 30/10/2013(Retro UNIX 8086 v1) (U0.ASM)
	;
	; Retro UNIX 8086 v1 feature only !
	;
	; INPUTS -> 
	;   BL = video page number	 
	;
	; OUTPUT ->
	;   none
	; ((Modified registers: eAX, BH, eCX, eDX, eSI, eDI))
	;
	; 04/12/2013
	sub	al, al
	; al = 0 (clear video page)
	; bl = video page
	mov	ah, 07h
	; ah = 7 (attribute/color)
	;xor 	cx, cx ; 0, left upper column (cl) & row (cl)
	; 24/12/2021
	xor	ecx, ecx
	mov	dx, 184Fh ; right lower column & row (dl=24, dh=79)
	call	scroll_up
	; bl = video page
	;xor	dx, dx ; 0 (cursor position) 
	; 24/12/2021
	xor	edx, edx 
	jmp 	set_cpos

%endif

sysmsg:
	; 24/12/2021 (Retro UNIX 386 v1.1)
	; 11/11/2015
	; 01/07/2015 - (Retro UNIX 386 v1 feature only!)
	; Print user-application message on user's console tty
	;
	; Input -> EBX = Message address
	;	   ECX = Message length (max. 255)
	;	   DL = Color (IBM PC Rombios color attributes)
	;
	cmp	ecx, MAX_MSG_LEN ; 255
	;ja	sysret ; nothing to do with big message size
	ja	short sysmsg8 ; 24/12/2021
	or	cl, cl
	;jz	sysret
	jz	short sysmsg8 ; 24/12/2021
	and	dl, dl
	jnz	short sysmsg0
	mov	dl, 07h ; default color
		; (black background, light gray character)
sysmsg0:
	mov	[u.base], ebx
	mov	[ccolor], dl ; color attributes
	mov	ebp, esp
	xor	ebx, ebx ; 0
	mov	[u.nread], ebx ; 0
	;
	cmp	[u.kcall], bl ; 0
	ja	short sysmsgk ; Temporary (01/07/2015)
	;
	mov	[u.count], ecx
	inc	ecx ; + 00h ; ASCIIZ
	; 24/12/2021
	; (dword alignment for esp)
	test	cl, 3
	jz	short sysmsg_7
	or	cl, 3
	inc	ecx
sysmsg_7:
	sub	esp, ecx
	mov	edi, esp
	mov	esi, esp
	mov	[u.pcount], bx ; reset page (phy. addr.) counter
	; 11/11/2015
	mov 	ah, [u.ttyp] ; recent open tty
	; 0 = none
	dec	ah
	jns	short sysmsg1 
	mov	bl, [u.uno] ; process number	
	mov	ah, [ebx+p.ttyc-1] ; user's (process's) console tty
sysmsg1:
	mov	[u.ttyn], ah
sysmsg2:
	call	cpass
	jz	short sysmsg5
	stosb
	and	al, al
	jnz	short sysmsg2
sysmsg3:
	cmp	ah, 7 ; tty number
	ja	short sysmsg6 ; serial port
	call	print_cmsg
sysmsg4:
	mov	esp, ebp	
sysmsg8: ; 24/12/2021	
	jmp	sysret
sysmsg5:
	mov	byte [edi], 0
	jmp	short sysmsg3
sysmsg6:
	mov	al, [esi]
	call	sndc
	jc	short sysmsg4
	cmp	byte [esi], 0  ; 0 is stop character
	jna	short sysmsg4
	inc 	esi
	mov	ah, [u.ttyn]
	jmp	short sysmsg6

sysmsgk: ; Temporary (01/07/2015)
	; The message has been sent by Kernel (ASCIIZ string)
	; (ECX -character count- will not be considered)
	mov	esi, [u.base]
	mov	ah, [ptty] ; present/current screen (video page)
	mov	[u.ttyn], ah
	mov	byte [u.kcall], 0
	jmp	short sysmsg3
	
print_cmsg: 
	; 01/07/2015 (retro UNIX 386 v1 feature only !)
	;
	; print message (on user's console tty) 
	;	with requested color
	;
	; INPUTS:
	;	esi = message address
	;	[u.ttyn] = tty number (0 to 7)
	;	[ccolor] = color attributes (IBM PC BIOS colors)
	;
	lodsb
pcmsg1:
	push 	esi
        movzx   ebx, byte [u.ttyn]
	mov	ah, [ccolor]
	call 	write_tty
	pop	esi
	lodsb
	and 	al, al  ; 0
	jnz 	short pcmsg1
	retn

sysgeterr:
	; 16/02/2022 - Retro UNIX 386 v1.1
	; 09/12/2015
	; 21/09/2015 - (Retro UNIX 386 v1 feature only!)
	; Get last error number or page fault count
	; (for debugging)
	;
	; Input -> EBX = return type
	;	   0 = last error code (which is in 'u.error')	
	;	   FFFFFFFFh = page fault count for running process
	;	   FFFFFFFEh = total page fault count
	;	   1 .. FFFFFFFDh = undefined 
	;
	; Output -> EAX = last error number or page fault count
	;	   (depending on EBX input)
	; 	
	and 	ebx, ebx
	jnz	short glerr_2
glerr_0:
	mov	eax, [u.error]
glerr_1:
	mov	[u.r0], eax
 	;retn
	; 16/02/2022 (BugFix)
	jmp	sysret
glerr_2:
	inc	ebx ; FFFFFFFFh -> 0, FFFFFFFEh -> FFFFFFFFh
	jz	short glerr_2 ; page fault count for process
	inc	ebx ; FFFFFFFFh -> 0	
	jnz	short glerr_0
	mov	eax, [PF_Count] ; total page fault count
        jmp     short glerr_1
glerr_3:
	mov 	eax, [u.pfcount]
	jmp	short glerr_1

; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
;	     - PRINTER BIOS (Functions)		

;;; IBM PC-AT BIOS v3 - PRT.ASM - 15/11/1985 ;;; 
;
;--- INT 17 H ------------------------------------------------------------------
; PRINTER_IO								       :
;	THIS ROUTINE PROVIDES COMMUNICATION WITH THE PRINTER		       :
; INPUT 								       :
;	(AH)= 00H  PRINT THE CHARACTER IN (AL)				       :
;		    ON RETURN, (AH)= 1 IF CHARACTER NOT BE PRINTED (TIME OUT)  :
;		    OTHER BITS SET AS ON NORMAL STATUS CALL		       :
;	(AH)= 01H  INITIALIZE THE PRINTER PORT				       :
;		    RETURNS WITH (AH) SET WITH PRINTER STATUS		       :
;	(AH)= 02H  READ THE PRINTER STATUS INTO (AH)			       :
;		   7	   6	   5	   4	   3	   2-1	   0	       :
;		   |	   |	   |	   |	   |	   |	   |_TIME OUT  :
;		   |	   |	   |	   |	   |	   |		       :
;		   |	   |	   |	   |	   |	   |_ UNUSED	       :
;		   |	   |	   |	   |	   |			       :
;		   |	   |	   |	   |	   |_ 1 = I/O ERROR	       :
;		   |	   |	   |	   |				       :
;		   |	   |	   |	   |_ 1 = SELECTED		       :
;		   |	   |	   |					       :
;		   |	   |	   |_ 1 = OUT OF PAPER			       :
;		   |	   |						       :
;		   |	   |_ 1 = ACKNOWLEDGE				       :
;		   |							       :
;		   |_ 1 = NOT BUSY					       :
;									       :
;	(DX) = PRINTER TO BE USED (0,1,2) CORRESPONDING TO ACTUAL VALUES       :
;		IN @PRINTER_BASE AREA					       :
; DATA AREA @PRINTER_BASE CONTAINS THE BASE ADDRESS OF THE PRINTER CARD(S)     :
; AVAILABLE (LOCATED AT BEGINNING OF DATA SEGMENT, 408H ABSOLUTE, 3 WORDS)     :
;									       :
; DATA AREA @PRINT_TIM_OUT (BYTE) MAY BE CHANGED TO CAUSE DIFFERENT	       :
; TIME OUT WAITS. DEFAULT=20 * 4					       :
;									       :
; REGISTERS	(AH) IS MODIFIED WITH STATUS INFORMATION		       :
;		ALL OTHERS UNCHANGED					       :
;-------------------------------------------------------------------------------

int17h:
	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
	; (Derived from: IBM PC-AT BIOS v3 - PRT.ASM - 15/11/1985)
	;
	; (Default printer port: 378h) ; LPT1
	; (Number of printers = 1)
	
	PRINTER_BASE equ 378h ; LPT1
	;PRINT_TIM_OUT equ 4*80*65536 
			; (Ref: IBM PC-AT BIOS v2 PRT.ASM)	 

	PRINT_TIM_OUT equ 36000 ; WAIT_PRN_NBUSY
			; (Ref: AWARD BIOS 1999 ATORGS.ASM)		

	; INPUT:
	;	ah = 0 -> print the character in AL 
	;		 (sys write with write count >0)
	;	ah = 1 -> initialize printer port
	;		 (sys open)	
	;	ah = 2 -> read the printer status 
	;		 (sys write with write count = 0)
	; OUTPUT:
	;	ah = printer status

	; Modified registers: eax, ecx, edx

PRINTER_IO_1:
	or	ah, ah
	jz	short _b20
	dec	ah
	jz	short _b80
	;dec 	ah
	;jz	short _b50
_b50:
	;-----	PRINTER STATUS
B50:
	push	eax		; SAVE (AL) REGISTER
B60:
	mov	dx, PRINTER_BASE+1
				; GET PRINTER ATTACHMENT BASE ADDRESS
				; POINT TO CONTROL PORT
	in	al, dx		; PRE-CHARGE +BUSY LINE IF FLOATING
	in	al, dx		; GET PRINTER STATUS HARDWARE BITS
	mov	ah, al		; SAVE
	and	ah, 0F8h	; TURN OFF UNUSED BITS
B70:
	pop	edx		; RECOVER (AL) REGISTER
	mov	al, dl		; MOVE CHARACTER INTO (AL)
	xor	ah, 48h		; FLIP A COUPLE OF BITS
B10:
	retn			; RETURN FROM ROUTINE WITH STATUS IN AH
_b20:
	;-----	PRINT THE CHARACTER IN (AL)
	mov	ecx, PRINT_TIM_OUT ; (1 second)
B20:
	push	eax		; SAVE VALUE TO PRINT
	mov	dx, PRINTER_BASE
	out	dx, al		; OUTPUT CHARACTER TO DATA PORT
	inc	dl		; POINT TO STATUS PORT

	;-----	CHECK FOR PRINTER BUSY
B25:
	;-----	WAIT BUSY
B35:
	in	al, dx		; GET STATUS
	mov	ah, al		; STATUS TO (AH) ALSO
	test	al, 80h		; IS THE PRINTER CURRENTLY BUSY? (*)
	jnz	short B40	; GO TO OUTPUT STROBE
	call	WAIT_REFRESH	; (wait for 30 micro seconds)
	loop	B35		; LOOP IF YES (*)

	or	ah, 1		; SET ERROR FLAG
	and	ah, 0F9h	; TURN OFF THE UNUSED BITS
	jmp	short B70	; RETURN WITH ERROR FLAG SET

B40:				; SEND STROBE PULSE
	mov	al, 0Dh		; SET THE STROBE LOW (BIT ON)
	inc	dx		; OUTPUT STROBE TO CONTROL PORT
	cli			; PREVENT INTERRUPT PULSE STRETCHING
	out	dx, al		; OUTPUT STROBE BIT > 1us < 5us
	; IODELAY
	;jmp	short $+2	; I/O DELAY TO ALLOW FOR LINE LOADING
	;jmp	short $+2	; AND FOR CORRECT PULSE WIDTH
	; NEWIODELAY
	out	0EBh, al

	mov	al, 0Ch		; SET THE -STROBE HIGH
	out	dx, al
	sti			; INTERRUPTS BACK ON
	;pop	eax		; RECOVER THE OUTPUT CHAR
	;jmp	short B50
	jmp	short B60

_b80:
	;-----	INITIALIZE THE PRINTER PORT
B80:
	push	eax		; SAVE (AL)
	mov	dx, PRINTER_BASE+2 ; POINT TO OUTPUT PORT
	mov	al, 8		; SET INIT LINE LOW
	out	dx, al
	;mov	eax, 1000*4	; ADJUST FOR INITIALIZATION DELAY LOOP
	mov	ecx, WAIT_PRN_INIT ; (65536 micro seconds)
B90:				; INIT_LOOP
	;dec	eax		; LOOP FOR RESET TO TAKE
	;jnz	short B90	; INIT_LOOP
	call	WAIT_REFRESH	; (wait for 30 micro seconds)
	loop	B90	
	mov	al, 0Ch		; NO INTERRUPTS, NON AUTO LF, INIT HIGH
	out	dx, al
	jmp	short B60	; EXIT THROUGH STATUS ROUTINE


; (According to) AWARD BIOS 1999 - ATORGS.ASM (dw -> equ, db -> equ)
; -------------------------------------------------------------------
;
;;Wait while printer initializes should be 65,536 microseconds.
;;65536/30 = 2185
;			PUBLIC	WAIT_PRN_INIT_LO
;WAIT_PRN_INIT_LO	DW	2185
;			PUBLIC	WAIT_PRN_INIT_HI
;WAIT_PRN_INIT_HI	DW	0
;
WAIT_PRN_INIT equ 2185 ; 12/06/2022
;
;;Wait for printer not busy should be 1,080,000 microseconds.
;;Memory refresh =15 us, therefore memory refresh period = 30 Us.
;;1,080,000 / 30 = 36,000
;			PUBLIC	WAIT_PRN_NBUSY_LO
;WAIT_PRN_NBUSY_LO	DW	36000
;			PUBLIC	WAIT_PRN_NBUSY_HI
;WAIT_PRN_NBUSY_HI	DB	0
;
;WAIT_PRN_NBUSY	equ 36000 ; 12/06/2022

; AWARD BIOS - 1999 - ATORGS.ASM (27/5/1999)
; ------------------------------------------
;WAIT_REFRESH:  Uses port 61, bit 4 to have CPU speed independent waiting.
;   	INPUT: BX:CX = number of refresh periods to wait
;     	       (refresh periods = 1 per 30 microseconds on most machines)
;	OUTPUT: BX:CX destroyed.
;
;	SAVES:	AX (except when NO STACK)
;
;	NOTES:	This routine can be (and is) used with no stack. When
;		used this way, AX is assumed to be destroyed.

WAIT_REFRESH:
	; 13/06/2022
	; Modified for Retro UNIX 386 v1.1
	
	; (wait for 30 micro seconds)

	SYS1	equ 61h ; PORT_B

WR_SHORT:
	push	eax
WR_STAT_0:
	in	al, SYS1	; wait for high to low
	test	al, 10h		; transition on memory
	jnz	short WR_STAT_0 
WR_STAT_1:
	in	al, SYS1
	test	al, 10h
	jz	short WR_STAT_1
	pop	eax
	retn
