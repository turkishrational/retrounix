; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2.2.2) - VIDEO.INC
; Last Modification: 14/06/2022
;		  (Video Data is in 'VIDATA.INC')
;
; ///////// VIDEO (CGA) FUNCTIONS ///////////////

; 27/02/2022
; 23/02/2022
; 21/02/2022 (Retro UNIX 386 v1.2)
; 07/02/2022 (Retro UNIX 386 V1&v1.1)
; 02/02/2022 (simplified scroll up)
; 16/01/2016
; 30/06/2015
; 27/06/2015
; 11/03/2015
; 02/09/2014
; 30/08/2014
; VIDEO FUNCTIONS
; (write_tty - Retro UNIX 8086 v1 - U9.ASM, 01/02/2014)

write_tty:
	; 02/02/2022
	; 13/08/2015
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1 - beginning)
	; 01/02/2014 (Retro UNIX 8086 v1 - last update)
	; 03/12/2013 (Retro UNIX 8086 v1 - beginning)	
	; (Modified registers: EAX, EBX, ECX, EDX, ESI, EDI)
	;
	; INPUT -> AH = Color (Forecolor, Backcolor)
	;	   AL = Character to be written
	;	   EBX = Video Page (0 to 7)
	;	   (BH = 0 --> Video Mode 3)

RVRT	equ	00001000b	; VIDEO VERTICAL RETRACE BIT
RHRZ	equ	00000001b	; VIDEO HORIZONTAL RETRACE BIT

; Derived from "WRITE_TTY" procedure of IBM "pc-at" rombios source code
; (06/10/1985), 'video.asm', INT 10H, VIDEO_IO
;
; 06/10/85  VIDEO DISPLAY BIOS
;
;--- WRITE_TTY ------------------------------------------------------------------
;										:
;   THIS INTERFACE PROVIDES A TELETYPE LIKE INTERFACE TO THE			:
;   VIDEO CARDS. THE INPUT CHARACTER IS WRITTEN TO THE CURRENT			:
;   CURSOR POSITION, AND THE CURSOR IS MOVED TO THE NEXT POSITION.		:
;   IF THE CURSOR LEAVES THE LAST COLUMN OF THE FIELD, THE COLUMN		:
;   IS SET TO ZERO, AND THE ROW VALUE IS INCREMENTED. IF THE ROW		:
;   ROW VALUE LEAVES THE FIELD, THE CURSOR IS PLACED ON THE LAST ROW,		:
;   FIRST COLUMN, AND THE ENTIRE SCREEN IS SCROLLED UP ONE LINE.		:
;   WHEN THE SCREEN IS SCROLLED UP, THE ATTRIBUTE FOR FILLING THE		:
;   NEWLY BLANKED LINE IS READ FROM THE CURSOR POSITION ON THE PREVIOUS		:
;   LINE BEFORE THE SCROLL, IN CHARACTER MODE. IN GRAPHICS MODE,		:
;   THE 0 COLOR IS USED.							:
;   ENTRY --									:
;     (AH) = CURRENT CRT MODE							:
;     (AL) = CHARACTER TO BE WRITTEN						:
;	    NOTE THAT BACK SPACE, CARRIAGE RETURN, BELL AND LINE FEED ARE	:
;	    HANDLED AS COMMANDS RATHER THAN AS DISPLAY GRAPHICS CHARACTERS	:
;     (BL) = FOREGROUND COLOR FOR CHAR WRITE IF CURRENTLY IN A GRAPHICS MODE	:
;   EXIT -- 									:
;     ALL REGISTERS SAVED							:
;--------------------------------------------------------------------------------

	cli
	;
	; READ CURSOR (04/12/2013)
	; Retro UNIX 386 v1 Modifications: 30/08/2014
	or	bh, bh
	;jnz	beeper
	; 02/02/2022
	jz	short u14
	jmp	beeper
u14:
	; 02/02/2022
	;; 01/09/2014
	;cmp	byte [CRT_MODE], 3
	;je	short m3
	;;
	;call	set_mode
m3:
	mov 	esi, ebx ; 13/08/2015 (0 to 7)
	;shl	si, 1
	; 02/02/2022
	shl	esi, 1
	add	esi, cursor_posn
	mov	dx, [esi]
	;
	; dx now has the current cursor position
	;
	cmp	al, 0Dh		; is it carriage return or control character
	jbe	short u8
	;
	; write the char to the screen
u0:	
	; ah = attribute/color
	; al = character
	; bl = video page number (0 to 7)
	; bh = 0
	;
	call	write_c_current
	;
	; position the cursor for next char
	inc	dl		; next column
	;cmp	dl, [CRT_COLS]
	cmp	dl, 80		; test for column overflow 
        ;jne	set_cpos
	; 02/02/2022
	je	short u13
	jmp	set_cpos
u13:
	mov	dl, 0		; column = 0
u10:				; (line feed found)
	cmp	dh, 25-1 	; check for last row
	jb 	short u6
	;
	; scroll required
u1:	
	; SET CURSOR POSITION (04/12/2013)
	call	set_cpos
	;
	; determine value to fill with during scroll
u2:
	; READ_AC_CURRENT		:
	;   THIS ROUTINE READS THE ATTRIBUTE AND CHARACTER
	;    AT THE CURRENT CURSOR POSITION
	;
	; INPUT				
	;	(AH) = CURRENT CRT MODE
	;	(BH) = DISPLAY PAGE ( ALPHA MODES ONLY )
	;	(DS) = DATA SEGMENT
	;	(ES) = REGEN SEGMENT
	; OUTPUT			
	;	(AL) = CHARACTER READ
	;	(AH) = ATTRIBUTE READ
	;
	; mov	ah, [CRT_MODE] ; move current mode into ah
	;
	; bl = video page number
	;
	call	find_position	; get regen location and port address
	; dx = status port
	; esi = cursor location/address
p11:
	sti			; enable interrupts
	nop			; allow for small interupts window
	cli			; blocks interrupts for single loop
	in	al, dx		; get status from adapter
	test	al, RHRZ	; is horizontal retrace low
	jnz	short p11	; wait until it is
p12:				; now wait for either retrace high
	in	al, dx		; get status
	test	al, RVRT+RHRZ	; is horizontal or vertical retrace high
	jz	short p12	; wait until either is active	
p13:
	add	esi, 0B8000h	; 30/08/2014 (Retro UNIX 386 v1)
	mov 	ax, [esi]	; get the character and attribute
	;
	; al = character, ah = attribute
	;
	sti
	; bl = video page number 	
u3:
	;;mov	ax, 0601h 	; scroll one line
	;;sub	cx, cx		; upper left corner
	;;mov	dh, 25-1 	; lower right row
	;;;mov	dl, [CRT_COLS]
	;mov	dl, 80		; lower right column	
	;;dec	dl
	;;mov	dl, 79

	;;call	scroll_up	; 04/12/2013
	;;; 11/03/2015
	; 02/09/2014
	;;;mov	cx, [crt_ulc] ; Upper left corner  (0000h)
	;;;mov	dx, [crt_lrc] ; Lower right corner (184Fh)
	; 11/03/2015
	;sub	cx, cx
	;mov	dx, 184Fh ; dl= 79 (column), dh = 24 (row)
	;
	; 02/02/2022 (simplied scroll up)
	; ((retro unix 8086 v1 'scroll_up' in 'u9.s'))
	;
	mov	al, 1		; scroll 1 line up
		; ah = attribute
	jmp	scroll_up
;u4:
	;;int	10h		; video-call return
				; scroll up the screen
				; tty return
;u5:
	;retn			; return to the caller

u6:				; set-cursor-inc
	inc	dh		; next row
				; set cursor
;u7:					
	;;mov	ah, 02h
	;;jmp	short u4 	; establish the new cursor
	;call	set_cpos
	;jmp 	short u5
	jmp     set_cpos

	; check for control characters
u8:
	je	short u9
	cmp	al, 0Ah		; is it a line feed (0Ah)
	je	short u10
	cmp	al, 07h 	; is it a bell
	je	short u11
	cmp	al, 08h		; is it a backspace
	;jne	short u0
	je	short bs	; 12/12/2013
	; 12/12/2013 (tab stop)
	cmp	al, 09h		; is it a tab stop
	jne	short u0
	mov	al, dl
	cbw
	mov	cl, 8
	div	cl
	sub	cl, ah
ts:
	; 02/09/2014
	; 01/09/2014
	mov	al, 20h
tsloop:
	;push	cx
	;push	ax
	; 02/02/2022
	push	ecx
	push	eax
	xor 	bh, bh
	;mov	bl, [active_page]
	call	m3
	; 02/02/2022
	pop	eax
 	pop	ecx
	;pop	ax  ; ah = attribute/color
	;pop	cx
	dec	cl
	jnz	short tsloop
	retn
bs:	
	; back space found
	or	dl, dl 		; is it already at start of line
	;je	short u7 	; set_cursor
	jz	short set_cpos
	;dec	dx     		; no -- just move it back
	; 02/02/2022
	dec	dl
	;jmp	short u7
	jmp	short set_cpos

	; carriage return found
u9:
	mov	dl, 0 		; move to first column
	;jmp	short u7
	jmp	short set_cpos

	; line feed found
;u10:
;	cmp	dh, 25-1 	; bottom of screen
;	jne	short u6 	; no, just set the cursor
;       jmp     u1              ; yes, scroll the screen

beeper: 
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 03/12/2013
	; bell found
u11:
	sti
	cmp	bl, [active_page]
	jne	short u12	; Do not sound the beep 
				; if it is not written on the active page
	mov	cx, 1331 	; divisor for 896 hz tone
	mov	bl, 31		; set count for 31/64 second for beep
	;call	beep		; sound the pod bell
	;jmp	short u5 	; tty_return
	;retn
	
TIMER	equ 	040h   		; 8254 TIMER - BASE ADDRESS
PORT_B	equ	061h		; PORT B READ/WRITE DIAGNOSTIC REGISTER
GATE2	equ	00000001b	; TIMER 2 INPUT CATE CLOCK BIT
SPK2	equ	00000010b	; SPEAKER OUTPUT DATA ENABLE BIT

beep:
	; 07/02/2015
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 03/12/2013
	;
	; TEST4.ASM - 06/10/85  POST AND BIOS UTILITY ROUTINES
	;
	; ROUTINE TO SOUND THE BEEPER USING TIMER 2 FOR TONE
	;
	; ENTRY:
	;    (BL) = DURATION COUNTER ( 1 FOR 1/64 SECOND )
	;    (CX) = FREQUENCY DIVISOR (1193180/FREQUENCY) (1331 FOR 886 HZ)
	; EXIT:			:
	;    (AX),(BL),(CX) MODIFIED.

	pushf  ; 18/01/2014	; save interrupt status
	cli			; block interrupts during update
	mov	al, 10110110b	; select timer 2, lsb, msb binary
	out	TIMER+3, al 	; write timer mode register
	jmp	$+2		; I/O delay
	mov	al, cl		; divisor for hz (low)
	out	TIMER+2,AL	; write timer 2 count - lsb
	jmp	$+2		; I/O delay
	mov	al, ch		; divisor for hz (high)
	out	TIMER+2, al	; write timer 2 count - msb
	in	al, PORT_B	; get current setting of port
	mov	ah, al		; save that setting
	or	al, GATE2+SPK2	; gate timer 2 and turn speaker on
	out	PORT_B, al	; and restore interrupt status
	;popf	; 18/01/2014
	sti
g7:				; 1/64 second per count (bl)
	mov	ecx, 1035	; delay count for 1/64 of a second	
	call	waitf		; go to beep delay 1/64 count
	dec	bl		; (bl) length count expired?
	jnz	short g7	; no - continue beeping speaker
	;
	;pushf			; save interrupt status
	cli  	; 18/01/2014	; block interrupts during update
	in	al, PORT_B	; get current port value
        ;or	al, not (GATE2+SPK2) ; isolate current speaker bits in case
        or      al, ~(GATE2+SPK2)
        and	ah, al		; someone turned them off during beep
	mov	al, ah		; recover value of port
        ;or	al, not (GATE2+SPK2) ; force speaker data off
	or 	al, ~(GATE2+SPK2) ; isolate current speaker bits in case
	out	PORT_B, al	; and stop speaker timer
	;popf			; restore interrupt flag state
	sti
	mov	ecx, 1035	; force 1/64 second delay (short)
	call	waitf		; minimum delay between all beeps
	;pushf			; save interrupt status
	cli			; block interrupts during update
	in	al, PORT_B	; get current port value in case	
	and	al, GATE2+SPK2	; someone turned them on
	or	al, ah		; recover value of port_b
	out	PORT_B, al	; restore speaker status
	popf			; restore interrupt flag state
u12:	
	retn

REFRESH_BIT equ	00010000b 	; REFRESH TEST BIT

WAITF:
waitf:
	; 30/08/2014 (Retro UNIX 386 v1)
	; 03/12/2013
	;
;	push	ax		; save work register (ah)	
;waitf1:
				; use timer 1 output bits
;	in	al, PORT_B	; read current counter output status
;	and	al, REFRESH_BIT	; mask for refresh determine bit
;	cmp	al, ah		; did it just change
;	je	short waitf1	; wait for a change in output line
;	;
;	mov	ah, al		; save new lflag state
;	loop	waitf1		; decrement half cycles till count end		
;	;
;	pop	ax		; restore (ah)
;	retn			; return (cx)=0

; 02/02/2022
; 06/02/2015 (unix386.s <-- dsectrm2.s)
; 17/12/2014 (dsectrm2.s)
; WAITF
; /// IBM PC-XT Model 286 System BIOS Source Code - Test 4 - 06/10/85 ///
;
;---WAITF-----------------------------------------------------------------------
;	FIXED TIME WAIT ROUTINE (HARDWARE CONTROLLED - NOT PROCESSOR)
; ENTRY:
;	(CX) =	COUNT OF 15.085737 MICROSECOND INTERVALS TO WAIT
;	      	MEMORY REFRESH TIMER 1 OUTPUT USED AS REFERENCE
; EXIT:
;	       	AFTER (CX) TIME COUNT (PLUS OR MINUS 16 MICROSECONDS)
;	(CX) = 0	
;-------------------------------------------------------------------------------

; Refresh period: 30 micro seconds (15-80 us)
; (16/12/2014 - AWARDBIOS 1999 - ATORGS.ASM, WAIT_REFRESH)

;WAITF:					; DELAY FOR (CX)*15.085737 US
	push	eax ; 02/02/2022	; SAVE WORK REGISTER (AH)
	;push	ax
	; 16/12/2014
	;shr	cx, 1			; convert to count of 30 micro seconds
	shr	ecx, 1	; 21/02/2015
;17/12/2014	
;WAITF1:
;	IN	AL, PORT_B   ;061h	; READ CURRENT COUNTER OUTPUT STATUS
;	AND	AL, REFRESH_BIT	;00010000b ; MASK FOR REFRESH DETERMINE BIT
;	CMP	AL, AH			; DID IT JUST CHANGE
;	JE	short WAITF1		; WAIT FOR A CHANGE IN OUTPUT LINE
;	MOV	AH, AL			; SAVE NEW FLAG STATE
;	LOOP	WAITF1			; DECREMENT HALF CYCLES TILL COUNT END		
	;
	; 17/12/2014
	;
	; Modification from 'WAIT_REFRESH' procedure of AWARD BIOS - 1999
	;
;WAIT_REFRESH:  Uses port 61, bit 4 to have CPU speed independent waiting.
;   	INPUT:  CX = number of refresh periods to wait
;     	       (refresh periods = 1 per 30 microseconds on most machines)
WR_STATE_0:
	IN	AL,PORT_B		; IN AL,SYS1
	TEST	AL,010H
	JZ	SHORT WR_STATE_0
WR_STATE_1:
	IN	AL,PORT_B		; IN AL,SYS1
	TEST	AL,010H
	JNZ	SHORT WR_STATE_1
        LOOP    WR_STATE_0
	;
	;pop	ax
	pop	eax ; 02/02/2022	; RESTORE (AH)
	RETn				; (CX) = 0

set_cpos:
	; 14/06/2022 (Retro UNIX 386 v1.2, Kernel v0.2.2.2)
	; 27/02/2022
	; 23/02/2022
	; 02/02/2022
	; 27/06/2015
	; 01/09/2014
	; 30/08/2014 (Retro UNIX 386 v1 - beginning)
	;
	; 12/12/2013 (Retro UNIX 8086 v1 - last update) 
	; 04/12/2013 (Retro UNIX 8086 v1 - beginning)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; SET_CPOS
	;	THIS ROUTINE SETS THE CURRENT CURSOR POSITION TO THE
	;	NEW X-Y VALUES PASSED
	; INPUT
	;	DX - ROW,COLUMN OF NEW CURSOR
	;	BH - DISPLAY PAGE OF CURSOR
	; OUTPUT
	;	CURSOR IS SET AT 6845 IF DISPLAY PAGE IS CURRENT DISPLAY
	;
        movzx   eax, bl	; BL = video page number ; 27/06/2015 (movzx)
        shl     al, 1   ; word offset
	mov	esi, cursor_posn
        add     esi, eax
	mov	[esi], dx ; save the pointer
	cmp	[active_page], bl
	jne	short m17

	; 14/06/2022
	;cli	; 27/02/2022

	;call	m18	; CURSOR SET
;m17:			; SET_CPOS_RETURN
	; 01/09/2014
;	retn
		; DX = row/column
m18:
	call	position ; determine location in regen buffer	
	;mov	cx, [CRT_START]
	; 23/02/2022
	movzx	ecx, word [CRT_START]
	add	ecx, eax
	;add	cx, ax  ; add char position in regen buffer
			; to the start address (offset) for this page
	;shr	cx, 1	; divide by 2 for char only count
	; 23/02/2022
	shr	ecx, 1
	mov	ah, 14	; register number for cursor
	
	; 14/06/2022
	;call	m16	; output value to the 6845
	;sti	; 27/02/2022
	;retn

	; 14/06/2022
	; 27/02/2022
	; 02/02/2022
	;-----	THIS ROUTINE OUTPUTS THE CX REGISTER
	;	TO THE 6845 REGISTERS NAMED IN (AH)
m16:
	; 14/06/2022
	cli	; 27/02/2022
	;mov	dx, [addr_6845] ; address register
	mov 	dx, 03D4h ; I/O address of color card
	mov	al, ah	; get value
	out	dx, al	; register set
	;inc	dx	; data register
	; 02/02/2022
	inc	dl
	jmp	$+2	; i/o delay
	mov	al, ch	; data
	out	dx, al	
	;dec	dx
	; 02/02/2022	
	dec	dl
	mov	al, ah
	inc	al	; point to other data register
	out	dx, al	; set for second register
	;inc	dx
	; 02/02/2022
	inc	dl
	jmp	$+2	; i/o delay
	mov	al, cl	; second data value
	out	dx, al
	; 14/06/2022
	sti	; 27/02/2022
;m17:
	retn
m17:
	; 14/06/2022
	; ('write_tty' must not return to 'putc' with cf)
	clc
	retn

set_ctype:
	; 07/02/2022
	; 02/09/2014 (Retro UNIX 386 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

;	CH) = BITS 4-0 = START LINE FOR CURSOR
;       ** HARDWARE WILL ALWAYS CAUSE BLINK
;       ** SETTING BIT 5 OR 6 WILL CAUSE ERRATIC BLINKING
;          OR NO CURSOR AT ALL
;	(CL) = BITS 4-0 = END LINE FOR CURSOR

;------------------------------------------------
; SET_CTYPE
;	THIS ROUTINE SETS THE CURSOR VALUE
; INPUT
;	(CX) HAS CURSOR VALUE CH-START LINE, CL-STOP LINE
; OUTPUT	
;	NONE
;------------------------------------------------

	mov	ah, 10	; 6845 register for cursor set
	;mov	[CURSOR_MODE], cx ; save in data area
	;call	m16	; output cx register
	;retn
	; 07/02/2022
	jmp	short m16

position:
	; 23/02/2022
	; 02/02/2022
	; 27/06/2015
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1)
	; 04/12/2013 (Retro UNIX 8086 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; POSITION
	;	THIS SERVICE ROUTINE CALCULATES THE REGEN BUFFER ADDRESS
	;	OF A CHARACTER IN THE ALPHA MODE
	; INPUT
	;	AX = ROW, COLUMN POSITION
	; OUTPUT
	;	AX = OFFSET OF CHAR POSITION IN REGEN BUFFER

		; DX = ROW, COLUMN POSITION
	;movzx	eax, byte [CRT_COLS] ; 27/06/2015
	xor	eax, eax ; 02/09/2014
	mov	al, 80	; determine bytes to row	
	mul	dh	; row value
	;xor	dh, dh	; 0
	;add	ax, dx	; add column value to the result
	; 23/02/2022
	add	al, dl
	adc	ah, 0	
	;shl	ax, 1	; * 2 for attribute bytes
	; 02/02/2022
	shl	eax, 1
		; EAX = AX = OFFSET OF CHAR POSITION IN REGEN BUFFER 
	retn

find_position:
	; 02/02/2022
	; 27/06/2015
	; 07/09/2014
	; 02/09/2014
	; 30/08/2014 (Retro UNIX 386 v1)
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	movzx	ecx, bl ; video page number ; 27/06/2015 (movzx)
	mov	esi, ecx
	;shl	si, 1
	; 02/02/2022
	shl	esi, 1
	mov	dx, [esi+cursor_posn]
	jz	short p21
	;xor	si, si
	; 02/02/2022
	xor	esi, esi
p20:
	;add	si, [CRT_LEN]
	add	si, 80*25*2 ; add length of buffer for one page		
	loop	p20
p21:
	and	dx, dx
	jz	short p22
	call 	position ; determine location in regen in page
	add	esi, eax ; add location to start of regen page
p22:	
	;mov	dx, [addr_6845] ; get base address of active display			
	;mov	dx, 03D4h ; I/O address of color card
	;add	dx, 6	; point at status port
	mov	dx, 03DAh ; status port
	; cx = 0
	retn

scroll_up:
	; 02/02/2022 (simplified scroll up)
	;	((retro unix 8086 v1 'scroll_up' in 'u9.s'))
	; 16/01/2016
	; 07/09/2014
	; 02/09/2014
	; 01/09/2014 (Retro UNIX 386 v1 - beginning)
	; 04/04/2014
	; 04/12/2013
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; SCROLL UP
	;	THIS ROUTINE MOVES A BLOCK OF CHARACTERS UP
	;	ON THE SCREEN
	; INPUT
	;	(AH) = CURRENT CRT MODE
	;	(AL) = NUMBER OF ROWS TO SCROLL
	;	(CX) = ROW/COLUMN OF UPPER LEFT CORNER
	;	(DX) = ROW/COLUMN OF LOWER RIGHT CORNER
	;	(BH) = ATTRIBUTE TO BE USED ON BLANKED LINE
	;	(DS) = DATA SEGMENT
	;	(ES) = REGEN BUFFER SEGMENT
	; OUTPUT
	;	NONE -- THE REGEN BUFFER IS MODIFIED
	;
	;	bh = 0  (02/09/2014)
	;
	; ((ah = 3))
	; cl = left upper column
	; ch = left upper row
	; dl = right lower column
	; dh = right lower row
	;
	; al = line count 
	; ah = attribute to be used on blanked line
	; bl = video page number (0 to 7)
	; 

	; 02/02/2022 'scroll_up' code
	; ------------------------------------------------------
	; (ref: Retro UNIX 8086 v1 'scroll_up' code in 'u9.asm')

	; INPUT:
	;		
	; al = line count 
	;	(0 or 1) .. 0 -> clear video page
	; ah = attribute to be used on blanked line
	; bl = video page number (0 to 7)

	;cli
	xor	ecx, ecx
	mov	cl, al ; line count (cl)
	mov	esi, 0B8000h
	cmp	bl, [active_page]
	je	short n1
	and	bl, bl
	jz	short n3
	mov	ch, bl ; video page number
n0:
	add	si, 25*80*2
	dec	ch
	jnz	short n0
	jmp	short n3
n1:
	add	si, [CRT_START]
	;
	mov	dx, 3DAh ; guaranteed to be color card here
n2:			 ; wait_display_enable
	in	al, dx	 ; get port
	test	al, RVRT ; wait for vertical retrace
	jz	short n2 ; wait_display_enable
	mov	al, 25h
	mov	dl, 0D8h ; address control port
	out	dx, al	 ; turn off video during vertical retrace
n3:
	; cl = line count
	; ah = attribute/color
	mov	edi, esi
	and	cl, cl
	jz	short n6
	add	si, 80*2 ; + 160 bytes
	mov	cx, 24*80 ; 24 rows/lines
	rep	movsw
	mov	cl, 80 ; 1 row (will be cleared)
n4:
	; ah = character attribute/cocor
	mov	al, 20h ; fill with blanks
	rep	stosw

	cmp	bl, [active_page]
	jne	short n5

	;mov	al, [crt_mode_set] ; get the value of mode set
	mov	al, 29h ; (ORGS.ASM), M7 mode set table value for mode 3
	;mov	dx, 03D8h ; always set color card port
	out	dx, al
n5:
	retn
n6:
	; clear video page
	mov	cx, 25*80 ; 25 rows/lines
	jmp	short n4

	; 23/02/2022
%if 0	; 16/01/2016 'scroll_up' code
	; ------------------------------------------------------

	; Test	Line Count
	or	al, al
	jz	short al_set
	mov	bh, dh	; subtract lower row from upper row
	sub	bh, ch
	inc	bh	; adjust difference by 1
	cmp	bh, al 	; line count = amount of rows in window?
	jne	short al_set ; if not the we're all set
	xor	al, al	; otherwise set al to zero
al_set:
	xor	bh, bh	; 0
	;push	ax
	push	eax ; 23/02/2022
	;mov 	esi, [crt_base]
        mov     esi, 0B8000h  
        cmp     bl, [active_page]
	jne	short n0
	;
        mov     ax, [CRT_START]
        add     si, ax
        jmp     short n1
n0:
        and     bl, bl
	jz	short n1
	mov	al, bl
n0x:
        ;add    si, [CRT_LEN]
        ;add    esi, 80*25*2 
        add     si, 80*25*2
        dec	al
	jnz	short n0x
n1:	
        ; Scroll position
	;push	dx ; 23/02/2022
	mov	dx, cx	; now, upper left position in DX
	call	position
	add	esi, eax
	mov	edi, esi
	;pop	dx	; lower right position in DX
	sub	dx, cx
	inc	dh	; dh = #rows 
	inc	dl	; dl = #cols in block
	;pop	ax	; al = line count, ah = attribute
	pop	eax ; 23/02/2022
	xor	ecx, ecx
	mov	cx, ax
	;mov	ah, [CRT_COLS]
	mov	ah, 80
	mul	ah	; determine offset to from address
	add	ax, ax  ; *2 for attribute byte
	;
	;push	ax	; offset 
	;push	dx
	; 23/02/2022
	push	eax
	push	edx
	;
	; 04/04/2014
	mov	dx, 3DAh ; guaranteed to be color card here
n8:                      ; wait_display_enable
        in      al, dx   ; get port
	test	al, RVRT ; wait for vertical retrace
	jz	short n8 ; wait_display_enable
	mov	al, 25h
	mov	dl, 0D8h ; address control port
	out	dx, al	; turn off video during vertical retrace
	;pop	dx	; #rows, #cols
       	;pop	ax	; offset
	; 23/02/2022
	pop	edx
	pop	eax
	xchg	ax, cx	; 
	; ecx = offset, al = line count, ah = attribute
;n9:
	or	al, al
        jz      short n3 
        add     esi, ecx ; from address for scroll
	mov	bh, dh  ; #rows in block
	sub	bh, al	; #rows to be moved
n2:
	; Move rows
	mov	cl, dl	; get # of cols to move
	push	esi
	push	edi	; save start address
n10:
	movsw		; move that line on screen
	dec	cl
        jnz     short n10
	pop	edi
	pop	esi	; recover addresses
        ;mov    cl, [CRT_COLS] 
	;add	cl, cl
        ;mov    ecx, 80*2
        mov     cx, 80*2
        add     esi, ecx  ; next line
        add     edi, ecx
	dec	bh	 ; count of lines to move
	jnz	short n2 ; row loop
	; bh = 0
	mov	dh, al	 ; #rows	
n3:
	; attribute in ah
	mov	al, ' '	 ; fill with blanks
n3x:
	; Clear rows
                ; dh =  #rows
        mov	cl, dl	; get # of cols to clear
        push    edi     ; save address
n11:
        stosw           ; store fill character
	dec	cl
        jnz     short n11
        pop     edi     ; recover address
	;mov	cl, [CRT_COLS]
	;add	cl, cl
        ;mov    ecx, 80*2
        mov	cl, 80*2
        add     edi, ecx
	dec	dh
	jnz	short n3x ; 16/01/2016
	;
	cmp	bl, [active_page]
	jne	short n6
	;mov	al, [CRT_MODE_SET] ; get the value of mode set
	mov	al, 29h ; (ORGS.ASM), M7 mode set table value for mode 3
	mov	dx, 03D8h ; always set color card port
	out	dx, al
n6:
	retn

%endif

write_c_current:
	; 02/02/2022
	; 30/08/2014 (Retro UNIX 386 v1)
	; 18/01/2014
	; 04/12/2013
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; WRITE_C_CURRENT
	;	THIS ROUTINE WRITES THE CHARACTER AT
	;	THE CURRENT CURSOR POSITION, ATTRIBUTE UNCHANGED
	; INPUT	
	;	(AH) = CURRENT CRT MODE
	;	(BH) = DISPLAY PAGE
	;	(CX) = COUNT OF CHARACTERS TO WRITE
	;	(AL) = CHAR TO WRITE
	;	(DS) = DATA SEGMENT
	;	(ES) = REGEN SEGMENT
	; OUTPUT
	;	DISPLAY REGEN BUFFER UPDATED

	cli		
	; bl = video page
	; al = character
	; ah = color/attribute
	;push	dx
	;push	ax	; save character & attribute/color
	; 02/02/2022
	push	edx
	push	eax
	call 	find_position  ; get regen location and port address
	; esi = regen location
	; dx = status port
	;
	; WAIT FOR HORIZONTAL RETRACE OR VERTICAL RETRACE
	;
p41:			; wait for horizontal retrace is low or vertical
	sti		; enable interrupts first
        cmp     bl, [active_page]
	jne	short p44 
	cli 		; block interrupts for single loop
	in	al, dx	; get status from the adapter
	test	al, RVRT ; check for vertical retrace first
	jnz	short p43 ; Do fast write now if vertical retrace
	test	al, RHRZ  ; is horizontal retrace low
	jnz	short p41 ; wait until it is
p42:			; wait for either retrace high
	in	al, dx	; get status again
	test	al, RVRT+RHRZ ; is horizontal or vertical retrace high
	jz	short p42 ; wait until either retrace active
p43:	
	sti
p44:
	;pop	ax	; restore the character (al) & attribute (ah)
	; 02/02/2022
	pop	eax
	add	esi, 0B8000h ; 30/08/2014 (crt_base) 
			; Retro UNIX 386 v1 feature only!
	mov	[esi], ax
	;pop	dx
	; 02/02/2022
	pop	edx
	retn

%if 0	; 02/02/2022

set_mode:
	; 02/02/2022
	; 16/01/2016
	; 02/09/2014 (Retro UNIX 386 v1)
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS

;------------------------------------------------------
; SET MODE					      :
;	THIS ROUTINE INITIALIZES THE ATTACHMENT TO    :
;	THE SELECTED MODE, THE SCREEN IS BLANKED.     :
; INPUT						      :
;	(AL) - MODE SELECTED (RANGE 0-7)	      :
; OUTPUT					      :
;	NONE					      :
;------------------------------------------------------

	push	edi ; 16/01/2016
	push	ebx
	push	edx
	push	ecx ; 16/01/2016
        push    eax

	;mov	dx, 03D4h 	; address or color card
	mov	al, 3
;M8:
	mov	[CRT_MODE], al  ; save mode in global variable
	mov	al, 29h
	;mov	[CRT_MODE_SET], al ; save the mode set value
	and	al, 037h	; video off, save high resolution bit	
	;push	dx  		; save port value
	;add	dx, 4		; point to control register
	mov	dx, 3D8h
	out	dx, al		; reset video to off to suppress rolling
	;pop	dx
;M9:
	mov	ebx, video_params ; initialization table
	;mov	ax, [ebx+10]      ; get the cursor mode from the table	
	;xchg 	ah, al
	;mov	[CURSOR_MODE], ax ; save cursor mode
	xor	ah, ah		  ; ah is register number during loop 
	
;-----	LOOP THROUGH TABLE, OUTPUTTING REGISTER ADDRESS, THEN VALUE FROM TABLE
	; 02/02/2022
	; dx = 3D8h
	xor	ecx, ecx
	mov	cl, 16
	;mov	ecx, 16 ; 16/01/2016
M10:			;  initialization loop
	mov	al, ah 	; get 6845 register number
	out	dx, al
	;inc	dx      ; point to data port
	; 02/02/2022
	inc	dl ; 3D9h
	inc	ah	; next register value
	mov	al, [ebx] ; get table value
	out	dx, al	; out to chip
	inc	ebx	; next in table
	;dec	dx	; back to pointer register
	; 02/02/2022
	dec	dl ; 3D8h
	loop	M10	; do the whole table
	
;-----	FILL REGEN AREA WITH BLANK
	;xor	ax, ax  
	;mov	[CRT_START], ax  ; start address saved in global
	;mov	[ACTIVE_PAGE], al ; 0 ; (re)set page value
	;mov	ecx, 8192 ; number of words in color card
	; black background, light gray characeter color, space character
	;mov	ax, 0720h ; fill char for alpha - attribute
;M13:			  ; clear buffer
	;add	edi, 0B8000h ; [crt_base]
	;rep	stosw	; FILL THE REGEN BUFFER WITH BLANKS

;-----	ENABLE VIDEO AND CORRECT PORT SETTING
	;mov	dx, 3D4h ; mov dx, word [ADDR_6845]
			 ; prepare to output to video enable port
	;;add	dx, 4	 ; point to the mode control gerister
	; 02/02/2022
	;mov	dx, 3D8h
	; 
	;mov	al, [CRT_MODE_SET] ; get the mode set value
	mov	al, 29h
	out	dx, al	 ; set video enable port

;----- 	DETERMINE NUMBER OF COLUMNS, BOTH FOR ENTIRE DISPLAY
;----- 	AND THE NUMBER TO BE USED FOR TTY INTERFACE
	;
	;mov byte [CRT_COLS], 80h ; initialize number of columns count
	;
;-----	SET CURSOR POSITIONS
	;push	edi
	;mov	word [CRT_LEN], 80*25*2
	mov	edi, cursor_posn
	mov	ecx, 4	; clear all cursor positions (16 bytes)
	xor	eax, eax
	rep 	stosd	; fill with zeroes
	;pop	edi

;-----	SET UP OVERSCAN REGISTER
	inc	dx	; set overscan port to a default
	mov	al, 30h	; 30H valuye for all modes except 640X200 bw
;M14:
	out	dx, al	; output the correct value to 3D9 port
	;mov	[CRT_PALETTE], al ; save the value for future use

;-----	NORMAL RETURN FROM ALL VIDEO RETURNS
	;
	pop	eax
	pop	ecx ; 16/01/2016
	pop	edx
	pop	ebx
	pop	edi ; 16/01/2016
	retn

%endif
	
tty_sw:
	; 02/02/2022
	; 30/06/2015
	; 27/06/2015 
	; 07/09/2014
	; 02/09/2014 (Retro UNIX 386 v1 - beginning)
	;
	; (Modified registers : EAX)
	;
        ;mov     byte [u.quant], 0  ; 04/03/2014
	;
;act_disp_page:
	; 30/06/2015
	; 04/03/2014  (act_disp_page --> tty_sw)
	; 10/12/2013
	; 04/12/2013
	;
	; VIDEO.ASM - 06/10/85  VIDEO DISPLAY BIOS
	;
	; ACT_DISP_PAGE
	;	THIS ROUTINE SETS THE ACTIVE DISPLAY PAGE, ALLOWING
	;	THE FULL USE OF THE MEMORY SET ASIDE FOR THE VIDEO ATTACHMENT
	; INPUT
	;	AL HAS THE NEW ACTIVE DISPLAY PAGE
	; OUTPUT
	;	THE 6845 IS RESET TO DISPLAY THAT PAGE

	;cli

	push	ebx
	;push	cx
	;push	dx
	; 02/02/2022
	push	ecx
	push	edx
	;
	mov	[active_page], al ; save active page value ; [ptty]
	;;mov	cx, [CRT_LEN] ; get saved length of regen buffer
	;mov	cx, 25*80*2
	; 02/02/2022
	mov	ecx, 25*80*2
	; 27/06/2015
	movzx	ebx, al
	; 02/02/2022
	mov	eax, ebx
	;
	;cbw	; 07/09/2014 (ah=0)
	;mul 	cx	; display page times regen length
	; 02/02/2022
	mul	ecx	
	; 10/12/2013
	mov	[CRT_START], ax ; save start address for later
	;mov	cx, ax	; start address to cx
	; 02/02/2022
	mov	ecx, eax
	;;sar	cx, 1
	;shr	cx, 1	; divide by 2 for 6845 handling
	; 02/02/2022
	shr	ecx, 1
	mov	ah, 12	; 6845 register for start address
	call	m16
	;sal	bx, 1
	; 01/09/2014
	shl	bl, 1	; * 2 for word offset
	add	ebx, cursor_posn
	mov	dx, [ebx] ; get cursor for this page
	call	m18
	;
	;pop	dx
	;pop	cx
	; 02/02/2022
	pop	edx
	pop	ecx
	pop	ebx
	;
	;sti
	;
	retn

; %include 'vidata.inc' ; VIDEO DATA ; 11/03/2015

; /// End Of VIDEO FUNCTIONS ///