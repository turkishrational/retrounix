; Retro UNIX 386 v1.1 Kernel (v0.2.1.6) - DISKIO.INC
; Last Modification: 18/07/2022
; 	(Initialized Disk Parameters Data is in 'DISKDATA.INC') 
; 	(Uninitialized Disk Parameters Data is in 'DISKBSS.INC') 

; DISK I/O SYSTEM - Erdogan Tan (Retro UNIX 386 v1 project)

; ///////// DISK I/O SYSTEM ///////////////

; 11/07/2022
;; 06/02/2015
;diskette_io:
;	pushfd
;	push 	cs
;	call 	DISKETTE_IO_1
;	retn
	
;;;;;; DISKETTE I/O ;;;;;;;;;;;;;;;;;;;; 06/02/2015 ;;;
;//////////////////////////////////////////////////////

; 11/07/2022 - (direct call instead of int 13h simulation)
;		Function in AL
;			0 = reset
;			1 = read
;			2 = write
;		Disk drive number in DL
;			0 & 1 = floppy disks	
;			80h .. 83h = hard disks
;		Sector address (LBA) in ECX
;		Buffer address in EBX
;		R/W sector count is (always) 1
;
;		Return:
;			Status in AH (>0 = error code)
;			if CF = 1 -> error code in AH
;			if CF = 0 -> successful
;			AL = undefined
;
;		Modified registers: (only) EAX

; 10/07/2022
; 08/07/2022 - (diskio code has been simplified/shortened 
;		by removing unused IBM PC-AT disk functions)
; DISKETTE I/O - Erdogan Tan (Retro UNIX 386 v1 project)
; 20/02/2015
; 06/02/2015 (unix386.s)
; 16/12/2014 - 02/01/2015 (dsectrm2.s)
;
; Code (DELAY) modifications - AWARD BIOS 1999 (ADISK.EQU, COMMON.MAC)
;
; ADISK.EQU

;----- Wait control constants 

;amount of time to wait while RESET is active.

WAITCPU_RESET_ON   EQU	21		;Reset on must last at least 14us
					;at 250 KBS xfer rate.
					;see INTEL MCS, 1985, pg. 5-456

WAITCPU_FOR_STATUS EQU	100		;allow 30 microseconds for
					;status register to become valid
					;before re-reading.

;After sending a byte to NEC, status register may remain
;incorrectly set for 24 us.

WAITCPU_RQM_LOW	   EQU	24		;number of loops to check for
					;RQM low.

; COMMON.MAC
;
;	Timing macros
;

%macro 		SIODELAY 0 		; SHORT IODELAY
		jmp short $+2
%endmacro		

%macro		IODELAY  0		; NORMAL IODELAY
		jmp short $+2
		jmp short $+2
%endmacro

%macro		NEWIODELAY 0
		out 0EBh,al
%endmacro 

; (According to) AWARD BIOS 1999 - ATORGS.ASM (dw -> equ, db -> equ)
;;; WAIT_FOR_MEM
;WAIT_FDU_INT_LO	equ	017798		; 2.5 secs in 30 micro units.
;WAIT_FDU_INT_HI	equ	1
WAIT_FDU_INT_LH		equ	83334		; 27/02/2015 (2.5 seconds waiting)
;;; WAIT_FOR_PORT
;WAIT_FDU_SEND_LO	equ	16667		; .5 secons in 30 us units.
;WAIT_FDU_SEND_HI	equ	0
WAIT_FDU_SEND_LH	equ 	16667		; 27/02/2015	
;Time to wait while waiting for each byte of NEC results = .5
;seconds.  .5 seconds = 500,000 micros.  500,000/30 = 16,667.
;WAIT_FDU_RESULTS_LO	equ	16667		; .5 seconds in 30 micro units.
;WAIT_FDU_RESULTS_HI	equ	0
WAIT_FDU_RESULTS_LH	equ	16667  ; 27/02/2015
;;; WAIT_REFRESH
;amount of time to wait for head settle, per unit in parameter
;table = 1 ms.
WAIT_FDU_HEAD_SETTLE	equ	33		; 1 ms in 30 micro units.


; //////////////// DISKETTE I/O ////////////////

; 11/12/2014 (copy from IBM PC-XT Model 286 BIOS - POSTEQU.INC)

;----------------------------------------
;	EQUATES USED BY POST AND BIOS	:
;----------------------------------------

;--------- 8042 KEYBOARD INTERFACE AND DIAGNOSTIC CONTROL REGISTERS ------------
;PORT_A		EQU	060H		; 8042 KEYBOARD SCAN CODE/CONTROL PORT
;PORT_B		EQU	061H		; PORT B READ/WRITE DIAGNOSTIC REGISTER
;REFRESH_BIT	EQU	00010000B	; REFRESH TEST BIT

;----------------------------------------
;	CMOS EQUATES FOR THIS SYSTEM	:
;-------------------------------------------------------------------------------
;CMOS_PORT	EQU	070H		; I/O ADDRESS OF CMOS ADDRESS PORT
;CMOS_DATA	EQU	071H		; I/O ADDRESS OF CMOS DATA PORT
;NMI		EQU	10000000B	; DISABLE NMI INTERRUPTS MASK -
					;  HIGH BIT OF CMOS LOCATION ADDRESS

;---------- CMOS TABLE LOCATION ADDRESS'S ## -----------------------------------
CMOS_DISKETTE	EQU	010H		; DISKETTE DRIVE TYPE BYTE	      ;
;		EQU	011H		; - RESERVED			      ;C
CMOS_DISK	EQU	012H		; FIXED DISK TYPE BYTE		      ;H
;		EQU	013H		; - RESERVED			      ;E
CMOS_EQUIP	EQU	014H		; EQUIPMENT WORD LOW BYTE	      ;C

;---------- DISKETTE EQUATES ---------------------------------------------------
INT_FLAG	EQU	10000000B	; INTERRUPT OCCURRENCE FLAG
DSK_CHG 	EQU	10000000B	; DISKETTE CHANGE FLAG MASK BIT
DETERMINED	EQU	00010000B	; SET STATE DETERMINED IN STATE BITS
HOME		EQU	00010000B	; TRACK 0 MASK
SENSE_DRV_ST	EQU	00000100B	; SENSE DRIVE STATUS COMMAND
TRK_SLAP	EQU	030H		; CRASH STOP (48 TPI DRIVES)
QUIET_SEEK	EQU	00AH		; SEEK TO TRACK 10
;MAX_DRV 	EQU	2		; MAX NUMBER OF DRIVES
HD12_SETTLE	EQU	15		; 1.2 M HEAD SETTLE TIME
HD320_SETTLE	EQU	20		; 320 K HEAD SETTLE TIME
MOTOR_WAIT	EQU	37		; 2 SECONDS OF COUNTS FOR MOTOR TURN OFF

;---------- DISKETTE ERRORS ----------------------------------------------------
;TIME_OUT	EQU	080H		; ATTACHMENT FAILED TO RESPOND
;BAD_SEEK	EQU	040H		; SEEK OPERATION FAILED
BAD_NEC 	EQU	020H		; DISKETTE CONTROLLER HAS FAILED
BAD_CRC 	EQU	010H		; BAD CRC ON DISKETTE READ
MED_NOT_FND	EQU	00CH		; MEDIA TYPE NOT FOUND
DMA_BOUNDARY	EQU	009H		; ATTEMPT TO DMA ACROSS 64K BOUNDARY
BAD_DMA 	EQU	008H		; DMA OVERRUN ON OPERATION
MEDIA_CHANGE	EQU	006H		; MEDIA REMOVED ON DUAL ATTACH CARD
RECORD_NOT_FND	EQU	004H		; REQUESTED SECTOR NOT FOUND
WRITE_PROTECT	EQU	003H		; WRITE ATTEMPTED ON WRITE PROTECT DISK
BAD_ADDR_MARK	EQU	002H		; ADDRESS MARK NOT FOUND
BAD_CMD 	EQU	001H		; BAD COMMAND PASSED TO DISKETTE I/O

;---------- DISK CHANGE LINE EQUATES -------------------------------------------
NOCHGLN 	EQU	001H		; NO DISK CHANGE LINE AVAILABLE
CHGLN		EQU	002H		; DISK CHANGE LINE AVAILABLE

;---------- MEDIA/DRIVE STATE INDICATORS ---------------------------------------
TRK_CAPA	EQU	00000001B	; 80 TRACK CAPABILITY
FMT_CAPA	EQU	00000010B	; MULTIPLE FORMAT CAPABILITY (1.2M)
DRV_DET 	EQU	00000100B	; DRIVE DETERMINED
MED_DET 	EQU	00010000B	; MEDIA DETERMINED BIT
DBL_STEP	EQU	00100000B	; DOUBLE STEP BIT
RATE_MSK	EQU	11000000B	; MASK FOR CLEARING ALL BUT RATE
RATE_500	EQU	00000000B	; 500 KBS DATA RATE
RATE_300	EQU	01000000B	; 300 KBS DATA RATE
RATE_250	EQU	10000000B	; 250 KBS DATA RATE
STRT_MSK	EQU	00001100B	; OPERATION START RATE MASK
SEND_MSK	EQU	11000000B	; MASK FOR SEND RATE BITS

;---------- MEDIA/DRIVE STATE INDICATORS COMPATIBILITY -------------------------
M3D3U		EQU	00000000B	; 360 MEDIA/DRIVE NOT ESTABLISHED
M3D1U		EQU	00000001B	; 360 MEDIA,1.2DRIVE NOT ESTABLISHED
M1D1U		EQU	00000010B	; 1.2 MEDIA/DRIVE NOT ESTABLISHED
MED_UNK 	EQU	00000111B	; NONE OF THE ABOVE

;---------- INTERRUPT EQUATES --------------------------------------------------
;EOI		EQU	020H		; END OF INTERRUPT COMMAND TO 8259
;INTA00		EQU	020H		; 8259 PORT
INTA01		EQU	021H		; 8259 PORT
INTB00		EQU	0A0H		; 2ND 8259
INTB01		EQU	0A1H		;

;-------------------------------------------------------------------------------
DMA08		EQU	008H		; DMA STATUS REGISTER PORT ADDRESS
DMA		EQU	000H		; DMA CH.0 ADDRESS REGISTER PORT ADDRESS
DMA18		EQU	0D0H		; 2ND DMA STATUS PORT ADDRESS
DMA1		EQU	0C0H		; 2ND DMA CH.0 ADDRESS REGISTER ADDRESS
;-------------------------------------------------------------------------------
;TIMER		EQU	040H		; 8254 TIMER - BASE ADDRESS

;-------------------------------------------------------------------------------
DMA_PAGE	EQU	081H		; START OF DMA PAGE REGISTERS

; 10/07/2022
; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
; 06/02/2015 (unix386.s, protected mode modifications)
; (unix386.s <-- dsectrm2.s)
; 11/12/2014 (copy from IBM PC-XT Model 286 BIOS - DSEG.INC)

; 10/12/2014
;
;int40h:
;	pushf
;	push 	cs
;	;cli
;	call 	DISKETTE_IO_1
;	retn

; DSKETTE ----- 04/21/86 DISKETTE BIOS
; (IBM PC XT Model 286 System BIOS Source Code, 04-21-86)
;

;-- Retro UNIX 386 v1.1 (Kernel v0.2.1.5) ---08/07/2022-------------------------
; DISKETTE I/O
;	THIS INTERFACE PROVIDES ACCESS TO THE 5 1/4 INCH 360 KB,
;	1.2 MB, 720 KB AND 1.44 MB DISKETTE DRIVES.
; INPUT
;	(AH)= 00H RESET DISKETTE SYSTEM
;		HARD RESET TO NEC, PREPARE COMMAND, RECALIBRATE REQUIRED
;		ON ALL DRIVES
;------------------------------------------------------------------------------- 
;	(AH)= 01H  READ THE DESIRED SECTORS INTO MEMORY
;-------------------------------------------------------------------------------
;	(AH)= 02H  WRITE THE DESIRED SECTORS FROM MEMORY
;-------------------------------------------------------------------------------
;
;	REGISTERS FOR READ/WRITE
;	(DL) - DRIVE NUMBER (0-1 ALLOWED, VALUE CHECKED)
;	(DH) - HEAD NUMBER (0-1 ALLOWED, NOT VALUE CHECKED)
;	(CH) - TRACK NUMBER (NOT VALUE CHECKED)
;		MEDIA	DRIVE	TRACK NUMBER
;		320/360	320/360	    0-39
;		320/360	1.2M	    0-39
;		1.2M	1.2M	    0-79
;		720K	720K	    0-79
;		1.44M	1.44M	    0-79	
;	(CL) - 	SECTOR NUMBER (NOT VALUE CHECKED)
;		MEDIA	DRIVE	SECTOR NUMBER
;		320/360	320/360	     1-8/9
;		320/360	1.2M	     1-8/9
;		1.2M	1.2M	     1-15
;		720K	720K	     1-9
;		1.44M	1.44M	     1-18		
;	(AL)	NUMBER OF SECTORS (NOT VALUE CHECKED)
;		MEDIA	DRIVE	MAX NUMBER OF SECTORS
;		320/360	320/360	        8/9
;		320/360	1.2M	        8/9
;		1.2M	1.2M		15
;		720K	720K		9
;		1.44M	1.44M		18
;
;	(EBX) - ADDRESS OF BUFFER
;
;-------------------------------------------------------------------------------
; OUTPUT FOR ALL FUNCTIONS
;	AH = STATUS OF OPERATION
;		STATUS BITS ARE DEFINED IN THE EQUATES FOR @DISKETTE_STATUS
;		VARIABLE IN THE DATA SEGMENT OF THIS MODULE
;	CY = 0	SUCCESSFUL OPERATION (AH=0 ON RETURN)
;	CY = 1	FAILED OPERATION (AH HAS ERROR REASON)
;	FOR READ/WRITE/VERIFY
;		DS,BX,DX,CX PRESERVED
;	NOTE: IF AN ERROR IS REPORTED BY THE DISKETTE CODE, THE APPROPRIATE 
;		ACTION IS TO RESET THE DISKETTE, THEN RETRY THE OPERATION.
;		ON READ ACCESSES, NO MOTOR START DELAY IS TAKEN, SO THAT 
;		THREE RETRIES ARE REQUIRED ON READS TO ENSURE THAT THE 
;		PROBLEM IS NOT DUE TO MOTOR START-UP.
;-------------------------------------------------------------------------------
;
; DISKETTE STATE MACHINE - ABSOLUTE ADDRESS 40:90 (DRIVE A) & 91 (DRIVE B)
;
;   -----------------------------------------------------------------
;   |       |       |       |       |       |       |       |       |
;   |   7   |   6   |   5   |   4   |   3   |   2   |   1   |   0   |
;   |       |       |       |       |       |       |       |       |
;   -----------------------------------------------------------------
;	|	|	|	|	|	|	|	|
;	|	|	|	|	|	-----------------
;	|	|	|	|	|		|
;	|	|	|	|    RESERVED		|
;	|	|	|	|		  PRESENT STATE
;	|	|	|	|	000: 360K IN 360K DRIVE UNESTABLISHED
;	|	|	|	|	001: 360K IN 1.2M DRIVE UNESTABLISHED
;	|	|	|	|	010: 1.2M IN 1.2M DRIVE UNESTABLISHED
;	|	|	|	|	011: 360K IN 360K DRIVE ESTABLISHED
;	|	|	|	|	100: 360K IN 1.2M DRIVE ESTABLISHED
;	|	|	|	|	101: 1.2M IN 1.2M DRIVE ESTABLISHED
;	|	|	|	|	110: RESERVED
;	|	|	|	|	111: NONE OF THE ABOVE
;	|	|	|	|
;	|	|	|	------>	MEDIA/DRIVE ESTABLISHED
;	|	|	|
;	|	|	-------------->	DOUBLE STEPPING REQUIRED
;	|	|					 (360K IN 1.2M DRIVE)
;	|	|
;	------------------------------>	DATA TRANSFER RATE FOR THIS DRIVE:
;
;						00: 500 KBS
;						01: 300 KBS
;						10: 250 KBS
;						11: RESERVED
;
;

struc MD
	.SPEC1:	  resb	1	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	.SPEC2:	  resb	1	; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	.OFF_TIM: resb	1	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	.BYT_SEC: resb	1	; 512 BYTES/SECTOR
	.SEC_TRK: resb	1	; EOT (LAST SECTOR ON TRACK)
	.GAP:	  resb	1	; GAP LENGTH
	.DTL:	  resb	1	; DTL
	.GAP3:	  resb	1	; GAP LENGTH FOR FORMAT
	.FIL_BYT: resb	1	; FILL BYTE FOR FORMAT
	.HD_TIM:  resb	1	; HEAD SETTLE TIME (MILLISECONDS)
	.STR_TIM: resb	1	; MOTOR START TIME (1/8 SECONDS)
	.MAX_TRK: resb	1	; MAX. TRACK NUMBER
	.RATE:	  resb	1	; DATA TRANSFER RATE
endstruc

BIT7OFF	EQU	7FH
BIT7ON	EQU	80H

; 11/07/2022 - (direct call instead of int 13h simulation)
;		Function in AL
;			0 = reset
;			1 = read
;			2 = write
;		Disk drive number in DL
;			0 & 1 = floppy disks	
;			80h .. 83h = hard disks
;		Sector address (LBA) in ECX
;		Buffer address in EBX
;		R/W sector count is (always) 1
;
;		Return:
;			Status in AH (>0 = error code)
;			if CF = 1 -> error code in AH
;			if CF = 0 -> successful
;			AL = undefined
;
;		Modified registers: (only) EAX

; 11/07/2022
;;int13h: ; 16/02/2015
;; 16/02/2015 - 21/02/2015
;int40h:
;	pushfd
;	push 	cs
;	call 	DISKETTE_IO_1
;	retn	

DISKETTE_IO_1:

	;sti				; INTERRUPTS BACK ON
	; 11/07/2022
	; save registers
	push	ebp			; ANY

	; 11/07/2022
	;push	edi			; ANY
	;push	edx			; DRIVE NUMBER (DL)
	;push	ebx			; BUFFER ADDRESS
	;push	ecx			; SECTOR ADDRESS (LBA)
	;push	esi			; ANY

	; 11/07/2022
	mov	ebp, ebx ; buffer address
	mov	byte [DSKETTE_STATUS], 0 ; RESET DISKETTE STATUS
	movzx	edi, dl ; drive number (0 or 1)
	
	or	al, al			; RESET ?
	jnz	short DISKETTE_RW_1	; NO

	call	DSK_RESET

	jmp	short DISKETTE_RW_2	

DISKETTE_RW_1:
	; 12/07/2022
	; 11/07/2022
	; ecx = sector address (LBA, < 2880)
	; ebp = buffer address
	; edi = drive number (0 or 1)
	;  al = function (read = 1 or write = 2)

	mov	dl, al ; *
convert_to_chs:
	;;;
	mov	al, 4 ; MD.SEC_TRK ; sector per track (drv.spt)
	call	GET_PARM
	; 12/07/2022
	mov	dh, ah ; spt
	mov	eax, ecx ; sector address (LBA) 
	div	dh  ; AX/DH
	mov	cl, ah ; sector number - 1
	inc	cl  ; sector number (1 based)
	sub	ch, ch ; head = 0 
	; heads = 2
	shr	al, 1 ; al = al/2
	adc	ch, 0 ; head = 1 or head = 0
	shl	ecx, 16
	mov	cl, al ; track (cylinder)
	mov	ch, dl ; function number 
	mov	esi, ecx ; byte 0 = track, byte 1 = function
			 ; byte 2 = sector, byte 3 = head
	rol	esi, 16
			 ; byte 0 = sector, byte 1 = head
			 ; byte 2 = track, byte 3 = function		
	;;; 
	cmp	dl, 2 ; *
	je	short DISKETTE_W
DISKETTE_R:	
	; dl = 1 ; *
	call	DSK_READ
	jmp	short DISKETTE_RW_2
DISKETTE_W:
	call	DSK_WRITE
DISKETTE_RW_2:
	; 11/07/2022
	; Restore registers
	;pop	esi
	;pop	ecx
	;pop	ebx
	;pop	edx
	;pop	edi

	; 11/07/2022
	pop	ebp
	retn

;-------------------------------------------------------------------------------
; DISK_READ	(AH = 01H)  ; Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;	DISKETTE READ.
;
; ON ENTRY:	EDI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
; 06/02/2015, ES:BX -> EBX (unix386.s)

DSK_READ:
	and	byte [MOTOR_STATUS], 01111111b ; INDICATE A READ OPERATION
	mov	ax, 0E646h		; AX = NEC COMMAND, DMA COMMAND
	;call	RD_WR_VF		; COMMON READ/WRITE/VERIFY
	;retn
	jmp	short RD_WR_VF

;-------------------------------------------------------------------------------
; DISK_WRITE	(AH = 02H)
;	DISKETTE WRITE.
;
; ON ENTRY:	EDI	: DRIVE #
;		SI-HI	: HEAD #
;		SI-LOW	: # OF SECTORS
;		ES	: BUFFER SEGMENT
;		[BP]	: SECTOR #
;		[BP+1]	: TRACK #
;		[BP+2]	: BUFFER OFFSET
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
; 06/02/2015, ES:BX -> EBX (unix386.s)

DSK_WRITE:
	mov	ax, 0C54Ah		; AX = NEC COMMAND, DMA COMMAND
        or	byte [MOTOR_STATUS], 10000000b ; INDICATE WRITE OPERATION
	;;call	RD_WR_VF		; COMMON READ/WRITE/VERIFY
	;;retn
	;jmp	short RD_WR_VF

;-------------------------------------------------------------------------------
; RD_WR_VF
;	COMMON READ, WRITE
;	MAIN LOOP FOR STATE RETRIES.
;
; ON ENTRY:	AH = READ/WRITE NEC PARAMETER
;		AL = READ/WRITE DMA PARAMETER
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

RD_WR_VF:
	; 18/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	push	eax ; 24/12/2021	; SAVE DMA, NEC PARAMETERS
	call	XLAT_NEW		; TRANSLATE STATE TO PRESENT ARCH.
	call	SETUP_STATE		; INITIALIZE START AND END RATE
	pop	eax ; 24/12/2021	; RESTORE READ/WRITE PARAMETER
DO_AGAIN:
	push	eax ; 24/12/2021	; SAVE READ/WRITE PARAMETER
	call	MED_CHANGE		; MEDIA CHANGE AND RESET IF CHANGED
	pop	eax ; 24/12/2021	; RESTORE READ/WRITE PARAMETER
	; 24/12/2021
	jnc	short RWV
	jmp	RWV_END			; MEDIA CHANGE ERROR OR TIME-OUT
RWV:
	push	eax ; 24/12/2021	; SAVE READ/WRITE/VERIFY PARAMETER
	mov	dh, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
	and	dh, RATE_MSK		; KEEP ONLY RATE
	call	CMOS_TYPE		; RETURN DRIVE TYPE IN AL
	; 20/02/2015
	jz	short RWV_ASSUME	; ERROR IN CMOS
	cmp	al, 1			; 40 TRACK DRIVE?
	jne	short RWV_1		; NO, BYPASS CMOS VALIDITY CHECK
	test	byte [DSK_STATE+edi], TRK_CAPA ; CHECK FOR 40 TRACK DRIVE
	jz	short RWV_2		; YES, CMOS IS CORRECT
	;mov	al, 2			; CHANGE TO 1.2M
	; 12/07/2022
	inc	al  ; al = 2
	jmp	short RWV_2
RWV_1:
	; 12/07/2022
	;jb	short RWV_2		; NO DRIVE SPECIFIED, CONTINUE
	test	byte [DSK_STATE+edi], TRK_CAPA ; IS IT REALLY 40 TRACK?
	jnz	short RWV_2		; NO, 80 TRACK
	mov	al, 1			; IT IS 40 TRACK, FIX CMOS VALUE
	; 12/07/2022
	;jmp	short RWV_3
RWV_2:
	; 12/07/2022
	;or	al, al			; TEST FOR NO DRIVE
	;jz	short RWV_ASSUME	; ASSUME TYPE, USE MAX TRACK
RWV_3:
	; 12/07/2022
	;mov	dl, al	; 11/07/2022
	call	DR_TYPE_CHECK		; RTN EBX = MEDIA/DRIVE PARAM TBL.
	jc	short RWV_ASSUME	; TYPE NOT IN TABLE (BAD CMOS)

;-----	SEARCH FOR MEDIA/DRIVE PARAMETER TABLE

	push	edi			; SAVE DRIVE #
	;xor	ebx, ebx		; EBX = INDEX TO DR_TYPE TABLE
	mov	ebx, DR_TYPE
	;mov	ecx, DR_CNT		; ECX = LOOP COUNT
	mov	cl, DR_CNT
RWV_DR_SEARCH:
	;mov	ah, [DR_TYPE+ebx]	; GET DRIVE TYPE
	mov	ah, [ebx]
	and	ah, BIT7OFF		; MASK OUT MSB
	cmp	al, ah			; DRIVE TYPE MATCH?
	; 12/07/2022
	;cmp	dl, ah ; 11/07/2022
	jne	short RWV_NXT_MD	; NO, CHECK NEXT DRIVE TYPE
RWV_DR_FND:
	;mov	edi, [DR_TYPE+ebx+1] 	; EDI = MEDIA/DRIVE PARAMETER TABLE
	inc	ebx
	mov	edi, [ebx]
	dec	ebx
RWV_MD_SEARH:
        cmp	dh, [edi+MD.RATE]       ; MATCH?
	je	short RWV_MD_FND	; YES, GO GET 1ST SPECIFY BYTE
RWV_NXT_MD:
	add	ebx, 5			; CHECK NEXT DRIVE TYPE
	;loop	RWV_DR_SEARCH
	dec	cl
	jnz	short RWV_DR_SEARCH 
	pop	edi			; RESTORE DRIVE #

;-----	ASSUME PRIMARY DRIVE IS INSTALLED AS SHIPPED

RWV_ASSUME:
	mov	ebx, MD_TBL1		; POINT TO 40 TRACK 250 KBS
	test	byte [DSK_STATE+edi], TRK_CAPA ; TEST FOR 80 TRACK
	jz	short RWV_MD_FND1	; MUST BE 40 TRACK
	mov	ebx, MD_TBL3		; POINT TO 80 TRACK 500 KBS
	jmp	short RWV_MD_FND1	; GO SPECIFY PARAMTERS

;-----	EBX POINTS TO MEDIA/DRIVE PARAMETER TABLE
	 			
RWV_MD_FND:
	mov	ebx, edi		; EBX = MEDIA/DRIVE PARAMETER TABLE
	pop	edi			; RESTORE DRIVE #
	
;-----	SEND THE SPECIFY COMMAND TO THE CONTROLLER

RWV_MD_FND1:
	call	SEND_SPEC_MD
	call	CHK_LASTRATE		; ZF=1 ATTEMP RATE IS SAME AS LAST RATE
	jz	short RWV_DBL		; YES,SKIP SEND RATE COMMAND
	call	SEND_RATE		; SEND DATA RATE TO NEC
RWV_DBL:
	push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	call	SETUP_DBL		; CHECK FOR DOUBLE STEP
	pop	ebx			; RESTORE ADDRESS
	jc	short CHK_RET		; ERROR FROM READ ID, POSSIBLE RETRY
	;pop	eax ; 24/12/2021	; RESTORE NEC COMMAND
	;push	eax ; 24/12/2021	; SAVE NEC COMMAND
	; 08/07/2022
	mov	eax, [esp]
	; 18/07/2022
	;push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	call	DMA_SETUP		; SET UP THE DMA
	;pop	ebx
	pop	eax ; 24/12/2021	; RESTORE NEC COMMAND
	jc	short RWV_BAC		; CHECK FOR DMA BOUNDARY ERROR
	push	eax ; 24/12/2021	; SAVE NEC COMMAND
	push	ebx			; SAVE MEDIA/DRIVE PARAM TBL ADDRESS
	call	NEC_INIT		; INITIALIZE NEC
	pop	ebx			; RESTORE ADDRESS
	jc	short CHK_RET		; ERROR - EXIT
	call	RWV_COM			; OP CODE COMMON TO READ/WRITE
	jc	short CHK_RET		; ERROR - EXIT
	call	NEC_TERM		; TERMINATE, GET STATUS, ETC.
CHK_RET:
	call	RETRY			; CHECK FOR, SETUP RETRY
	pop	eax ; 24/12/2021	; RESTORE READ/WRITE PARAMETER
	jnc	short RWV_END		; CY = 0 NO RETRY
        jmp	DO_AGAIN                ; CY = 1 MEANS RETRY
RWV_END:
	call	DSTATE			; ESTABLISH STATE IF SUCCESSFUL
	call	NUM_TRANS		; AL = NUMBER TRANSFERRED
RWV_BAC:
	; 08/07/2022			; BAD DMA ERROR ENTRY
	;push	eax ; 24/12/2021	; SAVE NUMBER TRANSFERRED
	;CALL	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	;pop	eax ; 24/12/2021	; RESTORE NUMBER TRANSFERRED
	;;call	SETUP_END		; VARIOUS CLEANUPS
	;;retn
	;jmp	SETUP_END

;-------------------------------------------------------------------------------
; SETUP_END
;	RESTORES @MOTOR_COUNT TO PARAMETER PROVIDED IN TABLE 
;	AND LOADS @DSKETTE_STATUS TO AH, AND SETS CY.
;
; ON EXIT:
;	AH, @DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
SETUP_END:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5) 
	;mov	dl, 2			; GET THE MOTOR WAIT PARAMETER
	;push	ax			; SAVE NUMBER TRANSFERRED
	push	eax ; 24/12/2021
	mov	al, 2 ; 08/07/2022
	call	GET_PARM
	mov	[MOTOR_COUNT], ah	; STORE UPON RETURN
	;pop	ax			; RESTORE NUMBER TRANSFERRED
	pop	eax ; 24/12/2021
	mov	ah, [DSKETTE_STATUS]	; GET STATUS OF OPERATION
	or	ah, ah			; CHECK FOR ERROR
	jz	short NUN_ERR		; NO ERROR
	xor 	al, al			; CLEAR NUMBER RETURNED
;NUN_ERR: 
	cmp	ah, 1			; SET THE CARRY FLAG TO INDICATE
	cmc				; SUCCESS OR FAILURE
NUN_ERR:
	retn

;-------------------------------------------------------------------------------
; DISK_RESET	(AH = 00H)	
;		RESET THE DISKETTE SYSTEM.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
DSK_RESET:
	; 17/07/2022
	; 12/07/2022
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	dx, 03F2h		; ADAPTER CONTROL PORT
	cli				; NO INTERRUPTS
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	and	al, 00111111b		; KEEP SELECTED AND MOTOR ON BITS
	rol	al, 4			; MOTOR VALUE TO HIGH NIBBLE
					; DRIVE SELECT TO LOW NIBBLE
	or	al, 00001000b		; TURN ON INTERRUPT ENABLE
	out	dx, al			; RESET THE ADAPTER
	mov	byte [SEEK_STATUS], 0	; SET RECALIBRATE REQUIRED ON ALL DRIVES
	;JMP	$+2			; WAIT FOR I/O
	;JMP	$+2			; WAIT FOR I/O (TO INSURE MINIMUM
					;      PULSE WIDTH)
	; 19/12/2014
	NEWIODELAY

	; 17/12/2014 
	; AWARD BIOS 1999 - RESETDRIVES (ADISK.ASM)
	mov	ecx, WAITCPU_RESET_ON	; cx = 21 -- Min. 14 micro seconds !?
wdw1:
	NEWIODELAY   ; 27/02/2015
	loop	wdw1
	;
	or	al, 00000100b		; TURN OFF RESET BIT
	out	dx, al			; RESET THE ADAPTER
	; 16/12/2014
	IODELAY
	;
	;sti				; ENABLE THE INTERRUPTS
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	jc	short DR_ERR		; IF ERROR, RETURN IT
	;mov	cx, 11000000b		; CL = EXPECTED @NEC_STATUS
	; 12/07/2022
	;xor	ecx, ecx
	; 17/07/2022
	;xor	ch, ch
	mov	cl, 11000000b
NXT_DRV:
	; 24/12/2021
	push	ecx			; SAVE FOR CALL
	mov	eax, DR_POP_ERR 	; LOAD NEC_OUTPUT ERROR ADDRESS
	push	eax			;
	mov	ah, 08h			; SENSE INTERRUPT STATUS COMMAND
	call	NEC_OUTPUT
	pop	eax			; THROW AWAY ERROR RETURN
	call	RESULTS			; READ IN THE RESULTS
	; 24/12/2021
	pop	ecx			; RESTORE AFTER CALL
	jc	short DR_ERR		; ERROR RETURN
	cmp	cl, [NEC_STATUS]	; TEST FOR DRIVE READY TRANSITION
	jnz	short DR_ERR		; EVERYTHING OK
	inc	cl			; NEXT EXPECTED @NEC_STATUS
	cmp	cl, 11000011b		; ALL POSSIBLE DRIVES CLEARED
	jbe	short NXT_DRV		; FALL THRU IF 11000100B OR >
	;
	call	SEND_SPEC		; SEND SPECIFY COMMAND TO NEC
RESBAC:
	call	SETUP_END		; VARIOUS CLEANUPS
	
	; 11/07/2022
	; CF = 1 -> error (error code in AH)
	; CF = 0 -> OK
	
	;; 24/12/2021
	;mov	ebx, esi		; GET SAVED AL TO BL
	;; 11/07/2022
	;; byte 0 = sector, byte 1 = head, byte 2 = track, byte 3 = function 
	;rol	ebx, 8
	; bl = function (reset = 0)
	;
	;mov	al, bl			; PUT BACK FOR RETURN
	
	; 11/07/2022  
	mov	al, 0	; (reset function = 0)

	retn

DR_POP_ERR:
	; 24/12/2021
	pop	ecx			; CLEAR STACK
DR_ERR:
	or	byte [DSKETTE_STATUS], BAD_NEC ; SET ERROR CODE
	jmp	short RESBAC		; RETURN FROM RESET

;-------------------------------------------------------------------------------
; FNC_ERR
;	INVALID FUNCTION REQUESTED OR INVALID DRIVE: 
;	SET BAD COMMAND IN STATUS.
;
; ON EXIT: 	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

	; 11/07/2022 - not needed (because diskio is used by kernel only)
	
;FNC_ERR:				; INVALID FUNCTION REQUEST
;	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;	; 24/12/2021
;	mov	eax, esi		; RESTORE AL
;	mov	ah, BAD_CMD		; SET BAD COMMAND ERROR
;	mov	[DSKETTE_STATUS], ah	; STORE IN DATA AREA
;	stc				; SET CARRY INDICATING ERROR
;	retn

;----------------------------------------------------------------
; DR_TYPE_CHECK							:
;	CHECK IF THE GIVEN DRIVE TYPE IN REGISTER (AL)		:
;	IS SUPPORTED IN BIOS DRIVE TYPE TABLE			:
; ON ENTRY:							:
;	AL = DRIVE TYPE						:
; ON EXIT:							:
;	CY = 0 	DRIVE TYPE SUPPORTED				:
;	     EBX = OFFSET TO MEDIA/DRIVE PARAMETER TABLE	:
;	CY = 1	DRIVE TYPE NOT SUPPORTED 			:
; REGISTERS ALTERED: EBX, AH ; 11/07/2022 			:
;----------------------------------------------------------------
DR_TYPE_CHECK:
	; 12/07/2022
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	; 24/12/2021
	;push	eax ; 11/07/2022
	;push	ecx ; 08/07/2022
	;xor	ebx,ebx			; EBX = INDEX TO DR_TYPE TABLE
	mov	ebx, DR_TYPE
	;;mov	ecx, DR_CNT		; ECX = LOOP COUNT
	;mov	cl, DR_CNT
	mov	ah, DR_CNT ; 11/07/2022
TYPE_CHK:	
	;;mov	ah, [DR_TYPE+ebx]	; GET DRIVE TYPE
	;mov	ah, [ebx]
	;cmp	al, ah			; DRIVE TYPE MATCH?
	cmp	al, [ebx] ; 11/07/2022
	je	short DR_TYPE_VALID	; YES, RETURN WITH CARRY RESET
	; 16/02/2015 (32 bit address modification)
	add	ebx, 5			; CHECK NEXT DRIVE TYPE
	;loop	TYPE_CHK
	;dec	cl
	dec	ah ; 11/07/2022
	jnz	short TYPE_CHK
	;
	mov	ebx, MD_TBL6		; 1.44MB fd parameter table
					; Default for GET_PARM (11/12/2014)
	;
	stc				; DRIVE TYPE NOT FOUND IN TABLE
	;jmp	short TYPE_RTN
	; 12/07/2022
	retn
DR_TYPE_VALID:
	;mov	ebx, [DR_TYPE+ebx+1] 	; EBX = MEDIA TABLE
	inc	ebx
	mov	ebx, [ebx]
TYPE_RTN:
	;pop	ecx ; 08/07/2022
	; 24/12/2021
	;pop	eax ; 11/07/2022
	retn
		
;----------------------------------------------------------------
; SEND_SPEC							:
;	SEND THE SPECIFY COMMAND TO CONTROLLER USING DATA FROM	:
;	THE DRIVE PARAMETER TABLE POINTED BY @DISK_POINTER	:
; ON ENTRY:	@DISK_POINTER = DRIVE PARAMETER TABLE		:
; ON EXIT:	NONE						:
; REGISTERS ALTERED: ECX, EDX					:
;----------------------------------------------------------------
SEND_SPEC:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	push	eax			; SAVE EAX
	mov	eax, SPECBAC		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	mov	ah, 03h			; SPECIFY COMMAND
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	;sub	dl, dl			; FIRST SPECIFY BYTE
	sub	al, al ; 08/07/2022
	call	GET_PARM		; GET PARAMETER TO AH
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	;mov	dl, 1			; SECOND SPECIFY BYTE
	mov	al, 1 ; 08/07/2022
	call	GET_PARM		; GET PARAMETER TO AH
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	pop	eax			; POP ERROR RETURN
SPECBAC:
	pop	eax			; RESTORE ORIGINAL EAX VALUE
	retn

;----------------------------------------------------------------
; SEND_SPEC_MD							:
;	SEND THE SPECIFY COMMAND TO CONTROLLER USING DATA FROM	:
;	THE MEDIA/DRIVE PARAMETER TABLE POINTED BY (EBX)	:
; ON ENTRY:	EBX = MEDIA/DRIVE PARAMETER TABLE		:
; ON EXIT:	NONE						:
; REGISTERS ALTERED: EAX ; 11/07/2022				:
;----------------------------------------------------------------
SEND_SPEC_MD:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	eax ; 11/07/2022	; SAVE RATE DATA
	mov	eax, SPEC_ESBAC		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	mov	ah, 03h			; SPECIFY COMMAND
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
        mov	ah, [ebx+MD.SPEC1]      ; GET 1ST SPECIFY BYTE
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
        mov	ah, [ebx+MD.SPEC2]      ; GET SECOND SPECIFY BYTE
	call	NEC_OUTPUT		; OUTPUT THE COMMAND
	pop	eax			; POP ERROR RETURN
SPEC_ESBAC:
	;pop	eax ; 11/07/2022	; RESTORE ORIGINAL EAX VALUE
	retn

;-------------------------------------------------------------------------------
; XLAT_NEW  
;	TRANSLATES DISKETTE STATE LOCATIONS FROM COMPATIBLE
;	MODE TO NEW ARCHITECTURE.
;
; ON ENTRY:	EDI = DRIVE #
;-------------------------------------------------------------------------------
XLAT_NEW:
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;cmp	edi, 1				; VALID DRIVE
	;ja	short XN_OUT			; IF INVALID BACK
	;
	cmp	byte [DSK_STATE+edi], 0		; NO DRIVE ?
	jz	short DO_DET			; IF NO DRIVE ATTEMPT DETERMINE
	;
	;mov	al, [HF_CNTRL]			; DRIVE INFORMATION
	;mov	ecx, edi			; ECX = DRIVE NUMBER
	;or	cl, cl
	;jz	short XN_0  ; 08/07/2022
	;shl	cl, 2				; CL = SHIFT COUNT, A=0, B=4
	;;mov	al, [HF_CNTRL]			; DRIVE INFORMATION
	;ror	al, cl				; TO LOW NIBBLE
;XN_0:	
	;and	al, DRV_DET+FMT_CAPA+TRK_CAPA	; KEEP DRIVE BITS
        ;and	byte [DSK_STATE+edi], ~(DRV_DET+FMT_CAPA+TRK_CAPA)
	;or	[DSK_STATE+edi], al		; UPDATE DRIVE STATE
XN_OUT:
	retn
DO_DET:
	;;call	DRIVE_DET			; TRY TO DETERMINE
	;;retn
	;jmp	DRIVE_DET

;-------------------------------------------------------------------------------
; DRIVE_DET
;	DETERMINES WHETHER DRIVE IS 80 OR 40 TRACKS AND
;	UPDATES STATE INFORMATION ACCORDINGLY.
; ON ENTRY:	EDI = DRIVE #
;-------------------------------------------------------------------------------
DRIVE_DET:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	call	MOTOR_ON		; TURN ON MOTOR IF NOT ALREADY ON
	call	RECAL			; RECALIBRATE DRIVE
	jc	short DD_BAC		; ASSUME NO DRIVE PRESENT
	mov	ch, TRK_SLAP		; SEEK TO TRACK 48
	call	SEEK
	jc	short DD_BAC		; ERROR NO DRIVE
	mov	ch, QUIET_SEEK+1	; SEEK TO TRACK 10
SK_GIN:
	dec	ch			; DECREMENT TO NEXT TRACK
	;push	cx		
	; 24/12/2021
	push	ecx			; SAVE TRACK
	call	SEEK
	jc	short POP_BAC		; POP AND RETURN
	mov	eax, POP_BAC		; LOAD NEC OUTPUT ERROR ADDRESS
	push	eax
	mov	ah, SENSE_DRV_ST	; SENSE DRIVE STATUS COMMAND BYTE
	call	NEC_OUTPUT		; OUTPUT TO NEC
	; 08/07/2022
	mov	eax, edi		; AL = DRIVE
	mov	ah, al			; AH = DRIVE
	call	NEC_OUTPUT		; OUTPUT TO NEC
	call	RESULTS			; GO GET STATUS
	pop	eax			; THROW AWAY ERROR ADDRESS
	;pop	cx			; RESTORE TRACK
	; 24/12/2021
	pop	ecx
	test	byte [NEC_STATUS], HOME	; TRACK 0 ?
	jz	short SK_GIN		; GO TILL TRACK 0
	or	ch, ch			; IS HOME AT TRACK 0
	jz	short IS_80		; MUST BE 80 TRACK DRIVE

;	DRIVE IS A 360; SET DRIVE TO DETERMINED;
;	SET MEDIA TO DETERMINED AT RATE 250.

	or	byte [DSK_STATE+edi], DRV_DET+MED_DET+RATE_250
	retn				; ALL INFORMATION SET
IS_80:
	or	byte [DSK_STATE+edi], TRK_CAPA ; SETUP 80 TRACK CAPABILITY
DD_BAC:
	retn
POP_BAC:
	;pop	cx			; THROW AWAY
	; 24/12/2021
	pop	ecx
	retn

;-------------------------------------------------------------------------------
; SETUP_STATE:	INITIALIZES START AND END RATES.
;-------------------------------------------------------------------------------
SETUP_STATE:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	test	byte [DSK_STATE+edi], MED_DET ; MEDIA DETERMINED ?
	jnz	short J1C		; NO STATES IF DETERMINED
       	mov	ax, (RATE_500*256)+RATE_300 ; AH = START RATE, AL = END RATE
	test	byte [DSK_STATE+edi], DRV_DET ; DRIVE ?
	jz	short AX_SET		; DO NOT KNOW DRIVE
	test	byte [DSK_STATE+edi], FMT_CAPA ; MULTI-RATE?
	jnz	short AX_SET		; JUMP IF YES
        mov	ax, RATE_250*257	; START A END RATE 250 FOR 360 DRIVE
AX_SET:	
	and	byte [DSK_STATE+edi], ~(RATE_MSK+DBL_STEP) ; TURN OFF THE RATE
	or	[DSK_STATE+edi], ah	; RATE FIRST TO TRY
	and	byte [LASTRATE], ~STRT_MSK ; ERASE LAST TO TRY RATE BITS
	ror	al, 4			; TO OPERATION LAST RATE LOCATION
	or	[LASTRATE], al		; LAST RATE
J1C:	
	retn

;-------------------------------------------------------------------------------
; MED_CHANGE	
;	CHECKS FOR MEDIA CHANGE, RESETS MEDIA CHANGE, 
;	CHECKS MEDIA CHANGE AGAIN.
;
; ON EXIT:	CY = 1 MEANS MEDIA CHANGE OR TIMEOUT
;		@DSKETTE_STATUS = ERROR CODE
;-------------------------------------------------------------------------------
MED_CHANGE:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	call	READ_DSKCHNG		; READ DISK CHANCE LINE STATE
	jz	short MC_OUT		; BYPASS HANDLING DISK CHANGE LINE
	and	byte [DSK_STATE+edi], ~MED_DET ; CLEAR STATE FOR THIS DRIVE

;	THIS SEQUENCE ENSURES WHENEVER A DISKETTE IS CHANGED THAT
;	ON THE NEXT OPERATION THE REQUIRED MOTOR START UP TIME WILL
;	BE WAITED. (DRIVE MOTOR MAY GO OFF UPON DOOR OPENING).

	mov	ecx, edi		; CL = DRIVE #
	mov	al, 1			; MOTOR ON BIT MASK
	shl	al, cl			; TO APPROPRIATE POSITION
	not	al			; KEEP ALL BUT MOTOR ON
	cli				; NO INTERRUPTS
	and	[MOTOR_STATUS], al	; TURN MOTOR OFF INDICATOR
	sti				; INTERRUPTS ENABLED
	call	MOTOR_ON		; TURN MOTOR ON

;-----	THIS SEQUENCE OF SEEKS IS USED TO RESET DISKETTE CHANGE SIGNAL

	call	DSK_RESET		; RESET NEC
	mov	ch, 1			; MOVE TO CYLINDER 1
	call	SEEK			; ISSUE SEEK
	xor	ch, ch			; MOVE TO CYLINDER 0
	call	SEEK			; ISSUE SEEK
	mov	byte [DSKETTE_STATUS], MEDIA_CHANGE ; STORE IN STATUS
OK1:
	call	READ_DSKCHNG		; CHECK MEDIA CHANGED AGAIN
	jz	short OK2		; IF ACTIVE, NO DISKETTE, TIMEOUT
OK4:
	mov	byte [DSKETTE_STATUS], TIME_OUT ; TIMEOUT IF DRIVE EMPTY
OK2:		
	stc				; MEDIA CHANGED, SET CY
	retn
MC_OUT:
	;clc	; 08/07/2022		; NO MEDIA CHANGED, CLEAR CY
	retn

;-------------------------------------------------------------------------------
; SEND_RATE
;	SENDS DATA RATE COMMAND TO NEC
; ON ENTRY:	EDI = DRIVE #
; ON EXIT:	NONE
; REGISTERS ALTERED: EDX, EAX ; 11/07/2022
;-------------------------------------------------------------------------------
SEND_RATE:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	ax			; SAVE REG.
	; 24/12/2021
	;push	eax ; 11/07/2022
	and	byte [LASTRATE], ~SEND_MSK ; ELSE CLEAR LAST RATE ATTEMPTED
	mov	al, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
	and	al, SEND_MSK		; KEEP ONLY RATE BITS
	or	[LASTRATE], al		; SAVE NEW RATE FOR NEXT CHECK
	rol	al, 2			; MOVE TO BIT OUTPUT POSITIONS
	mov	dx, 03F7h		; OUTPUT NEW DATA RATE
	out	dx, al
	;pop	ax			; RESTORE REG.
	; 24/12/2021
	;pop	eax ; 11/07/2022
	retn

;-------------------------------------------------------------------------------
; CHK_LASTRATE
;	CHECK PREVIOUS DATE RATE SNT TO THE CONTROLLER.
; ON ENTRY:
;	EDI = DRIVE #
; ON EXIT:
;	ZF =  1 DATA RATE IS THE SAME AS THE LAST RATE SENT TO NEC
;	ZF =  0 DATA RATE IS DIFFERENT FROM LAST RATE
; REGISTERS ALTERED: EAX ; 11/07/2022
;-------------------------------------------------------------------------------
CHK_LASTRATE:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	ax			; SAVE REG.
	; 24/12/2021
	;push	eax ; 11/07/2022
	mov	ah, [LASTRATE] ; 08/07/2022 (BugFix) 
					; GET LAST DATA RATE SELECTED
	mov	al, [DSK_STATE+edi]	; GET RATE STATE OF THIS DRIVE
       	and	ax, SEND_MSK*257        ; KEEP ONLY RATE BITS OF BOTH
	cmp	al, ah			; COMPARE TO PREVIOUSLY TRIED
					; ZF = 1 RATE IS THE SAME
	;pop	ax			; RESTORE REG.
	; 24/12/2021
	;pop	eax ; 11/07/2022
	retn

;-------------------------------------------------------------------------------
; DMA_SETUP
;	THIS ROUTINE SETS UP THE DMA FOR READ/WRITE/VERIFY OPERATIONS.
;
; ON ENTRY:	AL = DMA COMMAND
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

; SI = Head #, # of Sectors or DASD Type

; 22/08/2015
; 08/02/2015 - Protected Mode Modification
; 06/02/2015 - 07/02/2015
; NOTE: Buffer address must be in 1st 16MB of Physical Memory (24 bit limit).
; (DMA Addres = Physical Address)
; (Retro UNIX 386 v1 Kernel/System Mode Virtual Address = Physical Address)
;
; 04/02/2016 (clc)
; 20/02/2015 modification (source: AWARD BIOS 1999, DMA_SETUP)
; 16/12/2014 (IODELAY)

DMA_SETUP:
	; 18/07/2022
	; 11/07/2022
	;	ebp = buffer address
	
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;; 20/02/2015
	;mov	edx, [ebp+4] ; 11/07/2022 ; Buffer address
	; 08/07/2022 - not needed for Retro UNIX 386 v1.1
	;test	edx, 0FF000000h		; 16 MB limit (22/08/2015, bugfix)
	;jnz	short dma_bnd_err_stc

	; al = dma command
	
;	; 18/07/2022
;	;	(512 bytes/sector only!) 
;	; 24/12/2021
;	push	eax			; DMA command
;	;push	edx ; 11/07/2022	; *
;	;mov	dl, 3			; GET BYTES/SECTOR PARAMETER
;	mov	al, 3 ; 08/07/2022
;	call	GET_PARM		; 
;	mov	cl, ah 			; SHIFT COUNT (0=128, 1=256, 2=512 ETC)
;	;mov	ax, si			; Sector count
;	;mov	ah, al			; AH = # OF SECTORS
;	;sub	al, al			; AL = 0, AX = # SECTORS * 256
;	;shr	ax, 1			; AX = # SECTORS * 128
;	;shl	ax, cl			; SHIFT BY PARAMETER VALUE
;	; 08/07/2022
;	; 24/12/2021
;	;mov	edx, esi
;	sub	eax, eax
;	;mov	ah, dl
;	;shr	eax, 1
;	mov	al, 128
;	shl	eax, cl
;	;
;	dec	eax			; -1 FOR DMA VALUE
;	mov	ecx, eax
;	;pop	edx ; 11/07/2022	; *
;	; 24/12/2021
;	pop	eax

	; 08/07/2022
	;cmp	al, 42h
        ;jne	short NOT_VERF
	;mov	edx, 0FF0000h
	;jmp	short J33
;NOT_VERF:
	; 11/07/2022
	mov	edx, ebp
	;
	;add	dx, cx			; check for (64K) overflow
	; 18/07/2022
	; (512 bytes/sector)
	add	dx, 511 
	jc	short dma_bnd_err
	;
	;sub	dx, cx ; 11/07/2022	; Restore start address
J33:
	; 08/07/2022
	cli				; DISABLE INTERRUPTS DURING DMA SET-UP
	out	DMA+12, al		; SET THE FIRST/LAST F/F
	IODELAY				; WAIT FOR I/O
	out	DMA+11, al		; OUTPUT THE MODE BYTE
	;mov	eax, edx		; Buffer address
	; 11/07/2022
	mov	eax, ebp ; buffer address
	out	DMA+4, al		; OUTPUT LOW ADDRESS
	IODELAY				; WAIT FOR I/O
	mov	al, ah
	out	DMA+4, al		; OUTPUT HIGH ADDRESS
	shr	eax, 16
	IODELAY				; I/O WAIT STATE
	out	081h, al		; OUTPUT highest BITS TO PAGE REGISTER
	IODELAY
	;;mov	ax, cx			; Byte count - 1
	;mov	al, cl
	; 18/07/2022
	; (Byte count - 1 = 511)
	mov	al, 0FFh ; 511-256
	out	DMA+5, al		; LOW BYTE OF COUNT
	IODELAY				; WAIT FOR I/O
	;;mov	al, ah
	;mov	al, ch
	; 18/07/2022
	mov	al, 1 ; 256
	out	DMA+5, al		; HIGH BYTE OF COUNT
	IODELAY
	sti				; RE-ENABLE INTERRUPTS
	mov	al, 2			; MODE FOR 8237
	out	DMA+10, al		; INITIALIZE THE DISKETTE CHANNEL
	
	clc	; 04/02/2016
	
	retn

	; 18/07/2022
;dma_bnd_err_stc:
;	stc

dma_bnd_err:
	mov	byte [DSKETTE_STATUS], DMA_BOUNDARY ; SET ERROR
	retn				; CY SET BY ABOVE IF ERROR

;-------------------------------------------------------------------------------
; NEC_INIT	
;	THIS ROUTINE SEEKS TO THE REQUESTED TRACK AND INITIALIZES
;	THE NEC FOR THE READ/WRITE/VERIFY/FORMAT OPERATION.
;
; ON ENTRY:	AH = NEC COMMAND TO BE PERFORMED
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
NEC_INIT:
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	ax			; SAVE NEC COMMAND
	; 24/12/2021
	push	eax
	call	MOTOR_ON		; TURN MOTOR ON FOR SPECIFIC DRIVE

;-----	DO THE SEEK OPERATION

	;mov	ch, [ebp+1]		; CH = TRACK #
	; 11/07/2022
	mov	ecx, esi ; byte 2 = track, byte = 1 head, byte 0 = sector
	shr	ecx, 8
	; ch = track #	

	call	SEEK			; MOVE TO CORRECT TRACK
	;pop	ax			; RECOVER COMMAND
	; 24/12/2021
	pop	eax
	jc	short ER_1		; ERROR ON SEEK
	mov	ebx, ER_1		; LOAD ERROR ADDRESS
	push	ebx			; PUSH NEC_OUT ERROR RETURN

;-----	SEND OUT THE PARAMETERS TO THE CONTROLLER

	call	NEC_OUTPUT		; OUTPUT THE OPERATION COMMAND
	mov	eax, esi		; AH = HEAD #
	mov	ebx, edi		; BL = DRIVE #
	sal	ah, 2			; MOVE IT TO BIT 2
	and	ah, 00000100b		; ISOLATE THAT BIT
	or	ah, bl			; OR IN THE DRIVE NUMBER
	call	NEC_OUTPUT		; FALL THRU CY SET IF ERROR
	pop	ebx			; THROW AWAY ERROR RETURN
ER_1:
	retn

;-------------------------------------------------------------------------------
; RWV_COM
;	THIS ROUTINE SENDS PARAMETERS TO THE NEC SPECIFIC TO THE 
;	READ/WRITE/VERIFY OPERATIONS.
;
; ON ENTRY:	EBX = ADDRESS OF MEDIA/DRIVE PARAMETER TABLE
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
RWV_COM:
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	eax, ER_2		; LOAD ERROR ADDRESS
	push	eax			; PUSH NEC_OUT ERROR RETURN
	;mov	ah, [ebp+1]		; OUTPUT TRACK #
	; 11/07/2022
	mov	eax, esi ; byte 0 = sector, byte 1 = head, byte 2 = track
	shr	eax, 8
	; ah = track # 
	call	NEC_OUTPUT
	mov	eax, esi		; OUTPUT HEAD #
	; ah = head #
	call	NEC_OUTPUT
        ;mov	ah, [ebp]		; OUTPUT SECTOR #
	mov	eax, esi
	mov	ah, al
 	; ah = sector #
	CALL	NEC_OUTPUT
	;mov	dl, 3			; BYTES/SECTOR PARAMETER FROM BLOCK
	mov	al, 3 ; 08/07/2022
	call	GET_PARM 		; .. TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
	;mov	dl, 4			; EOT PARAMETER FROM BLOCK
	mov	al, 4 ; 08/07/2022
	call	GET_PARM 		; .. TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
        mov	ah, [ebx+MD.GAP]	; GET GAP LENGTH
_R15:
	call	NEC_OUTPUT
	;mov	dl, 6			; DTL PARAMETER PROM BLOCK
	mov	al, 6 ; 08/07/2022
	call	GET_PARM		; .. TO THE NEC
	call	NEC_OUTPUT		; OUTPUT TO CONTROLLER
	pop	eax			; THROW AWAY ERROR EXIT
ER_2:
	retn

;-------------------------------------------------------------------------------
; NEC_TERM
;	THIS ROUTINE WAITS FOR THE OPERATION THEN ACCEPTS THE STATUS 
;	FROM THE NEC FOR THE READ/WRITE/VERIFY/FORWAT OPERATION.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------

NEC_TERM:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;-----	LET THE OPERATION HAPPEN

	push	esi			; SAVE HEAD #, # OF SECTORS
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	pushfd	; 24/12/2021
	call	RESULTS			; GET THE NEC STATUS
	jc	short SET_END_POP
	popfd	; 24/12/2021
	jc	short SET_END		; LOOK FOR ERROR

;-----	CHECK THE RESULTS RETURNED BY THE CONTROLLER

	cld				; SET THE CORRECT DIRECTION
	mov	esi, NEC_STATUS		; POINT TO STATUS FIELD
	lodsb				; GET ST0
	and	al, 11000000b		; TEST FOR NORMAL TERMINATION
	jz	short SET_END
	cmp	al, 01000000b		; TEST FOR ABNORMAL TERMINATION
	jnz	short J18		; NOT ABNORMAL, BAD NEC

;-----	ABNORMAL TERMINATION, FIND OUT WHY

	lodsb				; GET ST1
	sal	al, 1			; TEST FOR EDT FOUND
	mov	ah, RECORD_NOT_FND
	jc	short J19
	sal	al, 2
	mov	ah, BAD_CRC
	jc	short J19
	sal	al, 1			; TEST FOR DMA OVERRUN
	mov	ah, BAD_DMA
	jc	short J19
	sal	al, 2			; TEST FOR RECORD NOT FOUND
	mov	ah, RECORD_NOT_FND
	jc	short J19
	sal	al, 1
	mov	ah, WRITE_PROTECT	; TEST FOR WRITE_PROTECT
	jc	short J19
	sal	al, 1			; TEST MISSING ADDRESS MARK
	mov	ah, BAD_ADDR_MARK
	jc	short J19

;----- 	NEC MUST HAVE FAILED
J18:
	mov	ah, BAD_NEC
J19:
	or	[DSKETTE_STATUS], ah
SET_END:
	cmp	byte [DSKETTE_STATUS], 1 ; SET ERROR CONDITION
	cmc
	pop	esi
	retn				; RESTORE HEAD #, # OF SECTORS

SET_END_POP:
	popfd	; 24/12/2021
	jmp	short SET_END

;-------------------------------------------------------------------------------
; DSTATE:	ESTABLISH STATE UPON SUCCESSFUL OPERATION.
;-------------------------------------------------------------------------------
DSTATE:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	cmp	byte [DSKETTE_STATUS], 0 ; CHECK FOR ERROR
	jne	short SETBAC		; IF ERROR JUMP
	or	byte [DSK_STATE+edi], MED_DET ; NO ERROR, MARK MEDIA AS DETERMINED
	test	byte [DSK_STATE+edi], DRV_DET ; DRIVE DETERMINED ?
	jnz	short SETBAC		; IF DETERMINED NO TRY TO DETERMINE
	mov	al, [DSK_STATE+edi]	; LOAD STATE
	and	al, RATE_MSK		; KEEP ONLY RATE
	cmp	al, RATE_250		; RATE 250 ?
	jne	short M_12		; NO, MUST BE 1.2M OR 1.44M DRIVE

;----- 	CHECK IF IT IS 1.44M

	call	CMOS_TYPE		; RETURN DRIVE TYPE IN (AL)
	;;20/02/2015
	;;jc	short M_12		; CMOS BAD
	jz	short M_12 ;; 20/02/2015
	cmp	al, 4			; 1.44MB DRIVE ?
	je	short M_12		; YES
M_720:
	and	byte [DSK_STATE+edi], ~FMT_CAPA ; TURN OFF FORMAT CAPABILITY
	or	byte [DSK_STATE+edi], DRV_DET  ; MARK DRIVE DETERMINED
	jmp	short SETBAC		; BACK
M_12:	
	or	byte [DSK_STATE+edi], DRV_DET+FMT_CAPA 
					; TURN ON DETERMINED & FMT CAPA
SETBAC:
	retn

;-------------------------------------------------------------------------------
; RETRY	
;	DETERMINES WHETHER A RETRY IS NECESSARY. 
;	IF RETRY IS REQUIRED THEN STATE INFORMATION IS UPDATED FOR RETRY.
;
; ON EXIT:	CY = 1 FOR RETRY, CY = 0 FOR NO RETRY
;-------------------------------------------------------------------------------
RETRY:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	cmp	byte [DSKETTE_STATUS], 0 ; GET STATUS OF OPERATION
	je	short NO_RETRY		; SUCCESSFUL OPERATION
	cmp	byte [DSKETTE_STATUS], TIME_OUT ; IF TIME OUT NO RETRY
	je	short NO_RETRY
	mov	ah, [DSK_STATE+edi]	; GET MEDIA STATE OF DRIVE
	test	ah, MED_DET		; ESTABLISHED/DETERMINED ?
	jnz	short NO_RETRY		; IF ESTABLISHED STATE THEN TRUE ERROR
	and	ah, RATE_MSK		; ISOLATE RATE
	mov	ch, [LASTRATE]		; GET START OPERATION STATE
	rol	ch, 4			; TO CORRESPONDING BITS
	and	ch, RATE_MSK		; ISOLATE RATE BITS
	cmp	ch, ah			; ALL RATES TRIED
	je	short NO_RETRY		; IF YES, THEN TRUE ERROR

;	SETUP STATE INDICATOR FOR RETRY ATTEMPT TO NEXT RATE
;	 00000000B (500) -> 10000000B	(250)
;	 10000000B (250) -> 01000000B	(300)
;	 01000000B (300) -> 00000000B	(500)

	cmp	ah, RATE_500+1		; SET CY FOR RATE 500
	rcr	ah, 1			; TO NEXT STATE
	and	ah, RATE_MSK		; KEEP ONLY RATE BITS
	and	byte [DSK_STATE+edi], ~(RATE_MSK+DBL_STEP)
					; RATE, DBL STEP OFF
	or	[DSK_STATE+edi], ah	; TURN ON NEW RATE
	mov	byte [DSKETTE_STATUS], 0 ; RESET STATUS FOR RETRY
	stc				; SET CARRY FOR RETRY
NO_RETRY:	; 08/07/2022
	retn				; RETRY RETURN

;NO_RETRY:
	;clc				; CLEAR CARRY NO RETRY
	;RETn				; NO RETRY RETURN

;-------------------------------------------------------------------------------
; NUM_TRANS
;	THIS ROUTINE CALCULATES THE NUMBER OF SECTORS THAT WERE
;	ACTUALLY TRANSFERRED TO/FROM THE DISKETTE.
;
; ON ENTRY:	[BP+1] = TRACK
;		SI-HI  = HEAD
;		[BP]   = START SECTOR
;
; ON EXIT:	AL = NUMBER ACTUALLY TRANSFERRED
;-------------------------------------------------------------------------------
NUM_TRANS:
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	xor	al, al			; CLEAR FOR ERROR
	cmp	byte [DSKETTE_STATUS], 0 ; CHECK FOR ERROR
	; 24/12/2021
	jne	short NT_OUT		; IF ERROR 0 TRANSFERRED
	;mov	dl, 4			; SECTORS/TRACK OFFSET TO DL
	mov	al, 4 ; 08/07/2022
	call	GET_PARM		; AH = SECTORS/TRACK
	;mov	bl, [NEC_STATUS+5]	; GET ENDING SECTOR
	mov	al, [NEC_STATUS+5]
	mov	ecx, esi		; CH = HEAD # STARTED
	mov	bl, cl ; 11/07/2022 ; sector #
	cmp	ch, [NEC_STATUS+4]	; GET HEAD ENDED UP ON
	jne	short DIF_HD		; IF ON SAME HEAD, THEN NO ADJUST
	; 11/07/2022
	;mov	ch, [NEC_STATUS+3]	; GET TRACK ENDED UP ON
	;cmp	ch, [ebp+1]		; IS IT ASKED FOR TRACK
	;jz	short SAME_TR		; IF SAME TRACK NO INCREASE
	shr	ecx, 8 ; byte 3 = track # --> byte 2
	cmp	ch, [NEC_STATUS+3]
	je	short SAME_TRK		
	; 11/07/2022
	;add	bl, ah			; ADD SECTORS/TRACK
	add	al, ah
DIF_HD:
	;add	bl, ah			; ADD SECTORS/TRACK
	add	al, ah
SAME_TRK:
	;sub	bl, [ebp]		; SUBTRACT START FROM END
	;mov	al, bl			; TO AL
	sub	al, bl
NT_OUT:
	retn

;-------------------------------------------------------------------------------
; SETUP_DBL
;	CHECK DOUBLE STEP.
;
; ON ENTRY :	EDI = DRIVE #
;
; ON EXIT :	CY = 1 MEANS ERROR
;-------------------------------------------------------------------------------
SETUP_DBL:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	ah, [DSK_STATE+edi]	; ACCESS STATE
	test	ah, MED_DET		; ESTABLISHED STATE ?
	jnz	short NO_DBL		; IF ESTABLISHED THEN DOUBLE DONE

;-----	CHECK FOR TRACK 0 TO SPEED UP ACKNOWLEDGE OF UNFORMATTED DISKETTE

	mov	byte [SEEK_STATUS], 0	; SET RECALIBRATE REQUIRED ON ALL DRIVES
	call	MOTOR_ON		; ENSURE MOTOR STAY ON
	mov	ch, 0			; LOAD TRACK 0
	call	SEEK			; SEEK TO TRACK 0
	call	READ_ID			; READ ID FUNCTION
	jc	short SD_ERR		; IF ERROR NO TRACK 0

;-----	INITIALIZE START AND MAX TRACKS (TIMES 2 FOR BOTH HEADS)

	mov	cx, 0450h 		; START, MAX TRACKS
	test	byte [DSK_STATE+edi], TRK_CAPA ; TEST FOR 80 TRACK CAPABILITY
	jz	short CNT_OK		; IF NOT COUNT IS SETUP
	mov	cl, 0A0h		; MAXIMUM TRACK 1.2 MB

;	ATTEMPT READ ID OF ALL TRACKS, ALL HEADS UNTIL SUCCESS; UPON SUCCESS,
;	MUST SEE IF ASKED FOR TRACK IN SINGLE STEP MODE = TRACK ID READ; IF NOT
;	THEN SET DOUBLE STEP ON.

CNT_OK:
       	mov	byte [MOTOR_COUNT], 0FFh ; ENSURE MOTOR STAYS ON FOR OPERATION 
	; 24/12/2021
	push	ecx			; SAVE TRACK, COUNT
	mov	byte [DSKETTE_STATUS], 0 ; CLEAR STATUS, EXPECT ERRORS
	xor	eax, eax		; CLEAR EAX
	shr	ch, 1			; HALVE TRACK, CY = HEAD
	rcl	al, 3			; AX = HEAD IN CORRECT BIT
	; 24/12/2021
	push	eax			; SAVE HEAD
	call	SEEK			; SEEK TO TRACK
	; 24/12/2021
	pop	eax			; RESTORE HEAD
	or	edi, eax		; DI = HEAD OR'ED DRIVE
	call	READ_ID			; READ ID HEAD 0
	pushf				; SAVE RETURN FROM READ_ID
	and	di, 11111011b		; TURN OFF HEAD 1 BIT
	popf				; RESTORE ERROR RETURN
	; 24/12/2021
	pop	ecx			; RESTORE COUNT
	jnc	short DO_CHK		; IF OK, ASKED = RETURNED TRACK ?
	inc	ch			; INC FOR NEXT TRACK
	cmp	ch, cl			; REACHED MAXIMUM YET
	jnz	short CNT_OK		; CONTINUE TILL ALL TRIED

;-----	FALL THRU, READ ID FAILED FOR ALL TRACKS

SD_ERR:	
	stc				; SET CARRY FOR ERROR
	retn				; SETUP_DBL ERROR EXIT

DO_CHK:
	mov	cl, [NEC_STATUS+3]	; LOAD RETURNED TRACK
	mov	[DSK_TRK+edi], cl	; STORE TRACK NUMBER
	shr	ch, 1			; HALVE TRACK
	cmp	ch, cl			; IS IT THE SAME AS ASKED FOR TRACK
	jz	short NO_DBL		; IF SAME THEN NO DOUBLE STEP
	or	byte [DSK_STATE+edi], DBL_STEP ; TURN ON DOUBLE STEP REQUIRED
NO_DBL:
	clc				; CLEAR ERROR FLAG
	retn

;-------------------------------------------------------------------------------
; READ_ID
;	READ ID FUNCTION.
;
; ON ENTRY:	EDI : BIT 2 = HEAD; BITS 1,0 = DRIVE
;
; ON EXIT: 	EDI : BIT 2 IS RESET, BITS 1,0 = DRIVE
;		@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION
;-------------------------------------------------------------------------------
READ_ID:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	eax, ER_3		; MOVE NEC OUTPUT ERROR ADDRESS
	push	eax
	mov	ah, 4Ah			; READ ID COMMAND
	call	NEC_OUTPUT		; TO CONTROLLER
	mov	eax, edi		; DRIVE # TO AH, HEAD 0
	mov	ah, al
	call	NEC_OUTPUT		; TO CONTROLLER
	call	NEC_TERM		; WAIT FOR OPERATION, GET STATUS
	pop	eax			; THROW AWAY ERROR ADDRESS
ER_3:
	retn

;-------------------------------------------------------------------------------
; CMOS_TYPE
;	RETURNS CMOS DISKETTE TYPE
;
; ON ENTRY:	EDI = DRIVE #
;
; ON EXIT:	AL = TYPE; CY REFLECTS STATUS
;-------------------------------------------------------------------------------

CMOS_TYPE: ; 11/12/2014
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	al, [edi+fd0_type] ; diskette type
	and 	al, al ; 18/12/2014
	retn

;-------------------------------------------------------------------------------
; GET_PARM
;	THIS ROUTINE FETCHES THE INDEXED POINTER FROM THE DISK_BASE
;	BLOCK POINTED TO BY THE DATA VARIABLE @DISK_POINTER. A BYTE FROM
;	THAT TABLE IS THEN MOVED INTO AH, THE INDEX OF THAT BYTE BEING
;	THE PARAMETER IN DL.
;
; ON ENTRY:	AL = INDEX OF BYTE TO BE FETCHED ; 08/07/2022
;
; ON EXIT:	AH = THAT BYTE FROM BLOCK
;		AL DESTROYED
;-------------------------------------------------------------------------------
GET_PARM:
	; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;	 ENTRY:
	;	     AL = INDEX
	;	    EDI = DRIVE #
	; 	 RETURN:
	;	     AH = REQUESTED PARAMETER
	;	     AL DESTROYED
	
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	; 08/02/2015 (protected mode modifications, bx -> ebx)
	;push	esi ; 11/07/2022
	push	ebx			; SAVE EBX	
	movzx	ebx, al			; EBX = INDEX
   	; 17/12/2014
	;mov	ax, [cfd] ; current (AL) and previous fd (AH)
	; 11/07/2022
	;cmp	al, ah
	;je	short gpndc

	; 11/07/2022
	mov	eax, edi
	cmp	al, [pfd] ; is same with previous drive # ?
	je	short gpndc	

	mov	[pfd], al ; current drive -> previous drive

	push	ebx ; 08/02/2015
	
	;mov	bl, al 
	; 11/12/2014
	;mov	al, [ebx+fd0_type]	; Drive type (0,1,2,3,4)
	; 11/07/2022
	mov	al, [edi+fd0_type]	; Drive type (0,1,2,3,4)	
	; 18/12/2014
	and	al, al
	jnz	short gpdtc
	mov	ebx, MD_TBL6		; 1.44 MB param. tbl. (default)
        jmp     short gpdpu
gpdtc:	
	call	DR_TYPE_CHECK
	; cf = 1 -> EBX points to 1.44MB fd parameter table (default)
gpdpu:
	mov	[DISK_POINTER], ebx
	pop	ebx
gpndc:
	;mov	esi, [DISK_POINTER] ; 08/02/2015, si -> esi
	;mov	ah, [esi+ebx]		; GET THE WORD
	; 11/07/2022
	add	ebx, [DISK_POINTER]
	mov	ah, [ebx] 
	pop	ebx			; RESTORE EBX
	;pop	esi ; 11/07/2022
	retn

;-------------------------------------------------------------------------------
; MOTOR_ON
;	TURN MOTOR ON AND WAIT FOR MOTOR START UP TIME. THE @MOTOR_COUNT
;	IS REPLACED WITH A SUFFICIENTLY HIGH NUMBER (0FFH) TO ENSURE
;	THAT THE MOTOR DOES NOT GO OFF DURING THE OPERATION. IF THE
;	MOTOR NEEDED TO BE TURNED ON, THE MULTI-TASKING HOOK FUNCTION
;	(AX=90FDH, INT 15) IS CALLED TELLING THE OPERATING SYSTEM
;	THAT THE BIOS IS ABOUT TO WAIT FOR MOTOR START UP. IF THIS
;	FUNCTION RETURNS WITH CY = 1, IT MEANS THAT THE MINIMUM WAIT
;	HAS BEEN COMPLETED. AT THIS POINT A CHECK IS MADE TO ENSURE
;	THAT THE MOTOR WASN'T TURNED OFF BY THE TIMER. IF THE HOOK DID
;	NOT WAIT, THE WAIT FUNCTION (AH=086H) IS CALLED TO WAIT THE
;	PRESCRIBED AMOUNT OF TIME. IF THE CARRY FLAG IS SET ON RETURN,
;	IT MEANS THAT THE FUNCTION IS IN USE AND DID NOT PERFORM THE
;	WAIT. A TIMER 1 WAIT LOOP WILL THEN DO THE WAIT.
;
; ON ENTRY:	EDI = DRIVE #
; ON EXIT:	EAX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
MOTOR_ON:
	; 12/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	push	ebx			; SAVE REG.
	call	TURN_ON			; TURN ON MOTOR
	jc	short MOT_IS_ON		; IF CY=1 NO WAIT
	;call	XLAT_OLD		; TRANSLATE STATE TO COMPATIBLE MODE
	; 08/07/2022
	;call	XLAT_NEW ; 12/07/2022	; TRANSLATE STATE TO PRESENT ARCH,
	;call	TURN_ON 		; CHECK AGAIN IF MOTOR ON
	;jc	short MOT_IS_ON		; IF NO WAIT MEANS IT IS ON
M_WAIT:
	;mov	dl,10			; GET THE MOTOR WAIT PARAMETER
	mov	al, 10 ; 08/07/2022
	call	GET_PARM
	; 08/07/2022			; AH = MOTOR WAIT PARAMETER
	cmp	ah, 8			; SEE IF AT LEAST A SECOND IS SPECIFIED			
	;jae	short GP2		; IF YES, CONTINUE
	jae	short J13
	mov	ah, 8			; ONE SECOND WAIT FOR MOTOR START UP

;-----	AS CONTAINS NUMBER OF 1/8 SECONDS (125000 MICROSECONDS) TO WAIT
GP2:	
;----- 	FOLLOWING LOOPS REQUIRED WHEN RTC WAIT FUNCTION IS ALREADY IN USE
J13:					; WAIT FOR 1/8 SECOND PER (AL)
	mov	ecx, 8286		; COUNT FOR 1/8 SECOND AT 15.085737 US
	call	WAITF			; GO TO FIXED WAIT ROUTINE
	;dec	al			; DECREMENT TIME VALUE
	dec	ah
	jnz	short J13		; ARE WE DONE YET
MOT_IS_ON:
	pop	ebx			; RESTORE REG.
	retn

;-------------------------------------------------------------------------------
; TURN_ON
;	TURN MOTOR ON AND RETURN WAIT STATE.
;
; ON ENTRY:	EDI = DRIVE #
;
; ON EXIT:	CY = 0 MEANS WAIT REQUIRED
;		CY = 1 MEANS NO WAIT REQUIRED
;		EAX, EBX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
TURN_ON:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	ebx, edi		; EBX = DRIVE #
	mov	cl, bl			; CL = DRIVE #
	rol	bl, 4			; BL = DRIVE SELECT
	cli				; NO INTERRUPTS WHILE DETERMINING STATUS
	mov	byte [MOTOR_COUNT], 0FFh ; ENSURE MOTOR STAYS ON FOR OPERATION
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	and	al, 00110000b		; KEEP ONLY DRIVE SELECT BITS
	mov	ah, 1			; MASK FOR DETERMINING MOTOR BIT
	shl	ah, cl			; AH = MOTOR ON, A=00000001, B=00000010

;  AL = DRIVE SELECT FROM @MOTOR_STATUS
;  BL = DRIVE SELECT DESIRED
;  AH = MOTOR ON MASK DESIRED

	cmp	al, bl			; REQUESTED DRIVE ALREADY SELECTED ?
	jne	short TURN_IT_ON	; IF NOT SELECTED JUMP
	test	ah, [MOTOR_STATUS]	; TEST MOTOR ON BIT
	jnz	short NO_MOT_WAIT	; JUMP IF MOTOR ON AND SELECTED

TURN_IT_ON:
	or	ah, bl			; AH = DRIVE SELECT AND MOTOR ON
	mov	bh, [MOTOR_STATUS]	; SAVE COPY OF @MOTOR_STATUS BEFORE
	and	bh, 00001111b		; KEEP ONLY MOTOR BITS
	and	byte [MOTOR_STATUS], 11001111b ; CLEAR OUT DRIVE SELECT
	or	[MOTOR_STATUS], ah	; OR IN DRIVE SELECTED AND MOTOR ON
	mov	al, [MOTOR_STATUS]	; GET DIGITAL OUTPUT REGISTER REFLECTION
	mov	bl, al			; BL=@MOTOR_STATUS AFTER, BH=BEFORE
	and	bl, 00001111b		; KEEP ONLY MOTOR BITS
	sti				; ENABLE INTERRUPTS AGAIN
	and	al, 00111111b		; STRIP AWAY UNWANTED BITS
	rol	al, 4			; PUT BITS IN DESIRED POSITIONS
	or	al, 00001100b		; NO RESET, ENABLE DMA/INTERRUPT
	mov	dx, 03F2h		; SELECT DRIVE AND TURN ON MOTOR
	out	dx, al
	cmp	bl, bh			; NEW MOTOR TURNED ON ?
	;je	short NO_MOT_WAIT	; NO WAIT REQUIRED IF JUST SELECT
	je	short no_mot_w1 ; 27/02/2015 
	clc				; RESET CARRY MEANING WAIT
	retn

NO_MOT_WAIT:
	sti
no_mot_w1: ; 27/02/2015
	stc				; SET NO WAIT REQUIRED
	;sti				; INTERRUPTS BACK ON
	retn

;-------------------------------------------------------------------------------
; HD_WAIT
;	WAIT FOR HEAD SETTLE TIME.
;
; ON ENTRY:	DI = DRIVE #
;
; ON EXIT:	EAX, EBX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
HD_WAIT:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;mov	dl, 9			; GET HEAD SETTLE PARAMETER
	mov	al, 9	; 08/07/2022
	CALL	GET_PARM
	or	ah, ah	; 17/12/2014	; CHECK FOR ANY WAIT?
	jnz	short DO_WAT		; IF THERE DO NOT ENFORCE
        test	byte [MOTOR_STATUS], 10000000b ; SEE IF A WRITE OPERATION
	;jz	short ISNT_WRITE	; IF NOT, DO NOT ENFORCE ANY VALUES
	;or	ah, ah			; CHECK FOR ANY WAIT?
	;jnz	short DO_WAT		; IF THERE DO NOT ENFORCE
	jz	short HW_DONE
	mov	ah, HD12_SETTLE		; LOAD 1.2M HEAD SETTLE MINIMUM
	mov	al, [DSK_STATE+edi]	; LOAD STATE
	and	al, RATE_MSK		; KEEP ONLY RATE
	cmp	al, RATE_250		; 1.2 M DRIVE ?
	jnz	short DO_WAT		; DEFAULT HEAD SETTLE LOADED
;GP3:
	mov	ah, HD320_SETTLE	; USE 320/360 HEAD SETTLE
;	jmp	short DO_WAT

;ISNT_WRITE:
;	or	ah, ah			; CHECK FOR NO WAIT
;	jz	short HW_DONE		; IF NOT WRITE AND 0 ITS OK

;-----	AH CONTAINS NUMBER OF MILLISECONDS TO WAIT
DO_WAT:
;	mov	al, ah			; AL = # MILLISECONDS
;	;xor	ah, ah			; AX = # MILLISECONDS
J29:					; 	1 MILLISECOND LOOP
	;mov	cx, WAIT_FDU_HEAD_SETTLE ; 33 ; 1 ms in 30 micro units.
	;mov	ecx, 66			; COUNT AT 15.085737 US PER COUNT
	; 08/07/2022
	sub	ecx, ecx
	mov	cl, 66
	call	WAITF			; DELAY FOR 1 MILLISECOND
	;dec	al			; DECREMENT THE COUNT
	dec	ah
	jnz	short J29		; DO AL MILLISECOND # OF TIMES
HW_DONE:
	retn

;-------------------------------------------------------------------------------
; NEC_OUTPUT
;	THIS ROUTINE SENDS A BYTE TO THE NEC CONTROLLER AFTER TESTING
;	FOR CORRECT DIRECTION AND CONTROLLER READY THIS ROUTINE WILL
;	TIME OUT IF THE BYTE IS NOT ACCEPTED WITHIN A REASONABLE AMOUNT
;	OF TIME, SETTING THE DISKETTE STATUS ON COMPLETION.
; 
; ON ENTRY: 	AH = BYTE TO BE OUTPUT
;
; ON EXIT:	CY = 0  SUCCESS
;		CY = 1  FAILURE -- DISKETTE STATUS UPDATED
;		        IF A FAILURE HAS OCCURRED, THE RETURN IS MADE ONE LEVEL
;		        HIGHER THAN THE CALLER OF NEC OUTPUT. THIS REMOVES THE
;		        REQUIREMENT OF TESTING AFTER EVERY CALL OF NEC_OUTPUT.
;
;		EAX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------

; 09/12/2014 [Erdogan Tan] 
;	(from 'PS2 Hardware Interface Tech. Ref. May 88', Page 09-05.)
; Diskette Drive Controller Status Register (3F4h)
;	This read only register facilitates the transfer of data between
;	the system microprocessor and the controller.
; Bit 7 - When set to 1, the Data register is ready to transfer data 
;	  with the system micrprocessor.
; Bit 6 - The direction of data transfer. If this bit is set to 0,
;	  the transfer is to the controller.
; Bit 5 - When this bit is set to 1, the controller is in the non-DMA mode.
; Bit 4 - When this bit is set to 1, a Read or Write command is being executed.
; Bit 3 - Reserved.
; Bit 2 - Reserved.
; Bit 1 - When this bit is set to 1, dskette drive 1 is in the seek mode.
; Bit 0 - When this bit is set to 1, dskette drive 0 is in the seek mode.

; Data Register (3F5h)
; This read/write register passes data, commands and parameters, and provides
; diskette status information.
  		
NEC_OUTPUT:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	ebx			; SAVE REG.
	mov	edx, 03F4h		; STATUS PORT
	;xor	ecx, ecx		; COUNT FOR TIME OUT
	; 16/12/2014
	; waiting for (max.) 0.5 seconds
        ;;mov	byte [wait_count], 0 ;; 27/02/2015
	;
	; 17/12/2014
	; Modified from AWARD BIOS 1999 - ADISK.ASM - SEND_COMMAND
	;
	;WAIT_FOR_PORT:	Waits for a bit at a port pointed to by DX to
	;		go on.
	;INPUT:
	;	AH=Mask for isolation bits.
	;	AL=pattern to look for.
	;	DX=Port to test for
	;	ECX=Number of memory refresh periods to delay.
	;	     (normally 30 microseconds per period.)
	;
	;WFP_SHORT:  
	;	Wait for port if refresh cycle is short (15-80 Us range).
	;

	mov	ecx, WAIT_FDU_SEND_LH   ; 16667 (27/02/2015)
;
;WFPS_OUTER_LP:
;	;
;WFPS_CHECK_PORT:
J23:
	in	al, dx			; GET STATUS
	and	al, 11000000b		; KEEP STATUS AND DIRECTION
	cmp	al, 10000000b		; STATUS 1 AND DIRECTION 0 ?
	jz	short J27		; STATUS AND DIRECTION OK
WFPS_HI:
	in	al, PORT_B	; 061h	; SYS1	; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WFPS_HI		; refresh.
WFPS_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	short WFPS_LO
	;loop	short WFPS_CHECK_PORT
	loop	J23	; 27/02/2015	; REPEAT TILL DELAY FINISHED

	; fail

;WFPS_TIMEOUT:

;-----	FALL THRU TO ERROR RETURN

	or	byte [DSKETTE_STATUS], TIME_OUT
	;pop	ebx			; RESTORE REG.
	pop	eax ; 08/02/2015	; DISCARD THE RETURN ADDRESS
	stc				; INDICATE ERROR TO CALLER
	retn

;-----	DIRECTION AND STATUS OK; OUTPUT BYTE

J27:	
	mov	al, ah			; GET BYTE TO OUTPUT
	inc	edx			; DATA PORT = STATUS PORT + 1
	out	dx, al			; OUTPUT THE BYTE
	;;NEWIODELAY  ;; 27/02/2015
	; 27/02/2015
	pushfd	; 24/12/2021		; SAVE FLAGS
	;mov	ecx, 3			; 30 TO 45 MICROSECONDS WAIT FOR
	sub	ecx, ecx
	mov	cl, 3 ; 24/12/2021
	call 	WAITF			; NEC FLAGS UPDATE CYCLE
	popfd	; 24/12/2021		; RESTORE FLAGS FOR EXIT
	;pop	ebx			; RESTORE REG
	retn				; CY = 0 FROM TEST INSTRUCTION

;-------------------------------------------------------------------------------
; SEEK
;	THIS ROUTINE WILL MOVE THE HEAD ON THE NAMED DRIVE TO THE NAMED
;	TRACK. IF THE DRIVE HAS NOT BEEN ACCESSED SINCE THE DRIVE
;	RESET COMMAND WAS ISSUED, THE DRIVE WILL BE RECALIBRATED.
;
; ON ENTRY:	EDI = DRIVE #
;		CH = TRACK #
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;		EAX, EBX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
SEEK:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	ebx, edi		; EBX = DRIVE #
	mov	al, 1			; ESTABLISH MASK FOR RECALIBRATE TEST
	xchg	cl, bl			; SET DRIVE VALUE INTO CL
	rol	al, cl			; SHIFT MASK BY THE DRIVE VALUE
	xchg	cl, bl			; RECOVER DRIVE VALUE
	test	al, [SEEK_STATUS]	; TEST FOR RECALIBRATE REQUIRED
	jnz	short J28A		; JUMP IF RECALIBRATE NOT REQUIRED

	or	[SEEK_STATUS], al	; TURN ON THE NO RECALIBRATE BIT IN FLAG
	call	RECAL			; RECALIBRATE DRIVE
	jnc	short AFT_RECAL		; RECALIBRATE DONE

;-----	ISSUE RECALIBRATE FOR 80 TRACK DISKETTES

	mov	byte [DSKETTE_STATUS], 0 ; CLEAR OUT INVALID STATUS
	call	RECAL			; RECALIBRATE DRIVE
	jc	short RB		; IF RECALIBRATE FAILS TWICE THEN ERROR

AFT_RECAL:
	mov	byte [DSK_TRK+edi], 0	; SAVE NEW CYLINDER AS PRESENT POSITION
	or	ch, ch			; CHECK FOR SEEK TO TRACK 0
	jz	short DO_WAIT		; HEAD SETTLE, CY = 0 IF JUMP

;-----	DRIVE IS IN SYNCHRONIZATION WITH CONTROLLER, SEEK TO TRACK

J28A:	test	byte [DSK_STATE+edi], DBL_STEP ; CHECK FOR DOUBLE STEP REQUIRED
	jz	short _R7		; SINGLE STEP REQUIRED BYPASS DOUBLE
	shl	ch, 1			; DOUBLE NUMBER OF STEP TO TAKE

_R7:	cmp	ch, [DSK_TRK+edi]	; SEE IF ALREADY AT THE DESIRED TRACK
	je	short RB		; IF YES, DO NOT NEED TO SEEK

	mov	edx, NEC_ERR		; LOAD RETURN ADDRESS
	push	edx ; (*)		; ON STACK FOR NEC OUTPUT ERROR
	mov	[DSK_TRK+edi], ch	; SAVE NEW CYLINDER AS PRESENT POSITION
	mov	ah, 0Fh			; SEEK COMMAND TO NEC
	call	NEC_OUTPUT
	mov	ebx, edi		; EBX = DRIVE #
	mov	ah, bl			; OUTPUT DRIVE NUMBER
	call	NEC_OUTPUT
	mov	ah, [DSK_TRK+edi]	; GET CYLINDER NUMBER
	call	NEC_OUTPUT
	call	CHK_STAT_2		; ENDING INTERRUPT AND SENSE STATUS

;-----	WAIT FOR HEAD SETTLE

DO_WAIT:
	pushfd	; 24/12/2021		; SAVE STATUS
	call	HD_WAIT			; WAIT FOR HEAD SETTLE TIME
	popfd	; 24/12/2021		; RESTORE STATUS
RB:
NEC_ERR:
	; 08/02/2015 (code trick here from original IBM PC/AT DISKETTE.ASM)
	; (*) nec_err -> retn (push edx -> pop edx) -> nec_err -> retn
	retn				; RETURN TO CALLER

;-------------------------------------------------------------------------------
; RECAL
;	RECALIBRATE DRIVE
;
; ON ENTRY:	EDI = DRIVE #
;
; ON EXIT:	CY REFLECTS STATUS OF OPERATION.
;-------------------------------------------------------------------------------
RECAL:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	cx
	; 24/12/2021
	push	ecx
	mov	eax, RC_BACK		; LOAD NEC_OUTPUT ERROR
	push	eax
	mov	ah, 07h			; RECALIBRATE COMMAND
	call	NEC_OUTPUT
	mov	ebx, edi		; EBX = DRIVE #
	mov	ah, bl
	call	NEC_OUTPUT		; OUTPUT THE DRIVE NUMBER
	call	CHK_STAT_2		; GET THE INTERRUPT AND SENSE INT STATUS
	pop	eax			; THROW AWAY ERROR
RC_BACK:
	;pop	cx
	; 24/12/2021
	pop	ecx
	RETn

;-------------------------------------------------------------------------------
; CHK_STAT_2
;	THIS ROUTINE HANDLES THE INTERRUPT RECEIVED AFTER RECALIBRATE,
;	OR SEEK TO THE ADAPTER. THE INTERRUPT IS WAITED FOR, THE
;	INTERRUPT STATUS SENSED, AND THE RESULT RETURNED TO THE CALLER.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;-------------------------------------------------------------------------------
CHK_STAT_2:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
        mov	eax, CS_BACK		; LOAD NEC_OUTPUT ERROR ADDRESS
	push	eax
	call	WAIT_INT		; WAIT FOR THE INTERRUPT
	jc	short J34		; IF ERROR, RETURN IT
	mov	ah, 08h			; SENSE INTERRUPT STATUS COMMAND
	call	NEC_OUTPUT
	call	RESULTS			; READ IN THE RESULTS
	jc	short J34
	mov	al, [NEC_STATUS]	; GET THE FIRST STATUS BYTE
	and	al, 01100000b		; ISOLATE THE BITS
	cmp	al, 01100000b		; TEST FOR CORRECT VALUE
	jz	short J35		; IF ERROR, GO MARK IT
	clc				; GOOD RETURN
J34:
	pop	eax			; THROW AWAY ERROR RETURN
CS_BACK:
	retn
J35:
	or	byte [DSKETTE_STATUS], BAD_SEEK
	stc				; ERROR RETURN CODE
	jmp	short J34

;-------------------------------------------------------------------------------
; WAIT_INT
;	THIS ROUTINE WAITS FOR AN INTERRUPT TO OCCUR A TIME OUT ROUTINE
;	TAKES PLACE DURING THE WAIT, SO THAT AN ERROR MAY BE RETURNED
;	IF THE DRIVE IS NOT READY.
;
; ON EXIT: 	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;-------------------------------------------------------------------------------

; 17/12/2014
; 2.5 seconds waiting !
;(AWARD BIOS - 1999, WAIT_FDU_INT_LOW, WAIT_FDU_INT_HI)
; amount of time to wait for completion interrupt from NEC.

WAIT_INT:
	; 12/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	sti				; TURN ON INTERRUPTS, JUST IN CASE
	; 12/07/2022
	;clc				; CLEAR TIMEOUT INDICATOR
       ;mov	bl, 10			; CLEAR THE COUNTERS
       ;xor	cx, cx			; FOR 2 SECOND WAIT

	; Modification from AWARD BIOS - 1999 (ATORGS.ASM, WAIT
	;
	;WAIT_FOR_MEM:	
	;	Waits for a bit at a specified memory location pointed
	;	to by ES:[DI] to become set.
	;INPUT:
	;	AH=Mask to test with.
	;	ES:[DI] = memory location to watch.
	;	BH:CX=Number of memory refresh periods to delay.
	;	     (normally 30 microseconds per period.)

	; waiting for (max.) 2.5 secs in 30 micro units.
;	mov 	cx, WAIT_FDU_INT_LO		; 017798
;;	mov 	bl, WAIT_FDU_INT_HI
;	mov 	bl, WAIT_FDU_INT_HI + 1
	; 27/02/2015
	mov 	ecx, WAIT_FDU_INT_LH	; 83334 (2.5 seconds)		
WFMS_CHECK_MEM:
	test	byte [SEEK_STATUS], INT_FLAG ; TEST FOR INTERRUPT OCCURRING
        jnz     short J37
WFMS_HI:
	in	al, PORT_B  ; 061h	; SYS1, wait for lo to hi
	test	al, 010h		; transition on memory
	jnz	short WFMS_HI		; refresh.
WFMS_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	short WFMS_LO
        loop	WFMS_CHECK_MEM
;WFMS_OUTER_LP:
;;	or	bl, bl			; check outer counter
;;	jz	short J36A		; WFMS_TIMEOUT
;	dec	bl
;	jz	short J36A	
;	jmp	short WFMS_CHECK_MEM

	;17/12/2014
	;16/12/2014
;	mov     byte [wait_count], 0    ; Reset (INT 08H) counter
;J36:
;	test	byte [SEEK_STATUS], INT_FLAG ; TEST FOR INTERRUPT OCCURRING
;	jnz	short J37
	;16/12/2014
	;loop	J36			; COUNT DOWN WHILE WAITING
	;dec	bl			; SECOND LEVEL COUNTER
	;jnz	short J36
;       cmp     byte [wait_count], 46   ; (46/18.2 seconds)
;	jb	short J36

;WFMS_TIMEOUT:
;J36A:
	or	byte [DSKETTE_STATUS], TIME_OUT ; NOTHING HAPPENED
	stc				; ERROR RETURN
J37:
	pushf				; SAVE CURRENT CARRY
	and	byte [SEEK_STATUS], ~INT_FLAG ; TURN OFF INTERRUPT FLAG
	popf				; RECOVER CARRY
	retn				; GOOD RETURN CODE

;-------------------------------------------------------------------------------
; RESULTS
;	THIS ROUTINE WILL READ ANYTHING THAT THE NEC CONTROLLER RETURNS 
;	FOLLOWING AN INTERRUPT.
;
; ON EXIT:	@DSKETTE_STATUS, CY REFLECT STATUS OF OPERATION.
;		EAX, EBX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
RESULTS:
	; 12/07/2022
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	push	edi
	mov	edi, NEC_STATUS		; POINTER TO DATA AREA
	mov	bl, 7			; MAX STATUS BYTES
	mov	dx, 03F4h		; STATUS PORT

;-----	WAIT FOR REQUEST FOR MASTER

_R10: 
	; 16/12/2014
	; wait for (max) 0.5 seconds
	;mov	bh, 2			; HIGH ORDER COUNTER
	;xor	cx, cx			; COUNTER

	;Time to wait while waiting for each byte of NEC results = .5
	;seconds.  .5 seconds = 500,000 micros.  500,000/30 = 16,667.
	; 27/02/2015

	mov 	ecx, WAIT_FDU_RESULTS_LH ; 16667  
	;mov	cx, WAIT_FDU_RESULTS_LO  ; 16667
	;mov	bh, WAIT_FDU_RESULTS_HI+1 ; 0+1

WFPSR_OUTER_LP:
	;
WFPSR_CHECK_PORT:
J39:					; WAIT FOR MASTER
	in	al, dx			; GET STATUS
	and	al, 11000000b		; KEEP ONLY STATUS AND DIRECTION
	cmp	al, 11000000b		; STATUS 1 AND DIRECTION 1 ?
	jz	short J42		; STATUS AND DIRECTION OK
WFPSR_HI:
	in	al, PORT_B	;061h	; SYS1	; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WFPSR_HI		; refresh.
WFPSR_LO:
	in	al, PORT_B		; SYS1
	test	al, 010h
	jz	short WFPSR_LO
        loop	WFPSR_CHECK_PORT
	
	;; 27/02/2015
	;;dec	bh
	;;jnz	short WFPSR_OUTER_LP
	;jmp	short WFPSR_TIMEOUT	; fail

	;;mov	byte [wait_count], 0
;J39:					; WAIT FOR MASTER
;	in	al, dx			; GET STATUS
;	and	al, 11000000b		; KEEP ONLY STATUS AND DIRECTION
;	cmp	al, 11000000b		; STATUS 1 AND DIRECTION 1 ?
;	jz	short J42		; STATUS AND DIRECTION OK
	;loop	J39			; LOOP TILL TIMEOUT
	;dec	bh			; DECREMENT HIGH ORDER COUNTER
	;jnz	short J39		; REPEAT TILL DELAY DONE
	;
	;;cmp	byte [wait_count], 10	; (10/18.2 seconds)
	;;jb	short J39	

;WFPSR_TIMEOUT:
	or	byte [DSKETTE_STATUS], TIME_OUT
	stc				; SET ERROR RETURN
	jmp	short POPRES		; POP REGISTERS AND RETURN

;-----	READ IN THE STATUS

J42:
	JMP	$+2			; I/O DELAY
	;inc	dx			; POINT AT DATA PORT
	inc	dl
	in	al, dx			; GET THE DATA
	; 16/12/2014
	NEWIODELAY
	
	;mov	[edi], al		; STORE THE BYTE
	;inc	edi			; INCREMENT THE POINTER
	; 11/07/2022
	stosb

	; 16/12/2014
;	push	cx
;	mov	cx, 30
;wdw2:
;	NEWIODELAY
;	loop	wdw2
;	pop	cx

	;mov	ecx,3			; MINIMUM 24 MICROSECONDS FOR NEC
	; 12/07/2022
	sub	ecx, ecx
	mov	cl, 3
	call	WAITF			; WAIT 30 TO 45 MICROSECONDS
	;dec	dx			; POINT AT STATUS PORT
	dec	dl
	in	al, dx			; GET STATUS
	; 16/12/2014
	NEWIODELAY
	;
	test	al, 00010000b		; TEST FOR NEC STILL BUSY
	jz	short POPRES		; RESULTS DONE ?

	dec	bl			; DECREMENT THE STATUS COUNTER
        jnz	short _R10              ; GO BACK FOR MORE
	or	byte [DSKETTE_STATUS], BAD_NEC ; TOO MANY STATUS BYTES
	stc				; SET ERROR FLAG

;-----	RESULT OPERATION IS DONE
POPRES:
	pop	edi
	retn				; RETURN WITH CARRY SET

;-------------------------------------------------------------------------------
; READ_DSKCHNG
;	READS THE STATE OF THE DISK CHANGE LINE.
;
; ON ENTRY:	EDI = DRIVE #
;
; ON EXIT:	EDI = DRIVE #
;		ZF = 0 : DISK CHANGE LINE INACTIVE
;		ZF = 1 : DISK CHANGE LINE ACTIVE
;		EAX, ECX, EDX DESTROYED
;-------------------------------------------------------------------------------
READ_DSKCHNG:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	call	MOTOR_ON		; TURN ON THE MOTOR IF OFF
	mov	dx, 03F7h		; ADDRESS DIGITAL INPUT REGISTER
	in	al, dx			; INPUT DIGITAL INPUT REGISTER
	test	al, DSK_CHG		; CHECK FOR DISK CHANGE LINE ACTIVE
	retn				; RETURN TO CALLER WITH ZERO FLAG SET

fdc_int:  
	  ; 30/07/2015	
	  ; 16/02/2015
;int_0Eh: ; 11/12/2014

;--- HARDWARE INT 0EH -- ( IRQ LEVEL  6 ) --------------------------------------
; DISK_INT
;	THIS ROUTINE HANDLES THE DISKETTE INTERRUPT.
;
; ON EXIT:	THE INTERRUPT FLAG IS SET IN @SEEK_STATUS.
;-------------------------------------------------------------------------------
DISK_INT_1:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	eax			; SAVE WORK REGISTER
	; 24/12/2021
	push	eax
	push	ds
	mov	ax, KDATA
	mov 	ds, ax
        or	byte [SEEK_STATUS], INT_FLAG ; TURN ON INTERRUPT OCCURRED
	mov	al, EOI			; END OF INTERRUPT MARKER
	out	INTA00, al		; INTERRUPT CONTROL PORT
	pop	ds
	;pop	ax			; RECOVER REGISTER
	; 24/12/2021
	pop	eax
	iretd				; RETURN FROM INTERRUPT

;-------------------------------------------------------------------------------
; DSKETTE_SETUP
;	THIS ROUTINE DOES A PRELIMINARY CHECK TO SEE WHAT TYPE OF
;	DISKETTE DRIVES ARE ATTACH TO THE SYSTEM.
;-------------------------------------------------------------------------------
DSKETTE_SETUP:
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;push	eax			; SAVE REGISTERS
	;push	ebx
	;push	ecx
	push	edx
	;push	edi
	; 14/12/2014
	;mov	dword [DISK_POINTER], MD_TBL6
	;
	;or	byte [RTC_WAIT_FLAG], 1	; NO RTC WAIT, FORCE USE OF LOOP

	xor	edi, edi 		; INITIALIZE DRIVE POINTER
	sub	ecx, ecx
	mov	[DSK_STATE], cx ; 0	; INITIALIZE STATES
	; 08/07/2022
	;and	byte [LASTRATE], ~(STRT_MSK+SEND_MSK) ; CLEAR START & SEND
	or	byte [LASTRATE], SEND_MSK ; INITIALIZE SENT TO IMPOSSIBLE
	mov	[SEEK_STATUS], cl ; 0	; INDICATE RECALIBRATE NEEDED
	mov	[MOTOR_COUNT], cl ; 0	; INITIALIZE MOTOR COUNT
	mov	[MOTOR_STATUS], cl ; 0	; INITIALIZE DRIVES TO OFF STATE
	mov	[DSKETTE_STATUS], cl ; 0 ; NO ERRORS
	;
	; 28/02/2015
	;mov	word [cfd], 100h 
	call	DSK_RESET
	pop	edx
	retn

;//////////////////////////////////////////////////////
;; END OF DISKETTE I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

; 12/07/2022
;int13h: ; 21/02/2015
;	pushfd
;	push 	cs
;	call 	DISK_IO
;	retn

;;;;;; DISK I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 21/02/2015 ;;;
;/////////////////////////////////////////////////////////////////////

; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
; ((Direct call instead of int 13h simulation))
;
;		Function in AL
;			0 = reset
;			1 = read
;			2 = write
;		Disk drive number in DL
;			0 & 1 = floppy disks	
;			80h .. 83h = hard disks
;		Sector address (LBA) in ECX
;		Buffer address in EBX
;		R/W sector count is (always) 1
;
;		Return:
;			Status in AH (>0 = error code)
;			if CF = 1 -> error code in AH
;			if CF = 0 -> successful
;			AL = undefined
;
;		Modified registers: (only) EAX

; 10/07/2022
; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

; DISK I/O - Erdogan Tan (Retro UNIX 386 v1 project)
; 23/02/2015
; 21/02/2015 (unix386.s)
; 22/12/2014 - 14/02/2015 (dsectrm2.s)
;
; Original Source Code:
; DISK ----- 09/25/85 FIXED DISK BIOS
; (IBM PC XT Model 286 System BIOS Source Code, 04-21-86)
;
; Modifications: by reference of AWARD BIOS 1999 (D1A0622) 
;		 Source Code - ATORGS.ASM, AHDSK.ASM
;

;The wait for controller to be not busy is 10 seconds.
;10,000,000 / 30 = 333,333.  333,333 decimal = 051615h
;;WAIT_HDU_CTLR_BUSY_LO	equ	1615h		
;;WAIT_HDU_CTLR_BUSY_HI	equ	  05h
WAIT_HDU_CTRL_BUSY_LH	equ	51615h	 ;21/02/2015		

;The wait for controller to issue completion interrupt is 10 seconds.
;10,000,000 / 30 = 333,333.  333,333 decimal = 051615h
;;WAIT_HDU_INT_LO	equ	1615h
;;WAIT_HDU_INT_HI	equ	  05h
WAIT_HDU_INT_LH		equ	51615h	; 21/02/2015

;The wait for Data request on read and write longs is
;2000 us. (?)
;;WAIT_HDU_DRQ_LO	equ	1000	; 03E8h
;;WAIT_HDU_DRQ_HI	equ	0
WAIT_HDU_DRQ_LH		equ	1000	; 21/02/2015

; Port 61h (PORT_B)
SYS1		equ	61h	; PORT_B  (diskette.inc)

; 23/12/2014
%define CMD_BLOCK       ebp-8  ; 21/02/2015

	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;--- INT 13H -------------------------------------------------------------------
;									       :
; FIXED DISK I/O INTERFACE						       :
;									       :
;	THIS INTERFACE PROVIDES ACCESS TO 5 1/4" FIXED DISKS THROUGH           :
;	THE IBM FIXED DISK CONTROLLER.					       :
;									       :
;	THE  BIOS  ROUTINES  ARE  MEANT  TO  BE  ACCESSED  THROUGH	       :
;	SOFTWARE  INTERRUPTS  ONLY.    ANY  ADDRESSES  PRESENT	IN	       :
;	THESE  LISTINGS  ARE  INCLUDED	 ONLY	FOR  COMPLETENESS,	       :
;	NOT  FOR  REFERENCE.  APPLICATIONS   WHICH  REFERENCE  ANY	       :
;	ABSOLUTE  ADDRESSES  WITHIN  THE  CODE	SEGMENTS  OF  BIOS	       :
;	VIOLATE  THE  STRUCTURE  AND  DESIGN  OF  BIOS. 		       :
;									       :
;------------------------------------------------------------------------------:
;									       :
; INPUT  (AH)= HEX COMMAND VALUE					       :
;									       :
;	(AH)= 00H  RESET DISK (DL = 80H,81H) / DISKETTE 		       :
;	(AH)= 01H  READ THE DESIRED SECTORS INTO MEMORY                        :
;	(AH)= 02H  WRITE THE DESIRED SECTORS FROM MEMORY                       :
;									       :
;------------------------------------------------------------------------------:
;									       :
;	REGISTERS USED FOR FIXED DISK OPERATIONS			       :
;									       :
;		(DL)	-  DRIVE NUMBER     (80H-81H FOR DISK. VALUE CHECKED)  :
;		(DH)	-  HEAD NUMBER	    (0-15 ALLOWED, NOT VALUE CHECKED)  :
;		(CH)	-  CYLINDER NUMBER  (0-1023, NOT VALUE CHECKED)(SEE CL):
;		(CL)	-  SECTOR NUMBER    (1-17, NOT VALUE CHECKED)	       :
;									       :
;			   NOTE: HIGH 2 BITS OF CYLINDER NUMBER ARE PLACED     :
;				 IN THE HIGH 2 BITS OF THE CL REGISTER	       :
;				 (10 BITS TOTAL)			       :
;									       :
;		(AL)	-  NUMBER OF SECTORS (MAXIMUM POSSIBLE RANGE 1-80H,    :
;					      FOR READ/WRITE LONG 1-79H)       :
;									       :
;		(EBX)   -  ADDRESS OF BUFFER FOR READS AND WRITES,	       :
;			   (NOT REQUIRED FOR VERIFY)			       :
;									       :
;------------------------------------------------------------------------------:
; OUTPUT								       :
;	AH = STATUS OF CURRENT OPERATION				       :
;	     STATUS BITS ARE DEFINED IN THE EQUATES BELOW		       :
;	CY = 0	SUCCESSFUL OPERATION (AH=0 ON RETURN)			       :
;	CY = 1	FAILED OPERATION (AH HAS ERROR REASON)			       :
;									       :
;	NOTE:	ERROR 11H  INDICATES THAT THE DATA READ HAD A RECOVERABLE      :
;		ERROR WHICH WAS CORRECTED BY THE ECC ALGORITHM.  THE DATA      :
;		IS PROBABLY GOOD,   HOWEVER THE BIOS ROUTINE INDICATES AN      :
;		ERROR TO ALLOW THE CONTROLLING PROGRAM A CHANCE TO DECIDE      :
;		FOR ITSELF.  THE  ERROR  MAY  NOT  RECUR  IF  THE DATA IS      :
;		REWRITTEN.						       :
;									       :
;	IF DRIVE PARAMETERS WERE REQUESTED (DL >= 80H), 		       :
;	   INPUT:							       :
;	     (DL) = DRIVE NUMBER					       :
;	   OUTPUT:							       :
;	     (DL) = NUMBER OF CONSECUTIVE ACKNOWLEDGING DRIVES ATTACHED (1-2)  :
;		    (CONTROLLER CARD ZERO TALLY ONLY)			       :
;	     (DH) = MAXIMUM USEABLE VALUE FOR HEAD NUMBER		       :
;	     (CH) = MAXIMUM USEABLE VALUE FOR CYLINDER NUMBER		       :
;	     (CL) = MAXIMUM USEABLE VALUE FOR SECTOR NUMBER		       :
;		    AND CYLINDER NUMBER HIGH BITS			       :
;									       :
;	REGISTERS WILL BE PRESERVED EXCEPT WHEN THEY ARE USED TO RETURN        :
;	INFORMATION.							       :
;									       :
;	NOTE: IF AN ERROR IS REPORTED BY THE DISK CODE, THE APPROPRIATE        :
;		ACTION IS TO RESET THE DISK, THEN RETRY THE OPERATION.	       :
;									       :
;-------------------------------------------------------------------------------

SENSE_FAIL	EQU	0FFH		; NOT IMPLEMENTED
NO_ERR		EQU	0E0H		; STATUS ERROR/ERROR REGISTER=0
WRITE_FAULT	EQU	0CCH		; WRITE FAULT ON SELECTED DRIVE
UNDEF_ERR	EQU	0BBH		; UNDEFINED ERROR OCCURRED
NOT_RDY 	EQU	0AAH		; DRIVE NOT READY
TIME_OUT	EQU	80H		; ATTACHMENT FAILED TO RESPOND
BAD_SEEK	EQU	40H		; SEEK OPERATION FAILED
BAD_CNTLR	EQU	20H		; CONTROLLER HAS FAILED
DATA_CORRECTED	EQU	11H		; ECC CORRECTED DATA ERROR
BAD_ECC 	EQU	10H		; BAD ECC ON DISK READ
BAD_TRACK	EQU	0BH		; NOT IMPLEMENTED
BAD_SECTOR	EQU	0AH		; BAD SECTOR FLAG DETECTED
;DMA_BOUNDARY	EQU	09H		; DATA EXTENDS TOO FAR
INIT_FAIL	EQU	07H		; DRIVE PARAMETER ACTIVITY FAILED
BAD_RESET	EQU	05H		; RESET FAILED
;RECORD_NOT_FND	EQU	04H		; REQUESTED SECTOR NOT FOUND
;BAD_ADDR_MARK	EQU	02H		; ADDRESS MARK NOT FOUND
;BAD_CMD 	EQU	01H		; BAD COMMAND PASSED TO DISK I/O

;--------------------------------------------------------
;							:
; FIXED DISK PARAMETER TABLE				:
;  -  THE TABLE IS COMPOSED OF A BLOCK DEFINED AS:	:
;							:
;  +0	(1 WORD) - MAXIMUM NUMBER OF CYLINDERS		:
;  +2	(1 BYTE) - MAXIMUM NUMBER OF HEADS		:
;  +3	(1 WORD) - NOT USED/SEE PC-XT			:
;  +5	(1 WORD) - STARTING WRITE PRECOMPENSATION CYL	:
;  +7	(1 BYTE) - MAXIMUM ECC DATA BURST LENGTH	:
;  +8	(1 BYTE) - CONTROL BYTE 			:
;		   BIT	  7 DISABLE RETRIES -OR-	:
;		   BIT	  6 DISABLE RETRIES		:
;		   BIT	  3 MORE THAN 8 HEADS		:
;  +9	(3 BYTES)- NOT USED/SEE PC-XT			:
; +12	(1 WORD) - LANDING ZONE 			:
; +14	(1 BYTE) - NUMBER OF SECTORS/TRACK		:
; +15	(1 BYTE) - RESERVED FOR FUTURE USE		:
;							:
;	 - TO DYNAMICALLY DEFINE A SET OF PARAMETERS	:
;	   BUILD A TABLE FOR UP TO 15 TYPES AND PLACE	:
;	   THE CORRESPONDING VECTOR INTO INTERRUPT 41	:
;	   FOR DRIVE 0 AND INTERRUPT 46 FOR DRIVE 1.	:
;							:
;--------------------------------------------------------

;--------------------------------------------------------
;							:
; HARDWARE SPECIFIC VALUES				:
;							:
;  -  CONTROLLER I/O PORT				:
;							:
;     > WHEN READ FROM: 				:
;	HF_PORT+0 - READ DATA (FROM CONTROLLER TO CPU)	:
;	HF_PORT+1 - GET ERROR REGISTER			:
;	HF_PORT+2 - GET SECTOR COUNT			:
;	HF_PORT+3 - GET SECTOR NUMBER			:
;	HF_PORT+4 - GET CYLINDER LOW			:
;	HF_PORT+5 - GET CYLINDER HIGH (2 BITS)		:
;	HF_PORT+6 - GET SIZE/DRIVE/HEAD 		:
;	HF_PORT+7 - GET STATUS REGISTER 		:
;							:
;     > WHEN WRITTEN TO:				:
;	HF_PORT+0 - WRITE DATA (FROM CPU TO CONTROLLER) :
;	HF_PORT+1 - SET PRECOMPENSATION CYLINDER	:
;	HF_PORT+2 - SET SECTOR COUNT			:
;	HF_PORT+3 - SET SECTOR NUMBER			:
;	HF_PORT+4 - SET CYLINDER LOW			:
;	HF_PORT+5 - SET CYLINDER HIGH (2 BITS)		:
;	HF_PORT+6 - SET SIZE/DRIVE/HEAD 		:
;	HF_PORT+7 - SET COMMAND REGISTER		:
;							:
;--------------------------------------------------------

;HF_PORT 	EQU	01F0H	; DISK PORT
;HF1_PORT	equ	0170h	
;HF_REG_PORT	EQU	03F6H
;HF1_REG_PORT	equ	0376h

HDC1_BASEPORT	equ	1F0h
HDC2_BASEPORT	equ	170h		

align 2

;-----		STATUS REGISTER

ST_ERROR	EQU	00000001B	;
ST_INDEX	EQU	00000010B	;
ST_CORRCTD	EQU	00000100B	; ECC CORRECTION SUCCESSFUL
ST_DRQ		EQU	00001000B	;
ST_SEEK_COMPL	EQU	00010000B	; SEEK COMPLETE
ST_WRT_FLT	EQU	00100000B	; WRITE FAULT
ST_READY	EQU	01000000B	;
ST_BUSY 	EQU	10000000B	;

;-----		ERROR REGISTER

ERR_DAM 	EQU	00000001B	; DATA ADDRESS MARK NOT FOUND
ERR_TRK_0	EQU	00000010B	; TRACK 0 NOT FOUND ON RECAL
ERR_ABORT	EQU	00000100B	; ABORTED COMMAND
;		EQU	00001000B	; NOT USED
ERR_ID		EQU	00010000B	; ID NOT FOUND
;		EQU	00100000B	; NOT USED
ERR_DATA_ECC	EQU	01000000B
ERR_BAD_BLOCK	EQU	10000000B


RECAL_CMD	EQU	00010000B	; DRIVE RECAL	(10H)
READ_CMD	EQU	00100000B	;	READ	(20H)
WRITE_CMD	EQU	00110000B	;	WRITE	(30H)
VERIFY_CMD	EQU	01000000B	;	VERIFY	(40H)
FMTTRK_CMD	EQU	01010000B	; FORMAT TRACK	(50H)
INIT_CMD	EQU	01100000B	;   INITIALIZE	(60H)
SEEK_CMD	EQU	01110000B	;	SEEK	(70H)
DIAG_CMD	EQU	10010000B	; DIAGNOSTIC	(90H)
SET_PARM_CMD	EQU	10010001B	; DRIVE PARMS	(91H)
NO_RETRIES	EQU	00000001B	; CHD MODIFIER	(01H)
ECC_MODE	EQU	00000010B	; CMD MODIFIER	(02H)
BUFFER_MODE	EQU	00001000B	; CMD MODIFIER	(08H)

;MAX_FILE	EQU	2
;S_MAX_FILE	EQU	2
MAX_FILE	equ	4		; 22/12/2014
S_MAX_FILE	equ	4		; 22/12/2014

DELAY_1 	EQU	25H		; DELAY FOR OPERATION COMPLETE
DELAY_2 	EQU	0600H		; DELAY FOR READY
DELAY_3 	EQU	0100H		; DELAY FOR DATA REQUEST

HF_FAIL 	EQU	08H		; CMOS FLAG IN BYTE 0EH

;-----		COMMAND BLOCK REFERENCE

;CMD_BLOCK      EQU     BP-8            ; @CMD_BLOCK REFERENCES BLOCK HEAD IN SS
					;  (BP) POINTS TO COMMAND BLOCK TAIL
					;	AS DEFINED BY THE "ENTER" PARMS
; 19/12/2014
ORG_VECTOR	equ	4*13h		; INT 13h vector
DISK_VECTOR	equ	4*40h		; INT 40h vector (for floppy disks)
;HDISK_INT	equ	4*76h		; Primary HDC - Hardware interrupt (IRQ14)
;HDISK_INT1	equ	4*76h		; Primary HDC - Hardware interrupt (IRQ14)
;HDISK_INT2	equ	4*77h		; Secondary HDC - Hardware interrupt (IRQ15)
;HF_TBL_VEC	equ	4*41h		; Pointer to 1st fixed disk parameter table
;HF1_TBL_VEC	equ	4*46h		; Pointer to 2nd fixed disk parameter table

align 2

;----------------------------------------------------------------
; FIXED DISK I/O SETUP						:
;								:
;  -  ESTABLISH TRANSFER VECTORS FOR THE FIXED DISK		:
;  -  PERFORM POWER ON DIAGNOSTICS				:
;     SHOULD AN ERROR OCCUR A "1701" MESSAGE IS DISPLAYED       :
;								:
;----------------------------------------------------------------

	; 12/07/2022
	; 11/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

DISK_SETUP:
	;cli
	;;mov	ax, ABS0 			; GET ABSOLUTE SEGMENT
	;xor	ax, ax
	;mov	ds, ax				; SET SEGMENT REGISTER
	;mov	ax, [ORG_VECTOR] 		; GET DISKETTE VECTOR
	;mov	[DISK_VECTOR], ax		; INTO INT 40H
	;mov	ax, [ORG_VECTOR+2]
	;mov	[DISK_VECTOR+2], ax
	;mov	word [ORG_VECTOR], DISK_IO	; FIXED DISK HANDLER
	;mov	[ORG_VECTOR+2], cs
	; 1st controller (primary master, slave) - IRQ 14
	;;mov	word [HDISK_INT], HD_INT	; FIXED DISK INTERRUPT
	;mov	word [HDISK_INT1], HD_INT	;
	;;mov	[HDISK_INT+2], cs
	;mov	[HDISK_INT1+2], cs
	; 2nd controller (secondary master, slave) - IRQ 15
	;mov	word [HDISK_INT2], HD1_INT	;
	;mov	[HDISK_INT2+2], cs
	;
	;;mov	word [HF_TBL_VEC], HD0_DPT	; PARM TABLE DRIVE 80
	;;mov	word [HF_TBL_VEC+2], DPT_SEGM
	;;mov	word [HF1_TBL_VEC], HD1_DPT	; PARM TABLE DRIVE 81
	;;mov	word [HF1_TBL_VEC+2], DPT_SEGM
	;push	cs
	;pop	ds
	;mov	word [HDPM_TBL_VEC],HD0_DPT	; PARM TABLE DRIVE 80h
	;mov	word [HDPM_TBL_VEC+2],DPT_SEGM
	mov 	dword [HDPM_TBL_VEC], (DPT_SEGM*16)+HD0_DPT
	;mov	word [HDPS_TBL_VEC],HD1_DPT	; PARM TABLE DRIVE 81h
	;mov	word [HDPS_TBL_VEC+2],DPT_SEGM
	mov 	dword [HDPS_TBL_VEC], (DPT_SEGM*16)+HD1_DPT
	;mov	word [HDSM_TBL_VEC],HD2_DPT	; PARM TABLE DRIVE 82h
	;mov	word [HDSM_TBL_VEC+2],DPT_SEGM
	mov 	dword [HDSM_TBL_VEC], (DPT_SEGM*16)+HD2_DPT
	;mov	word [HDSS_TBL_VEC],HD3_DPT	; PARM TABLE DRIVE 83h
	;mov	word [HDSS_TBL_VEC+2],DPT_SEGM
	mov 	dword [HDSS_TBL_VEC], (DPT_SEGM*16)+HD3_DPT
	;
	;;in	al, INTB01		; TURN ON SECOND INTERRUPT CHIP
	;;;and	al, 0BFh
	;;and	al, 3Fh			; enable IRQ 14 and IRQ 15
	;;;JMP	$+2
	;;IODELAY
	;;out	INTB01, al
	;;IODELAY
	;;in	al, INTA01		; LET INTERRUPTS PASS THRU TO
	;;and	al, 0FBh 		; SECOND CHIP
	;;;JMP	$+2
	;;IODELAY
	;;out	INTA01, al
	;
	;sti
	;;push	ds			; MOVE ABS0 POINTER TO
	;;pop	es			; EXTRA SEGMENT POINTER
	;;;call	DDS			; ESTABLISH DATA SEGMENT
	;;mov	byte [DISK_STATUS1],0 	; RESET THE STATUS INDICATOR
	;;mov	byte [HF_NUM],0		; ZERO NUMBER OF FIXED DISKS
	;;mov	byte [CONTROL_BYTE],0
	;;mov	byte [PORT_OFF],0	; ZERO CARD OFFSET
	; 20/12/2014 - private code by Erdogan Tan
		      ; (out of original PC-AT, PC-XT BIOS code)
	;mov	si, hd0_type
	mov	esi, hd0_type
	;;mov	cx, 4
	;mov	ecx, 4
	; 11/07/2022
	sub	ecx, ecx
	mov	cl, 4
hde_l:
	lodsb
	cmp	al, 80h			; 8?h = existing
	jb	short _L4
	inc	byte [HF_NUM]		; + 1 hard (fixed) disk drives
_L4: ; 26/02/2015
	loop	hde_l	
;_L4:					; 0 <= [HF_NUM] =< 4
;L4:
	; 
	;; 31/12/2014 - cancel controller diagnostics here
	;;;mov 	cx, 3  ; 26/12/2014 (Award BIOS 1999)
	;;mov 	cl, 3
	;;
	;;mov	dl, 80h			; CHECK THE CONTROLLER
;;hdc_dl:
	;;mov	ah, 14h			; USE CONTROLLER DIAGNOSTIC COMMAND
	;;int	13h			; CALL BIOS WITH DIAGNOSTIC COMMAND
	;;;jc	short CTL_ERRX		; DISPLAY ERROR MESSAGE IF BAD RETURN
	;;;jc	short POD_DONE ;22/12/2014
	;;jnc	short hdc_reset0
	;;loop	hdc_dl
	;;; 27/12/2014
	;;stc
	;;retn
	;
;;hdc_reset0:
	; 18/01/2015
	mov	cl, [HF_NUM]
	and	cl, cl
	jz	short POD_DONE
	;
	mov	dl, 7Fh
hdc_reset1:
	inc	dl
	;; 31/12/2015
	;;push	dx
	;;push	cx
	;;push	ds
	;;sub	ax, ax
	;;mov	ds, ax
	;;mov	ax, [TIMER_LOW]		; GET START TIMER COUNTS
	;;pop	ds
	;;mov	bx, ax
	;;add	ax, 6*182		; 60 SECONDS* 18.2
	;;mov	cx, ax
	;;mov	word [wait_count], 0	; 22/12/2014 (reset wait counter)
	;;
	;; 31/12/2014 - cancel HD_RESET_1
	;;call	HD_RESET_1		; SET UP DRIVE 0, (1,2,3)
	;;pop	cx
	;;pop	dx
	;;
	; 18/01/2015
	;mov	ah, 0Dh ; ALTERNATE RESET
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;mov	ah, 5 ; ALTERNATE RESET
	;;int	13h
	;call	int13h
	; 12/07/2022
	xor	al, al  ; reset
	call	DISK_IO	
	;
	loop	hdc_reset1
POD_DONE:
	retn

;----------------------------------------
;	FIXED DISK BIOS ENTRY POINT	:
;----------------------------------------

; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
; ((Direct call instead of int 13h simulation))
;
;		Function in AL
;			0 = reset
;			1 = read
;			2 = write
;		Disk drive number in DL
;			0 & 1 = floppy disks	
;			80h .. 83h = hard disks
;		Sector address (LBA) in ECX
;		Buffer address in EBX
;		R/W sector count is (always) 1
;
;		Return:
;			Status in AH (>0 = error code)
;			if CF = 1 -> error code in AH
;			if CF = 0 -> successful
;			AL = undefined
;
;		Modified registers: (only) EAX
	

; 11/07/2022
; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

DISK_IO:
	; 11/07/2022
	; save registers
	push	edi			; ANY
	push	esi			; ANY
	push	ebx			; BUFFER ADDRESS
	push	ecx			; SECTOR ADDRESS (LBA)
	push	edx			; DRIVE NUMBER (DL)

	;cmp	dl, 80h			; TEST FOR FIXED DISK DRIVE
	;;jae	short A1		; YES, HANDLE HERE
	;;;;int	40H			; DISKETTE HANDLER
	;;;call	int40h
	;;jb	DISKETTE_IO_1
	;; 24/12/2021
	;jnb	short A1
	;jmp	DISKETTE_IO_1

	; 11/07/2022
	cmp	dl, 80h
	jae	short A1

	call	DISKETTE_IO_1

DISK_IO_RTN:
	; restore registers
	pop	edx
	pop	ecx
	pop	ebx
	pop	esi
	pop	edi
	retn	

;RET_2:
;	retf	4			; BACK TO CALLER

A1:
	; 11/07/2022
	;sti				; ENABLE INTERRUPTS
	;cmp	dl, (80h + S_MAX_FILE - 1)
	;ja	short RET_2
	
	; 18/01/2015
	;;or	ah, ah
	;or	al, al ; 11/07/2022 (reset function)
	;jz	short A3 ; 08/07/2022
	
	;;cmp	ah, 5  ; Alternate reset
	;cmp	al, 5  ; 11/07/2022
	;je	short A2
	
	; 11/07/2022 - no need to check
	;		 (only kernel calls diskio functions)
	;;cmp	ah, M1L/4 ; cmp ah, 6
	;jb	short A3
	;; BAD COMMAND
        ;mov     byte [DISK_STATUS1], BAD_CMD
;RET_2:
	;retf	4

	; 11/07/2022
	;stc
	;retn
A2:
	;sub	ah, ah	; Reset
	; 11/07/2022
	;sub	al, al
A3:
					; SAVE REGISTERS DURING OPERATION
	enter	8,0			; SAVE (EBP) AND MAKE ROOM FOR @CMD_BLOCK
	
	; 11/07/2022
	; 08/07/2022
	;push	ebx			;  IN THE STACK, THE COMMAND BLOCK IS:
	;push	ecx			;   @CMD_BLOCK == BYTE PTR [EBP]-8
	;push	edx
	;push	esi
	;push	edi
	
	call	DISK_IO_CONT		; PERFORM THE OPERATION

	leave	; 11/07/2022
	
	mov	ah, [DISK_STATUS1]	; GET STATUS FROM OPERATION
	cmp	ah, 1			; SET THE CARRY FLAG TO INDICATE
	cmc				; SUCCESS OR FAILURE
	
	;pop	edi			; RESTORE REGISTERS
	;pop	esi
      	;pop	edx
	;pop	ecx
	;pop	ebx
	
	;leave				; ADJUST (ESP) AND RESTORE (EBP)
	
	; 11/07/2022
	;retf	4			; THROW AWAY SAVED FLAGS

	jmp	short DISK_IO_RTN

DISK_IO_CONT:
	; 17/07/2022
	; 11/07/2022
	;	INPUT:
	;	    AL = 0 : reset
	;	    AL = 1 : read
	;	    Al = 2 : write
	; 	
	; 10/07/2022
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
SU0:
	mov	byte [DISK_STATUS1], 0 	; RESET THE STATUS INDICATOR
	; 10/07/2022
	mov	esi, ebx ; 21/02/2015	; DATA (BUFFER) ADDRESS

	mov	bl, [HF_NUM]		; GET NUMBER OF DRIVES
	and	dl, 7Fh			; GET DRIVE AS 0 OR 1
					; (get drive number as 0 to 3)
	cmp	bl, dl
        ;;jbe	BAD_COMMAND_POP         ; INVALID DRIVE
        ;jbe    BAD_COMMAND ;; 14/02/2015
	; 24/12/2021
	ja	short su0_su1
	jmp	BAD_COMMAND
su0_su1:
        ;;03/01/2015
	sub	ebx, ebx
	mov	bl, dl
	mov	[LBAMode], bh  ; 0
	;test	byte [ebx+hd0_type], 1	; LBA ready ?
	;jz	short su1		; no
	;inc	byte [LBAMode]
;su1:
	; 21/02/2015 (32 bit modification)
	; 04/01/2015
	;push	ax ; ***
	; 24/12/2021
	push	eax ; *** ; function (in AL) ; 11/07/2022
	; 24/12/2021
	push	edx ; *
	push	eax ; function (in AL)	; 11/07/2022
	CALL	GET_VEC 		; GET DISK PARAMETERS
	; 02/02/2015
	mov	ax, [ebx+16]   ; I/O port base address (1F0h, 170h)
	mov	[HF_PORT], ax
	mov	dx, [ebx+18]   ; control port address (3F6h, 376h)
	mov	[HF_REG_PORT], dx
	mov	al, [ebx+20]   ; head register upper nibble (A0h,B0h,E0h,F0h)
	; 23/02/2015
	test	al, 40h	 ; LBA bit (bit 6)
	jz 	short su1
	inc	byte [LBAMode] ; 1 
su1: 	 
	shr 	al, 4
	and	al, 1			
	mov	[hf_m_s], al 
	;
	; 03/01/2015
	mov	al, [ebx+8]		; GET CONTROL BYTE MODIFIER
	;mov	dx, [HF_REG_PORT]	; Device Control register	
	out	dx, al			; SET EXTRA HEAD OPTION
					; Control Byte: (= 08h, here)
					; bit 0 - 0
					; bit 1 - nIEN (1 = disable irq)
					; bit 2 - SRST (software RESET)
					; bit 3 - use extra heads (8 to 15)
					;         -always set to 1-	
					; (bits 3 to 7 are reserved
					;          for ATA devices)
	mov	ah, [CONTROL_BYTE]	; SET EXTRA HEAD OPTION IN
	and	ah, 0C0h 		; CONTROL BYTE
	or	ah, al
	mov	[CONTROL_BYTE], ah	
	; 04/01/2015
	;pop	ax
	; 24/12/2021
	pop	eax ; function (in AL) ; 11/07/2022
	;pop	dx ; * ;; 14/02/2015
	; 24/12/2021
	pop	edx ; *
	;and	ah, ah	; Reset function ?
	and	al, al	; 11/07/2022
	jnz	short su2
	;pop	ax ; ***
	; 24/12/2021
	pop	eax ; *** 	
	;;pop	bx
        jmp     DISK_RESET
su2:
	; 11/07/2022
	; ecx = sector address (lba)
	;  dl = hard disk drive number (80h, 81h .. 83h)	 
	;  al = function (0 = read, 1 = write)	

	cmp	byte [LBAMode], 0
	jna	short su3 ; convert LBA address to CHS parameters
	
;	; 02/02/2015 (LBA read/write function calls)
;	;cmp	ah, 1Bh
;	cmp	ah, 3 ; 08/07/2022
;	jb	short lbarw1
;	;;cmp	ah, 1Ch
;	;cmp	ah, 4 ; 08/07/2022 
;	;ja 	short invldfnc
;	;;pop	dx ; * ; 14/02/2015
;	;mov	ax, cx ; Lower word of LBA address (bits 0-15)

	mov	eax, ecx ; LBA address (21/02/2015)

	; 11/07/2022
	;; 14/02/2015
	;mov	cl, dl ; 14/02/2015

	;;mov	dx, bx
	;mov	dx, si ; higher word of LBA address (bits 16-23)
	;;mov	bx, di
	;mov	si, di ; Buffer offset

	; 11/07/2022
	;jmp	short lbarw2

;lbarw1:
;	; convert CHS to LBA
;	;
;	; LBA calculation - AWARD BIOS - 1999 - AHDSK.ASM
;	; LBA = "# of Heads" * Sectors/Track * Cylinder + Head * Sectors/Track
;	;	+ Sector - 1
;	;push	dx ; * ;; 14/02/2015
;	; 24/12/2021
;	push	edx ; *
;	;xor	dh, dh
;	xor	edx, edx
;	mov	dl, [ebx+14]	; sectors per track (logical)
;	;xor	ah, ah
;	xor	eax, eax
;	mov	al, [ebx+2]	; heads (logical) 
;	dec	al
;	;inc	ax		; 0 =  256
;	inc	eax ; 24/12/2021
;	mul 	dx
;		; AX = # of Heads * Sectors/Track
;	mov	dx, cx
;	;and	cx, 3Fh	 ; sector (1 to 63)
;	and	ecx, 3fh
;	xchg	dl, dh
;	shr	dh, 6
;		; DX = cylinder (0 to 1023)
;	;mul 	dx
;		; DX:AX = # of Heads * Sectors/Track * Cylinder
;	mul	edx
;	dec	cl  ; sector - 1
;	;add	ax, cx
;	;adc	dx, 0
;		; DX:AX = # of Heads * Sectors/Track * Cylinder + Sector -1
;	add	eax, ecx
;	;pop	cx ; * ; ch = head, cl = drive number (zero based)
;	; 24/12/2021
;	pop	ecx ; * ; ch = head, cl = drive number (zero based)
;	;push	dx
;	;push	ax
;	push	eax
;	mov	al, [ebx+14]  ; sectors per track (logical)	
;	mul	ch
;		; AX = Head * Sectors/Track
;	cwd
;	;pop	dx
;	pop	edx
;	;add	ax, dx
;	;pop	dx
;	;adc	dx, 0 ; add carry bit
;	add	eax, edx
;
;lbarw2:
	; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	
	sub	edx, edx ; 21/02/2015
	
	; 11/07/2022
	;mov	dl, cl ; 21/02/2015
        
	mov     byte [CMD_BLOCK], 0 ; Features Register
				; NOTE: Features register (1F1h, 171h)
				; is not used for ATA device R/W functions. 
				; It is old/obsolete 'write precompensation'
				; register and error register
				; for old ATA/IDE devices.
	; 18/01/2014
	;mov	ch, [hf_m_s]	; Drive 0 (master) or 1 (slave)
	mov	cl, [hf_m_s]
	;shl	ch, 4		; bit 4 (drive bit)
	;or	ch, 0E0h	; bit 5 = 1
				; bit 6 = 1 = LBA mode
				; bit 7 = 1
	or	cl, 0Eh ; 1110b
	;and	dh, 0Fh		; LBA byte 4 (bits 24 to 27)
	and	eax, 0FFFFFFFh
	shl	ecx, 28 ; 21/02/2015
	;or	dh, ch
	or	eax, ecx	
	;;mov	[CMD_BLOCK+2], al ; LBA byte 1 (bits 0 to 7)
				  ; (Sector Number Register)
	;;mov	[CMD_BLOCK+3], ah ; LBA byte 2 (bits 8 to 15)
				  ; (Cylinder Low Register)
	;mov	[CMD_BLOCK+2], ax ; LBA byte 1, 2
	;mov	[CMD_BLOCK+4], dl ; LBA byte 3 (bits 16 to 23)
				  ; (Cylinder High Register)
	;;mov	[CMD_BLOCK+5], dh ; LBA byte 4 (bits 24 to 27)
				  ; (Drive/Head Register)
	
	;mov	[CMD_BLOCK+4], dx ; LBA byte 4, LBA & DEV select bits
	mov	[CMD_BLOCK+2], eax ; 21/02/2015
	;14/02/2015
	;mov	dl, cl ; Drive number (INIT_DRV)		
	jmp	short su4
su3:
	; 02/02/2015 
	; (Temporary functions 1Bh & 1Ch are not valid for CHS mode) 
	;cmp 	ah, 14h
	;jna 	short chsfnc
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;cmp	ah, 2
	;jna	short chsfnc
	; 11/07/2022 
	; (al = function, read = 1 or write = 2)
	cmp	al, 2 
	jna	short chsfnc
invldfnc:
        ; 14/02/2015  
	;pop	es ; **
        ;pop	ax ; ***
        ; 24/12/2021
	pop	eax ; ***
	;;jmp	short BAD_COMMAND_POP
	; 11/07/2022
        ;jmp     short BAD_COMMAND

	; 11/07/2022
BAD_COMMAND:
	mov	byte [DISK_STATUS1], BAD_CMD  ; COMMAND ERROR
	;mov	al, 0
	sub	al, al ; 0
	retn

chsfnc:	
	mov	ax, [ebx+5]		; GET WRITE PRE-COMPENSATION CYLINDER
	;shr	ax, 2
	; 17/07/2022
	shr	eax, 2
	mov	[CMD_BLOCK], al
	;
	;;mov	al, [ebx+8]		; GET CONTROL BYTE MODIFIER
	;;push	edx ; *
	;;mov	dx, [HF_REG_PORT]
	;;out	dx, al			; SET EXTRA HEAD OPTION
	;;pop	edx ; * 
	;;mov	ah, [CONTROL_BYTE]	; SET EXTRA HEAD OPTION IN
	;;and	ah, 0C0h 		; CONTROL BYTE	
	;;or	ah, al
	;;mov	[CONTROL_BYTE], ah
	;
	mov	al, cl			; GET SECTOR NUMBER
	and	al, 3Fh
	mov	[CMD_BLOCK+2], al
	mov	[CMD_BLOCK+3], ch 	; GET CYLINDER NUMBER
	mov	al, cl
	shr	al, 6
	mov	[CMD_BLOCK+4], al	; CYLINDER HIGH ORDER 2 BITS
	;;05/01/2015
	;;mov	al, dl			; DRIVE NUMBER
	mov	al, [hf_m_s]
	shl	al, 4
	and	dh, 0Fh			; HEAD NUMBER
	or	al, dh
	or	al, 80h+20h		; ECC AND 512 BYTE SECTORS
	mov	[CMD_BLOCK+5], al	; ECC/SIZE/DRIVE/HEAD
su4:
	;; 14/02/2015
        ;;pop	ax
        ;;mov	[CMD_BLOCK+1], AL	; SECTOR COUNT
        ;;push	ax
        ;;mov	al, ah			; GET INTO LOW BYTE
        ;;xor	ah, ah			; ZERO HIGH BYTE
        ;;sal	ax, 1			; *2 FOR TABLE LOOKUP
	;pop	ax ; ***
	; 24/12/2021
	pop	eax ; *** ; function (in AL) ; 11/07/2022
	
	;mov	[CMD_BLOCK+1], al
        mov	byte [CMD_BLOCK+1], 1 ; (always 1 sector r/w)

	; 11/07/2022
	;mov	ebx, esi
	; (esi = buffer address)	

	cmp	al, 2
	je	short DISK_WRITE

	;jmp	short DISK_READ

;	;xor	ebx, ebx
;	;mov	bl, ah
;
;       ;xor	bh, bh
;       ;sal	bx, 1
;       sal	bx, 2	; 32 bit offset (21/02/2015)
;	;;mov	si, ax			; PUT INTO SI FOR BRANCH
;       ;;cmp	ax, M1L			; TEST WITHIN RANGE
;       ;;jnb	short BAD_COMMAND_POP
;   	; 08/07/2022
;	;cmp	ebx, M1L
;	;jnb	short BAD_COMMAND
;
;	xchg	ebx, esi
;
;	;;;pop	ax			; RESTORE AX
;	;;;pop	bx			; AND DATA ADDRESS
;	
;	;;push	cx
;	;;push	ax			; ADJUST ES:BX
;	;mov	cx, bx			; GET 3 HIGH ORDER NIBBLES OF BX
;	;shr	cx, 4
;	;mov	ax, es
;	;add	ax, cx
;	;mov	es, ax
;	;and	bx, 000Fh		; ES:BX CHANGED TO ES:000X
;	;;pop	ax
;	;;pop	cx
;
;	jmp	dword [esi+M1]

;;BAD_COMMAND_POP:
;;	pop	ax
;;	pop	bx
;
;	; 11/07/2022
;BAD_COMMAND:
;	mov	byte [DISK_STATUS1], BAD_CMD  ; COMMAND ERROR
;	;mov	al, 0
;	sub	al, al ; 0
;	retn

; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	DISK READ ROUTINE    (AH = 01H) :
;----------------------------------------
; 
DISK_READ:
	mov	byte [CMD_BLOCK+6], READ_CMD
        ;jmp	COMMANDI

; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
; COMMANDI				:
;	REPEATEDLY INPUTS DATA TILL	:
;	NSECTOR RETURNS ZERO		:
;----------------------------------------
COMMANDI:
	; 11/07/2022 
	;	(check 64K boundary is not needed)
	;call	CHECK_DMA		; CHECK 64K BOUNDARY ERROR
	;jc	short CMD_ABORT
	
	;mov	di, bx
	; 11/07/2022
	; (esi = buffer address)
	;mov	edi, ebx ; 21/02/2015
	mov	edi, esi ; 11/07/2022	

	call	COMMAND 		; OUTPUT COMMAND
	jnz	short CMD_ABORT
CMD_I1:
	call	_WAIT			; WAIT FOR DATA REQUEST INTERRUPT
	jnz	short TM_OUT		; TIME OUT
	;;mov	cx,256			; SECTOR SIZE IN WORDS
	;mov	ecx, 256 ; 21/02/2015	
	sub	ecx, ecx
	inc	ch
	; ecx = 256
	;mov	dx, HF_PORT
	mov	dx, [HF_PORT]
	cli
	cld
	rep	insw			; GET THE SECTOR
	sti
	
	;test	byte [CMD_BLOCK+6], ECC_MODE ; CHECK FOR NORMAL INPUT
	;jz	short CMD_I3
	;call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	;jc	short TM_OUT
	;;mov	dx, HF_PORT
	;mov	dx,[HF_PORT]
	;xor	ecx, ecx	
	;;mov	ecx, 4  ; mov cx, 4	; OUTPUT THE ECC BYTES
	;mov	cl, 4
;CMD_I2: 
	;inc	al, dx
	;mov 	[edi], al ; 21/02/2015
	;inc	edi
	;loop	CMD_I2
CMD_I3: 
	call	CHECK_STATUS
	jnz	short CMD_ABORT		; ERROR RETURNED
	; 11/07/2022
	; (sector count = 1)
	;dec	byte [CMD_BLOCK+1]	; CHECK FOR MORE
	;jnz	SHORT CMD_I1
CMD_ABORT:
TM_OUT: 
	retn

;---------------------------------------------------

; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	DISK WRITE ROUTINE   (AH = 02H) :
;----------------------------------------

DISK_WRITE:
	mov	byte [CMD_BLOCK+6], WRITE_CMD
        ;JMP	COMMANDO

; 16/07/2022
; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
; COMMANDO				:
;	REPEATEDLY OUTPUTS DATA TILL	:
;	NSECTOR RETURNS ZERO		:
;----------------------------------------
COMMANDO:
	; 11/07/2022 
	;	(check 64K boundary is not needed)
	;call	CHECK_DMA		; CHECK 64K BOUNDARY ERROR
	;jc	short CMD_ABORT
CMD_OF:
	; 11/07/2022
	; (esi = ebx = buffer address)
	;mov	esi, ebx ; 21/02/2015
	call	COMMAND 		; OUTPUT COMMAND
	jnz	short CMD_ABORT
	call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	jc	short TM_OUT		; TOO LONG
CMD_O1:
	; 16/07/2022
	mov	dx, [HF_PORT]

	; 10/07/2022
	;mov	ecx, 256 ; 21/02/2015
	xor	ecx, ecx
	inc	ch
	; ecx = 256
	cli
	cld
	rep	outsw
	sti

	; 10/07/2022
	;test	byte [CMD_BLOCK+6], ECC_MODE ; CHECK FOR NORMAL OUTPUT
	;jz	short CMD_O3
	;
	;call	WAIT_DRQ		; WAIT FOR DATA REQUEST
	;jc	short TM_OUT
	;;mov	dx, HF_PORT
	;mov	dx, [HF_PORT]
	;sub	ecx, ecx	
	;;mov	ecx, 4  ; mov cx, 4	; OUTPUT THE ECC BYTES
	;mov	cl, 4
;CMD_O2:
	;;lodsb
	;mov	al, [esi]
	;out	dx, al
	;inc	esi
	;loop	CMD_O2

CMD_O3:
	call	_WAIT			; WAIT FOR SECTOR COMPLETE INTERRUPT
	jnz	short TM_OUT		; ERROR RETURNED
	call	CHECK_STATUS
	jnz	short CMD_ABORT

	; 11/07/2022
	; (sector count = 1)
	;test	byte [HF_STATUS], ST_DRQ ; CHECK FOR MORE
	;jnz	short CMD_O1
	
	;mov	dx, HF_PORT+2		; CHECK RESIDUAL SECTOR COUNT
	mov	dx, [HF_PORT]
	add	dl, 2
	;inc	dl
	;inc	dl
	in	al, dx			;
	test	al, 0FFh 		;
	jz	short CMD_O4		; COUNT = 0  OK
	mov	byte [DISK_STATUS1], UNDEF_ERR 
					; OPERATION ABORTED - PARTIAL TRANSFER
CMD_O4:
	retn

; 10/07/2022
; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	RESET THE DISK SYSTEM  (AH=00H) :
;----------------------------------------

; 18-1-2015 : one controller reset (not other one)

DISK_RESET:
	cli
	in	al, INTB01		; GET THE MASK REGISTER
	;JMP	$+2
	IODELAY
	;and	al, 0BFh 		; ENABLE FIXED DISK INTERRUPT
	and	al, 3Fh			; 22/12/2014 (IRQ 14 & IRQ 15)
	out	INTB01, al
	sti				; START INTERRUPTS
	; 14/02/2015
	;mov	di, dx
	; 24/12/2021
	mov	edi, edx	
	; 04/01/2015
	;xor	di,di
drst0:
	mov	al, 04h  ; bit 2 - SRST 
	;mov	dx, HF_REG_PORT
	mov	dx, [HF_REG_PORT]
	out	dx, al			; RESET
;	mov	cx, 10			; DELAY COUNT
;DRD:	dec	cx
;	jnz	short DRD		; WAIT 4.8 MICRO-SEC
	;mov	cx, 2			; wait for 30 micro seconds	
        ;mov	ecx, 2 ; 21/02/2015
	; 10/07/2022
	sub	ecx, ecx
	mov	cl, 2
	call    WAITF                   ; (Award Bios 1999 - WAIT_REFRESH,
                                        ; 40 micro seconds)
	mov	al, [CONTROL_BYTE]
	and	al, 0Fh			; SET HEAD OPTION
	out	dx, al			; TURN RESET OFF
	call	NOT_BUSY
	jnz	short DRERR		; TIME OUT ON RESET
	mov	dx, [HF_PORT]
	inc	dl  ; HF_PORT+1
	; 02/01/2015 - Award BIOS 1999 - AHDSK.ASM
        ;mov	cl, 10
        ;mov     ecx, 10 ; 21/02/2015
	; 10/07/2022
	;xor	ecx, ecx
	mov	cl, 10 
drst1:
	in	al, dx			; GET RESET STATUS
	cmp	al, 1
	; 04/01/2015
	jz	short drst2
	;jnz	short DRERR		; BAD RESET STATUS
        	; Drive/Head Register - bit 4
	;loop	drst1
	; 10/07/2022
	dec	cl
	jnz	short drst1
DRERR:	
	mov	byte [DISK_STATUS1], BAD_RESET ; CARD FAILED
	retn
drst2:
	; 14/02/2015
	;mov	dx, di
	; 24/12/2021
	mov	edx, edi
;drst3:
;	; 05/01/2015
;	shl 	di, 1
;	; 04/01/2015
;	mov	ax, [di+hd_cports]
;	cmp	ax, [HF_REG_PORT]
;	je	short drst4
;	mov	[HF_REG_PORT], ax
;	; 03/01/2015
;	mov	ax, [di+hd_ports]
;       mov     [HF_PORT], ax
;	; 05/01/2014
;	shr	di, 1
;	; 04/01/2015
;	jmp	short drst0	; reset other controller
;drst4:
;	; 05/01/2015
;	shr	di, 1
;	mov	al, [di+hd_dregs]
;	and	al, 10h ; bit 4 only
;	shr	al, 4 ; bit 4  -> bit 0
;	mov	[hf_m_s], al ; (0 = master, 1 = slave)
	;
	mov	al, [hf_m_s] ; 18/01/2015
	test	al, 1
	;jnz	short drst6
        jnz     short drst4
	and	byte [CMD_BLOCK+5], 0EFh ; SET TO DRIVE 0
;drst5:
drst3:
	call	INIT_DRV		; SET MAX HEADS
	;mov	dx, di
	call	HDISK_RECAL		; RECAL TO RESET SEEK SPEED
	; 04/01/2014
;	inc	di
;	mov	dx, di
;	cmp	dl, [HF_NUM]
;	jb	short drst3
;DRE:
	mov	byte [DISK_STATUS1], 0 	; IGNORE ANY SET UP ERRORS
	retn
;drst6:
drst4:		; Drive/Head Register - bit 4
	or	byte [CMD_BLOCK+5], 010h ; SET TO DRIVE 1     
        ;jmp    short drst5
        jmp     short drst3

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	INITIALIZE DRIVE     (AH = 09H) :
;----------------------------------------
	; 03/01/2015
	; According to ATA-ATAPI specification v2.0 to v5.0
	; logical sector per logical track
	; and logical heads - 1 would be set but
	; it is seen as it will be good
	; if physical parameters will be set here
	; because, number of heads <= 16.
	; (logical heads usually more than 16)
	; NOTE: ATA logical parameters (software C, H, S)
	;	== INT 13h physical parameters

;INIT_DRV:
;	mov	byte [CMD_BLOCK+6], SET_PARM_CMD
;	call	GET_VEC 		; ES:BX -> PARAMETER BLOCK
;	mov	al, [es:bx+2]		; GET NUMBER OF HEADS
;	dec	al			; CONVERT TO 0-INDEX
;	mov	ah, [CMD_BLOCK+5] 	; GET SDH REGISTER
;	and	ah, 0F0h 		; CHANGE HEAD NUMBER
;	or	ah, al			; TO MAX HEAD
;	mov	[CMD_BLOCK+5], ah
;	mov	al, [es:bx+14]		; MAX SECTOR NUMBER
;	mov	[CMD_BLOCK+1], al
;	sub	ax, ax
;	mov	[CMD_BLOCK+3], al 	; ZERO FLAGS
;	call	COMMAND 		; TELL CONTROLLER
;	jnz	short INIT_EXIT		; CONTROLLER BUSY ERROR
;	call	NOT_BUSY		; WAIT FOR IT TO BE DONE
;	jnz	short INIT_EXIT		; TIME OUT
;	call	CHECK_STATUS
;INIT_EXIT:
;	retn

; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

; 04/01/2015
; 02/01/2015 - Derived from from AWARD BIOS 1999
;				 AHDSK.ASM - INIT_DRIVE
INIT_DRV:
	;xor	ah, ah
	xor	eax, eax ; 21/02/2015
	mov	al, 11 ; Physical heads from translated HDPT
        cmp     [LBAMode], ah   ; 0
	ja	short idrv0
	mov	al, 2  ; Physical heads from standard HDPT
idrv0:
	; DL = drive number (0 based)
	call	GET_VEC
	;push	bx
	push	ebx ; 21/02/2015
	;add	bx, ax
	add	ebx, eax
	;; 05/01/2015
	mov	ah, [hf_m_s] ; drive number (0= master, 1= slave)
	;;and 	ah, 1 
	shl	ah, 4
	or	ah, 0A0h  ; Drive/Head register - 10100000b (A0h)	
	;mov	al, [es:bx]
	mov	al, [ebx] ; 21/02/2015
	dec	al	 ; last head number 
	;and	al, 0Fh
	or	al, ah	 ; lower 4 bits for head number
	;
	mov	byte [CMD_BLOCK+6], SET_PARM_CMD
	mov	[CMD_BLOCK+5], al
	;pop	bx
	pop	ebx
	sub	eax, eax ; 21/02/2015
	mov	al, 4 ; Physical sec per track from translated HDPT
	cmp	byte [LBAMode], 0
	ja	short idrv1
	mov	al, 14 ; Physical sec per track from standard HDPT
idrv1:
	;xor	ah, ah
	;add	bx, ax
	add	ebx, eax ; 21/02/2015
	;mov	al, [es:bx]
			; sector number
	mov	al, [ebx]
	mov	[CMD_BLOCK+1], al
	sub	al, al
	mov	[CMD_BLOCK+3], al ; ZERO FLAGS
	call	COMMAND 	  ; TELL CONTROLLER
	jnz	short INIT_EXIT	  ; CONTROLLER BUSY ERROR
	call	NOT_BUSY	  ; WAIT FOR IT TO BE DONE
	jnz	short INIT_EXIT	  ; TIME OUT
	;call	CHECK_STATUS
	;jmp	short CHECK_STATUS
;INIT_EXIT:
	;retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	CHECK FIXED DISK STATUS 	:
;----------------------------------------
CHECK_STATUS:
	call	CHECK_ST		; CHECK THE STATUS BYTE
	;jnz	short CHECK_S1		; AN ERROR WAS FOUND
	; 10/07/2022
	jnz	short CHECK_S2
	test	al, ST_ERROR		; WERE THERE ANY OTHER ERRORS
	jz	short CHECK_S1		; NO ERROR REPORTED
	call	CHECK_ER		; ERROR REPORTED
CHECK_S1:
	cmp	byte [DISK_STATUS1], 0 	; SET STATUS FOR CALLER
CHECK_S2:
INIT_EXIT:	; 10/07/2022
	retn

;----------------------------------------
;	TEST DISK READY      (AH = 10H) :
;----------------------------------------

TST_RDY:				; WAIT FOR CONTROLLER
	call	NOT_BUSY
	jnz	short TR_EX
	mov	al, [CMD_BLOCK+5] 	; SELECT DRIVE
	mov	dx, [HF_PORT]
	add	dl,6
	out	dx, al
	call	CHECK_ST		; CHECK STATUS ONLY
	jnz	short TR_EX
	mov	byte [DISK_STATUS1], 0 	; WIPE OUT DATA CORRECTED ERROR
TR_EX:	
	retn

;----------------------------------------
;	RECALIBRATE	     (AH = 11H) :
;----------------------------------------

HDISK_RECAL:
        mov	byte [CMD_BLOCK+6], RECAL_CMD ; 10h, 16
	call	COMMAND 		; START THE OPERATION
	jnz	short RECAL_EXIT	; ERROR
	call	_WAIT			; WAIT FOR COMPLETION
	jz	short RECAL_X 		; TIME OUT ONE OK ?
	call	_WAIT			; WAIT FOR COMPLETION LONGER
	jnz	short RECAL_EXIT	; TIME OUT TWO TIMES IS ERROR
RECAL_X:
	call	CHECK_STATUS
	cmp	byte [DISK_STATUS1], BAD_SEEK ; SEEK NOT COMPLETE
	jne	short RECAL_EXIT	; IS OK
	mov	byte [DISK_STATUS1], 0
RECAL_EXIT:
        cmp	byte [DISK_STATUS1], 0
	retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;--------------------------------------------------------
; COMMAND						:
;	THIS ROUTINE OUTPUTS THE COMMAND BLOCK		:
; OUTPUT						:
;	BL = STATUS					:
;	BH = ERROR REGISTER				:
;--------------------------------------------------------

COMMAND:
	;push	ebx ; 10/07/2022	; WAIT FOR SEEK COMPLETE AND READY
	;;mov	ecx, DELAY_2		; SET INITIAL DELAY BEFORE TEST
COMMAND1:
	;;push	ecx			; SAVE LOOP COUNT
	call	TST_RDY 		; CHECK DRIVE READY
	;;pop	ecx
	;pop	ebx ; 10/07/2022
	jz	short COMMAND2		; DRIVE IS READY
        cmp	byte [DISK_STATUS1], TIME_OUT ; TST_RDY TIMED OUT--GIVE UP
	;jz	short CMD_TIMEOUT
	;;loop	COMMAND1		; KEEP TRYING FOR A WHILE
	;jmp	short COMMAND4		; ITS NOT GOING TO GET READY
	jne	short COMMAND4
CMD_TIMEOUT:
	mov	byte [DISK_STATUS1], BAD_CNTLR
COMMAND4:
	;;pop	ebx ; 10/07/2022
        cmp	byte [DISK_STATUS1], 0	; SET CONDITION CODE FOR CALLER
	retn
COMMAND2:
	;;pop	ebx ; 10/07/2022
	;push	edi ; 10/07/2022
	mov	byte [HF_INT_FLAG], 0	; RESET INTERRUPT FLAG
	cli				; INHIBIT INTERRUPTS WHILE CHANGING MASK
	in	al, INTB01		; TURN ON SECOND INTERRUPT CHIP
	;and	al, 0BFh
	and	al, 3Fh			; Enable IRQ 14 & 15
	;JMP	$+2
	IODELAY
	out	INTB01, al
	in	al, INTA01		; LET INTERRUPTS PASS THRU TO
	and	al, 0FBh 		; SECOND CHIP
	;JMP	$+2
	IODELAY
	out	INTA01, al
	sti
	;xor	edi, edi		; INDEX THE COMMAND TABLE
	; 10/07/2022
	xor	ecx, ecx
	;mov	dx, HF_PORT+1		; DISK ADDRESS
	mov	dx, [HF_PORT]
	inc	dl
	test	byte [CONTROL_BYTE], 0C0h ; CHECK FOR RETRY SUPPRESSION
	jz	short COMMAND3
	mov	al, [CMD_BLOCK+6] 	; YES-GET OPERATION CODE
	and	al, 0F0h 		; GET RID OF MODIFIERS
	cmp	al, 20h			; 20H-40H IS READ, WRITE, VERIFY
	jb	short COMMAND3
	cmp	al, 40h
	ja	short COMMAND3
	or	byte [CMD_BLOCK+6], NO_RETRIES 
					; VALID OPERATION FOR RETRY SUPPRESS
COMMAND3:
	;mov	al, [CMD_BLOCK+edi]	; GET THE COMMAND STRING BYTE
	; 10/07/2022
	mov	al, [CMD_BLOCK+ecx]
	out	dx, al			; GIVE IT TO CONTROLLER
	IODELAY
	;inc	edi			; NEXT BYTE IN COMMAND BLOCK
	; 10/07/2022
	inc	ecx
	;inc	dx			; NEXT DISK ADAPTER REGISTER
	inc	edx   ; 10/07/2022	
	;cmp	di, 7 ; 01/01/2015	; ALL DONE?
	;jne	short COMMAND3		; NO--GO DO NEXT ONE
	cmp	cl, 7 ; 10/07/2022
	jb	short COMMAND3
	;pop	edi ; 10/07/2022
	retn				; ZERO FLAG IS SET

;CMD_TIMEOUT:
;	mov	byte [DISK_STATUS1], BAD_CNTLR
;COMMAND4:
;	pop	ebx
;	cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
;	retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	WAIT FOR INTERRUPT		:
;----------------------------------------
;WAIT:
_WAIT:
	sti				; MAKE SURE INTERRUPTS ARE ON
	;sub	cx, cx			; SET INITIAL DELAY BEFORE TEST
	;clc
	;mov	ax, 9000h		; DEVICE WAIT INTERRUPT
	;int	15h
	;jc	short WT2		; DEVICE TIMED OUT
	;mov	bl, DELAY_1		; SET DELAY COUNT

	;mov	bl, WAIT_HDU_INT_HI
	;; 21/02/2015
	;;mov	bl, WAIT_HDU_INT_HI + 1
	;;mov	cx, WAIT_HDU_INT_LO
	mov	ecx, WAIT_HDU_INT_LH
					; (AWARD BIOS -> WAIT_FOR_MEM)
;-----	WAIT LOOP

WT1:	
	;test	byte [HF_INT_FLAG], 80h	; TEST FOR INTERRUPT
	test 	byte [HF_INT_FLAG], 0C0h
	;loopz	WT1
	jnz	short WT3		; INTERRUPT--LETS GO
	;dec	bl
	;jnz	short WT1		; KEEP TRYING FOR A WHILE

WT1_hi:
	in	al, SYS1 ; 61h (PORT_B)	; wait for lo to hi
	test	al, 10h			; transition on memory
	jnz	short WT1_hi		; refresh.
WT1_lo:
	in	al, SYS1 		; 061h (PORT_B)	
	test	al, 10h			
	jz	short WT1_lo
	loop	WT1
	;;or	bl, bl
	;;jz	short WT2	
	;;dec	bl
	;;jmp	short WT1
	;dec	bl
	;jnz	short WT1	
WT2:	
	; 10/07/2022
	;mov	byte [DISK_STATUS1], TIME_OUT ; REPORT TIME OUT ERROR
	mov	al, TIME_OUT
	jmp	short WT4
WT3:
	;mov	byte [DISK_STATUS1], 0
	;mov	byte [HF_INT_FLAG], 0
	sub	al, al ; 0
	mov	byte [HF_INT_FLAG], al
WT4:
NB2:	
	mov	byte [DISK_STATUS1], al

	;cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
	and	al, al
	; zf = 0 -> time out, zf = 1 -> ok
	retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	WAIT FOR CONTROLLER NOT BUSY	:
;----------------------------------------
NOT_BUSY:
	sti				; MAKE SURE INTERRUPTS ARE ON
	;push	ebx
	;sub	cx, cx			; SET INITIAL DELAY BEFORE TEST
	mov	dx, [HF_PORT]
	add	dl, 7			; Status port (HF_PORT+7)
	;mov	bl, DELAY_1
					; wait for 10 seconds
	;mov 	cx, WAIT_HDU_INT_LO	; 1615h
	;;mov 	bl, WAIT_HDU_INT_HI	;   05h
	;mov	bl, WAIT_HDU_INT_HI + 1
	mov	ecx, WAIT_HDU_INT_LH  ; 21/02/2015
	;
	;;mov	byte [wait_count], 0    ; Reset wait counter
NB1:	
	in	al, dx			; CHECK STATUS
	;test	al, ST_BUSY
	and	al, ST_BUSY
	;loopnz NB1
	jz	short NB2 ; al = 0	; NOT BUSY--LETS GO
	;dec	bl			
	;jnz	short NB1		; KEEP TRYING FOR A WHILE

NB1_hi: 
	in	al, SYS1		; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short NB1_hi		; refresh.
NB1_lo: 
	in	al, SYS1
	test	al, 010h
	jz	short NB1_lo
	loop	NB1
	;dec	bl
	;jnz	short NB1
	;
	;;cmp	byte [wait_count], 182  ; 10 seconds (182 timer ticks)
	;;jb	short NB1
	;
	;mov	byte [DISK_STATUS1], TIME_OUT ; REPORT TIME OUT ERROR
	;jmp	short NB3
	mov	al, TIME_OUT
;NB2:	
	jmp	short NB2 ; 10/07/2022

;	;mov	byte [DISK_STATUS1], 0
;;NB3:	
;	;pop	ebx
;	mov	[DISK_STATUS1], al	;;; will be set after return
;	;cmp	byte [DISK_STATUS1], 0 	; SET CONDITION CODE FOR CALLER
;	or	al, al			; (zf = 0 --> timeout)
;	retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	WAIT FOR DATA REQUEST		:
;----------------------------------------
WAIT_DRQ:
	;mov	cx, DELAY_3
	;mov	dx, HF_PORT+7
	mov	dx, [HF_PORT]
	add	dl, 7
	;;mov	bl, WAIT_HDU_DRQ_HI	; 0
	;mov	cx, WAIT_HDU_DRQ_LO	; 1000 (30 milli seconds)
					; (but it is written as 2000
					; micro seconds in ATORGS.ASM file
					; of Award Bios - 1999, D1A0622)
	mov 	ecx, WAIT_HDU_DRQ_LH ; 21/02/2015 
WQ_1:
	in	al, dx			; GET STATUS
	test	al, ST_DRQ		; WAIT FOR DRQ
	jnz	short WQ_OK
	;loop	WQ_1			; KEEP TRYING FOR A SHORT WHILE
WQ_hi:	
	in	al, SYS1		; wait for hi to lo
	test	al, 010h		; transition on memory
	jnz	short WQ_hi		; refresh.
WQ_lo:  
	in	al, SYS1
	test	al, 010h
	jz	short WQ_lo
	loop	WQ_1

	mov	byte [DISK_STATUS1], TIME_OUT ; ERROR
	stc
WQ_OK:
	retn
;WQ_OK:
	;clc
	;retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	CHECK FIXED DISK STATUS BYTE	:
;----------------------------------------
CHECK_ST:
	;mov	dx, HF_PORT+7		; GET THE STATUS
	mov	dx, [HF_PORT]
	add	dl, 7
	in	al, dx
	mov	[HF_STATUS], al
	;mov	ah, 0
	sub	ah, ah ; 0
	test	al, ST_BUSY		; IF STILL BUSY
	jnz	short CKST_EXIT		; REPORT OK
	mov	ah, WRITE_FAULT
	test 	al, ST_WRT_FLT		; CHECK FOR WRITE FAULT
	jnz	short CKST_EXIT
	mov	ah, NOT_RDY
	test	al, ST_READY		; CHECK FOR NOT READY
	jz	short CKST_EXIT
	mov	ah, BAD_SEEK
	test	al, ST_SEEK_COMPL	; CHECK FOR SEEK NOT COMPLETE
	jz	short CKST_EXIT
	mov	ah, DATA_CORRECTED
	test	al, ST_CORRCTD		; CHECK FOR CORRECTED ECC
	jnz	short CKST_EXIT
	;mov	ah, 0
	xor	ah, ah ; 0
CKST_EXIT:
	mov	[DISK_STATUS1], ah	; SET ERROR FLAG
	cmp	ah, DATA_CORRECTED	; KEEP GOING WITH DATA CORRECTED
	je	short CKST_EX1
	;cmp	ah, 0
	and	ah, ah
CKST_EX1:
	retn

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

;----------------------------------------
;	CHECK FIXED DISK ERROR REGISTER :
;----------------------------------------
CHECK_ER:
	;mov	dx, HF_PORT+1		; GET THE ERROR REGISTER
	mov	dx, [HF_PORT]		;
	inc	dl
	in	al, dx
	; 10/07/2022
	;mov	[HF_ERROR], al
	;push	ebx	; 21/02/2015
	sub	ecx, ecx
	;mov	ecx, 8			; TEST ALL 8 BITS
	mov	cl, 8
CK1:	
	shl	al, 1			; MOVE NEXT ERROR BIT TO CARRY
	jc	short CK2		; FOUND THE ERROR
	loop	CK1			; KEEP TRYING
CK2:
	;mov	ebx, ERR_TBL		; COMPUTE ADDRESS OF
	;add	ebx, ecx		; ERROR CODE
	add	ecx, ERR_TBL ; 10/07/2022	

	;;;mov	ah, byte [cs:bx]	; GET ERROR CODE
	;;mov	ah, [bx]
	;mov	ah, [ebx] ; 21/02/2015
	mov	ah, [ecx]	
CKEX:
	mov	[DISK_STATUS1], ah	; SAVE ERROR CODE
	; 10/07/2022
	;pop	ebx
	;;cmp	ah, 0
	;and	ah, ah
	retn

;--------------------------------------------------------
; CHECK_DMA						:
;  -CHECK ES:BX AND # SECTORS TO MAKE SURE THAT IT WILL :
;   FIT WITHOUT SEGMENT OVERFLOW.			:
;  -ES:BX HAS BEEN REVISED TO THE FORMAT SSSS:000X	:
;  -OK IF # SECTORS < 80H (7FH IF LONG READ OR WRITE)	:
;  -OK IF # SECTORS = 80H (7FH) AND BX <= 00H (04H)	:
;  -ERROR OTHERWISE					:
;--------------------------------------------------------

	; 11/07/2022
	; (not needed for hard disks and 32 bit OS)
	;
	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;CHECK_DMA:
;	;;push	ax			; SAVE REGISTERS
;	;; 24/12/2021
;	;;push	eax
;	;mov	ax, 8000h		; AH = MAX # SECTORS
;					; AL = MAX OFFSET
;	; 10/07/2022
;	;test	byte [CMD_BLOCK+6], ECC_MODE
;	;jz	short CKD1
;	;mov	ax, 7F04h		; ECC IS 4 MORE BYTES
;CKD1:	
;	;cmp	ah, [CMD_BLOCK+1]	; NUMBER OF SECTORS
;	;ja	short CKDOK		; IT WILL FIT
;	;jb	short CKDERR		; TOO MANY
;	
;	cmp	byte [CMD_BLOCK+1], 80h
;	jb	short CKDOK
;	ja	short CKDERR
;	;cmp	al, bl			; CHECK OFFSET ON MAX SECTORS
;	;jb	short CKDERR		; ERROR
;CKD2:
;	or	bl, bl
;	jz	short CKDR
;	
;;CKDOK:	
;	;clc				; CLEAR CARRY
;	;;pop	ax
;	;; 24/12/2021
;	;pop	eax
;	;retn				; NORMAL RETURN
;CKDERR: 
;	stc				; INDICATE ERROR
;	mov	byte [DISK_STATUS1], DMA_BOUNDARY
;	;;pop	ax
;	;; 24/12/2021
;	;pop	eax	
;	retn
;
;	; 10/07/2022
;CKDOK:
;	clc
;CKDR:
;	retn

;----------------------------------------
;	SET UP EBX-> DISK PARMS	        :
;----------------------------------------
					
; INPUT -> DL = 0 based drive number
; OUTPUT -> EBX = disk parameter table address

	; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)

GET_VEC:
	;sub	ax, ax			; GET DISK PARAMETER ADDRESS
	;mov	es, ax
	;test	dl, 1
	;jz	short GV_0
;	les	bx, [HF1_TBL_VEC] 	; ES:BX -> DRIVE PARAMETERS
;	jmp	short GV_EXIT
;GV_0:
;	les 	bx,[HF_TBL_VEC]		; ES:BX -> DRIVE PARAMETERS
;
	xor	ebx, ebx
	mov	bl, dl
	;02/01/2015
	;xor	bh, bh
	;shl	bl, 1			; port address offset
	;mov	ax, [bx+hd_ports]	; Base port address (1F0h, 170h)
	;shl	bl, 1			; dpt pointer offset
	shl	bl, 2
	;add	bx, HF_TBL_VEC		; Disk parameter table pointer
	add	ebx, HF_TBL_VEC ; 21/02/2015
	;push	word [bx+2]		; dpt segment
	;pop	es
	;mov	bx, [bx]		; dpt offset
	mov	ebx, [ebx]		
;GV_EXIT:
	retn

	; 24/12/2021 - Retro UNIX 386 v1.1
hdc1_int: ; 21/02/2015
;--- HARDWARE INT 76H -- ( IRQ LEVEL  14 ) ----------------------
;								:
;	FIXED DISK INTERRUPT ROUTINE				:
;								:
;----------------------------------------------------------------

; 22/12/2014
; IBM PC-XT Model 286 System BIOS Source Code - DISK.ASM (HD_INT)
;	 '11/15/85'
; AWARD BIOS 1999 (D1A0622) 
;	Source Code - ATORGS.ASM (INT_HDISK, INT_HDISK1)

;int_76h:
HD_INT:
	;push	ax
	; 24/12/2021
	push	eax
	push	ds
	;CALL	DDS
	; 21/02/2015 (32 bit, 386 pm modification)
	mov	ax, KDATA
	mov 	ds, ax
	;
	;;MOV	@HF_INT_FLAG, 0FFH	; ALL DONE
        ;mov     byte [CS:HF_INT_FLAG], 0FFh
	mov	byte [HF_INT_FLAG], 0FFh
	;
	;push	dx
	; 24/12/2021
	push	edx
	mov	dx, HDC1_BASEPORT+7	; Status Register (1F7h)
					; Clear Controller
Clear_IRQ1415:				; (Award BIOS - 1999)
	in	al, dx			;
	;pop	dx
	; 24/12/2021
	pop	edx
	NEWIODELAY
	;
	mov	al, EOI			; NON-SPECIFIC END OF INTERRUPT
	out	INTB00, al		; FOR CONTROLLER #2
	;JMP	$+2			; WAIT
	NEWIODELAY
	out	INTA00, al		; FOR CONTROLLER #1
	pop	ds
	;sti				; RE-ENABLE INTERRUPTS
	;mov	ax, 9100h		; DEVICE POST
	;int	15h			; INTERRUPT
irq15_iret: ; 25/02/2015
	;pop	ax
	; 24/12/2021
	pop	eax
	iretd				; RETURN FROM INTERRUPT

	; 24/12/2021 - Retro UNIX 386 v1.1
hdc2_int: ; 21/02/2015
;--- HARDWARE INT 77H ++ ( IRQ LEVEL  15 ) ----------------------
;								:
;	FIXED DISK INTERRUPT ROUTINE				:
;								:
;----------------------------------------------------------------

;int_77h:
HD1_INT:
	;push	ax
	; 24/12/2021
	push	eax
	; Check if that is a spurious IRQ (from slave PIC)
	; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
	mov	al, 0Bh  ; In-Service Register
	out	0A0h, al
        jmp short $+2
	jmp short $+2
	in	al, 0A0h
	and 	al, 80h ; bit 7 (is it real IRQ 15 or fake?)
	jz	short irq15_iret ; Fake (spurious)IRQ, do not send EOI)
	;
	push	ds
	;CALL	DDS
	; 21/02/2015 (32 bit, 386 pm modification)
	mov	ax, KDATA
	mov 	ds, ax
	;
	;;MOV	@HF_INT_FLAG,0FFH	; ALL DONE
        ;or	byte [CS:HF_INT_FLAG], 0C0h 
	or	byte [HF_INT_FLAG], 0C0h
	;
	;push	dx
	; 24/12/2021
	push	edx
	mov	dx, HDC2_BASEPORT+7	; Status Register (177h)
					; Clear Controller (Award BIOS 1999)
	jmp	short Clear_IRQ1415

;%include 'diskdata.inc' ; 11/03/2015
;%include 'diskbss.inc' ; 11/03/2015

;////////////////////////////////////////////////////////////////////
;; END OF DISK I/O SYTEM ///
