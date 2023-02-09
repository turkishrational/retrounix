; Retro UNIX 386 v1.2 Kernel (v0.2.2.3) - DISKBSS.INC
; Last Modification: 12/07/2022 
; *****************************************************************************
;	(Initialized Disk Parameters Data section for 'DISKIO.INC') 
; *****************************************************************************
; Ref: Retro UNIX 386 v1 Kernel (v0.2.1.5) - DISKDATA.INC - 11/07/2022

;----------------------------------------
;	80286 INTERRUPT LOCATIONS	:
;	REFERENCED BY POST & BIOS	:
;----------------------------------------

DISK_POINTER:	dd	MD_TBL6		; Pointer to Diskette Parameter Table

; IBM PC-XT Model 286 source code ORGS.ASM (06/10/85) - 14/12/2014
;----------------------------------------------------------------
; DISK_BASE							:
;	THIS IS THE SET OF PARAMETERS REQUIRED FOR		:
;	DISKETTE OPERATION. THEY ARE POINTED AT BY THE		:
;	DATA VARIABLE @DISK_POINTER. TO MODIFY THE PARAMETERS,	:
;	BUILD ANOTHER PARAMETER BLOCK AND POINT AT IT		:
;----------------------------------------------------------------

;DISK_BASE:	
;	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
;	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
;	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
;	DB	2		; 512 BYTES/SECTOR
;	;DB	15		; EOT (LAST SECTOR ON TRACK)
;	db	18		; (EOT for 1.44MB diskette)
;	DB	01BH		; GAP LENGTH
;	DB	0FFH		; DTL
;	;DB	054H		; GAP LENGTH FOR FORMAT
;	db	06ch		; (for 1.44MB dsikette)
;	DB	0F6H		; FILL BYTE FOR FORMAT
;	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
;	DB	8		; MOTOR START TIME (1/8 SECONDS)

;----------------------------------------
;	ROM BIOS DATA AREAS		:
;----------------------------------------

;DATA		SEGMENT AT 40H		; ADDRESS= 0040:0000

;@EQUIP_FLAG	DW	?		; INSTALLED HARDWARE FLAGS

;----------------------------------------
;	DISKETTE DATA AREAS		:
;----------------------------------------

;@SEEK_STATUS	DB	?		; DRIVE RECALIBRATION STATUS
;					; BIT 3-0 = DRIVE 3-0 RECALIBRATION
;					; BEFORE NEXT SEEK IF BIT IS = 0
;@MOTOR_STATUS	DB	?		; MOTOR STATUS
;					; BIT 3-0 = DRIVE 3-0 CURRENTLY RUNNING
;					; BIT 7 = CURRENT OPERATION IS A WRITE
;@MOTOR_COUNT	DB	?		; TIME OUT COUNTER FOR MOTOR(S) TURN OFF
;@DSKETTE_STATUS DB	?		; RETURN CODE STATUS BYTE
;					; CMD_BLOCK  IN STACK FOR DISK OPERATION
;@NEC_STATUS	DB	7 DUP(?)	; STATUS BYTES FROM DISKETTE OPERATION

;----------------------------------------
;	POST AND BIOS WORK DATA AREA	:
;----------------------------------------

;@INTR_FLAG	DB	?		; FLAG INDICATING AN INTERRUPT HAPPENED

;----------------------------------------
;	TIMER DATA AREA 		:
;----------------------------------------

; 17/12/2014  (IRQ 0 - INT 08H)
;TIMER_LOW	equ	46Ch		; Timer ticks (counter)  @ 40h:006Ch
;TIMER_HIGH	equ	46Eh		; (18.2 timer ticks per second)
;TIMER_OFL	equ	470h		; Timer - 24 hours flag  @ 40h:0070h

;----------------------------------------
;	ADDITIONAL MEDIA DATA		:
;----------------------------------------

;@LASTRATE	DB	?		; LAST DISKETTE DATA RATE SELECTED
;@DSK_STATE	DB	?		; DRIVE 0 MEDIA STATE
;		DB	?		; DRIVE 1 MEDIA STATE
;		DB	?		; DRIVE 0 OPERATION START STATE
;		DB	?		; DRIVE 1 OPERATION START STATE
;@DSK_TRK	DB	?		; DRIVE 0 PRESENT CYLINDER
;		DB	?		; DRIVE 1 PRESENT CYLINDER

;DATA		ENDS			; END OF BIOS DATA SEGMENT

;--------------------------------------------------------
;	DRIVE TYPE TABLE				:
;--------------------------------------------------------
		; 16/02/2015 (unix386.s, 32 bit modifications)
DR_TYPE:
		DB	01		;DRIVE TYPE, MEDIA TABLE
                ;DW	MD_TBL1
		dd	MD_TBL1
		DB	02+BIT7ON
		;DW	MD_TBL2
                dd      MD_TBL2
DR_DEFAULT:	DB	02
                ;DW	MD_TBL3
		dd      MD_TBL3
		DB	03
                ;DW	MD_TBL4
		dd      MD_TBL4
		DB	04+BIT7ON
                ;DW	MD_TBL5
		dd      MD_TBL5
		DB	04
                ;DW	MD_TBL6
		dd      MD_TBL6
DR_TYPE_E       equ $                   ; END OF TABLE
;DR_CNT		EQU	(DR_TYPE_E-DR_TYPE)/3
DR_CNT		equ	(DR_TYPE_E-DR_TYPE)/5
;--------------------------------------------------------
;	MEDIA/DRIVE PARAMETER TABLES			:
;--------------------------------------------------------
;--------------------------------------------------------
;	360 KB MEDIA IN 360 KB DRIVE			:
;--------------------------------------------------------
MD_TBL1:        
	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	09		; EOT (LAST SECTOR ON TRACK)
	DB	02AH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	050H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	39		; MAX. TRACK NUMBER
	DB	RATE_250	; DATA TRANSFER RATE
;--------------------------------------------------------
;	360 KB MEDIA IN 1.2 MB DRIVE			:
;--------------------------------------------------------
MD_TBL2:        
	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	09		; EOT (LAST SECTOR ON TRACK)
	DB	02AH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	050H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	39		; MAX. TRACK NUMBER
	DB	RATE_300	; DATA TRANSFER RATE
;--------------------------------------------------------
;	1.2 MB MEDIA IN 1.2 MB DRIVE			:
;--------------------------------------------------------
MD_TBL3:
	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	15		; EOT (LAST SECTOR ON TRACK)
	DB	01BH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	054H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	79		; MAX. TRACK NUMBER
	DB	RATE_500	; DATA TRANSFER RATE
;--------------------------------------------------------
;	720 KB MEDIA IN 720 KB DRIVE			:
;--------------------------------------------------------
MD_TBL4:
	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	09		; EOT (LAST SECTOR ON TRACK)
	DB	02AH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	050H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	79		; MAX. TRACK NUMBER
	DB	RATE_250	; DATA TRANSFER RATE
;--------------------------------------------------------
;	720 KB MEDIA IN 1.44 MB DRIVE			:
;--------------------------------------------------------
MD_TBL5:
	DB	11011111B	; SRT=D, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	09		; EOT (LAST SECTOR ON TRACK)
	DB	02AH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	050H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	79		; MAX. TRACK NUMBER
	DB	RATE_250	; DATA TRANSFER RATE
;--------------------------------------------------------
;	1.44 MB MEDIA IN 1.44 MB DRIVE			:
;--------------------------------------------------------
MD_TBL6:
	DB	10101111B	; SRT=A, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT TIME AFTER OPERATION TILL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	18		; EOT (LAST SECTOR ON TRACK)
	DB	01BH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	06CH		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	15		; HEAD SETTLE TIME (MILLISECONDS)
	DB	8		; MOTOR START TIME (1/8 SECONDS)
	DB	79		; MAX. TRACK NUMBER
	DB	RATE_500	; DATA TRANSFER RATE


; << diskette.inc >>
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;----------------------------------------
;	ROM BIOS DATA AREAS		:
;----------------------------------------

;DATA		SEGMENT AT 40H		; ADDRESS= 0040:0000

;----------------------------------------
;	FIXED DISK DATA AREAS		:
;----------------------------------------

;DISK_STATUS1:	DB	0		; FIXED DISK STATUS
;HF_NUM:		DB	0		; COUNT OF FIXED DISK DRIVES
;CONTROL_BYTE:	DB	0		; HEAD CONTROL BYTE
;@PORT_OFF	DB	?		;  RESERVED (PORT OFFSET)

;----------------------------------------
;	ADDITIONAL MEDIA DATA		:
;----------------------------------------

;@LASTRATE	DB	?		; LAST DISKETTE DATA RATE SELECTED
;HF_STATUS	DB	0		; STATUS REGISTER
;HF_ERROR	DB	0		; ERROR REGISTER
;HF_INT_FLAG	DB	0		; FIXED DISK INTERRUPT FLAG
;HF_CNTRL	DB	0		; COMBO FIXED DISK/DISKETTE CARD BIT 0=1
;@DSK_STATE	DB	?		; DRIVE 0 MEDIA STATE
;		DB	?		; DRIVE 1 MEDIA STATE
;		DB	?		; DRIVE 0 OPERATION START STATE
;		DB	?		; DRIVE 1 OPERATION START STATE
;@DSK_TRK	DB	?		; DRIVE 0 PRESENT CYLINDER
;		DB	?		; DRIVE 1 PRESENT CYLINDER

;DATA		ENDS			; END OF BIOS DATA SEGMENT
;
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ERR_TBL:
	db	NO_ERR
	db	BAD_ADDR_MARK,BAD_SEEK,BAD_CMD,UNDEF_ERR
	db	RECORD_NOT_FND,UNDEF_ERR,BAD_ECC,BAD_SECTOR

; 11/07/2022
; 17/12/2014 (mov ax, [cfd])
; 11/12/2014
;cfd:		db 0			; current floppy drive (for GET_PARM)
; 17/12/2014				; instead of 'DISK_POINTER'
;pfd:		db 1			; previous floppy drive (for GET_PARM)
					; (initial value of 'pfd 
					; must be different then 'cfd' value
					; to force updating/initializing
					; current drive parameters) 

;; 11/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
pfd:		db 0FFh

align 2

HF_PORT:	dw 	1F0h  ; Default = 1F0h
			      ; (170h)
HF_REG_PORT:	dw	3F6h  ; HF_PORT + 206h

; 05/01/2015 
hf_m_s:         db      0     ; (0 = Master, 1 = Slave)

; *****************************************************************************
