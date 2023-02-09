; Retro UNIX 386 v1 Kernel (v0.2.1.5) - DISKBSS.INC
; Last Modification: 10/07/2022
;	(Uninitialized Disk Parameters Data section for 'DISKIO.INC') 
;
; *****************************************************************************

alignb 2

;----------------------------------------
;	TIMER DATA AREA 		:
;----------------------------------------

TIMER_LH:	; 16/02/205
TIMER_LOW:      resw	1               ; LOW WORD OF TIMER COUNT
TIMER_HIGH:     resw	1               ; HIGH WORD OF TIMER COUNT
TIMER_OFL:      resb 	1               ; TIMER HAS ROLLED OVER SINCE LAST READ

;----------------------------------------
;	DISKETTE DATA AREAS		:
;----------------------------------------

SEEK_STATUS:	resb	1
MOTOR_STATUS:	resb	1
MOTOR_COUNT:	resb	1
DSKETTE_STATUS:	resb	1
NEC_STATUS:	resb	7

;----------------------------------------
;	ADDITIONAL MEDIA DATA		:
;----------------------------------------

LASTRATE:	resb 	1
HF_STATUS:	resb 	1
;HF_ERROR:	resb 	1  ; 10/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)	
HF_INT_FLAG:	resb	1
;HF_CNTRL:	resb 	1  ; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;DSK_STATE:	resb 	4
DSK_STATE:	resb 	2  ; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
DSK_TRK:	resb 	2

;----------------------------------------
;	FIXED DISK DATA AREAS		:
;----------------------------------------

DISK_STATUS1:	resb 	1		; FIXED DISK STATUS
HF_NUM:		resb 	1		; COUNT OF FIXED DISK DRIVES
CONTROL_BYTE:	resb 	1		; HEAD CONTROL BYTE
;@PORT_OFF	resb	1		; RESERVED (PORT OFFSET)
;port1_off	resb	1		; Hard disk controller 1 - port offset
;port2_off	resb	1		; Hard disk controller 2 - port offset

alignb 4

;HF_TBL_VEC:	resd	1 		; Primary master disk param. tbl. pointer
;HF1_TBL_VEC:	resd	1		; Primary slave disk param. tbl. pointer
HF_TBL_VEC: ; 22/12/2014	
HDPM_TBL_VEC:	resd	1 		; Primary master disk param. tbl. pointer
HDPS_TBL_VEC:	resd	1		; Primary slave disk param. tbl. pointer
HDSM_TBL_VEC:	resd	1 		; Secondary master disk param. tbl. pointer
HDSS_TBL_VEC:	resd	1		; Secondary slave disk param. tbl. pointer

; 03/01/2015
LBAMode:     	resb	1

; *****************************************************************************
