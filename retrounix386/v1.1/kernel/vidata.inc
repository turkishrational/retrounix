; Retro UNIX 386 v1 Kernel - VIDATA.INC
; Last Modification: 11/03/2015
;		    (Data section for 'VIDEO.INC')	
;
; ///////// VIDEO DATA ///////////////

video_params:
	; 02/09/2014 (Retro UNIX 386 v1)
	;ORGS.ASM ----- 06/10/85   COMPATIBILITY MODULE
	; VIDEO MODE 3
	db	71h,50h,5Ah,0Ah,1Fh,6,19h	; SET UP FOR 80X25
	db	1Ch,2,7,6,7	; cursor start = 6, cursor stop = 7
	db	0,0,0,0

; /// End Of VIDEO DATA ///