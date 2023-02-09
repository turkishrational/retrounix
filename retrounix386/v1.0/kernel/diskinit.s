; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel - DISKINIT.INC
; Last Modification: 02/01/2022

; DISK I/O SYSTEM INITIALIZATION - Erdogan Tan (Retro UNIX 386 v1 project)

; ///////// DISK I/O SYSTEM STRUCTURE INITIALIZATION ///////////////

	; 02/01/2022 (Retro UNIX 386 v1.2)
	; 10/12/2014 - 02/02/2015 - dsectrm2.s
;L0:
	; 12/11/2014 (Retro UNIX 386 v1 - beginning)
	; Detecting disk drives... (by help of ROM-BIOS)
	mov	dx, 7Fh
L1:	
	inc	dl
	mov	ah, 41h ; Check extensions present
			; Phoenix EDD v1.1 - EDD v3
	mov	bx, 55AAh
	int 	13h
	jc	short L2

	cmp	bx, 0AA55h
	jne	short L2
	inc	byte [hdc]	; count of hard disks (EDD present)
        mov     [last_drv], dl  ; last hard disk number
	mov	bx, hd0_type - 80h
	add	bx, dx	 
	mov	[bx], cl ; Interface support bit map in CX
			 ; Bit 0 - 1, Fixed disk access subset ready
			 ; Bit 1 - 1, Drv locking and ejecting ready
			 ; Bit 2 - 1, Enhanced Disk Drive Support
                         ;            (EDD) ready (DPTE ready)
			 ; Bit 3 - 1, 64bit extensions are present
                         ;            (EDD-3)
			 ; Bit 4 to 15 - 0, Reserved
	cmp	dl, 83h	 ; drive number < 83h
	jb	short L1
L2:
	; 23/11/2014
	; 19/11/2014
	xor	dl, dl  ; 0
	; 04/02/2016 (esi -> si)
	mov	si, fd0_type
L3:
	; 14/01/2015
	mov	[drv], dl
	;
	mov 	ah, 08h ; Return drive parameters
	int	13h	
	jc	short L4
		; BL = drive type (for floppy drives)
		; DL = number of floppy drives
		;		
		; ES:DI = Address of DPT from BIOS
		;
	mov	[si], bl ;  Drive type
			; 4 = 1.44 MB, 80 track, 3 1/2"
	; 14/01/2015
	call	set_disk_parms
	; 10/12/2014
	cmp	si, fd0_type
	ja	short L4
	inc	si ; fd1_type
	mov	dl, 1
	jmp	short L3
L4:
	; Older BIOS (INT 13h, AH = 48h is not available)
	mov	dl, 7Fh
	; 24/12/2014 (Temporary)
	cmp	byte [hdc], 0 ; EDD present or not ?	
	;ja	L10       ; yes, all fixed disk operations
			  ; will be performed according to
			  ; present EDD specification
	; 02/01/2022
	jna	short L5
	jmp	L10
L5:
	inc 	dl
        mov     [drv], dl
        mov     [last_drv], dl ; 14/01/2015
	mov 	ah, 08h ; Return drive parameters
	int	13h	; (conventional function)
	;jc	L13	; fixed disk drive not ready
	; 02/01/2022
	jnc	short L6
	jmp	L13
L6:
        mov     [hdc], dl ; number of drives
	;; 14/01/2013
	;;push	cx
	call	set_disk_parms
	;;pop	cx
	;
	;;and	cl, 3Fh	 ; sectors per track (bits 0-6)
        mov     dl, [drv]
	mov	bx, 65*4 ; hd0 parameters table (INT 41h)	
	cmp	dl, 80h
	jna	short L7
	add	bx, 5*4	 ; hd1 parameters table (INT 46h)
L7:	
	xor	ax, ax
	mov	ds, ax
        mov     si, [bx]
        mov     ax, [bx+2] 
	mov	ds, ax
        cmp     cl, [si+FDPT_SPT] ; sectors per track 
	;jne	L12 ; invalid FDPT
	; 02/01/2022
	je	short L7_8
	jmp	L12
L7_8:
	mov	di, HD0_DPT
	cmp	dl, 80h
	jna	short L8
	mov	di, HD1_DPT 
L8:
	; 30/12/2014
	mov	ax, DPT_SEGM
	mov	es, ax
	; 24/12/2014
	mov	cx, 8
	rep	movsw  ; copy 16 bytes to the kernel's DPT location
	mov	ax, cs
	mov	ds, ax
	; 02/02/2015
        mov     cl, [drv]
	mov	bl, cl
	mov	ax, 1F0h
	and	bl, 1
	jz	short L9
	shl	bl, 4
	sub	ax, 1F0h-170h
L9:
	stosw	; I/O PORT Base Address (1F0h, 170h)
	add	ax, 206h
	stosw	; CONTROL PORT Address (3F6h, 376h)	
	mov	al, bl
	add	al, 0A0h
	stosb	; Device/Head Register upper nibble
	;
	inc	byte [drv]
	mov	bx, hd0_type - 80h
	add	bx, cx
        or      byte [bx], 80h  ; present sign (when lower nibble is 0)
	mov	al, [hdc]
	dec	al
	;jz	L13
	; 02/01/2022
	jz	short L9_10
	cmp	dl, 80h
        ;jna	L5
	;jmp	L13
	; 02/01/2022
	ja	short L9_10
	jmp	L5
L9_10:
        jmp     L13
L10:
	inc 	dl
	; 25/12/2014
	mov	[drv], dl
	mov 	ah, 08h ; Return drive parameters
	int	13h	; (conventional function)
	;jc	L13
	; 02/01/2022
	jc	short L9_10
	; 14/01/2015
	mov	dl, [drv]
	push	dx
	push	cx
	call	set_disk_parms
	pop	cx
	pop	dx
	; 02/01/2022 - Retro UNIX 386 v1.2
	; 04/02/2016 (esi -> si)
	;mov	si, _end ; 30 byte temporary buffer address
	;		 ; at the '_end' of kernel.
	;mov	word [si], 30
	; 06/07/2016
	mov	si, _int13h_48h_buffer
	; 09/07/2016
	mov	ax, 001Eh
	mov	[si], ah ; 0
	inc	si
	mov	word [si], ax
 	; word [si] = 30
	;
	mov	ah, 48h	 ; Get drive parameters (EDD function)
	int	13h
        ;jc	L13
	; 02/01/2022
	jc	short L9_10
	; 04/02/2016 (ebx -> bx)
	; 14/01/2015
	;sub	bx, bx
	sub	bh, bh ; 02/01/2022
	mov	bl, dl
	sub	bl, 80h
	add	bx, hd0_type
	mov 	al, [bx]
	or	al, 80h
	mov 	[bx], al	
	sub	bx, hd0_type - 2 ; 15/01/2015
	add	bx, drv.status
	mov	[bx], al
	; 04/02/2016 (eax -> ax)
	mov	ax, [si+16]
	test	ax, [si+18]
	jz	short L10_A0h 
			; 'CHS only' disks on EDD system 
			;  are reported with ZERO disk size
	sub	bx, drv.status
	shl	bx, 2
	add	bx, drv.size ; disk size (in sectors)
	mov	[bx], ax
	mov	ax, [si+18]
	;mov	[bx], ax
	; 02/01/2022 (BugFix)
	mov	[bx+2], ax

L10_A0h: ; Jump here to fix a ZERO (LBA) disk size problem 
	 ; for CHS disks (28/02/2015)
	; 30/12/2014
	mov	di, HD0_DPT
	mov	al, dl
	and 	ax, 3
	shl	al, 5 ; *32
	add 	di, ax
	mov	ax, DPT_SEGM
	mov	es, ax
	;
	mov	al, ch	; max. cylinder number (bits 0-7)
	mov	ah, cl	
	shr	ah, 6	; max. cylinder number (bits 8-9)
 	inc	ax	; logical cylinders (limit 1024)
	stosw		
	mov	al, dh	; max. head number
	inc	al
	stosb		; logical heads (limits 256)
	mov	al, 0A0h ; Indicates translated table
	stosb
	mov	al, [si+12]
	stosb		 ; physical sectors per track
 	xor	ax, ax
	;dec	ax	 ; 02/01/2015 
	stosw		 ; precompensation (obsolete)
	;xor	al, al	 ; 02/01/2015	
	stosb		 ; reserved
	mov	al, 8	 ; drive control byte
		         ; (do not disable retries, 
			 ; more than 8 heads)
	stosb
	mov	ax, [si+4]
	stosw		 ; physical number of cylinders	
	;push	ax	 ; 02/01/2015
	mov	al, [si+8]
	stosb		 ; physical num. of heads (limit 16)
	sub 	ax, ax
	;pop	ax	 ; 02/01/2015	
	stosw		 ; landing zone (obsolete)
	mov	al, cl	 ; logical sectors per track (limit 63)
	and 	al, 3Fh	
	stosb
	;sub	al, al	 ; checksum
	;stosb
	;
	add	si, 26   ; (BIOS) DPTE address pointer
	lodsw
	push	ax	 ; (BIOS) DPTE offset
	lodsw
	push	ax	 ; (BIOS) DPTE segment
	;
	; checksum calculation
	mov	si, di
	push	es
	pop	ds
	;mov	cx, 16
	mov 	cx, 15
	sub	si, cx
	xor	ah, ah
	;del	cl
L11:		
	lodsb
	add	ah, al
	loop	L11
	;
	mov	al, ah
	neg	al	; -x+x = 0
	stosb		; put checksum in byte 15 of the tbl
	;
	pop	ds	; (BIOS) DPTE segment
	pop	si	; (BIOS) DPTE offset	
	;
	; 23/02/2015
	push	di
	; ES:DI points to DPTE (FDPTE) location
	;mov	cx, 8
	mov	cl, 8
	rep	movsw	
	;
	; 23/02/2015
	; (P)ATA drive and LBA validation
	; (invalidating SATA drives and setting
	; CHS type I/O for old type fixed disks)
	pop	bx
	mov	ax, cs
	mov	ds, ax
	mov	ax, [es:bx]
	cmp	ax, 1F0h
	je	short L11a
	cmp	ax, 170h
	je	short L11a
	; invalidation 
	; (because base port address is not 1F0h or 170h)
	xor	bh, bh
	mov	bl, dl
	sub	bl, 80h
	mov	byte [bx+hd0_type], 0 ; not a valid disk drive !		
        or      byte [bx+drv.status+2], 0F0h ; (failure sign)
	jmp	short L11b
L11a:	
	; LBA validation
	mov	al, [es:bx+4] ; Head register upper nibble
	test	al, 40h ; LBA bit (bit 6)
	jnz	short L11b ; LBA type I/O is OK! (E0h or F0h)
	; force CHS type I/O for this drive (A0h or B0h)
	sub	bh, bh
	mov	bl, dl
	sub	bl, 80h ; 26/02/2015
        and     byte [bx+drv.status+2], 0FEh ; clear bit 0
				; bit 0 = LBA ready bit
	; 'diskio' procedure will check this bit !
L11b:
	cmp	dl, [last_drv] ; 25/12/2014
        jnb     short L13
        jmp     L10
L12:
	; Restore data registers
	mov	ax, cs
	mov	ds, ax	
L13:
	; 13/12/2014
	push	cs
	pop	es
L14:
	mov 	ah, 11h
	int 	16h
	;jz 	short L15 ; no keys in keyboard buffer
	; 02/01/2022
	jz	short L16
	mov	al, 10h
	int 	16h
	jmp 	short L14
L15:

; //////

; 02/01/2022 - Retro UNIX 386 v1.2
%if 0
	; 24/11/2014
	; 19/11/2014
	; 14/11/2014
	; Temporary code for disk searching code check
	;
	; This code will show existing (usable) drives and also
	; will show EDD interface support status for hard disks		
	; (If status bit 7 is 1, Identify Device info is ready,
	; no need to get it again in protected mode...) 
	;	
	; 13/11/2014
	mov	bx, 7
	mov	ah, 0Eh
	mov	al, [fd0_type]
	and	al, al
	jz	short L15a
	mov	dl, al
	mov	al, 'F'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '0'
	int 	10h
	mov	al, ' '
	int	10h
	call	L15c
	mov	al, ' '
	int	10h
	;
	mov	al, [fd1_type]
	and	al, al
	jz	short L15a
	mov	dl, al
	mov	al, 'F'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '1'
	int 	10h
	mov	al, ' '
	int	10h
	call	L15c
	mov	al, ' '
	int	10h
	mov	al, ' '
	int	10h
L15a:
	mov	al, [hd0_type]
	and	al, al
	jz	short L15b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '0'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L15c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd1_type]
	and	al, al
	jz	short L15b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '1'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L15c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd2_type]
	and	al, al
	jz	short L15b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '2'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L15c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd3_type]
	and	al, al
	jz	short L15b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '3'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L15c
	mov	al, ' '
	int	10h
	;
L15b:
	mov	al, 0Dh
	int 	10h	
	mov	al, 0Ah
	int 	10h
	;;xor	ah, ah
	;;int 	16h	
	;
        ;jmp	L16  ; jmp short L16
        ; 02/01/2022
	jmp	short L16
	;
L15c:
	mov	dh, dl
	shr	dh, 4
	add	dh, 30h
	and	dl, 15
	add	dl, 30h
	mov	al, dh
	int	10h
	mov	al, dl
	int	10h
	retn
	;
	; end of temporary code for disk searching code check

%endif

; //////

set_disk_parms:
	; 04/02/2016 (ebx -> bx)
	; 10/07/2015
	; 14/01/2015
	;push	bx
	sub	bh, bh
	mov	bl, [drv]
	cmp	bl, 80h
	jb	short sdp0
	sub	bl, 7Eh
sdp0:	
	add	bx, drv.status
  	mov	byte [bx], 80h ; 'Present' flag
	;
	mov	al, ch ; last cylinder (bits 0-7)
	mov	ah, cl ; 
	shr	ah, 6  ; last cylinder (bits 8-9)
	sub	bx, drv.status
	shl	bl, 1
	add	bx, drv.cylinders
	inc	ax  ; convert max. cyl number to cyl count		
	mov	[bx], ax
	push	ax ; ** cylinders
	sub	bx, drv.cylinders
	add	bx, drv.heads
	xor	ah, ah
	mov	al, dh ; heads
	inc	ax
	mov	[bx], ax
        sub     bx, drv.heads
        add     bx, drv.spt
	xor	ch, ch
	and	cl, 3Fh	; sectors (bits 0-6)
	mov	[bx], cx
        sub     bx, drv.spt
	shl	bx, 1
	add	bx, drv.size ; disk size (in sectors)
	; LBA size = cylinders * heads * secpertrack
	mul	cx 
	mov	dx, ax	; heads*spt					
	pop	ax ; ** cylinders
	dec	ax ; 1 cylinder reserved (!?)
	mul	dx ; cylinders * (heads*spt)		
	mov	[bx], ax
	mov	[bx+2], dx
	;
	;pop	bx
	retn

;align 2

;cylinders :  dw 0, 0, 0, 0, 0, 0
;heads	  :  dw 0, 0, 0, 0, 0, 0
;spt	  :  dw 0, 0, 0, 0, 0, 0
;disk_size :  dd 0, 0, 0, 0, 0, 0

;last_drv:
;	db  0
;drv_status:
;	db  0,0,0,0,0,0
;	db 0

; End Of DISK I/O SYSTEM STRUCTURE INITIALIZATION /// 06/02/2015

L16: