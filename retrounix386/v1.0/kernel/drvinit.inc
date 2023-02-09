; DISK I/O SYSTEM INITIALIZATION - Erdogan Tan (Retro UNIX 386 v1 project)

; ///////// DISK I/O SYSTEM STRUCTURE INITIALIZATION ///////////////

	; 02/02/2015
	; 15/01/2015
	; 14/01/2015
	; 05/01/2015 
	; 02/01/2015
	; 30/12/2014
	; 25/12/2014 (Translated HDPT)
	; 23/12/2014
	; 20/12/2014 (Fixed Disk ports)
	; 19/12/2014
	; 13/12/2014
	; 10/12/2014
	; (unix386.s)
L0:
	; 24/11/2014
	; 23/11/2014
	; 19/11/2014, 13/11/2014
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
	mov	esi, fd0_type
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
	mov	[esi], bl ;  Drive type
			; 4 = 1.44 MB, 80 track, 3 1/2"
	; 14/01/2015
	call	set_disk_parms
	; 10/12/2014
	cmp	esi, fd0_type
	ja	short L4
	inc	esi ; fd1_type
	mov	dl, 1
	jmp	short L3
L4:
	; Older BIOS (INT 13h, AH = 48h is not available)
	mov	dl, 7Fh
	; 24/12/2014 (Temporary)
	cmp	byte [hdc], 0 ; EDD present or not ?	
        ja      L10       ; yes, all fixed disk operations
			  ; will be performed according to
			  ; present EDD specification
L6:
	inc 	dl
        mov     [drv], dl
        mov     [last_drv], dl ; 14/01/2015
	mov 	ah, 08h ; Return drive parameters
	int	13h	; (conventional function)
        jc      L13	; fixed disk drive not ready
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
        jne     L12 ; invalid FDPT
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
        jz      L13
	cmp	dl, 80h
        jna     L6
        jmp     L13
L10:
	inc 	dl
	; 25/12/2014
	mov	[drv], dl
	mov 	ah, 08h ; Return drive parameters
	int	13h	; (conventional function)
        jc      L13
	; 14/01/2015
	mov	dl, [drv]
	push	dx
	push	cx
	call	set_disk_parms
	pop	cx
	pop	dx
	;
	mov	esi, _end ; 30 byte temporary buffer address 	
			 ; at the '_end' of kernel.
	mov	word [esi], 30
	mov	ah, 48h	 ; Get drive parameters (EDD function)
	int	13h
        jc      L13
	; 14/01/2015
	sub	ebx, ebx
	mov	bl, dl
	sub	bl, 80h
	add	ebx, hd0_type
	mov 	al, [ebx]
	or	al, 80h
	mov 	[ebx], al	
	sub	ebx, hd0_type - 2 ; 15/01/2015
	add	ebx, drv_status
	mov	[ebx], al
	mov	eax, [esi+16]
	sub	ebx, drv_status
	shl	ebx, 2
	add	ebx, disk_size
	mov	[ebx], eax
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
	; ES:DI points to DPTE (FDPTE) location
	;mov	cx, 8
	mov	cl, 8
	rep	movsw	
	;
	mov	ax, cs
	mov	ds, ax
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
	jz 	short L14K	
	mov	al, 10h
	int 	16h
	jmp 	short L14
L14K:
; //////
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
	jz	short L14a
	mov	dl, al
	mov	al, 'F'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '0'
	int 	10h
	mov	al, ' '
	int	10h
	call	L14c
	mov	al, ' '
	int	10h
	;
	mov	al, [fd1_type]
	and	al, al
	jz	short L14a
	mov	dl, al
	mov	al, 'F'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '1'
	int 	10h
	mov	al, ' '
	int	10h
	call	L14c
	mov	al, ' '
	int	10h
	mov	al, ' '
	int	10h
L14a:
	mov	al, [hd0_type]
	and	al, al
	jz	short L14b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '0'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L14c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd1_type]
	and	al, al
	jz	short L14b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '1'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L14c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd2_type]
	and	al, al
	jz	short L14b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '2'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L14c
	mov	al, ' '
	int	10h
	;
	mov	al, [hd3_type]
	and	al, al
	jz	short L14b
	mov	dl, al
	mov	al, 'H'
	int 	10h
	mov	al, 'D'
	int 	10h
	mov	al, '3'
	int 	10h
	mov	al, ' '
	int 	10h
	call	L14c
	mov	al, ' '
	int	10h
	;
L14b:
	mov	al, 0Dh
	int 	10h	
	mov	al, 0Ah
	int 	10h
	;;xor	ah, ah
	;;int 	16h	
	;
	jmp	short L15
	;
L14c:
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

; //////

L15:
	; 11/12/2014
	; SAVE ORIGINAL/OLD INTERRUPT VECTORS & POINTERS
;;;;
	push	ds
	xor	ax, ax
	mov	ds, ax
	mov	si, 08h*4	; IRQ 0 - INT 08h
	mov	di, ORG_INT08H
	movsd
	mov	si, 0Eh*4	; IRQ 6 - INT 0Eh
	mov	di, ORG_INT0EH	
	movsd
	mov	si, 13h*4	; INT 13h
	mov	di, ORG_INT13H	
	movsd
	mov	si, 40h*4	; INT 40h
	mov	di, ORG_INT40H
	movsd
	mov	si, 76h*4	; IRQ 14 - INT 76h
	mov	di, ORG_INT76H
	movsd
	mov	si, 77h*4	; IRQ 15 - INT 77h
	mov	di, ORG_INT77H
	movsd
	pop	ds

	push	es
	;sub 	ax, ax
	mov	es, ax

	mov	dx, [hd0_type] ; hd0, hd1
	mov	bx, [fd0_type] ; fd0, fd1

	; 23/12/2014
	and	bx, bx
	jz	short set_int08h
	; supress 'jmp short T6'
	;  (activate fdc motor control code)
	mov	word [T5], 9090h ; nop
				 ; nop
	mov	ax, int_40h	; DISKETTE_IO_1
	mov	di, 13h*4	; INT 13h vector
	stosw
	mov 	ax, cs
	stosw
	;
	mov	ax, int_0Eh	; IRQ 6 handler
	mov	di, 0Eh*4	; IRQ 6 vector
	stosw
	mov 	ax, cs
	stosw
	;
	or	dx, dx
	jnz	short set_int08h
	;
	mov	ax, int_40h	; DISKETTE_IO_1
	mov	di, 40h*4	; INT 40h vector
	stosw
	mov 	ax, cs
	stosw
	;	 
set_int08h:
	cli
	mov	ax, int_08h	; IRQ 0 handler
	mov	di, 08h*4	; IRQ 0 vector
	stosw
	mov 	ax, cs
	stosw
	sti
	;
	pop	es
	;
	or	bx, bx
	jz	short dsksetup
	CALL	DSKETTE_SETUP	; Initialize Floppy Disks
dsksetup:
	or	dx, dx
        jz      nodsk
	call   	DISK_SETUP	; Initialize Fixed Disks
        jnc     dsksetok
	mov 	si, setup_error_msg
	call 	print_msg
        jmp     dsksetok
setup_error_msg:
	db 0Dh, 0Ah
	db 'Disk Setup Error!' 
	db 0Dh, 0Ah,0

set_disk_parms:
	; 14/01/2015
	;push	ebx
	sub	ebx, ebx
	mov	bl, [drv]
	cmp	bl, 80h
	jb	short sdp0
	sub	bl, 7Eh
sdp0:	
	add	ebx, drv_status
  	mov	byte [ebx], 80h ; 'Present' flag
	;
	mov	al, ch ; last cylinder (bits 0-7)
	mov	ah, cl ; 
	shr	ah, 6  ; last cylinder (bits 8-9)
	sub	ebx, drv_status
	shl	bl, 1
	add	ebx, cylinders
	inc	ax  ; convert max. cyl number to cyl count		
	mov	[ebx], ax
	push	ax ; ** cylinders
	sub	ebx, cylinders
	add	ebx, heads
	xor	ah, ah
	mov	al, dh ; heads
	inc	ax
	mov	[ebx], ax
	sub	ebx, heads
	add	ebx, spt
	xor	ch, ch
	and	cl, 3Fh	; sectors (bits 0-6)
	mov	[ebx], cx
	sub	ebx, spt
	shl	ebx, 1
	add	ebx, disk_size
	; LBA size = cylinders * heads * secpertrack
	mul	cx 
	mov	dx, ax	; heads*spt					
	pop	ax ; ** cylinders
	dec	ax ; 1 cylinder reserved (!?)
	mul	dx ; cylinders * (heads*spt)		
	mov	[ebx], ax
	mov	[ebx+2], dx
	;
	;pop	ebx
	retn

dsksetok:
        ;?       
nodsk:
