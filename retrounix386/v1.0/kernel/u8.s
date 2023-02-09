; Retro UNIX 386 v1 Kernel (v0.2.0.22) - SYS8.INC
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
; Retro UNIX 8086 v1 - U8.ASM (18/01/2014) //// UNIX v1 -> u8.s
;
; ****************************************************************************
; 24/10/2015

; 14/07/2022 - Retro UNIX 386 v1 (Kernel v0.2.0.22)
;---------------------------------------------------------------------
; Buffer Header: -8 bytes-
; word 1 - byte 0 - device (disk index) number (0 to 5)
;	   byte 1 - status bits
;	 	bit 0 - valid buffer bit (1 = valid, 0 = invalid, new)
;		bit 1 - write bit (also modified bit)
;		bit 2 - read bit	
;		bit 3 to bit 7 are not used
; word 2 - byte & byte 3 not used
; word 3 & word 4 - byte 3, byte 5, byte 6, byte 7:
;		physical block/sector address (32 bit LBA)
;---------------------------------------------------------------------

;; I/O Buffer - Retro UNIX 386 v1 modification
;;     (8+512 bytes, 8 bytes header, 512 bytes data)
;; Word 1, byte 0 = device id
;; Word 1, byte 1 = status bits (bits 8 to 15)
;;          bit 9 = write bit
;;	    bit 10 = read bit	  
;;	    bit 12 = waiting to write bit	
;;	    bit 13 = waiting to read bit
;;	    bit 15 = inhibit bit
;; Word 2 (byte 2 & byte 3) = reserved (for now - 07/06/2015)
;; Word 3 + Word 4 (byte 4,5,6,7) = physical block number 
;;		   (In fact, it is 32 bit LBA for Retro UNIX 386 v1)
;;
;; I/O Buffer ((8+512 bytes in original Unix v1))
;;	      ((4+512 bytes in Retro UNIX 8086 v1))
;;
;; I/O Queue Entry (of original UNIX operating system v1)
;; Word 1, Byte 0 = device id
;; Word 1, Byte 1 = (bits 8 to 15)
;;          bit 9 = write bit
;;	    bit 10 = read bit	  
;;	    bit 12 = waiting to write bit	
;;	    bit 13 = waiting to read bit
;;	    bit 15 = inhibit bit
;; Word 2 = physical block number (In fact, it is LBA for Retro UNIX 8086 v1)
;;
;; Original UNIX v1 ->
;;		Word 3 = number of words in buffer (=256) 		
;; Original UNIX v1 -> 
;;		Word 4 = bus address (addr of first word of data buffer)
;;
;; Retro UNIX 8086 v1 -> Buffer Header (I/O Queue Entry) size is 4 bytes !
;;
;; Device IDs (of Retro Unix 8086 v1)
;;          0 = fd0
;;	    1 = fd1
;;	    2 = hd0
;;	    3 = hd1
;;	    4 = hd2
;;	    5 = hd3

; Retro UNIX 386 v1 - 32 bit modifications (rfd, wfd, rhd, whd) - 09/06/2015

	; 04/02/2022
rfd:	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013
   	; 13/03/2013 Retro UNIX 8086 v1 device (not an original unix v1 device)	
   	;sub 	ax, 3 ; zero based device number (Floppy disk)
      	;jmp 	short bread ; **** returns to routine that called readi			

	; 04/02/2022
rhd:	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013
   	; 14/03/2013 Retro UNIX 8086 v1 device (not an original unix v1 device)	
   	;sub 	ax, 3 ; zero based device number (Hard disk)
   	;jmp	short bread ; **** returns to routine that called readi	

bread: 
	; 19/07/2022
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 12/02/2022
	; 04/02/2022
	; 14/07/2015
	; 10/07/2015
	; 09/06/2015
	; 07/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 13/03/2013 - 29/07/2013 (Retro UNIX 8086 v1)
	;	
	; / read a block from a block structured device
	;
	; INPUTS ->
	;    [u.fofp] points to the block number
	;    ECX = maximum block number allowed on device
	;	 ; that was an arg to bread, in original Unix v1, but
	;	 ; CX register is used instead of arg in Retro Unix 8086 v1 		
	;    [u.count]	number of bytes to read in
	; OUTPUTS ->
	;    [u.base] starting address of data block or blocks in user area  	
	;    [u.fofp] points to next consecutive block to be read
	;
	; ((Modified registers: EAX, EDX, ECX, EBX, ESI, EDI))
	;
	; NOTE: Original UNIX v1 has/had a defect/bug here, even if read
	;       byte count is less than 512, block number in *u.fofp (u.off)
	;	is increased by 1. For example: If user/program request 
	;       to read 16 bytes in current block, 'sys read' increases
	;  	the next block number just as 512 byte reading is done.
	;       This wrong is done in 'bread'. So, in Retro UNIX 8086 v1, 
	;       for user (u) structure compatibility (because 16 bit is not
	;       enough to keep byte position/offset of the disk), this
	;	defect will not be corrected, user/program must request
	;	512 byte read per every 'sys read' call to block devices
	;       for achieving correct result. In future version(s), 
	;	this defect will be corrected by using different 
	;       user (u) structure. 26/07/2013 - Erdogan Tan 	

	   	; jsr r0,tstdeve / error on special file I/O 
			       ; / (only works on tape)
		; mov *u.fofp,r1 / move block number to r1
		; mov $2.-cold,-(sp) / "2-cold" to stack
;1:
		; cmp r1,(r0) / is this block # greater than or equal to
			    ; / maximum block # allowed on device
		; jnb short @f
		; bhis	1f / yes, 1f (error)
		; mov r1,-(sp) / no, put block # on stack
		; jsr r0,preread / read in the block into an I/O buffer
		; mov (sp)+,r1 / return block # to r1
		; inc r1 / bump block # to next consecutive block
		; dec (sp) / "2-1-cold" on stack
		; bgt 1b / 2-1-cold = 0?  No, go back and read in next block
;1:
		; tst (sp)+ / yes, pop stack to clear off cold calculation
	;push	ecx ; **
	;26/04/2013
	;sub	ax, 3 ; 3 to 8 -> 0 to 5
	sub	al, 3
		; AL = Retro Unix 8086 v1 disk (block device) number
	mov	[u.brwdev], al
	; 09/06/2015
	;movzx	ebx, al
	;mov	ecx, [ebx+drv.size] ; disk size (in sectors)
	; 04/02/2022 (BugFix)
	shl	al, 2 ; * 4
	mov	ecx, [eax+drv.size] ; disk size (in sectors)
bread_0:
	push	ecx ; ** ; 09/06/2015 
	; 10/07/2015 (Retro UNIX 386 v1 modification!)
	; [u.fofp] points to byte position in disk, not sector/block !
	mov	ebx, [u.fofp]
	mov	eax, [ebx]
	shr	eax, 9 ; convert byte position to block/sector number
		; mov *u.fofp,r1 / restore r1 to initial value of the
			       ; / block #
	cmp	eax, ecx
		; cmp r1,(r0)+ / block # greater than or equal to maximum
       	                     ; / block number allowed
	;jnb	error 	     ; 18/04/2013
		; bhis error10 / yes, error
	; 12/02/2022
	;jb	short bread_1
	;mov 	dword [u.error], ERR_DEV_VOL_SIZE  ; 'out of volume' error
	;jmp	error
	jnb	short brw_oov_err ; 'out of volume' error
bread_1:
	;inc 	dword [ebx] ; 10/07/2015 (Retro UNIX 386 v1 - modification!)
		; inc *u.fofp / no, *u.fofp has next block number
	; EAX = Block number (zero based)
		;;jsr r0,preread / read in the block whose number is in r1
preread: ;; call preread
	mov	edi, u.brwdev ; block device number for direct I/O
	call	bufaloc_0 ; 26/04/2013
	;; jc 	error
	; EBX = Buffer (Header) Address -Physical-
        ; EAX = Block/Sector number (r1)
	       ; jsr r0,bufaloc / get a free I/O buffer (r1 has block number)
	; 14/03/2013
        jz	short bread_2 ; Retro UNIX 8086 v1 modification
       		; br 1f / branch if block already in a I/O buffer
	or	word [ebx], 400h ; set read bit (10) in I/O Buffer
        	; bis $2000,(r5) / set read bit (bit 10 in I/O buffer)
	call	poke
        	; jsr r0,poke / perform the read
	;;jc	error ;2 0/07/2013
; 1:
 		; clr *$ps / ps = 0
        	; rts r0
	; 12/02/2022
	jnc	short bread_2
	jmp	dskrd_err

;; return from preread
bread_2:
	; 14/07/2022
	;or	word [ebx], 4000h 
	;	; bis $40000,(r5) 
	;		; / set bit 14 of the 1st word of the I/O buffer
bread_3: ; 1:
	; 14/07/2022
	;test	word [ebx], 2400h
	;	; bit $22000,(r5) / are 10th and 13th bits set (read bits)
	;jz	short bread_4
	;	; beq 1f / no
	;	; cmp cdev,$1 / disk or drum?
	;	; ble 2f / yes
	;	; tstb uquant / is the time quantum = 0?
	;	; bne 2f / no, 2f
	;	; mov r5,-(sp) / yes, save r5 (buffer address)
	;	; jsr r0,sleep; 31. 
	;		; / put process to sleep in channel 31 (tape)
	;	; mov (sp)+,r5 / restore r5
	;	; br 1b / go back
; 2: / drum or disk
        ;; mov     cx, [s.wait_]+2 ;; 29/07/2013
	;call	idle
	;	; jsr r0,idle; s.wait+2 / wait
	;jmp	short bread_3
       	;	; br 1b
bread_4: ; 1: / 10th and 13th bits not set
	;and	word [ebx], 0BFFFh ; 1011111111111111b
	;	; bic $40000,(r5) / clear bit 14
       	;	; jsr r0,tstdeve / test device for error (tape)
	; 14/07/2022
	add	ebx, 8
		; add $8,r5 / r5 points to data in I/O buffer
	; 09/06/2015
	cmp	word [u.pcount], 0
	ja	short bread_5
	call	trans_addr_w ; translate virtual address to physical (w)
bread_5:
	; EBX = system (I/O) buffer address
	call	dioreg
       		; jsr r0,dioreg / do bookkeeping on u.count etc.

	; 19/07/2022
	; EDI = user data offset (previous value of [u.pbase])
	; ESI = pointer to file offset 
	; EAX = system (I/O) buffer offset (>= EBX)
	; ECX = byte count
	; EBX = system buffer (data) address

	; 19/07/2022
	add	[esi], ecx ; new file (disk) offset
	mov	esi, eax 

	; esi = start address of the transfer (in the buffer)
	; edi = [u.pbase], destination address in user's memory space
	; ecx = transfer count (in bytes)
	;
;1: / r5 points to beginning of data in I/O buffer, r2 points to beginning
;   / of users data
	rep	movsb
		; movb (r5)+,(r2)+ / move data from the I/O buffer
       		; dec r3 / to the user's area in core starting at u.base
       		; bne 1b
	pop	ecx ; **
	cmp	dword [u.count], 0
		; tst u.count / done
	ja	short bread_0 ; 09/06/2015
       		; beq 1f / yes, return
		; tst -(r0) / no, point r0 to the argument again
       		; br bread / read some more
; 1:
	pop	eax ; ****
       		; mov (sp)+,r0
        retn		; 09/06/2015
	;jmp	ret_ 
		;jmp ret / jump to routine that called readi

	; 08/02/2022
brw_oov_err:
	mov 	dword [u.error], ERR_DEV_VOL_SIZE  ; 'out of volume' error
	jmp	error

	; 12/02/2022
wfd:    ; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013
   	; 14/03/2013 Retro UNIX 8086 v1 device (not an original unix v1 device)	
   	;sub 	ax, 3 ; zero based device number (Hard disk)
   	;jmp	short bwrite ; **** returns to routine that called writei

	; 12/02/2022				
whd:	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
   	; 14/03/2013 Retro UNIX 8086 v1 device (not an original unix v1 device)	
   	;sub 	ax, 3 ; zero based device number (Hard disk)
   	;jmp 	short bwrite ; **** returns to routine that called writei ('jmp ret')

bwrite: 
	; 19/07/2022
	;	(file offset bugfix for 'dskwr' error return situation)
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 12/02/2022
	; 04/02/2022
	; 14/07/2015
	; 10/07/2015
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/03/2013 - 20/07/2013 (Retro UNIX 8086 v1)
	;	
	;; / write on block structured device
	;
	; INPUTS ->
	;    [u.fofp] points to the block number
	;    ECX = maximum block number allowed on device
	;	 ; that was an arg to bwrite, in original Unix v1, but
	;	 ; CX register is used instead of arg in Retro Unix 8086 v1 		
	;    [u.count]	number of bytes to user desires to write
	; OUTPUTS ->
	;    [u.fofp] points to next consecutive block to be written into
	;
	; ((Modified registers: EDX, ECX, EBX, ESI, EDI))
	;
	; NOTE: Original UNIX v1 has/had a defect/bug here, even if write
	;       byte count is less than 512, block number in *u.fofp (u.off)
	;	is increased by 1. For example: If user/program request 
	;       to write 16 bytes in current block, 'sys write' increases
	;  	the next block number just as 512 byte writing is done.
	;       This wrong is done in 'bwrite'. So, in Retro UNIX 8086 v1, 
	;       for user (u) structure compatibility (because 16 bit is not
	;       enough to keep byte position/offset of the disk), this
	;	defect will not be corrected, user/program must request
	;	512 byte write per every 'sys write' call to block devices
	;       for achieving correct result. In future version(s), 
	;	this defect will be corrected by using different 
	;       user (u) structure. 26/07/2013 - Erdogan Tan 	

       		; jsr r0,tstdeve / test the device for an error
	;push	ecx ; **
	;26/04/2013
	;sub	ax, 3 ; 3 to 8 -> 0 to 5
	sub	al, 3
		; AL = Retro Unix 8086 v1 disk (block device) number
	mov	[u.brwdev], al
	; 09/06/2015
	;movzx	ebx, al
	;mov	ecx, [ebx+drv.size] ; disk size (in sectors)
	; 04/02/2022 (BugFix)
	shl	al, 2 ; * 4
	mov	ecx, [eax+drv.size] ; disk size (in sectors)
bwrite_0:
	push	ecx ; ** ; 09/06/2015
	; 10/07/2015 (Retro UNIX 386 v1 modification!)
	; [u.fofp] points to byte position in disk, not sector/block !
	mov	ebx, [u.fofp]
	mov	eax, [ebx]       
	shr	eax, 9 ; convert byte position to block/sector number
		; mov *u.fofp,r1 / put the block number in r1
	cmp	eax, ecx
		; cmp r1,(r0)+ / does block number exceed maximum allowable #
       	                     ; / block number allowed
	;jnb	error	     ; 18/04/2013
		; bhis error10 / yes, error
     	; 12/02/2022
	;jb	short bwrite_1
	;mov 	dword [u.error], ERR_DEV_VOL_SIZE  ; 'out of volume' error
	;jmp	error
	jnb	short brw_oov_err ; 'out of volume' error
bwrite_1:
	;inc 	dword [ebx] ; 10/07/2015 (Retro UNIX 386 v1 - modification!)
		; inc *u.fofp / no, increment block number
	; 09/06/2015 - 10/07/2015
	cmp	word [u.pcount], 0
	ja	short bwrite_2
	call	trans_addr_r ; translate virtual address to physical (r)
bwrite_2:
	mov	edi, u.brwdev  ; block device number for direct I/O
       	call	bwslot ; 26/04/2013 (wslot -> bwslot)	 				
		; jsr r0,wslot / get an I/O buffer to write into
		; add $8,r5 / r5 points to data in I/O buffer
        call	dioreg
		; jsr r0,dioreg / do the necessary bookkeeping

	; 19/07/2022
	; EDI = user data offset (previous value of [u.pbase])
	; ESI = pointer to file offset 
	; EAX = system (I/O) buffer offset (>= EBX)
	; ECX = byte count
	; EBX = system buffer (data) address

	; 19/07/2022 - Erdogan Tan
	; ((Against a possible disk write failure/error, 
	;   file offset must not be updated/increased before 'dskwr'
	;   but it was updated in 'dioreg'. I have modified 'dioreg'
	;   and 'bwrite' procedures for that.))

	; 19/07/2022
	push	esi ; (!)  ; save file offset (pointer)
	push	ecx ; (!!) ; save byte count
	;mov	esi, eax
	
	; esi = destination address (in the buffer)
	; edi = [u.pbase], start address of transfer in user's memory space
	; ecx = transfer count (in bytes)
; 1: / r2 points to the users data; r5 points to the I/O buffers data area
	;xchg 	esi, edi ; 14/07/2015

	; 19/07/2022
	mov	esi, edi
	mov	edi, eax
	
	rep	movsb
		; movb (r2)+,(r5)+ / ; r3, has the byte count
       		; dec r3 / area to the I/O buffer
       		; bne 1b

	call	dskwr
		; jsr r0,dskwr / write it out on the device

	; 19/07/2022
	; (there is not a disk write error, we can increase file offset)
	pop	eax ; (!!) ; byte count
	pop	edi ; (!)  ; file offset (pointer)
	;
	add	[edi], eax ; new file offset (old offset + byte count)

	pop	ecx ; **

        cmp     dword [u.count], 0
		; tst u.count / done
	ja	short bwrite_0 ; 09/06/2015
		; beq 1f / yes, 1f
		; tst -(r0) / no, point r0 to the argument of the call
       		; br bwrite / go back and write next block
; 1:
	pop	eax ; ****
       		; mov (sp)+,r0
	retn		; 09/06/2015
        ;jmp	ret_ 
		; jmp ret / return to routine that called writei
;error10:
;       jmp     error  ; / see 'error' routine

dioreg:
	; 19/07/2022
	;	(file offset bugfix for 'dskwr' error return situation)
	; 04/02/2022
	; 14/07/2015
	; 10/07/2015 (UNIX v1 bugfix - [u.fofp]: byte pos., not block)
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/03/2013 (Retro UNIX 8086 v1)
	;	
	; bookkeeping on block transfers of data
	;
	; * returns value of u.pbase before it gets updated, in EDI
	; * returns byte count (to transfer) in ECX (<=512)
	; 10/07/2015
	; * returns byte offset from beginning of current sector buffer
	; (beginning of data) in ESI


	; 19/07/2022
	; OUTPUTS:
	;   EDI = user data offset (previous value of [u.pbase])
	;   ESI = pointer to file offset 
	;   EAX = system (I/O) buffer offset
	;   ECX = byte count
	;   EBX = system buffer (data) address	

	mov	ecx, [u.count]
		; mov u.count,r3 / move char count to r3
        ; 04/02/2022
	xor	edx, edx
	mov	dh, 2
	; edx = 512
	cmp	ecx, edx ; 512
	;cmp 	ecx, 512
		; cmp r3,$512. / more than 512. char?
	jna	short dioreg_0
		; blos 1f / no, branch
	mov	ecx, edx ; 512
	;mov	ecx, 512
		; mov $512.,r3 / yes, just take 512.
dioreg_0:
	; 09/06/2015
	cmp	cx, [u.pcount]
	jna	short dioreg_1
	mov	cx, [u.pcount]
dioreg_1:
; 1:
	mov	edx, [u.base] ; 09/06/2015 (eax -> edx)
	        ; mov u.base,r2 / put users base in r2
	add	[u.nread], ecx
		; add r3,u.nread / add the number to be read to u.nread
	sub	[u.count], ecx
		; sub r3,u.count / update count
	add	[u.base], ecx
		; add r3,u.base / update base
	; 10/07/2015
	; Retro UNIX 386 v1 - modification !
	; (File pointer points to byte position, not block/sector no.)
	; (It will point to next byte position instead of next block no.)
	mov	esi, [u.fofp] ; u.fopf points to byte position pointer  
	mov	eax, [esi] ; esi points to current byte pos. on the disk
	; 19/07/2022
	;add	[esi], ecx ; ecx is added to set the next byte position
	and	eax, 1FFh  ; get offset from beginning of current block	
	;mov	esi, ebx   ; beginning of data in sector/block buffer
	;add	esi, eax   ; esi contains start address of the transfer
	; 19/07/2022
	add	eax, ebx   ; eax contains start address of the transfer	
	; 09/06/2015 - 10/07/2015
	sub	[u.pcount], cx
	and	edx, PAGE_OFF ; 0FFFh
	mov	edi, [u.pbase]
	and	edi, ~PAGE_OFF
	add	edi, edx
	mov	[u.pbase], edi
	add	[u.pbase], ecx ; 14/07/2015
	retn
		; rts r0 / return

dskrd:
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 12/02/2022
	; 18/08/2015
	; 02/07/2015
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/03/2013 - 29/07/2013 (Retro UNIX 8086 v1)
	;
	; 'dskrd' acquires an I/O buffer, puts in the proper
	; I/O queue entries (via bufaloc) then reads a block
	; (number specified in r1) in the acquired buffer.
	; If the device is busy at the time dskrd is called,	
	; dskrd calls idle.
	; 
	; INPUTS ->
	;    r1 - block number
	;    cdev - current device number 
	; OUTPUTS ->
	;    r5 - points to first data word in I/O buffer
	;
	; ((AX = R1)) input/output
	; ((BX = R5)) output 
	;
        ; ((Modified registers: EDX, ECX, EBX, ESI, EDI))  
	;
	call 	bufaloc
		; jsr r0,bufaloc / shuffle off to bufaloc; 
			       ; / get a free I/O buffer
	;;jc	error ; 20/07/2013
	jz	short dskrd_1 ; Retro UNIX 8086 v1 modification
       		; br 1f / branch if block already in a I/O buffer

	; 14/07/2022
	; (buffer header byte 1, bit 2 is disk read bit/flag)
dskrd_0: ; 10/07/2015 (wslot)
	or	word [ebx], 400h ; set read bit (10) in I/O Buffer
        ;	; bis $2000,(r5) / set bit 10 of word 1 of 
	;	               ; / I/O queue entry for buffer
	call	poke
		; jsr r0,poke / just assigned in bufaloc, 
			    ; /	bit 10=1 says read
	; 09/06/2015
	;jnc	short dskrd_1
	;mov	dword [u.error], ERR_DRV_READ ; disk read error !
	;jmp	error
	; 08/02/2022
	jc	short dskrd_3
dskrd_1: ; 1:
       	; 14/07/2022
	;
	;	;clr *$ps
       	;test	word [ebx], 2400h
	;	; bit $22000,(r5) / if either bits 10, or 13 are 1; 
	;			; / jump to idle
       	;jz	short dskrd_2
	;	; beq 1f
        ;;;mov   ecx, [s.wait_]
       	;call	idle
	;	; jsr r0,idle; s.wait+2
	;jmp 	short dskrd_1
       	;	; br 1b
dskrd_2: ; 1:
        add	ebx, 8
		; add $8,r5 / r5 points to first word of data in block 
			  ; / just read in
       	retn
		; rts r0
dskrd_err: 
	; 08/02/2022
	; (jump from 'bread' error)
dskrd_3:	
	; 08/02/2022
	cmp	byte [u.brwdev], 0FFh ; is error code set in [u.error] ?
	jne	short dskrd_4 ; no
	; yes, clear [u.brwdev] for next check and jump to 'error'
	mov	byte [u.brwdev], 0
	jmp	short dskrd_5
dskrd_4:
	mov	dword [u.error], ERR_DRV_READ ; disk read error !
dskrd_5:
	jmp	error

bwslot:
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 04/02/2022
	; 10/07/2015
	;	If the block/sector is not placed in a buffer
	;	before 'wslot', it must be read before
	;	it is written! (Otherwise transfer counts less
	;	than 512 bytes will be able to destroy existing 
	;	data on disk.)
	;
	; 11/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013(Retro UNIX 8086 v1)
	; Retro UNIX 8086 v1 modification !
	; ('bwslot' will be called from 'bwrite' only!)
	; INPUT -> EDI - points to device id (in u.brwdev)	
	;	-> EAX = block number
	;
	call	bufaloc_0
	jz	short wslot_0 ; block/sector already is in the buffer
	; 04/02/2022
	; ebx = buffer header address
bwslot_0:
	; 10/07/2015
	mov	esi, [u.fofp]
	mov	eax, [esi]
	and	eax, 1FFh ; offset from beginning of the sector/block
	jnz 	short bwslot_1 ; it is not a full sector write
		       ; recent disk data must be placed in the buffer
	cmp	dword [u.count], 512
	jnb	short wslot_0	
bwslot_1:
	call	dskrd_0
	; 04/02/2022
	; ebx = buffer data address = buffer header address + 8
	sub	ebx, 8 ; set ebx to the buffer header address again
	jmp 	short wslot_0

wslot:
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	;
	; 11/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 		(32 bit modifications)
	; 14/03/2013 - 29/07/2013(Retro UNIX 8086 v1)
	;
	; 'wslot' calls 'bufaloc' and obtains as a result, a pointer
	; to the I/O queue of an I/O buffer for a block structured
	; device. It then checks the first word of I/O queue entry.	
	; If bits 10 and/or 13 (read bit, waiting to read bit) are set,
	; wslot calls 'idle'. When 'idle' returns, or if bits 10 
	; and/or 13 are not set, 'wslot' sets bits 9 and 15 of the first
	; word of the I/O queue entry (write bit, inhibit bit).
	;
	; INPUTS ->
 	;    r1 - block number
	;    cdev - current (block/disk) device number
 	;
	; OUTPUTS ->
	;    bufp - bits 9 and 15 are set, 
	;           the remainder of the word left unchanged
	;    r5 - points to first data word in I/O buffer
	;
	; ((AX = R1)) input/output
	; ((BX = R5)) output 
	;
        ; ((Modified registers: EDX, ECX, EBX, ESI, EDI)) 

	call	bufaloc
	; 10/07/2015
		; jsr r0,bufaloc / get a free I/O buffer; pointer to first
        	; br 1f / word in buffer in r5
	; EBX = Buffer (Header) Address (r5) (ES=CS=DS, system/kernel segment)
        ; EAX = Block/Sector number (r1)
wslot_0: ;1:
	; 14/07/2022
	;
     	;test	word [ebx], 2400h
	;	; bit $22000,(r5) / check bits 10, 13 (read, waiting to read)
	;			; / of I/O queue entry
	;jz	short wslot_1
        ;       ; beq 1f  / branch if 10, 13 zero (i.e., not reading, 
	;	       ; / or not waiting to read)
	;
	;;mov	ecx, [s.wait_] ; 29/07/2013
	;call	idle
	;	; jsr r0,idle; / if buffer is reading or writing to read,
       	;                   ; / idle
	;jmp	short wslot_0
	;	; br 1b / till finished
wslot_1: ;1:
	;or	word [ebx], 8200h
       	;	; bis $101000,(r5) / set bits 9, 15 in 1st word of I/O queue
        ;			; / (write, inhibit bits)
	;	; clr *$ps / clear processor status

	; 14/07/2022
	; (set disk write bit/flag)
	;or	word [ebx], 200h	
	inc	ebx
	or	byte [ebx], 2
	add	ebx, 7

	;add	ebx, 8 ; 11/06/2015
		; add $8,r5 / r5 points to first word in data area 
			  ; / for this block
dskwr_1:	; 08/02/2022
	retn
		; rts r0
dskwr:
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 12/02/2022
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 14/03/2013 - 03/08/2013 (Retro UNIX 8086 v1)
	;
	; 'dskwr' writes a block out on disk, via ppoke. The only
	; thing dskwr does is clear bit 15 in the first word of I/O queue
	; entry pointed by 'bufp'. 'wslot' which must have been called
	; previously has supplied all the information required in the
	; I/O queue entry.
	;
	; (Modified registers: ECX, EDX, EBX, ESI, EDI)
	;
	;
	mov	ebx, [bufp]
	; 14/07/2022
	;and	word [ebx], 7FFFh ; 0111111111111111b
	;	; bic $100000,*bufp / clear bit 15 of I/O queue entry at
                                  ; / bottom of queue
	; 14/07/2022
	; (set disk write bit)
	; ('wslot' already sets disk write bit/flag)
	;mov	word [ebx], 200h

	call	poke
	; 09/06/2015
	jnc	short dskwr_1
	; 12/02/2022
	cmp	byte [u.brwdev], 0FFh ; is error code set in [u.error] ?
	jne	short dskwr_0 ; no
	; yes, clear [u.brwdev] for next check and jump to 'error'
	mov	byte [u.brwdev], 0
	jmp	short dskwr_2
dskwr_0:
	mov	dword [u.error], ERR_DRV_WRITE ; disk write error !
dskwr_2:
	jmp	error
;dskwr_1:
;	retn

;ppoke:
       		; mov $340,*$ps
       		; jsr r0,poke
       		; clr *$ps
		; rts r0
poke:
	; 14/07/2022
	;	! Major Modification !
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	;
	; 04/02/2022 (32 bit reg push pop)
	; 24/10/2015
	; 20/08/2015
	; 18/08/2015
	; 02/07/2015
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/03/2013 - 18/01/2014 (Retro UNIX 8086 v1)
	;
	; (NOTE: There are some disk I/O code modifications & extensions
	; & exclusions on original 'poke' & other device I/O procedures of 
	; UNIX v1 OS for performing disk I/O functions by using IBM PC 
	; compatible rombios calls in Retro UNIX 8086 v1 kernel.)
	;
	; Basic I/O functions for all block structured devices
	;
        ; (Modified registers: ECX, EDX, ESI, EDI)
	;
	; 20/07/2013 modifications
	;            (Retro UNIX 8086 v1 features only !)
	; INPUTS -> 
	;        EBX = buffer header address
	; OUTPUTS ->
	;	 cf=0 -> successed r/w (at least, for the caller's buffer) 
	;	 cf=1 -> error, word [EBX] = 0FFFFh
	;		(drive not ready or r/w error!)
	;	 (dword [EBX+4] <> 0FFFFFFFFh indicates r/w success)	
	;	 (dword [EBX+4] = 0FFFFFFFFh means RW/IO error)
	;        (also it indicates invalid buffer data)
	
	; 14/07/2022 - Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; Buffer header:
	;	byte 0 - device/disk (index) number
	;	byte 1 - status
	;	    before 'poke'	
	;		bit 0 - valid bit 
	;		   (0 for new buffer)
	;		   (1 for existing buffer)	
	;		bit 1 - write (modified) bit, disk write flag
	;		bit 2 - read bit, disk read flag
	;	    after 'poke'
	;		if disk r/w is successed
	;		   bit 0 = 1
	;		   bit 1 = 0
	;		   bit 2 = 0
	;		if disk r/w is failed
	;		   bit 0 = 0 (invalid buffer)
	;		   bit 1 = 0 (write bit)
	;		   bit 2 = 0 (read bit)	
 	;
	;	byte 2 & byte 3 - not used
	;	byte 4 to byte 7 - disk block/sector address
	
	; 04/02/2022
	push	eax
	call	diskio ; Retro UNIX 8086 v1 Only !
	pop	eax
	jnc	short seta ; 14/07/2022
	
	; 14/07/2022
	; (invalidate buffer)
	
	; 02/07/2015 (32 bit modification)
	; 20/07/2013
	mov	dword [ebx+4], 0FFFFFFFFh ; -1 
       		; mov $-1,2(r1) / destroy associativity
	mov	word [ebx], 0FFh ; 20/08/2015 
		; clrb 1(r1) / do not do I/O
	;stc
	retn
        	; rts r0
seta: 
	mov	byte [ebx+1], 1 ; clear write/read bits, set valid bit
	;clc
	retn

%if 0

poke:
	; 04/02/2022 (32 bit reg push pop)
	; 24/10/2015
	; 20/08/2015
	; 18/08/2015
	; 02/07/2015
	; 09/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 15/03/2013 - 18/01/2014 (Retro UNIX 8086 v1)
	;
	; (NOTE: There are some disk I/O code modifications & extensions
	; & exclusions on original 'poke' & other device I/O procedures of 
	; UNIX v1 OS for performing disk I/O functions by using IBM PC 
	; compatible rombios calls in Retro UNIX 8086 v1 kernel.)
	;
	; Basic I/O functions for all block structured devices
	;
        ; (Modified registers: ECX, EDX, ESI, EDI)
	;
	; 20/07/2013 modifications
	;            (Retro UNIX 8086 v1 features only !)
	; INPUTS -> 
	;        (EBX = buffer header address)
	; OUTPUTS ->
	;	 cf=0 -> successed r/w (at least, for the caller's buffer) 
	;	 cf=1 -> error, word [EBX] = 0FFFFh
	;		(drive not ready or r/w error!)
	;	 (dword [EBX+4] <> 0FFFFFFFFh indicates r/w success)	
	;	 (dword [EBX+4] = 0FFFFFFFFh means RW/IO error)
	;        (also it indicates invalid buffer data)
	;
	push	ebx
       		; mov r1,-(sp)
       		; mov r2,-(sp)
       		; mov r3,-(sp)
	push 	eax ; Physical Block Number (r1) (mget)
	;
	; 09/06/2015
	; (permit read/write after a disk R/W error)
	mov	cl, [ebx] ; device id (0 to 5)
	mov	al, 1
	shl	al, cl
	test 	al, [active] ; busy ? (error)
	jz	short poke_0
	not	al
	and	[active], al ; reset busy bit for this device only
poke_0:
        mov     esi, bufp + (4*(nbuf+2)) 
		; mov $bufp+nbuf+nbuf+6,r2 / r2 points to highest priority
					 ; / I/O queue pointer
poke_1: ; 1:
        sub	esi, 4
	mov	ebx, [esi]
		; mov -(r2),r1 / r1 points to an I/O queue entry
	mov	ax, [ebx] ; 17/07/2013
       	test	ah, 06h
	;test	word [ebx], 600h ; 0000011000000000b
		; bit $3000,(r1) / test bits 9 and 10 of word 1 of I/O 
			       ; / queue entry
        jz      short poke_5
		; beq 2f / branch to 2f if both are clear
	; 31/07/2013
	;test	ah, 0B0h ; (*)
	;;test	word [ebx], 0B000h ; 1011000000000000b
		; bit $130000,(r1) / test bits 12, 13, and 15
        ;jnz	short poke_5 ; 31/07/2013 (*)
		; bne 2f / branch if any are set
	;movzx	ecx, byte [ebx] ; 09/06/2015 ; Device Id
    		; movb (r1),r3 / get device id
	movzx	ecx, al ; 18/08/2015
	;mov	edi, ecx ; 26/04/2013
	xor 	eax, eax ; 0
	;cmp 	[edi+drv.error], al ; 0
		; tstb deverr(r3) / test for errors on this device
       	;jna	short poke_2 
		; beq 3f / branch if no errors
	; 02/07/2015
	;dec	eax
	;mov	[ebx+4], ax ; 0FFFFFFFFh ; -1 
       		; mov $-1,2(r1) / destroy associativity
	;shr	eax, 24
	;mov	[ebx], eax ; 000000FFh, reset
		; clrb 1(r1) / do not do I/O
	;jmp     short poke_5
        ;       ; br 2f
                ; rts r0
poke_2: ; 3:
	; 02/07/2015
	inc	cl ; 0FFh -> 0
	jz	short poke_5
	inc	al ; mov ax, 1
	dec	cl
	jz	short poke_3
	; 26/04/2013 Modification
	;inc	al ; mov ax, 1
	;or	cl, cl ; Retro UNIX 8086 v1 device id.
	;jz	short poke_3 ; cl = 0
	shl	al, cl ; shl ax, cl
poke_3:
	;test	[active], ax
	test	[active], al
		; bit $2,active / test disk busy bit
	jnz     short poke_5
		; bne 2f / branch if bit is set
	;or	[active], ax
	or	[active], al
		; bis $2,active / set disk busy bit
	;push	ax
	; 04/02/2022
	push	eax
	call	diskio ; Retro UNIX 8086 v1 Only !
	;mov    [edi+drv.error], ah
	; 04/02/2022
	pop	eax
	;pop	ax
	jnc	short poke_4 ; 20/07/2013
	;cmp 	[edi+drv.error], al ; 0	
	;jna	short poke_4
		; tstb deverr(r3) / test for errors on this device
       		; beq 3f / branch if no errors
	; 02/07/2015 (32 bit modification)
	; 20/07/2013
	mov	dword [ebx+4], 0FFFFFFFFh ; -1 
       		; mov $-1,2(r1) / destroy associativity
	mov	word [ebx], 0FFh ; 20/08/2015 
		; clrb 1(r1) / do not do I/O
	jmp     short poke_5
poke_4:	; 20/07/2013
	; 17/07/2013
	not 	al 
	and	[active], al ; reset, not busy
	; eBX = system I/O buffer header (queue entry) address
seta: ; / I/O queue bookkeeping; set read/write waiting bits.
	mov	ax, [ebx]
       		; mov (r1),r3 / move word 1 of I/O queue entry into r3
        and	ax, 600h
		; bic $!3000,r3 / clear all bits except 9 and 10
	and 	word [ebx], 0F9FFh
       		; bic $3000,(r1) / clear only bits 9 and 10
	shl	ah, 3
       		; rol r3
                ; rol r3
                ; rol r3	
	or	[ebx], ax
		; bis r3,(r1) / or old value of bits 9 and 10 with 
			   ; bits 12 and 13
	call	idle ; 18/01/2014
	;; sti
	;hlt 	; wait for a hardware interrupt
	;; cli
	; NOTE: In fact, disk controller's 'disk I/O completed' 
        ; interrupt would be used to reset busy bits, but INT 13h
	; returns when disk I/O is completed. So, here, as temporary
	; method, this procedure will wait for a time according to
	; multi tasking and time sharing concept.
	;
	; 24/10/2015
	;not	ax 
	mov 	ax, 0FFh ; 24/10/2015 (temporary)
	and	[ebx], ax ; clear bits 12 and 13
poke_5: ;2:
        cmp     esi, bufp
               ; cmp r2,$bufp / test to see if entire I/O queue 
                            ; / has been scanned
	ja      short poke_1
               ; bhi 1b
	; 24/03/2013
       		; mov (sp)+,r3
       		; mov (sp)+,r2
       		; mov (sp)+,r1
        pop 	eax  ; Physical Block Number (r1) (mget)
	pop 	ebx
	; 02/07/2015 (32 bit modification)
	; 20/07/2013
	;cmp 	dword [ebx+4], 0FFFFFFFFh
	cmp	byte [ebx], 0FFh ; 20/08/2015
	;	
	; 'poke' returns with cf=0 if the requested buffer is read 
	; or written succesfully; even if an error occurs while
	; reading to or writing from other buffers. 20/07/2013
	;
	; 09/06/2015
	cmc
	retn
                ; rts r0
%endif

bufaloc:
	; 15/07/2022
	; 14/07/2022
	;	Retro UNIX 386 v1 (Kernel v0.2.0.22)
	; 04/02/2022
	; 20/08/2015
	; 19/08/2015
	; 02/07/2015
	; 11/06/2015 (Retro UNIX 386 v1 - Beginning)
	;	     (32 bit modifications)	
	; 13/03/2013 - 29/07/2013 (Retro UNIX 8086 v1)
	;
	; bufaloc - Block device I/O buffer allocation
	; 
	; INPUTS ->
	;    r1 - block number
	;    cdev - current (block/disk) device number
	;    bufp+(2*n)-2 --- n = 1 ... nbuff
	; OUTPUTS ->
	;    r5 - pointer to buffer allocated
	;    bufp ... bufp+12 --- (bufp), (bufp)+2
	;
	; ((AX = R1)) input/output
	; ((BX = R5)) output
        ;    ((Modified registers: DX, CX, BX, SI, DI, BP))
	;    zf=1 -> block already in a I/O buffer
	;    zf=0 -> a new I/O buffer has been allocated
	;    ((DL = Device ID))
	;    (((DH = 0 or 1)))
	;    (((CX = previous value of word ptr [bufp])))
	;    ((CX and DH will not be used after return)))

	; 14/07/2022
	;	Modified registers: EBX, ECX, EDX, ESI, EDI

	;;push 	esi ; ***
		; mov r2,-(sp) / save r2 on stack
       		; mov $340,*$ps / set processor priority to 7
		; 20/07/2013
	; 26/04/2013
	;movzx	ebx, byte [cdev] ; 0 or 1
	;mov	edi, rdev  ; offset mdev = offset rdev + 1
	;add	edi, ebx
	; 09/01/2022
	movzx	edi, byte [cdev] ; 0 or 1
	add	edi, rdev
bufaloc_0: ; 26/04/2013 !! here is called from bread or bwrite !!
			;; EDI points to device id.
	movzx	ebx, byte [edi] ; [EDI] -> rdev/mdev or brwdev
	; 11/06/2015
	cmp 	byte [ebx+drv.status], 0F0h ; Drive not ready !
	jb	short bufaloc_9
	mov	dword [u.error], ERR_DRV_NOT_RDY
	jmp	error
bufaloc_9:
	mov	edx, ebx ; dh = 0, dl = device number (0 to 5)
bufaloc_10: ; 02/07/2015
	; 14/07/2022
	;xor 	ebp, ebp ; 0
	;push	ebp ; 0
        ;mov	ebp, esp	
	xor	edi, edi ; 0	
	push	edi
bufaloc_1: ;1:
		; clr -(sp) / vacant buffer
        mov 	esi, bufp
		; mov $bufp,r2 / bufp contains pointers to I/O queue 
			     ; / entrys in buffer area
bufaloc_2: ;2:
	mov	ebx, [esi]
       		; mov (r2)+,r5 / move pointer to word 1 of an I/O 
			    ; queue entry into r5
	; 14/07/2022
	mov	ecx, [ebx]	
	;test	word [ebx], 0F600h
	; 15/07/2022
	test	ch, 1 ; valid buffer (content) ?
		; bit $173000,(r5) / lock+keep+active+outstanding
        jnz	short bufaloc_3 ; yes
		; bne 3f / branch when 
		       ; / any of bits 9,10,12,13,14,15 are set
                       ; / (i.e., buffer busy)
	;mov    [ebp], esi ; pointer to I/0 queue entry
	; 14/07/2022
	; save free buffer pointer 
	mov	[esp], esi
                ; mov r2,(sp) ;/ save pointer to last non-busy buffer
			; / found points to word 2 of I/O queue entry)
	; continue to see if requested sector/block buffer
	;	already is one of existing (valid) buffers
	;jmp	short bufaloc_4
	jmp	short bufaloc_11 ; 14/07/2022
bufaloc_3: ;3:
	;mov	dl, [edi] ; 26/04/2013
	;
	cmp	cl, dl ; 14/07/2022
	;cmp	[ebx], dl	
		; cmpb (r5),cdev / is device in I/O queue entry same 
			       ; / as current device
	jne	short bufaloc_4
       		; bne 3f
	cmp	[ebx+4], eax
       		; cmp 2(r5),r1 / is block number in I/O queue entry, 
			     ; / same as current block number
       	jne	short bufaloc_4
		; bne 3f
	;add	esp, 4
	pop	ecx
       		; tst (sp)+ / bump stack pointer
	jmp	short bufaloc_7 ; Retro Unix 8086 v1 modification
				; jump to bufaloc_6 in original Unix v1
       		; br 1f / use this buffer
bufaloc_4: ;3:
	; 14/07/2022
	; save last valid buffer 
	; (will be used if there is not a free buffer)
	mov	edi, esi	
bufaloc_11:
	add	esi, 4 ; 20/08/2015
	;
	cmp	esi, bufp + (nbuf*4)
		; cmp r2,$bufp+nbuf+nbuf
	jb	short bufaloc_2
		; blo 2b / go to 2b if r2 less than bufp+nbuf+nbuf (all
                       ; / buffers not checked)
        pop	esi
		; mov (sp)+,r2 / once all bufs are examined move pointer
			     ; / to last free block
       	or	esi, esi 
	jnz	short bufaloc_5
		; bne 2f / if (sp) is non zero, i.e., 
	        ; / if a free buffer is found branch to 2f
	
	; 14/07/2022        
	; if there is not a free buffer
	; we can use last valid buffer (the oldest buffer)
	; ((ptr to new buffer is located at head of buff ptr chain))
	mov	esi, edi

	;;mov  ecx, [s.wait_]
	;call	idle
	;	; jsr r0,idle; s.wait+2 / idle if no free buffers
	;jmp 	short bufaloc_10 ; 02/07/2015
       	;	; br 1b
bufaloc_5: ;2:
		; tst (r0)+ / skip if warmed over buffer
	; 14/07/2022
	;inc	dh ; Retro UNIX 8086 v1 modification
bufaloc_6: ;1:
        mov    	ebx, [esi] 
		; mov -(r2),r5 / put pointer to word 1 of I/O queue 
			     ; / entry in r5
	;; 26/04/2013
        ;;mov	dl, [edi] ; byte [rdev] or byte [mdev]
	;mov 	[ebx], dl
		; movb cdev,(r5) / put current device number 
				 ; / in I/O queue entry
	; 14/07/2022
	; invalidate buffer before r/w (new) disk sector/block
	;mov	[ebx+1], dh ; 0
	; 14/07/2022
	mov	[ebx], edx  ; dh = 0
	
	mov 	[ebx+4], eax
		; mov r1,2(r5) / move block number into word 2 
			     ; / of I/O queue entry
	; 14/07/2022
	inc	dh ; dh = 1
bufaloc_7: ;1:
        cmp	esi, bufp
		; cmp r2,$bufp / bump all entrys in bufp 
			     ; / and put latest assigned
	jna	short bufaloc_8	
       		; blos 1f / buffer on the top 
			; / (this makes if the lowest priority)
	; 14/07/2022
	mov	edi, esi
	sub	esi, 4
	mov	ecx, [esi]
	mov	[edi], ecx ; 14/07/2022
	;mov	[esi+4], ecx
		; mov -(r2),2(r2) / job for a particular device
	jmp 	short bufaloc_7        
		; br 1b
bufaloc_8: ;1:
        mov	[esi], ebx
		; mov r5,(r2)
	;;pop	esi ; ***
       		; mov (sp)+,r2 / restore r2
       	or 	dh, dh ; 0 or 1 ?
		; Retro UNIX 8086 v1 modification
		; zf=1 --> block already is in an I/O buffer
		; zf=0 --> a new I/O buffer has been allocated
	retn
		; rts r0

diskio:
	; 12/07/2022 - Retro UNIX 386 v1 (Kernel v0.2.0.21)
	; ((Ref: Retro UNIX 386 v1.1 'diskio' modification: 12/07/2022))
	; 
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;	((simplified and shortened 'diskio.s' code)
	;
	; 12/02/2022
	; 04/02/2022 (Retro UNIX 386 v1 2022, Kernel v0.2.0.18)
	; 10/07/2015
	; 02/07/2015
	; 16/06/2015
	; 11/06/2015 (Retro UNIX 386 v1 - Beginning)
	;	     (80386 protected mode modifications)	
	; 15/03/2013 - 29/04/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 feature only !
	;
	; Derived from proc_chs_read procedure of TRDOS DISKIO.ASM (2011)
	; 04/07/2009 - 20/07/2011
	;
	; NOTE: Reads only 1 block/sector (sector/block size is 512 bytes)
	;
        ; INPUTS ->
	; 	   EBX = System I/O Buffer header address
	;
        ; OUTPUTS -> cf=0 --> done 
	; 	     cf=1 --> error code in AH
	;
	; (Modified registers: EAX, ECX, EDX)
	
;rw_disk_sector:
	; 12/07/2022 - Retro UNIX 386 v1 (Kernel v0.2.0.21)
	; 12/02/2022
	; 10/07/2015
	; 02/07/2015
	; 11/06/2015 - Retro UNIX 386 v1 - 'u8.s' 
	; 21/02/2015 ('dsectpm.s', 'read_disk_sector')
	; 16/02/2015 (Retro UNIX 386 v1 test - 'unix386.s')
	; 01/12/2014 - 18/01/2015 ('dsectrm2.s')
	;
	;;mov	dx, 0201h ; Read 1 sector/block
	;mov	dh, 2
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	mov	dh, 2 ; Write 1 sector/block
	mov	ax, [ebx]
	;
	push	esi ; ****
	push	ebx ; ***
	;
	movzx	ecx, al
	mov	esi, ecx
	;
	cmp	cl, dh ; 2
	jb	short rwdsk0
	add	al, 7Eh  ; 80h, 81h, 82h, 83h
rwdsk0:
	; 12/07/2022
	;mov	[drv], al
	add	esi, drv.status
	; 11/06/2015
	cmp	byte [esi], 0F0h 
	jb      short rwdsk1
	; 'drive not ready' error
	; 12/02/2022
	;mov	byte [u.brwdev], 0FFh ; error code in [u.error]
	;mov	dword [u.error], ERR_DRV_NOT_RDY
	;;jmp	error
	;stc	; cf = 1
	;retn
	; 12/02/2022
	mov	cl, ERR_DRV_NOT_RDY
	jmp	short rwdsk_err1
rwdsk1:
	;test	ah, 2
	;;test	ax, 200h ; Bit 9 of word 0 (status word)
	;		 ; write bit
	;jz	short rwdsk2
	
	;;test	ah, 4
	;;;test	ax, 400h ; Bit 10 of word 0 (status word)
	;;		 ; read bit
	;;jz	short diskio_ret

	;inc	dh ; 03h = write

	; 08/07/2022
	test	ah, dh ; test ah, 2
	jnz	short rwdsk2 ; dh = 02h = write
	dec	dh
		; dh = 01h = read
rwdsk2:
	mov	dl, al
	add	ebx, 4 ; sector/block address/number pointer
	mov	eax, [ebx] ; sector/block number (LBA)
	shl	cl, 2
	add	ecx, drv.size ; disk size
	cmp	eax, [ecx] ; Last sector + 1 (number of secs.)
	jb      short rwdsk3
 	; 'out of volume' error
	; 12/02/2022
	;mov	byte [u.brwdev], 0FFh ; error code in [u.error]
	;mov 	dword [u.error], ERR_DEV_VOL_SIZE
	;;jmp	error
	;stc	; cf = 1
	;retn
	; 12/02/2022
	mov	ecx, ERR_DEV_VOL_SIZE
rwdsk_err1:
	mov	byte [u.brwdev], 0FFh
	mov 	[u.error], ecx ; 12/02/2022
	jmp	short rwdsk_err2
rwdsk3:
	; 11/06/2015
	add	ebx, 4 ; buffer address
	mov	byte [retry_count], 4
	; 12/07/2022
	;test	byte [esi], 1 ; LBA ready ?
        ;jz	short rwdsk_chs
rwdsk_lba:
	; LBA read/write (with private LBA function) 
	;((Retro UNIX 386 v1 - DISK I/O code by Erdogan Tan))
        add     esi, drv.error - drv.status ; 10/07/2015
	mov	ecx, eax ; sector number
	; ebx = buffer (data) address
	; dl = physical drive number (0,1, 80h, 81h, 82h, 83h)
rwdsk_lba_retry:
	;mov	dl, [drv]
		; Function 1Bh = LBA read, 1Ch = LBA write
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;;mov	ah, 1Ch - 03h ; LBA write function number - 3
	;mov	ah, 2 ; LBA write function number - 2
	;add	ah, dh		
	;mov	al, 1
	;int	13h
	;call	int13h
	; 12/07/2022
	mov	al, dh	; function (1 or 2)
			; (1 = read, 2 = write )
	; ecx = disk sector address (LBA)
	; ebx = buffer address
	;  dl = (physical) disk number
	;  al = function (r/w)
	call	DISK_IO

	mov	[esi], ah ; error code ; 10/07/2015
	jnc	short rwdsk_lba_ok
	cmp	ah, 80h ; time out ?
        je      short rwdsk_lba_fails
	dec	byte [retry_count]
        jnz     short rwdsk_lba_reset ; 10/07/2015
rwdsk_err2:	; 12/02/2022
rwdsk_lba_fails:
	stc
rwdsk_lba_ok:
	pop	ebx ; ***
	pop	esi ; ****
	retn
rwdsk_lba_reset:
	;mov	ah, 0Dh ; Alternate reset
	;int	13h
	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
	;mov	ah, 5 ; Alternate reset	 
	;call	int13h
	;jnc	short rwdsk_lba_retry
	; 12/07/2022
	xor	al, al ; 0 ; reset
	call	DISK_IO
	jnc	short rwdsk_lba_retry

	mov	[esi], ah ; error code ; 10/07/2015
	jmp	short rwdsk_lba_ok

	; 12/07/2022 
	;	(CHS Read/Write setup is not needed here.)
	; 	(LBA address will be converted to CHS parameters
	;	in 'DISK_IO' procedure when/if it is required.)
	
;	; CHS read (convert LBA address to CHS values)	
;rwdsk_chs:
;	; 10/07/2015
;	sub	esi, drv.status
;	mov	ecx, esi
;	add 	esi, drv.error
;	; 02/07/2015
;	; 16/06/2015
;	; 11/06/2015 
;	push	ebx ; ** ; buffer
;	shl	ecx, 1
;	push	ecx ; * 
;	;
;	mov	ebx, ecx
;	mov	[rwdsk], dh ; 02/07/2015
;	xor	edx, edx ; 0
;	; 04/02/2022
;	;sub	ecx, ecx 
;	add     ebx, drv.spt
;	mov	cx, [ebx] ; sector per track
;		; EDX:EAX = LBA
;	div	ecx
;	mov	cl, dl	; sector number - 1
;	inc	cl	; sector number (1 based)
;	pop	ebx ; * ; 11/06/2015
;	;push	cx
;	; 04/02/2022
;	push	ecx
;	add     ebx, drv.heads
;	mov	cx, [ebx] ; heads
;	xor	edx, edx
;		; EAX = cylinders * heads + head
;	div	ecx
;	;pop	cx     ; sector number
;	; 04/02/2022
;	pop	ecx
;	mov	dh, dl ; head number
;	mov	dl, [drv]
;	mov	ch, al ; cylinder (bits 0-7)
;	shl	ah, 6
;	or	cl, ah ; cylinder (bits 8-9)
;		       ; sector (bits 0-7)
;	pop	ebx ; ** ; buffer ; 11/06/2015
;		; CL = sector (bits 0-5)
;		;      cylinder (bits 8-9 -> bits 6-7)
;		; CH = cylinder (bits 0-7)
;		; DH = head
;		; DL = drive
;	;
;	mov	byte [retry_count], 4
;rwdsk_retry:	
;	;mov	ah, [rwdsk] ; 02h = read, 03h = write
;	; 08/07/2022
;	mov	ah, [rwdsk] ; 01h = read, 02h = write
;	mov	al, 1 ; sector count	
;	;int	13h
;	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;	call	int13h
;	mov	[esi], ah ; error code ; 10/07/2015
;	jnc	short rwdsk_ok ; ah = 0
;	cmp	ah, 80h ; time out ?
;	je	short rwdsk_fails
;	dec	byte [retry_count]
;	jnz	short rwdsk_reset
;rwdsk_fails:
;	stc
;rwdsk_ok:
;	pop	ebx ; ***
;	pop	esi ; ****
;	retn
;rwdsk_reset:
;	; 02/02/2015
;	sub	ah, ah
;	cmp	dl, 80h
;	jb	short rwdsk_fd_reset
;	;mov	ah, 0Dh ; Alternate reset
;	; 08/07/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.5)
;	mov	ah, 5  ; Alternate reset
;rwdsk_fd_reset:
;	;int	13h
;	call	int13h
;	jnc	short rwdsk_retry
;	mov	[esi], ah ; error code ; 10/07/2015
;	jmp 	short rwdsk_ok

; Original UNIX v1 - drum (& disk) interrupt routine
;	(Equivalent to IRQ 14 & IRQ 15 disk/hardware interrupts)
;
; This feature is not used in Retro UNIX 386 (& 8086) for now.
; Because, current Retro UNIX 386 disk I/O -INT13H- routine is 
; derived from IBM PC AT -infact: XT286- BIOS source code, int 13h
; that uses hardware -transfer has been completed-  interrupt inside it. 
; In a next Retro UNIX 386 version, these interrupts
; (fdc_int, hdc1_int, hdc2_int) will be handled by a separate routine
; as in original unix v1.
; I am not removing IBM BIOS source code derivatives -compatible code-
; for now, regarding the new/next 32 bit TRDOS project by me
; (to keep source code files easy adaptable to 32 bit TRDOS.)
;
; Erdogan tan (10/07/2015) 

;drum: / interrupt handler
;       jsr     r0,setisp / save r1,r2,r3, and clockp on the stack
;       jsr     r0,trapt; dcs; rfap; 1 / check for stray interrupt or
;                                      / error
;               br 3f / no, error
;       br      2f / error
;
;disk:
;       jsr     r0,setisp / save r1,r2,r3, and clockp on the stack
;       jmp     *$0f
;0:
;       jsr     r0,trapt; rkcs; rkap; 2
;      	        br 3f / no, errors
;       mov     $115,(r2) / drive reset, errbit was set
;       mov     $1f,0b-2 / next time jmp *$0f is executed jmp will be
;                        / to 1f
;       br      4f
;1:
;       bit     $20000,rkcs
;       beq     4f / wait for seek complete
;       mov     $0b,0b-2
;       mov     rkap,r1
;2:
;       bit     $3000,(r1) / are bits 9 or 10 set in the 1st word of
;                          / the disk buffer
;       bne     3f / no, branch ignore error if outstanding
;       inc     r1
;       asr     (r1)
;       asr     (r1)
;       asr     (r1) / reissue request
;       dec     r1
;3:
;       bic     $30000,(r1) / clear bits 12 and 13 in 1st word of buffer
;       mov     ac,-(sp)
;       mov     mq,-(sp) / put these on the stack
;       mov     sc,-(sp)
;       jsr     r0,poke
;       mov     (sp)+,sc
;       mov     (sp)+,mq / pop them off stack
;       mov     (sp)+,ac
;4:
;       jmp     retisp / u4-3
;
;trapt:                  / r2 points to the
;       mov     (r0)+,r2 / device control register
;       mov     *(r0)+,r1 / transaction pointer points to buffer
;       tst     (sp)+
;       tstb    (r2) / is ready bit of dcs set?
;       bge     4b / device still active so branch
;       bit     (r0),active / was device busy?
;       beq     4b / no, stray interrupt
;       bic     (r0)+,active / yes, set active to zero
;       tst     (r2) / test the err(bit is) of dcs
;       bge     2f / if no error jump to 2f
;       tst     (r0)+ / skip on error
; 2:
;       jmp     (r0)
