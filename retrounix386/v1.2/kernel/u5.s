; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2) - SYS5.INC
; Last Modification: 17/07/2022 (Retro UNIX 386 v1.2, Kernel v0.2.2.3)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U5.AS M (07/08/2013) //// UNIX v1 -> u5.s
;
; ****************************************************************************

	; 09/03/2022
	; 09/01/2022
	; 22/11/2021 - Retro UNIX 386 v2 fs compatibility modification
mget_w:
	; 28/10/2021
	; 22/08/2021
	; 20/08/2021
	; 25/05/2020
	; 24/05/2020 - Retro UNIX 386 v2	
	mov	byte [mget_rw], 1 ; write access
	jmp	short mget
mget_r:
	; 28/10/2021
	; 22/08/2021
	; 25/05/2020
	; 24/05/2020 - Retro UNIX 386 v2
	mov	byte [mget_rw], 0 ; read access	
mget:
	; 17/07/2022
	; 09/03/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 28/11/2021
	; 22/11/2021
	; 28/10/2021 - temporary (simplified code)
	; 22/08/2021
	; 20/08/2021
	; 25/05/2020
	; 24/05/2020
	; 05/05/2020
	; 02/05/2020, 03/05/2020
	; 29/04/2020 - Retro UNIX 386 v2
	; New inode model/format (Modified UNIX v7 inode model)
	;	
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 22/03/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; Get existing or (allocate) a new disk block for file
	; 
	; INPUTS ->
	;    u.fofp (file offset pointer)
	;    inode 
	;    u.off (file offset)
	; OUTPUTS ->
	;    r1 (physical block number)
	;    r2, r3, r5 (internal)
	;
	; ((AX = R1)) output
	;    (Retro UNIX Prototype : 05/03/2013 - 14/11/2012, UNIXCOPY.ASM)
	;
	; Modified registers: eax, edx, ebx, ecx, esi, edi, ebp

	; Retro UNIX 386 v2 'mget' procedure
	; source code reference:
	; 'mget' procedure in UNIXHDCP.ASM (25/01/2020) by Erdogan Tan

	; Get sector/block address for current file pointer
	; (If file pointer points to a number greater than current file
	; size, a new -empty- sector/block will be created/written.)

	; Note: 'mget' procedure will be used for only RUNIX v2 FS.
	;	(installable file system drivers will not use 'mget')
	;	 !!! 512 bytes per sector !!!  ; 02/05/2020

	; ! 64 bit fofp ! but Retro UNIX 386 v2 kernel will use 32 bit
	; file offset for regular files and directories.
	; ('mget' procedure will be called for regular files & directories)

	; 28/10/2021
	; Note: 'mget' uses only current inode ([ii], 'inode:')
	; (inode must be loaded at 'inode:' addr by 'iget' before 'mget')  

		; mov *u.fofp,mq / file offset in mq
		; clr ac / later to be high sig
		; mov $-8,lsh   / divide ac/mq by 256.
		; mov mq,r2
		; bit $10000,i.flgs / lg/sm is this a large or small file
		; bne 4f / branch for large file

	; 09/01/2022
	; Return: eax = physical block/sector number
mget_0:	
        mov     esi, [u.fofp]
	mov	ebx, [esi]
        ; ebx = file offset

	; 20/08/2021
	shr	ebx, 9 ; / 512 (to convert byte offset to sector offset)

	test	byte [i.flgs+1], 10h ; is this a large or small file addr?
	jz	short mget_1 ; small file addressing
	jmp	mget_5	; large file addressing
mget_1:
	; If large file flag is clear -not set- RUNIX v2 FS inode
	; will contain 10 direct disk block/sector dword pointers.

	; 05/05/2020
	;shr	ebx, 9 ; / 512 (to convert byte offset to sector offset)

	cmp	ebx, 10	; block 10
	jnb	short mget_3 ; file offset >= 5120

	; 05/05/2020
	;cmp	ebx, 5120
	;jnb	short mget_3 ; file offset >= 5120

	;mov	bl, bh
	;and	bl, 1Eh ; clear all bits but bits 1,2,3,4 ; 0,2,..,18
	;	; max. value = 12h = 10010b = 18 (because ebx <= 5119) 
	;shl	bl, 1	; * 2 ; max. value = 24h = 100100b = 36
	;xor	bh, bh  ; clear bh
		; RUNIX v2 (RUFS 2) file system bytes per sector value
		; is always 512.
		; ebx = 4*(ebx/512)  = 0 to 36

	; 05/05/2020
	shl	bl, 2 ; * 4
		 ; ebx = 0 to 36

	mov	eax, [ebx+i.dskp] ; disk sector addr ptr 0 to 9 for file
	; 22/08/2021
	mov	esi, ebx
	;xor	ebx, ebx ; 0
	; 09/03/2022
	;; 09/01/2022
	;xor	bl, bl

	or	eax, eax
	jnz	short mget_2 ; existing block/sector (logical)

	push	esi ; * ; 22/08/2021
	call	alloc_m ; 24/05/2020	
	;call	alloc	; allocate a new sector/block for this file	
			; eax = sector/block number
			; ebx = buffer header address
	pop	esi ; * ; 22/08/2021
	; 28/11/2021
	;jc	short mget_2 
			; cf = 1 -> eax = 'no free block' error
	; 22/08/2021
	mov	[esi+i.dskp], eax  ; logical sector/block address

	; 09/01/2022
	call	setimod	; set inode modification flag
			; and set modification time

	; eax = logical sector/block number
	; ([idev] = logical drive number)
	; 28/10/2021
	; [cdev] = logical drive number (0 or 1)
	;	(0 = root fs, 1 = mounted fs)
mget_2:
	; 09/01/2022
	; eax = logical sector/block number
	test	byte [cdev], 1
	jnz	short mget_14
	; convert to physical sector/block number
	add	eax, [systm+SB.BootSectAddr]
	retn
	; 09/01/2022
mget_14:
	; convert to physical sector/block number
	add	eax, [mount+SB.BootSectAddr]
	retn
	
		; rts r0
mget_3: ; 3: / adding on block which changes small file to a large file
	; 22/11/2021
	call	alloc_m ; 24/05/2020
	;call	alloc	; allocate a new sector/block for this file
			; eax = sector/block number
			; ebx = buffer header  ; 24/05/2020
	; 28/11/2021
	;jc	short mget_2 ; error code in eax

	; 09/01/2022
	; convert to physical block/sector number
	push	eax ; *!* ; logical sector/block number 
	call	mget_2

        ; EAX (r1) = Physical block (sector) number
	call 	wslot
		; jsr r0,wslot / set up I/O buffer for write, r5 points to 
			     ; / first data word in buffer
        ; EAX (r1) = Physical block number
	; 28/11/2021
	; ebx = buffer (data) address
	; 22/11/2021
	xor	ecx, ecx
	mov 	cl, 10	; r3, transfer old physical block pointers
			; into new indirect block area for the new
			; large file		
	mov 	edi, ebx ; r5
	mov	esi, i.dskp  ; current inode's direct disk addr ptrs
	mov	edx, eax
	xor	eax, eax
mget_4:
	movsd 	
	mov	[esi-4], eax ; 0
	loop	mget_4

	; 22/11/2021
	mov 	cl, 128-10 ; clear rest of data buffer
;mget_4: ; 1
	rep 	stosd
		; clr (r5)+
		; dec r3
		; bgt 1b
	mov	eax, edx
        ; EAX (r1) = Physical block number
	call	dskwr
		; jsr r0,dskwr / write new indirect block on disk

	;; EAX (r1) = Physical block number

	; 09/01/2022        
	pop	dword [i.dskp] ; *!* ; logical sector/block number
 
	; eax = logical disk sector/block number/addr ; 09/01/2022
	;mov 	[i.dskp], eax ; 22/11/2021 
		; mov r1,i.dskp / put pointer to indirect block in i-node

	or	byte [i.flgs+1], 10000b ; 10h ; 16  ; large file flag
		; bis $10000,i.flgs / set large file bit 
				  ; / in i.flgs word of i-node
	call	setimod
		; jsr r0,setimod / set i-node modified flag

	jmp	mget_0 ; 09/01/2022
		; br mget

mget_5:  ; 4 ; large file
	; 13/11/2019 (UNIXHDCP.ASM)
	;
	; Retro UNIX 386 v2 disk inode contains..
	; (if large file flag is set)
	; 8 indirect disk block/sector dword pointers
	; +1 double indirect disk block/sector dword pointers
	; +1 triple indirect disk block/sector dword pointers

	; check indirect pointers limit (as file offset)
	; 8*128 = 1024 blocks (or sectors) or 512 KB
	; check dx (file offset hw) value
	
	; 03/05/2020
	;cmp	ebx, 524288  ; is file offset >= 524288 ?
	;jnb	short mget_6 ; yes, check double indirect limit

	; 05/05/2020
	cmp	ebx, 1024    ; block 1024 (byte 524288)	
	jnb	short mget_6 ; check double indirect limit

	mov	byte [level], 1 ; levels, 1 = indirect blocks

	; 05/05/2020
	;shr	ebx, 9	; ebx = sector offset (flat)

	mov	cl, bl
	and	cl, 127
	mov	[level+1], cl ; level 1, direct block ptr index (0 to 127)
	;shr	bx, 7 ; bl = level 0, indirect block pointer index (0 to 7)
	; 17/07/2022
	shr	ebx, 7
	;mov	[level+2], bl

	shl	bl, 2 ; * 4 to convert index number to offset

	;mov	esi, i.dskp
	;add	esi, ebx

	lea	esi, [ebx+i.dskp]

	jmp	short mget_8

mget_6:
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	; 03/05/2020
	; 13/11/2019 (UNIXHDCP.ASM)
	; check double indirect pointer limit (as file offset)
	; (128*128)+1024 = 16384+1024 blocks or 8 MB + 512 KB
	; check dx (file offset hw) value
	
	;cmp	ebx, 8912896 ; >= 17408 sectors (16384+1024) ?	
	;jnb	short mget_7 ; yes, use triple indirect pointer (block)

	; 05/05/2020
	cmp	ebx, 17408   ; block 17408 (byte 8912896)	
	jnb	short mget_7 ; use triple indirect pointer (block)

	mov	byte [level], 2  ; levels, 2 = double indirect blocks
	
	;sub	ebx, 524288 ; 1024 sectors
	;shr	ebx, 9	; ebx = sector offset (flat)
			; (from sector 1024)
	; 05/05/2020
	sub	ebx, 1024 ; offset from sector 1024
	
	mov	cl, bl
	and	cl, 127
	mov	[level+1], cl
			; level 2, direct block ptr index (0 to 127)	
	;shr	bx, 7
	; 17/07/2022
	shr	ebx, 7
	mov	[level+2], bl
			; level 1, indirect block ptr index (0 to 127)

	mov	esi, i.dskp + 32 ; 8*4
			; level 0, double indirect block pointer
	jmp	short mget_8

mget_7:
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	; 03/05/2020
	; 13/11/2019 (UNIXHDCP.ASM)
	; triple indirect pointer ; 8912896 to 1082654720 bytes
	;mov	esi, i.dskp + 36 ; 9*4

	mov	byte [level], 3  ; levels, 3 = triple indirect blocks

	;sub	ebx, 8912896 ; 17408 sectors (16384+1024)
	;shr	ebx, 9	; ebx = sector offset (flat)
			; (from sector 17408)
	; 05/05/2020
	sub	ebx, 17408 ; offset from sector 17408	

	mov	cl, bl
	and	cl, 127
	mov	[level+1], cl  
			; level 3, direct block ptr index (0 to 127)	
	shr	ebx, 7	
	mov	cl, bl
	and	cl, 127
	mov	[level+2], cl
			; level 2, indirect block ptr index (0 to 127)
	;shr	bx, 7
	; 17/07/2022
	shr	ebx, 7
	mov	[level+3], bl
			; level 1, double indir blk ptr index (0 to 127)

	mov	esi, i.dskp + 36 ; 9*4
			; level 0, triple indirect block pointer
mget_8:
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	mov	eax, [esi]
	or 	eax, eax ; R1
	jnz 	short mget_10 ; 2f
		; bne 2f / if no indirect block exists
	push	esi	; * ; 24/05/2020

	call	alloc_m ; 24/05/2020
	;call	alloc	; allocate a new block for this file	
			; eax = block number
			; ebx = buffer header address
			; [pdn] = physical drive number ; 25/05/2020
	;jc	mget_2	; cf -> 1 & eax = 0 -> no free block

	pop	esi	; * ; 24/05/2020
	;jc	short mget_9

	; 09/01/2022
	; eax = logical sector/block number

	mov	[esi], eax ; record sector/block address on i.dskp area

	call 	setimod
		; jsr r0,setimod / set i-node modified byte
	; EAX = new block number (logical)

	; 09/01/2022
	; convert to physical block/sector number
	call	mget_2
	;
	call 	clear
		; jsr r0,clear / clear new block
	; 09/03/2022
	;add	ebx, 8
	; ebx = buffer data address
	; 22/11/2021
	jmp	short mget_11
;mget_9:
;	pop	edx ; *  ; 09/01/2022
;	retn
mget_10: ;2
	; 09/01/2022
	; eax = logical sector/block number/address
	; convert to physical sector/block number/address
	call	mget_2	
	; 05/03/2013
	; EAX = r1, physical block number (of indirect block)
	call 	dskrd ; read indirect block
		; jsr r0,dskrd / read in indirect block
	;jc	short mget_9
mget_11:
	; 22/11/2021
	; eax = physical block/sector address
	; ebx = buffer (data) address ; 09/03/2022

	mov	edx, eax ; *  ; save physical sector number in edx

	movzx	eax, byte [level]
	mov	al, [eax+level] ; get sector pointer offset
	shl	eax, 2 ; * 4 ; 09/01/2022
	;shl	ax, 2 ; * 4

	add	ebx, eax

	mov 	eax, [ebx] ; put logical block no of block
			   ; in file sought in R1 (EAX)
		; mov (r2),r1 / put physical block no of block in file
	               	    ; / sought in r1

	and	eax, eax  ; if logical sector/block number is zero
			  ; then we need a new block for file
	jz	short mget_12

	dec	byte [level]
	jnz	short mget_10

	; 09/03/2022
	;sub	ebx, ebx ; ebx = 0 ; existing sector

	;retn
	; 09/01/2022
	; eax = logical block/sector number
	jmp	mget_2

mget_12:
	; 22/11/2021
	push	edx ; *
	push	ebx ; ** ; buffer data position
	call	alloc_m
	;call	alloc	 ; allocate a new block for this file	
			 ; eax = block number (logical)
			 ; ebx = buffer header address
	pop	esi ; **
	; 09/01/2022
	;pop	edx ; *
	;jc	short mget_9 ; cf -> 1 & eax = 0 -> no free block

	mov	[esi], eax ; record/save block number (logical)

	; 22/11/2021
	xchg	eax, [esp] ; *

	; eax (r1) = physical block number (of indirect block)
	call 	wslot
		; jsr r0,wslot
        ; eax (r1) = physical block number
	; ebx (r5) = pointer to buffer (indirect block)
	call 	dskwr
	; eax = r1 = physical block number (of indirect block)
		; jsr r0,dskwr / write newly modified indirect block 
			     ; / back out on disk
	pop	eax ; *  ; 31/07/2013
		; mov (sp),r1 / restore block number of new block	

	; 09/01/2022
	; eax = logical sector/block number/address of new block
	; convert to physical sector/block number/address
	call	mget_2	

	call 	clear
		; jsr r0,clear / clear new block

	dec	byte [level]
	jnz	short mget_11 
		; ebx = buffer (data) address ; 09/03/2022

	; 09/01/2022
	; eax = physical sector/block number/address of new block
	
	; 22/11/2021
	; ebx = buffer address
mget_13: ; 2
	; eax (r1) = Block number of new block
	retn
		; rts r0

	; 09/01/2022
	; 26/11/2021
	; 22/11/2021 - Retro UNIX 386 v2 fs compatibility modification
alloc_m:
	; 27/11/2021
	; 28/10/2021
	; 07/05/2021
	; 26/05/2020
	; 25/05/2020
	; 24/05/2020 - Retro UNIX 386 v2
	; 'alloc' call in 'mget'
	;
	; (('mget_r' will be returned with error))
	; (('mget_w' will continue to 'alloc'))

	; Note: 'sysread' will return with cf = 0, eax = 0
	; if [fofp] >= file size (eax = 0 --> EOF)
	; So, if we are here, that means [fofp] < file size
	;     but disk address is 0. 
	
	cmp	byte [mget_rw], 1 ; write access ? (mget_w) 
	jnb	short alloc ; yes (this is a call from 'mget_w')
	; ((cf = 1))	
	; no (this is a call from 'mget_r') !
	;mov	eax, ERR_FILE_SIZE ; file size error (inode error!)
		; [User may change this empty/invalid inode (disk) sector
		; pointer with a new valid inode (disk) sector pointer 
		; by writing new clear (zero) sector to same file position
		; in order to correct file size -inode parm/content- error.
		; It may be useful for recovering lost data if other sector
		; pointers are not empty.]	
	;retn
	; 27/11/2021
	mov	dword [u.error], ERR_FILE_SIZE 
			; file size error (inode error!)
	jmp	error
alloc:
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 27/11/2021
	; 26/11/2021 - Major modification for Runix v2 file system
	;	(I have called this kernel version as v1.2, it is a
	;	 debug/test version just before Retro UNIX 386 v2 kernel)
	;	((there were running problems on new version, 
	;	  so i am developing an intermediate version with minimum
	;	  modification on v1.1 code which is successfuly running))
	;	 ; /// Erdogan Tan - Istanbul, 26/11/2021 /// 
	;	 
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 01/04/2013 - 01/08/2013 (Retro UNIX 8086 v1)
	;
	; get a free block and 
	; set the corresponding bit in the free storage map
	; 
	; INPUTS ->
	;    cdev (current device)
	;    r2 
	;    r3
	; OUTPUTS ->
	;    r1 (physical block number of block assigned)
	;    smod, mmod, systm (super block), mount (mountable super block)	
	;
	; ((AX = R1)) output
	;    (Retro UNIX Prototype : 14/11/2012 - 21/07/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: edx, ecx)) 

	; 26/11/2021
	; INPUT:
	;	[cdev] = current device (root = 0, mounted = 1) 
	; OUTPUT:
	;	eax = disk block/sector number/address (physical)
	;	(ebx = disk buffer address for sector/block in eax)
	;
	;    Note:	
	;	Free blocks map will be modified and written to it's disk.
	;	Superblock (buffer) of [cdev] will be modified and it's 
	;	modification flag will be set; but the superblock 
	;	will not be written to it's disk in this 'alloc' procedure)
	;
	; Modified registers: eax, ebx, ecx, edx, esi, edi, ebp

	; 09/01/2022
	; Return: eax = logical block/sector number 

	; 26/11/2021
	mov 	ebx, systm ; (SuperBlock of root file system)
		; mov $systm,r2 / start of inode and free storage map for drum
	test	byte [cdev], 1
		; tst cdev
	jz	short alloc_1
		; beq 1f / drum is device
	mov	ebx, mount ; (SuperBlock of mounted file system)
		; mov $mount,r2 / disk or tape is device, start of inode and
			      ; / free storage map
alloc_1: ; 1
	; 26/11/2021 - Retro UNIX 386 v2 file system compatibility code
	test	byte [ebx+SB.ReadOnly], 1 ; if bit 0 is 1, it is RO fs	
	jnz	short alloc_2  ; 'read only file system' error
	;
	mov	ecx, [ebx+SB.FreeBlocks] ; count of free blocks/sectors
	and	ecx, ecx
	jnz	short alloc_3
alloc_2:
	; 'no free blocks on disk !' error
	mov	dword [u.error], ERR_ALLOC
	jmp	error
alloc_3:
	; 26/11/2021 - Retro UNIX 386 v2 file system compatibility code
	xor	eax, eax
	; 23/07/2021
	mov	ecx, [ebx+SB.FirstFreeBlk] ; 1st free sector number
					 ; (as logical sector number)
	or	ecx, ecx
	jz	short alloc_5 ; 0 = initial/reset value or 'not valid'
	inc	ecx ; 0FFFFFFFFh -> 0
	jz	short alloc_5 ; 'not valid' or 'not calculated'
	dec	ecx ; restore first free block value
	mov	eax, ecx
	; 1 allocation byte (in fbm) is for 8 sectors
	shr	ecx, 3	; first free block number / 8
	and	cl, ~3	; dword alignment (to backward)
	; 27/11/2021
	mov	[free_map_offset], ecx ; byte position on fbm
alloc_4:
	shr	eax, 12 ; 1 fbm sector for 4096 sectors
alloc_5:
	; 26/11/2021
	push	ebx ; ** ; superblock buffer address 
	; eax = fbm sector index
	mov	[free_map_index], eax
	add	eax, [ebx+SB.FreeMapAddr] 
			; free blocks map start sector addr
	; 09/01/2022
	add	eax, [ebx+SB.BootSectAddr] 
			; + Hidden Sectors
	;
	; eax = physical sector number
	call	dskrd
	; cpu returns here if there is/was not an error in 'dskrd'
	; eax = physical sector number
	; ebx = buffer data address
	; 27/11/2021
	mov	edx, [free_map_offset] ; byte position on fbm
	mov	[free_map_sector], eax ; physical sector number
	mov	ecx, 511
	;and	edx, 511
	and 	edx, ecx ; fbm byte offset in fbm buffer
	inc	ecx ; 512
	add	ecx, ebx
	add	edx, ebx ; + buffer address
	pop	ebx ; ** ; superblock buffer address 
	;
	; ebx = superblock buffer address
	; edx = fbm buffer address + byte offset (dword aligned)
	; ecx = fbm buffer address + 512
alloc_6:
	; 26/11/2021 - Retro UNIX 386 v2 file system compatibility code
	;		for Retro UNIX 386 v1.2
	; 25/05/2020 - Retro UNIX 386 v2 code
	; (dword scan) ((1 dword = 32 sectors, 1 byte = 8 sectors))
	; ((Note: Logical drive size must be multiplies of 32 sectors))
	; 19/05/2020
	; edx = free blocks map byte address 
	bsf	eax, [edx] ; Scans source operand for first bit set (1).
			  ; Clear ZF if a bit is found set (1) and 
			  ; loads the destination with an index to
			  ; first set bit. (0 -> 31) 
			  ; Sets ZF to 1 if no bits are found set.
	jnz	short alloc_8 ; ZF = 0 -> a free block has been found

	mov	eax, [free_map_offset] ; byte offset from start of fbm
	add	eax, 4 ; next dword
	cmp	eax, [ebx+SB.FreeMapSize] ; fbm size in bytes
	jb	short alloc_7
	; invalidate superblock's first free block field
	mov	dword [ebx+SB.FirstFreeBlk], 0FFFFFFFFh
	; 'no free blocks on disk !' error
	jmp	alloc_2
alloc_7:
		; 26/05/2020
		; NOTE: If ldrv size is not multiplies of 32 sectors,
		; mod (ldrv size / 32) sectors (at the end of ldrv)
		; can not be allocated (for regular files or dirs)! 
		; (Kernel or runix fs installation program can use
		;  this fact to save/duplicate critical sectors/data 
		;  onto end sectors of that logical drive's runix fs.)

	; 26/11/2021
	mov	[free_map_offset], eax
	; 28/07/2021
	; set next first free block value for search
	shl	eax, 3 ; * 8 ; first free block
 	; 26/11/2021
	mov	[ebx+SB.FirstFreeBlk], eax
	; search (scan) for next free block map dword (in buffer)
	add	edx, 4 ; next dword
	cmp	edx, ecx
	jb	short alloc_6
	; 27/11/2021
	jmp	short alloc_4

alloc_8:
	; 26/11/2021
	; convert bit index to xor value
	; and then set allocated flag for corresponding fbm bit 
	mov	ecx, eax
	;mov	cl, al 
	;sub	eax, eax
	;inc	al ; eax = 1
	mov	al, 1
	shl	eax, cl
	xor	[edx], eax  ; clear allocated block's bit in the fbm
	; zero bit means allocated disk block (1 means free disk block) 

	mov	eax, [free_map_offset] ; byte offset from start of fbm
		; note: eax is dword aligned (0,4,8,12..) !
	shl	eax, 3 ; * 8 ; free block number base
	add	eax, ecx ; add allocation bit index (0 to 31)	
	mov	[ebx+SB.FirstFreeBlk], eax ; current free block
				; (which is being allocated here)

	;mov	eax, [ebx+SB.FreeMapAddr] 
	;		; free blocks map start sector addr
	;add	eax, [free_map_index]
	
	;;add	eax, [ebx+SB.BootSectAddr] 
	;		; + Hidden Sectors
	; eax = physical sector number
	
	; 27/11/2021
	mov	eax, [free_map_sector] ; physical sector number
	
	call	wslot
	; ebx = buffer data address (write operation bit is set)
	; eax = physical sector number
	; Note: ebx contains addr of the 1st buffer in the buffer
	; (the last allocated buffer becomes the 1st in buffer chain)

	; 27/11/2021
	;mov	[free_map_buffer], ebx ; save free map buffer address

	call	dskwr ; writes the 1st buffer to sector
		      ; (at the beginning/head of the buffer chain)

	; if we are here, buffer has been written to disk successfully 
	; (otherwise cpu would jump to 'error' address)

	; set superblock modified flag
	mov	edx, smod
	mov	ebx, systm
	test	byte [cdev], 1 ; mounted device ?
	jz	short alloc_9  ; no, root device
	; yes, mounted device
	mov	ebx, mount
	;mov	edx, mmod
	inc	edx ; edx = offset mmod
alloc_9: 
	inc	byte [edx] ; superblock modified !
	
	; Allocating a new sector/block (for file) has been completed here.
	mov	eax, [ebx+SB.FreeBlocks]
	inc	eax ; 0FFFFFFFFh -> 0 
	jz	short alloc_10 ; not valid (not set)
	dec	eax ; Free Blocks
	dec	eax ; Free Blocks = Free Blocks - 1
	mov	[ebx+SB.FreeBlocks], eax
alloc_10:
	call	get_system_time
		; eax = current time (as unix epoch time)
	; 30/07/2021
	mov	[ebx+SB.ModifTime], eax

	mov	eax, [ebx+SB.FirstFreeBlk] ; allocated block address
	inc	dword [ebx+SB.FirstFreeBlk] 
				; +1, new value of first free block
				; (new search will be started from here)
				; ((it may not be free, no problem))

	; put free map buffer address in ebx
	; 27/11/2021
	;mov	ebx, [free_map_buffer]

	retn	

	; 11/02/2022
	; 09/01/2022
	; 27/11/2021 - Retro UNIX 386 v2 fs compatibility modification
free:
	; 12/04/2022 (BugFix)
	; 09/03/2022
	; 11/02/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 05/11/2021
	; 29/10/2021 - temporary (simplified code)
	; 14/08/2021
	; 12/06/2021
	; 07/06/2020
	; 25/05/2020 (Retro UNIX 386 v2 - Beginning)
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/04/2013 - 01/08/2013 (Retro UNIX 8086 v1)
	;
	; calculates byte address and bit position for given block number
	; then sets the corresponding bit in the free storage map
	; 
	; INPUTS ->
	;    r1 - block number for a block structured device
	;    cdev - current device 
	; OUTPUTS ->
	;    free storage map is updated
	;    smod is incremented if cdev is root device (fixed disk)
	;    mmod is incremented if cdev is a removable disk 	
	;
	;  (Retro UNIX Prototype : 01/12/2012, UNIXCOPY.ASM)
        ;  ((Modified registers: DX, CX))  

	; 27/11/2021
	; INPUT:
	;	[cdev] = current device (root = 0, mounted = 1) 
	;	eax = disk block/sector number/address (logical)
	; OUTPUT:
	;	ebx = superblock buffer address
	;
	;    Note:	
	;	Free blocks map will be modified and written to it's disk.
	;	Superblock (buffer) of [cdev] will be modified and it's 
	;	modification flag will be set; but the superblock 
	;	will not be written to it's disk in this 'free' procedure)
	;
	; Modified registers: ebx, ecx, edx, esi, edi, ebp 

	; 27/11/2021
	mov 	ebx, systm ; (SuperBlock of root file system)
	test	byte [cdev], 1
	jz	short free_1
	mov	ebx, mount ; (SuperBlock of mounted file system)
free_1: ; 1
	; 11/02/2022
	; 27/11/2021 - Retro UNIX 386 v2 file system compatibility code
	test	byte [ebx+SB.ReadOnly], 1 ; if bit 0 is 1, it is RO fs	
	jz	short free_2 
	; 'read only file system' error
	mov	dword [u.error], ERR_READ_ONLY_FS
	jmp	error
free_2:
	mov	ecx, eax ; logical sector/block number
	mov	[free_map_sector], eax
	shr	ecx, 3	; convert to fbmap byte offset
	shr	eax, 12 ; convert to fbmap sector index 
			; (1 fb sector for 4096 sectors)
	;and	cl, ~3	; dword alignment (to backward)
	; 11/02/2022
	;and	ecx, 511 ; convert to offset within fbm buffer
	and	ecx, 1FCh ; 508, dword aligned offset (32 bit scan)
	;
	mov	[free_map_offset], ecx ; byte position on fbm buffer
	; 27/11/2021
	; eax = fbm sector index
	add	eax, [ebx+SB.FreeMapAddr] 
			; free blocks map start sector address
	; 09/01/2022
	add	eax, [ebx+SB.BootSectAddr]
			; + Hidden Sectors
	; 12/04/2022
	push	ebx ; (*)
	; ebx = superblock address
	; eax = physical sector number
	call	dskrd
	; cpu returns here if there is/was not an error in 'dskrd'
	; eax = physical sector number
	; ebx = (fbm sector) buffer data address
	; 27/11/2021
	mov	[free_map_index], eax  ; save physical sector address
	; 12/04/2022
	mov	edx, [free_map_offset] ; byte position in fbm buffer
	; 11/02/2022
	;(EDX contains -rounded down- dword aligned offset value)
	add	edx, ebx ; + (fbm sector) buffer start address
	; 12/04/2022
	pop	ebx ; (*) superblock buffer address 	
	;
	sub	eax, eax
	mov	al, [free_map_sector] ; logical sector number
	;and	al, 7
	; 11/02/2022
	and	al, 31 ; 32 bit fbm scan (with dword aligned offset)

	; al = bit offset in fbm allocation byte
	; edx = buffer address (start+offset)
	; [free_map_offset] = byte position on fbm
	; 27/11/2021
	bts	[edx], eax  ; copy value of bit position in eax to cf
			    ; then set same bit position at [edx]
	jnc	short free_3 ; it was allocated sector

	; already free sector !? 
	; (nonsence or defective fs)

	xor	al, al ; 0  ; eax = 0
	dec	eax ; 0FFFFFFFFh
	;mov	[ebx+SB.FreeBlocks], eax 
		; invalidate free blocks count
	jmp	short free_5
free_3:
	; 27/11/2021
	mov	eax, [ebx+SB.FreeBlocks]
	inc	eax
	;jz	short free_4 ; 0FFFFFFFFh -> 0 (invalid!)
	; 09/03/2022
	jnz	short free_4
	dec	eax  ; 0 -> 0FFFFFFFFh (invalid!)
free_4: 
	push	eax ; * ; number of free blocks 
	;mov	eax, [free_map_index] ; fbm sector index
	;add	eax, [ebx+SB.FreeMapAddr] 
			; free blocks map start sector address
	;add	eax, [ebx+SB.BootSectAddr]
	;		; + Hidden Sectors
	mov	eax, [free_map_index] ; restore physical sector address
	; eax = physical sector number
	call	wslot
	; ebx = buffer data address (write operation bit is set)
	; eax = physical sector number
	; Note: ebx contains addr of the 1st buffer in the buffer
	; (the last allocated buffer becomes the 1st in buffer chain)

	call	dskwr ; writes the 1st buffer to sector
		      ; (at the beginning/head of the buffer chain)

	; if we are here, buffer has been written to disk successfully 
	; (otherwise cpu would jump to 'error' address)
	pop	eax ; *  ; number of free blocks 
free_5:
	; eax = number of free blocks 
	;
	; set superblock modified flag
	mov	edx, smod
	mov	ebx, systm
	test	byte [cdev], 1 ; mounted device ?
	jz	short free_6 ; no, root device
	; yes, mounted device
	mov	ebx, mount
	;mov	edx, mmod
	inc	edx ; edx = offset mmod
free_6: 
	mov	[ebx+SB.FreeBlocks], eax 

	mov	eax, [free_map_sector] ; logical sector number
	cmp	eax, [ebx+SB.FirstFreeBlk]
	jnb	short free_7
	mov	[ebx+SB.FirstFreeBlk], eax
free_7:
	inc	byte [edx] ; superblock modified !
	retn
	
iget:
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 30/11/2021
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/04/2013 - 07/08/2013 (Retro UNIX 8086 v1)
	;
	; get a new i-node whose i-number in r1 and whose device is in cdev
	;
	; ('iget' returns current i-number in r1, if input value of r1 is 0)
	; 
	; INPUTS ->
	;    ii - current i-number, rootdir
	;    cdev - new i-node device
	;    idev - current i-node device
	;    imod - current i-node modified flag
	;    mnti - cross device file i-number
	;    r1 - i-numbe rof new i-node
	;    mntd - mountable device number		
	; 	 
	; OUTPUTS ->
	;    cdev, idev, imod, ii, r1
	;
	; ((AX = R1)) input/output
	;
	;  (Retro UNIX Prototype : 14/07/2012 - 18/11/2012, UNIXCOPY.ASM)
        ;  ((Modified registers: eDX, eCX, eBX, eSI, eDI, eBP))  

	; 22/11/2021
	;;mov	dl, [cdev] ; 18/07/2013
	;;mov	dh, [idev] ; 07/08/2013
	; 26/05/2020 - Retro UNIX 386 v2
	;mov	dh, [cdev]
	;mov	dl, [idev]
	mov	dx, [idev] ; [idev] in dl, [cdev] in dh
	;
	; 22/11/2021
	and	eax, 0FFFFh
	cmp 	eax, [ii]
		; cmp r1,ii / r1 = i-number of current file
	jne 	short iget_1
		; bne 1f
	cmp	dl, dh
		; cmp idev,cdev
			  ; / is device number of i-node = current device
        je	short iget_5
	;	; beq 2f
	; 14/08/2021
iget_1: ; 1:
	xor	bl, bl
	cmp	[imod], bl ; 0	
		; tstb imod / has i-node of current file
			  ; / been modified i.e., imod set
	jna	short iget_2
		; beq 1f
	mov	[imod], bl ; 0
		; clrb imod / if it has, 
			   ; / we must write the new i-node out on disk
	; 22/11/2021 (32 bit push-pop)
	push	eax ; *
		; mov r1,-(sp)
	push	edx ; **
		; mov cdev,-(sp)
	mov	eax, [ii]
		; mov ii,r1
	;mov	dh, [idev]
	;mov	[cdev], dh
	; 09/01/2022
	mov	[cdev], dl  ; dl = [idev]
		; mov idev,cdev
	inc	bl ; 1
	; 31/07/2013
	mov     [rw], bl ; 1 == write 
	;;28/07/2013 rw -> u.rw
        ;;mov   [u.rw], bl ; 1 == write
	call	icalc
		; jsr r0,icalc; 1
	pop	edx ; **
	mov	[cdev], dh ; 22/11/2021
		; mov (sp)+,cdev
	pop	eax ; *
		; mov (sp)+,r1
iget_2: ; 1:
	;and	ax, ax
	and	eax, eax ; 22/11/2021 (32 bit inode number)
		; tst r1 / is new i-number non zero
	jz	short iget_4 ; 2f
		; beq 2f / branch if r1=0

	;mov 	dh, [cdev]
	or	dh, dh ; 22/11/2021
		; tst cdev / is the current device number non zero
			 ; / (i.e., device =/ drum)
	jnz	short iget_3 ;  1f
		; bne 1f / branch 1f cdev =/ 0  ;; (cdev != 0)
	; 22/11/2021
	cmp	eax, [mnti]
	;cmp	ax, [mnti]			
		; cmp r1,mnti / mnti is the i-number of the cross device
			    ; / file (root directory of mounted device)
	jne	short iget_3 ; 1f
		; bne 1f
        ;mov    bl, [mntd]
	inc	dh ; mov dh, 1 ; 22/11/2021
        mov	[cdev], dh ; 22/11/2021
		; mov mntd,cdev / make mounted device the current device
	; 22/11/2021
	mov	eax, [rootdir] ; rootdir = 1 (for runix v2 file system)
	;mov	ax, [rootdir]
		; mov rootdir,r1
iget_3: ; 1:
	; 22/11/2021
	mov	[ii], eax ; 32 bit inode number
	;mov	[ii], ax
		; mov r1,ii
	; 30/11/2021
	mov	[idev], dh ; cdev 
	;mov	[idev], dl ; cdev
		; mov cdev,idev
	xor	bl, bl
        ; 31/07/2013
	mov     [rw], bl ; 0 == read 
	;;28/07/2013 rw -> u.rw       
        ;;mov   [u.rw], bl ; 0 = read
	call	icalc
		; jsr r0,icalc; 0 / read in i-node ii
iget_4: ; 2:
	; 22/11/2021
	mov	eax, [ii]
	;mov	ax, [ii]
		; mov ii,r1
iget_5:
	retn
		; rts r0

icalc:
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 28/11/2021
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification 
	; 02/07/2015
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/04/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; calculate physical block number from i-number then
	; read or write that block
	;
	; 'icalc' is called from 'iget'
	;
	; for original unix v1:
	; / i-node i is located in block (i+31.)/16. and begins 32.*
       	; / (i+31.) mod 16. bytes from its start
	;
	; for retro unix 8086 v1:
	;  i-node is located in block (i+47)/16 and
	;  begins 32*(i+47) mod 16 bytes from its start
	;
	; INPUTS ->
	;    r1 - i-number of i-node
	; 	 
	; OUTPUTS ->
	;    inode r/w
	;
	; ((AX = R1)) input
	;
	;  (Retro UNIX Prototype : 14/07/2012 - 18/11/2012, UNIXCOPY.ASM)
        ;  ((Modified registers: eAX, eDX, eCX, eBX, eSI, eDI, eBP))  
	;

	; 28/11/2021
	;cmp	byte [idev], 0 ; [cdev] = [idev]
	cmp	byte [cdev], 0
	jna	short icalc_0r
	mov	ebp, mount  ; mounted file system's superblock 
	jmp	short icalc_0m
icalc_0r:
	; 28/11/2021
	mov	ebp, systm  ; root file system's superblock 
icalc_0m:
	; 22/11/2021
	;mov	edx, eax
	mov	edx, [ii] ; 32 bit inode number
	mov	eax, [ebp+SB.InodeTblAddr] ; inode table base address
	;
	dec	edx ; 0 based inode number 
		    ; (inode index in inode table)
	push	edx ; *
	shr	edx, 3
	jz	short icalc_0t ; 0 for inodes 1 to 8
	add	eax, edx
icalc_0t:
	; 09/01/2022
	; eax = locical block/sector number 
	add	eax, [ebp+SB.BootSectAddr]
	; eax = physical block/sector number 
	call	dskrd
		; jsr r0,dskrd / read in block containing i-node i.
	; 31/07/2013
        cmp     byte [rw], 0 ; Retro Unix 8086 v1 feature !
	;; 28/07/2013 rw -> u.rw
        ;;cmp	byte [u.rw], 0 ; Retro Unix 8086 v1 feature !
		; tst (r0)
	jna	short icalc_1
		; beq 1f / branch to wslot when argument
		       ; / in icalc call = 1
	; eAX = r1 = block number
	call	wslot
		; jsr r0,wslot / set up data buffer for write
			     ; / (will be same buffer as dskrd got)
	; EBX = r5 points to first word in data area for this block
icalc_1: ; 1:
	pop	edx ; *
	; 22/11/2021
	and 	edx, 07h ; 8 inodes per inode table sector
	shl 	edx, 6 ; * 64 (inode size)
	; edx = 64 * (mod(inodenumber-1)/8)
	mov	esi, ebx  ; ebx points to 1st word of the buffer
	add	esi, edx  ; edx is inode offset in the buffer
          	; eSI (r5) points to first word in i-node i.	
		; mov (sp)+,mq / calculate offset in data buffer; 
			     ; / 32.*(i+31.)mod16
		; mov $5,lsh / for i-node i.
		; add mq,r5 / r5 points to first word in i-node i.
	mov	edi, inode
		; mov $inode,r1 / inode is address of first word 
			      ; / of current i-node
	;mov 	ecx, 8 ; 02/07/2015 (32 bit modification)
	;	; mov $16.,r3
	; 28/11/2021
	sub	ecx, ecx
	mov	cl, 16 ; Retro UNIX 386 v2 inode size = 64 bytes
	; 31/07/2013
  	cmp     [rw], ch ; 0  ;; Retro Unix 8086 v1 feature !
       ;;28/07/2013 rw -> u.rw                 
       ;;cmp    [u.rw], ch ; 0  ;; Retro Unix 8086 v1 feature !
		; tst (r0)+ / branch to 2f when argument in icalc call = 0
	jna	short icalc_3
		; beq 2f / r0 now contains proper return address 
		       ; / for rts r0
icalc_2: ; 1:
	xchg 	esi, edi
	; overwrite old i-node (in buffer to be written)
	rep 	movsd
		; mov (r1)+,(r5)+ / over write old i-node
		; dec r3
		; bgt 1b
	;call	dskwr
	;	; jsr r0,dskwr / write inode out on device
	;retn
	;	; rts r0
	; 28/11/2021
	jmp	dskwr
icalc_3: ; 2:
	; copy new i-node into inode area of (core) memory
	rep 	movsd
		; mov (r5)+,(r1)+ / read new i-node into 
		                ; / "inode" area of core
		; dec r3
		; bgt 2b
	retn
		; rts r0

access:
	; 13/03/2022
	; 25/12/2021
	; 19/12/2021
	; 28/11/2021
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	; 31/10/2021 - temporary (simplified code)
	; 14/06/2021
	; 02/05/2021
	; 27/03/2021
	; 26/03/2021
	; 25/03/2021
	; 23/03/2021 (Retro UNIX 386 v2 - Beginning)
	;	((ref: unix v7 source - fio.c))
	; ref: INODE STRUCTURE of Retro UNIX v2 file system
	;	07/02/2020, 'RETRO UNIX v2 Inodes.pdf'
	;	 
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/04/2013 - 29/04/2013 (Retro UNIX 8086 v1)
	;
	; check whether user is owner of file or user has read or write
	; permission (based on i.flgs).
	;
	; INPUTS ->
	;    r1 - i-number of file
	;    u.uid
	; arg0 -> (owner flag mask)	 		
	;     Retro UNIX 8086 v1 feature -> owner flag mask in DL (DX) 	 
	; OUTPUTS ->
	;    inode (or jump to error)
	;
	; ((AX = R1)) input/output
	;
	;  ((Modified registers: eCX, eBX, eDX, eSI, eDI, eBP))

	; 26/03/2021
	; INPUT:
	;	eax = inode number (ax)
	;	 dx = mode (IEXEC, IREAD or IWRITE) 
	;	[cdev] = logical drive number ; 31/10/2021 
	;	
	; OUTPUT:
	;	inode (or jump to error)
	;	eax (ax) = [ii] = inode number
	;	[cdev] = logical drive number ; 31/10/2021 
	;
	; Modified registers: (eax), ebx, ecx, edx, esi, edi, ebp
	;
	; IREAD	 equ 100h ; read permission flag, owner
	; IWRITE equ 80h  ; write permission flag, owner
	; IEXEC	 equ 40h  ; execute permission flag, owner	 
		
	; 23/03/2021
	push	edx ; save flag (DX)
	;push	dx  ; save flag (DL)
	call	iget
		; jsr r0,iget / read in i-node for current directory
			    ; / (i-number passed in r1)
	;;mov	cl, [i.flgs]
	;;	; mov i.flgs,r2
	; 23/03/2021
	;mov	cx, [i.iflgs]	

	;;pop	dx  ; restore flag (DL)
	; 23/03/2021
	pop	edx ; restore flag (DX)

	; 28/11/2021 (iget will not return here if there is an error)
	; 31/10/2021
	;jc	short access_5

	; 31/10/2021
	; Note: If eax input is same with [mnti], 'iget' will change
	; [cdev] to 1 and eax will be 1 (root dir of mounted fs) 

	; 27/03/2021
	; 23/03/2021
	; (check if it is write permission flag)
	cmp	dx, 80h ; IWRITE
	ja	short access_r	; IREAD = 100h (owner)
	jb	short access_x	; IEXEC = 40h (owner)
	; IWRITE (80h)
access_w:
	; 23/03/2021
	; ((ref: Retro UNIX 386 v2, 'ux.s', 'ldrv' structure))
	;mov	bl, [idev]
	;call	getfs
 	;movzx	ebx, byte [idev]  ; logical drive number
	;shl	bx, 6 ; * 64	  ; of current inode	
	;add	ebx, ldrvtable	
	;;test	byte [ebx+ldrv.pflags], 01h ; read only fs flag
	;;jz	short access_0
	;; 14/06/2021
	; 14/06/2021
	; check block device (fs) if it is read only fs or not ? 
	;mov	edi, [ebx+ldrv.superblk]
	;test	byte [edi+SB.ReadOnly], 1 ; bit 0 = 1 ?
	;jz	short access_0
	; 31/10/2021
	cmp	byte [cdev], 1
	jb	short access_7 ; [cdev] = 0
	; [cdev] = 1
	; 28/11/2021
	mov	edi, mount ; mounted file system's superblock buffer
	jmp	short access_8
access_7:
	; 28/11/2021
	mov	edi, systm ; root file system's superblock buffer
access_8:
	test	byte [edi+SB.ReadOnly], 1 ; bit 0 = 1 ?
	jz	short access_0

	; 23/03/2021
	;mov	dword [u.error], ERR_READ_ONLY_FS
	;		; 'read only file system !' error
	;jmp	error

	; 19/12/2021
	test	byte [i.flgs+1], 80h ; regular file ?
	jz	short access_0  ; no, device file

	; 31/10/2021
	mov	eax, ERR_READ_ONLY_FS
			; 'read only file system !' error
	; 27/03/2021
	jmp	short access_5

	; check block device (fs) if it is read only fs or not ? 

	;mov	dh, [u.uid]
	;cmp	dh, [i.uid]
	;	; cmpb i.uid,u.uid / is user same as owner of file
	;jne	short access_1
		; bne 1f / no, then branch
access_x:
	; IEXEC (40h)
	; 27/03/2021
	mov	bx, [i.flgs]
	test	bh, 80h ; regular file ?
	jz	short access_4 ; not executable file !
	test	bh, 40h	; directory ?
	jnz	short access_4 ; not executable file !
	mov	cx, [u.uid]
	or	cx, cx
	jnz	short access_3 ; (restricted user)
	; (super user)
	;test	[i.flgs], dl ; execute permission for owner
	;test	byte [i.flgs], 49h ; for owner, group, others
	test	bl, 49h ; exec perm for owner, group, others
	jnz	short access_2
access_4:
	; 31/10/2021
	mov	eax, ERR_NOT_EXECUTABLE
			; 'not executable file !' error
	; 26/03/2021
;;sysexec_not_exf:
;	mov	dword [u.error], ERR_NOT_EXECUTABLE
;			; 'not executable file !' error
;access_5:
;	jmp	error
	
	; 31/10/2021
access_5:
	mov	[u.error], eax
	jmp	error

access_r:
	; 27/03/2021
access_0:
	; 23/03/2021
	mov	cx, [u.uid]
	or	cx, cx ; 0 ; root ?
	;jz	short access_2 ; yes
	; 25/12/2021
	jz	short access_1 ; yes
access_3:
	; 13/03/2022
	cmp	cx, [i.uid] ; owner ?
	je	short access_1 ; yes
	;; owner ?
	mov	bl, [i.gid]
	;cmp	cx, [i.uid]
	;;je	short access_1
	;; 02/05/2021
	;jne	short access_6
	; 13/03/2022
	; same group ?
	shr	dx, 3
	; check owner's group number/ID
	cmp	bl, [u.gid] ; same group number/ID ?
	je	short access_1 ; yes
	; one of others
	;shr	dx, 6
	;jmp	short access_1 ; others
	; 13/03/2022
	; one of others
	shr	dl, 3
;	; 13/03/2022
;	jmp	short access_1 ; others
;access_6:
;	;shr	cl, 2
;	;	; asrb r2 / shift owner read write bits into non owner
;	;	;       ; / read/write bits
;	;	; asrb r2
;
;	; 23/03/2021
;	; same group ?
;	shr	dx, 3
;	;mov	cl, [u.gid]
;	;cmp	cl, [i.gid]
;	;je	short access_1
;	; 02/05/2021
;	cmp	bl, [u.gid]
;	je	short access_1 ; same group, different owner
;
;	; others
;	;shr	dx, 3
;	shr	dl, 3
;	;jmp	short access_1	
;
;;access_1: ; 1:
;	;and	cl, dl
;	;	; bit r2,(r0)+ / test read-write flags against argument
;	;		     ; / in access call
;	;jnz	short access_2
;	;	; bne 1f
;	
;	;or	dh, dh	; super user (root) ?
;	;	; tstb u.uid
;	;jz	short access_2 ; yes, super user
;	;;jnz	error
;	;	; beq 1f
;	;	; jmp error
;	
;	;mov	dword [u.error], ERR_FILE_ACCESS 
;	;		; 'permission denied !' error
;	;jmp	error

access_1:
	; 23/03/2021
	test	dx, [i.flgs]
	jnz	short access_2

	;; r/w permission error
	;mov	dword [u.error], ERR_FILE_ACCESS 
	;		; 'permission denied !' error
	;;jmp	error

	; 31/10/2021
	mov	eax, ERR_FILE_ACCESS 
			; 'permission denied !' error
	; 27/03/2021
	jmp	short access_5

access_2: ; 1:
	;; DL = flags
	;retn
	;	; rts r0
	; 27/03/2021
	; DX = flag (100h-20h-04h or 80h-10h-02h or 40h-08h-01h)
	retn

	; 12/01/2022 - Retro UNIX 386 v1.2
%if 0

setimod:
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/04/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; 'setimod' sets byte at location 'imod' to 1; thus indicating that 
	; the inode has been modified. Also puts the time of modification
	; into the inode.
	;
	;  (Retro UNIX Prototype : 14/07/2012 - 23/02/2013, UNIXCOPY.ASM)
        ;  ((Modified registers: eDX, eCX, eBX)) 
	;
	
	;push 	edx
	push	eax

	mov 	byte [imod], 1
		; movb $1,imod / set current i-node modified bytes
	; Erdogan Tan 14-7-2012
	call 	epoch
		 ; mov s.time,i.mtim 
			    ; / put present time into file modified time
		 ; mov s.time+2,i.mtim+2

	mov 	[i.mtim], eax
	
	; Retro UNIX 386 v1 modification ! (cmp)
	; Retro UNIX 8086 v1 modification ! (test)
	cmp	dword [i.ctim], 0
	jnz	short setimod_ok

	mov 	[i.ctim], eax

setimod_ok: ; 31/07/2013
	pop	eax
	;pop	edx
	
	retn
		; rts r0
%endif

setimod:
	; 12/01/2022 - Retro UNIX 386 v1.2
	; 25/05/2020 - Retro UNIX 386 v2
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/04/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; 'setimod' sets byte at location 'imod' to 1; thus indicating that 
	; the inode has been modified. Also puts the time of modification
	; into the inode.
	;
	;  (Retro UNIX Prototype : 14/07/2012 - 23/02/2013, UNIXCOPY.ASM)
        ;  ((Modified registers: eDX, eCX, eBX)) 
	;

	; Reference:
	; 25/01/2020 - 'UNIXHDCP.ASM', 'setimod' procedure
	;	(Retro UNIX 386 v2 file system installation utility)

	; 25/05/2020
	;
	; INPUT:
	;	none
	; OUTPUT:
	;	inode times and [imod] will be set
	;	      ([imodx] will be reset)
	;	cf = 0 	

	; Modified registers: ecx, (edx)?
	
	;push 	edx
	push	eax

	mov 	byte [imod], 1
		; movb $1,imod / set current i-node modified bytes

	; 25/05/2020 - Retro UNIX 386 v2
	mov	eax, [i.ctim] ; inode (file) creation time
	and	eax, eax
	jnz	short setimod_4

	; New file
setimod_1:
	; 25/05/2020 - Retro UNIX 386 v2 by Erdogan Tan
	;	 (Modified UNIX v7 inode and new FS construction) 	
	;
	call	get_system_time ; get current (unix epoch) time

	;; Erdogan Tan 14-7-2012
	;call 	epoch
	;	 ; mov s.time,i.mtim 
	;		    ; / put present time into file modified time
	;	 ; mov s.time+2,i.mtim+2
	;
	;mov 	[i.mtim], eax

	; Retro UNIX 386 v1 modification ! (cmp)
	; Retro UNIX 8086 v1 modification ! (test)
	;cmp	dword [i.ctim], 0
	;jnz	short setimod_ok
		
	mov 	[i.ctim], eax
	
	; 25/05/2020
	jmp	short setimod_3

setimod_2:
	; File/Directory data (file size or content) is changed
	; This is last modification date&time
	mov	[i.mtim], eax
setimod_3:
	; 19/09/2019 ('UNIXHDCP.ASM', 'setimod' by Erdogan Tan)
	; This is last access or last changing date&time
	; of inode (chmod,chown,chgrp,link)
	; parameters (without file/dir data changing)
	mov	[i.atim], eax

setimod_ok: ; 31/07/2013
	pop	eax
	;pop	edx
	
	retn
		; rts r0
setimod_4:
	; 25/05/2020 - Retro UNIX 386 v2
	xor	ecx, ecx
	cmp	[i.mtim], ecx ; 0
	jna	short setimod_2

	call	get_system_time ; get current (unix epoch) time
	; ((system time will be updated directly by timer interrupt))

	cmp	byte [imodx], 0 ; flag means "file/dir data is same but
	jna	short setimod_2 ; inode has been changed"

	; File/Dir data (File size or content) is same but
	; inode's mode, link count, owner or group id has been changed
	; (so, we do not change last modification date&time)
	mov	byte [imodx], 0 ; reset inode modified (extended) flag
	jmp	short setimod_3

; 10/01/2022 - Retro UNIX 386 v1.2
%if 0
	; Retro UNIX 386 v1.1 'itrunc' code
itrunc:
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 23/04/2013 - 01/08/2013 (Retro UNIX 8086 v1)
	;
	; 'itrunc' truncates a file whose i-number is given in r1
	;  to zero length.
	;
	; INPUTS ->
	;    r1 - i-number of i-node
	;    i.dskp - pointer to contents or indirect block in an i-node
	;    i.flgs - large file flag		
	;    i.size - size of file	
	; 	 
	; OUTPUTS ->
	;    i.flgs - large file flag is cleared
	;    i.size - set to 0	
	;    i.dskp .. i.dskp+16 - entire list is cleared
	;    setimod - set to indicate i-node has been modified
	;    r1 - i-number of i-node  					
	;
	; ((AX = R1)) input/output
	;
	;  (Retro UNIX Prototype : 01/12/2012 - 10/03/2013, UNIXCOPY.ASM)
        ;  ((Modified registers: eDX, eCX, eBX, eSI, eDI, eBP))  

	call	iget
		; jsr r0,iget
	mov	esi, i.dskp
		; mov $i.dskp,r2 / address of block pointers in r2
	xor	eax, eax
itrunc_1: ; 1:
	lodsw
		; mov (r2)+,r1 / move physical block number into r1
	or 	ax, ax
	jz	short itrunc_5
		; beq 5f
	push	esi
		; mov r2,-(sp)
	test    word [i.flgs], 1000h    
		; bit $10000,i.flgs / test large file bit?
	jz	short itrunc_4
		; beq 4f / if clear, branch
	push	eax
		; mov r1,-(sp) / save block number of indirect block
	call	dskrd
		; jsr r0,dskrd / read in block, 1st data word 
			     ; / pointed to by r5
	; eBX = r5 = Buffer data address (the 1st word)
	mov	ecx, 256
		; mov $256.,r3 / move word count into r3
	mov	esi, ebx
itrunc_2: ; 2:
	lodsw
		; mov (r5)+,r1 / put 1st data word in r1; 
			     ; / physical block number
	and	ax, ax
	jz	short itrunc_3
		; beq 3f / branch if zero
	;push	ecx
	push	cx
		; mov r3,-(sp) / save r3, r5 on stack
	;push	esi
		; mov r5,-(sp)
	call	free
		; jsr r0,free / free block in free storage map
	;pop	esi
		; mov(sp)+,r5
	pop	cx
	;pop	ecx
		; mov (sp)+,r3
itrunc_3: ; 3:
	loop	itrunc_2
		; dec r3 / decrement word count
		; bgt 2b / branch if positive
	pop	eax
		; mov (sp)+,r1 / put physical block number of 
			     ; / indirect block
	; 01/08/2013
        and     word [i.flgs], 0EFFFh ; 1110111111111111b
itrunc_4: ; 4:
	call	free
		; jsr r0,free / free indirect block
	pop	esi
		; mov (sp)+,r2
itrunc_5: ; 5:
	cmp	esi, i.dskp+16
		; cmp r2,$i.dskp+16.
	jb	short itrunc_1	
		; bne 1b / branch until all i.dskp entries check
	; 01/08/2013
	;and     word [i.flgs], 0EFFFh ; 1110111111111111b
		; bic $10000,i.flgs / clear large file bit
	mov	edi, i.dskp
	mov	cx, 8
	xor 	ax, ax
	mov	[i.size], ax ; 0
		; clr i.size / zero file size
	rep	stosw
		; jsr r0,copyz; i.dskp; i.dskp+16. 
			   ; / zero block pointers
	call	setimod
		; jsr r0,setimod / set i-node modified flag
	mov	ax, [ii]
		; mov ii,r1
	retn
		; rts r0

	; Retro UNIX 386 v1.1 'imap' code
imap:
	; 17/07/2022 (Retro UNIX 386 v1.2)
	; 03/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 26/04/2013 (Retro UNIX 8086 v1)
	;
	; 'imap' finds the byte in core (superblock) containing
	; allocation bit for an i-node whose number in r1.
	;
	; INPUTS ->
	;    r1 - contains an i-number
	;    fsp - start of table containing open files
	;
	; OUTPUTS ->
	;    r2 - byte address of byte with the allocation bit
	;    mq - a mask to locate the bit position.	
	;	  (a 1 is in calculated bit posisiton)
	;
	; ((AX = R1)) input/output
	; ((DL/DX = MQ)) output
	; ((BX = R2)) output
	;
	;    (Retro UNIX Prototype : 02/12/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: eDX, eCX, eBX, eSI))  
	;
		; / get the byte that has the allocation bit for 
		; / the i-number contained in r1
	;mov	dx, 1
	mov	dl, 1
		; mov $1,mq / put 1 in the mq
	movzx	ebx, ax
		; mov r1,r2 / r2 now has i-number whose byte
 		          ; / in the map we must find
	sub	bx, 41
		; sub $41.,r2 / r2 has i-41
	mov	cl, bl
		; mov r2,r3 / r3 has i-41
	and	cl, 7
		; bic $!7,r3 / r3 has (i-41) mod 8 to get 
			   ; / the bit position
	jz	short imap1
	;shl	dx, cl
	shl	dl, cl
		; mov r3,lsh / move the 1 over (i-41) mod 8 positions
imap1:			   ; / to the left to mask the correct bit
	;shr	bx, 3
	; 17/07/2022
	shr	ebx, 3
		; asr r2
		; asr r2
		; asr r2 / r2 has (i-41) base 8 of the byte number
		       ; / from the start of the map
		; mov r2,-(sp) / put (i-41) base 8 on the stack
	mov	esi, systm
		; mov $systm,r2 / r2 points to the in-core image of
				; / the super block for drum
	;cmp	word [cdev], 0
	cmp	byte [cdev], 0
		; tst cdev / is the device the disk
	jna	short imap2
		; beq 1f / yes
	add	esi, mount - systm
		; add $mount-systm,r2 / for mounted device,
			; / r2 points to 1st word of its super block
imap2: ; 1:
	add	bx, [esi] ;; add free map size to si
		; add (r2)+,(sp) / get byte address of allocation bit
	add	bx, 4
	add	ebx, esi
        	; add (sp)+,r2 / ?
	;add	ebx, 4 ;; inode map offset in superblock
		      ;; (2 + free map size + 2)
		; add $2,r2 / ?
 	; DL/DX (MQ) has a 1 in the calculated bit position
        ; BX (R2) has byte address of the byte with allocation bit
	retn
		; rts r0
%endif

	; Retro UNIX 386 v2.0 'itrunc' code
itrunc:
	; 26/03/2022
	; 10/01/2022 - Retro UNIX 386 v1.2
	; 07/06/2020
	; 02/06/2020
	; 26/05/2020 - Retro UNIX 386 v2
	;
	; 'itrunc' truncates a file whose i-number is given in r1
	;  to zero length.
	; 07/06/2020
	; 02/06/2020
	; 26/05/2020 - Retro UNIX 386 v2
	;
	; 'itrunc' truncates a file whose i-number is given
	; in ax to zero length.
	;

	; Reference: 
	;	Retro UNIX 386 v2 FS installation utility
	; 17/01/2020 - 'UNIXHDCP.ASM', 'itrunc' procedure

	; * Free all the disk blocks associated
	; * with the specified inode structure.
	
	; INPUT: 
	;	(e)ax = inode number
	; OUTPUT: 
	;	inode will be modified (file size = 0)
	;	; 26/03/20221
	;	eax = 0 (if cf=0)

	; (Modified registers: eax, edx, ecx, ebx, esi, edi, ebp) 

	; 07/06/2020

	; [cdev] = current logical drive number
	;    eax = inode number (for [cdev])

	call 	iget
	; 10/01/2022
	;jc	short itrunc_0 ; error code in eax (al)

	;    eax = inode number, [ii]
	; [cdev] = current logical drive number (same with input)
	; [idev] = logical drive number of current inode, [ii]
	;   [ii] = current inode ([ii] = ax input, if ax was not zero)
	; [imod] = 0		
	; ([imodx] = 0)

	test	byte [i.flgs+1], 80h ; regular file or directory	
	jnz	short itrunc_1

	; 10/01/2022
	mov	dword [u.error], ERR_NOT_REGULAR ; 255 
			; 'not a regular file (or directory) !' error
itrunc_0:
	jmp	error
itrunc_1:
	test	byte [i.flgs+1], 10h ; large file (indirect blocks)
	jnz	short itrunc_5 ; large file
	mov	esi, i.dskp ; disk address pointer 0
itrunc_2:
	lodsd
	and	eax, eax
	jz	short itrunc_3 ; empty ! (no more sectors/blocks)
	; eax = block address to be released
	push	esi ; * ; 10/01/2022
	call	free
	; modified registers: ebx, ecx, edx, esi, edi, ebp 
	; 10/01/2022
	;jc	short itrunc_0 ; error code in eax
	pop	esi ; * ; 10/01/2022
	xor	eax, eax ; 0
	mov	[esi-4], eax ; 0
itrunc_3:
	cmp	esi, i.dskp+40
	jb	short itrunc_2 ; next disk address ptr
itrunc_4:
	mov	[i.size], eax ; 0
	;mov	[i.size_h], al ; 0
	
	; clear large file flag
	;and     byte [i.flgs+1], 0EFh ; 11101111b ; not 10h
		
	;call	setimod
	;retn
	jmp	setimod

itrunc_5:
	; free disk blocks by using triple indirect blocks at first
	mov	esi, i.dskp+36
	mov	eax, [esi]
	or	eax, eax
	jz	short itrunc_6

	mov	edx, 2 ; Triple indirect sign (level = 2)
	call	tloop
	; 10/01/2022
	;jc	short itrunc_0
		; eax = 0	
	mov	[esi], eax ; 0
itrunc_6:
	; free disk blocks by using double indirect blocks at second
	mov	esi, i.dskp+32
	mov	eax, [esi]
	and	eax, eax 
	jz	short itrunc_7

	mov	edx, 1 ; Double indirect sign (level = 1)
	call	tloop
	; 10/01/2022
	;jc	short itrunc_0
		; eax = 0	
	mov	[esi], eax ; 0
	;esi = i.dskp+32
itrunc_7:
	; free disk blocks by using single indirect blocks at third
	sub	esi, 4
	mov	eax, [esi]
	or	eax, eax
	jz	short itrunc_8

	sub	edx, edx ; 0  ; Single indirect sign (level = 0)
	call	tloop
	; 10/01/2022
	;jc	short itrunc_0
		; eax = 0
	mov	[esi], eax ; 0
itrunc_8:
	cmp	esi, i.dskp
	ja	short itrunc_7
	jmp	short itrunc_4

tloop:
	; 26/03/2022
	; 10/01/2022 - Retro UNIX 386 v1.2
	; 07/06/2020
	; 03/06/2020
	; 02/06/2020
	; 26/05/2020 - Retro UNIX 386 v2 by Erdogan Tan
	; 
	; * Free all the disk blocks associated
	; * with the specified inode structure.

	; Reference: 
	;	Retro UNIX 386 v2 FS installation utility
	; 17/01/2020 - 'UNIXHDCP.ASM', 'tloop' procedure

	; INPUT: 
	;	03/06/2020
	;	eax = indirect block number/address (logical)
	;	edx = level (in dl) 
	;	(0 = indirect, 1 = double indr. 2 = triple indr.) 	
	;
	; OUTPUT: 
	;	indirect blocks will be released
	;
	; Modified registers: eax, ebx, ecx, edx

	; 03/06/2020

	push	edi ; *
	push	esi ; **

	push	eax ; ***

	push	edx ; **** level

	; [ii] = current inode number
	;;[idev] = logical drive number of current inode, [ii]
	; eax = logical sector/block number

	; 26/03/2022
	; [cdev] = logical drive number (0 or 1)

	; 10/01/2022
	; convert to physical sector/block number/address
	call	mget_2	

	call	dskrd ; read disk sector

		;; ebx = buffer header address
		;; eax = physical sector/block number 
		;; [pdn] = phsysical drive (index) number
		
		;; Modified registers: eax, edx, ecx, esi, edi
		;; If cf = 1 --> eax = error code (al)

	; 10/01/2022
	; Return from 'dskrd': (Retro UNIX 386 v1.2)
	; 	eax = physical block/sector number
	; 	ebx = buffer data address
 	; Modified registers: edx, ecx, ebx, esi, edi, ebp
 
	pop	edx ; **** ; level in dl
	; 10/01/2022
	;jc	short tloop_5

	;mov	esi, [ebx+bufhdr.address] ; buffer address
	; 10/01/2022
	mov	esi, ebx ; buffer data address	

	; 07/06/2020
	mov	ecx, 512/4 ; 128 dwords	

	cmp	dl, 1 ; -
	jb	short tloop_7
	je	short tloop_1
	mov	edi, trpi_buf ; triple indirect buffer address
	jmp	short tloop_2
tloop_1:
	mov	edi, dbli_buf ; double indirect buffer address
tloop_2:
	; copy disk r/w buffer content to indirect block buffer
	;mov	ecx, 512/4 ; 128 dwords	
	rep	movsd

	dec	dl ; -
		   ; next indirect block level, 2 -> 1, 1 -> 0
	mov	esi, edi  ; (dbli, trpi) indirect buffer + 512
	mov	cl, 128 ; 128 dwords
tloop_3:
	sub	esi, 4  ; previous pointer

	mov	eax, [esi]

	or	eax, eax ; 0
	jz	short tloop_4

	; 26/03/2022
	push	edx ; @@
	; 03/06/2020
	push	ecx ; @
	call	tloop
	pop	ecx ; @
	; 26/03/2022
	pop	edx ; @@
	; 10/01/2022
	;jc	short tloop_5
tloop_4:
	loop	tloop_3

	jmp	short tloop_10

	; 10/01/2022
;tloop_5:
;	pop	edx ; ***  ; discard eax
;	; error code in eax
;tloop_6:
;	pop	esi ; **
;	pop	edi ; *
;	retn
	
	; free blocks in current indirect block
tloop_7:
	;mov	ecx, 512/4  ; 128 dwords
	add	esi, 508
tloop_8:
	mov	eax, [esi]
	and	eax, eax ; 0 ?
	jz	short tloop_9
	
	push	esi ; **** ; 10/01/2022
	push	ecx ; *****
	call	free
		; if cf = 0 -> eax = 0
		; if cf = 1 -> eax = error code (in al)
	pop	ecx ; *****
	pop	esi ; **** ; 10/01/2022
	; 10/01/2022
	;; 07/06/2020
	;jc	short tloop_6
tloop_9:
	sub	esi, 4
	loop	tloop_8
tloop_10:
	pop	eax ; *** ; free indirect block's itself
	call	free
		;; if cf = 0 -> eax = 0
		;; if cf = 1 -> eax = error code (in al)
	; 10/01/2022
	; (if we are here, there is not an error, cf=0) 
	;jmp	short tloop_6

	; 10/02/2022
	xor	eax, eax ; 0
	pop	esi ; **
	pop	edi ; *
	retn

	; Retro UNIX 386 v2.0 'imap' code
	;	(runix v2 fs inode map)
imap:
	; 18/04/2022
	; 26/03/2022
	; 12/03/2022
	; 10/01/2022 - Retro UNIX 386 v1.2
	;	(major modification)
	; 05/11/2021 - temporary (simplified code)
	; 21/08/2021
	; 15/08/2021
	; 14/06/2021
	; 02/05/2021
	; 01/04/2021, 08/04/2021, 24/04/2021
	; 29/03/2021
	; 28/03/2021 - Retro UNIX 386 v2 (beginning)
	;
	; 'imap' finds the byte in inode map containing
	; allocation bit for an i-node whose number in (E)AX
	;
	; (ref: 'UNIXHDCP.ASM', imap', 22/01/2020) 
	;
	; ((Retro UNIX 386 v2 'imap' code is mostly different
	; than Retro UNIX 386 v1.1 'imap' code.))
	;
	; INPUTS ->
	;   EAX = inode number
	;        0 = first free inode
	;       >0 = requested inode
	;   [cdev] = current (logical) drive/device
	;	
	; OUTPUTS ->
	;    EBX = address of the byte contains allocation bit
	;     DX has 1 at calculated bit position
	;   ; 05/11/2021	
	;    EBP = superblock buffer address  
	;    (EAX = inode number)
	;   12/03/2022
	;    (if EAX input is 0, EAX = first free inode > 1)
	;	 		  	
	;   ; 08/04/2021
	;     If cf=1 -> error code in eax (al) 	
	;
	; Modified registers: ebx, ecx, edx, esi, edi, ebp

	; 05/11/2021
	cmp	byte [cdev], 0
	jna	short imap_0
	; 10/01/2022
	mov	ebp, mount ; mounted fs superblock buffer
	jmp	short imap_1

imap_ialloc_err:
	; 10/01/2022
	mov	dword [u.error], ERR_INO_ALLOC ; 33
			; 'inode allocation error !'
	jmp	error
 
imap_0:
	; 10/01/2022
	mov	ebp, systm ; root fs superblock buffer
imap_1:
	; 26/03/2022
	; 10/01/2022 (Retro UNIX 386 v1.2)
imap_x:
	; 05/11/2021
	; 21/08/2021
	; 15/08/2021 (Retro UNIX 386 v2)
	; call from 'maknod2' ; 10/01/2022
	;
	; eax = inode number
	; [cdev] = device number for inode in eax
	; ebp = superblock buffer address

	; 10/01/2022 (Retro UNIX 386 v1.2)
	; 'imap' and 'imap_x' will return with..
	;    ECX = byte offset from inode map buff (imap_x)
	;    EBX = byte addr of the byte with allocation bit
	;    EDX (DL) has a 1 in the calculated bit position

	; 02/05/2021
	; convert inode number to first free inode if it is 0
	call	imap_3 ; 10/01/2022
	jc	short imap_ialloc_err
	; eax = inode number > 1 (1 = root directory inode)

	mov	[ebp+SB.LastInode], eax
	
	push	eax ; * ; save inode number

	; convert inode number to inode map sector index
	call	imap_8 ; 10/01/2022
	push	eax ; ** ; byte offset in inode map buffer

	; edx = inode map buffer sector index number (< 16)
	; eax = byte offset in inode map buffer (< 512)

	; 15/08/2021
	mov	[ebp+SB.ImapIndex], edx
	mov	eax, [ebp+SB.InodeMapAddr]
	add	eax, edx
	add	eax, [ebp+SB.BootSectAddr]
	; eax = physical sector number
	; [cdev] = current device/disk number
	
	; 10/01/2022 (Retro UNIX 386 v1.2)
	push	ebp ; ***
	call	dskrd
	pop	ebp ; ***
	; cpu returns here if there is/was not an error in 'dskrd'
	; eax = physical sector number
	; ebx = buffer data address

	; 26/03/2022
	; (ecx may be > 255 at return from dskrd)
	;sub	ecx, ecx ; 0
	; 10/01/2022
	;mov	cl, [esp+4] ; * ; inode number
	; 18/04/2022
	mov	ecx, [esp+4] ; * ; inode number
	dec	cl
	; 26/03/2022
	xor	edx, edx
	;mov	dl, 1
	inc	dl ; dl = 1
	;and	ecx, 7
	; 26/03/2022
	; 02/05/2021
	;dec	cl
	;jz	short imap_2 ; 15/08/2021
	and	cl, 7	; 1 imap byte is for 8 inodes 
	;jz	short imap_2
	shl	dl, cl
imap_2:
	; 10/01/2022
	; save physical sector number of imap sector in the SB
	mov	[ebp+SB.ImapBuffer], eax ; ! sector addr !
	; 
	pop	ecx ; ** ; byte offset in inode map buffer
	add	ebx, ecx ; + byte position
	pop	eax ; * ; inode number

	; 10/01/2022
	; ECX contains byte offset from inode map buff (imap_x)
	; EDX (DL) has a 1 in the calculated bit position
	; EBX has byte address of the byte with allocation bit

	retn

imap_3:
	; 10/01/2022
	; 15/08/2021
	; 02/05/2021
	; 28/03/2021
	; get first free inode number (from superblock)
	; and check inode number against inode count
	;
	; INPUT:
	;    eax = 0 -> get first free inode number
	;    (eax > 0 -> check inode number)
	;    ebp = superblock address ; 15/08/2021
	; OUTPUT:
	;    eax = inode number 
	;    (eax will be 2 when SB.FirstFreeIno is invalid)
	;    cf = 1 -> inode allocation error/problem
	
	and	eax, eax	
	jnz	short imap_5

	; get first free inode
	;mov	eax, [ebp+SB.FreeInodes]
	;and	eax, eax
	;jz	short imap_6
	; 10/01/2022
	cmp	[ebp+SB.FreeInodes], eax ; 0
	jna	short imap_6	
	mov	eax, [ebp+SB.FirstFreeIno] ; 1 based inode num
	inc	eax
	jnz	short imap_4 ; 0FFFFFFFFh -> 0
	; invalid first free inode value (not initialized)
	mov	al, 3 ; first free inode to be searched + 1
imap_4:
	dec	eax
imap_5:
	; 15/08/2021
	; check inode number against inode count of the fs
	cmp	[ebp+SB.InodeCount], eax
	retn	; if cf = 1 --> inode number > inode count
imap_6:
	; 'imap' inode allocation error !
	stc
imap_7:
	retn

imap_8:
	; 10/01/2022
	; 15/08/2021
	; 02/05/2021
	; 28/03/2021
	; convert inode count to inode map sector index
	;
	; INPUT: 
	;    eax = inode number
	; OUTPUT:
	;    edx = inode map sector offset/index number
	;    eax = byte offset from start of imapbuf	 

	xor	edx, edx
	dec	eax ; zero based inode number
	jz	short imap_7

	shr	eax, 3 ; 8 inodes per inode map byte
		; eax = byte offset from start of inode map
	mov	dl, ah ; ah <= 31 (1Fh) for Retro UNIX 386 v2
	shr	dl, 1
		; edx = inode map sector offset/index number
	;and	eax, 511
	and	ax, 511
		; eax = byte offset from start of imapbuf
	retn