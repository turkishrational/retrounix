; RUFSHDP.ASM
;----------------------------------------------------------------
; RETRO UNIX v0.3 - Modified UNIX v7 inode & devices - 01/09/2019
; RETRO UNIX v0.2 - 14 byte file name modifications (04/12/2015)
; RETRO UNIX v0.1 'fd0' formatting procedures
;
; Derived from UNIXPROC.ASM (for 1.44MB floppies) - 29/09/2019
;
; Last Update: 05/04/2022 - Retro UNIX 386 v2 ;******************
;
; 14/01/2020 - Retro UNIX 386 v2
; 09/07/2013 - Retro UNIX v1
; ERDOGAN TAN
; 01/03/2013, 03/03/2013, 05/03/2013
; 16/12/2012 -> sioreg (bugfix)
; [ 14-27/7/2012, 4-21/8/2012, 16/9/2012, 20/10/2012, 31/10/2012 ]
; These procedures will be located in UNIXFDFS.ASM file 
; when they are completed.
; (NOTE: only for (R)UFS initialization of FD0 1.44MB floppy disk

err_INVALIDDATA equ 100h
err_NOFREEBLOCK equ 200h

iget 	proc near
	; 27/11/2019
	; 07/11/2019 - Retro UNIX 386 v2
	; 16/09/2012
     	; 14/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	;; AX=R0, BX=R1 
	; RETRO UNIX v1 FS
	; initialization/format version
	; (cdev, idev,mnt, mntd are excluded)
	;; return => if cf=1 error number in [Error] 

	; 27/11/2019
	; INPUT:
	; 	BX = inode number (0 if current inode)
	; OUTPUT:
	;	BX = inode number if cf = 0
	;	
	; Modified registers: ax,bx,cx,dx 

	cmp bx,word ptr [ii] ; BX (R1) = i-number of current file
	je short iget_5
iget_1:
	;push ax ; **
	xor ah,ah ; mov ah,0
	mov al,byte ptr [imod]
	and al,al ; has i-node of current file been modified ?	
	jz short iget_2 ; no
	xor al,al ; mov al,0
	mov byte ptr [imod],al 
	push bx ; * inode number
	mov bx,word ptr [ii]	
	inc al ; mov al,1
	; ax = 1 = write
	call icalc
	pop bx ; *
	jc short iget_4
	; 27/11/2019
	xor ax,ax
iget_2:
	and bx,bx
	jz short iget_3 ; get current inode
	mov word ptr [ii],bx		
	; ax = 0 = read
	call icalc
iget_3:
	mov bx,word ptr [ii]
iget_4:
	;pop ax ; **
iget_5:
	retn

iget	endp

icalc 	proc near
	; 14/12/2019
	; 12/12/2019
	; 09/11/2019
	; 07/11/2019 - Retro UNIX 386 v2 
	;	       (for hard disk file system)
	; 07/09/2019
	; 02/09/2019
	; 01/09/2019 - Retro UNIX 386 v2
	; 17/08/2012
	; 16/08/2012
	; 15/08/2012
	; 14/08/2012
	; 13/08/2012
        ; 15/07/2012
     	; 14/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	;; AX=R0, BX=R1, CX=R3, DX=R5 
	; 0 = read, 1 = write
	; RETRO UNIX v1 FS
	; initialization/format version
	;
        ; i-node is located in block (i+47)/16 and
	; begins 32*(i+47) mod 16 bytes from its start
	;; return => if cf=1 error number in [Error]

	; input -> ax = 0 -> read, 1 = write

	; 09/11/2019
	; modified registers: ax,dx,cx,bx

	;add bx,47 ; add 47 to inode number, 15/8/2012
	;push bx ; R1 -> -(SP)
	;shr bx,1 ; divide by 16
	;shr bx,1
	;shr bx,1
	;shr bx,1

	; 07/11/2019
	; inode 1 (1st inode) is on sector 4 
	; sector 0 : boot sector
	; sector 1 : super block
	; sector 2 : inodes map
	; sector 3 : free blocks map
	; sector 4 to 35 : inodes (32 sectors)

	; 08/09/2019
	;add bx,31 ; add 31 to inode number
	;push bx
	;shr bx,1  ; divide by 8	
	;shr bx,1
	;shr bx,1
		; bx contains block number of block in which
		; inode exists

	; 07/11/2019 - Hard disk file system (RUFS v2)
	; inode table start sector: [systm.sb_InodeTblAddr]
	
	dec bx ; 0 based inode number

	mov dx,bx ; 12/12/2012

	shr bx,1
	shr bx,1
	shr bx,1
		; bx = sector offset (8 inodes per sector)

	; 09/11/2019
	mov byte ptr [I_rw],al ; 0 = read, 1 = write

	cmp byte ptr [I_valid],0
	jna short icalc_0
	cmp bx,word ptr [I_sector]
	je short icalc_1
	
	mov byte ptr [I_valid],0 ; inode sector validation, invalid
icalc_0:
	; 12/12/2019
	push dx ; inode number - 1 ; (root dir inode = 1)

	mov word ptr [I_sector],bx
	mov ax,word ptr [systm.sb_InodeTblAddr]
	mov dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0
	
	add ax,bx
	adc dx,0

	mov bx,offset I_buffer
	call dskrd
	pop dx ; 14/8/2012
	jc short icalc_5

	mov byte ptr [I_valid],1 ; inode sector validation, valid
icalc_1:
	;and dx,0Fh	; (i+47) mod 16
	;shl dx,1
	;shl dx,1
	;shl dx,1
	;shl dx,1
	;shl dx,1
		; DX = 32 * ((i+47) mod 16)	
		; DX (R5) points to first word in i-node i.

	and dx,07h	; (i+31) mod 8
	jz short @f ; 29/09/2019 
	;shl dx,1
	;shl dx,1
	;shl dx,1
	;shl dx,1	
	;shl dx,1
	;shl dx,1
	mov cl,6
	shl dx,cl	
		; DX = 64 * ((i+31) mod 8)
		; DX points to first word in i-node i.	
@@:
	; 14/8/2012
	push di
	push si
	
	mov si,offset inode ; 14/8/2012
		; inode is address of first word of current inode
	;mov cx,16 ; CX = R3
	; 02/09/2019
	mov cx,32 ; inode size/2 for Retro UNIX 386 v2 (& UNIX v7)	

	; 09/09/2019
	;push ax

	;mov di,offset Buffer ; 16/8/2012
	;mov di,word ptr [buff_o] ; 02/09/2019 - Retro UNIX 386 v2

	; 04/12/2019
	mov di,offset I_buffer

	add di,dx ; 13/8/2012

 	;and ax,ax
	;jz short icalc_3 ; 0 = read (and copy i-node to memory) 

	cmp byte ptr [I_rw],0
	jna short icalc_3 ; read	
icalc_2:
	; 14/8/2012
	; overwrite old i-node (in buffer to be written)
	rep movsw

	; 14/12/2019
	mov ax,word ptr [systm.sb_InodeTblAddr]
	mov dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0
	
	add ax,word ptr [I_sector]
	adc dx,0

	mov bx,offset I_buffer

	; 31/10/2012

	call dskwr
	jmp short icalc_4
icalc_3:
	xchg si,di ; 14/8/2012		
	; copy new i-node into inode area of (core) memory
	rep movsw
icalc_4:
	;pop ax ; 09/09/2019
	; 14/8/2012
	pop si
	pop di

	; OUTPUTS ->
	; inode 
	; DX/R5 (internal), BX/R1 (internal), CX/R3 (internal) 
icalc_5:	
	retn		 

icalc 	endp

dskwr	proc near
	; 14/01/2020
	; 08/01/2020
	; 18/12/2019
	; 27/11/2019
	; 07/11/2019 - Retro UNIX 386 v2
	;
	; Disk (Sector) Write
	;
	; INPUT: 
	;	DX:AX = Sector address/number (LBA)
	;	ES:BX = Buffer address
	; OUTPUT:
	;	CF = 0 -> succeeded
	;	CF = 1 -> Error, Error code in [Error]
	;
	; Modified registers: cx, (si)

	mov byte ptr [rw],3 ; write
	jmp short diskio

dskwr	endp
	
dskrd	proc near
	; 14/01/2020
	; 08/01/2020
	; 18/12/2019
	; 03/12/2019
	; 27/11/2019
	; 07/11/2019
	; 06/11/2019 - Retro UNIX 386 v2
	;
	; Disk (Sector) Read
	;
	; INPUT: 
	;	DX:AX = Sector address/number (LBA)
	;	ES:BX = Buffer address
	; OUTPUT:
	;	CF = 0 -> succeeded
	;	CF = 1 -> Error, Error code in [Error]
	;
	; Modified registers: cx

	mov byte ptr [rw],2 ; read
diskio:
	mov word ptr [Error],0
	mov byte ptr [RetryCount],4

	; 07/11/2019
	push dx ; *
	push ax ; **

	;add ax,word ptr [systm.sb_HiddenSects]
	;adc dx,word ptr [systm.sb_HiddenSects+2] 
	; 14/01/2020
	add ax,word ptr [systm.sb_BootSectAddr]	  ; Hidden Sectors, lw
	adc dx,word ptr [systm.sb_BootSectAddr+2] ; Hidden Sectors, hw

	cmp byte ptr [drive],90h ; Hard disk image file sign
	;je short image_file_rw
	jnb image_file_rw

	; 27/11/2019
	and dx,dx
	jz short chs_read_write

	cmp dx,word ptr [CHS_limit+2]
	ja short lba_read_write
	jb short chs_read_write

	cmp ax,word ptr [CHS_limit]
	ja short lba_read_write
chs_read_write:
	push dx ; ***
	push ax ; ****

	push bx ; *****
	
	mov cx,word ptr [sectors] ; spt
	call div32	; Special 32 bit divide !!!
                        ; To fix large disk problem.
                        ; by Erdogan Tan
                        ; (October 20th, 1999)

	mov cx,bx	;Sector (zero based)
	inc cx		; To make it 1 based
	push cx ; ******
	mov cx,word ptr [heads]
	call div32
	mov dh,bl	; bx = head (max. 255)
	pop cx ; ******	; ax=cylinder, dh=head, cx=sector

	pop bx ; *****	; es:bx = buffer address

	mov dl,byte ptr [drive]	; physical drive number
	mov ch,al
	ror ah,1
	ror ah,1
	;and cl,63
	or cl,ah

	mov ah,byte ptr [rw] ; 2 = read, 3 = write
 	mov al,1 ; 1 sector
	int 13h		; ROM BIOS Service func ( ah ) = 2
			; Read disk sectors
			; AL-sec num CH-track CL-sec
   			; DH-head DL-drive ES:BX-buffer
 			; CF-flag AH-stat AL-sec read
			; If CF = 1 then (If AH > 0)

	jnc short chs_rw_ok	
	
	; dl = physical drive number

	dec byte ptr [RetryCount]
	jz short chs_err_retn

	cmp ah,09h ; DMA crossed 64K segment boundary
	je short chs_err_retn

	xor ah,ah ; reset
	int 13h

	pop ax ; ****
	pop dx ; ***

	jmp short chs_read_write ; read (or write) again	

chs_err_retn:
lba_err_retn:
	stc
	mov byte ptr [Error],ah
chs_rw_ok:
	; 12/12/2019
lba_rw_ok: ; 03/12/2019
	pop ax ; ****
	pop dx ; ***
@@:
	pop ax ; **
	pop dx ; *
	retn

lba_read_write:
	; 08/01/2020
	; 18/12/2019
	; 12/12/2019
	; 07/11/2019
	; LBA read/write
	or byte ptr [rw],40h ; 42h = read, 43h = write
	; 06/11/2019
	cmp byte ptr [systm.sb_LBA_rw],1
	jnb short lba_read_again

lba_not_ready:
	mov byte ptr [Error],0FFh
	;stc
	;retn
	jmp short @b ; 08/01/2020

lba_read_again:
	push dx ; ***
	push ax ; ****

	; 18/12/2019
	push si ; *****

	; 03/12/2019
	xor cx,cx
	;push 0
	push cx ; 6*
	;push 0
	push cx ; 7*

	push dx ; 8*
	push ax ; 9*
	push es ; 10*
	push bx ; 11*

	;push 1
	mov cl,1
	push cx ; 12*
	;push 16
	mov cl,16
	push cx ; 13*
	
	mov si,sp
	mov dl,byte ptr [drive]
	mov ah,byte ptr [rw]
	xor al,al ; verify off
	int 13h
	pop ax ; 13*
	pop ax ; 12*
	pop bx ; 11*
	pop es ; 10*
	pop ax ; 9*
	pop dx ; 8*
	pop cx ; 7*
	pop cx ; 6*
	; 18/12/2019*
	pop si ; *****
	jnc short lba_rw_ok	

	; dl = physical drive number

	dec byte ptr [RetryCount]
	jz short lba_err_retn

	cmp ah,09h ; DMA crossed 64K segment boundary
	je short lba_err_retn

	xor ah,ah ; reset
	int 13h

	pop ax ; ****
	pop dx ; ***

	jmp short lba_read_again ; read again

dskrd	endp	
	
image_file_rw proc near
	; 07/11/2019 - Retro UNIX 386 v2 (hard disk images)
	; 09/09/2019 - Retro UNIX 386 v2 (floppy disk images)

	; reading/writing a block (sector) from/to disk image file

	; INPUT:
	; 	dx:ax = sector/block number (max. 4194303, 3FFFFFh)
        ;	es:bx = buffer address (es = ds)
	;       [img_file_handle] = file handle
	;	number of bytes to be written = 512
	;
	; OUTPUT:
	;	CF = 0 -> succeeded
	;	CF = 1 -> Error, Error code in [Error]
	;
	; Modified registers: cx

	cmp dx,3Fh  ; Max. 2GB disk image file (32bit seek limit)
	jna short hd_img_file_rw

invalid_disk_image_sector:
	stc
image_file_rw_err:
	mov byte ptr [Error],0FFh
image_file_rw_ok:
	pop ax ; **
	pop dx ; *
	retn
	
hd_img_file_rw:
	push dx ; ***
	push ax ; ****
	push bx ; ***** ; buffer offset
	push bx ; ******
	; dx:ax = sector number 
	mov cx,512
	call mul32
	; dx:ax = byte offset
	mov cx,dx
	mov dx,ax
	sub al,al ; specified offset is from the beginning of the file
	mov ah,42h ; seek (move file pointer)	
	mov bx,word ptr [img_file_handle]
	int 21h
	pop dx ; ****** ; buffer offset
	jc short hd_img_file_rw_err

	;mov bx,word ptr [img_file_handle]
	mov cx,512
	; ds:dx = buffer offset
	mov ah,3Dh ; 3Fh = read from file, 40h = write to file
	add ah,byte ptr [rw] ; 2 = read, 3 = write 
	int 21h
	jc short hd_img_file_rw_err
	cmp byte ptr [rw],2  ; read ?
	je short hd_img_file_rw_ok
	cmp ax,cx ; ax = actually written bytes
	jnb short hd_img_file_rw_ok

hd_img_file_rw_err:
	;pop bx ; *****
	;pop ax ; ****
	;pop dx ; ***
	;jmp short image_file_rw_err

	mov byte ptr [Error],0FFh

hd_img_file_rw_ok:
	pop bx ; *****
	pop ax ; ****
	pop dx ; ***
	jmp short image_file_rw_ok

image_file_rw endp

setimod proc near
	; 18/12/2019 - Retro DOS 386 v2 
	; 13/08/2012
	; 21/07/2012
	; 14/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	;; AX=R0, BX=R1, CX=R3, DX=R5 
	; [SP] = Argument 1, 0 = read, 1 = write
	; RETRO UNIX v1 FS
	; initialization/format version

	; 21/7/2012
	push dx
	push ax

	mov byte ptr [imod],1

	; Erdogan Tan 14-7-2012
	call epoch
	
	mov word ptr [i_mtim],ax  ; file modif. time
	mov word ptr [i_mtim]+2,dx

	; 18/12/2019 - Last access (inode modif.) time 
	mov word ptr [i_ltim],ax
	mov word ptr [i_ltim]+2,dx

	; 21/7/2012
	cmp word ptr [i_ctim],0
	ja short @f
	cmp word ptr [i_ctim]+2,0
	ja short @f

	mov word ptr [i_ctim],ax
	mov word ptr [i_ctim]+2,dx
@@:
	; 21/7/2012
	pop ax
	pop dx

	retn

setimod endp

imap	proc near
	; 02/01/2020
	; 07/11/2019 (dx <-> bx)
	; 02/09/2019 - Retro UNIX 386 v2
	; 21/08/2012
	; 05/08/2012
     	; 16/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	; RETRO UNIX v1 FS
	; initialization/format version
	;
	; get the byte that the allocation bit 
	; for the i-number contained in R1

	; INPUT: 
	;	dx = inode number
	;	(im_buffer)
	; OUTPUT:
	;	bx = address of the byte contains allocation bit
	;	ax has 1 at calculated bit position

	; Modified registers: ax,bx,cx, (si, di)
	
	; 02/01/2020
	push dx ; *
	
	dec dx	; zero based inode number

	push dx ; **

	;mov bx,dx   ; DX = R2, BX = R1 (input, i-number)
	;sub bx,41  ; DX has i-41
	; 02/09/2019 - Retro UNIX 386 v2
	;dec bx ; zero based inode number

	; 02/01/2020
	
	shr dx,1
	shr dx,1
	shr dx,1    ; DX has byte offset of zero based inode number
		    ; from the start of the (inode) map

	mov bl,dh ; <= 31
	shr bl,1 ; inode map sector (index/offset) number (<= 15)	
	xor bh,bh

	and dx,511 ; now DX points to byte offset in im_buffer

	; 02/01/2020
	cmp bx,word ptr [im_sector]
	je short imap_3 ; current inode map sector is same sector

	push bx ; *** new inode map sector
	push dx ; **** byte offset in inode map sector buffer
	push bx ; ***** new inode map sector

	mov bx,offset im_buffer		

	test byte ptr [smod],2 ; inode map modified 
	jz short imap_2 ; read inode map sector

	mov ax,word ptr [systm.sb_InodeMapAddr]
	mov dx,word ptr [systm.sb_InodeMapAddr+2]
	add ax,word ptr [im_sector]
	adc dx,0
	call dskwr
	jnc short imap_1
	pop bx ; *****
	pop bx ; ****
	pop dx ; ***
imap_0:
	pop dx ; **
	pop dx ; *
	retn
imap_1:
	and byte ptr [smod],0FDh ; reset bit 1
imap_2:
	; 02/01/2020
	pop ax ; *****
	sub dx,dx
	add ax,word ptr [systm.sb_InodeMapAddr]
	adc dx,word ptr [systm.sb_InodeMapAddr+2]
	call dskrd
	pop dx ; **** byte offset
	pop bx ; *** sector
	jc short imap_0
	mov word ptr [im_sector],bx ; ***
imap_3:
	; dx = byte offset in inode map sector buffer
	; bx = inode map sector (index/offset)
	
	pop cx ; ** ; inode number - 1

	mov ax,1    ;	
	and cl,7    ; CX has zero based inode number mod 8
		    ;	  to get the bit position	 		 	
	jz short imap_4 ; 21/8/2012
	shl ax,cl   ; AX has 1 in the calculated bit position
imap_4:
	;; 5/8/2012		
	;add bx,word ptr [systm] ; superblock free map size + 4
	;; 21/8/2012
	;add bx,offset systm+4 ; is inode map offset in superblock

	; 02/01/2020 - Retro UNIX 386 v2
	mov bx,offset im_buffer
	add bx,dx

	; CX (R3) used internally 	
	; AX (MQ) has a 1 in the calculated bit position
	; BX (R2) has byte address of the byte with allocation bit
imap_5:
	pop dx ; *
@@:	; 05/04/2022
	retn

imap	endp
	
writei	proc near
	; 05/04/2022
	; 30/12/2019
	; 18/12/2019
	; 09/11/2019
	; 07/11/2019
	; 03/09/2019 - Retro UNIX 386 v2
	; 31/10/2012
	; 18/08/2012
	; 17/07/2012
	; BX = R1, i-number
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	;; AX=R0, BX=R1, i-number 
	; RETRO UNIX v1 FS
	; initialization/format version
	;
	; writei: write file
	;
	; 8086 CPU & IBM PC architecture modifications by Erdogan Tan 
	;; return => if cf=1 error number in [Error]

	; input:
	; BX = R1 = I-Number
	; u.count = byte count
	; u.base = user buffer (offset)
	; u.fofp = (pointer to) current file offset
 
	xor ax,ax ; 0		   ; clr u.nread	
	mov word ptr [u_nread],ax  ; clear the number of bytes transmitted during
				   ; read or write calls 
				   ; tst u.count		
	cmp word ptr [u_count],ax  ; test the byte count specified by the user
	;;ja short write_1 ; 1f	   ; bgt 1f / any bytes to output; yes, branch
	;;retn			   ; rts 0 / no, return - no writing to do
	;jna short @f
	jna short @b ; 03/09/2019

write_1:
	;cmp bx,40		; cmp r1,$40.
				; does the i-node number indicate a special file?
	;ja  short dskw_0	; bgt dskw / no, branch to standard file output
;@@:
;	retn

;	shl	bx,1		; asl r1 
				; yes, calculate the index into the special file

;	cmp bx,offset write_3 - offset writei_2 + 2
;	ja short writei_error

;	jmp word ptr [write_2][BX]-2 ; *1f-2(r1)
				; jump table and jump to the appropriate routine
;write_2: ;1
;	dw offset wtty	; tty
;	dw offset wmem	; mem
;	dw offset wfd ; fd0
;	dw offset wfd ; fd1
;	dw offset whd ; hd0
;	dw offset whd ; hd1
;	dw offset whd ; hd2
;	dw offset whd ; hd3
;	dw offset xmtt ; tty0
;	dw offset xmtt ; tty1
;	dw offset xmtt ; tty2
;	dw offset xmtt ; tty3
;	dw offset xmtt ; tty4
;	dw offset xmtt ; tty5
;	dw offset xmtt ; tty6
;	dw offset xmtt ; tty7
;	dw offset w1pr ; lpr
; writei_3:	
;	dw offset writei_error

;wtty: ; write to concole tty
;	retn
;wmem: ; transfer characters from a user area of core to memory
;	retn

;wfd:  ; write to floppy disk (drive)	
;	retn

;whd:  ; write to hard/fixed disk (drive)	
;	retn
;wlpr  ; write to printer
;	retn
	
;xmtt:
;	retn

writei 	endp

dskw	proc near
	; 05/04/2022
	; 30/12/2019
	; 18/12/2019
	; 04/12/2019
	; 07/11/2019
	; 03/09/2019 - Retro UNIX 386 v2
	; 01/03/2013
	; 31/10/2012
	; 19/08/2012
	; 30/07/2012
     	; 17/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	; dskw: write routine for non-special files
	;
	; RETRO UNIX v1 FS
	; initialization/format version
	;
	; write data to a file
	;
	; BX (R1) = I-node number

dskw_0:
	push di
	push si

	push bx	; save i-number on stack

	mov byte ptr [Error],0FFh ; 03/09/2019

	call iget 	; jsr	r0,iget
		  	; write i-node out (if modified), read i-node 'r1'
		        ; into i-node area of core
	;jc dskw_5 ; 01/03/2013
	; 05/04/2022
	jnc short @f
	jmp dskw_5
@@:
	; 03/09/2019 - Retro UNIX 386 v2
	mov al,byte ptr [i_flgs+1]
	test al,80h		; regular file ?
	jnz short dskw_8	; yes
	test al,20h 		; device file ?
	;;jnz short dskw_7	; yes
	;jz short dskw_8 ; 07/09/2019
	;;test al,40h		; directory ?
	;;jnz short dskw_8	; yes
	jnz short dskw_7 ; 04/12/2019	
dskw_8:
	mov si,word ptr [u_fofp]
	;mov dx,word ptr [si] 
			; mov *u.fofp,r2 
			; put the file offset [(u.off) or the offset in
		        ; the fsp entry for this file] in r2
	;add dx,word ptr [u_count]
			; add u.count,r2 
			; no. of bytes to be written + file offset is
		        ; put in r2

	;cmp dx,word ptr [i_size] ; cmp r2,i.size
			; is this greater than the present size of
			; the file?
	;jna short dskw_1 ; blos 1f / no, branch

	;mov word ptr [i_size],dx ; mov	r2,i.size 
			; yes, increase the file size to file offset +
			; no. of data bytes

	; 09/11/2019 - 32 bit file size
	mov ax,word ptr [si]
	mov dx,word ptr [si+2]

	; 30/12/2019
	add ax,word ptr [u_count]
	adc dx,0
	
	cmp dx,word ptr [i_size+2]
	je short dskw_9	
	jb short dskw_1
	mov word ptr [i_size+2],dx 
	jmp short dskw_10
dskw_7:
	stc
	jmp short dskw_5
dskw_9:
	cmp ax,word ptr [i_size]
	jna short dskw_1	
dskw_10:  
	mov word ptr [i_size],ax

	call setimod	; jsr r0,setimod 
			; set imod=1 (i.e., core inode has been
			; modified), stuff time of modification into
			; core image of i-node
dskw_1: ; 1
	call mget	; jsr r0,mget 
			; get the block no. in which to write the next data
  		        ; byte
			; AX = R1 = Block Number
	jc short dskw_5 ; 01/03/2013
	; 09/11/2019
	; dx:ax = Block/Sector number

	; Check current sector in the buffer
	
	cmp byte ptr [buff_m],1
	jb short dskw_12

	mov cx,word ptr [buff_s]
	mov bx,word ptr [buff_s+2]
	;mov si,cx
	;or si,bx
	;jz short dskw_12 ; buff_s = 0 is invalid	
	;cmp si,0FFFFh	; buff_s = 0FFFFh is invalid
	;je short dskw_12

	; write buffer content if sector is not same

	cmp cx,ax
	jne short dskw_11
	cmp bx,dx
	je short dskw_12	
dskw_11:
	push dx
	push ax
	;mov ax,word ptr [buff_s]
	;mov dx,word ptr [buff_s+2]
	; 18/12/2019
	mov ax,cx
	mov dx,bx
	mov bx,offset Buffer
	call dskwr
	pop ax
	pop dx
	jc short dskw_5
	mov byte ptr [buff_m],0
dskw_12:
	mov si,word ptr [u_fofp]
	mov bx,word ptr [si]
	and bx,1FFh  	; bit	*u.fofp,$777
			; test the lower 9 bits of the file offset
	mov bx,offset Buffer ; 04/12/2019
	jnz short dskw_2 ; bne 2f 
			; if its non-zero, branch; if zero, file offset = 0,
		   	; 512, 1024,...(i.e., start of new block)
	cmp word ptr [u_count],512 ; cmp u.count,$512.
			; if zero, is there enough data to fill an
		        ; entire block? (i.e., no. of
	;jnb short dskw_6 ; bhis 3f / bytes to be written greater than 512.? 
			; Yes, branch. / Don't have to read block
	jnb short dskw_3 ; 09/11/2019
dskw_2: ; 2
	; in as no past info. is to be saved (the entire block will be
        ; overwritten).

	; 09/11/2019
	;mov bx,offset Buffer ; 04/12/2019
	cmp ax,word ptr [buff_s]
	;jne short dskw_13
	jne short dskw_6
	cmp dx,word ptr [buff_s+2]
	je short dskw_3
;dskw_13:
dskw_6:
	;mov bx,ax	; R1 (block number)
	call dskrd 	; jsr r0,dskrd 
			; no, must retain old info.. Hence, read block 'r1'
		        ; into an I/O buffer
	;jc short dskw_5 ; 01/03/2013
	; 04/12/2019
	jnc short dskw_3
dskw_5:
	pop bx

	pop si
	pop di

	retn

dskw_3: ; 3
	;call wslot

	call sioreg

	; SI = user data offset (r1)
	; DI = sector (I/O) buffer offset (r2)
	; CX = byte count (r3)

dskw_4: ; 2
	rep movsb

	mov byte ptr [buff_m],1
	; 09/11/2019
	mov word ptr [buff_s],ax
	mov word ptr [buff_s+2],dx
	;mov bx,offset Buffer ; offset sector_buffer	
	call dskwr ; jsr r0,dskwr / write the block and the i-node
        jc short dskw_5

	mov byte ptr [buff_m],0
	
        cmp word ptr [u_count],0 ; any more data to write?
	;ja dskw_1 ; 1b 	 ; yes, branch
	; 05/04/2022
	jna short @f
	jmp dskw_1
@@:
	mov byte ptr [Error],0 ; 03/09/2019
	; 04/12/2019
	jmp short dskw_5
;dskw_5:
;	pop bx
;
;	pop si
;	pop di
;
;	retn

;dskw_6:
	;cmp byte ptr [buff_m],1
	;jb short dskw_3
	;mov bx,offset Buffer ; 09/11/2019
	;call dskwr
	;jc short dskw_5
	;mov word ptr [buff_s],ax ; block number from mget procedure
	;mov word ptr [buff_s+2],dx 
	;cmp byte ptr [buff_m],0
	;jmp short dskw_3

dskw 	endp

; 04/09/2019 - Retro UNIX 386 v2
;sizing: db 0FFh ; (initial value is not important!)

mget 	proc near
	; 05/04/2022
	; 13/01/2020
	; 18/12/2019
	; 12/12/2019
	; 08/12/2019
	; 05/12/2019
	; get sector/block address for current file pointer
	; (if file pointer points a number greater than current file size, 
	; a new -empty- sector/block will be created/written) 
	; 04/12/2019
	; 01/12/2019
	; 27/11/2019
	; 13/11/2019
	; 09/11/2019 - Retro UNIX 386 v2 (32 bit sector/block numbers)
	; 15/09/2019
	; 04/09/2019 (simplified for initialization floppy disk)
	; 03/09/2019 - Retro UNIX 386 v2
	; 05/03/2013
	; 01/03/2013
	; 31/10/2012
	; 20/10/2012
	; 19/08/2012
	; 13/08/2012
	; 27/07/2012
     	; 21/07/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/06/1972
	;; return -> AX=R1
	; RETRO UNIX v1 FS
	; initialization/format version
	; cf -> 1 = error (no free block)

	; 01/12/2019
	; INPUT:
	;	[u_fofp] = pointer to file pointer
	; OUTPUT:
	;	dx:ax = sector/block address
	;
	; Modified registers: ax,dx,cx,bx

	;push bx
	;push cx
	;push dx
	 ;; contents of bx,cx,dx will be destroyed 
mget_0:
	mov bx,word ptr [u_fofp]
	; 13/11/2019
	mov ax,word ptr [bx]
	mov dx,word ptr [bx+2]
			
	; 31/10/2012
	;mov bx,word ptr [u_fofp]
	;mov ax,word ptr [bx]

	;mov bl,ah  ; div ax by 256
	;xor bh,bh

	; BX = R2
        ;test word ptr [i_flgs],4096 ; 1000h
			  	     ; is this a large or small file
	;jnz short mget_8 ; 4f ; large file

	test byte ptr [i_flgs+1],16 ; 10h
	;jnz mget_8 ; not small file
	; 05/04/2022
	jz short @f
	jmp mget_8
@@:
        ;test bl,0F0h ; !0Fh  ; branch if BX (R2) >= 16                    
	;jnz short mget_3 ; 3f

	; 13/11/2019
	; small file size limit = 5120 bytes (1400h)

	or dx,dx
	jnz short mget_3  ; requested file offset > 65535

	; 13/11/2019
	; Retro UNIX 386 v2 disk inode contains..
	; (if large file flag is clear -not set-)
	; 10 direct disk block/sector dword pointers

	; 15/09/2019
	;cmp bl,14h
	cmp ah,14h
	jnb short mget_3 ; 3f ; requested offset >= 5120

	; 01/12/2019
	mov bl,ah  ; div ax by 256
	xor bh,bh

	;and bl,0Eh  ; clear all bits but bits 1,2,3
	and bl,1Eh ; 15/09/2019 ; clear all bits but bits 1,2,3,4

	shl bl,1 ; 03/09/2019 - Retro UNIX 386 v2
	mov ax,word ptr [bx+i_dskp] ; AX = R1, physical block number
	; 09/11/2019
	mov dx,word ptr [bx+i_dskp+2] ; DX = hw of physical block number
	or ax,ax
	jnz short mget_2 ; if physical block number is zero
			 ; then we need a new block for file
	; 09/11/2019
	or dx,dx
	jnz short mget_2
	call alloc	 ; allocate a new block for this file	
			 ; AX (R1) = Block number
	;jc mget_6	 ; cf -> 1 & ax = 0 -> no free block
	; 05/04/2022
	jnc short @f
	jmp mget_6
@@:
	; 09/11/2019
	mov word ptr [bx+i_dskp],ax
	mov word ptr [bx+i_dskp+2],dx 

	call setimod

	; 09/09/2019
	;mov byte ptr [buff_c],1

	; 08/12/2019
	mov bx,offset Buffer

	; 04/12/2019
	cmp byte ptr [buff_m],0 ; buffer modified ?
	jna short mget_1

	push dx
	push ax
	mov ax,word ptr [buff_s]
	mov dx,word ptr [buff_s+2]
	;mov bx,offset Buffer
	call dskwr
	pop ax
	pop dx
	jc short mget_2
	mov byte ptr [buff_m],0 ; reset buffer modified sign
mget_1:
	mov word ptr [buff_s],ax
	mov word ptr [buff_s+2],dx

	; 05/12/2019
	mov byte ptr [buff_m],1 ; set buffer modified sign

	call clear	; clear Buffer

	; BX = offset Buffer

	call dskwr
	jc short mget_2

	mov byte ptr [buff_m],0 ; reset buffer modified sign

mget_2: ; 2
	; AX (R1) = Physical block number
	; DX = high word of block/sector number ; 09/11/2019

	;pop dx
	;pop cx
	;pop bx

	retn

mget_3: ; 3
	; adding on block which changes small file to large file
	call alloc
	jc short mget_6 ; 01/03/2013 
	; call wslot  ; setup I/O buffer for write
	;	   ; R5 points to the first data word in buffer

	; push ds
	; pop es

	; 13/01/2020
	mov bx,offset Buffer

	; 18/12/2019
	cmp byte ptr [buff_m],0 ; buffer modified ?
	jna short mget_4

	push dx
	push ax
	mov ax,word ptr [buff_s]
	mov dx,word ptr [buff_s+2]
	;mov bx,offset Buffer ; 13/01/2020
	call dskwr
	pop ax
	pop dx
	jc short mget_2
	mov byte ptr [buff_m],0 ; reset buffer modified sign
mget_4:
	; 09/11/2019
	mov word ptr [buff_s],ax  ; Block/Sector number
	mov word ptr [buff_s+2],dx

	; 13/01/2020 (si, di)
	;push si
	;push di
	push ax

	;mov cx,8  ; R3, transfer old physical block pointers
		   ; into new indirect block area for the new
		   ; large file	
	mov cx,10 ; 15/09/2019	
	;mov di,word ptr [buff_o]  ; 03/09/2019
	; 09/11/2019
	;mov di,offset Buffer ; BX = R5
	mov si,offset i_dskp 

	mov di,bx ; 13/01/2020
	
	xor ax,ax ; mov ax,0
mget_5: ; 1
	movsw
	movsw ; 07/09/2019
	mov word ptr [si-2],ax ; 13/01/2020
	mov word ptr [si-4],ax ; 07/09/2019
	loop mget_5
	
	;;mov cl,256-8 ; clear rest of data buffer
	;mov cl,256-16 ; 07/09/2019
	mov cl,256-20 ; 15/09/2019	

	rep stosw

	pop ax
	;pop di ; 13/01/2020
	;pop si

	mov byte ptr [buff_m],1 ; modified

	call dskwr
	jc short mget_7 ; 01/03/2013	

	mov word ptr [i_dskp],ax
	; 09/11/2019
	mov word ptr [i_dskp+2],dx
	mov byte ptr [buff_m],0 ; reset modified sign
	
	;or word ptr [i_flgs],4096 ; 1000h

	or byte ptr [i_flgs+1],10000b ; 10h ; 16

	call setimod

	; 15/09/2019
	jmp mget_0

mget_6: 
	mov word ptr [Error],err_NOFREEBLOCK
	
	;pop dx
	;pop cx
	;pop bx
mget_7:	
	retn

mget_8:	; 4 ; large file
	
	; 13/11/2019

	; Retro UNIX 386 v2 disk inode contains..
	; (if large file flag is set)
	; 8 indirect disk block/sector dword pointers
	; +1 double indirect disk block/sector dword pointers
	; +1 triple indirect disk block/sector dword pointers

	; check indirect pointers limit (as file offset)
	; 8*128 = 1024 blocks (or sectors) or 512 KB
	; check dx (file offset hw) value
	
	cmp dx,08h ; 524288 = 080000h
	jnb short mget_9 ; check double indirect limit

	mov byte ptr [level],1
	mov cx,9
	call shr32
	; ax = sector offset (flat)
	; dx = 0
	mov cl,al
	and cl,127
	mov byte ptr [level+1],cl
	mov cl,7	
	;call shr32
	shr ax,cl  ; 13/01/2020
	; ax = indirect pointer index (<= 7)
	; dx = 0
	;mov byte ptr [level+2],al
	; 15/11/2019
	shl al,1
	shl al,1
	mov si,ax
	jmp short mget_11
mget_9:
	; check double indirect pointers limit (as file offset)
	; (128*128)+1024 = 16384+1024 blocks or 8 MB + 512 KB
	; check dx (file offset hw) value
	
	cmp dx,88h ; 8912896 = 880000h	
	jnb short mget_10

	mov byte ptr [level],2
	sub dx,08h
	mov cx,9
	call shr32
	; dx:ax = sector offset (flat)
	mov cl,al
	and cl,127
	mov byte ptr [level+1],cl
	mov cl,7	
	call shr32
	; ax = indirect pointer index (<= 127)
	; dx = 0
	mov byte ptr [level+2],al
	mov si,8*4 ; 32
	jmp short mget_11
mget_10:
	; 13/11/2019
	; triple indirect pointers ; 8912386 to 1082654210 bytes
	mov si,9*4 ; 36

	mov byte ptr [level],3
	sub dx,88h
	mov cx,9
	call shr32
	; dx:ax = sector offset (flat)
	mov cl,al
	and cl,127
	mov byte ptr [level+1],cl
	mov cl,7	
	call shr32
	; ax = indirect pointer index (<= 127)
	; dx = 0
	mov dl,al
	and dl,127
	mov byte ptr [level+2],dl
	mov cl,7
	shr ax,cl
	mov byte ptr [level+3],al
mget_11:
	mov ax,word ptr [si+i_dskp]
	mov dx,word ptr [si+i_dskp+2]

	mov bx,offset Buffer  ; 01/12/2019
	
	or ax,ax
	jnz short mget_15 ; if physical block number is zero
			 ; then we need a new block for file
	or dx,dx
	jnz short mget_15

	call alloc	 ; allocate a new block for this file	
			 ; AX (R1) = Block number
	;jc mget_6	 ; cf -> 1 & ax = 0 -> no free block
	;;jc short mget_12 ; 30/12/2019
	; 05/04/2022
	jnc short @f
	jmp mget_6	
@@:
	mov word ptr [si+i_dskp],ax
	mov word ptr [si+i_dskp+2],dx

	; 18/12/2019
	cmp byte ptr [buff_m],0 ; buffer modified ?
	jna short mget_13

mget_20: ; 13/01/2020
	push dx
	push ax
	mov ax,word ptr [buff_s]
	mov dx,word ptr [buff_s+2]
	;mov bx,offset Buffer
	call dskwr
	pop ax
	pop dx
	;jc short mget_12
	;mov byte ptr [buff_m],0 ; reset buffer modified sign
	jnc short mget_14
mget_12:
	retn
mget_13:
	mov byte ptr [buff_m],1  ; buffer modified
mget_14:
	call clear ; clear buffer

	call setimod

	mov word ptr [buff_s],ax
	mov word ptr [buff_s+2],dx
	;mov bx,offset Buffer
	call dskwr
	jc short mget_12
	; 01/12/2019
	mov byte ptr [buff_m],0 ; reset buffer modified sign
	jmp short mget_18
mget_15:
	; 13/11/2019
	cmp ax,word ptr [buff_s]
	jne short mget_16	
	cmp dx,word ptr [buff_s+2]
	je short mget_18
mget_16:	
	cmp byte ptr [buff_m],0 ; buffer modified ?
	jna short mget_17

	; 01/12/2019		
	push dx
	push ax
	mov ax,word ptr [buff_s]
	mov dx,word ptr [buff_s+2]
	;mov bx,offset Buffer
	call dskwr
	pop ax
	pop dx
	jc short mget_12
	mov byte ptr [buff_m],0 ; reset buffer modified sign
mget_17:
	;mov bx,offset Buffer	
	call dskrd
	jc short mget_12

	mov word ptr [buff_s],ax
	mov word ptr [buff_s+2],dx
mget_18:
	; 13/01/2020
	cmp byte ptr [level],0
	jna short mget_12
	
	;dec byte ptr [level]
	;jz short mget_12

	mov bl,byte ptr [level]
	xor bh,bh
	mov bl,byte ptr [bx+level] ; sector pointer offset
	shl bl,1
	shl bx,1 ; 13/01/2020

	; 13/01/2020
	dec byte ptr [level]
	
	; 13/01/2020
	mov si,offset Buffer

	; 01/12/2019
	add bx,si ; offset Buffer ; 13/01/2020

	mov ax,word ptr [bx]
	mov dx,word ptr [bx+2]

	; 01/12/2019
	or ax,ax
	;jnz short mget_16 ; if physical block number is zero
			 ; then we need a new block for file
	jnz short mget_21 ; 13/01/2020
	or dx,dx
	;jnz short mget_16
	jz short mget_23 ; 13/01/2020
mget_21:
	; 13/01/2020
	mov bx,si ; offset Buffer

	cmp byte ptr [level],0
	;ja short mget_16
	ja short mget_17
mget_22:
	retn
mget_23:
	call alloc	 ; allocate a new block for this file	
			 ; AX (R1) = Block number
	;jc mget_6	; cf -> 1 & ax = 0 -> no free block
	; 13/01/2020
	;jnc short mget_19
	;
	;retn
	jc short mget_22	
mget_19:
	; 12/12/2019
	mov word ptr [bx],ax
	mov word ptr [bx+2],dx

	; 13/01/2020
	; 18/12/2019
	mov byte ptr [buff_m],1 ; set buffer modified sign
	;call setimod
	;mov bx,offset Buffer
	mov bx,si ; offset Buffer ; 13/01/2020
	;jmp mget_13
	; 13/01/2020
	jmp mget_20
	
mget	endp

alloc 	proc near
	; 03/01/2020
	; 06/12/2019
	; 27/11/2019
	; 07/11/2019
	; 28/10/2019
	; 24/10/2019 - Retro UNIX 386 v2 (UNIXHDFS)
	; allocate disk sector
	;; input -> none
	;; output -> DX:AX = allocated sector/block number
	;; 	cf = 1 -> could not be allocated
	; Modified registers: none (except ax,dx)

	mov ax,word ptr [systm.sb_FreeBlocks] 	
	mov dx,word ptr [systm.sb_FreeBlocks+2]
	or ax,dx
	jnz short alloc_0
alloc_err:
	stc
	retn
alloc_0:
	; 06/12/2019
	mov ax,word ptr [systm.sb_FirstFreeBlk]
	mov dx,word ptr [systm.sb_FirstFreeBlk+2]

	push cx ; *
	push bx ; **

	mov cx,dx
	mov bx,ax
	cmp cx,0FFFFh
	jb short alloc_1
	cmp bx,0FFFFh
	jb short alloc_2	
	xor ax,ax
	xor dx,dx
	; dx:ax = fbm sector index (as start sector) = 0
	xor bx,bx ; bit 0
	jmp short alloc_3	
alloc_1:
	or cx,bx
	jz short alloc_3
alloc_2:		
	mov cx,12   ; 512 block alloc bytes per free block map sector
	call shr32  ; (4096 block alloc bits per fm sector)	
	and bx,0Fh  ; start bit position of fbm buffer byte
alloc_3:
	; free map sector (index) in dx:ax

	push bx ; ***

	cmp ax,word ptr [fm_sector]
	jne short alloc_4
	cmp dx,word ptr [fm_sector+2]
	je short alloc_6	
alloc_4:
	; 12/12/2019
	mov bx,offset fbm_buffer

	test byte ptr [smod],1 ; free map modified flag
	jz short alloc_5

	push dx ; ****
	push ax ; *****

	;call sync ; writes current free map sector/block
	;	  ; and updates super block

	; 12/12/2019
	mov ax,word ptr [fm_sector]
	mov dx,word ptr [fm_sector+2]
	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]
	;mov bx,offset fbm_buffer
	call dskwr

	pop ax ; *****
	pop dx ; ****	
	jc short alloc_9 ; 07/11/2019

	; 12/12/2019
	and byte ptr [smod],0FEh ; reset free map modified flag
alloc_5:
	; 07/11/2019
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx
alloc_17: ; 27/11/2019
	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov bx,offset fbm_buffer 	
	call dskrd
	jc short alloc_9 ; 07/11/2019

	; 07/11/2019	
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	mov word ptr [fm_sector],ax
	mov word ptr [fm_sector+2],dx
alloc_6:	
	mov bx,offset fbm_buffer-2
	; 28/10/2019
alloc_7: 
	; dx:ax = [fm_sector] ; 07/11/2019
	inc bx
	inc bx 
	;mov dx,word ptr [bx]
	;or dx,dx
	; 07/11/2019
	mov cx,word ptr [bx]
	or cx,cx
	jnz short alloc_10 ; branch if any free blocks in this word	
	cmp bx,offset fbm_buffer+510 
	jb short alloc_7
	;mov ax,word ptr [fm_sector]
	;mov dx,word ptr [fm_sector+2]
	add ax,1
	adc dx,0
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx

	;mov cx,12  ; 512 block alloc bytes per free block map sector
	;call shl32 ; (4096 block alloc bits per fm sector)
	;cmp dx,word ptr [systm.sb_TotalSects+2]
	;jb short alloc_8
	;ja short alloc_3	
	;cmp ax,word ptr [systm.sb_TotalSects]
 	;jnb short alloc_9

	mov cx,9  ; 512 block alloc bytes per free block map sector
	call shl32 ; (4096 block alloc bits per fm sector)
	cmp dx,word ptr [systm.sb_FreeMapSize+2]
	jb short alloc_8
	;ja short alloc_3
	ja alloc_19 ; 03/01/2020		
	cmp ax,word ptr [systm.sb_FreeMapSize]
 	jb short alloc_8
	; 03/01/2020
alloc_19:
	xor ax,ax ; 07/11/2019
	xor dx,dx
	stc	; cf=1 --> error: no free blocks/sectors
	;jmp short alloc_9
	; 03/01/2020
alloc_9:
	pop bx ; ***
	pop bx ; **
	pop cx ; *
	retn

alloc_8:
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	mov bx,offset fbm_buffer  ; 12/12/2019
	jmp short alloc_17 ; 27/11/2019

alloc_10:
	; 07/11/2019
	mov dx,cx

	; 28/10/2019	
	; dx = value of (current) fbm buffer byte

	pop cx ; ***
	mov ax,1
	;and cx,0Fh ; bit position
	;jz short alloc_11
	and cx,cx
	jz short alloc_11
	shr dx,cl
	shl ax,cl ; 27/11/2019
alloc_11:
	; 08/12/2019
	mov byte ptr [bitpos],cl ; 0 to 15

	; BX = byte offset in fbm_buffer
	; AX = bit position to be reset
	shr dx,1
	jc short alloc_12
	; 08/12/2019
	cmp cl,0Fh ; 15
	jnb short alloc_18
	shl ax,1
	inc cl
	jmp short alloc_11
alloc_18:
	; 08/12/2019
	mov ax,word ptr [fm_sector]
	mov dx,word ptr [fm_sector+2]
	jmp short alloc_7
alloc_12:
	not ax ; requested bit is 0 (allocated), others are 1 (free)
	and word ptr [bx],ax

	mov ax,word ptr [fm_sector]
	mov dx,word ptr [fm_sector+2]
	mov cx,ax
	or cx,dx
	jz short alloc_13
	mov cx,12  ; convert fbm sector index number to bit offset
	call shl32
	; dx:ax = total bits number of sectors before current fbm sector
alloc_13:	
	; 08/12/2019
	sub bx,offset fbm_buffer
 	; bx = byte offset in fbm_buffer
	jz short alloc_14 
	shl bx,1
	shl bx,1
	shl bx,1
	; bx = bit offset (0 to 4080)
alloc_14:
	; ch = 0
	mov cl,byte ptr [bitpos] ; 0 to 15
	add bx,cx
	;	
	add ax,bx
	adc dx,0
	; dx:ax = bit offset from start of fbm = allocated sector number 

	or byte ptr [smod],1  ; set free map modified flag
			      ; in super block	

	; Do not change invalid count of free blocks
	; (0FFFFFFFFh) value here! -otherwise it may be a mistake!-
	mov cx,word ptr [systm.sb_FreeBlocks] 	
	and cx,word ptr [systm.sb_FreeBlocks+2]
	cmp cx,0FFFFh 
	jnb short alloc_15

	sub word ptr [systm.sb_FreeBlocks],1 	
	sbb word ptr [systm.sb_FreeBlocks+2],0

	or byte ptr [smod],0F0h  ; set sb modified flag
alloc_15:
	cmp ax,word ptr [systm.sb_FirstFreeBlk]
	jne short alloc_16
	cmp dx,word ptr [systm.sb_FirstFreeBlk+2]
	jne short alloc_16

	; set first free block to the next sector (in order)
	add word ptr [systm.sb_FirstFreeBlk],1
	adc word ptr [systm.sb_FirstFreeBlk+2],0

	or byte ptr [smod],0F0h  ; set sb modified flag
alloc_16:
	; 06/12/2019
	clc
	pop bx ; **
	pop cx ; *
	; DX:AX = Block/Sector number
	retn

alloc   endp

; 16/10/2019 - Retro UNIX 386 v2 (UNIXHDFS.COM for v2, initial workings)

free	proc near
	; 08/12/2019
	; 27/11/2019
	; 07/11/2019
	; 28/10/2019
	; 24/10/2019
	; release disk sector
	; 16/10/2019 - Retro UNIX 386 v2 (UNIXHDFS)
	; DX:AX = Sector to be freed/released
	; Modified registers: cx,bx ; 07/11/2019
	;		cf = 1 -> error

	; Calculate free map sector for requested sector in dx:ax

	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx

	mov bx,ax
	and bx,4095 ; 28/10/2019

	;mov cx,4096
	;call div32
	mov cx,12
	call shr32

	cmp ax,word ptr [fm_sector]
	jne short free_1
	cmp dx,word ptr [fm_sector+2]
	je short free_3
free_1:
	; 27/11/2019
	push bx ; ***

	; 10/12/2019
	mov bx,offset fbm_buffer  

	test byte ptr [smod],1 ; free map modified flag
	jz short free_2

	push dx ; **
	push ax ; *

	;call sync ; writes current free map sector/block
	;	   ; and updates super block

	; 10/12/2019
	mov ax,word ptr [fm_sector]
	mov dx,word ptr [fm_sector+2]

	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov bx,offset fbm_buffer 	
	call dskwr
	jc short free_9

	and byte ptr [smod],0FEh ; reset bit 0
free_9:
	pop ax ; *
	pop dx ; **
	jc short free_7
free_2:
	; 07/11/2019
	push dx ; **
	push ax ; *

	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov bx,offset fbm_buffer ; 10/12/2019	
	call dskrd
	
	pop ax ; *
	pop dx ; **
	pop bx ; ***
	jc short free_8

	; 07/11/2019
	mov word ptr [fm_sector],ax
	mov word ptr [fm_sector+2],dx
free_3:	
	mov cx,bx
	mov ax,1	
	and cx,0Fh
	jz short free_4
	shl ax,cl
free_4:
	shr bx,1
	shr bx,1
	shr bx,1
	shr bx,1

	shl bx,1 ; byte ofset with word boundary
	add bx,offset fbm_buffer

	; BX = byte offset in fbm_buffer
	; DX = bit position to be set

	or word ptr [bx],ax

	or byte ptr [smod],1  ; set free map modified flag
			      ; in super block
	; 28/10/2019
	; Do not change invalid count of free blocks
	; (0FFFFFFFFh) value here! -otherwise it may be a mistake!-
	mov cx,word ptr [systm.sb_FreeBlocks] 	
	and cx,word ptr [systm.sb_FreeBlocks+2]
	cmp cx,0FFFFh 
	jnb short free_5
	
	add word ptr [systm.sb_FreeBlocks],1 	
	adc word ptr [systm.sb_FreeBlocks+2],0
	; 28/10/2019
	or byte ptr [smod],0F0h  ; set sb modified flag
free_5:	
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]		

	cmp dx,word ptr [systm.sb_FirstFreeBlk+2]
	ja short free_8
	jb short free_6	

	cmp ax,word ptr [systm.sb_FirstFreeBlk]
	jnb short free_8
free_6:
	mov word ptr [systm.sb_FirstFreeBlk],ax
	mov word ptr [systm.sb_FirstFreeBlk+2],dx
	; 28/10/2019
	or byte ptr [smod],0F0h  ; set sb modified flag
	;clc
	retn
free_7:
	pop bx ; ***
free_8:
	retn

free	endp

; 04/12/2019

;alloc_inode proc near
;	; 29/10/2019
;	; 24/10/2019
;	; 23/10/2019 - Retro UNIX 386 v2 (UNIXHDFS)
;	; allocate inode
;	;; input -> none
;	;; output -> AX = inode number (which is allocated)
;	;; 	cf = 1 -> could not be allocated
;	; Modified registers: ax,dx,cx,bx
;	
;	mov ax,word ptr [systm.sb_FirstFreeIno]
;	inc ax ; 0FFFFh -> 0
;	jz short alloci_0
;	dec ax
;	jz short alloci_0
;	dec ax
;	; 24/10/2019
;	mov cl,12 
;	shr ax,cl  ; 4096 inodes per inode map sector
;		   ; (8 inodes per byte)	
;alloci_0:
;	cmp ax,word ptr [im_sector] ; zero based im sector index
;	je short alloci_2
;alloci_12:
;	test byte ptr [smod],2 ; inode map modified flag
;	jz short alloci_1
;
;	mov word ptr [save_ax],ax
;
;	call sync ; writes current inode map sector/block
;		  ; and updates super block
;	jc short alloci_6
;
;	mov ax,word ptr [save_ax]
;alloci_1:
;	xor dx,dx
;	add ax,word ptr [systm.sb_InodeMapAddr]	 
;	adc dx,word ptr [systm.sb_InodeMapAddr+2]
;	mov bx,offset im_buffer
;	call dskrd
;	jc short alloci_6
;	
;	mov ax,word ptr [save_ax]
;	mov word ptr [im_sector],ax	
;alloci_2:	
;	mov bx,offset im_buffer-2
;	xor ax,ax ; 0
;alloci_3: 
;	inc bx
;	inc bx 
;	mov dx,word ptr [bx]
;	cmp dx,0FFFFh
;	jb short alloci_7 ; branch if any free inodes in this word	
;	; 29/10/2019
;	cmp bx,offset im_buffer+510
;	jb short alloci_3
;	mov ax,word ptr [im_sector]
;	; 24/10/2019
;	push ax
;	or ax,ax
;	jz short alloci_13
;	mov cl,12 
;	shl ax,cl  ; 4096 inodes per inode map sector
;		   ; (8 inodes per byte)	
;alloci_13:
;	inc ax
;	mov word ptr [save_dx],ax
;	cmp ax,word ptr [systm.sb_InodeCount]
;	pop ax
;	jnb short alloci_14
;	inc ax
;	;jmp short alloci_0
;	jmp short alloci_12 ; 24/10/2019 
;	
;alloci_14:
;	;jmp short panic ; no free inodes
;
;	xor ax,ax
;	stc		; cf=1 --> error: no free inodes
;alloci_6:
;	retn	
;alloci_7:
;	mov ax,1	; bit 0
;	mov cx,ax
;alloci_8:
;	shr dx,1	; Branch when a free inode found,
;			; bit for inode k is in byte k/8 
;			; in bit k (mod 8) 	
;	jnc short alloci_9
;	shl ax,1	; set bir position number (for next)    
;	inc cx
;	jmp short alloci_8
;	
;alloci_9:
;	; ax : masking bit is '1' and others are '0'
;	or word ptr [bx],ax
;	; 1 -> allocated
;	mov ax,word ptr [im_sector]
;	or ax,ax
;	jz short alloci_10
;	push cx
;	mov cl,12 
;	shl ax,cl  ; 4096 inodes per inode map sector
;		   ; (8 inodes per byte)	
;	pop cx
;alloci_10:
;	add ax,cx
;	; ax = inode number
;
;	; 24/10/2019
;	; set first free inode search value
;	inc ax
;	mov word ptr [systm.sb_FirstFreeIno],ax
;	dec ax
;
;	cmp word ptr [systm.sb_FreeInodes],0FFFFh ; invalid	
;	jnb short alloci_11
;
;	dec word ptr [systm.sb_FreeInodes]	 
;alloci_11:
;freei_4:
;	or byte ptr [smod],0F2h ; inode map & sb modified sign
;freei_0: 
;	retn
;
;alloc_inode endp

;free_inode proc near
;	; 24/10/2019 - Retro UNIX 386 v2 (UNIXHDFS)
;	; deallocate inode
;	;; input ->
;	; AX = Inode number to be freed/released/deallocated
;	;; output ->
;	;	cf = 1 -> error
;	; Modified registers: ax,dx,cx,bx
;	
;	; check valid inode number
;	cmp ax,1
;	jb short freei_0
;	cmp word ptr [systm.sb_InodeCount],ax
;	jb short freei_0
;
;	; calculate inode map sector for this inode
;
;	dec ax  ; zero based inode number
;
;	mov word ptr [save_dx],ax
;
;	mov cl,12 
;	shr ax,cl  ; 4096 inodes per inode map sector
;		   ; (8 inodes per byte)	
;
;	cmp ax,word ptr [im_sector] ; zero based im sector index
;	je short freei_2
;	test byte ptr [smod],2 ; inode map modified flag
;	jz short freei_1
;
;	mov word ptr [save_ax],ax
;
;	call sync ; writes current inode map sector/block
;		  ; and updates super block
;	jc short freei_0
;
;	mov ax,word ptr [save_ax]
;freei_1:
;	xor dx,dx
;	add ax,word ptr [systm.sb_InodeMapAddr]	 
;	adc dx,word ptr [systm.sb_InodeMapAddr+2]
;	mov bx,offset im_buffer
;	call dskrd
;	jc short freei_0
; 	
;	mov ax,word ptr [save_ax]
;	mov word ptr [im_sector],ax	
;freei_2:	
;	mov bx,offset im_buffer
;	; get offset of the inode in the inode map buffer
;	mov dx,word ptr [save_dx]
;	mov cx,dx
;	and dx,4095
;	shr dx,1
;	shr dx,1
;	shr dx,1
;	shr dx,1 ; offset in words
;	shl dx,1 ; offset in bytes (word boundary)
;	mov ax,1
;	and cx,0Fh
;	shl ax,cl
;	not ax ; only selected bit position is 0, others are 1
;	add bx,dx
;	and word ptr [bx],ax ; reset bit position of the inode 
;	
;	mov ax,word ptr [save_dx]
;	inc ax ; 1 based inode number
;	
;	cmp word ptr [systm.sb_FirstFreeIno],ax
;	jna short freei_3
;
;	mov word ptr [systm.sb_FirstFreeIno],ax
;freei_3:
;	cmp word ptr [systm.sb_FreeInodes],0FFFFh ; invalid	
;	jnb freei_4
;
;	inc word ptr [systm.sb_FreeInodes]
;
;	jmp freei_4	 
;;freei_4:
;;	or byte ptr [smod],0F2h ; inode map & sb modified sign 
;;	retn
;
;free_inode endp  

clear	proc near
	; 13/11/2019 - Retro UNIX 386 v2
	; (Data) Buffer clearing
	
	push di
	push cx
	push ax

 	xor ax,ax

	mov di,offset Buffer
	mov cx,256 
	rep stosw

	pop ax
	pop cx
	pop di	
	
	retn

clear	endp

sioreg proc near
	; 30/12/2019
	; 09/11/2019 - Retro UNIX 386 v2 (for hard disk fs)
	; 04/09/2019 - RETRO UNIX v0.3 (Retro UNIX 386 v2)
	; 16/12/2012
	; 31/10/2012
	; 19/08/2012
	; 04/08/2012
	; Erdogan Tan - RETRO UNIX v0.1
	; input -> R5 (DX) = sector buffer (data) address
	;	   *u.fofp = file offset, to start writing
	;	   u.base = address of 1st byte of user data
	;	   u.count = byte count to be transferred
	;	   u.nread = number of bytes written out
	;		     previously.
	; output -> *u.fofp = last (written) byte + 1
	;	   u.count = number of bytes of data left
	;		     to be transferred.			
	;	   u.nread = updated to include the count
	;		    of bytes to be transferred.
	;	   R1 (SI) = address of 1st byte of data
	;	   R2 (DI) = specifies the byte in IO 
	;	            sector (I/O) buffer. (Offset)	
	;	   R3 (CX) = number of bytes of data to be
	;		    transferred to/from sector (I/O)
	;		    buffer.		

	;mov dx,offset Buffer	; R5
	; 31/10/2012	
        mov si,word ptr [u_fofp] ; mov	*u.fofp,r2 
	mov di,word ptr [si]	; file offset (in bytes) is moved to r2
	mov cx,di		; mov	r2,r3 / and also to r3

	or cx,0FE00h ; set bits 9...15 of file offset in R3
	and di,1FFh ; calculate file offset mod 512

	; 09/11/2019
	; 19/08/2012
	add di,offset Buffer ; DI/r2 now points to 1st byte in buffer
			     ; where data is to be placed
        
	;mov si,word ptr [u_base] ; address of data is in r1
	neg cx ; 512 - file offset(mod512) in R3 (cx)
			; the number of free bytes in the file block
        cmp cx,word ptr [u_count] ; compare this with the number of data bytes
				  ; to be written to the file
	jna short @f ; 2f
			   ; if less than branch. Use the number of free bytes
		           ; in the file block as the number to be written
        mov cx,word ptr [u_count]
			   ; if greater than, use the number of data bytes
		           ; as the number to be written
@@:	; 2
;sioreg_1:		
	add word ptr [u_nread],cx ; r3 + number of bytes
			; xmitted during write is put into
                        ; u.nread
        sub word ptr [u_count],cx
			; u.count = no. of bytes that still must be
		        ; written or read
	; 30/12/2019
	;mov si,word ptr [u_fofp]
        add word ptr [si],cx ; new file offset = number 
			; of bytes done + old file offset
	; 09/11/2019
	adc word ptr [si+2],0
	; Note: word ptr [u_base] + cx must not over 65535	

	; 16/12/2012 BugFix
	mov si,word ptr [u_base] ; address of data is in SI/r1

        add word ptr [u_base],cx ; u.base points to 1st of remaining
			; data bytes
	retn

sioreg	endp

epoch proc near
	; 07/11/2019 - Retro UNIX 386 v2 (mul32, bx->cx)
	; 21/07/2012
	; 15/07/2012
	; 14/07/2012		
	; Erdogan Tan - RETRO UNIX v0.1
	; compute current date and time as UNIX Epoch/Time
	; UNIX Epoch: seconds since 1/1/1970 00:00:00

	; 21/7/2012
	push bx
	push cx

	mov ah,02h                      ; Return Current Time
        int 1Ah
        xchg ch,cl
        mov word ptr [hour],cx
        xchg dh,dl
        mov word ptr [second],dx

        mov ah,04h                      ; Return Current Date
        int 1Ah
        xchg ch,cl
        mov word ptr [year],cx
        xchg dh,dl
        mov word ptr [month],dx

	mov cx,3030h

	mov al,byte ptr [hour] ; Hour
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
		
	mov byte ptr [hour],al

	mov al,byte ptr [hour]+1 ; Minute
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
		
	mov byte ptr [minute],al

	mov al,byte ptr [second] ; Second
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
		
	mov byte ptr [second],al

	
	mov ax,word ptr [year] ; Year (century)
        push ax
	   ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
		
	mov ah,100
	mul ah
	mov word ptr [year],ax

	pop ax
	mov al,ah
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
		
	add word ptr [year],ax


	mov al,byte ptr [month] ; Month
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov byte ptr [month],al	


	mov al,byte ptr [month]+1 ; Day
           ; AL <= BCD number)
        db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad ; AX= AH*10+AL

	mov byte ptr [Day],al
	
convert_to_epoch:

	mov dx,word ptr [year]
	sub dx,1970
	mov ax,365
	mul dx
	xor bh,bh
	mov bl,byte ptr [month]
	dec bl
	shl bl,1
	mov cx,word ptr DMonth[BX]
	mov bl,byte ptr [Day]
	dec bl
	
	add ax,cx
	adc dx,0
	add ax,bx
	adc dx,0
				; DX:AX = days since 1/1/1970
	mov cx,word ptr [year]
	sub cx,1969
	shr cx,1
	shr cx,1		
		; (year-1969)/4
	add ax,cx
	adc dx,0
				; + leap days since 1/1/1970

	cmp byte ptr [month],2  ; if past february
	jna short @f
	mov cx,word ptr [year]
	and cx,3 ; year mod 4
	jnz short @f		
				; and if leap year
	add ax,1 ; add this year's leap day (february 29)
	adc dx,0
@@: 			; compute seconds since 1/1/1970
	;mov bx,24
	;call proc_mul32
	; 07/11/2019
	mov cx,24
	call mul32
		
	mov bl,byte ptr [hour]
	add ax,bx
	adc dx,0
	
	;mov bx,60
	;call proc_mul32
	; 07/11/2019
	mov cx,60
	call mul32

	mov bl,byte ptr [minute]
	add ax,bx
	adc dx,0
	
	;mov bx,60
	;call proc_mul32
	; 07/11/2019
	;mov cx,60
	call mul32

	mov bl,byte ptr [second]
	add ax,bx
 	adc dx,0

	; DX:AX -> seconds since 1/1/1970 00:00:00

	; 21/7/2012
	pop cx
	pop bx
	
	retn

epoch endp

year:   dw 1970
month:  dw 1
day:    dw 1
hour:   dw 0
minute: dw 0
second: dw 0

DMonth:
dw 0
dw 31
dw 59
dw 90
dw 120
dw 151
dw 181
dw 212
dw 243
dw 273
dw 304
dw 334
;dw 365

; 08/12/2019
bitpos: db 0

Error: db 0 ; Hardware error
       db 0 ; Software error	  

smod: db 0
imod: db 0

ii: dw 0

; 07/11/2019
;drive: db 0 ; 03/12/2019
rw: db 0

; 19/12/2019
;dotodot:
;dw 3030h
;db "h"
;db 0Dh,0Ah,0

; 04/12/2019
align 4
level: dd 0  ; level, level+1, level+2, level+3 

; 30/11/2019
I_rw: db 0     ; inode table read (0) or write (1)	
I_valid: db 0  ; inode table buffer is valid (1) or not (0)
I_sector: dw 0 ; inode table sector index/offset
I_buffer: db 512 dup(0) ; inode table sector buffer