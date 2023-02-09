; Retro UNIX 386 v2.0 - Modified UNIX v7 inode model - 28/09/2019
; ****************************************************************************
; RUBS3LBA.ASM - Retro UNIX 386 v2 Boot Sector (09/01/2020) for 71h partitions 
; ----------------------------------------------------------------------------
; RUFSBS2.ASM - Retro UNIX 386 v2 Boot Sector (29/09/2019) 
; ----------------------------------------------------------------------------
; RUFSBS.ASM - Retro UNIX 8086 v1  (+ 386 v1) Boot Sector (04/12/2015) 
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; 1.44 MB Floppy Disk 
; Bootable Unix (RUFS) File System - Boot Sector Code 
;
; UNIXFDFS.ASM -> include 'RUFSBS.BIN' (04/12/2015)
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ****************************************************************************

; 18/12/2019
; (CHS and LBA read versions are separated due to 512 bytes size limit)
; RUBS3LBA.ASM - DISK READ CODE FOR LBA READ ; 23/12/2019
; RUBS3CHS.ASM - DISK READ CODE FOR CHS READ ; 23/12/2019

I_BUFFER equ 900h ; 18/12/2019
BF_BUFFER equ 700h
BF_INODE equ 600h
; 28/09/2019
inode_flgs equ 600h
inode_nlks equ 602h
inode_uid equ 604h
inode_gid equ 606h
inode_size_h equ 607h
inode_size equ 608h
inode_dskp equ 60Ch
inode_atim equ 634h
inode_mtim equ 638h
inode_ctime equ 63Ch

; 18/12/2019
boot_file_load_address equ 8000h 
boot_file_segment equ 800h

; 18/12/2019
sb_InodeTblAddr equ 40 ; Inode table start sector address (offset)
		       ; pointer in super block 	 

RUFS_BS		SEGMENT PUBLIC 'CODE'
		assume cs:RUFS_BS,ds:RUFS_BS,es:RUFS_BS,ss:RUFS_BS

		org 7C00h

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±              PROCEDURE unixbootsector
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

unixbootsector  proc    near

START:
		jmp     short @f

; RETRO UNIX 386 v2 Hard Disk FS - BootSector Identification (Data) Block
; 18-12-2019 RUFS Boot Sector for (partition ID) 71h hard disk partitions

bsFSystemID:    db 'RUFS'
bsVolumeSerial: dd 0
		db 'hd'
bsDriveNumber:  db 80h
bsReserved:     db 1	; 0 = 512 bytes/sector CHS, 1 = LBA
bsSecPerTrack:  db 63	; 63,17
bsHeads:        db 255	; 8 to 255 (may be 2 to 255)
bsTracks:       dw 1023	; 1 to 1024 (bsTracks)
bs_BF_I_number: dw 0	; startup (boot) file inode number
		db '@'	; magic byte !
		db 71h	; Retro UNIX Partition Identifier (as signature)
bsHiddenSectors: dd 0	; Absolute/LBA address of (this) Boot Sector
@@:  
		mov ax,cs ; cs = 0
		mov ds,ax
		mov es,ax

		cli
		mov ss,ax
		mov sp,0FFFEh
		sti

		;mov ax,word ptr [bs_BF_I_number]

		;or ax,ax              
                ;jz loc_no_bootable_disk

		; 18/12/2019
		cmp word ptr [bs_BF_I_number],ax ; 0
		;jna loc_no_bootable_disk
		ja short @f ; 23/12/2019

loc_no_bootable_disk:
		mov si, offset msg_press_any_key
		call print_string
		xor ax,ax
		int 16h
		int 19h  
@@:
		mov byte ptr [bsDriveNumber],dl ; from INT 19h
read_super_block:
		;mov ax,1
		inc ax ; 19/12/2019
		xor dx,dx
		; dx:ax = Super Block address (sector num. after Boot Sector)
		mov bx,7E00h  ; next sector (just after boot sector)
		call dskrd
		jc short loc_unix_bl_error_j ; 19/12/2019
load_boot_file:
i_get:
		mov ax,word ptr [bs_BF_I_number]
		shr ax,1
		shr ax,1
		shr ax,1
		    ; ax = sector offset for that boot file inode
		xor dx,dx	
		add ax,word ptr [bx+sb_InodeTblAddr]
		adc dx,word ptr [bx+sb_InodeTblAddr+2]

		; 18/12/2019
		mov bx,I_BUFFER ; 900h ; inode sector buffer
					 ; for boot file
		call dskrd
                jc short loc_unix_bl_error ; 18/12/2019

		mov al,byte ptr [bs_BF_I_number]
		dec al ; zero based inode number
		and ax,07h ; (zero bazed) inode number mod 8
				
		;mov cl,6 ; * 64
		;shl ax, cl
		; 19/12/2019
		;mov cl,64
		mov cx,64 ; 23/12/2019
		mul cl			
		  	; AX = inode ofset
		mov si,bx ; I_BUFFER
		add si,ax			

		mov di,BF_INODE

		;mov cx,32
		
		; copy new i-node into inode area of (core) memory
		;rep movsw
		rep movsb ; 19/12/2019
check_inode_flags:
		; Must be regular file, executable and must not be directory!
		; 23/12/2019
		mov bx,inode_flgs
		mov ax,word ptr [bx]
		and ax,0E040h ; 8000h+4000h+2000h+0040h
		cmp ax,8040h ; regular file + executable by owner
		jne short loc_unix_bl_error
	
		mov bx,inode_size ; inode size (dd) offset 
		
		mov ax,word ptr [bx]
		mov dx,word ptr [bx+2] ; Must be 0

		and ax,ax
		jz short loc_unix_bl_error

		or dx,dx
		;jnz short loc_unix_bl_error ; boot file must be <= 64KB
 
				      ; NOTE: Max. 32KB - stack space (64 bytes?)
				      ; is usable with this BS code.
				      ; (Because start offset is 8000h.) 
		jz short @f ; 07/01/2020	

		; 07/01/2020
loc_unix_bl_error_j:
		; 18/12/2019
		jmp short loc_unix_bl_error
@@:
		; 19/12/2019
		;mov word ptr [b_base],boot_file_load_address
read_i:
		;mov word ptr [b_nread],dx ; accumulated number of bytes transmitted
read_i_1:
		;mov ax,word ptr [inode_size] ; file size in bytes
		sub ax,word ptr [b_off] ; subtract file offset
		;jna read_i_retn
		; 23/12/2019
		ja short @f
		jmp read_i_retn
@@:		
		cmp ax,word ptr [b_count] 
				; are enough bytes left in file to carry out read
		jnb short read_i_2
		mov word ptr [b_count],ax
read_i_2:
		;;call m_get  ; returns physical block number of block in file 
		;;   	    ; where offset points
m_get:
		; 07/01/2020
		;; 22/12/2013
		; 05/03/2013
		; 03/03/2013
		; 28/10/2012
		; 20/10/2012
		; Boot sector version of "mget" procedure
		; Derived from (original) UNIX v1 source code
		; PRELIMINARY release of Unix Implementation Document, 
		; 20/6/1972
m_get_0:
		mov bl,byte ptr [b_off]+1
		xor bh,bh
		
		; 28/09/2019
		mov si,inode_flgs+1
		test byte ptr [si],16 ; 10h
				     ; is this a large or small file
		jnz short m_get_1 ; large file
		
		cmp bl,14h ; file size > 5120 bytes
		jnb short m_get_1 ; large_file ; 29/09/2019

		;and bl,0Eh  ; clear all bits but bits 1,2,3
		and bl,1Eh  ; 28/09/2019 ; clear all bits but bits 1,2,3,4

		shl bl,1 ; 28/09/2019 - Retro UNIX 386 v2 (dword block addresses)

		; 19/12/2019
		;mov ax,word ptr [bx+inode_dskp] 
		;mov dx,word ptr [bx+inode_dskp+2] 
		;		; dx:ax = physical block number
		;jmp short m_get_4

		; 19/12/2019
		add bx,inode_dskp
		jmp short m_get_4

check_disk_addr:
		; 19/12/2019
		mov ax,word ptr [bx]
		mov dx,word ptr [bx+2]
		and ax,ax
		jnz short chk_daddr_retn
		or dx,dx
		jnz short chk_daddr_retn
		pop ax ; near call retur address

;loc_unix_bl_error_j:
;		; 18/12/2019
;		jmp loc_unix_bl_error

		; 07/01/2020
loc_unix_bl_error:
		mov si, offset unix_bfl_error_msg
		call print_string
NeverComeHere:
		jmp short NeverComeHere

chk_daddr_retn:
		retn

m_get_1: 	; large file
		; 07/01/2020
		; 05/03/2013
		; 03/03/2013
		;mov ax,bx
		;mov cx,256
		;xor dx,dx
		;div cx
		;and bx,1FEh  ; zero all bit but 1,2,3,4,5,6,7,8
		      ; gives offset in indirect block
		;push bx              ; 
		;mov bx,ax  ; calculate offset in i-node for pointer
		    	; to proper indirect block
		;and bx,0Eh
		;mov ax,word ptr [inode_dskp+bx]
		and bl,0FEh
		shl bx,1 ; 28/09/2019 - Retro UNIX 386 v2 (dword block addr) 
		;;push bx
		;;mov di,bx 
		;mov si,bx ; 22/12/2013
		mov di,bx ; 19/12/2019

		; 23/12/2019
		;mov si,I_BUFFER ; 0:900h ; indirect block buffer

		; 07/01/2020
		add di,I_BUFFER

		cmp byte ptr [b_buffer],0 ; initial value = 0
		ja short m_get_3

		; 23/12/2019
		mov bx,inode_dskp

		; 23/12/2019
		call check_disk_addr
		; OK! valid disk address
;m_get_2:
		; 07/01/2020
		mov bx, I_BUFFER ; = 900h

		call dskrd ; read indirect block
		;jc short loc_unix_bl_error ; 22/12/2013
		jc short loc_unix_bl_error_j ; 18/12/2019 ; 07/01/2020
	
		; 18/12/2019
		inc byte ptr [b_buffer]  ; 1 = indirect block buffer is loaded
m_get_3:
		mov bx,di ; 07/01/2020

		;;pop ax
		;;add bx,ax ; R5, first word of indirect block
		;;add bx,si ; 22/12/2013
		; 07/01/2020
		;add bx,di ; 19/12/2019

		; 18/12/2019
		;mov ax,word ptr [bx]
		;mov dx,word ptr [bx+2]
;m_get_4:
		;or ax,ax
		;jnz short m_get_5
		;and dx,dx
		;jz short loc_unix_bl_error
m_get_4:
		; 19/12/2019
		call check_disk_addr
		; OK! valid disk address
m_get_5:
		mov bx,BF_BUFFER ; 700h ; Boot File buffer	
		; DX:AX = Physical block number
		call dskrd ; read in block
			    ; BX points to 1st dword of data in buffer
		jc short loc_unix_bl_error_j		
readi_sioreg:
		mov si,word ptr [b_off] ; R2
		mov cx,si ; cx = R3, si = R2
		or cx,0FE00h ; set bits 9...15 of file offset in R3
		and si,1FFh ; calculate file offset mod 512
		add si,bx ; offset Buffer ; si now points to 1st byte in buffer
		   	  ; where data is to be placed
		mov di,word ptr [b_base] ; R1
		neg cx ; 512 - file offset(mod512) in R3 (cx)
		cmp cx,word ptr [b_count]
		jna short @f ; 2f

		mov cx,word ptr [b_count]
@@:
		add word ptr [b_nread],cx ; r3 + number of bytes
			; xmitted during write is put into
			; u_nread
		sub word ptr [b_count],cx
		add word ptr [b_base],cx ; points to 1st of remaining
			; data bytes
		add word ptr [b_off],cx ; new file offset = number 
			; of bytes done + old file offset
; end of readi_sioreg
		; DI = file (user data) offset
		; SI = sector (I/O) buffer offset
		; CX = byte count 
		
		rep movsb

		cmp word ptr [b_count],cx ; 0
                jna short read_i_retn

		; 23/12/2019
		mov bx,inode_size
		mov ax,word ptr [bx] ; file size in bytes

		jmp read_i_1

read_i_retn: ; 22/12/2013

loc_launch_bootfile:
		mov si,offset msg_CRLF
		call print_string

		; 09/01/2020
		mov dl,byte ptr [bsDriveNumber]
	       
		mov ax,boot_file_segment ; 800h 
		mov ds,ax
		mov es,ax
		cli
		mov ss,ax
		;mov sp,0FFFEh 
		sti

	     ;; MASM.EXE don't accept
	     ;; jmp 0800h:0000h
	     ;; for OP Code: EA00000008
	     ;	db 0EAh
	     ;	dw 0
	     ;	dw 0800h
	
		; 18/12/2019
		push es
		push cx
		retf
		  
;NeverComeHere:  jmp short NeverComeHere 

		; 07/01/2020
;loc_unix_bl_error:
;		mov si, offset unix_bfl_error_msg
;		call print_string
;NeverComeHere:
;		jmp short NeverComeHere

unixbootsector endp

dskrd proc near
		; 23/12/2019
		; 18/12/2019
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
		; Modified registers: cx, ax, dx

		mov byte ptr [retry_count],4

		;push dx ; *
		;push ax ; **

		add ax,word ptr [bsHiddenSectors]
		adc dx,word ptr [bsHiddenSectors+2]

		;push si ; *** ; 23/12/2019

		; 23/12/2019
		mov si,offset device_address_packet

		mov word ptr [si+4],bx  ; dap_buffer_off
		mov word ptr [si+6],es  ; dap_buffer_seg
		mov word ptr [si+8],ax  ; dap_lba_lw
		mov word ptr [si+10],dx ; dap_lba_hw

		mov dl,byte ptr [bsDriveNumber]
lba_read:
		; 23/12/2019
		; 18/12/2019
		; LBA read

		;mov byte ptr [dap_blocks], 1 ; number of blocks to transfer
		mov byte ptr [si+2],1 ; 23/12/2019

		;mov ah,42h ; LBA read
		;xor al,al ; verify off
		mov ax,4200h ; 23/12/2019
		int 13h
		jnc short lba_read_ok	

		; dl = physical drive number

		dec byte ptr [retry_count]
		jz short lba_err_retn

		;cmp ah,09h ; DMA crossed 64K segment boundary
		;je short lba_err_retn

		xor ah,ah ; reset
		int 13h

		jmp short lba_read ; read again
lba_err_retn:
		;stc
lba_read_ok:
		;pop si ; *** ; 23/12/2019
		;pop ax ; **
		;pop dx ; *

		retn
dskrd endp	

print_string proc near

		mov bx,07h
		mov ah,0Eh 
loc_print:
		lodsb			; Load byte at DS:SI to AL
		and al,al            
		jz  short loc_return	; If AL = 00h then return
	      
		int 10h			; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					;AL-char BH-page BL-color
		jmp short loc_print           
loc_return:
		retn

print_string endp

device_address_packet:
		; 23/12/2019
dap_size:	db 16 ; Disk address packet size
		db 0 ; reserved
dap_blocks:	db 0 ; Number of block to transfer ; also return value
		db 0
dap_buffer_off: dw 0 ; Address of transfer buffer, 16 bit offset
dap_buffer_seg: dw 0 ; Address of transfer buffer, 16 bit segment
dap_lba_lw:	dw 0 ; Starting logical block address, lw
dap_lba_hw:	dw 0 ; Starting logical block address, hw
		dd 0 ; High 32 bit of 64 bit unsigned linear address

		dw 0 ; 07/01/2020	 	

unix_bfl_error_msg:
		db 07h,"RUNIX boot error!"
msg_CRLF:
		db 0Dh,0Ah,0

; 18/12/2019
;retry_count:	db 12 ; disk read retry count  (will be reset to 4)

b_count:	dw 32704 ; 07/01/2020

; 23/12/2019
b_base:		dw boot_file_load_address ; 19/12/2019
b_off:		dw 0
;b_count:	dw 32704 ; Max. permissible boot file size (seg limit - stk spc)
;b_nread:	dw 0

; 23/12/2019
b_buffer:	db 0 ; 0 = initial value

msg_press_any_key:
		db 07h
		db "Not a bootable disk!"
		;db 0Dh,0Ah,0
		db 0Dh,0Ah
retry_count:	db 0 ; 23/12/2019

		;db 'runix v2 LBA'  ; 28/09/2019
; 18/12/2019
;b_buffer:	db 0 ; 0 = initial value

		db 'v2 LBA'  ; 23/12/2019
		db 0

;b_base:	dw boot_file_load_address ; 19/12/2019
;b_off:		dw 0
;b_count:	dw 32704 ; Max. permissible boot file size (seg limit - stk spc)
b_nread:	dw 0

		;db 23  ; 23/12/2019
		;db 12
		;dw 2019 ; 19/12/2019
		dw 2020 ; 07/01/2020

		org 7DFEh

bsBootSign:     dw 0AA55h

RUFS_BS		ends

		end  START