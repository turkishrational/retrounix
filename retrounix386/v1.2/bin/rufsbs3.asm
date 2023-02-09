; Retro UNIX 386 v2.0 - Modified UNIX v7 inode model - 28/09/2019
; ****************************************************************************
; RUFSBS3.ASM - Retro UNIX 386 v2 Boot Sector (22/12/2019)
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
; Last Update: 22/12/2019
; (New runix fd boot sec. code ref: RUBS3CHS.ASM -for runix hdfs-, 19/12/2019)
; BugFix: 09/01/2020

I_BUFFER equ 900h ; 20/12/2019
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
inode_ctim equ 63Ch

boot_file_load_address equ 8000h ; 20/12/2019 
boot_file_segment equ 800h ; 20/12/2019 

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

; RETRO UNIX 8086 FS v0.1 BootSector Identification (Data) Block
; 29-10-2012 RUFS 1.44MB FD Boot Sector

bsFSystemID:    db 'RUFS'
bsVolumeSerial: dd 0
		db 'fd'
bsDriveNumber:  db 0
bsReserved:     db 0  ; 512 bytes per sector
bsSecPerTrack:  db 18
bsHeads:        db 2
bsTracks:       dw 80
bs_BF_I_number:  dw 0
		db '@'
; 20/12/2019
		db  0	; Retro UNIX Partition Identifier (as signature)
bsHiddenSectors: dd 0	; Absolute/LBA address of (this) Boot Sector
@@:  
		mov ax, cs ; cs = 0
		mov ds, ax
		mov es, ax

		cli
		mov ss, ax ; = 0
		mov sp, 0FFFEh
		sti

		;mov ax, word ptr [bs_BF_I_number]

		;or ax, ax              
                ;jz loc_no_bootable_disk

		; 20/12/2019
		cmp word ptr [bs_BF_I_number], ax ; 0
		;jna loc_no_bootable_disk
		ja short @f

loc_no_bootable_disk:
		mov si, offset msg_press_any_key
		call print_string
		xor ax, ax
		int 16h
		int 19h  
@@:  
		mov byte ptr [bsDriveNumber], dl ; from INT 19h

read_super_block: ; 20/12/2019
		;;mov ax, 1
		;inc ax
		;xor dx, dx
		;; dx:ax = Super Block address (sector num. after Boot Sector)
		;mov bx, 7E00h  ; next sector (just after boot sector)
		;call dskrd
		;;jc short loc_unix_bl_error ; 20/12/2019
			; super block reading error may be ignored
			; (boot file may check super block signature then
			;  may read sb again if it is not ready at 07E00h)
;load_boot_file:
		;;call load_boot_file
		;;jc short loc_unix_bl_error
load_boot_file:
		;; 22/12/2013
		; 28/10/2012
		; 20/10/2012
		;
		; RETRO UNIX v1 FS
		; Boot sector version
		;
		; loads boot file       
		;
		; ax = i-number
load_bf_1:
i_get:
		; 28/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode format)
		;; 22/12/2013
		; 20/10/2010 (i_i)
		; 14/10/2012
		; boot sector version of "iget" procedure
		; Derived from (original) UNIX v1 source code
		; PRELIMINARY release of Unix Implementation Document, 
		; 20/6/1972
		; input -> AX = inode number
		; RETRO UNIX v1 FS
		; boot sector version
		;; return => if cf=1 error number in [Error] 

		mov ax, word ptr [bs_BF_I_number] ; 20/12/2019

		;;cmp ax, word ptr [i_i] ; AX (R1) = i-number of current file
		;;je short i_get_3
	
		;; mov di, ax ; i-number
		;add ax, 47 ; add 47 to inode number
		add ax, 31 ; add 31 to inode number ; 28/09/2019 - Retro UNIX 386 v2	
		push ax ; 
		shr ax, 1 ; divide by 16
		shr ax, 1
		shr ax, 1
		;shr ax, 1 ; divide by 8 ; 28/09/2019
			; ax contains block number of block in which
			; inode exists

		; 20/12/2019
		mov bx, I_BUFFER ; 900h ; inode sector buffer
					; for boot file
		call dskrd
		;pop dx ;
		pop ax ; 21/12/2019
		;;jc short i_get_3 ; Error code in AH
                ;jc loc_unix_bl_error
		jc short loc_unix_bl_error_j ; 20/12/2019

		;;mov word ptr [i_i], di
i_get_1:
		;and dx, 0Fh  ; (i+47) mod 16
		;shl dx, 1
		;shl dx, 1
		;shl dx, 1
		;shl dx, 1
		;shl dx, 1 
		  	; DX = 32 * ((i+47) mod 16)
		; 29/09/2019
		;and dx, 07h  ; (i+31) mod 8
		;jz short @f ; 20/12/2019 

		;and ax, 07h ; 21/12/2019
		and al, 07h ; 22/12/2019	

		; 28/09/2019
		;mov cl, 6
		;shl dx, cl			
		;  	; DX = 64 * ((i+31) mod 16)  			
;@@:
	  		; DX points to first word in i-node i.
		;mov di, BF_INODE
			; inode is address of first word of current inode
		;mov cx, 16 ;
		;mov cx, 32 ; 28/09/2019

		;mov si, bx ; offset Buffer

		;add si, dx

		mov cl, 64
		mul cl			
		  	; AX = inode ofset
		mov si, bx ; I_BUFFER
		add si, ax

		mov di, BF_INODE	
i_get_2:
		; copy new i-node into inode area of (core) memory
		;rep movsw
		rep movsb
;;i_get_3:
		;;retn   

lbf_2:		;; 22/12/2013
check_inode_flags:
		;mov bx, inode_flgs

		;test word ptr [bx], 10h ; executable file attribute bit
		; 28/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode format)
		;test byte ptr [bx], 40h  ; executable file flag (for owner)
		;;jz short load_bf_stc 
                ;jz loc_unix_bl_error

		; 22/12/2019
		mov bx, inode_flgs
		mov ax, word ptr [bx]
		and ax, 0E040h ; 8000h+4000h+2000h+0040h
		cmp ax, 8040h ; regular file + executable by owner
		jne short loc_unix_bl_error_j
	
		mov bx, inode_size ; offset

                ;; 22/12/2013
                ;;cmp word ptr [bx], 0
		;;jna short load_bf_stc   
                ;;;jna short loc_unix_bl_error
                ;mov ax, word ptr [bx]
                ;and ax, ax
                ;jz loc_unix_bl_error 
		;jz short loc_unix_bl_error_j ; 20/12/2019      
		
		; 22/12/2019
		mov ax, word ptr [bx]
		mov dx, word ptr [bx+2] ; Must be 0

                and ax, ax
                jz short loc_unix_bl_error_j

		or dx, dx
                jnz short loc_unix_bl_error_j ; boot file must be <= 64KB

		; 20/12/2019
		;mov word ptr [b_base], boot_file_load_address

                ;;xor ax, ax 
                ;;mov word ptr [b_off], ax ; u_off is file offset

                ;; 22/12/2013
		; 20/12/2019
		;xor dx, dx
                ;mov word ptr [b_off], dx ; u_off is file offset

		;mov bx, inode_size
                ;;mov ax, word ptr [bx]
		; 20/12/2019
		;mov word ptr [b_count], ax
	
		;;mov ax, word ptr [i_i]
		;;call read_i     
		;;jc short load_bf_retn

read_i:
		;; 22/12/2013
		; 28/10/2012
		; 14/10/2012
		; Boot sector version of "readi" procedure
		; Derived from (original) UNIX v1 source code
		; PRELIMINARY release of Unix Implementation Document, 
		; 20/6/1972
		;;AX (R1) = i-number 
		; RETRO UNIX v1 FS
		; Boot sector version
		;
		; read from an i-node
		;
                ;;xor dx, dx ; 0
		; 20/12/2019
		;mov word ptr [b_nread], dx ; accumulated number of bytes transmitted
                ;;cmp word ptr [b_count], dx ; is number of byte to read greater than 0
                ;;jna short read_i_retn
read_i_1:
		; AX = I-Number
		;;push ax
		;;call i_get ; get i-node into i-node section of core
		;mov bx, inode_size
		;mov dx, word ptr [bx] ; file size in bytes in r2 (DX)
		
		;sub dx, word ptr [b_off] ; subtract file offset
		;;jna short read_i_3
		;jna read_i_retn ;; 22/12/2013
		;cmp dx, word ptr [b_count] 
		;		; are enough bytes left in file to carry out read
		;jnb short read_i_2
		;mov word ptr [b_count], dx

		; 20/12/2019
		sub ax, word ptr [b_off] ; subtract file offset
                jna read_i_retn
		cmp ax, word ptr [b_count] 
				; are enough bytes left in file to carry out read
		jnb short read_i_2
		mov word ptr [b_count], ax
read_i_2:
		;;call m_get  ; returns physical block number of block in file 
		;;   	    ; where offset points
m_get:
		;; 22/12/2013
		; 05/03/2013
		; 03/03/2013
		; 28/10/2012
		; 20/10/2012
		; Boot sector version of "mget" procedure
		; Derived from (original) UNIX v1 source code
		; PRELIMINARY release of Unix Implementation Document, 
		; 20/6/1972
		;
m_get_0:
		mov bl, byte ptr [b_off]+1
		xor bh, bh
		;mov si, inode_flgs
		;test word ptr [si], 4096 ; 1000h
		
		; 28/09/2019
		mov si, inode_flgs+1
		test byte ptr [si], 16 ; 10h
				     ; is this a large or small file
		jnz short m_get_1 ; large file

		;test bl, 0F0h ; !0Fh  ; error if BX (R2) >= 16                    
		;jnz short m_get_5

		;and bl, 0Eh  ; clear all bits but bits 1,2,3
		
		; 28/09/2019
		cmp bl, 14h ; file size > 5120 bytes
		;jnb short m_get_5 ; large file
		jnb short m_get_1 ; 29/09/2019	

		;and bl, 0Eh  ; clear all bits but bits 1,2,3
		and bl, 1Eh  ; 28/09/2019 ; clear all bits but bits 1,2,3,4

		shl bl, 1 ; 28/09/2019 - Retro UNIX 386 v2 (dword block addresses)

		mov ax, word ptr [inode_dskp+bx] ; AX = R1, physical block number
	
		jmp short m_get_3

loc_unix_bl_error_j:
		; 18/12/2019
		jmp loc_unix_bl_error

m_get_1: 	; large file
		; 05/03/2013
		; 03/03/2013
		;mov ax, bx
		;mov cx, 256
		;xor dx, dx
		;div cx
		;and bx, 1FEh  ; zero all bit but 1,2,3,4,5,6,7,8
		      ; gives offset in indirect block
		;push bx              ; 
		;mov bx, ax  ; calculate offset in i-node for pointer
		    	; to proper indirect block
		;and bx, 0Eh
		;mov ax, word ptr [inode_dskp+bx]
		and bl, 0FEh
		shl bx, 1 ; 28/09/2019 - Retro UNIX 386 v2 (dword block addresses) 
		;;push bx
		;; mov di, bx 
		;mov si, bx ; 22/12/2013
		mov di, bx ; 20/12/2019

		; 22/12/2019
		mov bx, I_BUFFER ; 900h ; indirect block buffer
		
		cmp byte ptr [b_buffer], 0 ; initial value = 0
		ja short m_get_2

		; 22/12/2019
		mov si, inode_dskp
		mov ax, word ptr [si]
		or ax, ax
		;;jz short m_get_4
		jz short loc_unix_bl_error ; 22/12/2013
;m_get_2:
		; 20/12/2019
		;mov bx, I_BUFFER ; 900h ; indirect block buffer
		
		;cmp byte ptr [b_buffer], 0 ; initial value = 0
		;ja short @f

		call dskrd ; read indirect block
		;;jc short m_get_5
		jc short loc_unix_bl_error ; 22/12/2013
		; 20/12/2019
		inc byte ptr [b_buffer]  ; 1 = indirect block buffer is loaded
;@@:
m_get_2:
		; bx = BF_BUFFER ; 0:700h
		;;pop ax
		;;add bx, ax ; R5, first word of indirect block
		;;add bx, di
		;add bx, si ; 22/12/2013
		add bx, di ; 20/12/2019
		mov ax, word ptr [bx] ; put physical block no of block
			      ; in file sought in R1 (AX)
m_get_3: ; 2
		; ax = R1, block number of new block
		;;cmp ax, 1
		;;retn
		or ax, ax
		jz short loc_unix_bl_error ; 22/12/2013
m_get_4:
		;;stc
m_get_5:
		;;pop bx
		;;retn
		;;;jc short loc_unix_bl_error ; 22/12/2013

		; 20/12/2019
		mov bx, BF_BUFFER ; 700h ; Boot File buffer	

		; AX = Physical block number
		call dskrd ; read in block, BX points to 1st word of data in
		   	    ; buffer
		;;jc short read_i_3
		;;jc short_read_i_retn
		jc short loc_unix_bl_error ;; 22/12/2013		
readi_sioreg:
		mov si, word ptr [b_off] ; R2
		mov cx, si ; cx = R3, si = R2
		or cx, 0FE00h ; set bits 9...15 of file offset in R3
		and si, 1FFh ; calculate file offset mod 512
		add si, bx ; offset Buffer ; si now points to 1st byte in buffer
		   	  ; where data is to be placed
		mov di, word ptr [b_base] ; R1
		neg cx ; 512 - file offset(mod512) in R3 (cx)
		cmp cx, word ptr [b_count]
		jna short @f ; 2f

		mov cx, word ptr [b_count]
@@:
		add word ptr [b_nread], cx ; r3 + number of bytes
			; xmitted during write is put into
			; u_nread
		sub word ptr [b_count], cx
		add word ptr [b_base], cx ; points to 1st of remaining
			; data bytes
		add word ptr [b_off], cx ; new file offset = number 
			; of bytes done + old file offset
; end of readi_sioreg
		; DI = file (user data) offset
		; SI = sector (I/O) buffer offset
		; CX = byte count 
		
		rep movsb
		;;pop ax

		cmp word ptr [b_count], cx ; 0
                jna short read_i_retn

		; 22/12/2019
		mov bx, inode_size
		mov ax, word ptr [bx] ; file size in bytes

		jmp read_i_1

read_i_retn: ;; 22/12/2013
		;;retn
;;read_i_3:       
	    ;;	pop ax ; i-number

;;read_i_retn:
	    ;;	retn 

	     ;;; jc short load_bf_retn	

		; 20/12/2019 
		;mov cx, word ptr [b_nread]
		;mov bx, inode_size
		
	     ;;	cmp cx, word ptr [bx]
	     ;; retn

;;load_bf_stc:
	     ;;	stc

;;load_bf_retn:
	     ;;	retn
      
             ;;; jc short loc_unix_bl_error

loc_launch_bootfile:
		mov si, offset msg_CRLF
		call print_string

		; 09/01/2020 BugFix (bsDriveNumber segment, ds = cs)
		mov dl, byte ptr [bsDriveNumber]	       

		mov ax, boot_file_segment ; 800h ; 20/12/2019
		mov ds, ax
		mov es, ax
		cli
		mov ss, ax
		;mov sp, 0FFFEh 
		sti

		; 09/01/2020 Bug (bsDriveNumber segment, ds <> cs)
		;mov dl, byte ptr [bsDriveNumber]

	     ;; MASM.EXE don't accept
	     ;; jmp 0800h:0000h
	     ;; for OP Code: EA00000008
	     ;	db 0EAh
	     ;	dw 0
	     ;	dw 0800h
	
		; 20/12/2019
		push es
		push cx
		retf
		  
;NeverComeHere:  jmp short NeverComeHere 

;loc_no_bootable_disk:
;		mov si, offset msg_press_any_key
;		call print_string
;		xor ax, ax
;		int 16h
;		int 19h

loc_unix_bl_error:
		mov si, offset unix_bfl_error_msg
		call print_string
NeverComeHere: ; 20/12/2019
		jmp short NeverComeHere

unixbootsector endp

dskrd  proc near
		; 22/12/2019	
		; 20/12/2019
		;; 22/12/2013
		; 28/10/2012 (bf_buff_s)
		; 20/10/2012
		; 14/10/2012
		; fd boot sector version of "dskrd" procedure
		; Derived from (original) UNIX v1 source code
		; PRELIMINARY release of Unix Implementation Document, 
		; 20/6/1972
		; RETRO UNIX v1 FS
		; floppy disk boot sector version
		;; return => if cf=1 error number in [Error]

		; ax = sector/block number

	 	;cmp  ax, word ptr [bf_buff_s] ; buffer sector
	 	;je   short dskrd_3

	 	;;mov si, ax

		; 20/12/2019
		;mov  bx, BF_BUFFER ; offset Buffer

	 	xor   ch, ch
	 	mov   cl, 4 ; Retry count
dskrd_1:
	 	push  cx
	 	mov   dx, 18           ; Sectors per track, 18
	 	div   dl                      
	 	mov   cl, ah           ; Sector (zero based)
	 	inc   cl               ; To make it 1 based
	 	shr   al, 1            ; Convert Track to Cylinder
	 	adc   dh, 0            ; Heads (0 or 1)

	 	mov   dl, byte ptr [bsDriveNumber] ; Physical drive number 
	 	mov   ch, al

	 	mov   ah, 2            ; 2=read         
	 	mov   al, 1
	 	int   13h              ; BIOS Service func ( ah ) = 2
				       ; Read disk sectors
				       ; BIOS Service func ( ah ) = 3
				       ; Write disk sectors
				       ;AL-sec num CH-cyl CL-sec
				       ; DH-head DL-drive ES:BX-buffer
				       ;CF-flag AH-stat AL-sec read
	 	pop   cx
	 	jnc   short dskrd_2

		; 22/12/2019
		dec   cl  ; Retry count
		jz    short @f

		; reset diskette
		push  ax
		xor   ah, ah
		int   13h
		pop   ax
		
	 	;loop dskrd_1
		jmp   short dskrd_1 
@@:		
		;stc
dskrd_2:
		;mov  word ptr [bf_buff_s], si 
dskrd_3:
	 	retn

dskrd  endp

print_string proc near

		mov  BX, 07
		mov  AH, 0Eh 
loc_print:
		lodsb			; Load byte at DS:SI to AL
		and  AL,AL            
		je   short loc_return	; If AL = 00h then return
	      
		int  10h		; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					;AL-char BH-page BL-color
		jmp  short loc_print           
loc_return:
		retn

print_string endp

		db 0 ; 22/12/2019

unix_bfl_error_msg:
		db 07h, "RUNIX boot error!" ; 20/12/2019
msg_CRLF:
		db 0Dh, 0Ah, 0

msg_press_any_key:
		db 07h
		db "Not a bootable floppy disk!"
		;db 0Dh, 0Ah, 0
		db 0Dh, 0Ah
; 20/12/2019
b_buffer:	db 0 ; 0 = initial value

b_base:		dw boot_file_load_address ; 20/12/2019
b_off:		dw 0
		; 20/12/2019
b_count:	dw 32704 ; Max. permissible boot file size (seg limit - stk spc)
b_nread:	dw 0

;bf_buff_s:	dw 0

;;i_i:          db 2 dup (0)

		;dd 2019 ; 28/09/2019

		db 'runix v2'  ; 28/09/2019

		db 0

		dw 2019 ; 20/12/2019

		db 0,12,0,22,0 ; 22/12/2019
		
		org 7DFEh

bsBootSign:     dw 0AA55h

RUFS_BS		ends

		end  START