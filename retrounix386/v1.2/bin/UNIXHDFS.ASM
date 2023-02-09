; ****************************************************************************
; UNIXHDFS.ASM
; ----------------------------------------------------------------------------
; RETRO UNIX v0.3 - Modified UNIX v7 inode model - 02/10/2019
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; 1.44 MB Floppy Disk 
; Bootable Unix (RUFS) File System Installation/Formatting Code 
;
; Derived from UNIXFDFS.ASM (for 1.44MB floppies) - 30/09/2019 (Retro UNIX v2)
;
; Last Modification: 20/07/2022 (Retro UNIX 386 v2)
; UNIXFDFS.ASM -> Last Modification: 04/12/2015 (Retro UNIX 386 v1)
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ----------------------------------------------------------------------------
; masm unixhdfs.asm unixhdfs.obj unixhdfs.lst
; link /t unixfdfs
; ****************************************************************************

; 20/07/2022
; 13/04/2022
; 05/04/2022
; 23/01/2020
; 03/10/2019
; 05/09/2019 Retro UNIX 386 v2 (Modified UNIX v7 inode model)

; 04/12/2015 Retro UNIX 386 v1 (14 byte file names)
; 21/04/2014 (tty8=COM1, tty9=COM2)
; 22/12/2013
; 09/07/2013

; (Retro Unix File System) Hard Disk Boot Sector Parameters (03/10/2019)
;
;jmp short @f 	       ; db EBh, 13h ; db 0EBh, 18h ; 19/12/2019
; 03/12/2019
;bsFSystemID	equ 2  ; db 'RUFS'
;bsVolumeSerial	equ 6  ; dd 0
;		       ; db 'hd'
;bsDriveNumber	equ 12 ; db 0
;bsReserved	equ 13 ; db 0  ; 512 bytes per sector
;bsSecPerTrack	equ 14 ; db 63 (17)
;bsHeads	equ 15 ; db 16 (2,4,8,16,32,64,128,255)
;bsTracks	equ 16 ; dw 1023 (1 to 1023) ; bsCylinders
;bs_BF_I_number	equ 18 ; dw 0
;bsMagic	equ 20 ; db '@'
;bsPartitionID  equ 21 ; db 71h
;bsHiddenSects  equ 22 ; dd 0 ; Hidden sectors (Boot Sector LBA)
; @@:	 		

; 19/12/2019 - Retro UNIX 386 v2 HD (071h) partition boot sector
; 04/12/2019
RUFS struc
;bsJumpCode    dw ? ; 0EB, 13h ; jmp short @f
bsJumpCode     dw ? ; 0EB, 18h ; 19/12/2019  	
bsFSystemID    db 4 dup(?)  ;'RUFS'
bsVolumeSerial dd ?
bsDriveID      dw ? ; 'hd'
bsDriveNumber  db ? ; 80h
bsReserved     db ? ; 0 = 512 bytes/sector CHS, 1 = LBA
bsSecPerTrack  db ? ; 63,17
bsHeads        db ? ; 8 to 255 (may be 2 to 255) 
bsCylinders    dw ? ; 1 to 1024 (bsTracks)
bs_BF_I_number dw 0 ; startup (boot) file inode number
bsMagic        db ? ; '@' ; magic byte !
bsPartitionID  db ? ; 71h, Retro UNIX 386 partition/volume ; 19/12/2019
bsHiddenSects  dd 0 ; Hidden sectors (Boot Sector LBA) ; 19/12/2019
;;@@:
RUFS ends ; 26 bytes of boot sector parms with short jmp code (19/12/2019)

; 03/10/2019
;NOTE: If byte [bsReserved] = 1, it means LBA read/write is needed..
;			(bytes per sector is still 512)
;      If byte [bsReserved] = 2 -may be in future- bytes per sector is 2048
;			(LBA r/w is certainly needed for 2048 bytes/sector)

; 14/01/2020 - Super Block modification :
;	     - Extended sections/divisions (consequental sectors)
;	     - (for swapping, configuration, boot space etc.)	
; 21/12/2019
; 19/12/2019 (UNIXHDFS.COM, RUFSHDI.ASM)
; 01/09/2019 - Retro UNIX 386 v2 SuperBlock

SuperBlock struc

sb_Header	dd ?
sb_BootSectAddr dd ?  ; Hidden Sectors
sb_VolumeSize	dd ?  ; Entire Volume/Partition Size (includes ext. volume)	
sb_Version	dd ?
sb_BlockSize	dd ?
sb_InodeCount	dd ? 	
sb_FreeMapAddr	dd ?
sb_FreeMapSize  dd ?
sb_InodeMapAddr	dd ?
sb_InodeMapSize dd ?
sb_InodeTblAddr dd ?
sb_InodeTblSize dd ?
sb_FreeInodes	dd ?
sb_FirstFreeIno dd ?
sb_FreeBlocks	dd ?
sb_FirstFreeBlk dd ?
sb_BootSecParms db 19 dup(?) ; v1 ; 19/12/2019
sb_BSExtension	db 5 dup(?) ; v2 HDFS ; 19/12/2019
sb_Status	dd ? ; 19/12/2019
sb_ModifTime	dd ?
sb_ExtdVolTbl	dd 0 ; 14/01/2020 ; Extended Volume Start/Table Address
sb_ExtdVolSize	dd 0 ; 14/01/2020 ; Extended Volume (swap section etc.) Size	
sb_LBA_rw	db 0 ; 03/10/2019
sb_ClusterSize	db 0 ; 03/10/2019
sb_ReadOnly	db 0 ; 03/10/2019
sb_Mounted	db 0 ; 03/10/2019
sb_MountInode	dd 0 ; 03/10/2019
sb_DevMajor	db 0 ; 03/10/2019
sb_DevMinor	db 0 ; 03/10/2019
sb_LongName	db 0 ; 03/10/2019
sb_Direntry32	db 0 ; 03/10/2019
sb_Reserved	db 508-116 dup(?) ; Must be 0 for current RUFS version
sb_Footer	dd ?

SuperBlock ends

; 14/01/2020
sb_HiddenSects equ sb_BootSecAddr
sb_TotalSects equ sb_VolumeSize

RUFS_INSTL      SEGMENT PUBLIC 'CODE'
		assume cs:RUFS_INSTL,ds:RUFS_INSTL,es:RUFS_INSTL,ss:RUFS_INSTL

rufs_hd_format proc near
		; 04/12/2019
		; 04/12/2015
		; 28/10/2012
		; 19/09/2012
		; 14/08/2012
		; 13/08/2012
		; 12/08/2012

		org 100h
INSTALL:
		; 05/09/2019
	        mov bx, SizeOfFile+100
                add bx, 15
                shr bx, 1
        	shr bx, 1
		shr bx, 1
		shr bx, 1
                mov ah, 4Ah ; modify memory allocation
               ;push cs
               ;pop es
                int 21h 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		mov si, offset 80h		; PSP command tail
		;mov cl, byte ptr [SI]
		lodsb
		mov cl, al
		or cl, cl                               
		;jz rufs_hd_format_7		; jump if zero
		; 05/04/2022
		jnz short @f
		jmp rufs_hd_format_7
@@:
		; 05/09/2019

		;inc si
get_args:
		lodsb
		cmp al, ' '
		ja short rufs_hd_format_1
		dec cl
		jnz short get_args
		jmp short rufs_hd_format_7

rufs_hd_format_1:
		cmp al, '-'
		jne short rufs_hd_format_2
		jmp check_hdi_option ; 04/12/2019
	
rufs_hd_format_2:
		cmp al, 'h'
		jne short rufs_hd_format_3
		;inc si
		;mov al, byte ptr [SI]
		lodsb
		cmp al, 'd'
		jne short rufs_hd_format_7
		;inc si
		;mov ax, word ptr [SI]
		lodsw
		cmp al, '0'                            
		jb short rufs_hd_format_7
		cmp al, '3'
		ja short rufs_hd_format_7
		cmp ah, 20h
		ja short rufs_hd_format_7
		mov byte ptr [RUFS_DRIVE], al
		sub al, '0'
		jmp short rufs_hd_format_5

rufs_hd_format_3:
		cmp al, 'C'
		jb short rufs_hd_format_7
		cmp al, 'F'                             ; A - Z
		jna short rufs_hd_format_4                    
		cmp al, 'c'                             ; a - z 
		jb short rufs_hd_format_7                 
		cmp al, 'f'                           
		ja short rufs_hd_format_7                 

		sub al, 'a'-'A'                         ; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_hd_format_4:
		mov byte ptr [RUFS_DRIVE], al
		sub al, 'C'                             ; make it zero based 

rufs_hd_format_5:
		add al, 80h ; 02/10/2019
		mov dl, al
		mov byte ptr [boot_sector.bsDriveNumber], dl ; 04/12/2019
		; 06/09/2019
		mov byte ptr [buff_d], al
		;mov byte ptr [drive], al ; 03/12/2019

		; 09/09/2019
		; Read boot sector to understand drive is ready or not?
		call drive_check
		; (we will not return here if cf = 1)
		; If cf = 0, we will return here

		;mov dl, byte ptr [buff_d]
		mov ah, 08h
		int 13h                                 ; return disk parameters
		push cs
		pop es                                  ; restore es
		;;jc rufs_hd_format_17
		;jc rufs_hd_format_13 ; 06/09/2019
		; 05/04/2022
		jnc short @f
		jmp rufs_hd_format_13	
@@:
		; 02/10/2019
		call set_hd_parms

		; 03/10/2019
rufs_hd_format_img:
		call get_runix_partition
		jc short runix_partition_error

		;;	

		mov si, offset Msg_DoYouWantToFormat
		call PRINT_MSG
rufs_hd_format_6:
		xor ax, ax
		int 16h                                 ; wait for keyboard command
		cmp al, 'C'-40h
		je short rufs_hd_format_8              
		cmp al, 27
		je short rufs_hd_format_8
		and al, 0DFh
		cmp al, 'Y'                             ; Yes?
		je short rufs_hd_format_10             ; write
		cmp al, 'N'                             ; No?
		je short rufs_hd_format_9              ; no write (exit)
rufs_hd_format_7:
		mov si, offset UNIX_Welcome
		call PRINT_MSG
rufs_hd_format_8:
		mov si, offset UNIX_CRLF
		call PRINT_MSG

		int 20h

infinive_loop:  jmp short infinive_loop

runix_partition_error:
		; 03/10/2019
		mov si, offset msg_inv_partition
		jmp short @f

check_hdi_option:
		lodsb
		cmp al, 'i'
		jne short rufs_hd_format_7
		dec cl
		jz short rufs_hd_format_7
		lodsb
		cmp al, ' '
		jne short rufs_hd_format_7
		dec cl
		jz short rufs_hd_format_7
check_hdi_opt_loop:
		lodsb
		cmp al, 20h
		ja short get_hdi_name ; 04/12/2019
		jb short rufs_hd_format_7
		dec cl
		jz short rufs_hd_format_7
		jmp short check_hdi_opt_loop

rufs_hd_format_9:
		mov si, offset msg_NO
		call PRINT_MSG

		jmp short rufs_hd_format_8

get_hdi_name:
		mov di, offset img_file_name
get_hdi_name_nxt_chr:
		stosb
		dec cl
		jz short get_hdi_name_ok		
		lodsb
		cmp al, 20h
		jna short get_hdi_name_ok		
		cmp di, offset img_file_name + 12	
		jb short get_hdi_name_nxt_chr

		jmp short invalid_file_name

get_hdi_name_ok:
		sub al, al
		stosb 

		; 03/10/2019	
		jmp short cap_file_name
		
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_hd_format_10:
		mov si, offset msg_YES
		call PRINT_MSG

		; 04/12/2019
		; 06/09/2019
		;mov dl, byte ptr [boot_sector.bsDriveNumber]
		;mov byte ptr [buff_d], dl
		;mov byte ptr [drive], dl ; 03/12/2019

rufs_hd_format_11:
		xor ax, ax
		int 1Ah                 ; get time of day
  					; set unique volume ID
		; 29/12/2019
		;mov word ptr [boot_sector.bsVolumeSerial], dx
		;mov word ptr [boot_sector.bsVolumeSerial+2], cx 
		mov word ptr [VolumeSerial], dx
		mov word ptr [VolumeSerial+2], cx 

rufs_hd_format_12:
		mov si, offset Msg_installing_file_system
		call PRINT_MSG

		call unix_fs_install
		jnc short rufs_hd_format_14

		mov ah, byte ptr [Error]

rufs_hd_format_13: ; loc_rw_error
		; 06/09/2019
		mov al, ah
		call proc_hex
		mov word ptr [Str_Err], ax
		mov si, offset Msg_Disk_RW_Error
		call PRINT_MSG
		mov si, Offset Msg_Error_Number
@@:
		call PRINT_MSG

close_file_then_terminate:		
		; 07/07/2015
		mov bx, word ptr [img_file_handle]
		and bx, bx
		jz  short terminate
close_img_file:
		mov ah, 3Eh ; close (floppy disk image) file
		int 21h		 
terminate:
		jmp rufs_hd_format_8 ; 06/09/2019

;		int 20h
;
;		; 06/09/2019
;hang:
;		jmp short hang

invalid_file_name:
		mov si, offset msg_inv_file_name
                jmp short @b

rufs_hd_format_14:
		mov  si, offset Msg_OK
		call PRINT_MSG

rufs_hd_format_15:
		mov si, offset Msg_writing_boot_sector
		call PRINT_MSG

		;mov byte ptr [RetryCount], 4

rufs_hd_format_16:
		; 03/12/2019 - Retro UNIX 386 v2 
		;	  (RUNIX file system for hard disks)
		; 06/09/2019 - Retro UNIX 386 v2
		xor ax, ax
		xor dx, dx
		mov bx, offset boot_sector
		call dskwr
		jc short rufs_hd_format_13	
		;jmp short rufs_hd_format_17

		;mov ax, 0301h				; write to disk
		;mov bx, offset boot_sector		; location of boot code
		;mov cx, 1				; cylinder = 0
		;					; sector = 1
		;mov dh, 0				; head = 0
		;
		;mov dl, byte ptr [boot_sector.bsDriveNumber] ; 04/12/2019
		;int 13h
		;jnc short rufs_hd_format_17
		;dec byte ptr [RetryCount]
		;jnz short rufs_hd_format_16
		;
		;jmp short rufs_hd_format_13

rufs_hd_format_17:
		mov  si, offset Msg_OK
		call PRINT_MSG

		;int 20h
		; 06/09/2019
		;jmp rufs_hd_format_8
		jmp short close_file_then_terminate

cap_file_name:
		; file name capitalization
		mov si, offset img_file_name
		mov di, si
		mov bx, si
cap_file_name0:
		lodsb
		cmp al, 'a'
		jnb short cap_file_name2
		and al, al
		jz short cap_file_name3
		cmp al, '.'
		jne short cap_file_name1
		mov bx, di ; dot position	
cap_file_name1:
		;stosd
		inc di ; 06/09/2019
		jmp short cap_file_name0 
cap_file_name2:
		cmp al, 'z'
		ja short cap_file_name1
                and al, 0DFh ; NOT 32
		stosb
		jmp short cap_file_name0
cap_file_name3:
		mov [di], al
		dec di
		cmp bx, di
		jnb short invalid_file_name
		sub di, bx
		sub bx, offset img_file_name
		cmp di, 3
		jna short cap_file_name4
		and bx, bx
		jnz short invalid_file_name
		jmp short open_image_file ; 03/10/2019		

file_error:
		mov si, offset file_error_msg
		jmp short @b

cap_file_name4:
		cmp bx, 8
		ja short invalid_file_name

		; 03/10/2019
open_image_file:
		; 05/09/2019 - Retro UNIX 386 v2
		; 07/07/2015 (UNIXCOPY.ASM)
		mov dx, offset img_file_name
		mov cx, 3Fh ; File Attributes
                ;xor cx, cx ; 06/09/2019
		mov ah, 4Eh ; MS Dos Function = Find First File
                int 21h
		jc short file_not_found_error

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21 ;05/01/2013	
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

chk_image_file_features:
		mov si, DTA_Attrib
                mov al, byte ptr [si]
                and al, 1Fh ; directory, volume label, system, hidden, read only
                jnz short file_attr_error       
		mov si, DTA_FileSize
		; 03/10/2019
		lodsw
		mov dx, ax
		lodsw
		xchg ax, dx

		; 19/12/2019
		test ax, 511
		jnz short invalid_image_file
			 ; end of file is not on sector boundary!

		mov word ptr [file_size], ax
		mov word ptr [file_size+2], dx

		mov al, 2 ; open for reading and writing
		mov dx, offset img_file_name
		mov ah, 3Dh ; open file
		int 21h
		jc short file_not_found_error

		mov word ptr [img_file_handle], ax

		mov ah, 3Fh ; read file
		mov bx, word ptr [img_file_handle]
		mov cx, 512
		mov dx, offset Buffer
		int 21h
		jc short file_error

		call check_hd_image_size
		jnc short valid_hd_image	
invalid_image_file:
		mov si, offset msg_inv_image_file
		jmp @b

file_not_found_error:
		mov si, offset msg_file_not_found
		jmp @b
file_attr_error:
		mov si, offset msg_file_attr_error
		jmp @b

valid_hd_image:
		; 19/12/2019
		; 06/09/2019
		; set (buffer) disk drive number for image file
		;mov byte ptr [buff_d], 90h ; floppy disk image sign
		; 03/12/2019
		mov byte ptr [drive], 90h ; hard disk image sign

		; 03/10/2019
		jmp rufs_hd_format_img

rufs_hd_format  endp

set_hd_parms	proc near
		; 18/12/2019
		; 02/10/2019 - Retro UNIX 386 v2
		; Input:
		;  (output of INT 13h, AH=08h Read Drive Parameters)
		;   DL = number of hard disk drives 
		;   DH = logical last index of heads
		;      = number_of - 1 (because index starts with 0)
		;   CX = [7:6] [15:8][7] = logical last index of cylinders
		;                        = number_of - 1 
		;			   (because index starts with 0)
		;        [5:0][7] = logical last index of sectors per track
		;		  = number_of (because index starts with 1)
		; Output:
		;	CHS parameters are set
		;
		; Modified registers: ax,(bx),cx,dx,(es),(di)

		xor ah,ah
		mov al,dh
		inc ax
		mov word ptr [heads],ax
		mov ax,cx
		and cx,63
		mov word ptr [sectors],cx		
		xchg ah,al
		and ah,0C0h
 		rol ah,1 	
		rol ah,1
		inc ax
		mov word ptr [cylinders],ax	

		; 18/12/2019
		;push ds
		;pop es

		retn

set_hd_parms	endp

get_runix_partition proc near
		; 01/12/2019
		; 03/10/2019
		; 02/10/2019 - Retro UNIX 386 v2
		; Check Retro UNIX v2 (71h) partition on MasterBoot
		; sector if it is valid, set 'hidden sectors',
		; and 'total sectors'
		
		; DL = Drive number (80h .. 83h)

		; Modified registers: ax,(bx),cx,dx,(es),(di)

		;mov dl,byte ptr [buff_d]

		;xor ah,ah
                ;int 13h
		;jnc short reset_ok
;harddisk_error:
		;retn
;reset_ok:
		;mov bx,offset Buffer
                ;mov ax,0201h
                ;mov cx,1
		;;mov dl,byte ptr [buff_d]
		;;mov dl,byte ptr [drive] ; 03/12/2019
                ;xor dh,dh
                ;;push ds
                ;;pop es
                ;int 13h
                ;jc short harddisk_error

; Here is the entry point for hard disk image (no int13h!)

                cmp word ptr [Buffer+510],0AA55h
                je short valid_masterboot
not_masterboot:
                stc
                retn
valid_masterboot:
		; 03/10/2019
		mov si,offset Buffer+01BEh+4 
			; partition type byte in partition table
check_runix_partition:
		cmp byte ptr [si],71h
		je short runix_partition_ok
		cmp si,offset Buffer+01BEh+48+4
		jnb short not_runix_partition
		add si,16
		jmp short check_runix_partition
not_runix_partition:
		stc
		retn		
runix_partition_ok:
		add si,4
		
		;mov ax,word ptr [si] ; start sector (of the runix partition)
		;mov word ptr [hidden_Sectors],ax
		;mov dx,word ptr [si+2]
		;mov word ptr [hidden_Sectors+2],dx
		;add si,4
		
		lodsw
		mov dx,ax
		lodsw
		xchg ax,dx
		mov word ptr [hidden_Sectors],ax
		mov word ptr [hidden_Sectors+2],dx
	  	
		; 19/12/2019	  
		mov cx,word ptr [si] ; total sectors (volume size)
		mov word ptr [total_Sectors],cx
		mov bx,word ptr [si+2]
		mov word ptr [total_Sectors+2],bx

		cmp byte ptr [drive], 90h ; hd image file ?
		je short lba_ok ; lets use LBA boot sector & LBA r/w	

		; 03/10/2019
		add cx,ax
		adc bx,dx
				
		mov ax, word ptr [sectors]
		mov dx, word ptr [heads]
		mul dx  ; max. possible value 255*63 = 16065
			; dx = 0
		mov dx, word ptr [cylinders]
		mul dx
		sub ax, 1
		sbb dx, 0
		     ; dx:ax = CHS limit (max. = 16065*1024-1 = 16450559)

		; 01/12/2019
		mov word ptr [CHS_limit], ax
		mov word ptr [CHS_limit+2], dx
	
		cmp dx, bx
		ja short chs_ok
		jb short lba_ok
		cmp ax, cx
		jnb short chs_ok
lba_ok:
		mov byte ptr [lba_rw], 1
		clc		
chs_ok:
		retn

get_runix_partition endp

check_hd_image_size proc near
		; 19/12/2019
		; 03/10/2019 - Retro UNIX 386 v2
		; DX:AX = image file size (in bytes)

		; Source code (derived) from
		; hdimage.s - 06/03/2019
		; (TRDOS 386 Hard Disk Image Formatting Utility)

		;test ax, 511
		;jnz short B_04 ; end of file is not on sector boundary!

		;mov word ptr [file_size], ax
		;mov word ptr [file_size+2], dx

		; 19/12/2019
		mov ax, word ptr [file_size]
		mov dx, word ptr [file_size+2]

		; Check for Singlix Master Boot code
		cmp word ptr [Buffer+444], 7BEh
		jne short A_15 ; no ..

		; It is seen as singlix MBR
		; Let's check for disk size words (CHS record)

		; convert (disk image) file size to sector count (disk size)
		; (dx:ax)/512
	
		mov cx, 512
		call div32

		; 12/02/2019
		mov word ptr [total_sectors], ax
		mov word ptr [total_sectors+2], dx

		mov cx, word ptr [Buffer+420] ; cylinders
		cmp cx, 16
		jb short A_15 ; invalid
		mov ax, word ptr [Buffer+422] ; heads
		cmp ax, 2
		jb short A_15 ; invalid
		mov dx, word ptr [Buffer+424] ; sectors
		cmp dx, 17
		jb short A_15 ; invalid
		mov word ptr [cylinders], cx
		mov word ptr [heads], ax
		mov word ptr [sectors], dx
		mul dx
		call mul32
		or bx, bx
		jnz short A_15 ; invalid
		cmp dx, word ptr [total_sectors+2]
		jne short A_15
		cmp ax, word ptr [total_sectors]
		jne short A_15 ; invalid 

		; valid singlix MBR & disk image file

		retn
A_15:
		; cylinders*63sectors*16heads (<=528MB)
		cmp dx, 1F80h ; 1024*16*63 (1F800000h)
		ja short A_21
		jb short A_16
		and ax, ax
		jnz short A_22

		; = 528MB (1024*16*63*512)
		;mov word ptr [heads], 16
		;mov ax, 1024
		;jmp short A_25
		; 04/12/2019
		jmp short A_27
A_16:
		; < 528MB
		mov cx, 63*16 ; 1008
		div cx
		and dx, dx
		jnz short A_17 ; 17 spt disk image check
		;mov word ptr [heads], 16
		;jmp short A_25
		jmp short A_28 ; 04/12/2019
B_04:
		stc
		retn
A_17:
		; 12/02/2019
		mov ax, word ptr [file_size]
		mov dx, word ptr [file_size+2]

		;; 10/02/2019
		;; Check 17 spt disk image
		;mov si, DTA_FileSize
		;lodsw
		;; max. size of hard disk image = 17sectors*16heads*1024cylinders
		;mov dx, [si]
	
		; 04/12/2019
		cmp dx, 880h ; 136MB upper limit (8800000h) for 17spt
		ja short B_04

		mov cx, 512
		call div32

		mov word ptr [pp_Sectors], ax
		mov word ptr [pp_Sectors+2], dx
	
		; Calculate with increased heads (count) order
		; (For example cyls = 1024 & heads = 8 is better than
		;      cyls = 512 & heads = 16, for 17 SPT.v)

		mov cx, 17*2 ; 34 ; heads = 2
A_18:
		call div32
			; Ýf remainder = 0 it is 17spt hard disk image file
		and bx, bx
		jz short A_20
A_19:	
		add cx, 17
		cmp cx, 17*16 ; 272 ; heads = 16
		ja short B_04 ; invalid image file !

		mov ax, word ptr [pp_Sectors]
		mov dx, word ptr [pp_Sectors+2]
	 	
		jmp short A_18
A_21:
		; cylinders*63sectors*32heads (>528MB, <=1GB)
		cmp dx, 3F00h ; 1024*32*63 (3F000000h)
		ja short A_23
		jb short A_22
		and ax, ax
		jnz short A_23
		; = 1GB (1024*32*63*512)
A_27:
		mov ax, 1024
A_28:
		mov word ptr [heads], 16
		jmp short A_25	
A_22:
		mov cx, 63*32
		div cx
		and dx, dx
		jnz short B_04
		mov word ptr [heads], 32
		jmp short A_25
A_23:
		; cylinders*63sectors*64heads (>1GB, <=2GB)
		mov cx, 63*64
		div cx
		and dx, dx
		jnz short B_04
A_24:
		mov word ptr [heads], 64
A_25:
		mov word ptr [sectors], 63
		mov word ptr [cylinders], ax
A_26:
		; 08/02/2019

		; calculate total sectors (by using CHS values)
		mov ax, word ptr [sectors]
		mul word ptr [heads]
		mov word ptr [min_sectors], ax ; Minimum sectors
		mov dx, word ptr [cylinders]
		mul dx
		mov word ptr [total_sectors], ax
		mov word ptr [total_sectors+2], dx
		
		clc
		retn
A_20:
		cmp ax, 1024
		ja short A_19

		mov bl, 17
		mov byte ptr [sectors], bl ; 17
		mov word ptr [cylinders], ax

		mov ax, cx
		div bl
		;xor ah, ah
		mov word ptr [heads], ax

		jmp short A_26

check_hd_image_size endp

PRINT_MSG	proc near
		mov BX,07h  
		mov AH,0Eh  

PRINT_MSG_LOOP:
		lodsb				; Load byte at DS:SI to AL
		and AL,AL            
		jz short PRINT_MSG_OK       
	
		int 10h				; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;AL-char BH-page BL-color
		jmp short PRINT_MSG_LOOP           

PRINT_MSG_OK:
		retn

PRINT_MSG	endp

proc_hex	proc near

		db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
		or AX,'00'                      ; Make it ZERO (ASCII) based

		xchg AH,AL 

; 1999
		cmp AL,'9'
		jna short pass_cc_al
		add AL,7
pass_cc_al:
		cmp AH,'9'
		jna short pass_cc_ah
		add AH,7
pass_cc_ah:

; 1998
		retn

proc_hex	endp

drive_check:
		; 03/12/2019
		; 03/10/2019
		; 09/09/2019 - Retro UNIX 386 v2
		
		; DL = Hard disk drive number 
		
		;;xor bx, bx ; sector 0
		;;mov word ptr [buff_s], bx
		;;mov word ptr [buff_o], offset Buffer
		;;call dskrd

		; 03/10/2019

		;xor ah,ah
		;int 13h
		;jc short drive_not_ready
reset_ok:
		mov bx,offset Buffer
		mov ax,0201h
		mov cx,1
		;mov dl,byte ptr [buff_d]
		;mov dl,byte ptr [drive] ; 03/12/2019
		xor dh,dh
		;push ds
		;pop es
		int 13h
		jnc short drive_check_ok
drive_not_ready:
		; 09/09/2019
		pop ax ; near call return address
		mov si, offset msg_drive_not_ready
		call PRINT_MSG
		jmp rufs_hd_format_8
drive_check_ok:
		retn

div32:
		; DX_AX/CX
		; Result: DX_AX, BX (remainder) 
		mov bx, ax
		;or dx, ax ; * DX_AX = 0 ?       
		;jz short div32_retn ; yes, do not divide! 
		mov ax, dx
  		xor dx, dx
  		div cx	; at first, divide DX
			; remainder is in DX 
		xchg ax, bx ; now quotient is in BX
  			; and initial AX value is in AX
		div cx	; now, DX_AX has been divided and
			; AX has quotient
			; DX has remainder
		xchg dx, bx	; finally, BX has remainder
;div32_retn:
 		retn

mul32:
		; DX_AX*CX
		; Result: BX_DX_AX 
		push cx
		mov bx,dx
		mul cx
 		xchg ax,bx
		push dx
		mul cx 
		pop cx 
		add ax,cx 
		adc dx,0
		xchg bx,ax
		xchg dx,bx
		pop cx
		retn

shl32:
		; 23/01/2020
		; 16/10/2019
		; 18/09/2019 (UNIXCOPY.ASM)
		; INPUT:
		;   CL = shift count
		;xor ch,ch
		jcxz norotal
rotashftl:
		; 23/01/2020
		shl ax,1
		rcl dx,1
		loop rotashftl
norotal:
		retn

shr32:
		; 16/10/2019
		; INPUT:
		;   CL = shift count
		;xor ch,ch
		jcxz norotar
rotashftr:
		shr dx,1
		rcr ax,1
		loop rotashftr
norotar:
		retn

; ***

file_error_msg:
		db 0Dh, 0Ah
		db 'File (Read/Write) Error !'
CRLF:
                db 0Dh, 0Ah, 0
;;;;;
;include	uinstall.asm
include		rufshdi.asm ; installation code for hard disk fs (71h)
;include        unixproc.asm 
include		rufshdp.asm ; runix v2 procedures for hard disk fs (71h)
;;;;;   

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

UNIX_Welcome:
		db 0Dh,0Ah
		db 'RETRO UNIX 386 v2 Hard Disk FS (RUFS) Format Utility'
		db 0Dh, 0Ah
		db 'by Erdogan TAN [20/07/2022]'
		db 0Dh,0Ah
		db 0Dh,0Ah
		db 'Usage: unixhdfs [Drive] ', 0Dh,0Ah
		db '       unixhdfs -i [hard disk image file name] '
		db 0Dh,0Ah
		db 0Dh,0Ah
		db "Drive names:"
		db 0Dh,0Ah
		db 0Dh,0Ah
		db "hd0    (Hard Disk 1)", 0Dh,0Ah
		db "hd1    (Hard Disk 2)", 0Dh,0Ah
		db "...", 0Dh,0Ah
		db "C:     (Hard Disk 1)", 0Dh,0Ah
		db "D:     (Hard Disk 2)", 0Dh,0Ah
		db 0Dh,0Ah
		db 0Dh,0Ah
		db 'Example 1: unixhdfs C: ', 0Dh,0Ah
		db 'Example 2: unixhdfs -i hd0.img '
		db 0Dh,0Ah
		db 0

Msg_DoYouWantToFormat:
		db 07h
		db 0Dh, 0Ah
		db 'WARNING!'
		db 0Dh, 0Ah
		; 04/12/2019
		db 'All data on the RUNIX partition will be erased.'
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db 'Do you want to format Retro UNIX Partition on Drive '
RUFS_DRIVE:
		db 'C: (Yes/No)? ', 0

Msg_Installing_File_System:
		db 0Dh, 0Ah
		db "Installing Retro UNIX v2 File Sytem...", 0

Msg_Writing_Boot_Sector:
		db 0Dh, 0Ah
		;db "Writing UNIX boot sector...", 0
		; 05/04/2022
		db "Writing Retro UNIX v2 boot sector...", 0

Cursor_Pos:     dw 0

Msg_Volume_Name:
		db 0Dh, 0Ah
		db "Volume Name: ", 0
Msg_OK:
		db ' OK.', 0

msg_YES:        db ' YES'
		db 0
msg_NO:         db ' NO'
		db 0   

; 12/8/2012
msg_disk_rw_error:
		db 0Dh, 0Ah
		db 'Disk r/w error!'
		db 0

; 09/09/2019
msg_drive_not_ready:
		db 0Dh, 0Ah
		db 'Drive not ready!'
		db 0

msg_error_Number:
		db 0Dh, 0Ah
		db 'Error No: '
str_err:        dw 3030h
		db 'h'
UNIX_CRLF:
		db 0Dh, 0Ah, 0

;Error_Code:	db 0

RetryCount:     db 4 ; 09/09/2019

str_volume_name:
		db 15 dup (0)

even

; 03/10/2019 - Retro UNIX 386 v2 - Hard Disk FS

heads:		dw 0
sectors:	dw 0 ; spt
cylinders:	dw 0
hidden_sectors:	dd 0
total_sectors:	dd 0
file_size:	dd 0
pp_sectors:	dd 0
min_sectors:	dw 0
lba_rw:		db 0

		db  'Turkish Rational UNIX', 0
		db  'RETRO UNIX 386 v2.0 HDFS by Erdogan TAN', 0
		db  '11/07/2012', 0, '20/07/2022', 0 
even

; 01/12/2019
CHS_limit:	dd 0
; 29/12/2019
VolumeSerial:	dd 0

; 23/12/2019
boot_sector:
; 09/01/2020
; 07/01/2020
; 19/12/2019
; 03/10/2019
; Retro UNIX 386 v2 Hard Disk Boot Sector Image
include         rubs3chs.txt ; rubs3chs.bin (bin2db) file ; 09/01/2020
lba_boot_sector:
include         rubs3lba.txt ; rubs3lba.bin (bin2db) file ; 09/01/2020

; 05/09/2019

img_file_name:  db 13 dup(0)
	        db 0
img_file_handle:
		dw 0

new_file:	db 0

msg_inv_file_name: ; 07/07/2015
		db 0Dh, 0Ah
                db "Invalid file name !", 0Dh, 0Ah
                db "(File name must fit for 8.3 DOS format) !"
                db 0Dh, 0Ah, 0

msg_file_attr_error: ; 07/07/2015
		db 0Dh, 0Ah
                db "File attribute is not proper for hard disk image !", 0Dh, 0Ah
                db "(Directory or write protected file) !"
                db 0Dh, 0Ah, 0    

msg_file_not_found: ; 03/10/2019
		db 0Dh, 0Ah
                db "File not found !", 0Dh, 0Ah
                db "(Hard disk image file must be in working directory) !"
                db 0Dh, 0Ah, 0  

msg_inv_image_file: ; 03/10/2019
		db 0Dh, 0Ah
		db "Invalid hard disk image file !", 0Dh, 0Ah
		db "(File size does not fit to possible CHS parameters) !"
		db 0Dh, 0Ah, 0

msg_inv_partition: ; 03/10/2019
		db 0Dh, 0Ah
		db "Retro UNIX (71h) disk partition not found !"
		db 0Dh, 0Ah, 0

msg_init_img_file:
		db 0Dh, 0Ah
		db "Initializing disk image file sectors...", 0

; 03/12/2019
save_ax:	dw 0FFFFh
save_dx:	dw 0FFFFh	

SizeOfFile	equ $-100

RUFS_INSTL	ends

		end  INSTALL