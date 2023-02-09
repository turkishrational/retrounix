; UNIXCOPY.ASM (Only for 1.44 MB floppy disks)
; ----------------------------------------------------------------------------
; Retro UNIX 386 v2.0 - Modified UNIX v7 inode model - 10/09/2019
; Retro UNIX Operating System Project - Kernel v0.3 by ERDOGAN TAN 
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1.1 - 14 byte file names (04/12/2015)
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; 1.44 MB Floppy Disk 
; Bootable Unix (RUFS) File System - DOS & UNIX FS file export/import Utility
; (08/12/2012)
;
; [ Last Modification: 15/04/2022 ] (Previous: 12/12/2021)
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ****************************************************************************
; 15/04/2022 - mkdir/rmdir link count bugfix
; 28/03/2022 - Improved First Free Inode & First Free Block calculation
; 27/03/2022 - Free Inode Count bugfix
; 23/03/2022 - First Free Inode bugfix
; (UNIXCOPY.COM for Retro UNIX 386 v1.2 & v2 file system)

; 12/12/2021
; UNIXCOPY.COM source code (Microsoft Macro Assembler syntax)
; c:\masm614\> masm unixcopy.asm unixcopy.obj unixcopy.lst
; c:\masm614\> link /t unixcopy.obj

; 21/12/2019 (UNIXCOPY.COM, UNIXCOPY.ASM)
; 19/12/2019 (UNIXHDFS.COM, RUFSHDI.ASM)
; 01/09/2019 - Retro UNIX 386 v2 SuperBlock

; 14/01/2020 - SuperBlock modification :
;	    - Extended sections/divisions (consequental sectors)
;	    - (for swapping, configuration, boot space etc.)	

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
sb_TotalSects  equ sb_VolumeSize

; 21/12/2019
; 19/12/2019 - Retro UNIX 386 v2 HD (071h) partition boot sector 
;	      (UNIXHDFS.ASM)
; 04/12/2015 (14 byte file names - Retro UNIX 386 v1.1)
; 14/07/2015 (8 byte file names - Retro UNIX 8086 v1 & Retro UNIX 386 v1.0)

bsFSystemID 	equ 2  ; db 'RUFS'	
bsVolumeSerial 	equ 6  ; dd 0 ; (4 bytes)
bsFDSign	equ 10 ; db 'fd'
bsDriveNumber 	equ 12 ; db 0 ; fd0 or fd1 (0 or 1)
bsReserved 	equ 13 ; db 0 ; (512 bytes per sector)	
bsSecPerTrack	equ 14 ; db 18 ; (9 or 15)	
bsHeads	equ 15 ; db  ; 2
bsTracks	equ 16 ; dw 80 ; bsCylinders
bs_bf_inode_num equ 18 ; dw 0 ; 0 or Boot/Startup File I-Number
bsInfoEndsign	equ 20 ; db '@'
; 21/12/2019
bsMagic	equ 20 ; db '@'
bsPartitionID	equ 21 ; db 0 ; db 71h
bsHiddenSects	equ 22 ; dd 0 ; Hidden sectors (Boot Sector LBA)

;ROOT_DIR_INODE_NUMBER equ 41
ROOT_DIR_INODE_NUMBER equ 1 ; Retro UNIX 386 v2 - 18/09/2019

; DTA (PSP+80h= Offset 128)
DTA_Attrib   equ 149	; PDP+21 ;05/01/2013	
DTA_Time     equ 150	; PSP+22
DTA_Date     equ 152	; PSP 24
DTA_FileSize equ 154	; PSP + 26
DTA_FileName equ 158	; PSP + 30

;err_INVALIDDATA equ 100h
;err_NOFREEBLOCK equ 200h

;;i_flags    equ 001Eh  ; Retro UNIX v1 (00011110b)
;i_flags     equ 01EDh  ; Retro UNIX v2 (0000000111101101b)	

; 12/12/2021

.8086

UNIXCOPY  SEGMENT PUBLIC 'CODE'
	assume cs:UNIXCOPY,ds:UNIXCOPY,es:UNIXCOPY,ss:UNIXCOPY

	org 100h

START_CODE:

proc_start  proc near
	; 23/01/2020
	; 22/01/2020 - Retro UNIX 386 v2 
	; 08/12/2012 (UNIXCOPY)
	;
	; 30/11/2012 (UNIXBOOT)
	;
	mov	bx, SizeOfFile+100
	add	bx, 15
	shr	bx, 1
	shr	bx, 1
	shr	bx, 1
	shr	bx, 1
	mov	ah, 4Ah ; modify memory allocation
	;push	cs
	;pop	es
	int	21h 
		   
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              
	mov	si, offset 80h	; PSP command tail
	lodsb
	mov	cl, al
	or	cl, cl
	jnz	short loc_get_args
	dec	cl
loc_no_valid_args:
	jmp	loc_unix_welcome          
loc_get_args:
	lodsb
	cmp	al, ' '	      
	jne	short loc_check_img_option ; 23/01/2020

	;dec	cl		   
	;jz	short loc_unix_welcome

	jmp	short loc_get_args

	; Check usage option
loc_check_img_option:
	; 23/01/2020
	cmp	al, '-'
	je	short check_fdi_option ; image file name

	; Check floppy disk drive name
loc_check_fd_name:
	; 23/01/2020
	; 07/07/2015
	;mov	di, offset img_file_name
	cmp	al, "f"
	;jne	short loc_chk_fname1       
	jne	short loc_chk_dos_fname
	;stosb
	lodsb
	cmp	al, "d"
	;jne	short loc_chk_fname1
	jne	short loc_no_valid_args
	;stosb
	;lodsb
	lodsw
	cmp	al, '0'
	;jb	short loc_chk_fname2	     
	jb	short loc_no_valid_args

	cmp	al, '1'
	;ja	short loc_chk_fname2  
	ja	short loc_no_valid_args
	;stosb
	;mov	dl, al
	;lodsb
	;cmp	al, 0Dh
	;ja	short loc_chk_fname2 
	cmp	ah, 20h
	;ja	short loc_chk_fname2  
	ja	short loc_no_valid_args
	
	;mov	byte ptr [UNIX_FD_Number], dl
	;sub	dl, '0'
	;mov	byte ptr [PhysicalDriveNumber], dl
@@:	
	mov	byte ptr [UNIX_FD_Number], al
	sub	al, '0'		; make it zero based  
	mov	byte ptr [PhysicalDriveNumber], al
	
	jmp	load_boot_sector

loc_chk_dos_fname:
	; 23/01/2020
	cmp	byte ptr [si], ':' ; A:, B:
	jne	short loc_no_valid_args
	inc	si
	cmp	byte ptr [si],20h
	ja	short loc_no_valid_args

	cmp	al, 'A'
	jb	short loc_no_valid_args
	cmp	al, 'B'		; A - Z
	jna	short @b	    
	cmp	al, 'a'		; a - z 
	jb	short loc_no_valid_args	
	cmp	al, 'b'	    
	ja	short loc_no_valid_args	

	sub	al, 'a'-'A'	; to upper case
	jmp	short @b	; 23/01/2020

check_fdi_option:
	lodsb
	cmp	al, 'i'
	jne	short loc_no_valid_args
	dec	cl
	jz	short loc_no_valid_args
	lodsb
	cmp	al, ' '
	jne	short loc_no_valid_args
	dec	cl
	jz	short loc_no_valid_args
check_fdi_opt_loop:
	lodsb
	cmp	al, 20h
	ja	short get_fdi_name
	jb	short loc_no_valid_args
	dec	cl
	jz	short loc_no_valid_args
	jmp	short check_fdi_opt_loop
get_fdi_name:
	mov di, offset img_file_name
get_fdi_name_nxt_chr:
	stosb
	dec	cl
	jz	short get_fdi_name_ok	
	lodsb
	cmp	al, 20h
	jna	short get_fdi_name_ok	
	cmp	di, offset img_file_name + 12	
	jb	short get_fdi_name_nxt_chr

	; 23/01/2020
loc_inv_fname:
	mov	si, offset msg_inv_file_name
	jmp	@f

;loc_check_file_name:
;	; 07/07/2015
;	lodsb
;loc_chk_fname1:
;	cmp	al, 0Dh
;	jna	short loc_chk_fname_ok
;loc_chk_fname2:
;	stosb	
;	cmp	di, offset img_file_name + 12
;	jb	short loc_check_file_name
;	jne	short loc_inv_fname
;loc_chk_fname_ok:

	; 23/01/2020
get_fdi_name_ok:
	sub	al, al
	stosb

	;jmp	short cap_file_name 

	; 23/01/2020
cap_file_name:
	; file name capitalization
	mov	si, offset img_file_name
	mov	di, si
	mov	bx, si
loc_cap_file_name0:
	lodsb
	cmp	al, 'a'
	jnb	short loc_cap_file_name2
	and	al, al
	jz	short loc_cap_file_name3
	cmp	al, '.'
	jne	short loc_cap_file_name1
	mov	bx, di ; dot position	
loc_cap_file_name1:
	stosb
	jmp	short loc_cap_file_name0 	
loc_cap_file_name2:
	cmp	al, 'z'
	ja	short loc_cap_file_name1
	and	al, 0DFh ; NOT 32
	stosb
	jmp	short loc_cap_file_name0

loc_cap_file_name3:
	mov	[di], al
	dec	di
	cmp	bx, di
	jnb	short loc_inv_fname	
	sub	di, bx
	sub	bx, offset img_file_name
	cmp	di, 3
	jna	short loc_cap_file_name4
	and	bx, bx
	jnz	short loc_inv_fname
	jmp	short loc_find_image_file	
loc_cap_file_name4:
	cmp	bx, 8
	ja	short loc_inv_fname	
loc_find_image_file:
	; 07/07/2015
	mov	dx, offset img_file_name
	mov	cx, 3Fh ; File Attributes
	mov	ah, 4Eh ; MS Dos Function = Find First File
	int	21h
	jnc	short loc_chk_image_file_features
	cmp	ah, 03h  ; dos error number > 3
	;ja	loc_error
	; 27/03/2022
	ja	short loc_jmp_loc_err

	mov	si, offset msg_file_not_found
	jmp	@f
loc_chk_image_file_features:
	mov	si, DTA_Attrib
	mov	al, byte ptr [SI]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	;jnz	loc_error
	; 27/03/2022
	jz	short loc_chk_image_file_size
loc_jmp_loc_err:
	jmp	loc_error 

loc_chk_image_file_size:
	mov	si, DTA_FileSize
	lodsw
	cmp	word ptr [si], 16h
	jne	short loc_inv_image_file
	cmp	ax, 8000h ; 1.44 MB floppy disk image (168000h bytes)
	je	short loc_open_image_file
loc_inv_image_file:
	mov	si, offset msg_inv_image_file
	jmp	@f 
loc_open_image_file:
	mov	al, 2 ; open for reading and writing
	;mov	dx, offset img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jc	short loc_error
	mov	word ptr [img_file_handle], ax	
	mov	byte ptr [PhysicalDriveNumber], 90h ; image file sign
	;
	mov	bx, ax
	mov	cx, 1024 ; read 1024 bytes (2 sectors)
	mov	dx, offset BSBuffer ; bootsector (& superblock) buffer
	mov	ah, 3Fh ; read file
	int	21h
	jc	short loc_error
	cmp	ax, 1024
	jne	short loc_error
	mov	bx, dx ; offset BSBuffer
	jmp	short load_fd_img_boot_sect_ok			
	;
load_boot_sector:
	; input -> dl = drive number
	xor	ah,ah
	int	13h
	jc	short loc_drv_read_error
load_boot_sector_ok:
	mov	bx, offset BSBuffer
	mov	ax,0202h  ; Read boot sector & superblock
	mov	cx,1
	xor	dh,dh
	int	13h
	jc	short loc_drv_read_error

load_fd_img_boot_sect_ok:
 	cmp	word ptr [bx]+510, 0AA55h
	jne	short loc_not_fd_rufs

	cmp	word ptr [bx]+bsFSystemID, 'UR'
	jne	short loc_not_fd_rufs

	cmp	word ptr [bx]+bsFSystemID+2, 'SF'
	je	short loc_check_fd_sign

loc_not_fd_rufs:
	mov	si, offset msg_Not_Unix_FS
	jmp	short @f

loc_error:
	mov	si, offset error_msg
	jmp	short @f

loc_check_fd_sign:
	cmp	word ptr [BX]+bsFDSign, 'df'
	jne	short loc_not_fd_rufs

	; 24/09/2019 - Retro UNIX 386 v2

	mov	ax, word ptr [systm.sb_FreeMapAddr]
	;add	ax, word ptr [systm.sb_BootSectAddr] ; hidden sectors
	; 22/01/2020
	mov	bx, offset fbm_buffer 
	call	dskrd
	jc	short loc_drv_read_error

	; 22/01/2020 
	;mov	si, offset WriteBuffer
	;mov	di, offset fbm_buffer
	;mov	cx, 256
	;rep	movsw

	mov	ax, word ptr [systm.sb_InodeMapAddr]
	;add	ax, word ptr [systm.sb_BootSectAddr] ; hidden sectors
	; 22/01/2020
	mov	bx, offset im_buffer
	call	dskrd
	jc	short loc_drv_read_error
 
	; 22/01/2020
	;mov	si, offset WriteBuffer
	;mov	di, offset im_buffer
	;mov	cx, 256
	;rep	movsw

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

loc_unix_welcome:
	pushf
	mov	si, offset UNIX_Welcome
	call	PRINT_MSG
	popf
	je	short loc_call_unix_prompt

	mov	si, offset usage
@@:
	call	PRINT_MSG

loc_close_file_then_terminate:	
	; 07/07/2015
	;mov	bx, [img_file_handle]
	; 15/04/2022 ('word ptr' bugfix)
	mov	bx, word ptr [img_file_handle]
	and	bx, bx
	jz	short terminate
close_img_file:
	mov	ah, 3Eh ; close (floppy disk image) file
	int	21h	
terminate:
	int	20h

loc_drv_read_error:
	mov	si, offset msg_unix_drv_read_error

	jmp	short @b

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; call command interpreter
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

loc_call_unix_prompt:
	; 25/09/2019
	mov	si, offset type_27h
	call	PRINT_MSG

	call	unix_prompt

	; 07/07/2015
	jmp	short loc_close_file_then_terminate

proc_start endp

PRINT_MSG proc near
	; 20/01/2013 'call unix_printchr'
 
PRINT_MSG_LOOP:
 	lodsb			; Load byte at DS:SI to AL
	and     AL,AL            
	jz      short PRINT_MSG_OK
   	mov     AH,0Eh	  
	mov     BX,07h             
	int     10h		; BIOS Service func ( ah ) = 0Eh
				; Write char as TTY
				;AL-char BH-page BL-colo
	;call 	unix_printchr	; 20/01/2013
	jmp     short PRINT_MSG_LOOP           

PRINT_MSG_OK:
	retn

PRINT_MSG endp

;unix_printchr	proc near
;	; 20/01/2013      
;	mov     AH,0Eh	  
;	mov     BX,07h             
;	int     10h	    	; BIOS Service func ( ah ) = 0Eh
;				; Write char as TTY
;				;AL-char BH-page BL-color
;	retn
;unix_printchr	endp


unix_prompt proc near
	; 07/07/2015
	; 8/12/2012
	; Derived from
	; proc_dos_prompt procedure of TRDOS, 
	; MAINPROG.ASM (1/1/2012). 
	;
	; proc_dos_prompt (15/09/2011)
	;

 	;push	ds
	;pop	es
unix_prompt_0:	
	; 07/07/2015
	cmp	byte ptr [PhysicalDriveNumber], 90h
	jb	short unix_prompt_1

	; 25/09/2019
@@:
	mov	ah,1 ; clear keyboard buffer
	int	16h
	jz	short @f ; keyboard buffer is empty
	xor	ah,ah ; get character
	int	16h
	jmp	short @b ; loop till keyboard buffer becomes empty
@@:
	mov	si, offset img_file_name
	call 	PRINT_MSG
	mov	si, offset unix_img_cdir
	jmp	short unix_prompt_15
unix_prompt_1:
	mov 	si, offset unix_cdrv
	call 	PRINT_MSG
unix_prompt_2:
	mov 	si, offset unix_cdir
unix_prompt_15:
	call 	PRINT_MSG
unix_prompt_3:
	mov 	al, byte ptr [unix_prompt_char]
 	;mov	ah,0Eh	  
	;mov	bx,07h             
	int     10h         
unix_prompt_4:     
	mov     ah,03h
	;mov	bx,07h	  
	int     10h
	mov     byte ptr [CursorColumn],dl
unix_prompt_5:
	mov     si, offset CommandBuffer
	call    proc_rw_char
	;mov	byte ptr [CommandBuffer]+75,0

	;mov	si, offset CommandBuffer
	mov     di, si
	xor 	bx, bx
	xor     cx, cx
unix_prompt_6:
	mov     al, byte ptr [SI][BX]
	inc	bl 
 	cmp     al, 20h
	ja      short unix_prompt_8
	jb      short unix_prompt_13
	cmp     bl, 74 ; 75 ?
	jb      short unix_prompt_6
	jmp 	short unix_prompt_13
unix_prompt_7:
	mov     al, byte ptr [SI][BX]
	inc     bl
	cmp     al,20h
	jna     short unix_prompt_9
unix_prompt_8:
	stosb   
	inc     cl
	cmp     bl, 74 ; 75 ?
	jb      short unix_prompt_7
	;jmp    short unix_prompt_12
unix_prompt_9:
	xor	al, al ; 0
unix_prompt_10:
	mov     byte ptr [DI], al
	inc    	di
	cmp     bl, 74 ; 75 ?
	jnb     short unix_prompt_12
	mov     al, byte ptr [SI][BX]
	inc     bl
	cmp     al, 20h
	jnb 	short unix_prompt_10
unix_prompt_11:
	mov     byte ptr [DI], 0
unix_prompt_12:
	call    command_interpreter

	cmp     byte ptr [program_exit], 1
	jnb	short unix_prompt_14

	mov     cx, 74 ; 75 ?
	mov     di, offset CommandBuffer
	xor     al,al
	rep     stosb
unix_prompt_13:
	mov     bx,07h 
	mov     al,0Dh
	mov     ah,0Eh
	int     10h
	mov     al,0Ah
	int     10h
	
	jmp     unix_prompt_0 ; loop

unix_prompt_14:
	retn

unix_prompt endp

proc_rw_char proc near
	; 8/12/2012 (modification for UNIXCOPY.ASM)
	; OUTPUT -> DS:SI = Entered String (ASCIIZ)

read_next_char:
	xor     ah,ah
	int     16h
	and     al,al
	jz      short loc_arrow    
	cmp     al,0E0h          
	je      short loc_arrow
	cmp     al,08h
	jne     short char_return
loc_back:
	mov     bl,7
	mov     ah,3
	int     10h
	cmp     dl,byte ptr [CursorColumn]
	ja      short prev_column
loc_beep:
	mov     ah, 0Eh
	mov     al, 7
	int     10h
	jmp     short read_next_char
prev_column:
	dec     dl
set_cursor_pos:
	mov     ah,02h
	int     10h
	mov     bl, dl
	sub     bl,byte ptr [CursorColumn] 
	mov     cx,1
	mov     ah,09h
	mov     al,20h
	mov     byte ptr [SI][BX],al
loc_write_it:
	mov     bl,7
	int     10h
	mov     dx,word ptr [CursorColumn]
	jmp     short read_next_char
loc_arrow:    
	cmp     AH,4Bh
	je      short loc_back
	cmp     AH,53h
	je      short loc_back
	jmp     short read_next_char
char_return:
	mov     bl,7
	mov     ah,3
	int     10h

	mov     ah, dl	
	sub     ah,byte ptr [CursorColumn] 
	cmp     al,20h
	jb      short loc_escape
	cmp     ah, 72 ; limit
	ja      short loc_beep

	mov     bl, ah
	xor     ah, ah
	mov     word ptr [SI][BX],ax
	mov     ah, 0Eh
	mov     bl, 7
	int     10h
	jmp     short read_next_char
pass_escape:
	cmp     al,0Dh
	jne     short read_next_char
	mov     ah, 0Eh	
	mov     bl,7
	int     10h
	mov     al,0Ah
	int     10h
	retn
loc_escape:
	cmp     al,1Bh
	jne     short pass_escape
	stc
	retn

proc_rw_char endp


command_interpreter proc near
	; 27/03/2022
	; 22/01/2020
	; 20/01/2020 (32 bit offset)
	; 04/01/2020
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 01/03/2013
	; 25/02/2013
	; 23/02/2013 ?/help
	; 17/02/2013 namei, inode, iget
	; 16/02/2013 fs, volume 
	; 21/01/2013 'ls -l'
	; 20/01/2013 ls (dir modifications)
	; 13/01/2013 chmod, chown, link
	; 07/01/2013 show tabspace (div) modif.
	; 06/01/2013 show
	; 06/01/2013 rm, mkdir, rmdir modifications
	; 05/01/2013 check file attributes
	; 30/12/2012
	; 24/12/2012 todos
	; 16/12/2012
	; 08/12/2012
	;
	lodsw   ; 25/02/2013
cl4:
	cmp	cl, 4
	;ja	cl5
	;jb	cl3
	; 27/03/2022
	ja	short cl5_or_above
	je	short loc_cmd_exit
	jmp	cl3
; EXIT	
loc_cmd_exit:
	cmp	ax, 'xe'
	jne	short loc_cmd_show
	lodsw
	cmp	ax, 'ti'
	jne	short @f
	lodsb
	or	al, al
	jnz	short @f	

	mov	byte ptr [program_exit], 1
@@:
	retn

	; 27/03/2022
cl5_or_above:
	jmp	cl5

; SHOW
loc_cmd_show:
	; 06/01/2013
	cmp	ax, 'hs'
	jne	short loc_cmd_link
	lodsw
	cmp	ax, 'wo'
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
show_uf1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short show_uf1
	jb	short @f
show_uf2:
	lodsb
	cmp	al, 20h
	ja	short show_uf2
	xor	al, al
	mov	byte ptr [SI]-1, al
show_uf3:
	call	show_file
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
	jmp	ci_error
@@:
	retn
; LINK
loc_cmd_link:
	cmp	ax, 'il'
	jne	loc_cmd_iget ; 17/02/2013
	lodsw
	cmp	ax, 'kn'
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
link_sf1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short link_sf1
	jb	short @b
link_sf2:
	lodsb
	cmp	al, 20h
	ja	short link_sf2
	xor	al, al
	mov	byte ptr [SI]-1, al
link_df1:
	mov	word ptr [arg], si
	lodsb
	cmp	al, 20h
	je	short link_df1
	jb	short @b
link_df2:
	lodsb
	cmp	al, 20h
	ja	short link_df2
	dec	si
	xor	al, al
	mov	byte ptr [SI], al
link_fsf:
	call	namei
	;jc	ci_error
	; 27/03/2022
	jc	short link_err

	mov	word ptr [uf_i_number], ax
link_fdf:
	mov	si, word ptr [arg]
	mov	word ptr [u_namep], si

	;call	namei
	call	namei_x ; 02/10/2019
	;jnc	ci_error
	; 27/03/2022
	jnc	short link_err

	cmp	ah, 0FFh
	;jne	ci_error
	; 27/03/2022
	jne	short link_err

	; [u_dirp] = empty directory entry slot

	mov	ax, word ptr [ii]
	mov	word ptr [pdir], ax
	
	mov	ax, word ptr [uf_i_number]
	call	iget
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
link_err:
	jmp	ci_error
@@:
	; 19/09/2019
	mov	byte ptr [imodx], 1 ; 19/09/2019 - Retro UNIX 386 v2
		; (flag means file data is same
		;  but inode's itself is modified)

	inc	byte ptr [inode_nlks] ; link count

	call	setimod ; jsr r0,setimod / set modified flag

	;inc	byte ptr [inode_nlks] ; link count

	mov	ax, word ptr [pdir]
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short link_err

	; namei -> u_namep points filename 
	;       after the last '/' of the path
	
	mov	ax, word ptr [uf_i_number]
	mov	word ptr [u_dirbuf], ax

	call	mkdir ; make directory entry
	;jc	ci_error
	; 27/03/2022
	jc	short link_err	

	jmp	ci_sync_exit
; IGET
loc_cmd_iget:   ; 17/02/2013, inode/iget
	cmp	ax, 'gi'
	jne	short loc_cmd_help ; 23/02/2013
	lodsw
	cmp	ax, 'te'
	jne	short @f
	lodsb
	or	al, al
	jnz	short @f	
ci_iget_getarg:
	mov	bx, si
	lodsb
	cmp	al, 20h
	;ja	inode_getarg2
	;je	short ci_iget_getarg
	; 27/03/2022
	jb	short loc_cmd_help
	je	short ci_iget_getarg
	jmp	inode_getarg2
; HELP
loc_cmd_help:	; 23/02/2013
	cmp	ax, 'eh'
	jne	short @f
	lodsw
	cmp	ax, 'pl'
	jne	short @f
	lodsb
	and	al, al
	jnz	short @f
ci_?:
	mov	si, offset UNIXCOPY_Commands
	call	PRINT_MSG
@@:
	retn  
cl5:
	cmp	cl, 5
	;ja	cl7
	;;jb	short @f
	; 27/03/2022
	jna	short loc_cmd_chdir
	jmp	cl7
; CHDIR
loc_cmd_chdir:
	cmp	ax,'hc'
	jne	loc_cmd_todos
	lodsw
	cmp	ax, 'id'
	jne	short loc_cmd_chmod
	lodsb
	cmp	al, 'r'
	jne	short @f
	lodsb
	or	al, al
	jnz	short @f
ci_cd_getarg:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short ci_cd_getarg
	jb	short @f
	; dec si

	mov	ax, word ptr [u_namep]
	mov	word ptr [arg], ax

	call	sys_chdir
	;jc	ci_error
	; 27/03/2022
	jc	short loc_chdir_err
	
	mov	si, word ptr [arg]
	call	update_cdir_string
@@:
	retn

; CHMOD
loc_cmd_chmod: ; 13/01/2013
	;cmp	ax, 'hc'
	;jne	short loc_cmd_todos
	;lodsw
	cmp	ax, 'om'
	jne	short loc_cmd_chown
	lodsb	
	cmp	al, 'd'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
ci_chmod_getarg:
	lodsb
	cmp	al, 20h
	je	short ci_chmod_getarg
	jb	short @b
	dec	si
	call	chmode
	;jc	ci_error
	; 27/03/2022
	jnc	short ci_chmod_print
loc_chdir_err:	; 27/03/2022
loc_chmod_err:
loc_chown_err:
loc_chgrp_err:
	jmp	ci_error

	; 27/03/2022
ci_chmod_print:
	; 01/10/2019 - Retro UNIX 386 v2
ci_chown_print:
ci_chgrp_print: ; 01/10/2019
	mov	si, offset msg_arg
	call	PRINT_MSG
	jmp	ci_sync_exit

; CHOWN
loc_cmd_chown: ; 13/01/2013
	;cmp	ax, 'hc'
	;jne	short loc_cmd_todos
	;lodsw
	cmp	ax, 'wo'
	;jne	short @b
	jne	short loc_cmd_chgrp ; 01/10/2019
	lodsb	
	cmp	al, 'n'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
ci_chown_getarg:
	lodsb
	cmp	al, 20h
	je	short ci_chown_getarg
	jb	short @b
	dec	si
	call	chowner
	;jc	ci_error
	; 27/03/2022
	jc	short loc_chown_err

	and	bx, bx
	jnz	short ci_chown_print
@@:
	retn

; CHGRP
loc_cmd_chgrp: ; 01/10/2019 - Retro UNIX 386 v2
	;cmp	ax, 'hc'
	;jne	short loc_cmd_todos
	;lodsw
	cmp	ax, 'rg'
	jne	short @b
	lodsb	
	cmp	al, 'p'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
ci_chgrp_getarg:
	lodsb
	cmp	al, 20h
	je	short ci_chgrp_getarg
	jb	short @b
	dec	si
	call	chgroup
	;jc	ci_error
	; 27/03/2022
	jc	short loc_chgrp_err

	and	bx, bx
	jnz	short ci_chgrp_print
@@:
	retn
; TODOS
loc_cmd_todos:
	; 24/12/2012
	cmp	ax, 'ot'
	jne	loc_cmd_mkdir ; 30/12/2012
	lodsw
	cmp	ax, 'od'
	jne	short @b
	lodsb	
	cmp	al, 's'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
todos_uf1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short todos_uf1
	jb	short @b
todos_uf2:
	lodsb
	cmp	al, 20h
	ja	short todos_uf2
	xor	al, al
	mov	byte ptr [SI]-1, al
todos_df1:
	mov	word ptr [arg], si
	lodsb
	cmp	al, 20h
	je	short todos_df1
	jb	short @b
todos_df2:
	lodsb
	cmp	al, 20h
	ja	short todos_df2
	dec	si
	xor	al, al
	mov	byte ptr [SI], al
todos_fuf:
	call	namei
	;;jnc	short @f
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
todos_err:
	jmp	ci_error

	;cmp	ah, 0FFh
	;jne	ci_error
	;;jmp	ci_error ; 'file not found' error
@@:
	mov	word ptr [uf_i_number], ax
todos_fdf:
	mov	dx, word ptr [arg]
	mov	cx, 3Fh ; File Attributes ; 05/01/2013 (3Fh)
	mov	ah, 4Eh ; MS Dos Function = Find First File
	int	21h
	;jnc	short todos_afow
	jnc	short @f ; 05/01/2013
todos_chk_err:
	cmp	ah,03h  ; dos error number > 3
	;ja	ci_error
	; 27/03/2022
	ja	short todos_err

	jmp	short todos_crdf
@@:
	; 05/01/2013
	mov	si, DTA_Attrib
	mov	al, byte ptr [SI]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	;jnz	ci_error
	; 27/03/2022
	jnz	short todos_err      
todos_afow:
	; overwrite question
	mov	si, offset msg_overwrite_question1
	call	PRINT_MSG
	mov	si, DTA_FileName
	call	PRINT_MSG
	mov	si, offset msg_overwrite_question2
	call	PRINT_MSG
	mov	si, offset msg_yes_no
	call	PRINT_MSG
todos_afow_input:	; ask for overwrite
	xor	ax, ax
	int	16h		; wait for keyboard command
	cmp	al, 'C'-40h
	je	short @f	   
	cmp	al, 27
	je	short @f
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short todos_afow_yes	; overwrite
	cmp	al, 'N'			; No?
	jne	short todos_afow_input         
todos_afow_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
@@:
	retn
todos_afow_yes:
	mov	si, offset msg_YES
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
todos_crdf:
	;mov	dx, word ptr [arg]
	xor	cx, cx ; File Attributes = 0
	mov	ah, 3Ch ; MS DOS Function = Create File
	int	21h
	;jc	ci_error
	; 27/03/2022
	jc	short todos_crdf_err

	mov	word ptr [FileHandle], ax
todos_odf:
	mov	dx, word ptr [arg]
	mov	ah, 3Dh ; MS DOS Function = Open File
	xor	al, al  
	int	21h
	;jc	ci_error
	; 27/03/2022
	jnc	short todos_ruf_wdf
todos_crdf_err:
todos_odf_err:
	jmp	ci_error
	
todos_ruf_wdf:
	xor	ax, ax 
	mov	word ptr [u_off], ax
	; 20/01/2020
	mov	word ptr [u_off+2], ax
todos_wf_msg:
	mov	si, offset Msg_writing_file
	call	PRINT_MSG
todos_iget:	
	mov	ax, word ptr [uf_i_number]
	call	iget
	jc	short todos_cdf

	; 20/01/2020
	mov	cx, 512
todos_read_unix_sf_1:
       	mov	ax, word ptr [inode_size]
	mov	dx, word ptr [inode_size+2] 
	sub	ax, word ptr [u_off]
	sbb	dx, word ptr [u_off+2] 
	jnz	short @f

	and	ax, ax
	jz	short todos_cdf

	cmp	ax, cx ; 512
	jna	short todos_read_unix_sf_2
@@:	
	mov	ax, cx ; 512
;loc_read_unix_sf:
todos_read_unix_sf_2:	
	mov	word ptr [u_count], ax

	mov	word ptr [u_base], offset ReadBuffer

	mov	ax, word ptr [uf_i_number] ; word ptr [u_dirbuf]
	call	readi
	jc	short todos_cdf
@@:
;loc_write_dos_df:	
	mov	ah, 40h ; Write File
	mov	cx, word ptr [u_nread] ; 0 -> eof
	mov	dx, offset ReadBuffer
	mov	bx, word ptr [FileHandle]
	int	21h
	jc	short todos_cdf


	;or	ax, ax  ; or cx, cx
	;;jnz	short loc_read_unix_sf ; ax = 512
	;; 20/01/2020
	;jnz	short todos_read_unix_sf_1

	; 20/01/2020
	;cmp	ax, 512
	cmp	cx, 512 ; no..
	je	short todos_read_unix_sf_1 
			; there may be next sector to read
	cmp	ax, cx ; write count = read count ? 
	jne	short todos_cdf ; jb short todos_cdf

	; If we are here (ax,cx < 512) 
	; it is end of file or there is a write error 
todos_cdf:
	pushf
	jc	short @f
todos_set_dfdt:
	mov	ax, word ptr [inode_ctim]   ; fromdos command ->
	mov	dx, word ptr [inode_ctim]+2 ; dos lmdt -> unix ctim

	call	convert_from_epoch

	mov	dx, word ptr [hour]
	mov	cl, 11
	shl	dx, cl   
	mov	ax, word ptr [minute]
	mov	cl, 5
	shl	ax, cl
	or	dx, ax
	mov	ax, word ptr [second]
	shr	ax, 1
	or	ax, dx
	push	ax ; time
	mov	dx, word ptr [year]
	sub	dx, 1980
	mov	cl, 9
	shl	dx, cl   
	mov	ax, word ptr [month]
	mov	cl, 5
	shl	ax, cl
	or	dx, ax
	mov	ax, word ptr [day]
	or	dx, ax
	pop	cx ; time

	mov	ax, 5701h ; set lm date&time
	mov	bx, word ptr [FileHandle]

	int	21h
@@:
	mov	ah, 3Eh ; Close File
	mov	bx, word ptr [FileHandle]
	int	21h
	popf
	;jc	ci_error
	; 27/03/2022
	jnc	short todos_retn
loc_mkdir_err:
	jmp	ci_error

todos_retn:
	mov	si, offset Msg_OK
	call	PRINT_MSG

	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
@@:	
	retn
; MKDIR
loc_cmd_mkdir:
	; 30/12/2012
	cmp	ax, 'km'
	jne	short loc_cmd_rmdir
	lodsw
	cmp	ax, 'id'
	jne	short @b
	lodsb	
	cmp	al, 'r'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
ci_mkdir_getarg1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short ci_mkdir_getarg1
	jb	short @b
ci_mkdir_getarg2: ; 06/01/2013
	lodsb
	cmp	al, 20h
	ja	short ci_mkdir_getarg2
	dec	si
	xor	al, al
	mov	byte ptr [SI], al
	
	mov	si, offset Msg_Making_Directory
	call	PRINT_MSG

	call	make_directory
	;jc	ci_error
	; 27/03/2022
	jc	short loc_mkdir_err

	jmp	ci_sync_exit
; RMDIR
loc_cmd_rmdir:
	; 05/01/2013
	cmp	ax, 'mr'
	jne	short loc_cmd_namei ; 17/02/2013
	lodsw
	cmp	ax, 'id'
	jne	short @b
	lodsb	
	cmp	al, 'r'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
ci_rmdir_getarg1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short ci_rmdir_getarg1
	jb	short @b
	; 06/01/2013
	mov	ah, al
ci_rmdir_getarg2: 
	lodsb
	cmp	al, 20h
	ja	short ci_rmdir_getarg2
	dec	si
	xor	al, al
	mov	byte ptr [SI], al
	
	mov	al, '.'
	cmp	ah, al ; dot
	jne	short @f

	mov	ah, byte ptr [SI]
	cmp	ah, 21h
	;jb	ci_error
	; 27/03/2022
	jb	short loc_rmdir_err

	cmp	ah, al ;'.' ; dotdot (parent dir)
	jne	short @f

	inc	si
	cmp	byte ptr [SI], 21h
	;jb	ci_error
	; 27/03/2022
	jnb	short @f
loc_rmdir_err:
	jmp	ci_error	
@@:
	; u_namep = pointer to directory path name

	call	namei
	;jc	ci_error
	; 27/03/2022
	jc	short loc_rmdir_err

	cmp	ax, ROOT_DIR_INODE_NUMBER
	;je	ci_error
	; 27/03/2022
	je	short loc_rmdir_err

	cmp	ax, word ptr [u_cdir]
	;je	ci_error
	; 27/03/2022
	je	short loc_rmdir_err

	push	ax
	mov	si, offset Msg_Removing_Directory
	call	PRINT_MSG
	pop	ax

	call	remove_directory
	;jc	ci_error
	; 27/03/2022
	jc	short loc_rmdir_err

	jmp	ci_sync_exit

; NAMEI	; 17/02/2013, print i-number of file/directory
loc_cmd_namei:
	cmp	ax, 'an'
	jne	short loc_cmd_inode
	lodsw
	cmp	ax, 'em'
	jne	short @f
	lodsb	
	cmp	al, 'i'	
	jne	short @f
	lodsb
	or	al, al
	jnz	short @f
namei_sf1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short namei_sf1
	jb	short @f
namei_sf2:
	lodsb
	cmp	al, 20h
	ja	short namei_sf2
	dec	si
	xor	al, al
	mov	byte ptr [SI], al
namei_fsf:
	call	namei
	jnc	short namei_iget
	cmp	ah, 0FFh
	;jb	ci_error
	; 27/03/2022
	jb	short loc_namei_err	

	mov	si, offset NotFound_msg
	call	PRINT_MSG
@@:
	retn

namei_iget:
	call	iget
	;jc	ci_error
	; 27/03/2022
	jnc	short namei_print_inum
loc_namei_err:
loc_inode_err:
	jmp	ci_error

namei_print_inum:
	mov	cx, ax
	mov	si, offset msgINumber
	call	PRINT_MSG	
	mov	ax, cx
	mov	cx, 3
	call	print_decimal_number
	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	;retn
	; 27/03/2022
	jmp	PRINT_MSG

; INODE	; 17/02/2013, print inode structure/details
loc_cmd_inode:
	cmp	ax, 'ni'
	jne	short @b
	lodsw
	cmp	ax, 'do'
	jne	short @b
	lodsb	
	cmp	al, 'e'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
inode_getarg1:
	mov	bx, si
	lodsb
	cmp	al, 20h
	je	short inode_getarg1
	ja	short inode_getarg2
	mov	ax, word ptr [ii]
	jmp	short @f
inode_getarg2:
	lodsb
	cmp	al, 20h
	ja	short inode_getarg2
	dec	si
	xor	ax, ax
	mov	byte ptr [SI], al
	mov	si, bx
@@:
	call	show_inode
	;jc	ci_error
	; 27/03/2022
	jc	short loc_inode_err
@@:	
	retn

cl3:
	cmp	cl, 3
	jb	short cl2
; DIR
loc_cmd_dir:	; 05/01/2013 @b->@f, dir_print modifications
	cmp	ax, 'id'
	;jne	short @f
	jne	short cls
	lodsb
	cmp	al, 'r'
	jne	short @f
	lodsb
	or	al, al
	jnz	short @f
	mov	byte ptr [ls_option], al ; 20/01/2013
dir_getarg:	; 30/12/2012
	lodsb
	cmp	al, 20h
	je	short dir_getarg
	jnb	short dir_namei
ls_getarg3:
	xor	ax, ax
	jmp	short dir_print
dir_namei:      ; 30/12/2012
	dec	si
	mov	word ptr [u_namep], si
	;call	namei
	call	namei_x ; 02/10/2019	
	jc	short ci_error
	;ax = i-number
dir_print:
	call	print_directory_list
	jnc	short @f
ci_error:
	mov	si, offset error_msg
	call	PRINT_MSG
@@:	
	retn

; 23/02/2013
cl1:
	cmp	al, '?'
	jne	short @b
	cmp	ah, 0
	;je	ci_?
	; 27/03/2022
	;jne	short @f
	jne	short @b
	jmp	ci_?
;@@:	
;	retn

cls:
; 25/09/2019 - Retro UNIX 386 v2 ('cls' command)
	cmp	ax,'lc'
	jne	short @b
	lodsb
	cmp	al,'s'
	jne	short @b
	lodsb
	and	al, al
	jnz	short @b

	; clear screen.. (by setting video mode to 3 again) 		
	mov	ax, 3
	int	10h
	retn

; 16/12/2012
cl2:
	cmp	cl, 2
	jb	short cl1 ; 23/02/2013
	;jb	@b
; CD (CHDIR)
loc_cmd_cd:
	cmp	ax, 'dc'
	jne	short loc_cmd_ls
	lodsb
	or	al, al
	jnz	short @b
	jmp	ci_cd_getarg
; LS (DIR)
loc_cmd_ls:	; 20/01/2013
	cmp	ax, 'sl'
	jne	short loc_cmd_rm
	lodsb
	or	al, al
	jnz	short @b
	mov	byte ptr [ls_option], 1
ls_getarg1:	; 21/01/2013
	lodsb
	cmp	al, 20h
	je	short ls_getarg1
	jb	short ls_getarg3
ls_getarg2:
	cmp	al,'-'
	jne	short dir_namei
	lodsb
	cmp	al, 'l' ; ls -l ?
	jne	short ls_getarg3
ls_getarg4:
	lodsb
	inc	byte ptr [ls_option]
	cmp	al, 20h
	je	short dir_getarg
	jb	short ls_getarg3
	dec	byte ptr [ls_option]
	jmp	short ls_getarg3
; RM
loc_cmd_rm:	
	cmp	ax, 'mr'
	jne	loc_cmd_fs ; 16/02/2013
	lodsb
	or	al, al
	jnz	short @b
rm_getarg:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short rm_getarg
	jb	short @b
rm_namei:
	call	namei
	;jc	ci_error
	; 27/03/2022
	jc	short loc_rm_err

	;cmp	word ptr [ii], 41 ; i-number of the directory
	;jne	short @f

	mov	si, offset BSBuffer + bs_bf_inode_num
	cmp	ax, word ptr [si] ; is it i-number of the boot file
	;je	short ci_error
	jne	short @f

	;cmp	word ptr [ii], 41 ; i-number of root directory
	cmp	word ptr [ii], 1 ; 25/09/2019 - Retro UNIX 386 v2
	;je	ci_error
	; 27/03/2022
	je	short loc_rm_err

	mov	word ptr [si], 0  ; reset wrong boot file configuration
@@:	
	mov	word ptr [uf_i_number], ax ; word ptr [u_dirbuf]
	; 05/01/2013
	mov	dx, word ptr [ii]
	mov	word ptr [pdir], dx
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short loc_rm_err

	mov	ax, word ptr [inode_flgs]
	; 25/09/2019 - Retro UNIX 386 v2
	;	(modified unix v7 inode format)
	test	ah, 80h
	;jz	ci_error ; not regular file or directory
	; 27/03/2022
	jz	short loc_rm_err
	
	test	ah, 40h	; 'directory' flag
	;jnz	ci_error
	; 27/03/2022
	jnz	short loc_rm_err	

	test	al, 80h  ; owner's write permission
	;jz	ci_error ; read only file !
	; 27/03/2022
	jnz	short rm_move_fn
loc_rm_err:
	jmp	ci_error

rm_move_fn:
	mov	si, offset u_dirbuf + 2
	mov	di, offset Boot_File_Name
	mov	cx, 14  ; 04/12/2015 (8 -> 14)
@@:
	lodsb
	and	al, al
	jz	short @f
	stosb
	loop	@b
	xor	al, al ; 06/01/2013
@@:
	mov	byte ptr [di], al ; 0

	mov	si, offset msg_remove_question1
	call	PRINT_MSG
	mov	si, Offset Boot_File_Name
	call	PRINT_MSG
	mov	si, offset msg_remove_question2
	call	PRINT_MSG
	mov	si, offset msg_yes_no
	call	PRINT_MSG
rm_yn_input:	; ask for remove
	xor	ax, ax
	int	16h	; wait for keyboard command
	cmp	al, 'C'-40h
	je	short @f	   
	cmp	al, 27
	je	short @f
	and	al, 0DFh
	cmp	al, 'Y'	; Yes?
	je	short rm_a_yes  ; overwrite
	cmp	al, 'N'	; No?
	jne	short rm_yn_input         
rm_a_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
@@:
	retn
rm_a_yes:
	mov	si, offset msg_YES
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
rm_unlink:
	mov	si, offset Msg_removing_file
	call	PRINT_MSG

	mov	ax, word ptr [uf_i_number]
	call	unlink
	;jc	ci_error
	; 27/03/2022
	jc	short loc_rm_err
	
	jmp	ci_sync_exit

; FS (Volume)  ; 16/02/2013   (File System / Volume Info)
loc_cmd_fs:	
	cmp	ax, 'sf'
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
vol_infO_print:
fs_info_print:
	call	print_volume_info
@@:
	retn
cl6: ; 16/02/2013	
	cmp	cl, 6
	jne	short @b 

; VOLUME (fs)  ; 16/02/2013
loc_cmd_volume:
	cmp	ax, 'ov'
	jne	short @b
	lodsw
	cmp	ax, 'ul'
	jne	short @b
	lodsw
	cmp	ax, 'em'
	jne	short @b
	lodsb
	or	al, al
	jz	short vol_infO_print
@@:
	retn
; 15/12/2012
cl7:
	cmp	cl, 7
	;jb	short cl6 ; 16/02/2013
	;ja	cl8
	; 27/03/2022
	je	short loc_cmd_fromdos
	jb	short cl6
	jmp	cl8
; FROMDOS
loc_cmd_fromdos:
	cmp	ax, 'rf'
	jne	short @b
	lodsw
	cmp	ax, 'mo'
	jne	short @b
	lodsw
	cmp	ax, 'od'
	jne	short @b
	lodsb	
	cmp	al, 's'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
fromdos_df1:
	mov	word ptr [arg], si
	lodsb
	cmp	al, 20h
	je	short fromdos_df1
	jb	short @b
fromdos_df2:
	lodsb
	cmp	al, 20h
	ja	short fromdos_df2
	xor	al, al
	mov	byte ptr [si]-1, al
fromdos_uf1:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short fromdos_uf1
	jb	short @b
fromdos_uf2:
	lodsb
	cmp	al, 20h
	ja	short fromdos_uf2
	dec	si
	xor	al, al
	mov	byte ptr [si], al
fromdos_fdf:
	mov	dx, word ptr [arg]
	mov	cx, 27h ; File Attributes
	mov	ah, 4Eh ; MS DOS Function = Find First File
	int	21h
	;jc	ci_error ; file not found
	; 27/03/2022
	jnc	short fromdos_fuf
fromdos_err:
	jmp	ci_error
	
fromdos_fuf:
	call	namei
	jnc	short @f

	cmp	ah, 0FFh
	;jne	ci_error
	; 27/03/2022
	jne	short fromdos_err
	
	xor	ax, ax
	mov	word ptr [uf_i_number], ax
	jmp	fromdos_s_fs_mdt
@@:
	mov	word ptr [uf_i_number], ax
	; 30/09/2019
	; 05/01/2013
	;mov	dx, word ptr [ii]
	;mov	word ptr [pdir], dx
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short fromdos_err

	mov	ax, word ptr [inode_flgs]
	; 25/09/2019 - Retro UNIX 386 v2
	;	(modified unix v7 inode format)
	test	ah, 80h
	;jz	ci_error  ; not regular file or directory
	; 27/03/2022
	jz	short fromdos_err
	
	test	ah, 40h ; 'directory' flag
	;jnz	ci_error
	; 27/03/2022
	jnz	short fromdos_err

	test	al, 80h ; owner's write permission
	;jz	ci_error  ; read only file !
	; 27/03/2022
	jz	short fromdos_err

fromdos_afow:
	mov	si, offset u_dirbuf + 2
	mov	di, offset Boot_File_Name
	mov	cx, 14 ; 04/12/2015 (8 -> 14)
@@:
	lodsb
	and	al, al
	jz	short @f
	stosb
	loop	@b
	xor	al, al ; 01/03/2013
@@:
	mov	byte ptr [di], al ; 0

	mov	si, offset msg_overwrite_question1
	call	PRINT_MSG
	mov	si, Offset Boot_File_Name
	call	PRINT_MSG
	mov	si, offset msg_overwrite_question2
	call	PRINT_MSG
	mov	si, offset msg_yes_no
	call	PRINT_MSG
fromdos_afow_input:  ; ask for overwrite
	xor	ax, ax
	int	16h		; wait for keyboard command
	cmp	al, 'C'-40h
	je	short @f	   
	cmp	al, 27
	je	short @f
	and	al, 0DFh
	cmp	al, 'Y'			; Yes?
	je	short fromdos_afow_yes	; overwrite
	cmp	al, 'N'			; No?
	jne	short fromdos_afow_input         
fromdos_afow_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
@@:
	retn
fromdos_afow_yes:
	mov	si, offset msg_YES
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
fromdos_uf_itrunc:
	; 30/09/2019
	; 05/01/2013
	;mov	ax, word ptr [pdir]
	;call	iget
	;jc	ci_error
	;
	mov	ax, word ptr [uf_i_number]
	call	itrunc ; truncate file
	;jc	ci_error
	; 27/03/2022
	jnc	short fromdos_s_fs_mdt
	jmp	ci_error

fromdos_s_fs_mdt:
	; 15/12/2012
	; Derived from UNIXBOOT.ASM (30/11/2012)

	; 14/01/2020 (32 bit file size permission)
	;;mov 	si, DTA_FileSize
	;mov	si, DTA_FileSize+2
	;;mov	ax, word ptr [si]
	;;mov 	dx, word ptr [si]+2
	      ;;or  ax, dx  ; 64KB file size limit
	;mov	ax, word ptr [si]
	;and	ax, ax
	;jnz	ci_error

	;mov 	word ptr [file_Size], ax

	mov     si, DTA_Date
	mov 	ax, word ptr [si]
	push 	ax
	and 	ax, 00011111b	 ; Day Mask
	mov 	word ptr [day], ax
	pop 	ax
     	mov     cl, 5
	shr     ax, cl	   ; shift right 5 times
	push 	ax 
	and     ax, 00001111b	; Month Mask
  	mov 	word ptr [month], ax
	pop 	ax
	mov     cl, 4
	shr     ax, cl
	;and	ax, 01111111b	; Result = Year - 1980
	add     ax, 1980
	mov     word ptr [year], ax
 		
	mov     si, DTA_Time
	mov 	ax, word ptr [si]
	push 	ax
	and     ax, 0000011111b	; Second Mask
	shl 	al, 1	
	mov     word ptr [second], ax
	pop 	ax
	mov     cl, 5
	shr     ax, cl	   ; shift right 5 times
	push	ax
	and     ax, 0000111111b	; Minute Mask
	mov     word ptr [minute], ax
	pop 	ax
	mov	cl, 6		; shift right 6 times
	shr     ax, cl	   ; (6+5=11)
	mov     word ptr [hour], ax	; ax = hours	
           
	call	convert_to_epoch

	mov	word ptr [uf_make_datetime], ax
	mov	word ptr [uf_make_datetime]+2, dx
fromdos_odf:
	mov	dx, word ptr [arg]
	mov	ah, 3Dh ; MS DOS Function = Open File
	xor	al, al  
	int	21h
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
fromdos_odf_err:	
	jmp	ci_error
@@:
	mov	word ptr [FileHandle], ax

	mov	ax, word ptr [uf_i_number]
	; 23/02/2013
	and	ax, ax
	jz	short @f
	;jnz	short fromdos_wf_msg  ;@f
	xor	ax, ax
	call	fromdos_maknod
	; 27/03/2022
	; (As result of adding [uf_i_number] check code in maknod,
	;  increasing free inode count here is not needed.)
	; ((otherwise, free inode count would need to be increased
	;   again; because maknod was decreasing it
	;   -before this modification-))
	; 27/03/2022	
	;inc	word ptr [systm.sb_FreeInodes]  ; (bugfix)

	jmp	short fromdos_wf_msg

@@: ; fromdos_mknod:
	;mov	ax, i_flags ; 1Eh (unix v1), 01EDh (Runix v2)
	; 25/09/2019 - Retro UNIX 386 v2
	mov	ax, 81EDh ; (1000000111101101b)	
	call	mak_nod
	jc	short fromdos_cf
	; 04/01/2020
	mov	word ptr [uf_i_number], ax
fromdos_wf_msg:
	mov	si, offset Msg_writing_file
	call	PRINT_MSG
@@:	; 16/12/2012
	xor	ax, ax 
	mov	word ptr [u_off], ax
	; 20/01/2020
	mov	word ptr [u_off+2], ax
@@:
;loc_read_dos_sf:	
	mov	ah, 3Fh ; Read File
	mov	cx, 512
	mov	dx, offset ReadBuffer
	mov	bx, word ptr [FileHandle]
	int	21h
	jc	short fromdos_cf

	or	ax, ax
	jz	short fromdos_cf
	
	mov	word ptr [u_count], ax

	mov	word ptr [u_base], offset ReadBuffer

	mov	ax, word ptr [uf_i_number] ; word ptr [u_dirbuf]
	call	writei
	jnc	short short @b ; loc_read_dos_sf
fromdos_cf:
	pushf
	mov	ah, 3Eh ; Close File
	mov	bx, word ptr [FileHandle]
	int	21h
	popf
	;jc	ci_error
	; 27/03/2022
	jc	short fromdos_cf_err
@@:
	; 23/02/2013
	xor	ax, ax
	mov	word ptr [inode_mtim], ax
	mov	word ptr [inode_mtim]+2, ax
	
	; 20/09/2019 - Retro UNIX v2
	;mov	word ptr [inode_atim], ax
	;mov	word ptr [inode_atim]+2, ax

	call	setimod
ci_sync_exit:
	call	sync
	;jc	ci_error
	; 27/03/2022
	jnc	short fromdos_retn
fromdos_cf_err:
fromdos_sync_err:
	jmp	ci_error

fromdos_retn:
	mov	si, offset Msg_OK
	call	PRINT_MSG

	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
@@:	
	retn
cl8:
	cmp	cl, 8
	;;jb	short @b
	;ja	cl10
	; 27/03/2022
	jna	short loc_cmd_bootfile
	jmp	cl10
; BOOTFILE
loc_cmd_bootfile:
	cmp	ax, 'ob'
	jne	short @b
	lodsw
	cmp	ax, 'to'
	jne	short @b
	lodsw
	cmp	ax, 'if'
	jne	short @b
	lodsw	
	cmp	ax, 'el'	
	jne	short @b
	lodsb
	or	al, al
	jnz	short @b
@@:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short @b
	ja	short ci_bf_namei

	mov	si, offset BSBuffer + bs_bf_inode_num
	mov	ax, word ptr [si]
	and	ax, ax
	jnz	short @f
ci_no_bootfile:	
	mov	si, offset msg_Startup_File_Not_Exists
	;call	PRINT_MSG
	;retn
	; 20/09/2019
	jmp	PRINT_MSG	
@@:
	call	find_bfn
	;jc	ci_error
	; 27/03/2022
	jnc	short ci_move_bfn_1
loc_bootfile_err:
	jmp	ci_error

ci_move_bfn_1:
	mov	si, offset u_dirbuf + 2
	mov	di, offset Boot_File_Name
	mov	cx, 14 ; ; 04/12/2015 (8 -> 14)
ci_move_bfn_2:
	lodsb
	; 04/12/2015
	and	al, al
	jz	short ci_move_bfn_3
	stosb
	loop	ci_move_bfn_2
ci_move_bfn_3:
	mov	byte ptr [di], al ; 0
	;
	;call	proc_display_startupfile_info
	;retn
	; 20/09/2019
	jmp	proc_display_startupfile_info	
ci_bf_namei:
	call	namei
	;jc	ci_error
	; 27/03/2022
	jc	short loc_bootfile_err
	
	cmp	word ptr [ii], ROOT_DIR_INODE_NUMBER
	;jne	ci_error
	; 27/03/2022
	jne	short loc_bootfile_err

	; 05/01/2013
	; ax = i-number of (new) boot file
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short loc_bootfile_err

	;test	word ptr [inode_flgs], 4000h ; directory ?
	test	byte ptr [inode_flgs+1], 40h ; 25/09/2019
	;jnz	ci_error
	; 27/03/2022
	jnz	short loc_bootfile_err	

	;test	byte ptr [inode_flgs+1], 80h ; 25/09/2019
	;jnz	ci_error
;@@:
	;mov	si, offset BSBuffer.bs_bf_inode_num ; 20/01/2020
	;mov	word ptr [si], ax

	; 22/01/2020
	; write boot (startup) file inode number into boot sector
	
	mov	word ptr [BSBuffer.bs_bf_inode_num], ax

	; 20/01/2020
	mov	byte ptr [bmod], 0FFh ; boot sector modified!

	call	sync
	;jc	ci_error
	; 27/03/2022
	jc	short loc_bootfile_err

	mov	si, offset msg_sf_configuration_set_ok
	;call	PRINT_MSG
;@@:
	;retn
	; 20/09/2019
	jmp	PRINT_MSG
@@:
	; 22/01/2020
	retn 	
cl10:
	cmp	cl, 10
	;jne	short @f
	jne	short @b ; 22/01/2020 
; NOBOOTFILE
loc_cmd_nobootfile:
	cmp	ax, 'on'
	;jne	short @f
	jne	short @b ; 22/01/2020 
	lodsw
	cmp	ax, 'ob'
	;jne	short @f
	jne	short @b ; 22/01/2020 
	lodsw
	cmp	ax, 'to'
	;jne	short @f
	jne	short @b ; 22/01/2020 
	lodsw
	cmp	ax, 'if'
	;jne	short @f
	jne	short @b ; 22/01/2020 
	lodsw	
	cmp	ax, 'el'	
	;jne	short @f
	jne	short @b ; 22/01/2020 
	lodsb
	or	al, al
	;jnz	short @f
	jnz	short @b ; 22/01/2020 

	mov	si, offset BSBuffer.bs_bf_inode_num ; 20/01/2020

	; 22/01/2020
	mov	ax, word ptr [si]	

	and	ax, ax
	;jz	ci_no_bootfile
	; 27/03/2022
	jnz	short @f
	jmp	ci_no_bootfile
@@:
	; 22/01/2020
	; clear boot (startup) file inode number in boot sector
	
	; 22/01/2020
	mov	byte ptr [bmod], 0FFh ; boot sector modified!

	xor	ax, ax
	mov	word ptr [si], ax ; 0
	call	sync
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
	jmp	ci_error
@@:
	mov	si, msg_sf_configuration_reset_ok

	; 22/01/2020
	;call	PRINT_MSG
;@@:
	;retn
	; 22/01/2020
	jmp	PRINT_MSG	

command_interpreter endp

update_cdir_string proc near
	; 13/01/2013 bugfix
	; 10/12/2012
	; 09/12/2012
	; input -> SI= chdir argument
ucds_0:
	mov	bx, offset unix_cdir
	inc	bx ; 13/01/2013
	mov	di, bx
	lodsb
	cmp	al, '/'
	jne	short @f
	xor	dx, dx
	mov	word ptr [CDirOffset], dx
	jmp	short ucds_6
@@:
	mov	dx, word ptr [CDirOffset]
	; 13/01/2013
	or	dx, dx
	jz	short @f
	add	di, dx
	mov	byte ptr [DI], '/'
	inc	di
	;
	jmp	short @f
ucds_8:
	inc	di
ucds_6:
	lodsb
	cmp	al, '/'
	je	short ucds_6
@@:
	or	al, al
	jz	short ucds_5
	cmp	al, '.'
	jne	short ucds_3
	lodsb
	cmp	al, '.'
	je	short ucds_2 ; dotdot
ucds_1: ;dot
	cmp	al, '/'
	je	short ucds_6
	or	al, al
	jz	short ucds_5
	mov	ah, '.'
	xchg	ah, al
	stosw
	jmp	short ucds_6
ucds_2: ; dotdot
	cmp	di, bx
	ja	short @f
	xor	dx, dx
	mov	byte ptr [DI], dl ; 0
	jmp	short ucds_7
@@: ; 13/01/2013
	dec	di
@@: ; move back
	dec	di ; 13/01/2013
	mov	al, byte ptr [DI]
        cmp	al, '/'
	jne	short @b ; 13/01/2013
	jmp	short ucds_8
ucds_4:
	stosb
	jmp	short ucds_6
ucds_3:
	stosb
	lodsb
	cmp	al, '/'
	je	short ucds_4
	and	al, al
	jnz	short ucds_3
ucds_5: ; 13/01/2013
	cmp	di, bx
	jna	short ucds_9
	dec	di
	cmp	byte ptr [DI], '/'
	je	short ucds_9
	inc	di
ucds_9:
	; 13/01/2013
	mov	byte ptr [DI], al ; 0
	mov	dx, di
	sub	dx, bx
ucds_7:
	mov	word ptr [CDirOffset], dx
	
	retn

update_cdir_string  endp

print_directory_list proc near
	; 27/03/2022
	; 22/01/2020
	; 20/01/2020
	; 25/09/2019
	; 24/09/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 04/12/2015 (14 byte file names)
	; 23/02/2013 long list printing (list_count)
	; 03/02/2013
	; 22/01/2013 ls -l command feature 
	; 21/01/2013 dir/ls options
	; 20/01/2013 directory sign ("/")
	; 30/12/2012

	or	ax, ax ; i-number of directory	
	jnz	short @f
	
	; 09/12/2012
pdl_0:	
	mov	ax, word ptr [u_cdir]
@@:
	call	iget
        jc	short @f ; 20/01/2013 ; jc short pdl_9

	;test	word ptr [inode_flgs], 4000h ; directory i-node ?
	;jnz	short pdl_2

	; 20/09/2019
	mov	ah, byte ptr [inode_flgs+1]
	and	ah, 0C0h ; regular + directory
	cmp	ah, 0C0h ; 24/09/2019
	je	short pdl_2	
pdl_1:
	mov	ah, 0FFh ; error number
	stc
@@:	; 20/01/2013
       ;jmp	short pdl_9
        retn
pdl_2:
	;mov	ax, word ptr [inode_size]
	;mov	word ptr [u_dirp], ax ; put size of directory in u.dirp
	
	xor	ax, ax 
	mov	word ptr [u_off], ax ; u.off is file offset used by user
	; 20/01/2020
	mov	word ptr [u_off+2], ax

	;mov	word ptr [u_fofp], offset u.off
		      ; u.fofp is a pointer to the offset portion 
		      ; of fsp entry
	mov	byte ptr [list_count], al ; 0 ; 23/02/2013 
pdl_3:
	mov	word ptr [u_base], offset u_dirbuf
		 ; u.dirbuf holds a file name copied from
		 ; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16)
		 ; u.dirbuff holds a file name copied from
		 ; a directory	
	mov	ax, word ptr [ii]
		
	call	readi	; read 16 bytes of file with i-number (R1)
		; i.e. read a directory entry
        jc	short @b ; jc short pdl_9
	
	mov	cx, word ptr [u_nread]
	or	cx, cx
	jna	short pdl_1 ; gives error return 
	
	mov	bx, word ptr [u_dirbuf]
	and	bx, bx       
	;jz	pdl_8
	; 27/03/2022
	jnz	short pdl_4
	jmp	pdl_8
pdl_4:
	mov	si, offset u_dirbuf + 2 ; r3, points to file name of directory entry
	mov	cx, 14 ; max. file name length (04/12/2015) 8 -> 14
	mov	di, offset DirFileName + 1 ; boot_File_Name
pdl_5:
	lodsb	; mov al, byte ptr [si] ; inc si
 	or	al, al
	jz	short pdl_6 ; If char is null,
		   ; the last char in string has been compared
	stosb  ; mov byte ptr [DI], al, inc di 
	loop	pdl_5
pdl_6: 
	; 21/01/2013
	mov	si, offset UNIX_CRLF
	call	PRINT_MSG
	cmp	byte ptr [ls_option], 1
	je	short pdl_7
	;mov	al, 0
	mov	byte ptr [di], al
	jb	short pdl_13
pdl_7:
	; 20/01/2013
	push	di
	mov	ax, word ptr [ii]
	mov	word ptr [pdir], ax
	mov	ax, word ptr [u_dirbuf]
	call	iget
	pop	di
	;jc	pdl_9
	; 27/03/2022
	jnc	short pdl_17
	jmp	pdl_9
pdl_17:
	; 22/01/2012
	cmp	byte ptr [ls_option], 1
	jna	short @f
        
pdl_11: ; 21/01/2013 ; Inode number
	mov	ax, word ptr [u_dirbuf]
	mov	cx, 3 ; 03/02/2013
	call	print_decimal_number
        jmp	short pdl_10
@@:
	;mov	ax, word ptr [inode_flgs]
	;test	ah, 40h ; 'directory' flag
        ;jz	short pdl_10

	; 20/09/2019
	mov	al, byte ptr [inode_flgs+1]
	and	al, 0C0h ; regular + directory
	cmp	al, 80h  ; regular (not directory, not device)
	;je	short pdl_10
	; 25/09/2019
	jna	short pdl_10 ; not directory (file or device)	

	mov	si, offset u_dirbuf + 2
	lodsb
@@:
	cmp	al, '.'
	jne	short @f
	lodsb
	or	al, al
	;jz	short pdl_10
	jz	short pdl_14 ; 25/09/2019
	jmp	short @b
@@:	
	mov	al, '/'
	mov	byte ptr [di], al
	inc	di
pdl_10:
	; 21/02/2013
	xor	al, al
pdl_14:
	mov	byte ptr [di], al
pdl_13: ; File/Directory name
	inc	byte ptr [list_count] ; 23/02/2013
	mov	si, offset DirFileName
	call	PRINT_MSG

	; 22/01/2013
	cmp	byte ptr [ls_option], 1
	;je	pdl_12  ; 03/02/2013 short -> near
	;jb	pdl_8 ; 23/02/2013 
	; 27/03/2022
	ja	short @f	
	jb	short pdl_18
	jmp	pdl_12

	; 03/02/2013
@@:	; Owner (uid)
        ;xor	bh, bh ; mov bh, 0
        mov	ah, 03h ; get cursor position and size.
        int	10h
@@:
	; 20/01/2020
	cmp	dl, 19 ; 04/12/2015 (13 -> 19)
	jnb	short @f
	mov	al, 20h
	call	putc
	inc	dl ; 22/01/2020
	jmp	short @b
	
pdl_18:	; 27/03/2022
	jmp	pdl_8
@@:
	; 24/09/2019
	mov	ax, word ptr [inode_uid]
	;mov	cx, 4 ; <= 9999 ?
	mov	cx, 3 ; <= 999 ? ; 25/09/20219
	call	print_decimal_number
	;mov	al,','
	mov	al,20h ; 25/09/2019 
	call	putc
	xor	ah, ah
	mov	al, byte ptr [inode_gid]
	mov	cx, 3
	call	print_decimal_number	
@@:
  	mov	al, 20h
	call	putc
	
  	mov	al, 20h
	call	putc

@@:	; Flags/Attributes

	; 25/09/2019
	mov	dx, word ptr [inode_flgs]
	test	dh, 80h
	jnz	short regular
	test	dh, 40h
	jnz	short blockdev
	; 20/01/2020
	test	dh, 20h
	jz	short regular_f
chardev:
	mov	al, 'c'
	jmp	short @f
blockdev:	
	mov	al, 'b'
	jmp	short @f
regular:
	mov	al,'d'
	test	dh, 40h
	jnz	short @f	
regular_f:		
	mov	al, '-'
@@:
	call	putc

	; rwx permissions for owner,group,others

	and	dx, 1FFh ; 111111111b
	
	;mov	cx, 7
	mov	cl, 7
	shl	dx, cl ; shift bit 8 to bit 15 position
	mov	cl, 3
rwxugo:
	mov	al, '-'
	shl	dx, 1
	jnc	short @f
	mov	al, 'r'
@@:
	call	putc
	mov	al, '-'
	shl	dx, 1
	jnc	short @f
	mov	al, 'w'
@@:	
	call	putc
	mov	al, '-'
	shl	dx, 1
	jnc	short @f
	mov	al, 'x'
@@:
	call	putc
	loop	rwxugo

	mov	al, 20h
	call	putc

@@: ; File Size ; 03/02/2013
	;mov	ax, word ptr [inode_size]
	;;;mov	cx, 5
	;;mov	cl, 5
	;mov	cl, 6 ; 25/09/2019
	;call	print_decimal_number

	; 20/01/2020
	call	print_file_size	
@@:
	mov	al, 20h
	call	putc

	mov	al, 20h
	call	putc

@@: ; 03/02/2013 ; File creation date & time	
	;mov	ax, word ptr [inode_ctim]
	;mov	dx, word ptr [inode_ctim]+2
	
	; 23/02/2013 ; File last modification date & time	
	mov	ax, word ptr [inode_mtim]
	mov	dx, word ptr [inode_mtim]+2
	
	call	convert_from_epoch
	; cx = day

	mov	ax, cx ; word ptr [day]
	mov	si, offset dec_num
	mov	bx, si
	add	bx, 2	
	;mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	byte ptr [BX], '/'
	mov	si, bx
	inc	si
	mov	ax, word ptr [month]
	;mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	add	bx, 3
	mov	byte ptr [BX], '/'
	mov	si, bx
	inc	si
	mov	ax, word ptr [year]
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal

	; 20/01/2020
	mov	byte ptr [si], 0

	mov	si, offset dec_num
	call	PRINT_MSG

	mov	al, 20h
	call	putc

	mov	si, offset dec_num
	mov	bx, si
	mov	ax, word ptr [hour]
	;mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	add	bx, 2
	mov	byte ptr [bx],':'

	mov	si, bx
	inc	si
	mov	ax, word ptr [minute]
	;mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	add	bx, 3
	;mov	byte ptr [bx], ':'
	;mov	si, bx
	;inc	si
	;mov	ax, word ptr [second]
	;;mov	cx, 2
	;mov	cl, 2
	;call	proc_bin_to_decimal
	xor	al, al
        mov	byte ptr [bx], al 

	mov	si, offset dec_num
	call	PRINT_MSG
pdl_12:
	mov	ax, word ptr [pdir]
	call	iget
        jc	short pdl_9
pdl_8:
	; 20/01/2020 (32 bit offset)
	mov	ax, word ptr [u_off]
	mov	dx, word ptr [u_off+2]
	cmp	dx, word ptr [inode_size+2]
        jb	short pdl_15
	cmp	ax, word ptr [inode_size]
        jnb	short @f

	; 30/12/2012
	;mov	ax, word ptr [u_off]
	;cmp	ax, word ptr [inode_size]
        ;jnb	short @f ; 22/02/2013 ; jb pdl_3
pdl_15:
	; 23/02/2013
	cmp	byte ptr [list_count], 21
	;jb	pdl_3
	; 01/01/2020
	jnb	short pdl_16
	jmp	pdl_3	
pdl_16:
	xor	ah, ah
	mov	byte ptr [list_count], ah ; 0
	int	16h
	cmp	al, 1Bh  ; ESC key
	;jne	pdl_3
	; 01/01/2020
	je	short @f
	jmp	pdl_3	
@@:
        mov	si, offset UNIX_CRLF
	call	PRINT_MSG
pdl_9:
	retn

putc:  ; 22/01/2013
	mov	ah, 0Eh
	;mov	bx, 07h
	int	10h
	xor	al, al

	retn

print_directory_list endp

sys_chdir proc near
	; 24/09/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 09/12/2012 unixcopy.asm
	;       Retro UNIX v1 FS file import/export version
	;             of syschdir function  
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;
	; RETRO UNIX v1 FS
	; syschdir:
	; makes the directory specified in the argument 
	; the current directory

	; mov word ptr [u_namep], si

syschdir_0:
	call	namei
	jc	short syschdir_5

syschdir_1:
	call	iget
	jc	short syschdir_5
syschdir_2:
	;test	word ptr [inode_flgs], 4000h ; directory i-node ?
	;jnz	short syschdir_4
	; 20/09/2019
	mov	dl, byte ptr [inode_flgs+1]
	and	dl, 0C0h
	cmp	dl, 0C0h ; 24/09/2019
	je	short syschdir_4
syschdir_3:
	mov	ah, 0FFh
	stc
	retn	
syschdir_4:	
	mov	word ptr [u_cdir], ax
	;mov	dx, word ptr [cdev]
	;mov	word ptr [u_cdev], dx

syschdir_5:
	retn

sys_chdir endp

sys_mkdir proc near
	; 15/04/2022 ('number of links' bugfix)
	; 28/03/2022
	; 04/01/2020
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	;
	; 05/01/2013 (bugfix)
	; 30/12/2012 unixcopy.asm
	;       Retro UNIX v1 FS file import/export version
	;             of sysmkdir function  
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;
	; RETRO UNIX v1 FS
	; sysmkdir:
	; make a directory
	;
	;
	; return => if cf=1 error code in AH
	; If cf=0 -> AX = I-Number (also in u.dirbuff)

 	;jsr    r0,arg2 / point u.namep to the file name
        ;jsr    r0,namei / get the i-number
        ;       br .+4 / if file not found branch around error
        ;br     error2 / directory already exists (error)
        ;tstb   u.uid / is user the super user
        ;bne    error2 / no, not allowed
        ;mov    (sp)+,r1 / put the mode in r1
	;bic    $!317,r1 / all but su and ex
        ;bis    $40000,r1 / directory flag
        ;jsr    r0,maknod / make the i-node for the directory
        ;br     sysret2 /

	mov	bx, word ptr [u_namep]
	mov	si, bx ; 05/01/2013

makdir_1:
	lodsb  
	or	al, al
	jz	short makdir_2
	cmp	al, '/'
	jne	short makdir_1
	mov	bx, si		
	jmp	short makdir_1

makdir_2:
	cmp	bx, word ptr [u_namep]
	je	short makdir_3
	dec	si
	dec	si ; 05/01/2013
        cmp	byte ptr [si], '/' ; is the last char '/'
	jne	short makdir_3

	cmp	si, word ptr [u_namep] ; 05/01/2013
	je	short makdir_3

	stc
@@:
	retn

makdir_3:
	mov	word ptr [pdir], bx
sysmkdir_0:
	call	namei
	jc	short sysmkdir_1

	stc
@@:
	retn

sysmkdir_1:
	cmp	ah, 0FFh
        jne	short @b

makdir_4:
	; 15/04/2022
        ;mov	ax, word ptr [ii]
	mov	bx, word ptr [pdir]
	;mov	word ptr [pdir], ax
	cmp	word ptr [u_namep], bx
	jb	short @b ; parent dir of the new sub dir not found

 	; 15/04/2022
	mov	ax, word ptr [ii]
	mov	word ptr [pdir], ax ; parent directory 
				    ; (of the new sub dir)
sysmkdir_flags: ; ax = r1 = mode
	;mov	ax, 0C00Eh ; Flags (1100000000001110b)
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure) 
	mov	ax, 0C1EDh ; Flags (0100000111101101b)
sysmkdir_maknod:
	;call	mak_nod	
	; ax = I-Number (also in u.dirbuff)

	;retn

	; 28/03/2022
	; (clear/reset [uf_i_number] as new file/directory sign)
	mov	word ptr [uf_i_number], 0

	; 04/01/2020
	;jmp	short mak_nod ; 20/01/2020 (short jump)

sys_mkdir endp

mak_nod proc near
	; 28/03/2022
	; 27/03/2022 (BugFix)
	; 20/01/2020
	; 25/09/2019
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 01/03/2013
	; 23/02/2013
	; 15/12/2012 UNIXCOPY.ASM version of maknod
	; 02/12/2012 (maknod_imap -> call imap)
	; 25/11/2012
	; 18/11/2012
	; 11/11/2012
	; unixboot.asm (boot file configuration)
	; version of 'maknod'
	;
	; 30/10/2012
	; AX = R1, mode
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	; RETRO UNIX v1 FS
	;
	; maknod : create an i-node and make a directory entry
	;
	; 8086 CPU & IBM PC architecture modifications by Erdogan Tan 
	;
	; return => if cf=1 error code in AH
	; If cf=0 -> AX = I-Number (also in u.dirbuff)

	;or 	ah, 80h  ; 10000000b, allocate flag set  ;; unix v1
	; 25/09/2019
	;or	ah, 80h	; 11000000b, regular file (or dir) ;; runix v2	 
	
	push 	ax ; *	; put mode on stack
	
	;mov	ax, word ptr [ii] ; move current i-number to AX/r1
	;push 	ax
	push 	word ptr [ii] ; **

	;mov	ax, 41	; r1 = 41 ; unix v1
	mov	ax, 1	; check inodes after root directory, 19/09/2019
		; (root directory inode number = 1) ; runix v2
maknod_1: ; 1 	; scan for a free i-node
	inc	ax 	; r1 = r1 + 1
	; 2/12/2012
	call	imap	; get byte address and bit position in inode map in 
		; r2 (DX) & mq (BX)
            ; DX (MQ) has a 1 in the calculated bit position
            ; BX (R2) has byte address of the byte with allocation bit

        test    byte ptr [bx], dl ; bitb mq,(r2) / is the i-node active
	jnz	short maknod_1    ; bne	1b / yes, try the next one
        or      byte ptr [bx], dl ; bisb mq,(r2)
		; no, make it active (put a 1 in the bit map)
	; ax = i-number
	call	iget      	 ; jsr	r0,iget / get i-node into core
	;jc	short maknod_3	
	; 20/01/2020
	jnc	short @f

maknod_3:
	; 15/12/2012
	pop 	ax ; ** ; current i number
maknod_2:
	pop	ax ; *  ; file mode (inode flags)
	retn
@@:	
	;test	word ptr [inode_flgs], 8000h  ; is i-node already allocated
	test	byte ptr [inode_flgs+1], 0 ; 19/09/2019 - Retro UNIX 386 v2
	jnz	short maknod_1	; 1b / yes, look for another one
	
	mov	word ptr [u_dirbuf], ax ; mov r1, u.dirbuf 
				; no, put i-number in u.dirbuf

	pop 	ax ; ** ; 15/12/2012	; get current i-number back	
	call 	iget	  ; jsr r0,iget / get i-node in core
	jc	short maknod_2

	call 	mkdir	; jsr r0,mkdir 
		; make a directory entry in current directory
	jc	short maknod_2 ; 01/03/2013 

	mov	ax, word ptr [u_dirbuf] ; mov u.dirbuf,r1 
		;  ax / r1 = new inode number
	call	iget
	jc	short maknod_2

        ; jsr r0,copyz; inode; inode+32. / 0 it out 
	;mov	cx, 16
	mov	cx, 32 ; 19/09/2019 - Retro UNIX 386 v2
	;	      ;	    (inode size = 64 bytes)
	xor	ax, ax ; 0
	mov	di, offset inode 
	rep	stosw

	pop	word ptr [inode_flgs] ; * ; mov (sp)+,i.flgs / fill flags

	; 19/09/2019 - Retro UNIX v2 (modified unix v7 inodes)
	mov 	cl, byte ptr [u_gid] ; user's group id
	mov 	byte ptr [inode_gid], cl	

	mov 	cl, byte ptr [u_uid] ; movb u.uid,i.uid / user id
	mov 	byte ptr [inode_uid], cl ; 23/02/2013 al -> cl
	mov     byte ptr [inode_nlks], 1 ; movb $1,i.nlks / 1 link
	
	;call	epoch

	;mov 	word ptr [s_time], ax
	;mov 	word ptr [s_time]+2, dx	

	;mov 	word ptr [inode_ctim], ax ; mov s.time,i.ctim / time created
	;mov 	word ptr [inode_ctim]+2, dx ; mov s.time+2,i.ctim+2

	; 25/11/2012
	; 23/02/2013
fromdos_maknod:
	;xor	ax, ax
	xor 	dx, dx	
	; File/Directory data/content modification date&time
	mov 	word ptr [inode_mtim], ax  ; 0
	mov 	word ptr [inode_mtim]+2, dx ; 0
	; 19/09/2019 - Retro UNIX 386 v2
	; Last access date&time or inode modification date&time
	mov 	word ptr [inode_atim], ax  ; 0
	mov 	word ptr [inode_atim]+2, dx ; 0

	;test 	word ptr [inode_flgs], 4000h  ; Directory
	; 19/09/2019
	test	byte ptr [inode_flgs+1], 40h  ; Directory
	jnz 	short maknod_4
	mov 	ax, word ptr [uf_make_datetime]
	mov	dx, word ptr [uf_make_datetime]+2
maknod_4:
	mov 	word ptr [inode_ctim], ax  
	mov 	word ptr [inode_ctim]+2, dx

	call	setimod

	; 28/03/2022
	mov	ax, word ptr [ii] ; inode number of current inode

	; 27/03/2022 (BugFix)
	cmp	word ptr [uf_i_number], 0 ; existing file ?
	ja	short @f  ; yes (do not decrease free inode count)
	; new file (decrease free inode count)

	; 29/09/2019
	dec	word ptr [systm.sb_FreeInodes]

	; 28/03/2022
	; (first free inode has been searched from inode 1;
	;  so, ax contains previous first free inode value and
	;  it is better if the new value is set one/just next to ax)
  
	mov	dx, ax
	inc	dx ; next inode (next ffi search will be started from)
	mov	word ptr [systm.sb_FirstFreeIno], dx
	sub	dx, dx
	mov	word ptr [systm.sb_FirstFreeIno+2], dx ; 0
	retn

@@:		; 27/03/2022
	; 28/03/2022
	;mov	ax, word ptr [ii] ; inode number of current inode
	; 20/01/2020
	;mov 	ax, word ptr [u_dirbuf]
	cmp	ax, word ptr [systm.sb_FirstFreeIno]
	jne	short @f
	;cmp	word ptr [systm.sb_FirstFreeIno+2],0
	;jne	short @f
	mov	word ptr [systm.sb_FirstFreeIno], 0FFFFh ; invalidate
	mov	word ptr [systm.sb_FirstFreeIno+2], 0FFFFh
@@:
	clc
	;mov 	ax, word ptr [u_dirbuf] ; 20/01/2020

	retn

;maknod_3:
;	; 15/12/2012
;	pop 	ax ; **
;maknod_2:
;	pop	ax ; *
;	retn

mak_nod	endp

remove_directory proc near
	; 15/04/2022 ('number of links' bugfix)
	; 24/01/2020
	; 20/01/2020
	; 01/10/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 04/12/2015 (14 byte directory names)
	; 05/01/2013
	;
	; mov word ptr [u.namep], si

	;call	namei
        ;jc	@f

	;cmp	ax, ROOT_DIR_INODE_NUMBER
        ;je	rmdir_stc_retn

	;cmp	ax, word ptr [u_cdir]
        ;je	rmdir_stc_retn

       ; INPUT ->
       ; ax = i_number of directory (to be removed)
       ; u_off = directory entry location + 16 (in parent dir)
       ; [ii] = i_number of parent directory

	cmp	ax, word ptr [ii]	; '.' entry
	je	short rmdir_stc_retn

	mov	word ptr [uf_i_number], ax  ; i_number of dir or file 

	mov	dx, word ptr [u_off]
	; 20/01/2020
	mov	word ptr [FileHandle], dx  ; 16 bit file pointer/offset
	;mov	word ptr [d_off], dx ; directory entry location + 16 (+10) 
	;mov	dx, word ptr [u_off+2]
	;mov	word ptr [d_off+2], dx

	mov	dx, word ptr [ii] ; i-number of parent directory 
	mov	word ptr [pdir], dx

	call	iget
	jc	short @f

	;mov	ax, word ptr [inode_flgs]
	;test	ah, 40h ;'directory' flag
	;jz	short rmdir_stc_retn	
	;test	al, 4h ; 'write' flag
	;jz	short rmdir_stc_retn

	; 20/09/2019 - Retro UNIX 386 v2
	mov	ax, word ptr [inode_flgs]
	; 15/04/2022
	;and	al, 80h ; owner's write permission
	;jz	short rmdir_stc_retn
	;and	ah, 0C0h
	;cmp	ah, 0C0h ; 01/10/2019
	;je	short rmdir_1
;rmdir_stc_retn:
;	stc
;@@:
;	retn

	; 15/04/2022
	and	ah, 0C0h
	cmp	ah, 0C0h
	jne	short @f ; (cf=1)
	and	al, 80h ; owner's write permission
	jnz	short rmdir_1
	
rmdir_stc_retn:
	stc
@@:
	retn

rmdir_1:
	xor	ax, ax 
	mov	word ptr [u_off], ax 
	; 20/01/2020
	mov	word ptr [u_off+2], ax ; hw of 32 bit file pointer/offset

	;mov	word ptr [u_fofp], offset u.off	

rmdir_readi_loop:
	mov	word ptr [u_base], offset u_dirbuf
		 ; u.dirbuf holds a file name copied from
		 ; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 	
 	
	mov	ax, word ptr [ii]
		
	call	readi	; read 16 bytes of file with i-number
			; i.e. read a directory entry
	jc	short @b ; 24/09/2019

	mov	cx, word ptr [u_nread]

	or	cx, cx
	;;jnz	short rmdir_stc_retn
	;jnz	short @b
	jnz	short rmdir_unlink ; 01/10/2019	

	;cmp	cx, 16  ; 04/12/2015 (10 -> 16)
	;jb	short @b

	mov	bx, word ptr [u_dirbuf]
	and	bx, bx       
	jz	short rmdir_readi_loop

	mov	ax, word ptr [u_dirbuf]+2
	cmp	al, '.'
	jne	short rmdir_stc_retn

	and	ah, ah
	jz	short rmdir_readi_loop

	cmp	ah, '.'	; ".."
	jne	short rmdir_stc_retn

	mov	ah, byte ptr [u_dirbuf]+4

	or	ah, ah
	jnz	short rmdir_stc_retn

	; 20/01/2020 (32 bit offset value)
	;mov	ax, word ptr [u_off+2]
	;cmp	ax, word ptr [inode_size+2]
	;jb	short rmdir_readi_loop
	
	mov	ax, word ptr [u_off]
	; 01/10/2019 - Retro UNIX 386 v2 (the 1st entry is '.', not '..')
		; 04/12/2015 (10 -> 16)
	;cmp	ax, 16	; protection for removing default system directories
	;jna	short rmdir_stc_retn ; because, the 1st dir enty of them is ".."

	cmp	ax, word ptr [inode_size]
	jb	short rmdir_readi_loop

	; 01/10/2019
	;cmp	word ptr [inode_size+2], 0
	;jna	short rmdir_unlink
	;cmp	ax, 65536-16
	;jb	short rmdir_readi_loop

rmdir_unlink:
	; 15/04/2022
	; unlink dot ('.')
	;mov	ax, word ptr [uf_i_number]
	;call	iget
	dec	byte ptr [inode_nlks]
	call	setimod

	mov	ax, word ptr [uf_i_number]
	; 20/01/2020
	mov	dx, word ptr [FileHandle] ; 16 bit file pointer/offset
	;mov	dx, word ptr [d_off+2]
	;mov	word ptr [u_off+2], dx
	;mov	dx, word ptr [d_off]
	; 24/01/2020
	mov	word ptr [u_off], dx

	; 15/04/2022
;	; 20/09/2019
;	;call	unlink
;;@@:
;	;retn
;	; 20/01/2020
;	;jmp	short unlink

;rmdir_stc_retn:
;	stc
;	retn

	; 15/04/2022
	; unlink dotdot ('..')
	call	unlink
	; decrease link count of the parent directory
	mov	ax, word ptr [pdir] ; parent dir
	call	iget
	;cmp	byte ptr [inode_nlks], 2
	;jna	short @f
	dec	byte ptr [inode_nlks] ; must be >= 2 after this
	;call	setimod
	jmp	setimod
;@:
;	;retn

remove_directory endp

unlink	proc near
	; 20/01/2020
	; 18/09/2019 - Retro UNIX 386 v2 (modified unix v7 inodes)
	;
	; 04/12/2015 (14 byte file names)
	; 05/01/2013 UNIXCOPY.ASM modification (pdir -> iget)
	; 16/12/2012 UNIXCOPY.ASM version
        ; 02/12/2012
        ; unix boot file configuration version
	; of "sysunlink" function of retro unix v1.0 by Erdogan Tan
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972 
	; ('sysunlink', unix kernel function)
	; 
	; INPUT -> AX (R1) = inode number
	;          [u_off] = Directory Entry Offset + 16 (+ 10)
	;          ;;; [ii] = i-number of current directory
	; Return -> CF = 0 -> Successed, CF = 1 -> failed
	;		(error code in AX)

	;jsr r0,arg; u.namep / u.namep points to name
	;jsr r0,namei / find the i-number associated 
	;	with the path name
	;br error9 / not found

	push	ax ;mov r1,-(sp) / put its i-number on the stack
	;jsr r0,isdir / is it a directory
	xor	ax, ax
	mov	word ptr [u_dirbuf], ax ; clr u.dirbuf / no, clear 
		;the location that will get written
		 ;/ into the i-number portion of the entry
	sub	word ptr [u_off], 16 ; 04/12/2015 (10 -> 16)
		; sub $10.,u.off 
		; / move u.off back 1 directory entry
	; 20/01/2020
	sbb	word ptr [u_off+2], ax ; 0

	;mov	ax, word ptr [ii] 
	mov	ax, word ptr [pdir] ; 05/01/2013
	call	iget
	jnc	short @f
	pop	ax
	retn
	;
@@:
	call	wdir ;jsr r0,wdir / free the directory entry
	pop	ax  ;mov (sp)+,r1 / get i-number back
	jc	short @f

        call	iget ; jsr r0,iget / get i-node
	jc	short @f
        call	setimod ; jsr r0,setimod / set modified flag
	;dec	word ptr [inode_nlks] ; Retro UNIX 386 v2 (word)
        dec	byte ptr [inode_nlks] ; decb i.nlks 
				; / decrement the number of links
	jnz	short @f ;bgt sysret9 
		;/ if this was not the last link to file return

	; 19/09/2019

;	call	anyi ;jsr r0,anyi / if it was, see if anyone has it open.
;	    	;Then / free contents of file and destroy it.
;		;br sysret9
;@@:
;	retn

unlink	endp

anyi	proc near
	; 28/03/2022
	; 20/01/2020
	; 18/09/2019 - Retro UNIX 386 v2 (modified unix v7 inodes)
	;
        ; 02/12/2012
        ; unix boot file configuration version
	; of "anyi" procedure of retro unix v1.0 by Erdogan Tan
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972 
	; ('anyi' procedure)
	; 
	; INPUT -> AX (R1) = inode number
	; Return -> CF = 0 -> Successed, CF = 1 -> failed
	;		
	; mov $fsp,r2 / move start of fsp table to r2
anyi_1: ;1
	; cmp r1,(r2) / do i-numbers match?
	; beq 1f / yes, 1f
	; neg r1 / no complement r1
	; cmp r1,(r2) / do they match now?
	; beq 1f / yes, transfer
	; / i-numbers do not match
	; add $8,r2 / no, bump to next entry in fsp table
	; cmp r2,$fsp+[nfiles*8] / are we at last entry in the table
	; blt 1b / no, check next entries i-number
	; tst r1 / yes, no match
	; bge .+4
	; neg r1 / make i-number positive

	; 20/01/2020
	;mov	ax, word ptr [ii]

	call	imap	; jsr r0,imap / get address of allocation bit 
	    		;     in the i-map in r2
        ; DX (MQ) has a 1 in the calculated bit position
        ; BX (R2) has byte address of the byte with allocation bit
	push	bx ; retro unix modification (not as original unix code)
	push	dx ; retro unix modification (not as original unix code)
	; AX = i-number
	call	itrunc ; jsr r0,itrunc / free all blocks related to i-node
	
	pop	dx ; retro unix modification (not as original unix code)
	pop	bx ; retro unix modification (not as original unix code)

	jc	short @f

        ; (AX=0)
	; retro unix modification-> 'call itrunc' moved up for
	; keeping superblock unmodified if itrunc return with an error

        not	dx
        and	byte ptr [bx],dl ; bicb mq,(r2) 
		     ; / clear bit for i-node in the imap
	; 28/03/2022
	xor	dx, dx
	; 29/09/2019
	inc	word ptr [systm.sb_FreeInodes]
	mov	ax, word ptr [ii] ; inode number of current inode
	cmp	ax, word ptr [systm.sb_FirstFreeIno]
	jnb	short anyi_2
	;cmp	word ptr [systm.sb_FirstFreeIno+2],0
	;jne	short anyi_2
	mov	word ptr [systm.sb_FirstFreeIno],ax
	; 20/01/2020
	;mov	word ptr [systm.sb_FirstFreeIno+2],0 ; 0FFFFh -> 0
	;;clc
	; 28/03/2022
	mov	word ptr [systm.sb_FirstFreeIno+2], dx ; 0
	clc
anyi_2:
	; 28/03/2022
	; cf = 0
	;xor	ax,ax
	;mov	word ptr [inode_flgs], ax ; 0 ; clr i.flgs
			; / clear all flags in the i-node
	; 28/03/2022
	mov	word ptr [inode_flgs], dx ; 0
@@:
	retn	; rts r0 / return

;anyi_2: ;1 / i-numbers match
 	;incb	7(r2) / increment upper byte of the 4th word
	;rts	r0 / in that fsp entry (deleted flag of fsp entry)

anyi	endp

show_file proc near
	; 20/01/2020
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	; 07/01/2013
	; 06/01/2013
	; derived from TRDOS command interpreter file (CMDINTR.ASM)
	; 'show' procedure (13/09/2011)

	call	namei
        jc	short suf_4

        call	iget
	jc	short suf_4

	; 20/09/2019
	;;test	word ptr [inode_flgs], 4000h  ; Directory
	;test	byte ptr [inode_flgs+1], 40h
	;jnz	short suf_4

	; 20/09/2019
	;test	byte ptr [inode_flgs+1], 80h ; Regular file
	;jz	short suf_4		

	mov	al, byte ptr [inode_flgs+1]
	and	al, 0C0h
	cmp	al, 80h
	jne	short suf_4	

        mov	si, offset UNIX_CRLF
        call	PRINT_MSG

	; 20/01/2020
	mov	ax, word ptr [inode_size+2]
	mov	dx, 512
	or	ax, ax
	jnz	short suf_0
	mov	ax, word ptr [inode_size]
	cmp	ax, dx ; 512
	jna	short suf_1
suf_0:
	mov	ax, dx ; 512
suf_1:
	xor	dx, dx 
        mov	word ptr [u_off], dx
	; 20/01/2020
	mov	word ptr [u_off+2], dx 

	mov	cx, 22
suf_2:	
	push	cx	
	mov	word ptr [u_count], ax
	mov	word ptr [u_base], offset ReadBuffer
	mov	ax, word ptr [ii] ; word ptr [u_dirbuf]
	call	readi
	pop	cx
	jc	short suf_4

	mov	di, word ptr [u_nread]

	or	di, di
	jz	short suf_4

	mov	si, offset ReadBuffer

        jmp	short suf_6
suf_3:
        and	cx, cx
        jnz	short suf_6
	xor	ah, ah
	int	16h
	cmp	al, 1Bh ; ESCAPE Key
	jne	short suf_5
suf_4:
	mov	si, offset UNIX_CRLF
        ;call	PRINT_MSG
	;retn
	; 20/01/2020
	jmp	PRINT_MSG
suf_5:
	mov	cx, 20
suf_6:
        xor	bh, bh ; mov bh, 0
        mov	bl, 7

	lodsb
	cmp	al, 0Dh ; ENTER/RETURN Char
	jne	short suf_7
	dec	cx
	jmp	short suf_8	
suf_7:
	cmp	al, 09h ; TAB Space Char
        je	short suf_10
suf_8:
        mov	ah, 0Eh
        ;xor	bh, bh ; mov bh, 0
        ;mov	bl, 7
	int	10h
suf_9:
	dec	di
	jnz	short suf_3

	;mov	ax, word ptr [u_nread]

	; 20/01/2020
        mov	ax, word ptr [inode_size]
	mov	dx, word ptr [inode_size+2] 
	sub	ax, word ptr [u_off]
	sbb	dx, word ptr [u_off+2] 
	jnz	short suf_14 
suf_13:
	cmp	ax, 512
	jna	short suf_2
suf_14:
	mov	ax, 512
	jmp	short suf_2
suf_10:
        push	cx
        ;xor	bh, bh ; mov bh, 0
        mov	ah, 03h ; get cursor position and size.
        int	10h
	mov	al, dl
	mov	cx, 8
;suf_11a:
;	cmp	al, cl
;	jb	short suf_11b
;	sub	al, cl
;	jmp	short suf_11a
;suf_11b:
;	sub	cl, al
suf_11:
	; 07/01/2013
	xor	ah, ah
	div	cl
	sub	cl, ah	
	;
        mov	al, 20h
        mov	ah, 0Eh 
        ;mov	bl, 7 ; char color attribute	
suf_12:
	int	10h
        loop	suf_12
        pop	cx
	jmp	short suf_9 

show_file endp

namei	proc near
	; 20/01/2020
	; 02/10/2019
	; 24/09/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	; 04/12/2015 (14 byte file names)
	; 05/01/2013
	; 09/12/2012 unixcopy.asm
	;       Retro UNIX v1 FS file import/export version
	; 31/10/2012
	; 14/10/2012
     	; 07/10/2012
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;
	; RETRO UNIX v1 FS
	;
	; return i-number of file (in AX)
	;
	; input:
	; u_namep = pointer to file path name
	; u_cdir = i-number of users directory
	; ;;u_cdev = device number
	; output:
	; cf= 0 -> no error, i-number in AX (R1)
	; cf= 1 -> error code in AX
	;	

	mov	si, word ptr [u_namep]
namei_x: ; 02/10/2019 {si = [u_namep]}
	cmp	byte ptr [si], '/' ; is first char in file name a /
	jne	short @f
	mov	ax, ROOT_DIR_INODE_NUMBER ; = 41 ( = 1 for runix v2)
	; Put i-number of root directory in R1
	;xor	dx, dx
	inc	si  ; go to next char
	mov	word ptr [u_namep], si
	jmp	short namei_0
@@:
	;mov	dx, word ptr [u_cdev]
	mov	ax, word ptr [u_cdir] 
	; put i-number of current directory in R1
namei_0:
	;mov	word ptr [cdev], dx 
	; device file for users directory into cdev
; 1
	cmp	byte ptr [si], 0 ; is the character in file name a nul
        jna	short namei_7 ;nig

namei_1: ; 1
	; get i-node with i-number r1
	call	iget
	jc	short namei_7

	;test	word ptr [inode_flgs], 4000h ; directory i-node ?
        ;jz	short namei_6 ; got an error
	test	byte ptr [inode_flgs+1], 40h ; 24/09/2019
        jnz	short @f
;nib:
namei_6:
	mov	ah, 0FFh ; Error code
	stc
;nig:
namei_7:
	retn
@@:
	mov	ax, word ptr [inode_size]
	mov	word ptr [u_dirp], ax ; put size of directory in u.dirp
	
	xor	ax, ax 
	mov	word ptr [u_off], ax ; u.off is file offset used by user
	; 20/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019

	;mov	word ptr [u_fofp], offset u.off
		; u.fofp is a pointer to the offset portion 
		; of fsp entry
namei_2: ; 2
	mov	word ptr [u_base], offset u_dirbuf
		; u.dirbuf holds a file name copied from
		; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 
	
	mov	ax, word ptr [ii]
		
	call	readi	; read 16 bytes of file with i-number (R1)
		; i.e. read a directory entry
	jc	short namei_7

	mov	cx, word ptr [u_nread]

	or	cx, cx
	jna	short namei_6	; nib ; gives error return 
	
	mov	bx, word ptr [u_dirbuf]
	and	bx, bx       
	jnz	short namei_3	; 3f. branch when active directory entry
		; (i-node word in entry non zero)	
	mov	ax, word ptr [u_off]
	sub	ax, 16   ; 04/12/2015 (10 -> 16)
	mov	word ptr [u_dirp], ax
	jmp	short namei_2 ; 2b

namei_3: ; 3
	mov	si, word ptr [u_namep] ; r2, u.namep points into a file name string
	mov	di, offset u_dirbuf + 2 ; r3, points to file name of directory entry
	mov	dx, offset u_dirbuf + 16 ; 04/12/2015 (10 -> 16)
@@:	; 3
	lodsb  ; mov al, byte ptr [SI], inc si   (al = r4)
 	or	al, al
	jz	short namei_4 ; 3f. If char is nul, then the last char in string has
		; been compared
	cmp	al, "/"	; is char a "/"
	je	short namei_4 ; 3f
	cmp	di, dx	; offset u_dirbuf + 16 ; 04/12/2015 (10 -> 16) ; r3, 
		; have i checked all 14 bytes of file name
	je	short @b ; 3b
	scasb		; cmpb (r3)+, r4   (DI=R3, AL=R4)
		; compare char in u.namep string to file name char
		; read from
	je	short @b ; directory; brach if chars match
	
	jmp	short namei_2 ; 2b
		; File names do not match, go to next directory entry 	
namei_4: ; 3
	cmp	di, dx	; offset u_dirbuf + 16 ; 04/12/2015 (10 -> 16) ; r3, 
		; if equal all 14 bytes were matched
	je	short namei_5 ; 3f

	mov	ah, byte ptr [di]
	;inc	di  ; 05/01/2013
	and	ah, ah  ; tstb (r3)+, bne 2b
	jnz	short namei_2 ; 2b

namei_5: ; 3
	mov	word ptr [u_namep], si ; r2
		; u.namep points to char following a "/" or nul
	;mov	bx, word ptr [u_dirbuf] ; r1
	
	and	al, al	; r4. If r4=0 the end of file name reached,
		; if r4="/" then go to next directory
	mov	ax, bx

        jnz	namei_1 ; 1b

	retn

namei	endp

readi	proc near
	; 20/01/2020 (32 bit file pointer/offset)
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	; 01/03/2013
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
	; 20/01/2020
	; INPUT:
	;	ax = inode number
	;	word [u_count] = count
	;	[u_fofp] = u_off
	;	dword ptr [u_off] = file pointer
	;	dword [inode_size] = file size
	;	
	; OUTPUT:
	;	ax = inode number
	;	cf = 0 -> reading ok, [Error] = 0
	;	cf = 1 -> error, error code in [Error] 
	; 	
	; Modified registers: bx,dx,cx,si,di

	xor	dx,dx ; 0
	mov	word ptr [u_nread],dx ; accumulated number of bytes transmitted
	cmp	word ptr [u_count],dx ; is number of byte to read greater than 0
	jna	short readinode_retn

readinode_1:
	; AX = I-Number
	push	ax ; *

	call	iget	; get i-node into i-node section of core
	jc	short readinode_3 ; 01/03/2013

	; 20/01/2020
	mov	al,byte ptr [inode_flgs+1]
	test	al,80h	; regular file ?
	jnz	short readinode_4	; yes
	test	al,20h 	; device file ?
	;;jnz	short readinode_5 ; yes
	;jz	short readinode_4
	;;test	al,40h	; directory ?
	;;jnz	short readinode_4 ; yes
	jz	short readinode_4

	; 29/12/2019
	mov	byte ptr [Error],0FFh ; invalid file

readinode_5:
	stc
readinode_3:
	pop	ax ; * ; i-number
readinode_retn:
	retn 

readinode_4:
	; 20/01/2020
	;mov	si,word ptr [u_fofp]

	mov	si,offset u_off

	; 32 bit file size & 32 bit file pointer (17/12/2019)

	mov	ax,word ptr [inode_size]  ; file size lw
	mov	dx,word ptr [inode_size+2] ; file size rw
	; Note: we don't regard 5th byte of file size (inode_size_h) for now!

	sub	ax,word ptr [si]
	sbb	dx,word ptr [si+2]
	jnz	short readinode_2 ; remain bytes more than requested count

	or	ax,ax
	jz	short readinode_3

	cmp	ax,word ptr [u_count] 
	; are enough bytes left in file to carry out read
	jnb	short readinode_2 ; remain bytes more than requested count
	mov	word ptr [u_count],ax  ; fix read count to end of file

readinode_2:
	call	mget	; returns physical block number of block in file 
		; where offset points
	jc	short readinode_3 ; 01/03/2013
	; AX = Physical block number

	; 20/01/2020
	; Check current sector in the buffer

	cmp	byte ptr [buff_m],1
	jb	short readinode_6

	mov	dx,word ptr [buff_s]
	;or	dx,dx
	;jz	short readinode_sioreg ; buff_s = 0 is invalid	
	;cmp	dx,0FFFFh  ; buff_s = 0FFFFh is invalid
	;je	short readinode_sioreg

	; write buffer content if sector is not same

	cmp	dx,ax
	je	short readinode_sioreg

	push	ax
	;mov	ax,word ptr [buff_s]
	mov	ax,dx
	mov	bx,offset Buffer
	call	dskwr
	pop	ax
	jc	short readinode_3
	mov	byte ptr [buff_m],0

readinode_6:	
	cmp	ax,word ptr [buff_s]
	je	short readinode_sioreg

	mov	bx,offset Buffer
	call	dskrd	; read in block, BX points to 1st word of data in
		; buffer
	jc	short readinode_3

	mov	word ptr [buff_s],ax

readinode_sioreg:
	;mov	si,word ptr [u_off] ; R2
	;mov	cx,si ; cx = R3, si = R2
	
	; 20/01/2020
	call	sioreg

	; SI = file (user data) offset
	; DI = sector (I/O) buffer offset
	; CX = byte count

	xchg	si,di

        rep	movsb

	;pop	ax ; * ; i-number

	cmp	word ptr [u_count],0
	;;ja	short readinode_1
	ja	short readinode_4

	mov	byte ptr [Error],0

	;retn

	pop	ax ; * ; i-number

	retn 

readi	endp

iget 	proc near
	; 20/01/2020
	; 25/09/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 18/11/2012 unix boot file configuration version
	; of "iget" procedure.
	; 16/9/2012
     	; 14/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; AX=R1 
	; RETRO UNIX v1 FS
	;; return => if cf=1 error number in [Error] 

	; 20/01/2020
	; INPUT:
	; 	AX = inode number (0 if current inode)
	; OUTPUT:
	;	BX = inode number if cf = 0
	;	
	; Modified registers: ax, bx, cx, dx  

	cmp	ax,word ptr [ii] ; AX (R1) = i-number of current file
	je	short iget_4
iget_1:
	; 20/09/2019
	;mov	dl,byte ptr [imod]
	;and	dl,dl ; has i-node of current file been modified ?	
	;jz	short iget_2

	xor	dx, dx
	cmp	word ptr [imod],dx ; 0  ; byte ptr [imod] & byte ptr [imodx]
	jz	short iget_2

	; 20/01/2020
	;;xor	dl,dl ; mov al,0
	;;mov	byte ptr [imod],dl
	;mov	word ptr [imod],dx ; 0 
	
	push	ax ; * ; inode number
	mov	ax,word ptr [ii]	
	;inc	dl ; mov dl, 1
	mov	dl,1 ; 20/01/2020
	; dl = 1 = write
	call	icalc
	pop	dx  ; inode number, 1 to 256 (for 1.44MB fd)
	jc	short iget_4 ; error code in ax

	; 20/01/2020
	mov	ax,dx
	xor	dx,dx
	mov	word ptr [imod],dx ; 0 ; reset [imod] & [imodx]
iget_2:
	and	ax,ax
	jz	short iget_3
	mov	word ptr [ii],ax
	;xor	dl,dl	
	; dl = 0 = read
	call	icalc
iget_3:
	mov	ax,word ptr [ii]
iget_4:
	retn

iget	endp

icalc 	proc near
	; 20/01/2020
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 18/11/2012 unix boot file configuration version
	; of "icalc" procedure.
	; 17/8/2012
     	; 14/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; AX=R1
	; 0 = read, 1 = write
	; RETRO UNIX v1 FS
	;
        ; i-node is located in block (i+47)/16 and
	; begins 32*(i+47) mod 16 bytes from its start
	;; return => if cf=1 error number in [Error]

	; input -> dl = 0 -> read, 1 = write
	;	  ax = inode number

	; modified registers: ax, dx, cx, bx

	; 20/01/2020
	mov	byte ptr [I_rw], dl ; 0 = read, 1 = write

	;;add	ax, 47 ; add 47 to inode number
	;add	ax, 31 ; add 31 to inode number ; 19/09/2019

	; 20/01/2020
	dec	ax ; zero based inode number
	; ax = 0 to 255
	mov	dx, word ptr [systm.sb_InodeTblAddr] ; 4 for 1.44MB fd

	push	ax	; * ; R1 -> -(SP)

	shr	ax, 1   ; divide by 8 for Retro UNIX 386 v2
	shr	ax, 1   ;	(8 inodes per sector)
	shr	ax, 1
	; ax contains block number of block
	; in which inode exists

	mov	bx, offset I_buffer

	cmp	byte ptr [I_valid], 0
	jna	short icalc_0

	cmp	ax, word ptr [I_sector]
	je	short icalc_1
	
	mov	byte ptr [I_valid], 0 ; inode sector validation, invalid
icalc_0:
	mov	word ptr [I_sector], ax
	add	ax, dx 
	; ax = physical address of requested inode sector

	;mov	bx,offset I_buffer

	call	dskrd
	jnc	short @f

	; disk read error
	pop	dx ; *
	retn
@@:
	mov byte ptr [I_valid],1 ; inode sector validation, valid
icalc_1:
	;and	dx, 0Fh	; (i+47) mod 16
	;shl	dx, 1
	;shl	dx, 1
	;shl	dx, 1
	;shl	dx, 1
	;shl	dx, 1 
	 ; DX = 32 * ((i+47) mod 16)	
	  ; DX (R5) points to first word in i-node i.
	
	; 20/01/2020
	pop	ax ; * ; zero based inode number

	mov	si, bx ; offset I_buffer

	and	ax, 07h 
	jz	short @f

	mov	cl, 6
	shl	ax, cl
	; DX = 64 * ((i+31) mod 8)
	; DX points to first word in i-node i.	

	add	si, ax
@@:
	mov	di, offset inode
	; inode is address of first word of current inode

	;mov	cx, 16 ; CX = R3
	; 19/09/2019	
	mov	cx, 32 ; inode size/2 for Retro UNIX 386 v2 (& UNIX v7)

	cmp	byte ptr [I_rw],0
	jna	short icalc_3 ; 0 = read (and copy i-node to memory) 
icalc_2:
	xchg	si,di
	; over write old i-node (in buffer to be written)
	rep	movsw

	; 20/01/2020

	mov	ax, word ptr [I_sector]
	add	ax, word ptr [systm.sb_InodeTblAddr]

	;mov	bx, offset I_buffer

	; bx = offset I_buffer

	;call	dskwr
      	
        ;retn

	jmp	dskwr ; 25/09/2019 
icalc_3:
	; copy new i-node into inode area of (core) memory
	rep	movsw
icalc_4:
	retn

icalc 	endp

mget 	proc near
	; 27/03/2022
	; 25/01/2020
	; 23/01/2020
	; 22/01/2020
	; 21/01/2020
	; 20/01/2020 (32 bit file pointer/offset)
	;	    ! (16 bit disk sector address) !	
	; 19/09/2019 - Retro UNIX 386 v2
	; 	(simplified for initialization floppy disk)
	;	NOTE: Maximum file size is -still- 65535 bytes  
	;	     at this stage (for initialization floppy)  
	; 05/03/2013
	; 03/03/2013
	; 01/03/2013
	; 18/11/2012
	; 14/11/2012 unix boot file configuration version
	; of "mget" procedure
	; 31/10/2012
	; 20/10/2012
	; 19/8/2012
	; 13/8/2012
	; 27/7/2012
     	; 21/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; return -> AX=R1
	; RETRO UNIX v1 FS
	; initialization/format version
	; cf -> 1 = error (no free block)

	; 20/01/2020
	; INPUT:
	;	[u_off] = file pointer
	; OUTPUT:
	;	ax = sector/block address
	;
	; Modified registers: ax,dx,cx,bx,si,di

	;push	bx
	;push	cx
	;push	dx
	;; contents of bx, cx, dx will be destroyed 
mget_0:
	;mov	bl,byte ptr [u_off+1] ; Max. file size = 64KB-1
        ;xor	bh,bh

	; 20/01/2020
	mov	bx,offset u_off

	mov	ax,word ptr [bx]
	mov	dx,word ptr [bx+2]

  	;test	word ptr [inode_flgs],4096 ; 1000h
		      ; is this a large or small file
	;jnz	short mget_8 ; 4f ; large file

	test	byte ptr [inode_flgs+1],16 ; 10h
	;jnz	short mget_8 ; not small file
	; 22/01/2020
	jz	short @f
	jmp	mget_8	
@@:	
        ;test	bl,0F0h ; !0Fh  ; error if BX (R2) >= 16	    
	;jnz	short mget_3

	; small file size limit = 5120 bytes (1400h)

	or	dx,dx
	jnz short mget_3 ; requested file offset > 65535

	; 19/09/2019
	;cmp	bl,14h
	; 20/01/2020
	cmp	ah,14h
	jnb	short mget_3 ; 3f ; requested offset >= 5120

	; 01/12/2019
	mov	bl,ah  ; div ax by 256
	xor	bh,bh

	;and	bl,0Eh  ; clear all bits but bits 1,2,3
	and	bl,1Eh  ; 19/09/2019 ; clear all bits but bits 1,2,3,4

	shl	bl,1 ; 19/09/2019 - Retro UNIX 386 v2

	mov	ax,word ptr [inode_dskp+bx] ; AX = R1, physical block number
	or	ax,ax
	jnz	short mget_2	; if physical block number is zero
		; then need a new block for file
	; 20/01/2020
	;or	dx,dx
	;jnz	short mget_2

	call	alloc	; allocate a new block for this file	
		; AX (R1) = Block number
	;jc	short mget_6 ; cf -> 1 & ax = 0 -> no free block

	; 22/01/2020
	jnc	short @f

mget_6: 
	;mov	ax,err_NOFREEBLOCK

	;pop	dx
	;pop	cx
	;pop	bx
mget_7:
	retn
@@:
	mov	word ptr [bx+inode_dskp],ax
	; 20/01/2020
	;mov	word ptr [bx+inode_dskp+2],dx ; 0 

	call	setimod

	; 20/01/2020
	mov	bx,offset Buffer

	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_1

	push	ax
	mov	ax,word ptr [buff_s]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	jc	short mget_2
	mov	byte ptr [buff_m],0 ; reset buffer modified sign
mget_1:
	mov	word ptr [buff_s],ax

	mov	byte ptr [buff_m],1 ; set buffer modified sign

	call	clear	; clear Buffer

	; BX = offset Buffer

	call	dskwr
	jc	short mget_2

	mov	byte ptr [buff_m],0 ; reset buffer modified sign
	
mget_2: ; 2
	; AX (R1) = Physical block number (if cf = 0)

	;pop	dx
	;pop	cx
	;pop	bx

	retn 

mget_3: ; 3
	; adding on block which changes small file to large file
	call	alloc 
	jc	short mget_6 ; 01/03/2013 
	; call	wslot  ; setup I/O buffer for write
	;	  ; R5 points to the first data word in buffer

	;push	ds
	;pop	es

	; 20/01/2020
	mov	bx,offset Buffer

	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_4

	push	ax ; +
	mov	ax,word ptr [buff_s]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax  ; +
	jc	short mget_2
	mov	byte ptr [buff_m],0 ; reset buffer modified sign
mget_4:
	mov	word ptr [buff_s],ax  ; Block/Sector number

	; 13/01/2020 (si, di)
	;push	si
	;push	di
	push	ax ; *

	;mov	cx,8	; R3, transfer old physical block pointers
		; into new indirect block area for the new
		; large file	
	mov	cx,10 ; 15/09/2019	
	; 09/11/2019
	;mov	di,offset Buffer ; BX = R5

	; 26/01/2020
	mov	si,offset inode_dskp

	; 20/01/2020
	mov	di,bx ; offset Buffer 

	xor	ax,ax ; mov ax,0
mget_5: ; 1
	movsw
	; 29/09/2019
	mov	word ptr [si-2], ax
	; 19/09/2019
	movsw
	mov	word ptr [si-2], ax 
	loop	mget_5
	
	;mov	cl, 256-8 ; clear rest of data buffer
	mov	cl, 256-20 ; 19/09/2019

	rep	stosw  ; clear buffer offset 40 to 512 (472 bytes)

	pop	ax ; *
	;pop	di
	;pop	si

	;mov	byte ptr [buff_m], 1 ; modified

	call	dskwr	
	jc	short mget_7 ; 20/01/2020

	;mov	ax, word ptr [buff_s]

	mov	word ptr [inode_dskp], ax

	;or	word ptr [inode_flgs], 4096 ; 1000h
	; 19/09/2019
	or	byte ptr [inode_flgs+1], 16 ; 10h 
	
	call	setimod

	jmp	mget_0

;mget_6: 
;	;mov	ax, err_NOFREEBLOCK
;
;	;pop	dx
;	;pop	cx
;	;pop	bx
;mget_7:
;	retn

mget_8: ; 4 ; large file
	; 20/01/2020
	; (32 bit file size <= 512KB)
 	
	; Retro UNIX 386 v2 disk inode contains..
	; (if large file flag is set)
	; 8 indirect disk block/sector dword pointers
	; +1 double indirect disk block/sector dword pointers
	; +1 triple indirect disk block/sector dword pointers

	; check indirect pointers limit (as file offset)
	; 8*128 = 1024 blocks (or sectors) or 512 KB
	; check dx (file offset hw) value

	cmp	dx,08h ; 524288 = 080000h
	jb	short mget_9 ; proper for runix floppy disk (<=512KB)

 	; (!this 'mget' can not handle double indirect pointers!)
	; (!also it is nonsence if we would use >512Kb files for runix fd!)

	stc  ; error ! (big file!)
	retn
mget_9:
	; 20/01/2020
	mov	cx,9
	call	shr32
	; ax = sector offset (flat)
	; dx = 0
	mov	cl,al
	and	cl,127
	mov	byte ptr [index],cl ; direct sector/pointer index (0 to 127)
	mov	cl,7	
	;call	shr32
	shr	ax,cl  ; 13/01/2020
	; ax = indirect pointer index (<= 7)
	; dx = 0
	shl	al,1
	shl	al,1
	mov	si,ax

	; ***

	mov	ax,word ptr [si+inode_dskp]

	mov	bx,offset Buffer
	
	or	ax,ax
	jnz	short mget_14	; if physical block number is zero
		; then we need a new block for file
	
	call	alloc	; allocate a new block for this file	
		; AX (R1) = Block number
	;;jc	mget_6	; cf -> 1 & ax = 0 -> no free block
	; 27/03/2022
	;jnc	short @f
	;jmp	mget_6 ; retn ; 27/03/2022
	jc	short mget_11
;@@:
	mov	word ptr [si+inode_dskp],ax
	;mov	word ptr [si+inode_dskp+2],dx ; 0

	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_12
mget_10:
	push	ax
	mov	ax,word ptr [buff_s]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	;jc	short mget_6
	;mov	byte ptr [buff_m],0 ; reset buffer modified sign
	jnc	short mget_13
mget_11:
	retn
mget_12:
	mov	byte ptr [buff_m],1  ; buffer modified
mget_13:
	call	clear ; clear buffer

	call	setimod

	mov	word ptr [buff_s],ax

	; 25/01/2020 ('setimod' modifies bx register)
	mov	bx,offset Buffer

	call	dskwr
	jc	short mget_11

	mov	byte ptr [buff_m],0 ; reset buffer modified sign
	jmp	short mget_16
mget_14:
	cmp	ax,word ptr [buff_s]
	je	short mget_16
	
	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_15

	push	ax
	mov	ax,word ptr [buff_s]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	jc	short mget_11

	mov	byte ptr [buff_m],0 ; reset buffer modified sign
mget_15:
	;mov	bx,offset Buffer	
	call	dskrd
	jc	short mget_11

	mov	word ptr [buff_s],ax
mget_16:
	; 21/01/2020
	;mov	si,offset Buffer
	;mov	bl,byte ptr [index] ; direct sector/pointer index (0 to 127)
	;cmp	bl,127 ; *!*
	;ja	short mget_17

	; 23/01/2020
	;mov	bx,offset Buffer
	cmp	byte ptr [index],127 ; 7Fh
	ja	short mget_18
	mov	si,bx
	mov	bl,byte ptr [index]

	mov	byte ptr [index],0FFh ; 255 ; *!* ; 21/01/2020	

	xor	bh,bh
	shl	bl,1
	shl	bx,1

	add	bx,si ; offset Buffer

	mov	ax,word ptr [bx]

	or	ax,ax
	jnz	short mget_17	; if physical block number is zero
		; then we need a new block for file
	call	alloc	; allocate a new block for this file	
		; AX (R1) = Block number
	;jc	short mget_6 ; cf -> 1 & ax = 0 -> no free block
	; 22/01/2020
	jc	short mget_18

	mov	word ptr [bx],ax

	mov	byte ptr [buff_m],1 ; set buffer modified sign
	mov	bx,si ; offset Buffer
	jmp	short mget_10

mget_17:
	; ax = (direct) disk sector/block address
	mov	bx,si ; offset Buffer
mget_18:
	retn

mget	endp

alloc	proc near
	; 28/03/2022
	; 25/01/2020
	; 23/01/2020
	; 21/01/2020
	; 20/01/2020
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 14/11/2012 unix boot file configuration version
	; of "alloc" procedure
	; 21/8/2012
	; 18/8/2012
	; 17/8/2012
	; 5/8/2012	
 	; 21/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; input -> AX=R1 
	;; output -> AX=R1
	; RETRO UNIX v1 FS

	; 20/01/2020
	;; input -> none
	;; output -> AX = allocated sector/block number
	;; 	cf = 1 -> could not be allocated
	; Modified registers: cx, dx

	;push	cx
	push	bx ; R2
	;push	dx ; R3

	;mov	bx, offset systm ; SuperBlock 
	; start of inode and free storage map for disk
alloc_1: ; 1
	;mov	ax, word ptr [systm.sb_FreeMapSize] 
		; # of bytes in free storage map
	;shl	ax, 1	; multiply AX (R1) by 8 gives # of blocks
	;shl	ax, 1
	;shl	ax, 1
	;mov	cx, ax ; R1, bit count of free storage map

	; 25/01/2020
	mov	cx, word ptr [systm.sb_FreeMapSize] 
		; # of bytes in free storage map	
	shl	cx, 1	; multiply CX by 8 gives # of blocks
	shl	cx, 1
	shl	cx, 1

	mov	bx, offset fbm_buffer-2
			; 19/09/2019 - Retro UNIX 386 v2
	xor	ax, ax ; 0
alloc_2: ; 1
	inc	bx ; 18/8/2012
	inc	bx ; 
	mov	dx, word ptr [bx] ; mov (R2)+, R3
	or	dx, dx
	jnz	short alloc_3 ; 1f
		; branch if any free blocks in this word	
	add	ax, 16
	cmp	ax, cx    
	jb	short alloc_2 ; 1b

	;jmp	short panic ; no free storage 

	xor	ax, ax
	stc	; cf=1 --> error: no free block

	jmp	short alloc_7	

alloc_3: ; 1
        shr	dx, 1	; R3  ; Branch when free block found,
		; bit for block k is in byte k/8 
		; in bit k (mod 8) 	
	jc	short alloc_4 ; 1f
	inc	ax ; R1	; increment bit count in bit k (mod 8)    
	jmp	short alloc_3 ; 1b
	
alloc_4:
	;call	free_3

alloc_free_3:	
	mov	dx, 1	
	;mov	cx, ax
	;and	cx, 0Fh
	; 23/01/2020
	mov	cl, al
	and	cl, 0Fh
	jz	short @f
	shl	dx, cl
@@:
	mov	bx, ax
	shr	bx, 1
	shr	bx, 1
	shr	bx, 1
	shr	bx, 1
free_4: ; 1
	shl	bx, 1 ; 21/8/2012
         ; BX (R2) = k/8	
	;add	bx, offset systm+2 ; SuperBlock+2
	add	bx, offset fbm_buffer ; 19/09/2019

alloc_5: ; 1 
	; 21/8/2012
	not	dx ; masking bit is '0' and others are '1'
	and	word ptr [bx], dx  ; bic r3, (r2) 	
	; 0 -> allocated

	; 21/01/2020
	mov	cx, word ptr [systm.sb_FreeBlocks+2]
	and	cx, cx
	jnz	short @f ; (cx = 0FFFFh)
	mov	cx, word ptr [systm.sb_FreeBlocks] 
	inc	cx
	jz	short @f ; (cx = 0FFFFh) -> (cx = 0) 

	; 29/09/2019
	dec	word ptr [systm.sb_FreeBlocks]
@@:
	; 28/03/2022
	; (first free block has been searched from block 0;
	;  so, ax contains previous first free block value and
	;  it is better if the new value is set one/just next to ax)
  
	mov	dx, ax
	inc	dx ; next block 
		   ; (next ffb search will be started from)
	mov	word ptr [systm.sb_FirstFreeBlk], dx
	sub	dx, dx
	mov	word ptr [systm.sb_FirstFreeBlk+2], dx

	; 28/03/2022
	;; 21/01/2020
	;sub	dx, dx
	;cmp	ax, word ptr [systm.sb_FirstFreeBlk]
	;;jne	short alloc_6
	;ja	short alloc_6 ; 23/01/2020
	;
	;cmp	word ptr [systm.sb_FirstFreeBlk+2], dx ; 0
	;;jne	short alloc_6
	;ja	short alloc_6 ; 23/01/2020
	;
	;;mov	word ptr [systm.sb_FirstFreeBlk], 0FFFFh ; invalidate
	;;mov	word ptr [systm.sb_FirstFreeBlk+2], 0FFFFh
	;
	;; 21/01/2020
	;; set first free block to the next sector (in order)
	;;add	word ptr [systm.sb_FirstFreeBlk], 1
	;;adc	word ptr [systm.sb_FirstFreeBlk+2], 0
	;	
	;inc	word ptr [systm.sb_FirstFreeBlk]
alloc_6:
	;clc ; 23/01/2020
	;inc	byte ptr [smod] ; superblock modified sign
	;mov	byte ptr [smod], 1
alloc_7:
	;pop	dx ; R3
	pop	bx ; R2
	;pop	cx 
	; AX (R1) = Block number
	retn

alloc	endp

free	proc near
	; 23/01/2020
	; 22/01/2020
	; 21/01/2020
	; 10/09/2019 - Retro UNIX 386 v2
        ; 01/12/2012
        ; unix boot file configuration version
	; of "free" procedure of retro unix v1.0 by Erdogan Tan
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972 
	; ('free' procedure)
	; 
	; INPUT -> ax (R1) = physical block number
	; Return -> CF = 0 -> Successed, CF = 1 -> failed
	;		
	; Modified registers: cx, bx, dx

	;push	bx	; mov r2,-(sp) / save r2, r3
	;;push	cx	; mov r3,-(sp)
	;push	dx

	;call	free_3	; jsr r0,3f 
	; / set up bit mask and word no. in free storage map
	; / for block
free_3: ; 3
	mov	dx,1    
	;mov	cx,ax   ; mov r1,r2 / block number, k, = 1
	;and	cx,0Fh  ; bic $!7,r2 / clear all bits but 0,1,2; r2 = (k) mod (8)
		; clr r3
	; 23/01/2020
	mov	cl,al
	and	cl,0Fh
	jz	short @f
		; bisb 2f(r2),r3 / use mask to set bit in r3 
		; corresponding to / (k) mod 8
	shl	dx,cl
@@:
	mov	bx,ax	; mov r1,r2 / divide block number by 16
	shr	bx,1	; asr r2	
	shr	bx,1	; asr r2	
	shr	bx,1	; asr r2	
	shr	bx,1	; asr r2
	   	; bcc 1f / branch if bit 3 in r1 was 0 i.e., 	
		; bit for block is in / lower half of word
		; swab r3 / swap bytes in r3; bit in 
		; upper half of word in free / storage map	
free_1: ; 1
	shl	bx,1 ; asl r2 / multiply block number by 2; r2 = k/8
	;add	bx,offset systm+2 ; add $systm+2,r2 
		 ; / address of word of free storage map for drum
	   	 ; / with block bit in it
	; retn  ; rts r0 (return from free_3)
	; 10/09/2019
	add	bx,offset fbm_buffer
@@:
	or	word ptr [bx],dx  ; bis r3, (r2)
	; / set free storage block bit; indicates free block	
	; 0 -> allocated, 1 -> free

	;;inc	byte ptr [smod] ; incb smod / set superblock modified for drum
	;mov	byte ptr [smod],1 ; / set superblock modified for drum
	;pop	dx
	;pop	cx	; mov (sp)+,r3 / restore r2, r3
	;pop	bx	; mov (sp)+,r2
	; AX (R1) = Block number

	; 21/01/2020
	mov	cx, word ptr [systm.sb_FreeBlocks+2]
	and	cx, cx
	jnz	short @f
	mov	cx, word ptr [systm.sb_FreeBlocks] 
	inc	cx
	jz	short @f

	; 29/09/2019
	inc	word ptr [systm.sb_FreeBlocks]
@@:
	xor	dx, dx

	cmp	ax,word ptr [systm.sb_FirstFreeBlk]
	jnb	short @f
	
	; 21/01/2020
	mov	word ptr [systm.sb_FirstFreeBlk],ax

	; 22/01/2020
	;cmp	word ptr [systm.sb_FirstFreeBlk+2],dx ; 0
	;je	short @f

	mov	word ptr [systm.sb_FirstFreeBlk+2],dx ; 0

	clc
@@:
	retn

free	endp

setimod proc near
	; 25/01/2020
	; 30/09/2019
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 23/02/2013 (fromdos) file m. date&time modification
	; 14/11/2012 unix boot file configuration version
	; of "setimod" procedure
	; 13/8/2012
	; 21/7/2012
	; 14/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; AX=R0, BX=R1, CX=R3, DX=R5 
	; [SP] = Argument 1, 0 = read, 1 = write
	; RETRO UNIX v1 FS
	; initialization/format version

	; 25/01/2020
	; Modified registers: bx (in 'epoch'), cx, dx

	;push	dx
	push	ax
	mov	byte ptr [imod], 1
	; 23/02/2013
	mov	ax, word ptr [inode_ctim]
	mov	dx, word ptr [inode_ctim]+2
	and	ax, ax
	jnz	short setimod_3
	and	dx, dx
	jnz	short setimod_3
setimod_1:
	; Erdogan Tan 14-7-2012
	call	epoch
	mov	word ptr [inode_ctim], ax
	mov	word ptr [inode_ctim]+2, dx
	; 19/09/2019
	jmp	short setimod_5
setimod_6:
	clc
setimod_2:
	; File/Directory data (file size or content) is changed
	; This is last modification date&time
	mov	word ptr [inode_mtim], ax
	mov	word ptr [inode_mtim]+2, dx
setimod_5:
	; 19/09/2019
	; This is last access or last changing date&time
	; of inode (chmod,chown,chrp,link)
	; parameters (without file/dir data changing)
	mov	word ptr [inode_atim], ax
	mov	word ptr [inode_atim]+2, dx
setimod_4:
	pop	ax
        ;pop	dx
	retn
setimod_3:
 	; 23/02/2013
	xor	cx, cx
	cmp	word ptr [inode_mtim], cx
	jna	short setimod_2
	cmp	word ptr [inode_mtim]+2, cx
	jna	short setimod_2

	call	epoch

	; 19/09/2019 - Retro UNIX 386 v2 by Erdogan Tan
	cmp	byte ptr [imodx], 1 ; flag means "file/dir data is same but
	;jb	short setimod_2	   ; inode is changed"
	jb	short setimod_6 ; 30/09/2019

	; 19/09/2019
	; File/Dir data (File size or content) is same but
	; inode's mode, link count, owner or group id is changed
	; (so, we do not change last modification date&time)
	mov	byte ptr [imodx], 0 ; reset inode modified (extended) flag
	jmp	short setimod_5

setimod endp

clear	proc near
	; 20/01/2020
	; 13/11/2019 - Retro UNIX 386 v2
	; (Data) Buffer clearing

	; Modified registers: cx, di
	
	;push	di
	;push	cx
	push	ax

 	xor	ax,ax

	mov	di,offset Buffer
	mov	cx,256 
	rep	stosw

	pop	ax
	;pop	cx
	;pop	di	
@@:	
	retn

clear	endp

make_directory proc near
	; 15/04/2022 ('number of links' bugfix)
	; 24/01/2020
	; 04/12/2015 (14 byte directory names)	
	; 30/12/2012
	;
	; mov word ptr [u.namep], si

	call	sys_mkdir
	;jc	short @f
	jc 	short @b

        ;ax = i-number

	;mov	ax, word ptr [ii]
	;mov	word ptr [u_dirbuf], ax

	mov	word ptr [u_namep], offset dot ; ('.')
	xor	ax, ax
	mov	word ptr [u_dirp], ax ; 0	
	call	mkdir 	; make a directory entry 
			; in current (ii) directory
	;jc	short @f
	jc	short @b

	mov	word ptr [u_dirp], 16 ; 04/12/2015 (10 -> 16)
	mov	ax, word ptr [pdir] 
	mov	word ptr [u_dirbuf], ax
	mov	word ptr [u_namep], offset dotdot ; ('..')

	; 15/04/2022
;	;call	mkdir
;	; 20/01/2020
;	;jmp	short mkdir
;;@@:
;	;retn

	call	mkdir	; make a directory entry 
			; in current (ii) directory
	; 15/04/2022
	;jc	short @f
	jc	short @b ; 15/04/2022
	call	mkdir_inc_nlinks	
	mov	ax, word ptr [pdir] ; parent dirs i-number
	call	iget
        ;jc	short @f
	jc	short @b
mkdir_inc_nlinks:
	inc	byte ptr [inode_nlks] ; increase link count
	;call	setimod
	jmp	setimod
;@@:
;	retn

make_directory endp

mkdir proc near
	; 20/01/2020
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	;
	; 04/12/2015 (14 byte directory names)
	; 11/11/2012
	; unixboot.asm (boot file configuration)
	; version of 'mkdir'
	;
	; 31/10/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	; RETRO UNIX v1 FS
	;
	; mkdir : make a directory entry
	;
	; 8086 CPU & IBM PC architecture modifications by Erdogan Tan 
	;
	; return => if cf=1 error number in [Error], ax = mode
	; If cf=0 -> AX = I-Number (also in u.dirbuff)
	;
	; input:
	; 	u.namep = file name
	; 	ii = current directory's i-number
	; 	u.dirbuf = directory entry (source) location
	; output:
	; 	u.dirbuf+2 to u.dirbuf+16 (u.dirbuf+10) = file name
	; 	u.off = directory entry offset in current directory
	; 	u.base = start of u.dirbuf
	; ;;;r1 (AX) = i-number of current directory

mkdir_0:
	; jsr r0,copyz; u.dirbuf+2; u.dirbuf+10. / clear this
	mov 	cx, 7  ; ; 04/12/2015 (4 -> 7)
	xor 	ax, ax
	mov 	di, offset u_dirbuf+2
	rep 	stosw	
         
        mov	si, word ptr [u_namep] ; mov u.namep,r2 
		; r2 points to name of directory entry

	mov	di, offset u_dirbuf+2 ; mov $u.dirbuf+2,r3
		; r3 points to u.dirbuf+2
mkdir_1: ;1 / put characters in the directory name in u.dirbuf+2 - u.dirbuf+16
	lodsb 	;movb  (r2)+,r1 / move character in name to r1
	and 	al, al
	jz 	short mkdir_2	; beq 1f / if null, done
	cmp 	al, '/'		; cmp r1,$'/ / is it a "/"?
	je	short mkdir_stc	; beq error9 / yes, error
	; 04/12/2015 (10 -> 16)
	cmp	di, offset u_dirbuf+16 ; cmp r3,$u.dirbuf+10. 
		      ; have we reached the last slot for
		      ; a char?
	je	short mkdir_1	; beq 1b / yes, go back
	
	stosb	; movb r1,(r3)+ 
		; no, put the char in the u.dirbuf
	jmp 	short mkdir_1	; br 1b / get next char

	; 21/01/2020
mkdir_stc:
	; invalid file name, al="/", ah=0
	mov	ah, 1
	stc
@@:
	retn

mkdir_2: ;1
	mov 	ax, word ptr [u_dirp] ; mov u.dirp,u.off
	mov 	word ptr [u_off], ax  ; pointer to empty current directory
		     		; slot to u.off
	; 20/01/2020
	mov	word ptr [u_off+2], 0
wdir:
	mov	word ptr [u_base], offset u_dirbuf
		; mov$u.dirbuf,u.base
		; u.base points to created file name
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16)
		; mov $10.,u.count
 	mov	ax, word ptr [ii] ; mov ii,r1 

		; r1 has i-number of current directory
;	call	writei	; jsr r0,writei / write into directory
;@@:	
;	retn	; rts r0

	; 21/01/2020
	;jmp	short writei

mkdir 	endp

writei	proc near
	; 21/01/2020
	; 20/01/2020 (32 bit file pointer/offset)
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 18/11/2012
	; 11/11/2012
	; unixboot.asm (boot file configuration)
	; version of 'writei'
	;
	; 31/10/2012
	; 18/08/2012
	; 17/07/2012
	; BX = R1, i-number
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;
	; RETRO UNIX v1 FS
	; initialization/format version
	;
	; writei: write file
	;
	; 8086 CPU & IBM PC architecture modifications by Erdogan Tan 
	;; return => if cf=1 error number in [Error]

	; input:
	; AX = R1 = I-Number
	; u.count = byte count
	; u.base = user buffer (offset)
	; u.off (u.fofp) = (pointer to) current file offset
 
	xor	dx,dx ; 0 ; clr u.nread	
	mov	word ptr [u_nread],dx
			; clear the number of bytes transmitted during
			; read or write calls 
			; tst u.count	
	cmp	word ptr [u_count],dx ; test the byte count specified by the user
	;ja	short write_1 ; 1f    ; bgt 1f / any bytes to output; yes, branch
	;retn	; rts 0 / no, return - no writing to do
	;jna	short write_inode_retn
	; 20/01/2020
	jna	short @b ; 03/09/2019	
dskw:
	; 21/01/2020
write_1:
	; 20/01/2020
	push	di
	push	si

	push	ax	; save i-number on stack

	mov	byte ptr [Error],0FFh ; 21/01/2020

	call	iget	; jsr	r0,iget
			; write i-node out (if modified), read i-node 'r1'
			; into i-node area of core
	;jc	dskw_5 ; 01/03/2013
	jnc	short @f
	jmp	dskw_5
@@:
	; 03/09/2019 - Retro UNIX 386 v2
	mov	al,byte ptr [inode_flgs+1]
	test	al,80h		; regular file ?
	jnz	short dskw_8	; yes
	test	al,20h 		; device file ?
	;;jnz	short dskw_7	; yes
	;jz	short dskw_8 
	;;test	al,40h		; directory ?
	;;jnz	short dskw_8	; yes
	jnz	short dskw_7 	
dskw_8:
	;mov	si,word ptr [u_fofp]
	mov	si,offset u_off

	;mov	dx,word ptr [si] 
		; mov *u.fofp,r2 
		; put the file offset [(u.off) or the offset in
	; the fsp entry for this file] in r2
	;add	dx,word ptr [u_count]
		; add u.count,r2 
		; no. of bytes to be written + file offset is
	; put in r2

	;cmp	dx,word ptr [inode_size] ; cmp r2,i.size
		; is this greater than the present size of
		; the file?
	;jna	short dskw_1 ; blos 1f / no, branch

	;mov	word ptr [inode_size],dx ; mov	r2,i.size 
		; yes, increase the file size to file offset +
		; no. of data bytes

	; 32 bit file size
	mov	ax,word ptr [si]
	mov	dx,word ptr [si+2]

	add	ax,word ptr [u_count]
		; add u.count,r2 
		; no. of bytes to be written + file offset is
		; put in r2
	adc	dx,0
	; is this greater than the present size of  the file?
	cmp	dx,word ptr [inode_size+2]
	je	short dskw_9	
	jb	short dskw_1
	mov	word ptr [inode_size+2],dx 
	jmp	short dskw_10
dskw_7:
	stc
	jmp	short dskw_5
dskw_9:
	cmp	ax,word ptr [inode_size] ; is this greater than the present size of
	jna	short dskw_1	    ; the file?	
dskw_10:  
	mov	word ptr [inode_size],ax ; yes, increase the file size to file offset +
		    ; no. of data bytes

	call	setimod	; jsr r0,setimod 
			; set imod=1 (i.e., core inode has been
			; modified), stuff time of modification into
			; core image of i-node
dskw_1: ; 1
 	call	mget	; jsr r0,mget 
			; get the block no. in which to write the next data
			; byte
			; AX = R1 = Block Number
	jc	short dskw_5 ; 01/03/2013

	; ax = Block/Sector number

	; 20/01/2020

	; Check current sector in the buffer

	cmp	byte ptr [buff_m],1
	jb	short dskw_12

	mov	dx,word ptr [buff_s]
	;or	dx,dx
	;jz	short dskw_12 ; buff_s = 0 is invalid	
	;cmp	dx,0FFFFh     ; buff_s = 0FFFFh is invalid
	;je	short dskw_12

	; write buffer content if sector is not same

	cmp	ax,dx
	je	short dskw_12
dskw_11:
	push	ax
	;mov	ax,word ptr [buff_s]
	mov	ax,dx
	mov	bx,offset Buffer
	call	dskwr
	pop	ax
	jc	short dskw_5
	mov	byte ptr [buff_m],0
dskw_12:
	; 20/01/2020
	;mov	si,offset u_off
	;mov	bx,word ptr [si]

	mov	bx,word ptr [u_off]
	and	bx,1FFh	; bit *u.fofp,$777
			; test the lower 9 bits of the file offset
	mov	bx,offset Buffer ; 21/01/2020
	jnz	short dskw_2 ; bne 2f 
			; if its non-zero, branch; if zero, file offset = 0,
			; 512, 1024,...(i.e., start of new block)
	cmp	word ptr [u_count],512	; cmp u.count,$512.
			; if zero, is there enough data to fill an
			; entire block? (i.e., no. of
	;jnb	short dskw_6 ; bhis 3f / bytes to be written greater than 512.? 
		    ; Yes, branch. / Don't have to read block
	jnb	short dskw_3
dskw_2: ; 2
	; in as no past info. is to be saved (the entire block will be
        ; overwritten).

	;mov	bx,offset Buffer
	cmp	ax,word ptr [buff_s]
	; 22/01/2020
	je	short dskw_3
;dskw_13:
dskw_6:
			; AX=R1 (block number)
	call	dskrd	; jsr r0,dskrd 
			; no, must retain old info.. Hence, read block 'r1'
			; into an I/O buffer
	;jc	short dskw_5 ; 01/03/2013
	; 20/01/2020
	jnc	short dskw_3
dskw_5:
	pop	ax

	pop	si
	pop	di

	retn

dskw_3:
writeinode_sioreg:
	;call	wslot

	; 20/01/2020

	call	sioreg

	; SI = user data offset (r1)
	; DI = sector (I/O) buffer offset (r2)
	; CX = byte count (r3)

dskw_4: ; 2
	rep	movsb

	mov	byte ptr [buff_m],1

	mov	word ptr [buff_s],ax

	;mov	bx,offset Buffer ; offset sector_buffer	
	call	dskwr ; jsr r0,dskwr / write the block and the i-node
	jc	short dskw_5

	mov	byte ptr [buff_m],0
	
        cmp	word ptr [u_count],0 ; any more data to write?
	;ja	dskw_1 ; 1b   ; yes, branch
	jna	short @f
	jmp	dskw_1
@@:
	mov	byte ptr [Error],0
	jmp	short dskw_5

writei	endp

dskwr	proc near
	; 22/01/2020
	; 20/01/2020 - Retro UNIX 386 v2
	;
	; Disk (Sector) Write
	;
	; INPUT: 
	;	AX = Sector address/number (LBA)
	;	ES:BX = Buffer address
	; OUTPUT:
	;	CF = 0 -> succeeded
	;	CF = 1 -> Error, Error code in [Error]
	;
	; Modified registers: cx, dx

	mov	byte ptr [rw],3 ; write
	jmp	short diskio

dskwr	endp

dskrd	proc near
	; 24/01/2020
	; 22/01/2020
	; 20/01/2020 - Retro UNIX 386 v2
	;
	; Disk (Sector) Read
	;
	; INPUT: 
	;	AX = Sector address/number (LBA)
	;	ES:BX = Buffer address
	; OUTPUT:
	;	CF = 0 -> succeeded
	;	CF = 1 -> Error, Error code in [Error]
	;
	; Modified registers: cx, dx

	mov	byte ptr [rw],2 ; read
diskio:
	;mov	word ptr [Error],0 ; Reset error code
	; 24/01/2020
	mov	byte ptr [Error],0 ; Reset error code

	; ax = sector/block number

	cmp	byte ptr [PhysicalDriveNumber], 90h ; fd image file sign
	jnb	short image_file_rw ; 07/07/2015

chs_read_write:
	;xor	ch,ch
	mov	cl,4 ; Retry count
chs_rw_again:
	push	ax ; *  ; sector address

	push	cx ; ** ; (remain) retry count

	mov	dx,18		; Sectors per track
	div	dl	      
	mov	cl,ah		; Sector (zero based)
	inc	cl		; To make it 1 based
	shr	al,1		; Convert Track to Cylinder
	adc	dh,0		; Heads (0 or 1)

	mov	dl,byte ptr [PhysicalDriveNumber] 
	mov	ch,al

	mov	ah,byte ptr [rw] ; 2 = read, 3 = write 	
	mov	al,1	; 1 sector
	int	13h	; BIOS Service func ( ah ) = 2
			; Read disk sectors
			; BIOS Service func ( ah ) = 3
			; Write disk sectors
			; AL-sec num CH-cyl CL-sec
			; DH-head DL-drive ES:BX-buffer
			; CF-flag AH-stat AL-sec read
	pop	cx ; **
	jnc	short chs_rw_ok

	dec	cl		
	jz	short chs_rw_err

	cmp	ah,09h ; DMA crossed 64K segment boundary
	je	short chs_rw_err

	xor	ah,ah ; reset
	int	13h

	pop	ax ; *

	jmp	short chs_rw_again

chs_rw_err:
	stc
	mov	byte ptr [Error],ah
chs_rw_ok:	
	pop	ax ; *
	retn

dskrd	endp

image_file_rw proc near
	; 24/01/2020
	; 20/01/2020
	; 19/09/2019 - Retro UNIX 386 v2
	; 14/07/2015
	; 07/07/2015
	; writing a block (sector) to floppy disk image file
	; INPUTS:
	; 	ax = sector/block number
        ;	offset WriteBuffer = buffer address
	;       [img_file_handle] = file handle
	;	number of bytes to be written = 512

	push	ax ; * ; sector address ; 24/01/2020 

	push	bx ; buffer address ; 19/09/2019 
	mov	dx,512
	mul	dx
	mov	cx,dx
	mov	dx,ax
	sub	al,al ; specified offset is from the beginning of the file
	mov	ah,42h ; seek (move file pointer)	
	mov 	bx,word ptr [img_file_handle]
	int	21h
	pop	dx ; buffer offset
	jc	short img_file_rw_err

	;mov	bx,word ptr [img_file_handle]
	mov	cx,512
	; ds:dx = buffer offset
	mov	ah,3Dh ; 3Fh = read from file, 40h = write to file
	add	ah,byte ptr [rw] ; 2 = read, 3 = write 
	int	21h
	jc	short img_file_rw_err
	mov	bx,dx ; buffer address
	;xor	dx,dx
	cmp	ax,cx ; ax = actually written bytes
	jnb	short img_file_rw_ok
img_file_rw_err:
	mov	byte ptr [Error],0FFh
img_file_rw_ok:
	pop	ax ; * ; 24/01/2020
	retn

image_file_rw endp

sioreg proc near
	; 20/01/2020
	; 19/11/2019 - Retro UNIX 386 v2 (32 bit file offset)
	; 04/09/2019 - RETRO UNIX v0.3 (Retro UNIX 386 v2)
	; 16/12/2012
	; 31/10/2012
	; 19/08/2012
	; 04/08/2012
	; Erdogan Tan - RETRO UNIX v0.1
	; input -> R5 (DX) = sector buffer (data) address
	;	  *u.fofp = file offset, to start writing
	;	  u.base = address of 1st byte of user data
	;	  u.count = byte count to be transferred
	;	  u.nread = number of bytes written out
	;	    previously.
	; output -> *u.fofp = last (written) byte + 1
	;	  u.count = number of bytes of data left
	;	    to be transferred.		
	;	  u.nread = updated to include the count
	;	   of bytes to be transferred.
	;	  R1 (SI) = address of 1st byte of data
	;	  R2 (DI) = specifies the byte in IO 
	;	    sector (I/O) buffer. (Offset)	
	;	  R3 (CX) = number of bytes of data to be
	;	   transferred to/from sector (I/O)
	;	   buffer.

	; Modified registers: cx, si, di	

	;mov	dx,offset Buffer ; R5
	; 31/10/2012	
       ;mov	si,word ptr [u_fofp] ; mov *u.fofp,r2 
	; 29/12/2019
	mov	si,offset u_off

	mov	di,word ptr [si] ; file offset (in bytes) is moved to r2
	mov	cx,di	; mov r2,r3 / and also to r3

	or	cx,0FE00h ; set bits 9...15 of file offset in R3
	and	di,1FFh ; calculate file offset mod 512

	; 09/11/2019
	; 19/08/2012
	add	di,offset Buffer ; DI/r2 now points to 1st byte in buffer
		; where data is to be placed
        
	;mov	si,word ptr [u_base] ; address of data is in r1
	neg	cx ; 512 - file offset(mod512) in R3 (cx)
		   ; the number of free bytes in the file block
        cmp	cx,word ptr [u_count] ; compare this with the number of data bytes
		 		      ; to be written to the file
	jna	short @f ; 2f
		  ; if less than branch. Use the number of free bytes
	  	  ; in the file block as the number to be written
        mov	cx,word ptr [u_count]
		  ; if greater than, use the number of data bytes
	   	  ; as the number to be written
@@:	; 2
;sioreg_1:	
	add	word ptr [u_nread],cx ; r3 + number of bytes
		; xmitted during write is put into
	 	; u.nread

        sub	word ptr [u_count],cx
		; u.count = no. of bytes that still must be
	; written or read
	; 30/12/2019
	;;mov	si,word ptr [u_fofp]
        ;; 29/12/2019
	;mov	si,offset u_off

	add	word ptr [si],cx ; new file offset = number
				 ; of bytes done + old file offset
	; 09/11/2019
	adc	word ptr [si+2],0

	; Note: word ptr [u_base] + cx must not over 65535	

	; 16/12/2012 BugFix
	mov	si,word ptr [u_base] ; address of data is in SI/r1

        add	word ptr [u_base],cx ; u.base points to 1st of remaining
		    		     ; data bytes
	retn

sioreg	endp 

epoch proc near
	; 14/11/2012
	; unixboot.asm (boot file configuration)
	; version of "epoch" procedure in "unixproc.asm"
	; 21/7/2012
	; 15/7/2012
	; 14/7/2012	
	; Erdogan Tan - RETRO UNIX v0.1
	; compute current date and time as UNIX Epoch/Time
	; UNIX Epoch: seconds since 1/1/1970 00:00:00

	; 21/7/2012
	;push	bx
	;push	cx

	mov	ah, 02h		; Return Current Time
        int	1Ah
        xchg	ch, cl
        mov	word ptr [hour], cx
        xchg	dh, dl
        mov	word ptr [second], dx

        mov	ah, 04h		; Return Current Date
        int	1Ah
        xchg	ch, cl
        mov	word ptr [year], cx
        xchg	dh, dl
        mov	word ptr [month], dx

	mov	cx, 3030h
	
	mov	al, byte ptr [hour] ; Hour
	   	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	mov	byte ptr [hour], al

	mov	al, byte ptr [hour]+1 ; Minute
           	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	mov	byte ptr [minute], al

	mov	al, byte ptr [second] ; Second
            	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	mov	byte ptr [second], al
	
	mov	ax, word ptr [year] ; Year (century)
        push	ax
	   	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	mov	ah, 100
	mul	ah
	mov	word ptr [year], ax

	pop	ax
	mov	al, ah
           	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	add	word ptr [year], ax

	mov	al, byte ptr [month] ; Month
            	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL
	
	mov	byte ptr [month], al	

	mov	al, byte ptr [month]+1 ; Day
            	; AL <= BCD number)
        db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
        aad	; AX= AH*10+AL

	mov	byte ptr [Day], al
	
convert_to_epoch:
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998

	mov	dx, word ptr [year]
	sub	dx, 1970
	mov	ax, 365
	mul	dx
	xor	bh, bh
	mov	bl, byte ptr [month]
	dec	bl
	shl	bl, 1
	mov	cx, word ptr DMonth[BX]
	mov	bl, byte ptr [Day]
	dec	bl
	
	add	ax, cx
	adc	dx, 0
	add	ax, bx
	adc	dx, 0
		; DX:AX = days since 1/1/1970
	mov	cx, word ptr [year]
	sub	cx, 1969
	shr	cx, 1
	shr	cx, 1	
	; (year-1969)/4
	add	ax, cx
	adc	dx, 0
		; + leap days since 1/1/1970

	cmp	byte ptr [month], 2 ; if past february
	jna	short @f
	mov	cx, word ptr [year]
	and	cx, 3 ; year mod 4
	jnz	short @f	
		; and if leap year
	add	ax, 1 ; add this year's leap day (february 29)
	adc	dx, 0
@@: 		; compute seconds since 1/1/1970
	mov	bx, 24
	call	mul32

	mov	bl, byte ptr [hour]
	add	ax, bx
	adc	dx, 0
	
	mov	bx, 60
	call	mul32

	mov	bl, byte ptr [minute]
	add	ax, bx
	adc	dx, 0
	
	mov	bx, 60
	call	mul32

	mov	bl, byte ptr [second]
	add	ax, bx
 	adc	dx, 0

	; DX:AX -> seconds since 1/1/1970 00:00:00

	; 21/7/2012
	;pop	cx
	;pop	bx
	
	retn

epoch endp

convert_from_epoch proc near
	; 30/11/2012
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	;
	; INPUT:
	; DX:AX = Unix (Epoch) Time
	mov	cx, 60
	call	div32
	;mov	word ptr [imin], ax   ; whole minutes
	;mov	word ptr [imin]+2, dx ; since 1/1/1970
	mov	word ptr [second], bx ; leftover seconds
	;mov	cx, 60
	call	div32
	;mov	word ptr [ihrs], ax   ; whole hours
	;mov	word ptr [ihrs]+2, dx ; since 1/1/1970
	mov	word ptr [minute], bx ; leftover minutes
	;mov	cx, 24
	mov	cl, 24
	call	div32
	;mov	word ptr [iday], ax ; whole hours
		   ; since 1/1/1970
	;mov	word ptr [iday]+2, dx ; DX = 0
	mov	word ptr [hour], bx   ; leftover hours
	add	ax, 365+366	 ; whole day since
		 ; 1/1/1968 	
	;adc	dx, 0	;  DX = 0
	;mov	word ptr [iday], ax
	push	ax
	mov	cx, (4*365)+1	; 4 years = 1461 days
	call	div32
	pop	cx
	;mov	word ptr [lday], ax ; count of quadyrs (4 years)
	push	bx
	;mov	word ptr [qday], bx ; days since quadyr began
	cmp	bx, 31 + 29	; if past feb 29 then
	cmc		 	; add this quadyr's leap day
	adc	ax, 0	 	; to # of qadyrs (leap days)
	;mov	word ptr [lday], ax ; since 1968		 
	;mov	cx, word ptr [iday]
	xchg	cx, ax	 ; CX = lday, AX = iday	 
	sub	ax, cx	 ; iday - lday
	mov	cx, 365
	;xor	dx, dx	 ; DX  = 0
	; AX = iday-lday, DX = 0
	call	div32
	;mov	word ptr [iyrs], ax ; whole years since 1968
	; jday = iday - (iyrs*365) - lday
	;mov	word ptr [jday], bx ; days since 1/1 of current year
	add	ax, 1968	 ; compute year
	mov	word ptr [year], ax
	mov	dx, ax	
	;mov	ax, word ptr [qday]
	pop	ax
	cmp	ax, 365		; if qday <= 365 and qday >= 60	
	ja	short @f	; jday = jday +1
	cmp	ax, 60		; if past 2/29 and leap year then
        cmc			; add a leap day to the # of whole
	adc	bx, 0		; days since 1/1 of current year
@@:		
	;mov	word ptr [jday], bx
	mov	cx, 12		; estimate month
	xchg	cx, bx		; CX = jday, BX = month 	
	mov	ax, 366		; mday, max. days since 1/1 is 365
	and	dx, 11b		; year mod 4	(and dx, 3) 
@@:	; Month calculation	; 0 to 11  (11 to 0)	
	cmp	cx, ax		; mday = # of days passed from 1/1
	jnb	short @f
	dec	bx	 ; month = month - 1
	shl	bx, 1 
	mov	ax, word ptr DMonth[BX] ; # elapsed days at 1st of month
	shr	bx, 1	 ; bx = month - 1 (0 to 11)
	cmp	bx, 1	 	; if month > 2 and year mod 4  = 0	
	jna	short @b	; then mday = mday + 1
	or	dl, dl		; if past 2/29 and leap year then
	jnz	short @b	; add leap day (to mday)
	inc	ax	 ; mday = mday + 1
	jmp	short @b
@@:
	inc	bx 	 ; -> bx = month, 1 to 12
	mov	word ptr [month], bx
	sub	cx, ax	 ; day = jday - mday + 1	
	inc	cx 		 
	mov	word ptr [day], cx
	
	; ax, bx, cx, dx is changed at return
	; output ->
	; [year], [month], [day], [hour], [minute], [second]
	; 

	retn

convert_from_epoch endp

mul32	proc near

	;push	cx

	mov	cx, bx
	mov	bx, dx

	mul	cx

	xchg	ax, bx

	push	dx

	mul	cx 

	pop	cx 

	add	ax, cx 
	adc	dx, 0

	xchg	bx, ax
	xchg	dx, bx

	;pop	cx

	retn

mul32	endp

div32	proc near
	; 1999
	; (Rx_Dos_Div32) 32 bit divide procedure 
	; by Erdogan Tan
	; Input -> DX_AX = 32 bit dividend
	;          CX = 16 bit divisor
	; output -> DX_AX = 32 bit quotient
	;          BX = 16 bit remainder
	mov  bx, dx
	xchg ax, bx
	xor  dx, dx
	div  cx         ; at first, divide DX
	xchg ax, bx     ; remainder is in DX
			; now, BX has quotient
			; save remainder
	div  cx         ; so, DX_AX divided and
			; AX has quotient
			; DX has remainder
	xchg dx, bx     ; finally, BX has remainder
sync_0:	; 19/09/2019
	retn

div32	endp

sync 	proc near
	; 24/01/2020
	; 23/01/2020
	; 22/01/2020
	; 28/09/2019
	; 18/09/2019 - Retro UNIX 386 v2 (RUNIX v2 file system)
	;
	; 14/07/2015
	; 07/07/2015
	; 18/11/2012 unix boot file configuration version
	; of "sync" procedure of retro unix v1.0 by Erdogan Tan
	; 12/8/2012
	; updates superblock and the last i-node on disk 
	; if modified
	; e.g. smod = 1, imod = 1, buffer_m = 1
	;
	; RETRO UNIX v1 FS

	xor	ax, ax ; mov ax, 0
	call	iget ; (write modified i-node)
	jc	short sync_0 ; 19/09/2019

	; 22/01/2020
	cmp	byte ptr [buff_m], 0
	jna	short sync_1 ; 23/01/2020

	mov	ax, word ptr [buff_s]
	mov	bx, offset Buffer
	call	dskwr
	jc	short sync_0

	mov	byte ptr [buff_m], 0
sync_1:
	; 29/09/2019
	; set superblock modification time
	call	epoch
	mov	word ptr [systm.sb_ModifTime],ax
	mov	word ptr [systm.sb_ModifTime+2],dx

	; 29/09/2019
	; set first free inode number and first free block number
	; if they are invalid (in sb)

	mov	dx, 0FFFFh
	cmp	word ptr [systm.sb_FirstFreeIno], dx ; 0FFFFh
	jne	short @f
	cmp	word ptr [systm.sb_FirstFreeIno+2], dx ; 0FFFFh	
	jne	short @f
	call	set_firstfreeinode	
@@:
	cmp	word ptr [systm.sb_FirstFreeBlk], dx ; 0FFFFh
	jne	short @f
	cmp	word ptr [systm.sb_FirstFreeBlk+2], dx ; 0FFFFh	
	jne	short @f
	call	set_firstfreeblock
@@:	
        ; 14/07/2015
	; 07/07/2015
	mov	dl, byte ptr [PhysicalDriveNumber]
	cmp	dl, 90h
	;jb	short sync_2 ; physical (real) disk
	; 22/01/2020
	jnb	short @f
	jmp	sync_2
@@:
	; 22/01/2020
	sub	dx, dx
	mov	cx, dx

	mov	si, offset BSBuffer	
	mov	di, 1024

	cmp	byte ptr [bmod], 0
	ja	short @f

	shr	di, 1 ; 512 

	mov	dx, di ; mov dx, 512 ; 24/01/2020
	;add	si, 512
	add	si, dx ; add si, 512 ; offset systm
	; cx:dx = file pointer = 512
@@:
	; disk image file (virtual disk)
	;sub	dx, dx ; 0
	;mov	cx, dx ; 0
	sub	al, al ; specified offset is from the beginning of the file
	mov	ah, 42h ; seek (move file pointer)	
	mov	bx, word ptr [img_file_handle]
	int	21h
        ;jc	loc_error
	; 27/03/2022
	jc	short sync_err

	mov	cx, di ; byte count (512 or 1024)
	mov	dx, si ; buffer address (BSBuffer or systm)

	;mov	bx, word ptr [img_file_handle]
        ;mov	cx, 1024 ; write 1024 bytes (2 sectors)
	;mov	dx, offset BSBuffer ; bootsector (& superblock) buffer
	mov	ah, 40h ; write file
	int	21h
	;jc	loc_error
	; 27/03/2022
	jc	short sync_err

	; 24/01/2020
	cmp	ax, cx ; 512 or 1024
        ;jne	loc_error
	; 27/03/2022
	jne	short sync_err

        ;mov	bx, dx ; offset BSBuffer
	;retn

	; 18/09/2019 - Retro UNIX 386 v2
	; (following code is only valid for floppy disks)
	; (single sector free block map and inode map, no 'hidden sectors')
	; (NOTE: modified inodes are already written by 'iget' procedure)

	; 23/01/2020
	mov	dx, word ptr [systm.sb_InodeMapAddr]
	xor	ax, ax
	mov	cx, 9 ; 28/09/2019
	; ax:dx = 32 bit sector address	
	call	shl32
	mov	cx, ax
	; cx:dx = 32 bit (byte) offset	

	sub	al, al ; specified offset is from the beginning of the file
	mov	ah, 42h ; seek (move file pointer)	
	;mov	bx, word ptr [img_file_handle]
	int	21h
	;jc	loc_error
	; 27/03/2022
	jc	short sync_err

	mov	dx, offset im_buffer ; inode map 
	mov	cx, 512  ; 1 sector
	;mov	bx, word ptr [img_file_handle]
	mov	ah, 40h ; write file
	int	21h
        ;jc	loc_error
	; 27/03/2022
	jnc	short @f
sync_err:
	jmp	loc_error
@@:
	cmp	ax, 512
	;jne	loc_error
	jne	short sync_err

	; 23/01/2020
	mov	dx, word ptr [systm.sb_FreeMapAddr]
	xor	ax, ax
	mov	cx, 9 ; 28/09/2019
	; ax:dx = 32 bit sector address	
	call	shl32
	mov cx, ax
	; cx:dx = 32 bit (byte) offset	

	sub	al, al ; specified offset is from the beginning of the file
	mov	ah, 42h ; seek (move file pointer)	
	;mov	bx, word ptr [img_file_handle]
	int	21h
       	;jc	loc_error
	; 27/03/2022
	jc	short sync_err

	mov	dx, offset fbm_buffer ; free blocks map 
	mov	cx, 512  ; 1 sector
	;mov	bx, word ptr [img_file_handle]
	mov	ah, 40h ; write file
	int	21h
        ;jc	loc_error
	; 27/03/2022
	jc	short sync_err

	cmp	ax, 512
        ;jne	loc_error
	; 27/03/2022
	jne	short sync_err

	retn

sync_2:	
	; 22/01/2020
	mov	bx, offset systm

       	mov	ax, 0301h  ; Write superblock (only)
       	mov	cx, 2      ; (sector to be written, superblock)  	

	cmp	byte ptr [bmod], 0
	jna	short @f

	inc	al ; ax = 0302h ; Write boot sector & superblock
	dec	cl ; cx = 1	; (1st sector to be written,boot sector)  
	
	mov	byte ptr [bmod], 0 ; reset

	;sub	bx, 512

	mov	bx, offset BSBuffer
@@:	
	; 22/01/2020
	;mov	bx, offset BSBuffer
	;mov	ax, 0302h  ; Write boot sector & superblock
	;mov	cx, 1
        xor	dh, dh

       ;mov	dl, byte ptr [PhysicalDriveNumber]
       
	int	13h
	jc	short sync_3

	; 18/09/2019 - Retro UNIX 386 v2
	; (following code is only valid for floppy disks)
	; (single sector free block map and inode map, no 'hidden sectors')
	; (NOTE: modified inodes are already written by 'iget' procedure)

	mov	bx, offset im_buffer ; inode map 
	mov	ax, word ptr [systm.sb_InodeMapAddr]
	call	dskwr
	jc	short sync_3

	mov	bx, offset fbm_buffer  ; free block map
	mov	ax, word ptr [systm.sb_FreeMapAddr]
	call	dskwr
sync_3:
        retn	

sync	endp

shl32	proc near
	; 23/01/2020
	; 18/09/2019
	; INPUT:
	;   CL = shift count
	;   ax:dx = 32 bit number to be shifted to left
	;xor	ch,ch
	jcxz	norotal
rotashftl:
	shl	dx,1
	rcl	ax,1
	loop	rotashftl
norotal:
	retn

shl32	endp

shr32	proc near	
	; 22/01/2020
	; INPUT:
	;   CL = shift count
	;   dx:ax = 32 bit number to be shifted to right
	;xor	ch,ch
	jcxz	norotar
rotashftr:
	shr	dx,1
	rcr	ax,1
	loop	rotashftr
norotar:
	retn

shr32	endp

set_firstfreeinode proc near
	; 23/03/2022 (BugFix)
	; 11/12/2021
	; 29/09/2019 - Retro UNIX 386 v2
	; set first free inode number in sb
	; (note: this procedure will also set free inodes again) 

	mov	si,offset im_buffer
	mov	cx,word ptr [systm.sb_InodeMapSize]
	xor	dx,dx
	xor	bx,bx
sffi_1:
	push	cx
	lodsb
	mov	cl,8
sffi_2:
	inc	bx ; 23/03/2022 (BugFix) (1 based inode number)
	shr	al,1
	jc	short sffi_4 ; 1 = (inode) allocated

	and	dx,dx
	jnz	short sffi_3
	mov	word ptr [systm.sb_FirstFreeIno],bx
	; 11/12/2021
	mov	word ptr [systm.sb_FirstFreeIno+2],dx ; 0
sffi_3:
	inc	dx
sffi_4:
	;inc	bx
	loop	sffi_2
	pop	cx
	loop	sffi_1
sffi_5:
	mov	word ptr [systm.sb_FreeInodes],dx
	retn

set_firstfreeinode endp

set_firstfreeblock proc near
	; 11/12/2021
	; 29/09/2019 - Retro UNIX 386 v2
	; set first free block number in sb
	; (note: this procedure will also set free blocks again)

	mov	si,offset fbm_buffer
	mov	cx,word ptr [systm.sb_FreeMapSize]
	xor	dx,dx
	xor	bx,bx
sffb_1:
	push	cx
	lodsb
	mov	cl,8
sffb_2:
	shr	al,1
	jnc	short sffb_4  ; 0 = (block) allocated

	and	dx,dx
	jnz	short sffb_3
	mov	word ptr [systm.sb_FirstFreeBlk],bx
	; 11/12/2021
	mov	word ptr [systm.sb_FirstFreeBlk+2],dx ; 0
sffb_3:
	inc	dx
sffb_4:
	inc	bx
	loop	sffb_2
	pop	cx
	loop	sffb_1
sffb_5:
	mov	word ptr [systm.sb_FreeBlocks],dx
	retn

set_firstfreeblock endp

find_bfn proc near
	; 12/12/2021
	; 22/01/2020
	; 20/01/2020
	; 18/09/2019 - Retro UNIX 386 v2 (modified unix v7 inodes)
	;
	; 04/12/2015 (14 byte file names)
	; 26/11/2012
	; 25/11/2012
	;
	; find boot file name by i-number (ax)
	;
	; cf -> 1 means error, ax = 0 -> not found

	mov	word ptr [uf_i_number], ax
	push	si

	mov	ax, ROOT_DIR_INODE_NUMBER ; 41 (unix v1), 1 (runix v2)
	call	iget
	jc	short find_bfn_retn
	
	;test	word ptr [inode_flgs], 4000h ; directory i-node ?
	;jnz	short @f

	;mov	ah, 0FFh ; error number
	;stc
	;jmp	short find_bfn_retn
;;@@:
	;xor	ax, ax 
	;mov	word ptr [u_off], ax ; u_off is file offset used by user
	;; 20/01/2020
	;mov	word ptr [u_off+2], ax
	; 22/01/2020
	xor	dx, dx
	mov	word ptr [u_off], dx ; 0
	mov	word ptr [u_off+2], dx ; 0
	
	; ax = ROOT_DIR_INODE_NUMBER = 1

find_bfn_1:
;find_bfn_readi:
	mov	word ptr [u_base], offset u_dirbuf
			; u.dirbuff holds a file name copied from a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 	
 	
	; 22/01/2020
	;mov	ax, ROOT_DIR_INODE_NUMBER ; = 1 ; 18/09/2019

	call	readi ; read 16 bytes of file with i-number
	  	      ; i.e. read a directory entry
	jc	short find_bfn_retn

	;mov	ax, word ptr [u_nread]

	;or	ax, ax
	;jz	short find_bfn_3 ; gives error return 
	; 22/01/2020
	cmp	word ptr [u_nread], 16
	jb	short find_bfn_retn ; cf = 1 ; error

	mov	ax, word ptr [u_dirbuf]
	; 12/12/2021
	cmp	ax, word ptr [uf_i_number] ; Check i-number of directory entry
	je	short find_bfn_2  ; if same with specified uf_i_number
		   		  ; it is the boot file 
	mov	ax, ROOT_DIR_INODE_NUMBER ; = 1
	jmp	short find_bfn_1
find_bfn_2:
	call	iget
find_bfn_retn:
	pop	si
	retn

;find_bfn_3:
;	stc
;	jmp	short find_bfn_retn
	
find_bfn endp

proc_display_startupfile_info proc near
	; 24/09/2019 - Retro UNIX 386 v2 (modified Unix v7 inode format)
	; 30/11/2012	
	; 29/11/2012 ; @@
	; 25/11/2012
              
 	mov	si, offset Msg_StartupFile_Name
	call	PRINT_MSG

	mov	si, offset Boot_File_Name
	call	PRINT_MSG

	mov	si, offset Str_Inode_Number
	call	PRINT_MSG

	mov	si, offset BSBuffer
	mov	ax, word ptr [SI]+bs_bf_inode_num

	mov	si, offset Decimal_i_no_str
	mov	cx, 5
	call	proc_bin_to_decimal

	mov	si, offset Decimal_i_no_str 

	mov	cx, 4
@@:
	cmp	byte ptr [si], '0'
	ja	short @f
	inc	si
	loop	@b 
@@:
	call	PRINT_MSG

	mov	si, offset Str_startup_file_size
	call	PRINT_MSG

	mov	ax, word ptr [Inode_size]
	mov	si, offset Decimal_size_str
	;mov	cx, 5
	mov	cl, 5
	call	proc_bin_to_decimal

	mov	si, offset Decimal_size_str

	mov	cl, 4
@@:
	cmp	byte ptr [si], '0'
	ja	short @f
	inc	si
	loop	@b 
@@:
	call	PRINT_MSG

	mov	si, offset Str_Bytes
	call	PRINT_MSG

	; 24/09/2019
	
	mov	ax, word ptr [Inode_atim]
	mov	dx, word ptr [Inode_atim]+2

	call	convert_from_epoch
	
	mov	ax, word ptr [year]
	mov	si, offset str_ayear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	
	mov	ax, word ptr [month]
	mov	si, offset str_amonth
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [day]
	mov	si, offset str_aday
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [hour]
	mov	si, offset str_ahour
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [minute]
	mov	si, offset str_aminute
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [second]
	mov	si, offset str_asecond
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [Inode_mtim]
	mov	dx, word ptr [Inode_mtim]+2

	call	convert_from_epoch
	
	mov	ax, word ptr [year]
	mov	si, offset str_myear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	
	mov	ax, word ptr [month]
	mov	si, offset str_mmonth
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [day]
	mov	si, offset str_mday
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [hour]
	mov	si, offset str_mhour
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [minute]
	mov	si, offset str_mminute
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [second]
	mov	si, offset str_msecond
	mov	cl, 2
	call	proc_bin_to_decimal

	; 30/11/2012
	
	mov	ax, word ptr [Inode_ctim]
	mov	dx, word ptr [Inode_ctim]+2

	call	convert_from_epoch
	
	mov	ax, word ptr [year]
	mov	si, offset str_cyear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	
	mov	ax, word ptr [month]
	mov	si, offset str_cmonth
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [day]
	mov	si, offset str_cday
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [hour]
	mov	si, offset str_chour
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [minute]
	mov	si, offset str_cminute
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	ax, word ptr [second]
	mov	si, offset str_csecond
	mov	cl, 2
	call	proc_bin_to_decimal

	mov	si, offset Str_SF_date_Time
	call	PRINT_MSG
	
	retn  

proc_display_startupfile_info endp

proc_bin_to_decimal proc near
	; 30/11/2012 (CX input)	
	; 25/11/2012 unixboot.asm version	
	; 6-5-2009
	;  Erdogan Tan
	; INPUT: DS:SI = Target location
	;        AX = Binary Number
	;        CX = Number of digits	
	; OUTPUT: Decimal chars at DS:SI
	; CX, AX, DX will be changed.

	;push	bp
	;push	si
loc_reset_str_NumberInput:
	mov	byte ptr [si], "0"
	inc	si
	loop	loc_reset_str_NumberInput
	mov	bp, sp
	xor	dx, dx
	mov	cx, 10
loc_rediv_NumberInput:
	div	cx
	add	dl,'0'
	push	dx
	xor	dx, dx
	dec	si
	or	ax, ax
	jnz	short loc_rediv_NumberInput
loop_popcx_NumberInput: 
	pop	dx
	mov	byte ptr [si], dl
	inc	si
	cmp	bp, sp
	jne	short loop_popcx_NumberInput
	;pop	si
	;pop	bp  
 
	retn

proc_bin_to_decimal endp

imap	proc near
	; 22/01/2020
	; 25/09/2019
	; 18/09/2019 - Retro UNIX 386 v2 (modified unix v7 inodes)
	; 	(only for floppy disks, without 'hidden sectors')  
        ; 02/12/2012
        ; unix boot file configuration version
	; of "imap" procedure of retro unix v1.0 by Erdogan Tan
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972 
	; ('imap' procedure)

	; 18/09/2019
	cmp	ax,word ptr [systm.sb_InodeCount]
	jna	short @f
	; Temporary (set inode number to the last inode)
	mov	ax,word ptr [systm.sb_InodeCount]
@@:
	; 11/11/2012 (maknod_imap location -> imap procedure)
        mov	bx,ax	; BX = R2, AX = R1 (input, i-number)
	;sub	bx,41   ; BX has i-41

        mov	dx,1	; bit 0 = 1
   
	dec	bx	; 18/09/2019 - Retro UNIX 386 V2 	
		; (root directory inode number = 1)
	jnz	short @f ; inode map offset > 0

	; 25/09/2019
	mov	bx, offset im_buffer 
	
	retn
@@:	  
	;mov	dx,1
	mov	cl,bl   ; CX = R3   
	and	cl,7    ; CX has (i-41) mod 8 to get the bit position 		
	;jz	short @f ; 21/8/2012
	;shl	dx,cl   ; DX has 1 in the calculated bit position
	shl	dl,cl ; 22/01/2020
@@:
        shr	bx,1
        shr	bx,1
        shr	bx,1	; BX has (i-41) base 8 of byte number
		; from the start of the (inode) map	

	;add	bx,word ptr [systm] ; superblock free map size + 4
	;add	bx,offset systm + 4 ; is inode map offset in superblock

	; Retro UNIX 386 v2
	add	bx, offset im_buffer ; 25/09/2019 

        ; DX (MQ) has a 1 in the calculated bit position
        ; BX (R2) has byte address of the byte with allocation bit

	retn

imap	endp

; UNIX v7, x86 - iget.c (Robert Nordier, 1999)

itrunc proc near
	; 22/01/2020 ('itrunc' for Retro UNIX v2 bootable floppy disks)
	; 30/09/2019
	; 18/09/2019
	; 17/09/2019 - Retro UNIX v2
	; * Free all the disk blocks associated
	; * with the specified inode structure.
	;
	; (Simplified for Retro UNIX v2 bootable floppy disk...)
	; (< 32MB disks)

	; INPUT: 
	;	AX = inode number
	; OUTPUT: 
	;	inode will be modified (file size = 0)

	; 30/09/2019
	call 	iget ; jsr r0,iget
	jc	short itrunc_9 ; 30/09/2019

	test	byte ptr [inode_flgs+1],80h ; regular file or directory	
	jz	short itrunc_10 ; 22/01/2020

itrunc_1:
	test	byte ptr [inode_flgs+1],10h ; large file (indirect blocks)
	jnz	short itrunc_4
	mov	si,offset inode_dskp
itrunc_2:
	lodsw	
	and	ax,ax
	jz	short itrunc_3
	call	free
	; 30/09/2019
	xor	ax,ax
	mov	word ptr [si-2],ax ; 0
	inc	si
	inc	si
	cmp	si,offset inode_dskp+40
	jb	short itrunc_2
	; 30/09/2019
	xor	ax,ax
itrunc_3:
	;xor	ax,ax
	mov	word ptr [inode_size],ax ; 0
	mov	word ptr [inode_size+2],ax ; 0
	
	; 30/09/2019
	; Clear inode block addresses
	;mov	cx,20
	;mov	di,offset inode_dskp
	;rep	stosw

	; 30/09/2019
	; clear large file flag
	and     byte ptr [inode_flgs+1],0EFh ; 11101111b ;  not 10h
	
	;call	setimod
	;retn
	jmp	setimod

itrunc_4:
	; free disk blocks by using triple indirect blocks at first
	;mov	ax,word ptr [inode_dskp+36]
	; 30/09/2019
	mov	si,offset inode_dskp+36
	mov	ax,word ptr [si]
	;
	and	ax,ax
	jz	short itrunc_5 ; pass to double indirect blocks

	;mov	bx,2 ; Triple indirect sign
	;push	bx
	;push	ax
	;call	tloop
	;jc	short itrunc_0
	;; 30/09/2019
	;mov	word ptr [si],0
	;;mov	word ptr [si+2],0

	; 22/01/2020
	; Triple indirect block pointer must be 0 for 1.44MB fd

itrunc_10:
	stc ; error
itrunc_0:
	mov	ax,0FFFFh
itrunc_9:
	retn
 
itrunc_5:
	; free disk blocks by using double indirect blocks at second
	;mov	ax,word ptr [inode_dskp+32]
	; 30/09/2019
	mov	si,offset inode_dskp+32
	mov	ax,word ptr [si]
	;
	and	ax,ax
	jz	short itrunc_6 ; pass to single indirect blocks
	mov	bx,1 ; Double indirect sign
	push	bx
	push	ax
	call	tloop
	jc	short itrunc_0
	; 30/09/2019
	mov	word ptr [si],0
	;mov	word ptr [si+2],0
itrunc_6:
	; free disk blocks by using single indirect blocks at third
	;mov	si,offset inode_dskp+28
	; 30/09/2019
	; si = offset inode_dskp+32
	;mov	si,offset inode_dskp+32
itrunc_7:
	sub	si,4
	mov	ax,word ptr [si]
	and	ax,ax
	jz	short itrunc_8 ; skip
	sub	bx,bx ; 0  ; Single indirect sign
	push	bx
	push	ax
	call	tloop
	jc	short itrunc_0
	; 30/09/2019
	mov	word ptr [si],0
	;mov	word ptr [si+2],0
itrunc_8:
	;sub	si,4	
	cmp	si,offset inode_dskp
	;jnb	short itrunc_7
	ja	short itrunc_7
	xor	ax,ax
	jmp	short itrunc_3
	
itrunc endp

tloop proc near
	; 22/01/2020 ('tloop' for Retro UNIX v2 bootable floppy disks)
	; 30/09/2019
	; 18/09/2019 - Retro UNIX v2
	; * Free all the disk blocks associated
	; * with the specified inode structure.
	;
	; (Simplified for Retro UNIX v2 bootable floppy disk...)

	; INPUT: 
	;	sp+2 = ax, indirect block number (sector address)
	;	sp+4 = bx, levels 
	;	(0 = indirect, 1 = double indr. 2 = triple indr.) 	
	
	; OUTPUT: indirect blocks will be released

	pop	cx ; return address

	pop	ax ; ++ indirect block number
	pop	bx ; + levels  ; 0 = single, 1 = double, 2 = triple 
		; 22/01/2020
		; Note: triple indirect block addressing 
		; is not applicable for 1.44MB floppy disks.
	; ( Double indirect limit = (8*64KB)+(128*64KB) = 8.5MB )
	
	push	cx ; near call return address

	push	di ; 30/09/2019
	push	si ; 30/09/2019

	push	bp ; **
	mov	bp,ax ;*

	;call	bread
	push	bx ; 30/09/2019
	mov	bx,offset WriteBuffer ; 22/01/2020
	call	dskrd
	pop	bx ; 30/09/2019
	jc	short tloop_7
	; ax = block/sector number/address
	mov	si,offset WriteBuffer ; 24/09/2019
	cmp	bx,1
	jb	short tloop_4
	; 22/01/2020
	;je	short tloop_1
	;mov	di,offset trpi_buffer
	;jmp	short tloop_2
tloop_1:
	mov	di,offset dbli_buffer
tloop_2:	
 	mov	cx,256
	push	di
	rep	movsw
	mov	cl,128
	pop	si
	dec	bx ; 2 -> 1, 1 -> 0
	; 30/09/2019
	inc 	di
	inc	di
tloop_3:
	sub	di,4
	;push	di
	;push	si
	push	bx
	push	cx	
	mov	ax,word ptr [di]
	push	bx ;+
	push	ax ;++
	call	tloop
	pop	cx
	pop	bx
	;pop	si
	;pop	di
	jc	short tloop_7
	loop	tloop_3

	; 22/01/2020
	jmp	short tloop_8
	
	; free blocks in current indirect block
tloop_4:
	mov	cx,128
	add	si,508 ; 30/09/2019
tloop_5:
	mov	ax,word ptr [si]
	or	ax,ax
	jz	short tloop_6
	push	cx ; 30/09/2019
	call	free
	pop	cx ; 30/09/2019
tloop_6:
	sub	si,4
	loop	tloop_5
tloop_8:	; 22/01/2020
	mov	ax,bp ;* ; free indirect block's itself
	call	free
tloop_7:
	pop	bp ; **
	pop	si ; 30/09/2019
	pop	di ; 30/09/2019
	retn

tloop endp

chmode  proc near
	; 02/10/2019
	; 01/10/2019
	; 10/09/2019 - Retro UNIX 386 v2
	; 13/01/2013
	; 'change mode' procedure
	; Format: chmod <octal number> <unix file name>
	;
	; output -> cf=1 -> error
	; 	-> cf=0 -> word ptr [arg] > 0 -> mode (string, 2 chars)
	;	   word ptr [arg] = 0 -> ignored (none is done)	
        
	xor	ax, ax
	mov	di, offset arg
	mov	word ptr [di], ax ; 0  ; reset
	mov	byte ptr [di+2], al ; 0 ; 01/10/2019
	xor	dx, dx ; 0
chmode_1:
	lodsb
	cmp	al, '0'
	jb	short chmode_3
	cmp	al, '7'
	ja	short chmode_stc_retn

	; 02/10/2019
	sub	al,'0'
	stosb	
	inc	dl
	cmp	dl, 3
	jnb	short chmode_2 ; 01/10/2019
	jmp	short chmode_1

chmode_stc_retn:
	cmc
chmode_retn:
	retn

chmode_2:
	; 01/10/2019
	lodsb
chmode_3:
	or	dl, dl
	jz	short chmode_stc_retn ; zero access rights or invalid !
	cmp	al, 20h
	jne	short chmode_stc_retn ; there is not a file name argument!
chmode_4:
	cmp	byte ptr [si], al ; 20h
	; no error if the 4th character is a space
	ja	short chmode_5
	jb	short chmode_stc_retn ; there is not a file name argument!
	inc	si
	jmp	short chmode_4 
chmode_5:
	; 02/10/2019
	mov	ax, '00'
	mov	bl, al
	xchg	ax, word ptr [arg]
	xchg	bl, byte ptr [arg+2]	
	; owner's permissions always must be more than group and others 
	or	ah, bl ; group's permissions must be more than others
   	or	al, ah
	add	word ptr [arg], ax
	add	byte ptr [arg+2], bl
	mov	dl, al
	shl	dl, 1
	shl	dl, 1
	shl	dl, 1
	or	dl, ah
	shl	dl, 1
	shl	dl, 1
	shl	dx, 1
	or	dl, bl
	mov	bp, dx  		
chmode_6:
	mov	word ptr [u_namep], si ; 02/10/2019
	;call	namei
	call	namei_x ; 02/10/2019	
	jc	short chmode_retn

	; ax = i-number
	call	iget
	jc	short chmode_retn

	; 02/10/2019
	mov	ax, bp
	;and	ax, 1FFh

	mov	bx, offset inode_flgs
	; 01/10/2019
	mov	dx, word ptr [bx]
	and	dx, 0FE00h ; clear permission bits

	or	ax, dx  ; combine other feature bits with permission bits 
chmode_7:
	mov	word ptr [bx], ax
	mov	byte ptr [imod], 1
	; 19/09/2019
	mov	byte ptr [imodx], 1 ; 19/09/2019 - Retro UNIX 386 v2
		; (flag means file data is same
		;  but inode's itself is modified)
	jmp	short chmode_retn	

chmode  endp 

chgroup proc near
	; 10/09/2019 - Retro UNIX 386 v2
	; 'change group' procedure
	; Format: chgrp <decimal number> <unix file name>
	;
	; output -> cf=1 -> error
	; 	-> cf=0 -> 
	; 	   BX > 0 -> offset arg == group (decimal string, 3 chars)
	;	   BX = 0 -> ignored (none is done)

	mov	byte ptr [chg], 1  ; change group sign ; 1 = change group
	jmp	short cg_og

chg:	db 0FFh ; Change signature/flag

chgroup endp

chowner proc near
	; 01/10/2019
	; 10/09/2019 - Retro UNIX 386 v2
	; 13/01/2013
	; 'change owner' procedure
	; Format: chown <decimal number> <unix file name>
	;
	; output -> cf=1 -> error
	; 	-> cf=0 -> 
	; 	   BX > 0 -> offset arg == owner (decimal string, 3 chars)
	; 	   BX = 0 -> ignored (none is done)	

	mov	byte ptr [chg], 0  ; change group sign ; 0 = change owner
cg_og:
	xor	ax, ax
	mov	di, offset arg
	mov	word ptr [di], ax ; 0
	mov	byte ptr [di+2], al ; 0 ; 01/10/2019
	xor	bx, bx
	;mov	cx, 3
	mov	cx, '90'
	;xor	dx, dx
	xor	dl, dl
chowner_1:
	lodsb
	cmp	al, cl ; '0'
	jb	short chowner_5
	cmp	al, ch ; '9'
	ja	short chowner_stc_retn ; cmc
	inc	dl
chowner_2:
	or	bl, bl
	jnz	short chowner_3

	cmp	al, cl ; '0'
	je	short chowner_1
	jmp	short chowner_4

chowner_3:
	push	ax
	mov	al, 10
	mul	bl
	mov	bx, ax
	pop	ax
chowner_4:
	sub	al, cl ; '0'
	add	bx, ax
	or	bh, bh
	jz	short chowner_7
	xor	bx, bx

chowner_stc_retn:
	cmc
	retn	

chowner_5:
	and	dl, dl
	jz	short chowner_retn	
	cmp	al, 20h
	je	short chowner_8
chowner_6:
	mov	bx, 0
	jmp	short chowner_stc_retn

chowner_7:
	add	al, cl ;'0'
	stosb
	jmp	short chowner_1
	;loop	chowner_1
	;cmp	byte ptr [SI], 20h
	;; no error if the 4th character is a carriage return
	;jne	short chowner_6

	;inc	si
chowner_8:
	mov	word ptr [u_namep], si
	lodsb
	cmp	al, 20h
	je	short chowner_8
	jb	short chowner_6 ; no error (carriage return)

	;mov	byte ptr [u_uid], bl
	push	bx
	call	namei
	jc	short chowner_9

	; ax = i-number
	call	iget

chowner_9:
	;pushf
	;mov	bl, byte ptr [u_uid]
	;xor	bh, bh
	;mov	byte ptr [u_uid], bh ; 0
	;popf
	pop	bx
	jc	short chowner_retn

	; 10/09/2019 - Retro UNIX 386 v2
	;mov	byte ptr [inode_uid], bl
	cmp	byte ptr [chg],0
	jna	short chowner_10 ; change user ID
	; change group ID	
	mov	byte ptr [inode_gid], bl
	jmp	short chowner_11	
chowner_10:
	mov	byte ptr [inode_uid], bl
chowner_11:	;
	mov	byte ptr [imod], 1
	; 19/09/2019
	mov	byte ptr [imodx], 1 ; 19/09/2019 - Retro UNIX 386 v2
		; (flag means file data is same
		;  but inode's itself is modified)
	or	 bl, bl
	jnz	short chowner_retn

	mov	bh, '0'
	mov	byte ptr [arg], bh 

chowner_retn:	
	retn
		
chowner endp

print_file_size proc near
	; 12/12/2021
	; 22/01/2020
	mov	ax, word ptr [inode_size]
	mov	dx, word ptr [inode_size+2]
	;mov	cx, 11
	mov	cl, 11
	jmp	short pdn0

print_file_size endp

print_decimal_number proc near
	; 12/12/2021
	; 22/01/2020 (32 bit binary number in dx:ax)
	; 03/02/2013
	; 21/01/2013 
	; print decimal number
	;
	; INPUT -> AX = Integer
	; 32/02/2013 CX = Number of decimal digits
	; OUTPUT -> decimal number as string

	xor	dx,dx ; set high word of binary number to zero
pdn0:
	; 15/01/2020
	;mov	si, offset dec_num
	;
	;mov	bx, si
	;dec	si ; 29/12/2019
	;add	si, cx ; 03/02/2013
	;mov	di, si
	;;mov	cx, 10
	;mov	cl, 10
	;mov	dl, '0'
;@@: 
	;mov	byte ptr [bx], dl
	;inc	bx
	;loop	@b
	;
	;xor	dl, dl
	;mov	byte ptr [bx], dl

	; 15/01/2020
	;mov	bx, 10
	;xor	dx, dx
;pdn_itoa:
	;div	bx
	; 03/02/2013
	;add	byte ptr [si], dl ; 03/02/2013
	;and	dl, dl
	;jnz	short @f
	;;and	al, al
	;; 10/01/2020
	;and	ax, ax
	;jz	short pdn_14
;@@:	
	;dec	si
	;xor	dl, dl
	;jmp	short pdn_itoa

	; 22/01/2020
	mov	si, offset dec_num
	;
	mov	bx, si
	dec	si
	add	si, cx
	mov	di, si
	;;mov	cx, 11
	;mov	cl, 11
	;mov	ch, '0'
	; 12/12/2021
	mov	cx, 300Bh
@@: 
	mov	byte ptr [bx], ch
	inc	bx
	dec	cl
	jnz	short @b
	
	;xor	ch, ch
	;mov	byte ptr [bx], ch

	; ***** ; 22/01/2020

	; 22/01/2020
	mov	cx, 10
	or	dx, dx
	jnz	short pfn_itoa	
pdn_itoa:
	div	cx
	; 03/02/2013
	add	byte ptr [si], dl
	and	dl, dl
	jnz	short @f
	;and	al, al
	; 10/01/2020
	and	ax, ax
	jz	short pdn_14 ; *
@@:	
	dec	si
	;xor	dx, dx
	xor	dl, dl
	jmp	short pdn_itoa
	
	; 22/01/2020
	;mov	cx, 10
pfn_itoa:
	call	div32
	add	byte ptr [si], bl
	and	bl, bl
	jnz	short @f
	and	ax, ax
	jnz	short @f
	and	dx, dx
	jz	short pdn_14 ; *
@@:	
	dec	si
	jmp	short pfn_itoa

	; ***** ; 22/01/2020

pdn_14:
	mov	si, offset dec_num
	mov	bx, si
@@:	; leading zeros will not be printed
        mov	al, byte ptr [bx] ; 03/02/2013
	cmp	al, '0'
	ja	short @f
	cmp	bx, di
	jnb	short @f
	mov	al, 20h
	mov	byte ptr [bx], al 
	inc	bx
	jmp	short @b
@@:
pdn_putc:
	mov	ah, 0Eh
	mov	bx, 07h
@@:
	lodsb
	
	int	10h

	cmp	si, di
	jna	short @b

	;mov	al, 20h
	;int	10h
	
	retn 
	
print_decimal_number endp

print_volume_info proc near
	; 12/12/2021
	; 24/09/2019 - Retro UNIX 386 v2
	;	      (modified unix v7 inode format)
	; 16/02/2013

	mov	bx, offset BSBuffer
	add	bx, bsVolumeSerial+2
	mov	cx, 2
	mov	di, offset msgVolume_Serial
@@:
	mov	ax, word ptr [bx]
	call	proc_hex_double
	stosw
	mov	ax, dx
	stosw
	dec	cx
	jz	short @f
	inc	di
	sub	bx, 2	
	jmp	short @b
@@:
	mov	si, offset msgVolume_Info
	call	PRINT_MSG

	; 24/09/2019
@@:
	mov	si, offset msgVol_Size_Hdr
	call PRINT_MSG
	mov	ax, word ptr [systm.sb_VolumeSize] ; total sectors
	mov	cl, 4 ; mov cx, 4
	call	print_decimal_number
	mov	si, offset msgVolume_Size
	call	PRINT_MSG

	mov	si, offset msgVol_freeblocks_Hdr
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_FreeBlocks] ; free sectors 
	;mov	cx, 4	
	mov	cl, 4 ; 12/12/2021
	call	print_decimal_number
	mov	si, offset msgVolume_freeblocks
	call	PRINT_MSG

@@:
	mov	si, offset msgVol_icount_Hdr
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_InodeCount] ; number of inodes
	mov	cl, 4 ; mov cx, 4
	call	print_decimal_number
	mov	si, offset msgVolume_icount
	call	PRINT_MSG

	mov	si, offset msgVol_free_icount_Hdr
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_FreeInodes] ; num of free inodes
	;mov	cx, 4	
	mov	cl, 4
	call	print_decimal_number
	mov	si, offset msgVolume_free_icount
	call	PRINT_MSG

	retn	
	
print_volume_info endp

proc_hex_double proc near
	; 16/02/2013 (AX:DX)
	; 28/01/2002 (DX:AX)
	; From binary (word) to hexadecimal (character) converter
	;
	; input -> 
	;	AX = word (binary number) to be converted
	; output ->
	;	AX = First 2 characters of hexadecimal number
	;	DX = Last 2 characters of hexadecimal number

	push	cx
        xor	dx, dx
        mov	cx, 10h
        div	cx	; Q in AX, R in DX (DL)
        push	dx	; DH= 0, R in DL <- CX= 10h 
        xor	dl, dl
        div	cx	; DH= 0, R in DL, AX <= FFh
        div	cl	; AL <= 0Fh
			; R in AH, Q in AL
        pop	cx	; R in CL
	mov	dh, cl
	
        or	dx,'00'

        cmp	dl,'9'
        jna	short pass_cc_dl
        add	dl,7
pass_cc_dl:
        cmp	dh,'9'
        jna	short pass_cc_dh
        add	dh,7
pass_cc_dh:
        or	ax, '00'

        cmp	al,'9'
        jna	short pass_cc_al
        add	al,7
pass_cc_al:
        cmp	ah,'9'
        jna	short pass_cc_ah
        add	ah,7
pass_cc_ah:
        pop	cx

        retn

proc_hex_double endp

show_inode proc near
	; 24/09/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode format)
	; 17/02/2013
	; print inode details
	; Format: inode <decimal number>, iget <decimal number>
	; INPUT -> AX <> 0 -> Current Inode [ii]
	;	AX = 0 -> use inode number input
	;
	and	ax, ax
	jnz	short show_inode_7
	mov	word ptr [arg], ax ; 0
	xor	dx, dx
show_inode_1:
	lodsb
	cmp	al, '0'
	jb	short show_inode_4
	cmp	al, '9'
        ja	short show_inode_stc_retn ; cmc
	sub	al, '0'
show_inode_2:
	or	dx, dx
	jnz	short show_inode_5
show_inode_3:
	mov	dx, ax
	jmp	short show_inode_1
show_inode_4:
	or	dx, dx
	jz	short show_inode_stc_retn
	cmp	al, 20h
	jna	short show_inode_6
show_inode_stc_retn:
	cmc
show_inode_retn:
	retn
show_inode_5:
	cmp	dx, 256
	jnb	short show_inode_stc_retn
	mov	ah, dl
	mov	dl, al
	mov	al, 10
	mul	ah
	add	dx, ax 
	jmp	short show_inode_1
show_inode_6:
	;mov	bx, word ptr [systm]
	;add	bx, offset systm+2
	;mov	ax, word ptr [bx] ; inode map bytes
	
	; 20/09/2019 - Retro UNIX 386 v2
	mov	ax, word ptr [systm.sb_InodeMapSize]

	shl	ax, 1
	shl	ax, 1
	shl	ax, 1 ; inode count

	; 20/09/2019 - Retro UNIX 386 v2	
	;add	ax, 40 ; + device file inodes
	
	cmp	ax, dx
	jb	short show_inode_retn ; not a valid i-number
	mov	ax, dx
	mov	word ptr [arg], ax
	; ax = i-number
	call	iget
	jc	short show_inode_retn
show_inode_7:
	;mov	ax, word ptr [ii]
	call	proc_hex_double
	mov	word ptr [txt_inode_number], ax
	mov	word ptr [txt_inode_number+2], dx
	mov	ax, word ptr [inode_flgs]
	push	ax
	call	proc_hex_double
	mov	word ptr [txt_inode_flags_h], ax
	mov	word ptr [txt_inode_flags_h+2], dx
	pop	dx
	mov	di, offset txt_inode_flags_b
	mov	cx, 16
@@:
	xor	al, al ; 0
	shl	dx, 1
	adc	al, '0'
	stosb
	loop @b

	; 24/09/2019
	mov	ax, word ptr [inode_nlks]
	call	proc_hex_double
	mov	word ptr [txt_inode_nlks], ax
	mov	word ptr [txt_inode_nlks+2], dx

	mov	ax, word ptr [inode_uid]
	call	proc_hex_double
	mov	word ptr [txt_inode_uid], ax
	mov	word ptr [txt_inode_uid+2], dx

	mov	ax, word ptr [inode_gid]  ; & size_h
	call	proc_hex_double
	mov	word ptr [txt_inode_gid], dx
	mov	word ptr [txt_inode_size_h], ax

        mov	ax, word ptr [inode_size+2]
	call	proc_hex_double
	mov	word ptr [txt_inode_size], ax
	mov	word ptr [txt_inode_size+2], dx

        mov	ax, word ptr [inode_size]
	call	proc_hex_double
	mov	word ptr [txt_inode_size+4], ax
	mov	word ptr [txt_inode_size+6], dx

	mov	cl, 8
	mov	si, offset inode_dskp
	mov	di, offset txt_inode_dskp
show_indir_bn:
@@:
	lodsw
	mov	dx,ax
	lodsw
	push	dx	
	call	proc_hex_double	
	stosw
	mov	ax, dx
	stosw
	pop	ax
	call	proc_hex_double	
	stosw
	mov	ax, dx
	stosw
	dec	cl
	jz	short @f
	inc	di
	inc	di
	jmp	short @b
@@:
	cmp	si, offset inode_dskp+32
	ja	short @f	
	mov	cl,2
	mov	di, offset txt_inode_dskp_i
	jmp	short show_indir_bn
@@:
	;mov	si, offset inode_atim
	mov	ax, word ptr [si]
	mov	dx, word ptr [si+2]
	push	dx
	push	ax
	push	dx
	call	proc_hex_double
	mov	word ptr [txt_inode_atim_h+4], ax	
	mov	word ptr [txt_inode_atim_h+6], dx
	pop	ax
	call	proc_hex_double
	mov	word ptr [txt_inode_atim_h], ax	
	mov	word ptr [txt_inode_atim_h+2], dx
	pop	ax
	pop	dx
	call	convert_from_epoch
	mov	ax, word ptr [year]
	mov	si, offset txt_inode_ayear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	mov	ax, word ptr [month]
	mov	si, offset txt_inode_amonth
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [day]
	mov	si, offset txt_inode_aday
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [hour]
	mov	si, offset txt_inode_ahour
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [minute]
	mov	si, offset txt_inode_aminute
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [second]
	mov	si, offset txt_inode_asecond
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	si, offset inode_mtim
	mov	ax, word ptr [si]
	mov	dx, word ptr [si+2]
	push	dx
	push	ax
	push	dx
	call	proc_hex_double
	mov	word ptr [txt_inode_mtim_h+4], ax	
	mov	word ptr [txt_inode_mtim_h+6], dx
	pop	ax
	call	proc_hex_double
	mov	word ptr [txt_inode_mtim_h], ax	
	mov	word ptr [txt_inode_mtim_h+2], dx
	pop	ax
	pop	dx
	call	convert_from_epoch
	mov	ax, word ptr [year]
	mov	si, offset txt_inode_myear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	mov	ax, word ptr [month]
	mov	si, offset txt_inode_mmonth
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [day]
	mov	si, offset txt_inode_mday
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [hour]
	mov	si, offset txt_inode_mhour
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [minute]
	mov	si, offset txt_inode_mminute
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [second]
	mov	si, offset txt_inode_msecond
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	si, offset inode_ctim
	mov	ax, word ptr [si]
	mov	dx, word ptr [si+2]
	push	dx
	push	ax
	push	dx
	call	proc_hex_double
	mov	word ptr [txt_inode_ctim_h+4], ax	
	mov	word ptr [txt_inode_ctim_h+6], dx
	pop	ax
	call	proc_hex_double
	mov	word ptr [txt_inode_ctim_h], ax	
	mov	word ptr [txt_inode_ctim_h+2], dx
	pop	ax
	pop	dx
	call	convert_from_epoch
	mov	ax, word ptr [year]
	mov	si, offset txt_inode_cyear
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal
	mov	ax, word ptr [month]
	mov	si, offset txt_inode_cmonth
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [day]
	mov	si, offset txt_inode_cday
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [hour]
	mov	si, offset txt_inode_chour
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [minute]
	mov	si, offset txt_inode_cminute
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	ax, word ptr [second]
	mov	si, offset txt_inode_csecond
	mov	cl, 2
	call	proc_bin_to_decimal
@@:
	mov	si, offset msg_inode_details	
	call	PRINT_MSG

	; 25/09/2019 - Retro UNIX 386 v2 (device inodes)
	test	byte ptr [inode_flgs+1],80h
	jnz	short @f
	
	; write device's major and minor number
        mov	ax, word ptr [inode_dskp]
	call	proc_hex_double
	mov	word ptr [txt_major], ax
	mov	word ptr [txt_minor], dx
	mov	si, offset txt_device
	;call	PRINT_MSG
	jmp	PRINT_MSG	
@@:
	retn

show_inode endp

; 22/01/2020
;PhysicalDriveNumber: db 0
;FileHandle: dw 0

; 04/12/2015 (Retro UNIX 8086 v1 -> Retro UNIX 386 v1)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

UNIX_Welcome:
	db 0Dh,0Ah
	db 'Retro UNIX 386 v2 FS File Import/Export Utility (for 1.44MB floppy disks)'
	db 0Dh,0Ah
	db 'UNIXCOPY by Erdogan TAN 2012 - [15/04/2022]'
	db 0Dh,0Ah,0
type_27h: ; 25/09/2019
	db '(Type ', 27h,'?',27h,' to see valid commands)'
	db 0Dh,0Ah
	db 0Dh,0Ah,0
usage:
	db 0Dh,0Ah ; 25/09/2019
	db 'Usage: unixcopy [Floppy Drive] ', 0Dh,0Ah
	db '       unixcopy -i [floppy disk image file name] '
	db 0Dh,0Ah
	db 0Dh,0Ah
	db "Floppy Drive names:"
	db 0Dh,0Ah
	db 0Dh,0Ah
	db "fd0    (Floppy Disk 1, A:)", 0Dh, 0Ah
	db "fd1    (Floppy Disk 2, B:)", 0Dh, 0Ah
	db "...", 0Dh, 0Ah
	db "A:     (Floppy Disk 1)", 0Dh,0Ah
	db "B:     (Floppy Disk 2)", 0Dh,0Ah
      	db 0Dh,0Ah
	db "Floppy Disk Image File name examples:"
	db 0Dh,0Ah
	db 0Dh,0Ah
	db "fd0.img", 0Dh, 0Ah
	db "fd1.img", 0Dh, 0Ah
	db "runixfd.img", 0Dh, 0Ah
	db 0
unix_cdrv:
UNIX_FD_Name:
	db 'fd'
UNIX_FD_Number:
	db '0:', 0
unix_img_cdir:
	db '!'  ; 07/07/2015
unix_cdir:
	db '/'
	db 37 dup(0)

CDirOffset:
	dw 0

unix_prompt_char:
	db '>'

CursorColumn:
	dw 0

CommandBuffer:
	db 74 dup(0)

program_exit:
	db 0

pdir:	dw 0 

msg_arg:
	db 0Dh, 0Ah ; 13/01/2012 (chmod)
arg: 	dw 0
	dw 0 ; 13/01/2012  ; 01/10/2019

msg_yes_no:
	db '(Yes/No)? ', 0

msg_unix_drv_read_error:
	db 0Dh, 0Ah
	db "Drive not ready or read error!"
	db 0Dh, 0Ah, 0

msg_inv_file_name: ; 07/07/2015
	db 0dh, 0Ah
	db "Invalid file name !", 0Dh, 0Ah
	db "(File name must fit for 8.3 DOS format) !"
	db 0Dh, 0Ah, 0  

msg_file_not_found: ; 07/07/2015
	db 0Dh, 0Ah
	db "File not found !", 0Dh, 0Ah
	db "(File must be in current directory) !"
	db 0Dh, 0Ah, 0

msg_inv_image_file: ; 07/07/2015
	db 0Dh, 0Ah
	db "Invalid floppy disk image file !", 0Dh, 0Ah
	db "(File size must be 1474560 bytes) !"
	db 0Dh, 0Ah, 0

Msg_Not_Unix_FS:
	db 0Dh, 0Ah
	db "Drive has not got a Retro UNIX v2 FS !"
	db 0Dh, 0Ah, 0

Msg_writing_file:
	db 0Dh, 0Ah
	db "Writing file..."
	db 0

Msg_Removing_file:
	db 0Dh, 0Ah
	db "Deleting file..."
	db 0

Msg_DosFile_Name:
	db 0Dh, 0Ah
	db "DOS File Name : ", 0

Msg_StartupFile_Name:
	db 0Dh, 0Ah
	db "Startup File Name : ", 0

Msg_3dot_OK:
	db "..."
Msg_OK:
	db ' OK.', 0Dh, 0Ah, 0

msg_YES:
	db ' YES'
	db 0
msg_NO:
	db ' NO'
	db 0   
error_msg:
	db 0Dh, 0Ah
	db 'Error !'
UNIX_CRLF:
	db 0Dh, 0Ah, 0

msg_making_directory:
	db 0Dh, 0Ah
	db "Making directory..."
	db 0

msg_removing_directory:
	db 0Dh, 0Ah
	db "Removing directory..."
	db 0

msg_unix_drv_write_error:
	db 0Dh, 0Ah
	db 'Drive not ready or write error!'
	db 0Dh, 0Ah
	db 0
msg_Startup_File_Not_Exists:
	db 0Dh, 0Ah
	db 'Startup File is not configured yet ! '
	db 0Dh, 0Ah, 0 

msg_sf_configuration_set_ok:
	db 0Dh, 0Ah
	db "Startup file configuration SET is OK."
	db 0Dh, 0Ah, 0

msg_sf_configuration_reset_ok:
	db 0Dh, 0Ah
	db "Startup file configuration RESET is OK."
	db 0Dh, 0Ah, 0

msg_overwrite_question1:
	; 1/12/2012
	db 0Dh, 0Ah
	db 'Do you want to overwrite '
	db 27h
	db 0

msg_overwrite_question2: 
	db 27h
	db ' file '
	db 0	

msg_remove_question1:
	; 1/12/2012
	db 0Dh, 0Ah
	db 'Do you want to delete '
	db 27h
	db 0

msg_remove_question2: 
	db 27h
	db ' file '
	db 0
align 2

RetryCount:
	dw 0

; 07/07/2015
img_file_name:
	db 13 dup(0)
	db 0
img_file_handle:
	dw 0
;img_file_pos:
;	dd 0 ; file (position) pointer
;
DirFileName:
	db 20h ; 06/01/2013
BOOT_FILE_NAME: db 14 dup(0) ; 04/12/2015 (9 -> 14+'?')
	db '?' ; Here will be ZERO if name length is 14
	       ; (ci_move_bfn_3:)

uf_make_datetime: dd 0 ; 25/11/2012

uf_i_number:	dw 0 ; 25/11/2012

; 19/09/2019 - Retro UNIX 386 v2 inode format 
;		(modified unix v7 inode format)
bootfile_inode: 
inode:
inode_flgs:	dw 81B4h ; Flags (1000000110110100b) (81B4h for UNIX v7)
inode_nlks:	dw 1	; number of links 
inode_uid:	dw 3	; user ID (3 = bin)
inode_gid:	db 3	; group ID (3 = bin)
inode_size_h:	db 0    ; file size bits 33-40 (=0)
inode_size:	dd 0	; file size
inode_dskp:	dd 10 dup (0) ; indirect or direct contents blocks
inode_atim:	dd 0	; lass access -inode modif.- date & time
inode_mtim:	dd 0	; (file) modification date & time
inode_ctim:	dd 0	; (file) creation date & time

; 20/01/2020
index:	db 0 ; disk sector/block address index (in indirect block)

rw:	db 0

; 20/09/2019
; NOTE: word ptr [imod] is used to check inode modification in iget 

imod:	db 0
imodx:	db 0 ; 19/09/2019 - Retro UNIX 386 v2 
;	     (extended flag for inode modification)	

; 20/01/2020
I_rw:	db 0 ; inode table read (0) or write (1)	
I_valid:
	db 0 ; inode table buffer is valid (1) or not (0)
I_sector:
	dw 0 ; inode table sector index/offset

U:
u_uid:	db 0
u_gid:	db 0 ; 19/09/2019 - Retro UNIX 386 v2
u_cdir: dw ROOT_DIR_INODE_NUMBER
u_namep:
	dw 0
u_dirp: dw 0  ; 16 bit for (retro unix bootable) flopppy disks 	
	;dw 0 ; 32 bit for hard disks (retro unix partitions)
u_base: dw 0
u_off:	dw 0
	dw 0 ; 20/01/2020 (32 bit file pointer/offset)
u_count:
	dw 0
u_nread:
	dw 0
u_dirbuf:
	db 16 dup(0) ; 04/12/2015 (10 -> 16)
ii:	dw 0

; 22/01/2020
FileHandle:
	dw 0
; 22/01/2020
buff_d:
PhysicalDriveNumber:
	db 0
buff_m:
	db 0
buff_s: dw 0FFFFh  ; 16 bit for (retro unix bootable) flopppy disks	
	;dw 0FFFFh ; 32 bit for hard disks (retro unix partitions)

year:	dw 1970
month:	dw 1
day:	dw 1
hour:	dw 0
minute:	dw 0
second:	dw 0

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

; 30/11/2012
;imin:	dd 0
;ihrs:	dd 0
;iday:	dw 0
;lday:	dw 0
;qday:	dw 0
;iyrs:	dw 0
;jday:	dw 0
;mday:	dw 0

; 25/11/2012
str_inode_number:
	db 0Dh, 0Ah
	db 'Startup File I-Number: ', 0
Decimal_i_no_str:	
	db 6 dup (0)

Str_startup_file_size:
	db 0Dh, 0Ah
	db 'Startup File Size : ', 0
Str_Bytes:
	db ' bytes', 0

Decimal_size_str: db 6 dup (0)

Str_sf_date_time:
	db 0Dh, 0Ah
	db 'Creating Date & Time    : '
Str_cday:
	db '00'
	db '/'
Str_cmonth:
	db '00'
	db '/'
Str_cyear:
	db '0000'
	db 20h, 20h
Str_chour:
	db '00'
	db ':'
Str_cminute:
	db '00'
	db ':'
Str_csecond:
	db '00'
	db 0Dh, 0Ah
	db 'Last Modif. Date & Time : '
Str_mday:
	db '00'
	db '/'
Str_mmonth:
	db '00'
	db '/'
Str_myear:
	db '0000'
	db 20h, 20h
Str_mhour:
	db '00'
	db ':'
Str_mminute:
	db '00'
	db ':'
Str_msecond:
	db '00'
	db 0Dh, 0Ah
	db 'Mode Change Date & Time : '
Str_aday:
	db '00'
	db '/'
Str_amonth:
	db '00'
	db '/'
Str_ayear:
	db '0000'
	db 20h, 20h
Str_ahour:
	db '00'
	db ':'
Str_aminute:
	db '00'
	db ':'
Str_asecond:
  	db '00'
	db 0Dh, 0Ah, 0

;23/02/2013
list_count:
	db 0FFh
; 20/01/2013
ls_option:
	db 0
; 21/01/2013
;dec_num:
;	db 10 dup(20h) ; 03/02/2013, 3 bytes -> 10 bytes
;	db 0
; 22/01/2020
dec_num: 
	db 11 dup(20h)
	db 0
; 21/01/2020
Error:	db 0 ; Disk r/w error code

;30/12/2012
DotDot: db '.'
Dot:	db '.'
	db 0

; 22/01/2020
bmod:	db 0 ; boot sector (bs_bf_inode_num) modified flag
	
;24/09/2019
;16/02/2013
msgVolume_Info:
	db 0Dh, 0Ah
	db "Retro UNIX 386 v2 (RUFS) File System", 0Dh, 0Ah
	db "by Erdogan Tan (2013-2022)"
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Volume Serial No: "
msgVolume_Serial:
	db "0000-0000h"
	db 0Dh, 0Ah, 0
msgVol_Size_Hdr:
	db "Volume Size : ", 0
msgVolume_Size:
	;db "0000" 
	db " blocks", 0Dh, 0Ah, 0
msgVol_freeblocks_Hdr:
	db "Free Count  : ", 0
msgVolume_freeblocks:
	;db "0000" 
	db " blocks", 0Dh, 0Ah, 0	
msgVol_icount_Hdr:
	db "# of Inodes : ", 0
msgVolume_icount:
	;db "0000" 
	db 0Dh, 0Ah, 0
msgVol_free_icount_Hdr:
	db 'Free Inodes : ', 0
msgVolume_free_icount:
	;db "0000" 
	db 0Dh, 0Ah, 0

NotFound_msg:
	db 0Dh, 0Ah
	db "Not found !"
	db 0Dh, 0Ah, 0
msgINumber:
	db 0Dh, 0Ah
	db "Inode Number :", 0

; 21/09/2019
msg_inode_details:
	db 0Dh, 0Ah
	db "RETRO UNIX V2 I-NODE STRUCTURE DETAILS OF I-NODE "
txt_inode_number:
	db "0000h"
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Flags : "
txt_inode_flags_h:
	db "0000h"
	db 20h
	db "["
txt_inode_flags_b:
	db "0000000000000000b"
	db "]"
	db 0Dh, 0Ah
	db "# of Links : "
txt_inode_nlks:
	db "0000h"
	db 0Dh, 0Ah
	db "User ID : "
txt_inode_uid:
	db "0000h"
	db 0Dh, 0Ah
	db "Group ID : "
txt_inode_gid:
	db "00h"
	db 0Dh, 0Ah
	db "Size hb : "
txt_inode_size_h:
	db "00h"
	db 0Dh, 0Ah
	db "Size : "
txt_inode_size:
	db "00000000h"
	db 0Dh, 0Ah
	db "Disk Blocks : "
	db 0Dh,0Ah ; 24/09/2019
txt_inode_dskp:	
	db "00000000h 00000000h 00000000h 00000000h "
	db "00000000h 00000000h 00000000h 00000000h"
	db 0Dh,0Ah
txt_inode_dskp_i:
	db "00000000h 00000000h"
	db 0Dh, 0Ah
	db "Inode Changing Time : "
txt_inode_atim_h:
	db "00000000h"
	db 20h, 20h
	db "["
txt_inode_aday:	
	db "00"
	db "/"
txt_inode_amonth:
	db "00"
	db "/"
txt_inode_ayear:
	db "0000"
	db ","
txt_inode_ahour:
	db "00"
	db ":"
txt_inode_aminute:
	db "00"
	db ":"
txt_inode_asecond:	
	db "00"
	db "]"
	db 0Dh, 0Ah
	db "Modification Time : "
txt_inode_mtim_h:
	db "00000000h"
	db 20h, 20h
	db "["
txt_inode_mday:	
	db "00"
	db "/"
txt_inode_mmonth:
	db "00"
	db "/"
txt_inode_myear:
	db "0000"
	db ","
txt_inode_mhour:
	db "00"
	db ":"
txt_inode_mminute:
	db "00"
	db ":"
txt_inode_msecond:	
	db "00"
	db "]"
	db 0Dh, 0Ah
	db "Creation Time : "
txt_inode_ctim_h:
	db "00000000h"
	db 20h, 20h
	db "["
txt_inode_cday:
	db "00"
	db "/"
txt_inode_cmonth:
	db "00"
	db "/"
txt_inode_cyear:
	db "0000"
	db ","
txt_inode_chour:
	db "00"
	db ":"
txt_inode_cminute:
	db "00"
	db ":"
txt_inode_csecond:	
	db "00"
	db "]"
	db 0Dh, 0Ah, 0
; 25/09/2019
txt_device: 
	db 0Dh,0Ah	
	db "DEVICE", 0Dh,0Ah 
	db "Major Number : " 	
txt_major:
	db "00h", 0Dh,0Ah
	db "Minor Number : "
txt_minor:
	db "00h", 0Dh,0Ah, 0

UNIXCOPY_Commands: ; 23/02/2013
db 0Dh, 0Ah
db "UNIXCOPY COMMANDS      [", 27h, "/", 27h, " means alternative, ",  27h, "<...>", 27h, " means command argument]", 0Dh, 0Ah
db "dir <directory name>   : print directory entries without details", 0Dh, 0Ah 
db "ls <directory name>    : print directory entries, ", 27h, "/",  27h," means entry is directory", 0Dh, 0Ah
db "ls -l <directory name> : print directory entries with details", 0Dh, 0Ah
db "chdir/cd <directory name> : change directory", 0Dh, 0Ah
db "fromdos <dos file name> <unix file name> : copy/import dos file to unix fs", 0Dh, 0Ah
db "todos <unix file name> <dos file name>   : copy/export unix file to dos fs", 0Dh, 0Ah  
db "rm <file name>         : remove/delete/unlink file", 0Dh, 0Ah 
db "mkdir <directory name> : make new sub directory", 0Dh, 0Ah
db "rmdir <directory name> : remove/delete sub directory", 0Dh, 0Ah
db "link <source file name> <destination file name> : link file to file", 0Dh, 0Ah 
db "exit                   : return to dos", 0Dh, 0Ah
db "show <file name>       : show file, print/display file contents", 0Dh, 0Ah
db "inode/iget <inode number> : print inode details for (decimal) inode number", 0Dh, 0Ah
db "chmod <mode> <file name>  : change file mode (according to octal number)", 0Dh, 0Ah
db "chown <owner> <file name> : change file's owner (according to decimal number)", 0Dh, 0Ah
db "chgrp <group> <file name> : change file's group (according to decimal number)", 0Dh, 0Ah  ; Retro UNIX 386 v2
db "namei <file name>      : return/print inode number of file (as decimal)", 0Dh, 0Ah 
db "fs/volume              : print (current) unix fs (super block) info", 0Dh, 0Ah
db "bootfile <file name>   : select/configure file as startup/boot file", 0Dh, 0Ah
db "nobootfile             : reset/cancel bootfile configuration", 0Dh, 0Ah
db "?/help                 : print/display UNIXCOPY commands summary (as above)", 0Dh, 0Ah, 0

align 16

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  buffers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

boot_sector:
BSBUFFER:	db 512 dup(0)
; superblock
super_block:
systm:		db 512 dup(0)

; 22/01/2020
Buffer:
sector_buffer:	db 512 dup(0) ; Masterboot sector & common buffer

; 20/01/2020
I_buffer:	db 512 dup(0)	; Inode table sector buffer

ReadBuffer:     db 512 dup(0)
WriteBuffer:    db 512 dup(0)

; 24/09/2019
im_buffer:	db 512 dup(0)	; Inode map sector buffer
fbm_buffer:	db 512 dup(0)	; Free blocks map sector buffer
dbli_buffer:	db 512 dup(0)
; 22/01/2020 (triple indirect addressing is not applicable for 1.44MB floppy disks)
;trpi_buffer:	db 512 dup(0)

SizeOfFile      equ $-100
	
UNIXCOPY  ends

          end  START_CODE