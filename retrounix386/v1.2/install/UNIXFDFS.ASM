; ****************************************************************************
; UNIXFDFS.ASM
; ----------------------------------------------------------------------------
; RETRO UNIX v0.3 - Modified UNIX v7 inode model - 01/09/2019
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; 1.44 MB Floppy Disk 
; Bootable Unix (RUFS) File System Installation/Formatting Code 
;
; Last Modification: 13/04/2022 (Retro UNIX 386 v2)
; UNIXFDFS.ASM -> Last Modification: 04/12/2015 (Retro UNIX 386 v1)
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ----------------------------------------------------------------------------
; masm unixfdfs.asm unixfdfs.obj unixfdfs.lst
; link /t unixfdfs
; ****************************************************************************

; 05/04/2022
; 14/03/2022
; 23/01/2020
; 05/09/2019 Retro UNIX 386 v2 (Modified UNIX v7 inode model)

; 04/12/2015 Retro UNIX 386 v1 (14 byte file names)
; 21/04/2014 (tty8=COM1, tty9=COM2)
; 22/12/2013
; 09/07/2013

; (Retro Unix File System) Boot Sector Parameters
;
; jmp short @f 	       ; db EBh, 18h ; 21/12/2019
bsFSystemID	equ 2  ; db 'RUFS'
bsVolumeSerial	equ 6  ; dd 0
		       ; db 'fd'
bsDriveNumber	equ 12 ; db 0
bsReserved	equ 13 ; db 0  ; 512 bytes per sector (and CHS read)
bsSecPerTrack	equ 14 ; db 18
bsHeads		equ 15 ; db 2
bsTracks	equ 16 ; dw 80
bs_BF_I_number	equ 18 ; dw 0
bsMagic		equ 20 ; db '@'
; 21/12/2019 (extensions for hard disk 71h parition)
bsPartitionID	equ 21 ; db 0 ; db 71h
bsHiddenSects	equ 22 ; dd 0 ; Hidden sectors (Boot Sector LBA)
; @@:	 

RUFS_INSTL      SEGMENT PUBLIC 'CODE'
		assume cs:RUFS_INSTL,ds:RUFS_INSTL,es:RUFS_INSTL,ss:RUFS_INSTL

rufs_fd_format proc near
		; 23/01/2020
		; 04/12/2015
		; 28/10/2012
		; 19/9/2012
		; 14/8/2012
		; 13/8/2012
		; 12/8/2012

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
		;jz short rufs_fd_format_7	; jump if zero
		jnz short @f
		jmp rufs_fd_format_7
@@:
		; 05/09/2019

		;inc si
get_args:
		lodsb
		cmp al, ' '
		ja short rufs_fd_format_1
		dec cl
		jnz short get_args
		jmp short rufs_fd_format_7

rufs_fd_format_1:
		cmp al, '-'
		jne short rufs_fd_format_2
		jmp check_fdi_option ; 23/01/2020
	
rufs_fd_format_2:
		cmp al, "f"
		jne short rufs_fd_format_3
		;inc si
		;mov al, byte ptr [SI]
		lodsb
		cmp al, "d"
		jne short rufs_fd_format_7
		;inc si
		;mov ax, word ptr [SI]
		lodsw
		cmp al, '0'                            
		jb short rufs_fd_format_7
		cmp al, '1'
		ja short rufs_fd_format_7
		cmp ah, 20h
		ja short rufs_fd_format_7
		mov byte ptr [RUFS_DRIVE], al
		sub al, '0'
		jmp short rufs_fd_format_5

rufs_fd_format_3:
		; 23/01/2020
		cmp byte ptr [si], ':'			; A:, B:
		jne short rufs_fd_format_7
		inc si
		cmp byte ptr [si],20h
		ja short rufs_fd_format_7

		cmp al, 'A'
		jb short rufs_fd_format_7
		cmp al, 'B'                             ; A - Z
		jna short rufs_fd_format_4                    
		cmp al, 'a'                             ; a - z 
		jb short rufs_fd_format_7                 
		cmp al, 'b'                           
		ja short rufs_fd_format_7                 

		sub al, 'a'-'A'                         ; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_fd_format_4:
		mov byte ptr [RUFS_DRIVE], al
		sub al, 'A'                             ; make it zero based 

rufs_fd_format_5:
		mov dl, al
		mov byte ptr [boot_sector+bsDriveNumber], dl
		; 06/09/2019
		mov byte ptr [buff_d], al

		mov ah, 08h
		int 13h                                 ; return disk parameters
		push cs
		pop es                                  ; restore es
		;jc rufs_fd_format_17
		jc rufs_fd_format_13 ; 06/09/2019

		; 06/09/2019
		; If this disk is a Virtual disk, BL will not return with 04h!
		;cmp bl, 04h				; Drive Type
		;jb rufs_fd_format_17

		; 09/09/2019
		; Read boot sector to understand drive is ready or not?
		call drive_check

		mov si, offset Msg_DoYouWantToFormat
		call PRINT_MSG

rufs_fd_format_6:
		xor ax, ax
		int 16h                                 ; wait for keyboard command
		cmp al, 'C'-40h
		je short rufs_fd_format_8              
		cmp al, 27
		je short rufs_fd_format_8
		and al, 0DFh
		cmp al, 'Y'                             ; Yes?
		je short rufs_fd_format_10             ; write
		cmp al, 'N'                             ; No?
		je short rufs_fd_format_9              ; no write (exit)

rufs_fd_format_7:
		mov si, offset UNIX_Welcome
		call PRINT_MSG

rufs_fd_format_8:
		mov si, offset UNIX_CRLF
		call PRINT_MSG

		int 20h

infinive_loop:  jmp short infinive_loop

check_fdi_option:
		lodsb
		cmp al, 'i'
		jne short rufs_fd_format_7
		dec cl
		jz short rufs_fd_format_7
		lodsb
		cmp al, ' '
		jne short rufs_fd_format_7
		dec cl
		jz short rufs_fd_format_7
check_fdi_opt_loop:
		lodsb
		cmp al, 20h
		ja short get_fdi_name
		jb short rufs_fd_format_7
		dec cl
		jz short rufs_fd_format_7
		jmp short check_fdi_opt_loop

rufs_fd_format_9:
		mov si, offset msg_NO
		call PRINT_MSG

		jmp short rufs_fd_format_8

get_fdi_name:
		mov di, offset img_file_name
get_fdi_name_nxt_chr:
		stosb
		dec cl
		jz short get_fdi_name_ok		
		lodsb
		cmp al, 20h
		jna short get_fdi_name_ok		
		cmp di, offset img_file_name + 12	
		jb short get_fdi_name_nxt_chr

		jmp short invalid_file_name

get_fdi_name_ok:
		sub al, al
		stosb 

check_file_name_chars:
		call check_invalid_filename_chars
		jc short invalid_file_name		
		jmp cap_file_name
		
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_fd_format_10:
		mov si, offset msg_YES
		call PRINT_MSG

		; 06/09/2019
		;mov dl, byte ptr [boot_sector+bsDriveNumber]
		;mov byte ptr [buff_d], dl

rufs_fd_format_11:
		xor ax, ax
		int 1Ah                 ; get time of day
  					; set unique volume ID
		mov word ptr [boot_sector+bsVolumeSerial], dx
		mov word ptr [boot_sector+bsVolumeSerial+2], cx 

rufs_fd_format_12:
		mov si, offset Msg_installing_file_system
		call PRINT_MSG

		call unix_fs_install
		jnc short rufs_fd_format_14

		mov ah, byte ptr [Error]

rufs_fd_format_13: ; loc_rw_error
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
		jmp rufs_fd_format_8 ; 06/09/2019

;		int 20h
;
;		; 06/09/2019
;hang:
;		jmp short hang

invalid_file_name:
		mov si, offset msg_inv_file_name
                jmp short @b

rufs_fd_format_14:
		mov  si, offset Msg_OK
		call PRINT_MSG

rufs_fd_format_15:
		mov si, offset Msg_writing_boot_sector
		call PRINT_MSG

		;mov byte ptr [RetryCount], 4

rufs_fd_format_16:
		; 06/09/2019 - Retro UNIX 386 v2
		mov word ptr [buff_s], 0
		mov word ptr [buff_o], offset boot_sector
		call dskwr
		jc short rufs_fd_format_13	
		;jmp short rufs_fd_format_17

		;mov ax, 0301h				; write to disk
		;mov bx, offset boot_sector		; location of boot code
		;mov cx, 1				; cylinder = 0
		;					; sector = 1
		;mov dh, 0				; head = 0
		;
		;mov dl, byte ptr [boot_sector+bsDriveNumber]
		;int 13h
		;jnc short rufs_fd_format_17
		;dec byte ptr [RetryCount]
		;jnz short rufs_fd_format_16
		;
		;jmp short rufs_fd_format_13

rufs_fd_format_17:
		mov  si, offset Msg_OK
		call PRINT_MSG

		;int 20h
		; 06/09/2019
		;jmp rufs_fd_format_8
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
		jmp short find_image_file		

file_error:
		mov si, offset file_error_msg
		jmp @b

cap_file_name4:
		cmp bx, 8
		ja short invalid_file_name
	
find_image_file:
		; 05/09/2019 - Retro UNIX 386 v2
		; 07/07/2015 (UNIXCOPY.ASM)
		mov dx, offset img_file_name
		mov cx, 3Fh ; File Attributes
                ;xor cx, cx ; 06/09/2019
		mov ah, 4Eh ; MS Dos Function = Find First File
                int 21h
		jnc short chk_image_file_features
		
		; 06/09/2019 - Retro UNIX 386 v2 development work
		; NOTE: Normally, (this) error code in ax is 12h,
		; 'no more files' error -as experienced in windows 7 cmd-
		; (but -perhaps- ax may have 02h, 'file not found' error) 

		; 06/09/2019
		cmp al,12h  ; 18, no more files
		je short create_image_file ; there is not a file in the dir
					   ; with same name;
					   ; so, we can create a new one!	 
		cmp al,02h  ; 2, file not found error
		jne short file_error ; we need to stop here
				     ; if it is another type of error 
create_image_file:
		; create a new fd image file
		;mov dx, offset img_file_name
		mov cx, 0 ; File Attributes
		mov ah, 3Ch ; MS-DOS Function = Create File
		int 21h
		jc short file_error

		mov byte ptr [new_file], 0FFh

		mov si, CRLF
		call PRINT_MSG

		; 05/09/2019
open_image_file:
		mov al, 2 ; open for reading and writing
		;mov dx, offset img_file_name
		mov ah, 3Dh ; open file
		int 21h
		jc short file_error

		mov word ptr [img_file_handle], ax

		; 06/09/2019
		; set (buffer) disk drive number for image file
		mov byte ptr [buff_d], 90h ; floppy disk image sign

		cmp byte ptr [new_file], 0
		jna rufs_fd_format_11

		; set sector buffer for new file initialization

		mov cx, 256
		mov di, offset sector_buffer
		mov ax, 0F6F6h
		rep stosw

		mov si, offset msg_init_img_file
		call PRINT_MSG

		mov dx, offset sector_buffer
                mov bx, word ptr [img_file_handle]
		mov cx, 2880 ; sectors
write_file_next_sector:
		push cx
		mov cx, 512
		mov ah, 40h ; Write File
                int 21h
		pop cx
		jc short file_error
		cmp ax, 512
		jne short file_error
		loop write_file_next_sector

		; reset sector buffer for new file initialization

		mov cx, 256
		mov di, offset sector_buffer
		sub ax, ax
		rep stosw

		mov si, Msg_OK
		call PRINT_MSG

		jmp rufs_fd_format_11

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21 ;05/01/2013	
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

chk_image_file_features:
		mov si, DTA_Attrib
                mov al, byte ptr [SI]
                and al, 1Fh ; directory, volume label, system, hidden, read only
                jnz file_error       
		mov si, DTA_FileSize
		lodsw
		cmp word ptr [SI], 16h
		jne short invalid_image_file
		cmp ax, 8000h ; 1.44 MB floppy disk image (168000h bytes)
                je short open_image_file
invalid_image_file:
		mov si, offset msg_inv_image_file
		jmp @b 

rufs_fd_format  endp

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

; 05/09/2019

check_invalid_filename_chars proc near
               ; 10/07/2010 (FILE.ASM, TRDOS 8086)
               ; Derivation from proc_check_invalid_filename_chars
               ; in the old TRDOS.ASM 09/02/2005

                ; INPUT: DS:SI = ASCIIZ FileName 
                ; OUTPUT:
                ; stc -> Invalid
                ; clc -> Valid
                ; 10/02/2010
                ; AX, CX, DI will be changed

                ;push es
                ;push ds
                ;pop es
                ;push si
                mov di, offset invalid_fname_chars
                mov al, byte ptr [SI]
check_filename_next_char:
                mov cx, sizeInvFnChars
                mov di, offset invalid_fname_chars
loop_scan_invalid_filename_char:
		scasb 
                je short loc_invalid_filename 
                loop loop_scan_invalid_filename_char
                inc si
                mov al, byte ptr [SI]
                ; 10/07/2010
                cmp al, 1Fh  ; 20h and above 
                ja short check_filename_next_char
                xor ah, ah 
loc_check_invalid_filename_retn:
                ;pop si
                ;pop es
                retn
loc_invalid_filename:
              ; 10/07/2010
               ;mov ax, 0Bh ; Invalid format
                            ; Invalid file name chars
                stc
                ;jmp short loc_check_invalid_filename_retn
		retn

invalid_fname_chars:
		db 22h, 27h, 28h, 29h, 2Ah, 2Bh, 2Ch, 2Fh
		db 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh, 40h
		db 5Bh, 5Ch, 5Dh, 5Eh, 60h
sizeInvFnChars  equ ($ - invalid_fname_chars)                

check_invalid_filename_chars endp

drive_check:
		; 09/09/2019 - Retro UNIX 386 v2
		xor bx, bx ; sector 0
		;mov word ptr [buff_s], bx
		;mov word ptr [buff_o], offset Buffer
		call dskrd
		jnc short drive_check_ok
drive_not_ready:
		; 09/09/2019
		pop ax ; near call return address
		mov si, offset msg_drive_not_ready
		call PRINT_MSG
		jmp rufs_fd_format_8
drive_check_ok:
		retn

file_error_msg:
		db 0Dh, 0Ah
		db 'File Error !'
CRLF:
                db 0Dh, 0Ah, 0

;;;;;
include         uinstall.asm
include         unixproc.asm
;;;;;   

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

UNIX_Welcome:
		db 0Dh,0Ah
		db 'RETRO UNIX 386 v2 1.44 MB Floppy Disk (RUFS) Format Utility'
		db 0Dh, 0Ah
		db 'by Erdogan TAN [13/04/2022]'
		db 0Dh,0Ah
		db 0Dh,0Ah
		db 'Usage: unixfdfs [Drive] ', 0Dh,0Ah
		db '       unixfdfs -i [floppy disk image file name] '
		db 0Dh,0Ah
		db 0Dh,0Ah
		db "Drive names:"
		db 0Dh,0Ah
		db 0Dh,0Ah
		db "fd0    (Floppy Disk 1)", 0Dh, 0Ah
		db "fd1    (Floppy Disk 2)", 0Dh, 0Ah
		db "...", 0Dh, 0Ah
		db "A:     (Floppy Disk 1)", 0Dh, 0Ah
		db "B:     (Floppy Disk 2)", 0Dh, 0Ah
		db 0Dh,0Ah
		db 0Dh,0Ah
		db 'Example 1: unixfdfs A: ', 0Dh,0Ah
		db 'Example 2: unixfdfs -i a.img '
		db 0Dh,0Ah
		db 0

Msg_DoYouWantToFormat:
		db 07h
		db 0Dh, 0Ah
		db 'WARNING!'
		db 0Dh, 0Ah
		db 'All data on the drive will be erased.'
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db 'Do you want to format drive '
RUFS_DRIVE:
		db 'A: (Yes/No)? ', 0

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

str_volume_name: db 15 dup (0)

		db  'Turkish Rational UNIX', 0
		db  'RETRO UNIX 386 v2.0 by Erdogan TAN', 0
		db  '11/07/2012', 0, '13/04/2022', 0 
even
boot_sector:
; Retro UNIX 8086 v1 (+ 386 v1) Boot Sector Image
;include	rufsbs.txt ; rufsbs.bin (bin2db) file
; 29/09/2019
; Retro UNIX 386 v2 Boot Sector Image
;include	rufsbs2.txt ; rufsbs2.bin (bin2db) file
; 09/01/2020 (Boot Sector BugFix, bsDriveNumber segment bug)
; 22/12/2019
; Retro UNIX 386 v2 Boot Sector Image
include		rufsbs3.txt ; rufsbs3.bin (bin2db) file

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

msg_inv_image_file: ; 07/07/2015
		db 0Dh, 0Ah
		db "Invalid floppy disk image file !", 0Dh, 0Ah
		db "(File size must be 1474560 bytes) !"
		db 0Dh, 0Ah, 0

msg_init_img_file:
		db 0Dh, 0Ah
		db "Initializing disk image file sectors...", 0

SizeOfFile      equ $-100

RUFS_INSTL      ends

		end  INSTALL