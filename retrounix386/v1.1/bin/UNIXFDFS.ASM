; ****************************************************************************
; UNIXFDFS.ASM
; ----------------------------------------------------------------------------
;
; RETRO UNIX 8086 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.1) by ERDOGAN TAN (Beginning: 11/07/2012) 
; 1.44 MB Floppy Disk 
; Bootable Unix (RUFS) File System Installation/Formatting Code 
;
; UNIXFDFS.ASM -> Last Modification: 04/04/2022
;
; Derivation from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
;
; ****************************************************************************
; 04/04/2022 - Inode Table/List Start Address Modification. (3 -> 2)
; 03/04/2022 - Inode Table/List Modifications (BugFix)
; 24/01/2022
; 09/01/2020 - Boot Sector (bsDriveNumber, DL) BugFix

; 14/03/2022 (*)
; 24/01/2022 (*)
; 04/12/2015 Retro UNIX 386 v1.1 (14 byte file names)
; 21/04/2014 (tty8=COM1, tty9=COM2)
; 22/12/2013
; 09/07/2013

; 14/03/2022 (*)
; NOTE: Previous COM/binary file (which was assembled in 24/01/2022) contains
;	runix 386 v1.0 'uinstall.asm' code (for 8 byte file names) mistakenly.
;	So, this modification is only for assembling 'unixfdfs.com' with v1.1
;	'uinstall.asm' file (builds sys directories with 14 byte file names). 	 	 

; 24/01/2022
; (UNIXFDFS.COM)
; Assembler: MASM 6.14
;	masm unixfdfs.asm, unixfdfs.obj, unixfdfs.lst
;	link /t unixfdfs.obj

; (Retro Unix File System) Boot Sector Parameters
;
;jmp short @f 	       ; db EBh, 13h
bsFSystemID	equ 2  ; db 'RUFS'
bsVolumeSerial	equ 6  ; dd 0
		       ; db 'fd'
bsDriveNumber	equ 12 ; db 0
bsReserved	equ 13 ; db 0  ; 512 bytes per sector
bsSecPerTrack	equ 14 ; db 18
bsHeads		equ 15 ; db 2
bsTracks	equ 16 ; dw 80
bs_BF_I_number	equ 18 ; dw 0
		       ; db '@'
; @@:	 

RUFS_INSTL      SEGMENT PUBLIC 'CODE'
		assume cs:RUFS_INSTL,ds:RUFS_INSTL,es:RUFS_INSTL,ss:RUFS_INSTL

rufs_fd_format proc near
		; 04/12/2015
		; 28/10/2012
		; 19/9/2012
		; 14/8/2012
		; 13/8/2012
		; 12/8/2012

		org 100h
INSTALL:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

		mov si, offset 80h		; PSP command tail
		mov cl, byte ptr [SI]
		or cl, cl                               
		jz short rufs_fd_format_7	; jump if zero

rufs_fd_format_1:
		inc si
		mov al, byte ptr [SI]
		cmp al, ' '			; is it SPACE ?
		jne short rufs_fd_format_2

		dec cl                                  
		jne short rufs_fd_format_1                 
		jmp short rufs_fd_format_7

rufs_fd_format_2:
		cmp al, "f"
		jne short rufs_fd_format_3
		inc si
		mov al, byte ptr [SI]
		cmp al, "d"
		jne short rufs_fd_format_7
		inc si
		mov ax, word ptr [SI]
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
		cmp al, 'A'
		jb short rufs_fd_format_7
		cmp al, 'B'			; A - Z
		jna short rufs_fd_format_4                    
		cmp al, 'a'			; a - z 
		jb short rufs_fd_format_7                 
		cmp al, 'b'                           
		ja short rufs_fd_format_7                 

		sub al, 'a'-'A'			; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_fd_format_4:
		mov byte ptr [RUFS_DRIVE], al
		sub al, 'A'			; make it zero based 

rufs_fd_format_5:
		mov dl, al
		mov byte ptr [boot_sector+bsDriveNumber], dl
		mov ah, 08h
		int 13h				; return disk parameters
		push cs
		pop es				; restore es
		jc  rufs_fd_format_17

		cmp bl, 04h			; Drive Type
		jb  rufs_fd_format_17

		mov si, offset Msg_DoYouWantToFormat
		call PRINT_MSG

rufs_fd_format_6:
		xor ax, ax
		int 16h				; wait for keyboard command
		cmp al, 'C'-40h
		je short rufs_fd_format_8              
		cmp al, 27
		je short rufs_fd_format_8
		and al, 0DFh
		cmp al, 'Y'			; Yes?
		je short rufs_fd_format_10	; write
		cmp al, 'N'			; No?
		je short rufs_fd_format_9	; no write (exit)

rufs_fd_format_7:
		mov si, offset UNIX_Welcome
		call PRINT_MSG

rufs_fd_format_8:
		mov si, offset UNIX_CRLF
		call PRINT_MSG

		int 20h

infinive_loop:  jmp short infinive_loop


rufs_fd_format_9:
		mov si, offset msg_NO
		call PRINT_MSG

		jmp short rufs_fd_format_8

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rufs_fd_format_10:
		mov si, offset msg_YES
		call PRINT_MSG

rufs_fd_format_11:
		xor ax, ax
		int 1Ah                 ; get time of day
  					; set unique volume ID
		mov word ptr [boot_sector+bsVolumeSerial], dx
		mov word ptr [boot_sector+bsVolumeSerial+2], cx 

rufs_fd_format_12:
		mov si, offset Msg_installing_file_system
		call PRINT_MSG

		mov dl, byte ptr [boot_sector+bsDriveNumber]

		call unix_fs_install
		jnc short rufs_fd_format_14

		mov ah, byte ptr [Error]

rufs_fd_format_13: ; loc_rw_error
		mov al, ah
		push ax
		mov si, offset Msg_Disk_RW_Error
		call PRINT_MSG
		pop ax
		call proc_hex
		mov word ptr [Str_Err], ax
		mov si, Offset Msg_Error_Number
		call PRINT_MSG

		int 20h

rufs_fd_format_14:
		mov  si, offset Msg_OK
		call PRINT_MSG

rufs_fd_format_15:
		mov si, offset Msg_writing_boot_sector
		call PRINT_MSG

		mov byte ptr [RetryCount], 4

rufs_fd_format_16:
		mov ax, 0301h			; write to disk

		mov bx, offset boot_sector	; location of boot code

		mov cx, 1			; cylinder = 0
						; sector = 1
		mov dh, 0			; head = 0
		mov dl, byte ptr [boot_sector+bsDriveNumber]

		int 13h
		jnc short rufs_fd_format_17
		dec byte ptr [RetryCount]
		jnz short rufs_fd_format_16

		jmp short rufs_fd_format_13

rufs_fd_format_17:
		mov  si, offset Msg_OK
		call PRINT_MSG

		;int 20h
		jmp rufs_fd_format_8

rufs_fd_format  endp


PRINT_MSG     proc near
		mov     BX,07h  
		mov     AH,0Eh  

PRINT_MSG_LOOP:
		lodsb			; Load byte at DS:SI to AL
		and     AL,AL            
		jz      short PRINT_MSG_OK       
	
		int     10h		; BIOS Service func ( ah ) = 0Eh
					; Write char as TTY
					;AL-char BH-page BL-color
		jmp     short PRINT_MSG_LOOP           

PRINT_MSG_OK:
		retn

PRINT_MSG     endp


proc_hex        proc    near

		db 0D4h,10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
		or AX,'00'		; Make it ZERO (ASCII) based

		xchg AH,AL 

; 1999
		cmp AL,'9'
		jna pass_cc_al
		add AL,7
pass_cc_al:
		cmp AH,'9'
		jna pass_cc_ah
		add AH,7
pass_cc_ah:

; 1998
		retn

proc_hex        endp


;;;;;
include         uinstall.asm
include         unixproc.asm
;;;;;   

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

UNIX_Welcome:
		db 0Dh, 0Ah
		db 'RETRO UNIX 1.44 MB Floppy Disk (RUFS) Format Utility'
		db 0Dh, 0Ah
		db 'UNIXFDFS by Erdogan TAN 2015 - [04/04/2022]'
		db 0Dh,0Ah
		db 0Dh,0Ah
		db 'Usage: unixfdfs [Drive] '
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
		db 0Dh, 0Ah
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
		;db "Installing UNIX v1.1 File Sytem...", 0
		; 04/04/2022
		db "Installing Retro UNIX v1.1 File Sytem...", 0

Msg_Writing_Boot_Sector:
		db 0Dh, 0Ah
		;db "Writing UNIX boot sector...", 0
		; 04/04/2022
		db "Writing Retro UNIX boot sector...", 0

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

msg_error_Number:
		db 0Dh, 0Ah
		db 'Error No:'
str_err:        dw 3030h
		db 'h'
UNIX_CRLF:
		db 0Dh, 0Ah, 0

Error_Code:     db 0

RetryCount:     dw 0

str_volume_name: db 15 dup (0)

		db  'Turkish Rational UNIX', 0
		db  'RETRO UNIX 386 v1.1 by Erdogan TAN', 0
		db  '11/07/2012', 0, '04/04/2022', 0 

even
boot_sector:
; 09/01/2020 - Boot Sector (bsDriveNumber, DL) BugFix
; Retro UNIX 8086 v1 (+ 386 v1) Boot Sector Image
;include	rufsbs.txt ; rufsbs.bin (bin2db) file
;
; 04/04/2022 (new boot sector, inode table address modif.)
include		rufsbs1.txt ; rufsbs1.bin (bin2db) file

RUFS_INSTL      ends

		end  INSTALL