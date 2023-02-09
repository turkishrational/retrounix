; ****************************************************************************
; UNIXHDCP.ASM (File copy utility for Retro UNIX 386 v2 Hard Disk FS)
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
; [ Last Modification: 20/07/2022 ] - Retro UNIX 386 v2 Bootable Hard Disk FS
; (Previous Modification: 25/01/2020 - (bootable harddisk file system, RUFS))
; UNIXCOPY.ASM -> Last Modification: 02/10/2019 (bootable floppy disk)
;
; Retro UNIX has been derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ----------------------------------------------------------------------------
; masm unixhdcp.asm unixhdcp.obj unixhdcp.lst
; link /t unixhdcp
; ****************************************************************************
; 20/07/2022 - LBA ready (read/write) flag bugfix
; 15/04/2022 - mkdir/rmdir link count bugfix
; 28/03/2022 - Improved First Free Inode & First Free Block calculation
; 27/03/2022 - Retro UNIX 386 v2 Hard Disk FS (Free Inode Count bugfix)
; 23/03/2022 - Retro UNIX 386 v2 Hard Disk FS (BugFix) ((Retro UNIX 386 v1.2))
; 25/01/2020 - Retro UNIX 386 v2 Hard Disk FS (unlink/rm,'free' proc bug)
; 17/12/2019 - Retro UNIX 386 v2 Hard Disk FS (RUFS for 71h partitions)
; 02/10/2019 - Retro UNIX 386 v2 UNIXCOPY utility for Retro UNIX Floppy Disks

; Some new code of procedures, buffers (method) and structures (construction)
; are copied from UNIXHDFS.ASM, 14/12/2019.

; (Retro Unix File System) Hard Disk Boot Sector Parameters (03/10/2019)
;
;jmp short @f 	       ; db EBh, 13h ; db 0EBh, 18h ; 19/12/2019
; 03/12/2019
;bsFSystemID	equ 2  ; db 'RUFS'
;bsVolumeSerial	equ 6  ; dd 0
;		       ; db 'hd'
;bsDriveNumber	equ 12 ; db 0
;bsReserved	equ 13 ; db 0 ; 512 bytes per sector
;bsSecPerTrack	equ 14 ; db 63 (17)
;bsHeads	equ 15 ; db 16 (2,4,8,16,32,64,128,255)
;bsTracks	equ 16 ; dw 1023 (1 to 1023) ; bsCylinders
;bs_BF_I_number	equ 18 ; dw 0
;bsMagic	equ 20 ; db '@'
;bsPartitionID  equ 21 ; db 71h
;bsHiddenSects  equ 22 ; dd 0 ; Hidden sectors (Boot Sector LBA)
; @@:	 

; 04/12/2019
RUFS struc
bsJumpCode     dw ? ; 0EBh,18h ; jmp short @f  	
bsFSystemID    db 4 dup(?) ;'RUFS'
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
RUFS ends

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
sb_DevMajor db 0 ; 03/10/2019
sb_DevMinor db 0 ; 03/10/2019
sb_LongName	db 0 ; 03/10/2019
sb_Direntry32	db 0 ; 03/10/2019
sb_Reserved	db 508-116 dup(?) ; Must be 0 for current RUFS version
sb_Footer	dd ?

SuperBlock ends

; 14/01/2020
sb_HiddenSects equ sb_BootSecAddr
sb_TotalSects  equ sb_VolumeSize

; 14/01/2020
; 'Hidden Sectors' value in super block is 'sb_BootSectAddr'.	 	
; (Absolute start address of the RUNIX partition and boot sector)
; Note: 'systm.sb_HiddenSect' area (in SB) has been changed to
; 	'systm.ExtdVolTbl' -it will not be used for now-
;	 and 'systm.sb_TotalSects' are has been changed to
;	'systm.sb_ExtdVolSize' -it wil not be used for now-

; *****************

; UNIX v1 I-node Flags: 
; 1000000000000000b 	i-node is allocated (8000h)
; 0100000000000000b	directory (4000h)
; 0010000000000000b	file has been modified (2000h)	 	
; 0001000000000000b	large file (1000h)
; 0000000000100000b	set user id on execution (20h)
; 0000000000010000b	executable (10h)
; 0000000000001000b	read, owner (8)
; 0000000000000100b	write, owner (4)
; 0000000000000010b	read, non-owner (2)
; 0000000000000001b	write, non-owner (1)

; UNIX v7 I-node Flags: 
; 1000000000000000b 	IFREG - regular file (8000h)
; 0100000000000000b	IFDIR - directory (4000h)
; 0010000000000000b	IFCHR - character special (2000h)
; 0001000000000000b	reserved (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)	 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	ISVTX - save swapped text (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write,owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h)
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04h)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

; UNIX SysV I-node Flags: (di_mode)
; 1000000000000000b 	IFREG - regular file (8000h)
; 0100000000000000b	IFDIR - directory (4000h)
; 0010000000000000b	IFCHR - character special (2000h)
; 0001000000000000b	IFIFO - fifo special (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)	 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	ISVTX - save swapped text (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write,owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h)
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04h)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

; IFREG	equ 8000h	; /* regular */
; IFMPB	equ 7000h	; /* multiplexed block special */
; IFBLK	equ 6000h	; /* block special */
; IFNAM	equ 5000h	; /* special named file - subtype in r_dev *
; IFDIR	equ 4000h	; /* directory */
; IFMPC	equ 3000h	; /* multiplexed char special */
; IFCHR	equ 2000h	; /* character special */
; IFIFO	equ 1000h	; /* fifo special */
; ISUID	equ 0800h	; /* set user id on execution */
; ISGID	equ 0400h	; /* set group id on execution */
; ISVTX	equ 0200h	; /* save swapped text even after use */
; IREAD equ 0100h	; /* read permission */
; IWRITE equ 0080h	; /* write permission */
; IEXEC	equ 0040h	; /* execute permission */

; Retro UNIX 386 v2 I-node Flags: (di_mode) ;; FIRST DRAFT - 01/09/2019
; 1000000000000000b 	IFREG - regular file (8000h)
; 0100000000000000b	IFDIR - directory (4000h)
; 0010000000000000b	IFCHR - character special (2000h)
; 0001000000000000b	IFIFO - fifo special (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)	 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	IEXTD - use extents (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write,owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h)
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04hq	)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

;; SECOND DRAFT - 03/09/2019

; Retro UNIX 386 v2 I-node Flags: (di_mode) for files
; 1000000000000000b 	IFREG - 1 = regular file (8000h)
; 0100000000000000b	IFDIR - 1 = directory (4000h)
; 0010000000000000b	ISIZ2 - sizing higher bit (2000h)
; 0001000000000000b	ISIZ1 - sizing lower bit (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)	 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	IEXTT - 1 = use extents (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write,owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h)
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04h)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

;; THIRD DRAFT - 15/09/2019
; 18/12/2019 - Mounted flag - IFMNT (2000h)

; Retro UNIX 386 v2 I-node Flags: (di_mode) for files
; 1000000000000000b 	IFREG - 1 = regular file (8000h)
; 0100000000000000b	IFDIR - 1 = directory (4000h)
; 0010000000000000b	IRSVD - 0 = reserved bit (2000h) ; Mounted flag
; 0001000000000000b	ILARG - Large file addressing bit (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	IEXTT - 1 = use extents (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write, owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h)
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04h)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

; Retro UNIX 386 v2 I-node Flags: (di_mode) for devices
; 1000000000000000b 	IFREG - 0 = device file (8000h)
; 0100000000000000b	IFBLK - 1 = block device (4000h)
; 0010000000000000b	IFCHR - character special (2000h) -always 1-
; 0001000000000000b	IFIFO - fifo special (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)	 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	IEXTR - 1 = external device driver (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write, owner (80h)
; 0000000001000000b	IEXEC - execute, owner (40h) 
; 0000000000100000b	read, group (20h)
; 0000000000010000b	write, group (10h)
; 0000000000001000b	execute, group (08h)
; 0000000000000100b	read, others (04h)
; 0000000000000010b	write, others (02h)
; 0000000000000001b	execute, others (01h)

; 15/09/2019
; Flag bit 12 is 0 for small file addressing
; ------------------------------------------
; Disk Block Pointers
;
; di_addr[0] = direct block 0 address
; di_addr[4] = direct block 1 address
; di_addr[8] = direct block 2 address
; di_addr[12] = direct block 3 address
; di_addr[16] = direct block 4 address
; di_addr[20] = direct block 5 address
; di_addr[24] = direct block 6 address
; di_addr[28] = direct block 7 address
; di_addr[32] = direct block 8 address
; di_addr[36] = direct block 9 address

; Flag bit 12 is 1 for large file addressing
; ------------------------------------------
; Disk Block Pointers
;
; di_addr[0] = indirect block 0 address
; di_addr[4] = indirect block 1 address
; di_addr[8] = indirect block 2 address
; di_addr[12] = indirect block 3 address
; di_addr[16] = indirect block 4 address
; di_addr[20] = indirect block 5 address
; di_addr[24] = indirect block 6 address
; di_addr[28] = indirect block 7 address
; di_addr[32] = double indirect block (0) address
; di_addr[36] = triple indirect block (0) address

; 15/09/2019
; Flag bit 8 (if it is set)
; -----------------------------------
; Use extents (contiguous blocks)
;
; di_addr[0] = extent 0 start address (or indirect 0)
; di_addr[4] = extent 0 block count (or indirect 0 bc)
; di_addr[8] = extent 1 start address (or indirect 1)
; di_addr[12] = extent 1 block count (or indirect 1 bc)
; di_addr[16] = extent 2 start address (or indirect 2)
; di_addr[20] = extent 2 block count (or indirect 2 bc)
; di_addr[24] = extent 3 start address (or double indirect)
; di_addr[28] = extent 3 block count (or double indirect bc)
; di_addr[32] = extent 4 start address (or triple indirect)
; di_addr[36] = extent 4 block count (or triple indirect bc)

; UNIX v7 I-node (on disk) : 
;
;struc dinode
;  .di_mode:  resw 1	; /* mode and type of file */
;  .di_nlink: resw 1	; /* number of links to file */
;  .di_uid:   resw 1 	; /* owner's user id */
;  .di_gid:   resw 1	; /* owner's group id */
;  .dinode_size: resd 1 ; /* number of bytes in file */
;  .di_addr:  resb 40 ; (3*13)+1 ; /* disk block addresses */
;  .di_atime: resd 1	; /* time last accessed */
;  .di_mtime: resd 1	; /* time last modified */
;  .di_ctime: resd 1	; /* time created */
;endstruc

; (first draft)
; Retro UNIX 386 v2 I-node (on disk) : 
;
;struc dinode
;  .di_mode:  resw 1	; /* mode and type of file */
;  .di_nlink: resw 1	; /* number of links to file */
;  .di_uid:   resw 1	; /* owner's user id */
;  .di_gid:   resw 1	; /* owner's group id */
;  .dinode_size:   resd 1 ; /* number of bytes in file */
;  .dinode_size_h: resw 1 ; /* number of bytes in file */
;  .di_cssc: resb 1	; Cluster/Block size shift count - 1 ; 04/09/2019	
;  .di_bssc: resb 1	; Block/Sector size shift count - 1 ; 04/09/2019	
;  .di_addr:  resd 8 ; resb 32 ; /* disk block addresses */
;  .di_atime: resd 1	; /* time last accessed */
;  .di_mtime: resd 1	; /* time last modified */
;  .di_ctime: resd 1	; /* time created */
;  .di_reserved2: resd 1 ; reserved (zero)
;endstruc

; 17/12/2019
; 15/09/2019 (second draft)
; Retro UNIX 386 v2 I-node (on disk) : 

;struc dinode
;  .di_mode:  resw 1	; /* mode and type of file */
;  .di_nlink: resw 1	; /* number of links to file */
;  .di_uid:   resw 1	; /* owner's user id */  - 0 to 655535 -
;  .di_gid:   resb 1	; /* owner's group id */ - o to 255 -
;  .dinode_size_h: resb 1 ; /* number of bytes in file */ ; - byte 5 -
;  .dinode_size:   resd 1 ; /* number of bytes in file */
;  .di_addr:  resd 10 ; resb 40 ; /* disk block addresses */
;  .di_atime: resd 1	; /* time last accessed */
;  .di_mtime: resd 1	; /* time last modified */
;  .di_ctime: resd 1	; /* time created */
;endstruc

; 24/12/2019
; 17/12/2019
dinode struc
  di_mode    dw 0	; /* mode and type of file */
  di_nlink   dw 0	; /* number of links to file */
  di_uid     dw 0	; /* owner's user id */  - 0 to 655535 -
  di_gid     db 0	; /* owner's group id */ - o to 255 -
  dinode_size_h db 0	; /* number of bytes in file */ ; - byte 5 -
  dinode_size   dd 0	; /* number of bytes in file */
  di_addr    dd 10 dup(0) ; db 40 dup(0) ; /* disk block addresses */
  di_atime   dd 0	; /* time last accessed */
  di_mtime   dd 0	; /* time last modified */
  di_ctime   dd 0	; /* time created */
dinode ends

; 04/09/2019 - Retro UNIX 386 v2

;BLOCK SIZE SHIFT COUNTS or STORAGE/DISK (SECTor SIZE) TYPE
;----------------------------------------------------------
;0 = 512 bytes per sector floppy drive or hard disk
;1 = 1024 bytes per sector, tape drive
;2 = 2048 bytes per sector DVD, CDROM, new (2TB) hard disks

;CLUSTER SIZE SHIFT COUNTS or FILE SYSTEM BLOCK SIZE
;----------------------------------------------------------
;0 = 1 block (1 sector)
;1 = 2 blocks
;2 = 4 blocks
;3 = 8 blocks
;4 = 16 blocks
;5 = 32 blocks
;6 = 64 blocks
;7 = 128 blocks
;8 = 256 blocks

;SIZING LIMITS (as CLUSTER/BLOCK size) - cluster size = 1
;----------------------------------------------------------
; small - direct block addresses - 10 blocks (5120 bytes) 
; normal - indirect block addresses - 8*128 blocks (512KB) 
; large - double indirect blocks - 128*128 blks + norm (8MB+512KB)
; huge - triple indirect blocks - 128*128*128 blks (1GB+8MB+512KB)

;SIZING LIMITS (as CLUSTER/BLOCK size) - cluster size = 8
;----------------------------------------------------------
; small - direct block addresses - 10*8 blocks (40960 bytes)  
; normal - indirect block addresses - 8*1024 blocks (4MB) 
; large - double indirect blocks - 1024*1024 blks + norm (512MB+4MB)
; huge - triple indirect blocks - 1024*1024*1024 blks (512GB+516MB)

;15/09/2019
;NOTE: Addresses are in sector unit (512 bytes or 2048 bytes).
;      But, block (cluster) size is 1 or 8 sectors, default is 1 sector 	
;      (Cluster size for 2048 bytes/sector disks may be 2,4,8 sectors)
 
;DISK ADDR AND FILE SIZE LIMITS For EXTENTS !!! LIMITS !!!
;----------------------------------------------------------
; (depending on free contiguous blocks/sectors on disk fs)
; direct - 5 extents - 2 terabytes (avr. 400GB per extent)  
; indirect - 3*64 extents - 2TB (average 10GB per extent)
; double ind. - 64*64 extents - 2TB (avr. 512MB per ext.)
; triple ind. - 262144 extents - 2TB (avr. 8MB per extent)

; EXTENT SIZING For 8GB FILE !!! SAMPLE !!!
;----------------------------------------------------------
; direct - 5 extents - 8GB (average 1.6GB per extent)  
; indirect - 192 extents - 8GB (average 42.7MB per extent)
; double ind. - 4288 extents - 8GB (avr. 1.9MB per extent)
; triple ind. - 266432 extents - 8GB (avr. 31.5KB per extent)

; ***************************

;ROOT_DIR_INODE_NUMBER equ 41
ROOT_DIR_INODE_NUMBER equ 1 ; Retro UNIX 386 v2 - 18/09/2019

; DTA (PSP+80h= Offset 128)
DTA_Attrib equ 149 ; PDP+21 ;05/01/2013	
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

;err_INVALIDDATA equ 100h
;err_NOFREEBLOCK equ 200h

;;i_flags    equ 001Eh  ; Retro UNIX v1 (00011110b)
;i_flags     equ 01EDh  ; Retro UNIX v2 (0000000111101101b)	

.8086

UNIXCOPY  SEGMENT PUBLIC 'CODE'
	assume cs:UNIXCOPY,ds:UNIXCOPY,es:UNIXCOPY,ss:UNIXCOPY

	org 100h

START_CODE:

proc_start  proc near
	; 23/01/2020
	; 01/01/2020
	; 17/12/2019 - Retro UNIX 386 v2 (RUFS for hard disks)
	;
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
              
	mov	si, offset 80h             ; PSP command tail
	lodsb
	mov	cl, al
	or	cl, cl
	jnz	short loc_get_args
	dec	cl
	jmp	short loc_hd_copy_7 ; loc_unix_welcome ; 24/12/2019         
loc_get_args:
	lodsb
	cmp	al, ' '	      
	jne	short loc_hd_copy_1

	;dec	cl		  
	;jz	short loc_unix_welcome

	jmp	short loc_get_args

loc_hd_copy_1:
	cmp	al, '-'
	jne	short loc_hd_copy_2
	jmp	check_hdi_option
	
loc_hd_copy_2:
	cmp	al, 'h'
	jne	short loc_hd_copy_3
	;inc	si
	;mov	al, byte ptr [si]
	lodsb
	cmp	al, 'd'
	jne	short loc_hd_copy_7
	;inc	si
	;mov	ax, word ptr [si]
	lodsw
	cmp	al, '0'	            
	jb	short loc_hd_copy_7
	cmp	al, '3'
	ja	short loc_hd_copy_7
	cmp	ah, 20h
	ja	short loc_hd_copy_7
	;mov	byte ptr [RUFS_DRIVE], al
	sub	al, '0'
	jmp	short loc_hd_copy_5
loc_hd_copy_3:
	; 23/01/2020
	cmp	byte ptr [si], ':'		; C:, D:, E:, F:
	jne	short loc_hd_copy_7
	inc	si
	cmp	byte ptr [si],20h
	ja	short loc_hd_copy_7
	
	cmp	al, 'C'
	jb	short loc_hd_copy_7
	cmp	al, 'F'	             ; A - Z
	jna	short loc_hd_copy_4	    
	cmp	al, 'c'	             ; a - z 
	jb	short loc_hd_copy_7	 
	cmp	al, 'f'	           
	ja	short loc_hd_copy_7	 

	sub	al, 'a'-'A'	         ; to upper case
loc_hd_copy_4:
	;mov	byte ptr [RUFS_DRIVE], al
	sub	al, 'C'	             ; make it zero based 
loc_hd_copy_5:
	add	al, 80h
	mov	dl, al
	; 01/01/2020
	;mov	byte ptr [buff_d], al
	;mov	byte ptr [PhysicalDriveNumber], al
	mov	byte ptr [Drive], al

	; Read masterboot sector to understand drive is ready or not?
	call	drive_check
	; (we will not return here if cf = 1)
	; If cf = 0, we will return here

	;mov	dl, byte ptr [buff_d]
	; 01/01/2020
	;mov	dl, byte ptr [PhysicalDriveNumber]

	mov	ah, 08h
	int	13h	; return disk parameters
	push	cs
	pop	es	; restore es
	jc	short loc_hd_copy_6

	call	set_hd_parms

	; 17/12/2019
	jmp	loc_hd_copy_9

loc_hd_copy_7:
	; 24/12/2019
	jmp	loc_unix_welcome

runix_partition_error:
	; 24/12/2019
	mov	si, offset msg_inv_partition
	jmp	short @f

check_hdi_option:
	lodsb
	cmp	al, 'i'
	jne	short loc_hd_copy_7
	dec	cl
	jz	short loc_hd_copy_7
	lodsb
	cmp	al, ' '
	jne	short loc_hd_copy_7
	dec	cl
	jz	short loc_hd_copy_7
check_hdi_opt_loop:
	lodsb
	cmp	al, 20h
	ja	short get_hdi_name
	jb	short loc_hd_copy_7
	dec	cl
	jz	short loc_hd_copy_7
	jmp	short check_hdi_opt_loop

get_hdi_name:
	mov	di, offset img_file_name
get_hdi_name_nxt_chr:
	stosb
	dec	cl
	jz	short get_hdi_name_ok	
	lodsb
	cmp	al, 20h
	jna	short get_hdi_name_ok	
	cmp	di, offset img_file_name + 12	
	jb	short get_hdi_name_nxt_chr

	jmp	invalid_file_name

get_hdi_name_ok:
	sub	al, al
	stosb 

	; 03/10/2019	
	jmp	short cap_file_name
	
loc_hd_copy_6: 
	mov	al, byte ptr [Error]
	call	proc_hex
	mov	word ptr [str_Err], ax
	mov	si, offset Msg_Disk_RW_Error
	call	PRINT_MSG
	mov	si, Offset Msg_Error_Number
@@:
	call	PRINT_MSG

close_file_then_terminate:	
	mov	bx, word ptr [img_file_handle]
	and	bx, bx
	jz	short terminate
close_img_file:
	mov	ah, 3Eh ; close (floppy disk image) file
	int	21h	 
terminate:
	mov	si, offset UNIX_CRLF
loc_hd_copy_8:
	call	PRINT_MSG

	int	20h

infinive_loop:
	jmp	short infinive_loop

loc_unix_welcome:
	; 24/12/2019
	mov	si, offset UNIX_Welcome
	call	PRINT_MSG
	
	mov	si, offset usage
	jmp	short loc_hd_copy_8

cap_file_name:
	; file name capitalization
	mov	si, offset img_file_name
	mov	di, si
	mov	bx, si
cap_file_name0:
	lodsb
	cmp	al, 'a'
	jnb	short cap_file_name2
	and	al, al
	jz	short cap_file_name3
	cmp	al, '.'
	jne	short cap_file_name1
	mov	bx, di ; dot position	
cap_file_name1:
	;stosd
	inc	di ; 06/09/2019
	jmp	short cap_file_name0 
cap_file_name2:
	cmp	al, 'z'
	ja	short cap_file_name1
	and	al, 0DFh ; NOT 32
	stosb
	jmp	short cap_file_name0
cap_file_name3:
	mov	[di], al
	dec	di
	cmp	bx, di
	jnb	short invalid_file_name
	sub	di, bx
	sub	bx, offset img_file_name
	cmp	di, 3
	jna	short cap_file_name4
	and	bx, bx
	jnz	short invalid_file_name
	jmp	short open_image_file ; 03/10/2019	

file_error:
	mov	si, offset file_error_msg
jump_back: ; 24/12/2019
	jmp	short @b

cap_file_name4:
	cmp	bx, 8
	ja	short invalid_file_name

	; 03/10/2019
open_image_file:
	; 05/09/2019 - Retro UNIX 386 v2
	; 07/07/2015 (UNIXCOPY.ASM)
	mov	dx, offset img_file_name
	mov	cx, 3Fh ; File Attributes
	;xor	cx, cx ; 06/09/2019
	mov	ah, 4Eh ; MS DOs Function = Find First File
	int	21h
	jc	short file_not_found_error

;; DTA (PSP+80h= Offset 128)
;DTA_Attrib equ 149 ; PDP+21 ;05/01/2013	
;DTA_Time equ 150 ; PSP+22
;DTA_Date equ 152 ; PSP 24
;DTA_FileSize equ 154 ; PSP + 26
;DTA_FileName equ 158 ; PSP + 30

chk_image_file_features:
	mov	si, DTA_Attrib
	mov	al, byte ptr [si]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	jnz	short file_attr_error       
	mov	si, DTA_FileSize
	; 03/10/2019
	lodsw
	mov	dx, ax
	lodsw
	xchg	ax, dx

	; 23/12/2019
	test	ax, 511
	jnz	short invalid_image_file
		 ; end of file is not on sector boundary!

	mov	word ptr [file_size], ax
	mov	word ptr [file_size+2], dx

	mov	al, 2 ; open for reading and writing
	mov	dx, offset img_file_name
	mov	ah, 3Dh ; open file
	int	21h
	jc	short file_not_found_error

	mov	word ptr [img_file_handle], ax

	mov	ah, 3Fh ; read file
	mov	bx, word ptr [img_file_handle]
	mov	cx, 512
	mov	dx, offset Buffer
	int	21h
	jc	short file_error

	call	check_hd_image_size
	jnc	short valid_hd_image

invalid_image_file:
	mov	si, offset msg_inv_image_file
	;jmp	short @b
	jmp	short jump_back ; 24/12/2019
file_not_found_error:
	mov	si, offset msg_file_not_found
	;jmp	short @b
	jmp	short jump_back ; 24/12/2019
file_attr_error:
	mov	si, offset msg_file_attr_error
	;jmp	short @b
	jmp	short jump_back ; 24/12/2019
invalid_file_name:
	mov	si, offset msg_inv_file_name
	;jmp	short @b
	jmp	short jump_back ; 24/12/2019

valid_hd_image:
	; 23/12/2019
	; 06/09/2019
	; set (buffer) disk drive number for image file
	;mov	byte ptr [buff_d], 90h ; floppy disk image sign
	; 03/12/2019
	mov	byte ptr [Drive], 90h ; hard disk image sign
	; 01/01/2020
	;mov	byte ptr [PhysicalDriveNumber], 90h

	; 17/12/2019
	;jmp	short loc_hd_copy_9
loc_hd_copy_9:
	; 17/12/2019
	
	call	get_runix_partition
	;jc	runix_partition_error ; 24/12/2019
	; 27/03/2022
	jnc	short loc_hd_copy_12
	jmp	runix_partition_error
loc_hd_copy_12:
	; 30/12/2019
	;mov	ax, word ptr [hidden_sectors] ; = start sector
	;mov	dx, word ptr [hidden_sectors+2] ; = start sector + 2
	;mov	word ptr [systm.sb_HiddenSects], ax
	;mov	word ptr [systm.sb_HiddenSects+2], dx
	; 14/01/2020
	; mov	word ptr [systm.sb_BootSectAddr], ax
	; mov	word ptr [systm.sb_BootSectAddr+2], dx 

	; read runix (RUFS) boot sector
	xor	ax, ax
	xor	dx, dx	
	; dx:ax = 0 = boot sector address (in runix partition)
	mov	bx, offset boot_sector ; offset BSBUFFER
	call	dskrd
	;jc	loc_hd_copy_6  ; Drive not ready or read error
		  	       ; or Disk r/w error
	; 27/03/2022
	jc	short loc_hd_copy_err

	; 25/12/2019
	; check runix fs boot sector is valid or not!
	cmp	word ptr [boot_sector.bsFSystemID], 'UR'  ; 'RUFS'
	jne	short loc_hd_copy_10 ; Not a valid RUFS	
	cmp	word ptr [boot_sector.bsFSystemID+2], 'SF'
	jne	short loc_hd_copy_10
	cmp	word ptr [boot_sector.bsDriveID], 'dh' ; 'hd'
	jne	short loc_hd_copy_10		
	cmp	byte ptr [boot_sector.bsMagic], '@' ; ! magic byte !
	jne	short loc_hd_copy_10
	;cmp	byte ptr [boot_sector.bsPartitionID], 71h ; Runix HDFS ?
	;jne	short loc_hd_copy_10

	; valid runix boot sector, lets read Super Block	

	;add	ax,1
	;adc	dx,0
	; 30/12/2019
	; dx:ax = 0
	;mov	ax,1
	;xor	dx,dx
	inc	ax

	mov	bx,offset super_block ; offset systm
	call	dskrd
	;jc	loc_hd_copy_6  ; Drive not ready or read error
		  	       ; or Disk r/w error
	; 27/03/2022
	jnc	short loc_hd_copy_14
loc_hd_copy_err:
	jmp	loc_hd_copy_6
loc_hd_copy_14:
	; Check super block is valid or not
	cmp	word ptr [systm.sb_Header], 0171h
	jne	short loc_hd_copy_10
	cmp	byte ptr [systm.sb_Header+2], 0A1h
	jne	short loc_hd_copy_10
	cmp	word ptr [systm.sb_Footer], 0A100h
	jne	short loc_hd_copy_10
	cmp	word ptr [systm.sb_Footer+2], 7101h
	;jne	loc_hd_copy_10 ; Not a valid RUFS
	je	short loc_hd_copy_11
	; 24/12/2019
loc_hd_copy_10:
	mov	si, offset msg_not_runix_fs ; Not a valid RUFS
	jmp	@b
	; 23/12/2019
loc_hd_copy_11:
	; Initialize basic RUFS buffers

	; Inode map buffer
	; 30/12/2019
	mov	ax, word ptr [systm.sb_InodeMapAddr]
	mov	dx, word ptr [systm.sb_InodeMapAddr+2]
	mov	bx, offset im_buffer
	call	dskrd
	;jc	loc_hd_copy_6  ; Drive not ready or read error
		  	       ; or Disk r/w error
	; 27/03/2022
	jc	short loc_hd_copy_err

	;xor	ax, ax
	;mov	word ptr [im_sector],ax ; 0

	; Free blocks map buffer
	; 30/12/2019
	mov	ax, word ptr [systm.sb_FreeMapAddr]
	mov	dx, word ptr [systm.sb_FreeMapAddr+2]
	mov	bx, offset fbm_buffer
	call	dskrd
	;jc	loc_hd_copy_6  ; Drive not ready or read error
		  	       ; or Disk r/w error
	; 27/03/2022
	jc	short loc_hd_copy_err

	;xor	ax, ax
	;mov	word ptr [fbm_sector],ax ; 0
	;mov	word ptr [fbm_sector+2],ax ; 0

	; Inode table buffer (read the first 8 inodes to the buffer)
	; 30/12/2019
	mov	ax, word ptr [systm.sb_InodeTblAddr]
	mov	dx, word ptr [systm.sb_InodeTblAddr+2]
	mov	bx, offset I_buffer ; 24/12/2019
	call	dskrd
	;jc	loc_hd_copy_6  ; Drive not ready or read error
		  	       ; or Disk r/w error
	; 27/03/2022
	jc	short loc_hd_copy_err

	; 24/12/2019
	;;xor	ax, ax
	;;mov	word ptr [I_sector], ax ; 0
	;mov	word ptr [I_sector], 0
	mov	byte ptr [I_valid], 1 ; inode sector validation, valid

	; 17/12/2019	
	; OK! Lets go to command interpreter (prompt) stage
	
	;jmp	loc_call_unix_prompt

;	int	20h
;
;	; 06/09/2019
;hang:
;	jmp	short hang

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; call	command interpreter
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

loc_call_unix_prompt:
	; 25/09/2019
	mov	si, offset type_27h
	call	PRINT_MSG

	call	unix_prompt

	; 07/07/2015
	jmp	close_file_then_terminate ; 24/12/2019

proc_start endp

; 17/12/2019 - UNIXHDFS.ASM (14/12/2019)

set_hd_parms	proc near
	; 02/10/2019 - Retro UNIX 386 v2
	; Input:
	;  (output of INT 13h, AH=08h Read Drive Parameters)
	;   DL = number of hard disk drives 
	;   DH = logical last index of heads
	;      = number_of - 1 (because index starts with 0)
	;   CX = [7:6][15:8] = logical last index of cylinders
	;	 = number_of - 1 (because index starts with 0)
	;        [5:0] = logical last index of sectors per track
	;	 = number_of (because index starts with 1)
	; Output:
	;	CHS parameters are set
	;
	; Modified registers: ax,(bx),cx,dx,(es),(di)

	xor	ah,ah
	mov	al,dh
	inc	ax
	mov	word ptr [heads],ax
	mov	ax,cx
	and	cx,63
	mov	word ptr [sectors],cx	
	xchg	ah,al
	and	ah,0C0h
 	rol	ah,1 	
	rol	ah,1
	inc	ax
	mov	word ptr [cylinders],ax	

	;push	ds
	;pop	es

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

	;mov	dl,byte ptr [buff_d]
	; 01/01/2020
	;mov	dl,byte ptr [PhysicalDriveNumber]

	;xor	ah,ah
	;int	13h
	;jnc	short reset_ok
;harddisk_error:
	;retn
;reset_ok:
	;mov	bx,offset Buffer
	;mov	ax,0201h
	;mov	cx,1
	;;mov	dl,byte ptr [buff_d]
	;;mov	dl,byte ptr [drive] ; 03/12/2019
	;xor	dh,dh
	;;push	ds
	;;pop	es
	;int	13h
	;jc	short harddisk_error

; Here is the entry point for hard disk image (no int13h!)

	cmp	word ptr [Buffer+510],0AA55h
	je	short valid_masterboot
not_masterboot:
	stc
	retn
valid_masterboot:
	; 03/10/2019
	mov	si,offset Buffer+01BEh+4 
		; partition type byte in partition table
check_runix_partition:
	cmp	byte ptr [si],71h
	je	short runix_partition_ok
	cmp	si,offset Buffer+01BEh+48+4
	jnb	short not_runix_partition
	add	si,16
	jmp	short check_runix_partition
not_runix_partition:
	stc
	retn	
runix_partition_ok:
	add	si,4
	
	;mov	ax,word ptr [si] ; start sector (of the runix partition)
	;mov	word ptr [hidden_Sectors],ax
	;mov	dx,word ptr [si+2]
	;mov	word ptr [hidden_Sectors+2],dx
	;add	si,4
	
	lodsw
	mov	dx,ax
	lodsw
	xchg	ax,dx
	mov	word ptr [hidden_Sectors],ax
	mov	word ptr [hidden_Sectors+2],dx
	  	  
	mov	bx,word ptr [si] ; total sectors (volume size)
	mov	word ptr [total_Sectors],bx
	mov	cx,word ptr [si+2]
	mov	word ptr [total_Sectors+2],cx

	; 03/10/2019
	add	cx,ax
	adc	bx,dx
		
	mov	ax, word ptr [sectors]
	mov	dx, word ptr [heads]
	mul	dx  ; max. possible value 255*63 = 16065
		; dx = 0
	mov	dx, word ptr [cylinders]
	mul	dx
	sub	ax, 1
	sbb	dx, 0
	     ; dx:ax = CHS limit (max. = 16065*1024-1 = 16450559)

	; 01/12/2019
	mov	word ptr [CHS_limit], ax
	mov	word ptr [CHS_limit+2], dx
	
	cmp	dx, bx
	ja	short chs_ok
	jb	short lba_ok
	cmp	ax, cx
	jnb	short chs_ok
lba_ok:
	mov	byte ptr [lba_rw], 1
	clc	
chs_ok:
	retn

get_runix_partition endp

check_hd_image_size proc near
	; 23/12/2019
	; 03/10/2019 - Retro UNIX 386 v2
	; DX:AX = image file size (in bytes)

	; Source code (derived) from
	; hdimage.s - 06/03/2019
	; (TRDOS 386 Hard Disk Image Formatting Utility)

	;test 	ax, 511
	;jnz	short B_04 ; end of file is not on sector boundary!

	;mov	word ptr [file_size], ax
	;mov	word ptr [file_size+2], dx

	; 23/12/2019
	mov	ax, word ptr [file_size]
	mov	dx, word ptr [file_size+2]

	; Check for Singlix Master Boot code
	cmp	word ptr [Buffer+444], 7BEh
	jne	short A_15 ; no ..

	; It is seen as singlix MBR
	; Let's check for disk size words (CHS record)

	; convert (disk image) file size to sector count (disk size)
	; (dx:ax)/512
	
	mov	cx, 512
	call	div32

	; 12/02/2019
	mov	word ptr [total_sectors], ax
	mov	word ptr [total_sectors+2], dx

	mov	cx, word ptr [Buffer+420] ; cylinders
	cmp	cx, 16
	jb	short A_15 ; invalid
	mov	ax, word ptr [Buffer+422] ; heads
	cmp	ax, 2
	jb	short A_15 ; invalid
	mov	dx, word ptr [Buffer+424] ; sectors
	cmp	dx, 17
	jb	short A_15 ; invalid
	mov	word ptr [cylinders], cx
	mov	word ptr [heads], ax
	mov	word ptr [sectors], dx
	mul	dx
	call	mul32
	or	bx, bx
	jnz	short A_15 ; invalid
	cmp	dx, word ptr [total_sectors+2]
	jne	short A_15
	cmp	ax, word ptr [total_sectors]
	jne	short A_15 ; invalid 

	; valid singlix MBR & disk image file

	retn
A_15:
	; cylinders*63sectors*16heads (<=528MB)
	cmp	dx, 1F80h ; 1024*16*63 (1F800000h)
	ja	short A_21
	jb	short A_16
	and	ax, ax
	jnz	short A_22

	; = 528MB (1024*16*63*512)
	;mov	word ptr [heads], 16
	;mov	ax, 1024
	;jmp	short A_25
	; 04/12/2019
	jmp	short A_27
A_16:
	; < 528MB
	mov	cx, 63*16 ; 1008
	div	cx
	and	dx, dx
	jnz	short A_17 ; 17 spt disk image check
	;mov	word ptr [heads], 16
	;jmp	short A_25
	jmp	short A_28 ; 04/12/2019
B_04:
	stc
	retn
A_17:
	; 12/02/2019
	mov	ax, word ptr [file_size]
	mov	dx, word ptr [file_size+2]

	;; 10/02/2019
	;; Check 17 spt disk image
	;mov	si, DTA_FileSize
	;lodsw
	;; max. size of hard disk image = 17sectors*16heads*1024cylinders
	;mov	dx, word ptr [si]
	
	; 04/12/2019
	cmp	dx, 880h ; 136MB upper limit (8800000h) for 17spt
	ja	short B_04

	mov	cx, 512
	call	div32

	mov	word ptr [pp_Sectors], ax
	mov	word ptr [pp_Sectors+2], dx
	
	; Calculate with increased heads (count) order
	; (For example cyls = 1024 & heads = 8 is better than
	;      cyls = 512 & heads = 16, for 17 SPT.v)

	mov	cx, 17*2 ; 34 ; heads = 2
A_18:
	call	div32
		; Ýf remainder = 0 it is 17spt hard disk image file
	and	bx, bx
	jz	short A_20
A_19:	
	add	cx, 17
	cmp	cx, 17*16 ; 272 ; heads = 16
	ja	short B_04 ; invalid image file !

	mov	ax, word ptr [pp_Sectors]
	mov	dx, word ptr [pp_Sectors+2]
	 	
	jmp	short A_18
A_21:
	; cylinders*63sectors*32heads (>528MB, <=1GB)
	cmp	dx, 3F00h ; 1024*32*63 (3F000000h)
	ja	short A_23
	jb	short A_22
	and	ax, ax
	jnz	short A_23
	; = 1GB (1024*32*63*512)
A_27:
	mov	ax, 1024
A_28:
	mov	word ptr [heads], 16
	jmp	short A_25	
A_22:
	mov	cx, 63*32
	div	cx
	and	dx, dx
	jnz	short B_04
	mov	word ptr [heads], 32
	jmp	short A_25
A_23:
	; cylinders*63sectors*64heads (>1GB, <=2GB)
	mov	cx, 63*64
	div	cx
	and	dx, dx
	jnz	short B_04
A_24:
	mov	word ptr [heads], 64
A_25:
	mov	word ptr [sectors], 63
	mov	word ptr [cylinders], ax
A_26:
	; 08/02/2019

	; calculate total sectors (by using CHS values)
	mov	ax, word ptr [sectors]
	mul	word ptr [heads]
	mov	word ptr [min_sectors], ax ; Minimum sectors
	mov	dx, word ptr [cylinders]
	mul	dx
	mov	word ptr [total_sectors], ax
	mov	word ptr [total_sectors+2], dx
	
	clc
	retn
A_20:
	cmp	ax, 1024
	ja	short A_19

	mov	bl, 17
	mov	byte ptr [sectors], bl ; 17
	mov	word ptr [cylinders], ax

	mov	ax, cx
	div	bl
	;xor	ah, ah
	mov	word ptr [heads], ax

	jmp	short A_26

check_hd_image_size endp

PRINT_MSG	proc near
	mov	bx,07h  
	mov	ah,0Eh  
PRINT_MSG_LOOP:
	lodsb		; Load byte at DS:SI to AL
	and	al,al            
	jz	short PRINT_MSG_OK       
	
	int	10h	; BIOS Service func (ah) = 0Eh
			; Write char as TTY
			;AL-char BH-page BL-color
	jmp	short PRINT_MSG_LOOP           
PRINT_MSG_OK:
	retn

PRINT_MSG	endp

proc_hex	proc near

	db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
	or	ax,'00'	; Make it ZERO (ASCII) based

	xchg	ah,al 
; 1999
	cmp	al,'9'
	jna	short pass_cc_al
	add	al,7
pass_cc_al:
	cmp	ah,'9'
	jna	short pass_cc_ah
	add	ah,7
pass_cc_ah:

; 1998
	retn

proc_hex	endp

drive_check:
	; 03/12/2019
	; 03/10/2019
	; 09/09/2019 - Retro UNIX 386 v2
	
	; DL = Hard disk drive number 
	
	;;xor	bx, bx ; sector 0
	;;mov	word ptr [buff_s], bx
	;;mov	word ptr [buff_o], offset Buffer
	;;call	dskrd

	; 03/10/2019

	;xor	ah,ah
	;int	13h
	;jc	short drive_not_ready
reset_ok:
	mov	bx,offset Buffer
	mov	ax,0201h
	mov	cx,1
	;mov	dl,byte ptr [buff_d]
	;mov	dl,byte ptr [Drive] ; 03/12/2019
	; 01/01/2020
	;mov	dl,byte ptr [PhysicalDriveNumber]

	xor	dh,dh
	;push	ds
	;pop	es
	int	13h
	jnc	short drive_check_ok
drive_not_ready:
	; 09/09/2019
	pop	ax ; near call	return address
	mov	si, offset msg_drive_not_ready
	call	PRINT_MSG
	; 24/12/2019
	jmp	terminate
drive_check_ok:
	retn

shl32:
	; 23/01/2020
	; 16/10/2019
	; 18/09/2019 (UNIXCOPY.ASM)
	; INPUT:
	;   CL = shift count
	;xor	ch,ch
	jcxz	norotal
rotashftl:
	; 23/01/2020
	shl	ax,1
	rcl	dx,1
	loop	rotashftl
norotal:
	retn

shr32:
	; 16/10/2019
	; INPUT:
	;   CL = shift count
	;xor	ch,ch
	jcxz	norotar
rotashftr:
	shr	dx,1
	rcr	ax,1
	loop	rotashftr
norotar:
	retn

; ***

unix_prompt proc near
	; 01/01/2020
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
	;cmp	byte ptr [PhysicalDriveNumber],90h
	; 01/01/2020
	cmp	byte ptr [Drive],90h
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
	mov	si,offset img_file_name
	call	PRINT_MSG
	mov	si,offset unix_img_cdir
	jmp	short unix_prompt_15
unix_prompt_1:
	mov	si,offset unix_cdrv
	call	PRINT_MSG
unix_prompt_2:
	mov	si,offset unix_cdir
unix_prompt_15:
	call	PRINT_MSG
unix_prompt_3:
	mov	al,byte ptr [unix_prompt_char]
 	;mov	ah,0Eh	  
	;mov	bx,07h             
	int	10h         
unix_prompt_4:     
	mov	ah,03h
	;mov	bx,07h	  
	int	10h
	mov	byte ptr [CursorColumn],dl
unix_prompt_5:
	mov	si,offset CommandBuffer
	call	proc_rw_char
	;mov	byte ptr [CommandBuffer]+75,0

	;mov	si,offset CommandBuffer
	mov	di,si
	xor	bx,bx
	xor	cx,cx
unix_prompt_6:
	mov	al,byte ptr [si][bx]
	inc	bl 
	cmp	al,20h
	ja      short unix_prompt_8
	jb      short unix_prompt_13
	cmp	bl,74 ; 75 ?
	jb      short unix_prompt_6
	jmp	short unix_prompt_13
unix_prompt_7:
	mov	al,byte ptr [si][bx]
	inc	bl
	cmp	al,20h
	jna     short unix_prompt_9
unix_prompt_8:
	stosb   
	inc	cl
	cmp	bl,74 ; 75 ?
	jb      short unix_prompt_7
	;jmp	short unix_prompt_12
unix_prompt_9:
	xor	al,al ; 0
unix_prompt_10:
	mov	byte ptr [di],al
	inc	di
	cmp	bl,74 ; 75 ?
	jnb	short unix_prompt_12
	mov	al,byte ptr [si][bx]
	inc	bl
	cmp	al,20h
	jnb 	short unix_prompt_10
unix_prompt_11:
	mov	byte ptr [di],0
unix_prompt_12:
	call	command_interpreter

	cmp	byte ptr [program_exit],1
	jnb	short unix_prompt_14

	mov	cx,74 ; 75 ?
	mov	di,offset CommandBuffer
	xor	al,al
	rep	stosb
unix_prompt_13:
	mov	bx,07h 
	mov	al,0Dh
	mov	ah,0Eh
	int	10h
	mov	al,0Ah
	int	10h
	
	jmp	unix_prompt_0 ; loop

unix_prompt_14:
	retn

unix_prompt	endp

proc_rw_char proc near
	       ; 8/12/2012 (modification for UNIXCOPY.ASM)
	       ; OUTPUT -> DS:SI = Entered String (ASCIIZ)
read_next_char:
	xor	ah,ah
	int	16h
	and	al,al
	jz	short loc_arrow    
	cmp	al,0E0h          
	je	short loc_arrow
	cmp	al,08h
	jne	short char_return
loc_back:
	mov	bl,7
	mov	ah,3
	int	10h
	cmp	dl,byte ptr [CursorColumn]
	ja	short prev_column
loc_beep:
	mov	ah,0Eh
	mov	al,7
	int	10h
	jmp	short read_next_char
prev_column:
	dec	dl
set_cursor_pos:
	mov	ah,02h
	int	10h
	mov	bl,dl
	sub	bl,byte ptr [CursorColumn] 
	mov	cx,1
	mov	ah,09h
	mov	al,20h
	mov	byte ptr [si][bx],al
loc_writeit:
	mov	bl,7
	int	10h
	mov	dx,word ptr [CursorColumn]
	jmp	short read_next_char
loc_arrow:    
	cmp	AH,4Bh
	je	short loc_back
	cmp	AH,53h
	je	short loc_back
	jmp	short read_next_char
char_return:
	mov	bl,7
	mov	ah,3
	int	10h

	mov	ah,dl	
	sub	ah,byte ptr [CursorColumn] 
	cmp	al,20h
	jb	short loc_escape
	cmp	ah,72 ; limit
	ja	short loc_beep

	mov	bl,ah
	       xor	ah,ah
	mov	word ptr [si][bx],ax
	mov	ah,0Eh
	mov	bl,7
	int	10h
	jmp	short read_next_char
pass_escape:
	cmp	al,0Dh
	jne	short read_next_char
	       mov	ah,0Eh	
	mov	bl,7
	int	10h
	mov	al,0Ah
	int	10h
	retn
loc_escape:
	cmp	al,1Bh
	jne	short pass_escape
	stc
	retn

proc_rw_char endp

command_interpreter proc near
	; 22/01/2020
	; 15/01/2020
	; 05/01/2020
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
show_uf_err:
	jmp	ci_error
@@:
	retn
; LINK
loc_cmd_link:
	cmp	ax, 'il'
	;jne	loc_cmd_iget ; 17/02/2013
	; 27/03/2022
	je	short link_sf0
	jmp	loc_cmd_iget
link_sf0:
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
	jnc	short @f
link_err:
	jmp	ci_error
@@:
	;mov	word ptr [uf_i_number], ax
	; 29/12/2019
	mov	word ptr [uf_i_number], bx
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
	
	;mov	ax, word ptr [uf_i_number]
	; 26/12/2019
	mov	bx, word ptr [uf_i_number]	
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short link_err

	; 19/09/2019
	mov	byte ptr [imodx], 1 ; 19/09/2019 - Retro UNIX 386 v2
		; (flag means file data is same
		;  but inode's itself is modified)

	inc	byte ptr [inode_nlks] ; link count

        call	setimod ; jsr r0,setimod / set modified flag

	;inc	byte ptr [inode_nlks] ; link count

	;mov	ax, word ptr [pdir]
	; 26/12/2019
	mov	bx, word ptr [pdir]
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short link_err

	; namei -> u_namep points filename 
	;          after the last '/' of the path
	
	mov	ax, word ptr [uf_i_number]
	mov	word ptr [u_dirbuf], ax

	call	mk_dir ; make directory entry
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
	je	short ci_iget_getarg
	; 27/03/2022
	jb	short loc_cmd_help
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
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
;@@:
;	retn  
cl5:
	cmp	cl, 5
	;ja	cl7
	;;jb	short @f
	; 27/03/2022
	jna	short loc_cmd_chdir
	jmp	cl7
; CHDIR
loc_cmd_chdir:
	cmp	ax, 'hc'
	;jne	loc_cmd_todos
	; 27/03/2022
	je	short ci_chdir_chk
	jmp	loc_cmd_todos
ci_chdir_chk:
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
	;dec	si

	mov	ax, word ptr [u_namep]
	mov	word ptr [arg], ax

	call	sys_chdir
	;jc	ci_error
	; 27/03/2022
	jc	short chdir_err
	
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
chmod_err:
chdir_err:
chown_err:
chgrp_err:
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
	jc	short chown_err
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
	jc	short chgrp_err

	and	bx, bx
	jnz	short ci_chgrp_print
@@:
	retn
; TODOS
loc_cmd_todos:
	; 24/12/2012
	cmp	ax, 'ot'
	;jne	loc_cmd_mkdir ; 30/12/2012
	; 27/03/2022
	je	short todos_uf0
	jmp	loc_cmd_mkdir
todos_uf0:
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
	;;jnc	short @f ; *
	;jc	ci_error
	; 27/03/2022
	jnc	short @f
todos_err1:
	jmp	ci_error
@@:
	;cmp	ah, 0FFh
	;jne	ci_error
	;jmp	ci_error ; 'file not found' error
@@: ; *
	;mov	word ptr [uf_i_number], ax
	; 29/12/2019
	mov	word ptr [uf_i_number], bx
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
	ja	short todos_err1

	jmp	short todos_crdf

@@:	; 05/01/2013
	mov	si, DTA_Attrib
	mov	al, byte ptr [SI]
	and	al, 1Fh ; directory, volume label, system, hidden, read only
	;jnz	ci_error
	; 27/03/2022
	jnz	short todos_err1
todos_afow:	; overwrite question
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
	int	16h	; wait for keyboard command
	cmp	al, 'C'-40h
	je	short @f	   
	cmp	al, 27
	je	short @f
	and	al, 0DFh
	cmp	al, 'Y'	; Yes?
	je	short todos_afow_yes ; overwrite
	cmp	al, 'N'	; No?
	jne	short todos_afow_input         
todos_afow_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
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
	mov	ah, 3Ch ; MS Dos Function = Create File
	int	21h
	;jc	ci_error
	; 27/03/2022
	jc	short todos_err2

	mov	word ptr [FileHandle], ax
todos_odf:
	mov	dx, word ptr [arg]
	mov	ah, 3Dh ; MS Dos Function = Open File
	xor	al, al  
	int	21h
	;jc	ci_error
	; 27/03/2022
	jnc	short todos_ruf_wdf
todos_err2:
	jmp	ci_error

todos_ruf_wdf:
	xor	ax, ax ; 0 
	mov	word ptr [u_off], ax
	        ; 15/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019
todos_wf_msg:
	mov	si, offset Msg_writing_file
	call	PRINT_MSG
todos_iget:	
	;mov	ax, word ptr [uf_i_number]
	; 26/12/2019
	mov	bx, word ptr [uf_i_number]
	call	iget
	jc	short todos_cdf

	; 15/01/2020
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

	;mov	ax, word ptr [uf_i_number] ; word ptr [u_dirbuf]
	; 26/12/2019
	mov	bx, word ptr [uf_i_number] ; word ptr [u_dirbuf]
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

	;cmp	ax, cx ; write count = read count ? 
	;jne	short todos_cdf ; jb short todos_cdf

	;or	ax, ax  ; or cx, cx
	;;jnz	short loc_read_unix_sf ; ax = 512
	;; 15/01/2020
	;jnz	short todos_read_unix_sf_1

	; 15/01/2020
	;cmp	ax, 512
	cmp	cx, 512 ; no..
	je	short todos_read_unix_sf_1 
			; there may be next sector to read

	cmp	ax, cx	; write count <= read count ? 
	;jb	short todos_cdf
	
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
	jmp	ci_error
todos_retn:
	mov	si, offset Msg_OK
	call	PRINT_MSG

	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG	
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
	jc	short ci_mkdir_err

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
	jb	short ci_rmdir_err

	cmp	ah, al ;'.' ; dotdot (parent dir)
	jne	short @f

	inc	si
	cmp	byte ptr [SI], 21h
	;jb	ci_error
	jnb	short @f
ci_mkdir_err:
ci_rmdir_err:
namei_err:
 	jmp	ci_error
@@:
	; u_namep = pointer to directory path name

	call	namei
	;jc	ci_error
	; 27/03/2022
	jc	short ci_rmdir_err

	;cmp	ax, ROOT_DIR_INODE_NUMBER
	cmp	bx, ROOT_DIR_INODE_NUMBER ; 29/12/2019
	;je	ci_error
	; 27/03/2022
	je	short ci_rmdir_err

	;cmp	ax, word ptr [u_cdir]
	cmp	bx, word ptr [u_cdir] ; 29/12/2019
	;je	ci_error
	; 27/03/2022
	je	short ci_rmdir_err

	;push ax
	push	bx ; 29/12/2019
	mov	si, offset Msg_Removing_Directory
	call	PRINT_MSG
	pop	ax

	call	remove_directory
	;jc	ci_error
	; 27/03/2022
	jc	short ci_rmdir_err

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
	jb	short namei_err

	mov	si, offset NotFound_msg
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
@@:
	retn
namei_iget:
	; 26/12/2019
	;mov	bx, ax
	; bx = inode number
	call	iget
namei_print_inum:
	;jc	ci_error
	; 27/03/2022
	jc	short namei_err

	;mov	cx, ax
	mov	cx, bx ; 29/12/2019
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
	jnc	short @f
inode_err:
	jmp	ci_error
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
	mov	ax, bx ; 29/12/2019
	; ax = i-number
dir_print:
	call	print_directory_list
	jnc	short @f
ci_error:
	mov	si, offset error_msg
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
@@:	
	retn
; 23/02/2013
cl1:
	cmp	al, '?'
	jne	short @b
	cmp	ah, 0
	;je	ci_?
	; 27/03/2022
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
	mov	ax,3
	int	10h
@@:
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
	cmp	al, 'l'
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
	;jne	loc_cmd_fs ; 16/02/2013
	; 27/03/2022
	je	short rm_chr3
	jmp	loc_cmd_fs
rm_chr3:
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
	jc	short rm_err
@@:
	;cmp	word ptr [ii], 41	; i-number of the directory
	;jne	short @f

	mov	si, offset BSBuffer.bs_BF_I_number
	;cmp	ax, word ptr [si]	; is it i-number of the boot file
	cmp	bx, word ptr [si] ; 29/12/2019
	;je	short ci_error
	jne	short @f

	;cmp	word ptr [ii], 41	; i-number of root directory
	cmp	word ptr [ii], 1	; 25/09/2019 - Retro UNIX 386 v2
	;je	ci_error
	; 27/03/2022
	je	short rm_err

	mov	word ptr [si], 0  ; reset wrong boot file configuration
@@:	
	;mov	word ptr [uf_i_number], ax ; word ptr [u_dirbuf]
	; 29/12/2019
	mov	word ptr [uf_i_number], bx	

	; 05/01/2013
	mov	dx, word ptr [ii]
	mov	word ptr [pdir], dx
	; 26/12/2019
	;mov	bx, ax ; inode number
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short rm_err
;@@:
	mov	ax, word ptr [inode_flgs]
	; 25/09/2019 - Retro UNIX 386 v2
	;	(modified unix v7 inode format)
	test 	ah, 80h
	;jz	ci_error  ; not regular file or directory
	; 27/03/2022
	jz	short rm_err
	
	test 	ah, 40h ; 'directory' flag
	;jnz	ci_error
	; 27/03/2022
	jnz	short rm_err

	test 	al, 80h ; owner's write permission
	;jz	ci_error  ; read only file !
	; 27/03/2022
	jnz	short rm_move_fn
rm_err:
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
	int	16h		; wait for keyboard command
	cmp	al, 'C'-40h
	je	short @f	   
	cmp	al, 27
	je	short @f
	and	al, 0DFh
	cmp	al, 'Y'		; Yes?
	je	short rm_a_yes	; overwrite
	cmp	al, 'N'		; No?
	jne	short rm_yn_input         
rm_a_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
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
	jc	short rm_err
	
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
	;jb	cl6 ; 16/02/2013
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
	mov	word ptr [uf_i_number], ax ; 0
	jmp	fromdos_s_fs_mdt
@@:
	;mov	word ptr [uf_i_number], ax
	; 29/12/2019
	mov	word ptr [uf_i_number], bx

	; 30/09/2019
	; 05/01/2013
	;mov	dx, word ptr [ii]
	;mov	word ptr [pdir], dx
	; 26/12/2019
	;mov	bx, ax ; inode number
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short fromdos_err

	mov	ax, word ptr [inode_flgs]
	; 25/09/2019 - Retro UNIX 386 v2
	;	(modified unix v7 inode format)
	test 	ah, 80h
	;jz	ci_error  ; not regular file or directory	
	; 27/03/2022
	jz	short fromdos_err

	test 	ah, 40h ; 'directory' flag
	;jnz	ci_error
	; 27/03/2022
	jnz	short fromdos_err

	test 	al, 80h ; owner's write permission
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
	loop @b
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
	cmp	al, 'Y'		; Yes?
	je	short fromdos_afow_yes    	; overwrite
	cmp	al, 'N'	     ; No?
	jne	short fromdos_afow_input         
fromdos_afow_no:
	mov	si, offset msg_No
	call	PRINT_MSG
	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG	
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
	;mov	bx, word ptr [pdir] ; 26/12/2019 (ax -> bx)
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
	;;mov	si, DTA_FileSize
	;mov	si, DTA_FileSize+2
	;;mov	ax, word ptr [si]
	;;mov	dx, word ptr [si]+2
	;;or	ax, dx  ; 64KB file size limit
	;mov	ax, word ptr [si]
	;and	ax, ax
	;jnz	ci_error

	;mov	word ptr [file_Size], ax
	; 01/01/2020	
	;mov	word ptr [file_Size+2], dx

	mov	si, DTA_Date
	mov	ax, word ptr [si]
	push	ax
	and	ax, 00011111b	         ; Day Mask
	mov	word ptr [day], ax
	pop	ax
     	mov	cl, 5
	shr     ax, cl	   ; shift right 5 times
	push 	ax 
	and	ax, 00001111b            ; Month Mask
  	mov	word ptr [month], ax
	pop	ax
	mov	cl, 4
	shr	ax, cl
	;and    ax, 01111111b            ; Result = Year - 1980
	add	ax, 1980
	mov	word ptr [year], ax
 		
	mov	si, DTA_Time
	mov	ax, word ptr [si]
	push	ax
	and	ax, 0000011111b          ; Second Mask
	shl	al, 1	 
	mov	word ptr [second], ax
	pop	ax
	mov	cl, 5
	shr     ax, cl	   ; shift right 5 times
	push	ax
	and	ax, 0000111111b          ; Minute Mask
	mov	word ptr [minute], ax
	pop	ax
	mov	cl, 6	   ; shift right 6 times
	shr     ax, cl	   ; (6+5=11)
	mov	word ptr [hour], ax	 ; ax = hours	

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
fromdos_of_err:
fromdos_cf_err:
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
	mov	word ptr [u_off], ax  ; 0
	; 14/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019 
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
ght:
	mov	word ptr [u_base], offset ReadBuffer

	;mov	ax, word ptr [uf_i_number] ; word ptr [u_dirbuf]
	; 29/12/2019
	mov	bx, word ptr [uf_i_number]

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
	jc	short ci_sync_err
fromdos_retn:
	mov	si, offset Msg_OK
	call	PRINT_MSG

	mov	si, offset UNIX_CRLF
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
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

	mov	si, offset BSBuffer.bs_BF_I_number
	mov	ax, word ptr [SI]
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
ci_sync_err:
find_bfn_err:
new_bf_err:
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
	loop ci_move_bfn_2
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
	jc	short new_bf_err

	; 29/12/2019
	; bx = inode number
	
	cmp	word ptr [ii], ROOT_DIR_INODE_NUMBER
	;jne	ci_error
	; 27/03/2022
	jne	short new_bf_err

	; 05/01/2013
	; ax = i-number of (new) boot file
	;mov	bx, ax ; 26/12/2019
	call	iget
	;jc	ci_error
	; 27/03/2022
	jc	short new_bf_err

	;test 	word ptr [inode_flgs], 4000h ; directory ?
	test 	byte ptr [inode_flgs+1], 40h ; 25/09/2019
	;jnz	ci_error
	; 27/03/2022
	jnz	short new_bf_err

	;test 	byte ptr [inode_flgs+1], 80h ; 25/09/2019
	;jnz	ci_error
;@@:
	;mov	si, offset BSBuffer.bs_BF_I_number
	;;mov	word ptr [si], ax
	;; 29/12/2019
	;mov	word ptr [si], bx
	; 22/01/2020
	mov	word ptr [BSBuffer.bs_BF_I_number], bx

	; 05/01/2020
	mov	byte ptr [bmod], 0FFh ; boot sector modified!	 

	call	sync
	;jc	ci_error
	; 27/03/2022
	jc	short ci_sync_err

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
	;jne	short @f ; retn
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

	mov	si, offset BSBuffer.bs_BF_I_number

	; 22/01/2020
	mov	ax, word ptr [si]

	and	ax, ax
	;jz	ci_no_bootfile
	; 27/03/2022
	jnz	short @f
	jmp	ci_no_bootfile
@@:
	xor	ax, ax
	mov	word ptr [si], ax ; 0

	; 22/01/2020
	mov	byte ptr [bmod], 0FFh ; boot sector modified!

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
	; 18/01/2020
	; 17/01/2020
	; 16/01/2020
	; 15/01/2020
	; 29/12/2019
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
	; 26/12/2019
	mov	bx, ax
	call	iget
        jc	short @f ; 20/01/2013 ; jc short pdl_9

	;test 	word ptr [inode_flgs], 4000h ; directory i-node ?
	;jnz	short pdl_2

	; 20/09/2019
	mov	ah, byte ptr [inode_flgs+1]
	and	ah, 0C0h ; regular + directory
	cmp	ah, 0C0h ; 24/09/2019
	je	short pdl_2	
pdl_1:
	mov	ah, 0FFh ; error number
	stc
@@: ; 20/01/2013
       ;jmp	short pdl_9
        retn
pdl_2:
	;mov	ax, word ptr [inode_size]
	;mov	dx, word ptr [inode_size+2] ; 01/01/2020
	;mov	word ptr [u_dirp], ax ; put size of directory in u.dirp
	;mov	word ptr [u_dirp+2], dx ; 01/01/2020
	
	xor	ax, ax ; 0
	mov	word ptr [u_off], ax ; u.off is file offset used by user
	; 15/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019

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
	;mov	ax, word ptr [ii]

	; 26/12/2019
	mov	bx, word ptr [ii]
	 	
	call	readi ; read 16 bytes of file with i-number (R1)
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
	jz	short pdl_6 ; 3f. If char is nul, then the last char in string has
		 	    ; been compared
	stosb	; mov byte ptr [di], al ; inc di 
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
	;mov	ax, word ptr [u_dirbuf]
	; 26/12/2019
	mov	bx, word ptr [u_dirbuf]
	call	iget
	pop	di
	;jc	pdl_9
	; 27/03/2022
	jnc	short @f
	jmp	pdl_9
@@:
	; 22/01/2012
	cmp	byte ptr [ls_option], 1
	jna	short @f
        
pdl_11: ; 21/01/2013 ; Inode number
	mov	ax, word ptr [u_dirbuf]
	;mov	cx, 3 ; 03/02/2013
	; 16/01/2020
	mov	cx, 6 ; ' '+'65534' ; 1 space + 5 digits
	call	print_decimal_number
        jmp	short pdl_10
@@:
	;mov	ax, word ptr [inode_flgs]
	;test 	ah, 40h ; 'directory' flag
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
        ;xor	bh, bh ; mov	bh, 0
        mov	ah, 03h ; get cursor position and size.
        int 10h
@@:
	; 17/01/2020
	;cmp	dl, 19 ; 04/12/2015 (13 -> 19)
	cmp	dl, 22 ; 17/01/2020
	jnb	short @f
	mov	al, 20h
	call	putc
	inc	dl
	jmp	short @b

pdl_18:	; 27/03/2022
	jmp	pdl_8
@@:
	; 24/09/2019
	mov	ax, word ptr [inode_uid]
	;mov	cx, 4 ; <= 9999 ?
	mov	cx, 3 ; <= 999 ? ; 25/09/2019
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
	test 	dh, 80h
	jnz	short regular
	test 	dh, 40h
	jnz	short blockdev
	; 18/01/2020
	test 	dh, 20h
	jz	short regular_f
chardev:
	mov	al, 'c'
	jmp	short @f
blockdev:	
	mov	al, 'b'
	jmp	short @f
regular:
	mov	al,'d'
	test 	dh, 40h
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

	; 15/01/2020
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
	; mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	mov	byte ptr [bx], '/'
	mov	si, bx
	inc	si
	mov	ax, word ptr [month]
	; mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	add	bx, 3
	mov	byte ptr [bx], '/'
	mov	si, bx
	inc	si
	mov	ax, word ptr [year]
	;mov	cx, 4
	mov	cl, 4
	call	proc_bin_to_decimal

	; 16/01/2020
	mov	byte ptr [si], 0 ; 16/01/2020

	mov	si, offset dec_num
	call	PRINT_MSG

	mov	al, 20h
	call	putc

	mov	si, offset dec_num
	mov	bx, si
	mov	ax, word ptr [hour]
	; mov	cx, 2
	mov	cl, 2
	call	proc_bin_to_decimal
	add	bx, 2
	mov	byte ptr [bx],':'

	mov	si, bx
	inc	si
	mov	ax, word ptr [minute]
	; mov	cx, 2
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
	;mov	ax, word ptr [pdir]
	; 26/12/2019
	mov	bx, word ptr [pdir]
	call	iget
        jc	short pdl_9
pdl_8:
	; 15/01/2020
	; 01/01/2020 (32 bit offset)
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
	;call	PRINT_MSG
	; 22/01/2020
	jmp	PRINT_MSG
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
	; 01/01/2020
	; 29/12/2019
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

	; mov	word ptr [u_namep], si

syschdir_0:
	call	namei
	jc	short syschdir_5

syschdir_1:
	; 26/12/2019
	;mov	bx, ax
	call	iget
	jc	short syschdir_5
syschdir_2:
	;test 	word ptr [inode_flgs], 4000h ; directory i-node ?
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
	;mov	word ptr [u_cdir], ax

	; 01/01/2020
	mov	word ptr [u_cdir],bx

	;mov	dx, word ptr [cdev]
	;mov	word ptr [u_cdev], dx

syschdir_5:
	retn

sys_chdir endp

sys_mkdir proc near
	; 15/04/2022 ('number of links' bugfix)
	; 28/03/2022
	; 29/12/2019
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
        ;mov	   (sp)+,r1 / put the mode in r1
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

	; bx = inode number

	; 29/12/2019
	mov	ax, 0FFFFh 

	stc
@:
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
	; 28/03/2022
	; (clear/reset [uf_i_number] as new file/directory sign)
	mov	word ptr [uf_i_number], 0

	;call	mak_nod	
	; ax = I-Number (also in u.dirbuff)
	;retn

	; 29/12/2019
	;jmp	short mak_nod ; 17/01/2020 (short jump)

sys_mkdir endp

mak_nod proc near
	; 28/03/2022
	; 27/03/2022
	; 18/01/2020
	; 03/01/2020
	; 01/01/2020
	; 26/12/2019
	; 25/09/2019
	; 19/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	;
	; 01/03/2013
	; 23/02/2013
	; 15/12/2012 UNIXCOPY.ASM version of maknod
	; 02/12/2012 (maknod_imap -> call	imap)
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

	;or	ah, 80h  ; 10000000b, allocate flag set  ;; unix v1
	; 25/09/2019
	;or	ah, 80h	 ; 11000000b, regular file (or dir) ;; runix v2	  
	
	push	ax ; *	 ; put mode on stack
	
	;mov	ax, word ptr [ii] ; move current i-number to AX/r1
	;push 	ax
	push	word ptr [ii] ; **

	;mov	ax, 41	 ; r1 = 41 ; unix v1
	mov	ax, 1	 ; check inodes after root directory, 19/09/2019
		 ; (root directory inode number = 1) ; runix v2
maknod_1: ; 1 	 ; scan for a free i-node
	inc	ax 	 ; r1 = r1 + 1
	; 2/12/2012
	call	imap     ; get byte address and bit position in inode map in 
		; r2 (DX) & mq (BX)
		; DX (MQ) has a 1 in the calculated bit position
        	; BX (R2) has byte address of the byte with allocation bit

        test	byte ptr [bx], dl ; bitb mq,(r2) / is the i-node active
	jnz	short maknod_1    ; bne	1b / yes, try the next one
        or	byte ptr [bx], dl ; bisb mq,(r2)
		; no, make it active (put a 1 in the bit map)
	; 01/01/2020
	or	byte ptr [smod], 2 ; inode map modified flag

	; ax = i-number

	; 26/12/2019
	mov	bx, ax

	call	iget      	  ; jsr	r0,iget / get i-node into core
	;jc	short maknod_3	
	; 01/01/2020
	jnc	short @f

maknod_3:
	; 15/12/2012
	pop	ax ; ** ; current i number
maknod_2:
	pop	ax ; *  ; file mode (inode flags)
	retn
@@:	
	; 03/01/2020
	mov	ax, bx

	;test	word ptr [inode_flgs], 8000h  ; is i-node already allocated
	test	byte ptr [inode_flgs+1], 0 ; 19/09/2019 - Retro UNIX 386 v2
	jnz	short maknod_1	; 1b / yes, look for another one
	
	mov	word ptr [u_dirbuf], ax ; mov	r1, u.dirbuf 
		        ; no, put i-number in u.dirbuf

	;pop 	ax ; 15/12/2012	; get current i-number back	
	; 26/12/2019
	pop	bx ; ** ; current i number
	call	iget	; jsr r0,iget / get i-node in core
	jc	short maknod_2

	call	mk_dir	; jsr r0,mkdir 
		; make a directory entry in current directory
	jc	short maknod_2 ; 01/03/2013 

	;mov	ax, word ptr [u_dirbuf] ; mov	u.dirbuf,r1 
		; ax / r1 = new inode number
	; 26/12/2019
	mov	bx, word ptr [u_dirbuf]
	call	iget
	jc	short maknod_2

        ; jsr r0,copyz; inode; inode+32. / 0 it out 
	;mov	cx, 16
	mov	cx, 32 ; 19/09/2019 - Retro UNIX 386 v2
	;	       ;	     (inode size = 64 bytes)
	xor	ax, ax ; 0
	mov	di, offset inode 
	rep	stosw

	pop	word ptr [inode_flgs] ; * ; mov	(sp)+,i.flgs / fill flags

	; 19/09/2019 - Retro UNIX v2 (modified unix v7 inodes)
	mov	cl, byte ptr [u_gid] ; user's group id
	mov	byte ptr [inode_gid], cl	

	mov	cl, byte ptr [u_uid] ; movb u.uid,i.uid / user id
	mov	byte ptr [inode_uid], cl ; 23/02/2013 al -> cl
	mov	byte ptr [inode_nlks], 1 ; movb $1,i.nlks / 1 link
	
	;call	epoch

	;mov	word ptr [s_time], ax
	;mov	word ptr [s_time]+2, dx	

	;mov	word ptr [inode_ctim], ax ; mov	s.time,i.ctim / time created
	;mov	word ptr [inode_ctim]+2, dx ; mov	s.time+2,i.ctim+2

	; 25/11/2012
	; 23/02/2013
fromdos_maknod:
	;xor	ax, ax
	xor	dx, dx	
	; File/Directory data/content modification date&time
	mov	word ptr [inode_mtim], ax  ; 0
	mov	word ptr [inode_mtim]+2, dx ; 0
	; 19/09/2019 - Retro UNIX 386 v2
	; Last access date&time or inode modification date&time
	mov	word ptr [inode_atim], ax  ; 0
	mov	word ptr [inode_atim]+2, dx ; 0

	;test	word ptr [inode_flgs], 4000h  ; Directory
	; 19/09/2019
	test	byte ptr [inode_flgs+1], 40h  ; Directory
	jnz	short maknod_4
	mov	ax, word ptr [uf_make_datetime]
	mov	dx, word ptr [uf_make_datetime]+2
maknod_4:
	mov	word ptr [inode_ctim], ax  
	mov	word ptr [inode_ctim]+2, dx

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
	; 18/01/2020
	;mov	ax, word ptr [u_dirbuf]
	cmp	ax, word ptr [systm.sb_FirstFreeIno]
	jne	short @f
	;cmp	word ptr [systm.sb_FirstFreeIno+2], 0
	;jne	short @f
	mov	word ptr [systm.sb_FirstFreeIno], 0FFFFh ; invalidate
	mov	word ptr [systm.sb_FirstFreeIno+2], 0FFFFh
@@:
	clc
	;mov	ax, word ptr [u_dirbuf] ; 18/01/2020
	retn

;maknod_3:
;	; 15/12/2012
;	pop	ax
;maknod_2:
;	pop	ax
;	retn

mak_nod	endp

remove_directory proc near
	; 15/04/2022 ('number of links' bugfix)
	; 17/01/2020
	; 15/01/2020
	; 29/12/2019
	; 01/10/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)
	; 04/12/2015 (14 byte directory names)
	; 05/01/2013
	;
	; mov	word ptr [u.namep], si

	;call	namei
        ;jc	short @f

	;cmp	ax, ROOT_DIR_INODE_NUMBER
        ;je	short rmdir_stc_retn

	;cmp	ax, word ptr [u_cdir]
        ;je	short rmdir_stc_retn

       ; INPUT ->
       ; ax = i_number of directory (to be removed)
       ; u_off = directory entry location + 16 (in parent dir)
       ; [ii] = i_number of parent directory

	cmp	ax, word ptr [ii]	; '.' entry
	je	short rmdir_stc_retn

	mov	word ptr [uf_i_number], ax  ; i_number of dir or file 

	mov	dx, word ptr [u_off]
	; 01/01/2020
	;mov	word ptr [FileHandle], dx	
	mov	word ptr [d_off], dx ; directory entry location + 16 (+10) 
	; 15/01/2020
	mov	dx, word ptr [u_off+2]
	mov	word ptr [d_off+2], dx

	mov	dx, word ptr [ii] ; i-number of parent directory 
	mov	word ptr [pdir], dx

	; 26/12/2019
	mov	bx, ax

	call	iget
	jc	short @f

	;mov	ax, word ptr [inode_flgs]
	;test 	ah, 40h ;'directory' flag
	;jz	short rmdir_stc_retn	
	;test 	al, 4h ; 'write' flag
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
	; 15/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019

	;mov	word ptr [u_fofp], offset u.off	

rmdir_readi_loop:
	mov	word ptr [u_base], offset u_dirbuf
		  ; u.dirbuf holds a file name copied from
		  ; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 	
 	
	;mov	ax, word ptr [ii]
	; 26/12/2019
	mov	bx, word ptr [ii]
	 	
	call	readi ; read 16 bytes of file with i-number
		; i.e. read a directory entry
	jc	short @b ; 24/09/2019

	mov	cx, word ptr [u_nread]

	or	cx, cx
	;;jna	short rmdir_stc_retn
	;jna	short @b
	jna	short rmdir_unlink ; 01/10/2019	

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

	; 15/01/2020
	; 01/01/2020 (32 bit offset value and 32 bit directory size)
	mov	ax, word ptr [u_off+2]
	cmp	ax, word ptr [inode_size+2]
	jb	short rmdir_readi_loop

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
	; 01/01/2020
	;mov	dx, word ptr [FileHandle]
	; 15/01/2020
	mov	dx, word ptr [d_off+2]
	mov	word ptr [u_off+2], dx
	mov	dx, word ptr [d_off]
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
	mov	bx, word ptr [pdir] ; parent dir
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
	; 15/01/2020
	; 01/01/2020
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
	;	               (error code in AX)

	;jsr r0,arg; u.namep / u.namep points to name
	;jsr r0,namei / find the i-number associated 
	;               with the path name
	;br error9 / not found

	push	ax ;mov	r1,-(sp) / put its i-number on the stack
		;jsr r0,isdir / is it a directory
	xor	ax, ax
	mov	word ptr [u_dirbuf], ax ; clr u.dirbuf / no, clear 
		 ;the location that will get written
		 ;/ into the i-number portion of the entry
	sub	word ptr [u_off], 16 ; 04/12/2015 (10 -> 16)
		 ; sub $10.,u.off 
		 ; / move u.off back 1 directory entry
	; 15/01/2020
	; 01/01/2020
	sbb	word ptr [u_off+2], ax ; 0

	;;mov	ax, word ptr [ii] 
	;mov	ax, word ptr [pdir] ; 05/01/2013
	; 26/12/2019
	mov	bx, word ptr [pdir]
	call	iget
	jnc	short @f
	pop	ax
	retn
	;
@@:
	call	wdir ;jsr r0,wdir / free the directory entry
	;pop	ax   ;mov (sp)+,r1 / get i-number back
	; 26/12/2019
	pop	bx
	jc	short @f

        call	iget ;jsr r0,iget / get i-node
	jc	short @f
        call	setimod  ;jsr r0,setimod / set modified flag
	;dec	word ptr [inode_nlks] ; Retro UNIX 386 v2 (word)
        dec	byte ptr [inode_nlks] ;decb i.nlks 
		; / decrement the number of links
	jnz	short @f ;bgt sysret9 
		;/ if this was not the last link to file return

	; 19/09/2019

;	call	anyi ;jsr r0,anyi / if it was, see if anyone has it open.
;	             ;Then / free contents of file and destroy it.
;	             ;br sysret9
;@@:
;	retn

	; 17/01/2020
	; jmp	short anyi ; 17/01/2020

unlink	endp

anyi	proc near
	; 28/03/2022
	; 17/01/2020
	; 11/01/2020
	; 01/01/2020
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

	; 11/01/2020
	mov	ax, word ptr [ii]

	call	imap ; jsr r0,imap / get address of allocation bit 
				; in the i-map in r2
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
	; keeping super block unmodified if itrunc return with an error

        not	dx
        and	byte ptr [bx],dl ; bicb mq,(r2) 
		      ; / clear bit for i-node in the imap
	; 01/01/2020
	or	byte ptr [smod],2 ; inode map modified flag

	; 28/03/2022
	xor	dx, dx
	; 29/09/2019
	inc	word ptr [systm.sb_FreeInodes]
	mov	ax,word ptr [ii] ; inode number of current inode
	cmp	ax,word ptr [systm.sb_FirstFreeIno]
	jnb	short anyi_2
	;cmp	word ptr [systm.sb_FirstFreeIno+2], 0
	;jne	short anyi_2
	mov	word ptr [systm.sb_FirstFreeIno], ax
	; 17/01/2020
	;mov	word ptr [systm.sb_FirstFreeIno+2], 0 ; 0FFFFh -> 0
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
        ; incb 7(r2) / increment upper byte of the 4th word
	; rts r0 / in that fsp entry (deleted flag of fsp entry)

anyi	endp

show_file proc near
	; 12/01/2020
	; 29/12/2019
	; 20/09/2019 - Retro UNIX 386 v2 (modified unix v7 inode structure)	
	; 07/01/2013
	; 06/01/2013
	; derived from TRDOS command interpreter file (CMDINTR.ASM)
	; 'show' procedure (13/09/2011)

	call	namei
        jc	short suf_4

	; 29/12/2019
	; 26/12/2019
	;mov	bx, ax

        call	iget
	jc	short suf_4

	; 20/09/2019
	;;test 	word ptr [inode_flgs], 4000h  ; Directory
	;test 	byte ptr [inode_flgs+1], 40h
	;jnz	short suf_4

	; 20/09/2019
	;test 	byte ptr [inode_flgs+1], 80h ; Regular file
	;jz	short suf_4		

	mov	al, byte ptr [inode_flgs+1]
	and	al, 0C0h
	cmp	al, 80h
	jne	short suf_4	

        mov	si, offset UNIX_CRLF
        call	PRINT_MSG

	; 12/01/2020
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
	xor	dx, dx ; 0
        mov	word ptr [u_off], dx
	; 12/01/2020
	mov	word ptr [u_off+2], dx ; 29/12/2019 
	mov	cx, 22
suf_2:	
	push	cx	
	mov	word ptr [u_count], ax
	mov	word ptr [u_base], offset ReadBuffer
	;mov	ax, word ptr [ii] ; word ptr [u_dirbuf]
	; 26/12/2019
	mov	bx, word ptr [ii] 
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
	; 12/01/2020
	jmp	PRINT_MSG
suf_5:
	mov	cx, 20
suf_6:
        xor	bh, bh ; mov	bh, 0
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
        ;xor	bh, bh ; mov	bh, 0
        ;mov	bl, 7
	int	10h
suf_9:
	dec	di
	jnz	short suf_3

	;mov	ax, word ptr [u_nread]

	; 12/01/2020
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
        ;xor	bh, bh ; mov	bh, 0
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
	; 15/01/2020
	; 29/12/2019
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
	;mov	ax, ROOT_DIR_INODE_NUMBER ; = 41 ( = 1 for runix v2)
	; Put i-number of root directory in R1
	; 29/12/2019
	mov	bx, ROOT_DIR_INODE_NUMBER ; 1

	;xor	dx, dx
	inc	si  ; go to next char
	mov	word ptr [u_namep], si
	jmp	short namei_0
@@:
	;mov	dx, word ptr [u_cdev]

	;mov	ax, word ptr [u_cdir] 
	; put i-number of current directory in R1
	; 29/12/2019
	mov	bx, word ptr [u_cdir]

namei_0:
	;mov	word ptr [cdev], dx 
	; device file for users directory into cdev
; 1
	cmp	byte ptr [si], 0 ; is the character in file name a nul
        jna	short namei_7 ;nig

namei_1: ; 1	
	; get i-node with i-number r1
	
	; 26/12/2019
	;mov	bx, ax

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
	; 17/01/2020 (32 bit directory size)
	mov	ax, word ptr [inode_size]
	mov	dx, word ptr [inode_size+2] ; 01/01/2020
	mov	word ptr [u_dirp], ax ; put size of directory in u.dirp
	mov	word ptr [u_dirp+2], dx ; 01/01/2020	
	xor	ax, ax 
	mov	word ptr [u_off], ax ; u.off is file offset used by user
	; 15/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019

	;mov	word ptr [u_fofp], offset u.off
		; u.fofp is a pointer to the offset portion 
		; of fsp entry
namei_2: ; 2
	mov	word ptr [u_base], offset u_dirbuf
		; u.dirbuf holds a file name copied from
		; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 	
 	
	;mov	ax, word ptr [ii]
	; 26/12/2019
	mov	bx, word ptr [ii]
	 	
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
	; 15/01/2020
	mov	ax, word ptr [u_off]
	mov	dx, word ptr [u_off+2] ; 01/01/2020 (32 bit offset)	
	sub	ax, 16   ; 04/12/2015 (10 -> 16)
	sbb	dx, 0 ; 01/01/2020
	mov	word ptr [u_dirp], ax
	mov	word ptr [u_dirp+2], dx ; 01/01/2020
	jmp	short namei_2 ; 2b

namei_3: ; 3
	mov	si, word ptr [u_namep] ; r2, u.namep points into a file name string
	mov	di, offset u_dirbuf + 2 ; r3, points to file name of directory entry
	mov	dx, offset u_dirbuf + 16 ; 04/12/2015 (10 -> 16)
@@:	; 3
	lodsb  ; mov al, byte ptr [si] ; inc si ; (al = r4)
 	or	al, al
	jz	short namei_4 ; 3f. If char is nul, then the last char in string has
				; been compared
	cmp	al, "/"	; is char a "/"
	je	short namei_4 ; 3f
	cmp	di, dx	; offset u_dirbuf + 16 ; 04/12/2015 (10 -> 16) ; r3, 
		; have i checked all 14 bytes of file name
	je	short @b ; 3b
	scasb	; cmpb (r3)+, r4 (DI=R3, AL=R4)
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
	; 29/12/2019
	;mov	ax, bx

        ;jnz	namei_1 ; 1b
	; 27/03/2022
	jz	short namei_8
	jmp	namei_1
namei_8:
	; bx = inode number

	retn

namei	endp

readi	proc near
	; 16/01/2020
	; 29/12/2019
	; 26/12/2019 
	; 17/12/2019
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
	; 29/12/2019
	; INPUT:
	;	bx = inode number
	;	word [u_count] = count
	;	[u_fofp] = u_off
	;	dword ptr [u_off] = file pointer
	;	dword [inode_size] = file size
	;	
	; OUTPUT:
	;	bx = inode number
	;	cf = 0 -> reading ok, [Error] = 0
	;	cf = 1 -> error, error code in [Error] 
	; 	
	; Modified registers: ax,dx,cx,si,di
	
	xor	dx,dx ; 0
	mov	word ptr [u_nread],dx ; accumulated number of bytes transmitted
	cmp	word ptr [u_count],dx ; is number of byte to read greater than 0
	jna	short readinode_retn

readinode_1:
	; BX = I-Number ; 26/12/2019
	push	bx ; *

	call	iget	; get i-node into i-node section of core
	jc	short readinode_3 ; 01/03/2013

	; 17/12/2019
	mov	al,byte ptr [inode_flgs+1]
	test 	al,80h	; regular file ?
	jnz	short readinode_4	; yes
	test 	al,20h 	; device file ?
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
	pop	bx ; * ; i-number
readinode_retn:
	retn 

readinode_4:
	; 17/12/2019
	;mov	si,word ptr [u_fofp]

	; 29/12/2019
	mov	si,offset u_off

	; 32 bit file size & 32 bit file pointer (17/12/2019)

	mov	ax,word ptr [inode_size]  ; file size lw
	mov	dx,word ptr [inode_size+2] ; file size rw
	; Note: we don't regard 5th byte of file size (inode_size_h) for now!

	sub	ax,word ptr [si]
	sbb	dx,word ptr [si+2]
	jnz	short readinode_2 ; remain bytes more than requested count

	; 16/01/2020
	or	ax,ax
	jz	short readinode_3

	cmp	ax,word ptr [u_count] 
		; are enough bytes left in file to carry out read ?
	jnb	short readinode_2 ; remain bytes more than requested count
	mov	word ptr [u_count],ax  ; fix read count to end of file

readinode_2:
	call	mget	; returns physical block number of block in file 
			; where offset points
	jc	short readinode_3 ; 01/03/2013

	; 17/12/2019
	; DX:AX = Physical block number

	; Check current sector in the buffer

	cmp	byte ptr [buff_m],1
	jb	short readinode_7

	mov	cx,word ptr [buff_s]
	mov	bx,word ptr [buff_s+2]
	;mov	si,cx
	;or	si,bx
	;jz	short readinode_sioreg ; buff_s = 0 is invalid	
	;cmp	si,0FFFFh	; buff_s = 0FFFFh is invalid
	;je	short readinode_sioreg

	; write buffer content if sector is not same

	cmp	cx,ax
	jne	short readinode_6
	cmp	bx,dx
	je	short readinode_sioreg
readinode_6:
	push	dx
	push	ax
	; 17/12/2019
	;mov	ax,word ptr [buff_s]
	;mov	dx,word ptr [buff_s+2]
	mov	ax,cx
	mov	dx,bx
	mov	bx,offset Buffer
	call	dskwr
	pop	ax
	pop	dx
	jc	short readinode_3
	mov	byte ptr [buff_m],0
readinode_7:	
	; 17/12/2019
	cmp	ax,word ptr [buff_s]
	jne	short readinode_8
	cmp	dx,word ptr [buff_s+2]
	je	short readinode_sioreg
readinode_8:
	mov	bx,offset Buffer ; 29/12/2019
	call	dskrd	; read in block, BX points to 1st word of data in
		; buffer
	jc	short readinode_3

	; 17/12/2019
	mov	word ptr [buff_s],ax
	mov	word ptr [buff_s+2],dx
readinode_sioreg:
        ;mov	si,word ptr [u_off] ; R2
	;mov	cx,si ; cx = R3, si = R2
	
	; 17/12/2019
	call	sioreg

	; SI = file (user data) offset
	; DI = sector (I/O) buffer offset
	; CX = byte count

	xchg	si,di ; 17/12/2019 

        rep	movsb

	;pop	ax ; * ; i-number

	cmp	word ptr [u_count],0
	;;ja	short readinode_1
	ja	short readinode_4 ; 17/12/2019

	mov	byte ptr [Error],0 ; 17/12/2019

	;retn

	pop	bx ; * ; i-number ; 26/12/2019

	retn 

readi	endp

iget 	proc near
	; 22/01/2020
	; 25/12/2019
	; 27/11/2019
	; 07/11/2019 - Retro UNIX 386 v2
	; 16/9/2012
     	; 14/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
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

	cmp	bx,word ptr [ii] ; BX (R1) = i-number of current file
	je	short iget_5
iget_1:
	;push	ax ; **
	xor	ah,ah ; mov	ah,0
	mov	al,byte ptr [imod]
	and	al,al ; has i-node of current file been modified ?	
	jz	short iget_2 ; no
	;xor	al,al ; mov	al,0
	;mov	byte ptr [imod],al 
	push	bx ; * inode number
	mov	bx,word ptr [ii]	
	;inc	al ; mov	al,1
	mov	al,1 ; 25/12/2019	
	; ax = 1 = write
	call	icalc
	pop	bx ; *
	jc	short iget_4
	; 27/11/2019
	xor	ax,ax
	; 25/12/2019
	;mov	byte ptr [imod],al ; 0
	; 22/01/2020
	mov	word ptr [imod],ax ; 0 ; reset [imod] & [imodx]
iget_2:
	and	bx,bx
	jz	short iget_3 ; get current inode
	mov	word ptr [ii],bx	
	; ax = 0 = read
	call	icalc
iget_3:
	mov	bx,word ptr [ii]
iget_4:
	;pop	ax ; **
iget_5:
	retn

iget	endp

icalc 	proc near
	; 17/01/2020 
	; 26/12/2019
	; 14/12/2019
	; 12/12/2019
	; 09/11/2019
	; 07/11/2019 - Retro UNIX 386 v2 
	;	       (for hard disk file system)
	; 07/09/2019
	; 02/09/2019
	; 01/09/2019 - Retro UNIX 386 v2
	; 17/8/2012
	; 16/8/2012
	; 15/8/2012
	; 14/8/2012
	; 13/8/2012
        ; 15/7/2012
     	; 14/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	;; AX=R0, BX=R1, CX=R3, DX=R5 
	; 0 = read, 1 = write
	; RETRO UNIX v1 FS
	; initialization/format version
	;
        ; i-node is located in block (i+47)/16 and
	; begins 32*(i+47) mod 16 bytes from its start
	;; return => if cf=1 error number in [Error]
	;
	; input -> ax = 0 -> read, 1 = write
	;
	; 09/11/2019
	; modified registers: ax,dx,cx,bx

	;add	bx,47 ; add 47 to inode number, 15/8/2012
	;push	bx ; R1 -> -(SP)
	;shr	bx,1 ; divide by 16
	;shr	bx,1
	;shr	bx,1
	;shr	bx,1

	; 07/11/2019
	; inode 1 (1st inode) is on sector 4 
	; sector 0 : boot sector
	; sector 1 : super block
	; sector 2 : inodes map
	; sector 3 : free blocks map
	; sector 4 to 35 : inodes (32 sectors)

	; 08/09/2019
	;add	bx,31 ; add 31 to inode number
	;push	bx
	;shr	bx,1  ; divide by 8	
	;shr	bx,1
	;shr	bx,1
	; bx contains block number of block in which
	; inode exists

	; 07/11/2019 - Hard disk file system (RUFS v2)
	; inode table start sector: [systm.sb_InodeTblAddr]
	
	dec	bx ; 0 based inode number

	;mov	dx,bx ; 12/12/2012
	mov	cx,bx ; 26/12/2019

	; 17/01/2020
	mov	byte ptr [I_rw],al ; 0 = read, 1 = write

	mov	ax,word ptr [systm.sb_InodeTblAddr]
	mov	dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0

	and	cx,0FFF8h
	jz	short @f ; [I_sector] = 0 for inodes 1 to 8

	; 26/12/2019 (bx -> cx)
	shr	cx,1
	shr	cx,1
	shr	cx,1
	; cx = sector offset (8 inodes per sector)

	; 09/11/2019
	;mov	byte ptr [I_rw],al ; 0 = read, 1 = write

	; 26/12/2019
	;mov	ax,word ptr [systm.sb_InodeTblAddr]
	;mov	dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0
	
	add	ax,cx ; cx = [I_sector] ?
	adc	dx,0
@@:
	cmp	byte ptr [I_valid],0
	jna	short icalc_0
	; 26/12/2019
	cmp	cx,word ptr [I_sector]
	je	short icalc_1
	
	mov	byte ptr [I_valid],0 ; inode sector validation, invalid
icalc_0:
	; 26/12/2019
	mov	word ptr [I_sector],cx
	
	;mov	ax,word ptr [systm.sb_InodeTblAddr]
	;mov	dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0
	
	;add	ax,word ptr [I_sector]
	;adc	dx,0

	push	bx ; inode number - 1 ; (root dir inode = 1)

	mov	bx,offset I_buffer
	call	dskrd
	;pop	dx ; 14/8/2012
	pop	bx ; 26/12/2019
	jc	short icalc_5

	mov	byte ptr [I_valid],1 ; inode sector validation, valid
icalc_1:
	; 26/12/2019
	push	di
	push	si

	mov	di,bx ; 26/12/2019

	;and	dx,0Fh	; (i+47) mod 16
	;shl	dx,1
	;shl	dx,1
	;shl	dx,1
	;shl	dx,1
	;shl	dx,1
	; DX = 32 * ((i+47) mod 16)	
	; DX (R5) points to first word in i-node i.

	;and	dx,07h	; (i+31) mod 8
	and	di,07h ; 26/12/2019
	jz	short @f ; 29/09/2019 
	;shl	dx,1
	;shl	dx,1
	;shl	dx,1
	;shl	dx,1	
	;shl	dx,1
	;shl	dx,1
	mov	cl,6
	;shl	dx,cl	
	; DX = 64 * ((i+31) mod 8)
	; DX points to first word in i-node i.	
	shl	di,cl ; 26/12/2019
@@:
	; 14/8/2012
	;push	di
	;push	si
	
	mov	si,offset inode ; 14/8/2012
	; inode is address of first word of current inode
	;mov	cx,16 ; CX = R3
	; 02/09/2019
	mov	cx,32 ; inode size/2 for Retro UNIX 386 v2 (& UNIX v7)	

	; 09/09/2019
	;push	ax

	;mov	di,offset Buffer ; 16/8/2012
	;mov	di,word ptr [buff_o] ; 02/09/2019 - Retro UNIX 386 v2

	; 04/12/2019
	;mov	di,offset I_buffer

	;;add	di,dx ; 13/8/2012
	;add	di,bx ; 26/12/2019

	; 30/12/2019
	add	di,offset I_buffer

 	;and	ax,ax
	;jz	short icalc_3 ; 0 = read (and copy i-node to memory) 

	cmp	byte ptr [I_rw],0
	jna	short icalc_3 ; read	
icalc_2:
	; 14/8/2012
	; overwrite old i-node (in buffer to be written)
	rep	movsw

	; 26/12/2019
	; 14/12/2019
	;mov	ax,word ptr [systm.sb_InodeTblAddr]
	;mov	dx,word ptr [systm.sb_InodeTblAddr+2] ; = 0
	
	;add	ax,word ptr [I_sector]
	;adc	dx,0

	mov	bx,offset I_buffer

	; 31/10/2012

	call	dskwr
	jmp	short icalc_4
icalc_3:
	xchg	si,di ; 14/8/2012	
	; copy new i-node into inode area of (core) memory
	rep	movsw
icalc_4:
	;pop	ax ; 09/09/2019
	; 14/8/2012
	pop	si
	pop	di

	; OUTPUTS ->
	; inode 
	; DX/R5 (internal), BX/R1 (internal), CX/R3 (internal) 
icalc_5:	
	retn	 

icalc 	endp

mget 	proc near
	; 25/01/2020
	; 22/01/2020
	; 13/01/2020
	; 07/01/2020
	; 30/12/2019
	; 29/12/2019
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
	; 20/6/1972
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
	; Modified registers: ax,dx,cx,bx,si,di

	;push	bx
	;push	cx
	;push	dx
	 ;; contents of bx,cx,dx will be destroyed 
mget_0:
	;mov	bx,word ptr [u_fofp]
	; 29/12/2019
	mov	bx,offset u_off
	; 13/11/2019
	mov	ax,word ptr [bx]
	mov	dx,word ptr [bx+2]
		
	; 31/10/2012
	;mov	bx,word ptr [u_fofp]
	;mov	ax,word ptr [bx]

	;mov	bl,ah  ; div ax by 256
	;xor	bh,bh

	; BX = R2
        ;test 	word ptr [inode_flgs],4096 ; 1000h
		  	     ; is this a large or small file
	;jnz	short mget_8 ; 4f ; large file

	test 	byte ptr [inode_flgs+1],16 ; 10h
	;jnz	mget_8 ; not small file
	; 22/01/2020
	jz	short @f
	jmp	mget_8	
@@:	
        ;test 	bl,0F0h ; !0Fh  ; branch if BX (R2) >= 16	    
	;jnz	short mget_3 ; 3f

	; 13/11/2019
	; small file size limit = 5120 bytes (1400h)

	or	dx,dx
	jnz	short mget_3  ; requested file offset > 65535

	; 13/11/2019
	; Retro UNIX 386 v2 disk inode contains..
	; (if large file flag is clear -not set-)
	; 10 direct disk block/sector dword pointers

	; 15/09/2019
	;cmp	bl,14h
	cmp	ah,14h
	jnb	short mget_3 ; 3f ; requested offset >= 5120

	; 01/12/2019
	mov	bl,ah  ; div ax by 256
	xor	bh,bh

	;and	bl,0Eh  ; clear all bits but bits 1,2,3
	and	bl,1Eh ; 15/09/2019 ; clear all bits but bits 1,2,3,4

	shl	bl,1 ; 03/09/2019 - Retro UNIX 386 v2
	mov	ax,word ptr [bx+inode_dskp] ; AX = R1, physical block number
	; 09/11/2019
	mov	dx,word ptr [bx+inode_dskp+2] ; DX = hw of physical block number
	or	ax,ax
	jnz	short mget_2 ; if physical block number is zero
		 ; then we need a new block for file
	; 09/11/2019
	or	dx,dx
	jnz	short mget_2

	call	alloc	 ; allocate a new block for this file	
		 ; AX (R1) = Block number
	;jc	short mget_6 ; cf -> 1 & ax = 0 -> no free block
	; 22/01/2020
	jnc	short @f

mget_6: 
	;mov	word ptr [Error],err_NOFREEBLOCK
	
	;pop	dx
	;pop	cx
	;pop	bx
mget_7:	
	retn
@@:
	; 09/11/2019
	mov	word ptr [bx+inode_dskp],ax
	mov	word ptr [bx+inode_dskp+2],dx 

	call	setimod

	; 09/09/2019
	;mov	byte ptr [buff_c],1

	; 08/12/2019
	mov	bx,offset Buffer

	; 04/12/2019
	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_1

	push	dx
	push	ax
	mov	ax,word ptr [buff_s]
	mov	dx,word ptr [buff_s+2]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	pop	dx
	jc	short mget_2
	mov	byte ptr [buff_m],0 ; reset buffer modified sign

mget_1:
	mov	word ptr [buff_s],ax
	mov	word ptr [buff_s+2],dx

	; 05/12/2019
	mov	byte ptr [buff_m],1 ; set buffer modified sign

	call	clear	; clear Buffer

	; BX = offset Buffer

	call	dskwr
	jc	short mget_2

	mov	byte ptr [buff_m],0 ; reset buffer modified sign

mget_2: ; 2
	; AX (R1) = Physical block number
	; DX = High word of block/sector number ; 09/11/2019

	;pop	dx
	;pop	cx
	;pop	bx

	retn

mget_3: ; 3
	; adding on block which changes small file to large file
	call	alloc
	jc	short mget_6 ; 01/03/2013 
	; 22/01/2020
	;jnc	short @f

;mget_6: 
;	;mov	word ptr [Error],err_NOFREEBLOCK
;	
;	;pop	dx
;	;pop	cx
;	;pop	bx
;mget_7:	
;	retn
;@@:	
	;call	wslot  ; setup I/O buffer for write
	;	   ; R5 points to the first data word in buffer

	;push	ds
	;pop	es

	; 07/01/2020
	mov	bx,offset Buffer

	; 18/12/2019
	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_4

	push	dx
	push	ax
	mov	ax,word ptr [buff_s]
	mov	dx,word ptr [buff_s+2]
	;mov	bx,offset Buffer ; 07/01/2020
	call	dskwr
	pop	ax
	pop	dx
	jc	short mget_2
	mov	byte ptr [buff_m],0 ; reset buffer modified sign
mget_4:
	; 09/11/2019
	mov	word ptr [buff_s],ax  ; Block/Sector number
	mov	word ptr [buff_s+2],dx

	; 13/01/2020 (si, di)
	;push	si
	;push	di
	push	ax

	;mov	cx,8  ; R3, transfer old physical block pointers
		      ; into new indirect block area for the new
		      ; large file	
	mov	cx,10 ; 15/09/2019	
	; 09/11/2019
	;mov	di,offset Buffer ; BX = R5 ; 07/01/2020
	;mov	di,word ptr [buff_o]  ; 03/09/2019
	mov	si,offset inode_dskp
	; 07/01/2020
	mov	di,bx ; offset Buffer 

	xor	ax,ax ; mov	ax,0
mget_5: ; 1
	movsw
	movsw ; 07/09/2019
	mov	word ptr [si-2],ax ; 13/01/2020
	mov	word ptr [si-4],ax ; 07/09/2019
	loop	mget_5
	
	;;mov	cl,256-8 ; clear rest of data buffer
	;mov	cl,256-16 ; 07/09/2019
	mov	cl,256-20 ; 15/09/2019	

	rep	stosw  ; clear buffer offset 40 to 512 (472 bytes)

	pop	ax
	;pop	di ; 13/01/2020
	;pop	si

	mov	byte ptr [buff_m],1 ; modified

	call	dskwr
	;jc	short mget_7 ; 01/03/2013
	; 22/01/2020
	jc	short mget_2	

	mov	word ptr [inode_dskp],ax
	; 09/11/2019
	mov	word ptr [inode_dskp+2],dx
	mov	byte ptr [buff_m],0 ; reset modified sign
	
	;or	word ptr [inode_flgs],4096 ; 1000h

	or	byte ptr [inode_flgs+1],10000b ; 10h ; 16

	call	setimod

	; 15/09/2019
	jmp	mget_0

;mget_6: 
;	;mov	word ptr [Error],err_NOFREEBLOCK
;	
;	;pop	dx
;	;pop	cx
;	;pop	bx
;mget_7:	
;	retn

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
	
	cmp	dx,08h ; 524288 = 080000h
	jnb	short mget_9 ; check double indirect limit

	mov	byte ptr [level],1
	mov	cx,9
	call	shr32
	; ax = sector offset (flat)
	; dx = 0
	mov	cl,al
	and	cl,127
	mov	byte ptr [level+1],cl
	mov	cl,7	
	;call	shr32
	shr	ax,cl  ; 13/01/2020
	; ax = indirect pointer index (<= 7)
	; dx = 0
	;mov	byte ptr [level+2],al
	; 15/11/2019
	shl	al,1
	shl	al,1
	mov	si,ax

	jmp	short mget_11
mget_9:
	; check double indirect pointers limit (as file offset)
	; (128*128)+1024 = 16384+1024 blocks or 8 MB + 512 KB
	; check dx (file offset hw) value
	
	cmp	dx,88h ; 8912896 = 880000h	
	jnb	short mget_10

	mov	byte ptr [level],2
	sub	dx,08h
	mov	cx,9
	call	shr32
	; dx:ax = sector offset (flat)
	mov	cl,al
	and	cl,127
	mov	byte ptr [level+1],cl
	mov	cl,7	
	call	shr32
	; ax = indirect pointer index (<= 127)
	; dx = 0
	mov	byte ptr [level+2],al
	mov	si,8*4 ; 32
	jmp	short mget_11
mget_10:
	; 13/11/2019
	; triple indirect pointers ; 8912386 to 1082654210 bytes
	mov	si,9*4 ; 36

	mov	byte ptr [level],3
	sub	dx,88h
	mov	cx,9
	call	shr32
	; dx:ax = sector offset (flat)
	mov	cl,al
	and	cl,127
	mov	byte ptr [level+1],cl
	mov	cl,7	
	call	shr32
	; ax = indirect pointer index (<= 127)
	; dx = 0
	mov	dl,al
	and	dl,127
	mov	byte ptr [level+2],dl
	mov	cl,7
	shr	ax,cl
	mov	byte ptr [level+3],al
mget_11:
	mov	ax,word ptr [si+inode_dskp]
	mov	dx,word ptr [si+inode_dskp+2]

	mov	bx,offset Buffer  ; 01/12/2019
	
	or	ax,ax
	jnz	short mget_15 ; if physical block number is zero
		; then we need a new block for file
	or	dx,dx
	jnz	short mget_15
	call	alloc	 ; allocate a new block for this file	
		; AX (R1) = Block number
	;jc	mget_6	 ; cf -> 1 & ax = 0 -> no free block
	jc	short mget_12 ; 30/12/2019
	
	mov	word ptr [si+inode_dskp],ax
	mov	word ptr [si+inode_dskp+2],dx

	; 18/12/2019
	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_13

mget_20: ; 07/01/2020
	push	dx
	push	ax
	mov	ax,word ptr [buff_s]
	mov	dx,word ptr [buff_s+2]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	pop	dx
	;jc	short mget_12
	;mov	byte ptr [buff_m],0 ; reset buffer modified sign
	jnc	short mget_14
mget_12:
	retn
mget_13:
	mov	byte ptr [buff_m],1  ; buffer modified
mget_14:
	call	clear ; clear buffer

	call	setimod

	mov	word ptr [buff_s],ax
	mov	word ptr [buff_s+2],dx

	; 25/01/2020 ('setimod' modifies bx register)	
	mov	bx,offset Buffer

	call	dskwr
	jc	short mget_12

	; 01/12/2019
	mov	byte ptr [buff_m],0 ; reset buffer modified sign
	jmp	short mget_18
mget_15:
	; 13/11/2019
	cmp	ax,word ptr [buff_s]
	jne	short mget_16	
	cmp	dx,word ptr [buff_s+2]
	je	short mget_18
mget_16:	
	cmp	byte ptr [buff_m],0 ; buffer modified ?
	jna	short mget_17

	; 01/12/2019	
	push	dx
	push	ax
	mov	ax,word ptr [buff_s]
	mov	dx,word ptr [buff_s+2]
	;mov	bx,offset Buffer
	call	dskwr
	pop	ax
	pop	dx
	jc	short mget_12
	mov	byte ptr [buff_m],0 ; reset buffer modified sign
mget_17:
	;mov	bx,offset Buffer	
	call	dskrd
	jc	short mget_12

	mov	word ptr [buff_s],ax
	mov	word ptr [buff_s+2],dx
mget_18:
	; 07/01/2020
	cmp	byte ptr [level],0
	jna	short mget_12
	
	;dec	byte ptr [level]
	;jz	short mget_12

	mov	bl,byte ptr [level]
	xor	bh,bh
	mov	bl,byte ptr [bx+level] ; sector pointer offset
	shl	bl,1
	shl	bx,1 ; 13/01/2020

	; 07/01/2020
	dec	byte ptr [level]

	; 13/01/2020
	mov	si,offset Buffer

	; 01/12/2019
	add	bx,si ; offset Buffer ; 13/01/2020

	mov	ax,word ptr [bx]
	mov	dx,word ptr [bx+2]

	; 01/12/2019
	or	ax,ax
	;jnz	short mget_16 ; if physical block number is zero
		 ; then we need a new block for file
	jnz	short mget_21 ; 13/01/2020
	or	dx,dx
	;jnz	short mget_16
	jz	short mget_23 ; 13/01/2020
mget_21:
	; 13/01/2020
	mov	bx,si ; offset Buffer

	cmp	byte ptr [level],0
	;ja	short mget_16
	ja	short mget_17
mget_22:
	retn
mget_23:
	call	alloc	; allocate a new block for this file	
		; AX (R1) = Block number
	;jc	mget_6	; cf -> 1 & ax = 0 -> no free block
	; 07/01/2020
	;jnc	short mget_19
	;
	;retn
	jc	short mget_22	
mget_19:	
	; 12/12/2019
	mov	word ptr [bx],ax
	mov	word ptr [bx+2],dx

	; 07/01/2020
	; 18/12/2019
	mov	byte ptr [buff_m],1 ; set buffer modified sign
	;call	setimod
	;mov	bx,offset Buffer
	mov	bx,si ; offset Buffer ; 13/01/2020
	;jmp	mget_13
	; 07/01/2020
	jmp	mget_20
	
mget	endp

alloc 	proc near
	; 28/03/2022
	; 23/03/2022
	; 19/01/2020
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

	mov	ax,word ptr [systm.sb_FreeBlocks] 	
	mov	dx,word ptr [systm.sb_FreeBlocks+2]
	or	ax,dx
	jnz	short alloc_0
alloc_err:
	stc
	retn
alloc_0:
	; 06/12/2019
	mov	ax,word ptr [systm.sb_FirstFreeBlk]
	mov	dx,word ptr [systm.sb_FirstFreeBlk+2]

	push	cx ; *
	push	bx ; **

	mov	cx,dx
	mov	bx,ax
	cmp	cx,0FFFFh
	jb	short alloc_1
	cmp	bx,0FFFFh
	jb	short alloc_2	
	xor	ax,ax
	xor	dx,dx
	; dx:ax = fbm sector index (as start sector) = 0
	xor	bx,bx ; bit 0
	jmp	short alloc_3	
alloc_1:
	or	cx,bx
	jz	short alloc_3
alloc_2:	
	;mov	cx,12   ; 512 block alloc bytes per free block map sector
	;call	shr32	; (4096 block alloc bits per fm sector)	
	;and	bx,0Fh  ; start bit position of fbm buffer byte
	
	; 19/01/2020
	mov	cx,3	; 1 alloc byte (in fbm) is for 8 sectors 
	call	shr32
	mov	bx,ax    	
	and	bx,1FFh	; 511
	mov	cl,9
	call	shr32   ; 512 alloc bytes per fbm sector (4096 bits)
alloc_3:
	; free map sector (index) in dx:ax

	; 19/01/2020
	add	bx,offset fbm_buffer

	;push	bx ; *** ; byte offset in fbm sector (to search at first)
	; 23/03/2022
	;mov	word ptr [save_bx],bx ; *** ; 19/01/2020

	cmp	ax,word ptr [fm_sector]
	jne	short alloc_4
	cmp	dx,word ptr [fm_sector+2]
	;je	short alloc_6
	; 23/03/2022
	je	short alloc_7	
alloc_4:
	; 23/03/2022
	mov	word ptr [save_bx],bx ; ***
	; 12/12/2019
	mov	bx,offset fbm_buffer

	test 	byte ptr [smod],1 ; free map modified flag
	jz	short alloc_5

	push	dx ; ****
	push	ax ; *****

	;call	sync ; writes current free map sector/block
	;	     ; and updates super block

	; 12/12/2019
	mov	ax,word ptr [fm_sector]
	mov	dx,word ptr [fm_sector+2]
	add	ax,word ptr [systm.sb_FreeMapAddr]
	adc	dx,word ptr [systm.sb_FreeMapAddr+2]
	;mov	bx,offset fbm_buffer
	call	dskwr

	pop	ax ; *****
	pop	dx ; ****	
	jc	short alloc_9 ; 07/11/2019

	; 12/12/2019
	and	byte ptr [smod],0FEh ; reset free map modified flag
alloc_5:
	; 07/11/2019
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx
alloc_17: ; 27/11/2019
	add	ax,word ptr [systm.sb_FreeMapAddr]
	adc	dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov	bx,offset fbm_buffer 	
	call	dskrd
	jc	short alloc_9 ; 07/11/2019

	; 07/11/2019	
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	mov	word ptr [fm_sector],ax
	mov	word ptr [fm_sector+2],dx
alloc_6:
	; 19/01/2020
	;mov	bx,offset fbm_buffer-2
	mov	bx,word ptr [save_bx]

	; 28/10/2019
alloc_7: 
	; dx:ax = [fm_sector] ; 07/11/2019
	; 19/01/2020
	;inc	bx
	;inc	bx
	;mov	dx,word ptr [bx]
	;or	dx,dx
	; 07/11/2019
	;mov	cx,word ptr [bx]
	;or	cx,cx
	; 19/01/2020
	mov	dl,byte ptr [bx]
	or	dl,dl
	jnz	short alloc_10 ; branch if any free blocks in this word
	; 19/01/2020
alloc_20: 
	inc	bx
	;cmp	bx,offset fbm_buffer+510 
	cmp	bx,offset fbm_buffer+512 ; 19/01/2020 
	jb	short alloc_7
	;mov	ax,word ptr [fm_sector]
	;mov	dx,word ptr [fm_sector+2]
	add	ax,1
	adc	dx,0
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx

	;mov	cx,12  ; 512 block alloc bytes per free block map sector
	;call	shl32  ; (4096 block alloc bits per fm sector)
	;cmp	dx,word ptr [systm.sb_TotalSects+2]
	;jb	short alloc_8
	;ja	short alloc_3	
	;cmp	ax,word ptr [systm.sb_TotalSects]
 	;jnb	short alloc_9

	mov	cx,9  ; 512 block alloc bytes per free block map sector
	call	shl32 ; (4096 block alloc bits per fm sector)
	cmp	dx,word ptr [systm.sb_FreeMapSize+2]
	jb	short alloc_8
	;ja	short alloc_3
	ja	short alloc_19 ; 03/01/2020	
	cmp	ax,word ptr [systm.sb_FreeMapSize]
 	jb	short alloc_8
	; 03/01/2020
alloc_19:
	; 28/03/2022
	; (invalidate first free block value)
	xor	ax, ax
	xor	dx, dx
	mov	cx, 0FFFFh
	;; 28/03/2022
	;;cmp	cx, word ptr [systm.sb_FirstFreeBlk+2]
	;;jne	short alloc_21
	;;cmp	cx, word ptr [systm.sb_FirstFreeBlk]
	;;je	short alloc_22
;;alloc_21:
	;; 28/03/2022
	;; ((invalidate first free block value then..
	;; search a free block again from start of fbm))
	mov	word ptr [systm.sb_FirstFreeBlk], cx
	mov	word ptr [systm.sb_FirstFreeBlk+2], cx
	;!(next time free block search will start from block 0)!
	;;sub	cx, cx
	;;sub	bx, bx
	;;jmp	alloc_3
;;alloc_22:
	; (no free blocks !)
	;xor	ax,ax ; 07/11/2019
	;xor	dx,dx
	stc	; cf=1 --> error: no free blocks/sectors
	;jmp	short alloc_9
	; 03/01/2020
alloc_9:
	;pop bx ; *** ; 19/01/2020
	pop	bx ; **
	pop	cx ; *
	retn
alloc_8:
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	; 19/01/2020
	mov	bx,offset fbm_buffer  ; 12/12/2019
	mov	word ptr [save_bx],bx ; *** ; 19/01/2020
	jmp	short alloc_17 ; 27/11/2019
alloc_10:
	; 19/01/2020
	; 07/11/2019
	;mov	dx,cx

	; 28/10/2019	
	; dx = value of (current) fbm buffer word

	;pop cx ; ***

	;mov	ax,1
	; 19/01/2020
	mov	al,1

	;and	cx,0Fh ; bit position
	;jz	short alloc_11
	;and	cx,cx
	;jz	short alloc_11
	;shr	dx,cl
	;shl	ax,cl ; 27/11/2019
	
	; 19/01/2020
	xor	cx,cx
alloc_11:
	; 08/12/2019
	;mov	byte ptr [bitpos],cl ; 0 to 15
	; BX = byte offset in fbm_buffer
	; AX = bit position to be reset
	;shr	dx,1
	; 19/01/2020
	shr	dl,1
	jc	short alloc_12
	; 08/12/2019
	;cmp	cl,0Fh ; 15
	;jnb	short alloc_18
	
	; 19/01/2020
	cmp	cl,7
	jnb	short alloc_18

	;shl	ax,1
	inc	cl
	shl	al,1
	jmp	short alloc_11
alloc_18:
	; 08/12/2019
	mov	ax,word ptr [fm_sector]
	mov	dx,word ptr [fm_sector+2]
	;jmp	short alloc_7
	; 19/01/2020
	jmp	short alloc_20
alloc_12:
	; 19/01/2020
	mov	word ptr [save_cx],cx ; 0 to 7 - 19/01/2020

	;not ax ; requested bit is 0 (allocated), others are 1 (free)
	;and word ptr [bx],ax
	; 19/01/2020
	not	al
	and	byte ptr [bx],al

	mov	ax,word ptr [fm_sector]
	mov	dx,word ptr [fm_sector+2]
	mov	cx,ax
	or	cx,dx
	jz	short alloc_13
	mov	cx,12  ; convert fbm sector index number to bit offset
	call	shl32
	; dx:ax = total bits number of sectors before current fbm sector
alloc_13:	
	; 08/12/2019
	sub	bx,offset fbm_buffer
 	; bx = byte offset in fbm_buffer
	jz	short alloc_14 
	shl	bx,1
	shl	bx,1
	shl	bx,1
	; bx = bit offset (0 to 4080)
alloc_14:
	; ch = 0
	;mov	cl,byte ptr [bitpos] ; 0 to 15
	;add	bx,cx
	; 19/01/2020
	add	bx,word ptr [save_cx] ; 0 to 7
	;	
	add	ax,bx
	adc	dx,0
	; dx:ax = bit offset from start of fbm = allocated sector number

	or	byte ptr [smod],1 ; set free map modified flag
		      		  ; in super block	

	; Do not change invalid count of free blocks
	; (0FFFFFFFFh) value here! -otherwise it may be a mistake!-
	mov	cx,word ptr [systm.sb_FreeBlocks] 	
	and	cx,word ptr [systm.sb_FreeBlocks+2]
	cmp	cx,0FFFFh 
	jnb	short alloc_15

	sub	word ptr [systm.sb_FreeBlocks],1
	sbb	word ptr [systm.sb_FreeBlocks+2],0

	; 28/03/2022
	;or	byte ptr [smod],0F0h  ; set sb modified flag
alloc_15:
	; 28/03/2022
	; (dx:ax contains previous first free block value and
	;  it is better if the new value is set one/just next to dx:ax)

	mov	cx, ax
	mov	bx, dx
	add	cx, 1
	adc	bx, 0
		   ; (next ffb search will be started from)
	mov	word ptr [systm.sb_FirstFreeBlk], cx
	mov	word ptr [systm.sb_FirstFreeBlk+2], bx

	;cmp	ax,word ptr [systm.sb_FirstFreeBlk]
	;jne	short alloc_16
	;cmp	dx,word ptr [systm.sb_FirstFreeBlk+2]
	;jne	short alloc_16
	;
	;; set first free block to the next sector (in order)
	;add	word ptr [systm.sb_FirstFreeBlk],1
	;adc	word ptr [systm.sb_FirstFreeBlk+2],0

	or	byte ptr [smod],0F0h  ; set sb modified flag
alloc_16:
	; 06/12/2019
	clc
	pop	bx ; **
	pop	cx ; *
	; DX:AX = Block/Sector number
	retn

alloc   endp

free	proc near
	; 23/03/2022
	; 19/01/2020
	; 08/12/2019
	; 27/11/2019
	; 07/11/2019
	; 28/10/2019
	; 24/10/2019
	; release disk sector
	; 16/10/2019 - Retro UNIX 386 v2 (UNIXHDFS)
	; DX:AX = Sector to be freed/released
	; Modified registers: cx,bx ; 07/11/2019
	;	cf = 1 -> error

	; Calculate free map sector for requested sector in dx:ax

	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx

	mov	bx,ax
	and	bx,4095 ; 1FFh ; 28/10/2019

	;mov	cx,4096
	;call	div32
	mov	cx,12
	call	shr32

	; 23/03/2022 (BugFix)
	;mov	word ptr [save_bx],bx ; ***

	cmp	ax,word ptr [fm_sector]
	jne	short free_1
	cmp	dx,word ptr [fm_sector+2]
	je	short free_3 ; (!*!)
free_1:
	; 27/11/2019
	;push	bx ; ***

	; 23/03/2022
	; 19/01/2020
	mov	word ptr [save_bx],bx ; ***

	; 10/12/2019
	mov	bx,offset fbm_buffer  

	test 	byte ptr [smod],1 ; free map modified flag
	jz	short free_2

	push	dx ; **
	push	ax ; *

	;call	sync ; writes current free map sector/block
	;	   ; and updates super block

	; 10/12/2019
	mov	ax,word ptr [fm_sector]
	mov	dx,word ptr [fm_sector+2]

	add	ax,word ptr [systm.sb_FreeMapAddr]
	adc	dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov	bx,offset fbm_buffer 	
	call	dskwr
	;jc	short free_9

	; 19/01/2020
;free_9:
	pop	ax ; *
	pop	dx ; **
	jc	short free_7

	; 19/01/2020
	and	byte ptr [smod],0FEh ; reset bit 0
free_2:
	; 07/11/2019
	push	dx ; **
	push	ax ; *

	add	ax,word ptr [systm.sb_FreeMapAddr]
	adc	dx,word ptr [systm.sb_FreeMapAddr+2]

	;mov	bx,offset fbm_buffer ; 10/12/2019	
	call	dskrd
	
	pop	ax ; *
	pop	dx ; **
	;pop	bx ; *** ; 19/01/2020
	;jc	short free_8
	jc	short free_7 ; 19/01/2020

	; 07/11/2019
	mov	word ptr [fm_sector],ax
	mov	word ptr [fm_sector+2],dx
;free_3:; 23/03/2022 (Bug! wrong jump location..) (!*!)	
	; 19/01/2020
	mov	bx,word ptr [save_bx] ; *** ; 0 to 4095
free_3:	; 23/03/2022 (BugFix) (correct jump location) (!*!)
	;mov	cx,bx
	mov	cl,bl ; 19/01/2020
	;mov	ax,1
	mov	al,1 ; 19/01/2020	
	;and	cx,0Fh
	and	cl,7 ; 19/01/2020
	jz	short free_4
	;shl	ax,cl
	shl	al,cl ; 19/01/2020
free_4:
	shr	bx,1
	shr	bx,1
	shr	bx,1
	; 19/01/2020 (bx = byte offset, 0 to 511)
	;shr	bx,1

	;shl	bx,1 ; byte offset with word boundary

	add	bx,offset fbm_buffer

	; BX = byte offset in fbm_buffer
	; DX = bit position to be set

	;or	word ptr [bx],ax
	; 19/01/2020
	or	byte ptr [bx],al

	or	byte ptr [smod],1 ; set free map modified flag
				  ; in super block
	; 28/10/2019
	; Do not change invalid count of free blocks
	; (0FFFFFFFFh) value here! -otherwise it may be a mistake!-
	mov	cx,word ptr [systm.sb_FreeBlocks] 	
	and	cx,word ptr [systm.sb_FreeBlocks+2]
	cmp	cx,0FFFFh 
	jnb	short free_5
	
	add	word ptr [systm.sb_FreeBlocks],1 	
	adc	word ptr [systm.sb_FreeBlocks+2],0
	; 28/10/2019
	or	byte ptr [smod],0F0h  ; set sb modified flag
free_5:	
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]	

	cmp	dx,word ptr [systm.sb_FirstFreeBlk+2]
	;ja	short free_8
	ja	short free_7 ; 19/01/2020
	jb	short free_6	

	cmp	ax,word ptr [systm.sb_FirstFreeBlk]
	;jnb	short free_8
	jnb	short free_7 ; 19/01/2020
free_6:
	mov	word ptr [systm.sb_FirstFreeBlk],ax
	mov	word ptr [systm.sb_FirstFreeBlk+2],dx
	; 28/10/2019
	or	byte ptr [smod],0F0h  ; set sb modified flag
	;clc
	;retn	; 19/01/2020 (free_7 -> retn)
free_7:
	;pop	bx ; *** ; 19/01/2020
;free_8:
	retn

free	endp

setimod proc near
	; 25/01/2020
	; 05/01/2020
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
	; Modified registers: bx (in 'epoch'), cx

	push	dx ; 05/01/2020
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
        pop	dx ; 05/01/2020
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
	;jb	short setimod_2	    ; inode is changed"
	jb	short setimod_6 ; 30/09/2019

	; 19/09/2019
	; File/Dir data (File size or content) is same but
	; inode's mode, link count, owner or group id is changed
	; (so, we do not change last modification date&time)
	mov	byte ptr [imodx], 0 ; reset inode modified (extended) flag
	jmp	short setimod_5

setimod endp

clear	proc near
	; 13/11/2019 - Retro UNIX 386 v2
	; (Data) Buffer clearing
	
	push	di
	push	cx
	push	ax

 	xor	ax,ax

	mov	di,offset Buffer
	mov	cx,256 
	rep	stosw

	pop	ax
	pop	cx
	pop	di	
@@:	
	retn

clear	endp

make_directory proc near
	; 15/04/2022 ('number of links' bugfix)
	; 17/01/2020
	; 01/01/2020
	; 04/12/2015 (14 byte directory names)	
	; 30/12/2012
	;
	; mov	word ptr [u.namep], si

	call	sys_mkdir
	;jc	short @f
	jc 	short @b ; 15/04/2022
	
        ;ax = i-number

	;bx = i_number ; 01/01/2020

	;mov	ax, word ptr [ii]
	;mov	word ptr [u_dirbuf], ax

	mov	word ptr [u_namep], offset dot ; ('.')
	xor	ax, ax
	mov	word ptr [u_dirp], ax ; 0
	; 17/01/2020
	mov	word ptr [u_dirp+2], ax ; 0 ; 01/01/2020 	
	call	mk_dir 	; make a directory entry 
			; in current (ii) directory
	;jc	short @f
	jc	short @b ; 15/04/2022

	mov	word ptr [u_dirp], 16 ; 04/12/2015 (10 -> 16)
	; 17/01/2020
	mov	word ptr [u_dirp+2], 0 ; 01/01/2020 (32 bit value)
	mov	ax, word ptr [pdir] 
	mov	word ptr [u_dirbuf], ax
	mov	word ptr [u_namep], offset dotdot ; ('..')

	; 15/04/2022
;	;call	mk_dir
;	; 20/01/2020
;	;jmp	short mk_dir
;;@@:
;	;retn

	call	mk_dir	; make a directory entry 
			; in current (ii) directory
	; 15/04/2022
	;jc	short @f
	jc	short @b ; 15/04/2022
	call	mkdir_inc_nlinks	
	mov	bx, word ptr [pdir] ; parent dirs i-number
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

mk_dir proc near
	; 17/01/2020
	; 15/01/2020
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
	;jsr r0,copyz; u.dirbuf+2; u.dirbuf+10. / clear this
	mov	cx, 7  ; ; 04/12/2015 (4 -> 7)
	xor	ax, ax
	mov	di, offset u_dirbuf+2
	rep 	stosw	
         
        mov	si, word ptr [u_namep] ; mov u.namep,r2 
			; r2 points to name of directory entry

	mov	di, offset u_dirbuf+2 ; mov $u.dirbuf+2,r3
			; r3 points to u.dirbuf+2
mkdir_1: ;1 / put characters in the directory name in u.dirbuf+2 - u.dirbuf+16
	lodsb	;movb  (r2)+,r1 / move character in name to r1
	and	al, al
	jz	short mkdir_2	; beq 1f / if null, done
	cmp	al, '/'		; cmp	r1,$'/ / is it a "/"?
	je	short mkdir_stc	; beq error9 / yes, error
	; 04/12/2015 (10 -> 16)
	cmp	di, offset u_dirbuf+16 ; cmp r3,$u.dirbuf+10. 
		; have we reached the last slot for a char?
	je	short mkdir_1	; beq 1b / yes, go back
	
	stosb	; movb r1,(r3)+ 
		; no, put the char in the u.dirbuf
	jmp	short mkdir_1	; br 1b / get next char

	; 17/01/2020
mkdir_stc:
	; invalid file name, al="/", ah=0
	mov	ah, 1
	stc
@@:	; 17/01/2020
	retn

mkdir_2: ;1
	mov	ax, word ptr [u_dirp] ; mov u.dirp,u.off
	mov	dx, word ptr [u_dirp+2] ; 01/01/2020 (32 bit offset)
	mov	word ptr [u_off], ax  ; pointer to empty current directory
				      ; slot to u.off
	; 15/01/2020
	mov	word ptr [u_off+2], dx ; 01/01/2020
wdir:
	mov	word ptr [u_base], offset u_dirbuf
			; mov$u.dirbuf,u.base
				; u.base points to created file name
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16)
			; mov $10.,u.count
 	;mov	ax, word ptr [ii]  ; mov ii,r1 
	; 29/12/2019 
	mov	bx, word ptr [ii]
	
		; r1 has i-number of current directory
	;call	writei	; jsr r0,writei / write into directory
;@@:	
	;retn	; rts r0
	
	; 17/01/2020
	;jmp	short writei 
	
mk_dir 	endp

writei	proc near
	; 15/01/2020
	; 01/01/2020
	; 17/12/2019
	; 09/11/2019
	; 07/11/2019
	; 03/09/2mmn019 - Retro UNIX 386 v2
	; 31/10/2012
	; 18/08/2012
	; 17/07/2012
	; BX = R1, i-number
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
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
 
	xor	ax,ax ; 0	   ; clr u.nread	
	mov	word ptr [u_nread],ax  ; clear the number of bytes transmitted during
		   ; read or write calls 
		   ; tst u.count	
	cmp	word ptr [u_count],ax ; test the byte count specified by the user
	;;ja	short write_1 ; 1f    ; bgt 1f / any bytes to output; yes, branch
	;;retn	   ; rts 0 / no, return - no writing to do
	;jna	short @f
	jna	short @b ; 03/09/2019

write_1:
	;cmp	bx, 40	; cmp	r1,$40.
		; does the i-node number indicate a special file?
	;ja	short dskw_0 ; bgt dskw / no, branch to standard file output
;@@:
;	retn

;	shl	bx,1	; asl r1 
		; yes, calculate the index into the special file
;	cmp	bx,offset write_3 - offset writei_2 + 2
;	ja	short writei_error

;	jmp	word ptr [write_2][BX]-2 ; *1f-2(r1)
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
	; 15/01/2020
	; 01/01/2020
	; 29/12/2019
	; 17/12/2019
	; 04/12/2019
	; 07/11/2019
	; 03/09/2019 - Retro UNIX 386 v2
	; 01/03/2013
	; 31/10/2012
	; 19/8/2012
	; 30/7/2012
     	; 17/7/2012
     	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	; dskw: write routine for non-special files
	;
	; RETRO UNIX v1 FS
	; initialization/format version
	;
	; write data to a file
	;
	; BX (R1) = I-node number

dskw_0:
	push	di
	push	si

	push	bx	; save i-number on stack

	mov	byte ptr [Error],0FFh ; 03/09/2019

	call	iget 	; jsr r0,iget
	  	; write i-node out (if modified), read i-node 'r1'
	        ; into i-node area of core
	;jc	dskw_5 ; 01/03/2013
	jnc	short @f
	; 27/03/2022
	jmp	dskw_5
@@:
	; 03/09/2019 - Retro UNIX 386 v2
	mov	al,byte ptr [inode_flgs+1]
	test 	al,80h	     ; regular file ?
	jnz	short dskw_8 ; yes
	test 	al,20h 	     ; device file ?
	;;jnz	short dskw_7 ; yes
	;jz	short dskw_8 ; 07/09/2019
	;;test 	al,40h	     ; directory ?
	;;jnz	short dskw_8 ; yes
	jnz	short dskw_7 ; 04/12/2019	
dskw_8:
	;mov	si,word ptr [u_fofp]
	; 29/12/2019
	mov	si,offset u_off

	;mov	dx,word ptr [si] 
		; mov	*u.fofp,r2 
		; put the file offset [(u.off) or the offset in
	        ; the fsp entry for this file] in r2
	;add	dx,word ptr [u_count]
		; add u.count,r2 
		; no. of bytes to be written + file offset is
	        ; put in r2

	;cmp	dx,word ptr [inode_size] ; cmp	r2,i.size
		; is this greater than the present size of
		; the file?
	;jna	short dskw_1 ; blos 1f / no, branch

	;mov	word ptr [inode_size],dx ; mov	r2,i.size 
		; yes, increase the file size to file offset +
		; no. of data bytes

	; 09/11/2019 - 32 bit file size
	mov	ax,word ptr [si]
	mov	dx,word ptr [si+2]

	; 01/01/2020
	add	ax,word ptr [u_count]
		; add u.count,r2 
		; no. of bytes to be written + file offset is
		; put in r2
	adc	dx,0
	
	cmp	dx,word ptr [inode_size+2]
	je	short dskw_9	
	jb	short dskw_1
	mov	word ptr [inode_size+2],dx 
	jmp	short dskw_10
dskw_7:
	stc
	jmp	short dskw_5
dskw_9:
	cmp	ax,word ptr [inode_size]
	jna	short dskw_1	
dskw_10:  
	mov	word ptr [inode_size],ax

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

	; 09/11/2019
	; dx:ax = Block/Sector number

	; Check current sector in the buffer
	
	cmp	byte ptr [buff_m],1
	jb	short dskw_12

	mov	cx,word ptr [buff_s]
	mov	bx,word ptr [buff_s+2]
	;mov	si,cx
	;or	si,bx
	;jz	short dskw_12 ; buff_s = 0 is invalid	
	;cmp	si,0FFFFh     ; buff_s = 0FFFFh is invalid
	;je	short dskw_12

	; write buffer content if sector is not same

	cmp	cx,ax
	jne	short dskw_11
	cmp	bx,dx
	je	short dskw_12	
dskw_11:
	push	dx
	push	ax
	; 17/12/2019
	;mov	ax,word ptr [buff_s]
	;mov	dx,word ptr [buff_s+2]
	mov	ax,cx
	mov	dx,bx
	mov	bx,offset Buffer
	call	dskwr
	pop	ax
	pop	dx
	jc	short dskw_5
	mov	byte ptr [buff_m],0
dskw_12:
	;mov	si,word ptr [u_fofp]

	; 29/12/2019
	;mov	si,offset u_off
	;mov	bx,word ptr [si]

	mov	bx,word ptr [u_off] ; 15/01/2020

	and	bx,1FFh	; bit *u.fofp,$777
		; test the lower 9 bits of the file offset
	mov	bx,offset Buffer ; 04/12/2019
	jnz	short dskw_2 ; bne 2f 
		; if its non-zero, branch; if zero, file offset = 0,
	   	; 512, 1024,...(i.e., start of new block)
	cmp	word ptr [u_count],512 ; cmp u.count,$512.
		; if zero, is there enough data to fill an
	        ; entire block? (i.e., no. of
	;jnb	short dskw_6 ; bhis 3f
		; / bytes to be written greater than 512.? 
		; Yes, branch. / Don't have to read block
	jnb	short dskw_3 ; 09/11/2019
dskw_2: ; 2
	; in as no past info. is to be saved (the entire block will be
        ; overwritten).

	; 09/11/2019
	;mov	bx,offset Buffer ; 04/12/2019
	cmp	ax,word ptr [buff_s]
	;jne	short dskw_13
	jne	short dskw_6
	cmp	dx,word ptr [buff_s+2]
	je	short dskw_3
;dskw_13:
dskw_6:
	;mov	bx,ax	; R1 (block number)
	call	dskrd 	; jsr r0,dskrd 
		; no, must retain old info.. Hence, read block 'r1'
	        ; into an I/O buffer
	;jc	short dskw_5 ; 01/03/2013
	; 04/12/2019
	jnc	short dskw_3
dskw_5:
	pop	bx

	pop	si
	pop	di

	retn

dskw_3: ; 3
	;call	wslot

	call	sioreg

	; SI = user data offset (r1)
	; DI = sector (I/O) buffer offset (r2)
	; CX = byte count (r3)

dskw_4: ; 2
	rep	movsb

	mov	byte ptr [buff_m],1
	; 09/11/2019
	mov	word ptr [buff_s],ax
	mov	word ptr [buff_s+2],dx
	;mov	bx,offset Buffer ; offset sector_buffer	
	call	dskwr ; jsr r0,dskwr / write the block and the i-node
        jc	short dskw_5

	mov	byte ptr [buff_m],0
	
        cmp	word ptr [u_count],0 ; any more data to write?
	;ja	dskw_1 ; 1b ; yes, branch
	; 01/01/2020
	jna	short @f
	jmp	dskw_1
@@:
	mov	byte ptr [Error],0 ; 03/09/2019
	; 04/12/2019
	jmp	short dskw_5
;dskw_5:
;	pop	bx
;
;	pop	si
;	pop	di
;
;	retn

;dskw_6:
	;cmp	byte ptr [buff_m],1
	;jb	short dskw_3
	;mov	bx,offset Buffer ; 09/11/2019
	;call	dskwr
	;jc	short dskw_5
	;mov	word ptr [buff_s],ax ; block number from mget procedure
	;mov	word ptr [buff_s+2],dx 
	;cmp	byte ptr [buff_m],0
	;jmp	short dskw_3

dskw 	endp

dskwr	proc near
	; 14/01/2020
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
	; Modified registers: cx

	mov	byte ptr [rw],3 ; write
	jmp	short diskio

dskwr	endp
	
dskrd	proc near
	; 14/01/2020
	; 30/12/2019
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

	mov	byte ptr [rw],2 ; read
diskio:
	mov	word ptr [Error],0
	mov	byte ptr [RetryCount],4

	; 07/11/2019
	push	dx ; *
	push	ax ; **

	; 30/12/2019 (dskrd is called before reading super block)
	;	  (so, it is better to use hidden_sectors directly)
	;	
	;add	ax,word ptr [systm.sb_HiddenSects]
	;adc	dx,word ptr [systm.sb_HiddenSects+2] 
	add	ax,word ptr [hidden_sectors]   ; = [systm.sb_BootSectAddr]
	adc	dx,word ptr [hidden_sectors+2] ; = [systm.sb_BootSectAddr+2]

	cmp	byte ptr [Drive],90h ; Hard disk image file sign
	;;je	short image_file_rw
	;jnb	image_file_rw
	; 27/03/2022
	jb	short @f
	jmp	image_file_rw
@@:
	; 27/11/2019
	and	dx,dx
	jz	short chs_read_write

	cmp	dx,word ptr [CHS_limit+2]
	ja	short lba_read_write
	jb	short chs_read_write

	cmp	ax,word ptr [CHS_limit]
	ja	short lba_read_write
chs_read_write:
	push	dx ; ***
	push	ax ; ****

	push	bx ; *****
	
	mov	cx,word ptr [sectors] ; spt
	call	div32	; Special 32 bit divide !!!
	        ; To fix large disk problem.
	        ; by Erdogan Tan
	        ; (October 20th, 1999)

	mov	cx,bx	;Sector (zero based)
	inc	cx	; To make it 1 based
	push	cx ; ******
	mov	cx,word ptr [heads]
	call	div32
	mov	dh,bl	; bx = head (max. 255)
	pop	cx ; ****** ; ax=cylinder, dh=head, cx=sector

	pop	bx ; *****  ; es:bx = buffer address

	mov	dl,byte ptr [drive] ; physical drive number
	mov	ch,al
	ror	ah,1
	ror	ah,1
	;and	cl,63
	or	cl,ah

	mov	ah,byte ptr [rw] ; 2 = read, 3 = write
 	mov	al,1 ; 1 sector
	int	13h  ; ROM BIOS Service func (ah) = 2
		; Read disk sectors
		; AL-sec num CH-track CL-sec
   		; DH-head DL-drive ES:BX-buffer
 		; CF-flag AH-stat AL-sec read
		; If CF = 1 then (If AH > 0)

	jnc	short chs_rw_ok	
	
	; dl = physical drive number

	dec	byte ptr [RetryCount]
	jz	short chs_err_retn

	cmp	ah,09h ; DMA crossed 64K segment boundary
	je	short chs_err_retn

	xor	ah,ah ; reset
	int	13h

	pop	ax ; ****
	pop	dx ; ***

	jmp	short chs_read_write ; read (or write) again	

chs_err_retn:
lba_err_retn:
	stc
	mov	byte ptr [Error],ah
chs_rw_ok:
	; 12/12/2019
lba_rw_ok: ; 03/12/2019
	pop	ax ; ****
	pop	dx ; ***
@@:
	pop	ax ; **
	pop	dx ; *
	retn

lba_read_write:
	; 20/07/2022
	; 08/01/2020
	; 18/12/2019
	; 12/12/2019
	; 07/11/2019
	; LBA read/write
	or	byte ptr [rw],40h ; 42h = read, 43h = write
	
	; 06/11/2019
	;cmp	byte ptr [systm.sb_LBA_rw],1
	;jnb	short lba_read_again
	; 20/07/2022
	cmp	byte ptr [LBA_rw],1
	jnb	short lba_read_again

lba_not_ready:
	mov	byte ptr [Error],0FFh
	;stc
	;retn
	jmp	short @b ; 08/01/2020

lba_read_again:
	push	dx ; ***
	push	ax ; ****

	; 18/12/2019
	push	si ; *****

	; 03/12/2019
	xor	cx,cx
	;push 0
	push	cx ; 6*
	;push 0
	push	cx ; 7*

	push	dx ; 8*
	push	ax ; 9*
	push	es ; 10*
	push	bx ; 11*

	;push 1
	mov	cl,1
	push	cx ; 12*
	;push 16
	mov	cl,16
	push	cx ; 13*
	
	mov	si,sp
	mov	dl,byte ptr [drive]
	mov	ah,byte ptr [rw]
	xor	al,al ; verify off
	int	13h
	pop	ax ; 13*
	pop	ax ; 12*
	pop	bx ; 11*
	pop	es ; 10*
	pop	ax ; 9*
	pop	dx ; 8*
	pop	cx ; 7*
	pop	cx ; 6*
	; 18/12/2019
	pop	si ; *****
	jnc	short lba_rw_ok	

	; dl = physical drive number

	dec	byte ptr [RetryCount]
	jz	short lba_err_retn

	cmp	ah,09h ; DMA crossed 64K segment boundary
	je	short lba_err_retn

	xor	ah,ah ; reset
	int	13h

	pop	ax ; ****
	pop	dx ; ***

	jmp	short lba_read_again ; read again

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

	cmp	dx,3Fh  ; Max. 2GB disk image file (32bit seek limit)
	jna	short hd_img_file_rw

invalid_disk_image_sector:
	stc
image_file_rw_err:
	mov	byte ptr [Error],0FFh
image_file_rw_ok:
	pop	ax ; **
	pop	dx ; *
	retn
	
hd_img_file_rw:
	push	dx ; ***
	push	ax ; ****
	push	bx ; ***** ; buffer offset
	push	bx ; ******
	; dx:ax = sector number 
	mov	cx,512
	call	mul32
	; dx:ax = byte offset
	mov	cx,dx
	mov	dx,ax
	sub	al,al ; specified offset is from the beginning of the file
	mov	ah,42h ; seek (move file pointer)	
	mov	bx,word ptr [img_file_handle]
	int	21h
	pop	dx ; ****** ; buffer offset
	jc	short hd_img_file_rw_err

	;mov	bx,word ptr [img_file_handle]
	mov	cx,512
	; ds:dx = buffer offset
	mov	ah,3Dh ; 3Fh = read from file, 40h = write to file
	add	ah,byte ptr [rw] ; 2 = read, 3 = write 
	int	21h
	jc	short hd_img_file_rw_err
	cmp	byte ptr [rw],2  ; read ?
	je	short hd_img_file_rw_ok
	cmp	ax,cx ; ax = actually written bytes
	jnb	short hd_img_file_rw_ok

hd_img_file_rw_err:
	;pop bx ; *****
	;pop ax ; ****
	;pop dx ; ***
	;jmp	short image_file_rw_err

	mov	byte ptr [Error],0FFh

hd_img_file_rw_ok:
	pop	bx ; *****
	pop	ax ; ****
	pop	dx ; ***
	jmp	short image_file_rw_ok

image_file_rw endp

; 17/12/2019

sioreg proc near
	; 29/12/2019	
	; 09/11/2019 - Retro UNIX 386 v2 (for hard disk fs)
	; 04/09/2019 - RETRO UNIX v0.3 (Retro UNIX 386 v2)
	; 16/12/2012
	; 31/10/2012
	; 19/08/2012
	; 04/08/2012
	; Erdogan Tan - RETRO UNIX v0.1
	; input ->
	;	R5 (DX) = sector buffer (data) address
	;	*u.fofp = file offset, to start writing
	;	u.base  = address of 1st byte of user data
	;	u.count = byte count to be transferred
	;	u.nread = number of bytes written out
	;		  previously.
	; output ->
	;	*u.fofp = last (written) byte + 1
	;	u.count = number of bytes of data left
	;		  to be transferred.		
	;	u.nread = updated to include the count
	;		  of bytes to be transferred.
	;	R1 (SI) = address of 1st byte of data
	;	R2 (DI) = specifies the byte in IO 
	;		  sector (I/O) buffer. (Offset)	
	;	R3 (CX) = number of bytes of data to be
	;		  transferred to/from sector (I/O)
	;		  buffer.
	;
	; Modified registers: cx, si, di	

	;mov	dx,offset Buffer     ; R5
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
	; 25/01/2020
	; 29/12/2019
	; 14/11/2012
	; unixboot.asm (boot file configuration)
	; version of "epoch" procedure in "unixproc.asm"
	; 21/7/2012
	; 15/7/2012
	; 14/7/2012	
	; Erdogan Tan - RETRO UNIX v0.1
	; compute current date and time as UNIX Epoch/Time
	; UNIX Epoch: seconds since 1/1/1970 00:00:00

	; 25/01/2020
	; Modified registers: ax, bx, cx, dx

	; 21/7/2012
	;push	bx
	;push	cx

	mov	ah, 02h	      ; Return Current Time
        int	1Ah
        xchg	ch,cl
        mov	word ptr [hour], cx
        xchg	dh,dl
        mov	word ptr [second], dx

        mov	ah, 04h	      ; Return Current Date
        int	1Ah
        xchg	ch,cl
        mov	word ptr [year], cx
        xchg	dh,dl
        mov	word ptr [month], dx

	mov	cx, 3030h

	mov	al, byte ptr [hour] ; Hour
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov	byte ptr [hour], al

	mov	al, byte ptr [hour]+1 ; Minute
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov	byte ptr [minute], al

	mov	al, byte ptr [second] ; Second
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov	byte ptr [second], al

	mov	ax, word ptr [year] ; Year (century)
        push	ax
	   ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov	ah, 100
	mul	ah
	mov	word ptr [year], ax

	pop	ax
	mov	al, ah
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	add	word ptr [year], ax

	mov	al, byte ptr [month] ; Month
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL
	
	mov	byte ptr [month], al	

	mov	al, byte ptr [month]+1 ; Day
           ; AL <= BCD number)
        db 0D4h,10h	; Undocumented inst. AAM
			; AH = AL / 10h
			; AL = AL MOD 10h
        aad ; AX= AH*10+AL

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

	cmp	byte ptr [month], 2  ; if past february
	jna	short @f
	mov	cx, word ptr [year]
	and	cx, 3 ; year mod 4
	jnz	short @f	
		; and if leap year
	add	ax, 1 ; add this year's leap day (february 29)
	adc	dx, 0
@@: 		; compute seconds since 1/1/1970
	mov	cx, 24 ; 29/12/2019
	call	mul32

	mov	bl, byte ptr [hour]
	add	ax, bx
	adc	dx, 0
	
	;mov	cx, 60 ; 29/12/2019
	mov	cl, 60	
	call	mul32

	mov	bl, byte ptr [minute]
	add	ax, bx
	adc	dx, 0
	
	;;mov	cx, 60 ; 29/12/2019
	;mov	cl, 60	
	call	mul32 ; 29/12/2019

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
	;mov	word ptr [iday], ax   ; whole hours
		  		      ; since 1/1/1970
	;mov	word ptr [iday]+2, dx ; DX = 0
	mov	word ptr [hour], bx   ; leftover hours
	add	ax, 365+366	  ; whole day since
		  		  ; 1/1/1968 	
	;adc	dx, 0	          ;  DX = 0
	;mov	word ptr [iday], ax
	push	ax
	mov	cx, (4*365)+1	  ; 4 years = 1461 days
	call	div32
	pop	cx
	;mov	word ptr [lday], ax  ; count of quadyrs (4 years)
	push	bx
	;mov	word ptr [qday], bx  ; days since quadyr began
	cmp	bx, 31 + 29	     ; if past feb 29 then
	cmc			; add this quadyr's leap day
	adc	ax, 0		; to # of qadyrs (leap days)
	;mov	word ptr [lday], ax  ; since 1968		  
	;mov	cx, word ptr [iday]
	xchg	cx, ax	  	; CX = lday, AX = iday	  
	sub	ax, cx	  	; iday - lday
	mov	cx, 365
	;xor	dx, dx	  	; DX  = 0
	; AX = iday-lday, DX = 0
	call	div32
	;mov	word ptr [iyrs], ax  ; whole years since 1968
	; jday = iday - (iyrs*365) - lday
	;mov	word ptr [jday], bx  ; days since 1/1 of current year
	add	ax, 1968	; compute year
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
@@:	; Month calculation	  ; 0 to 11  (11 to 0)	
	cmp	cx, ax		; mday = # of days passed from 1/1
	jnb	short @f
	dec	bx		; month = month - 1
	shl	bx, 1 
	mov	ax, word ptr DMonth[BX] ; # elapsed days at 1st of month
	shr	bx, 1		; bx = month - 1 (0 to 11)
	cmp	bx, 1		; if month > 2 and year mod 4  = 0	
	jna	short @b	; then mday = mday + 1
	or	dl, dl		; if past 2/29 and leap year then
	jnz	short @b	; add leap day (to mday)
	inc	ax		; mday = mday + 1
	jmp	short @b
@@:
	inc	bx		; -> bx = month, 1 to 12
	mov	word ptr [month], bx
	sub	cx, ax		; day = jday - mday + 1	
	inc	cx 		  
	mov	word ptr [day], cx
	
	; ax, bx, cx, dx is changed at return
	; output ->
	; [year], [month], [day], [hour], [minute], [second]
	; 

	retn

convert_from_epoch endp

mul32 proc near
	; DX_AX*CX
	; Result: BX_DX_AX 

	push	cx

	mov	bx,dx
	mul	cx
	xchg	ax,bx

	push	dx
	mul	cx 
	pop	cx 

	add	ax,cx 
	adc	dx,0
	xchg	bx,ax
	xchg	dx,bx

	pop	cx

	retn

mul32 endp

div32 proc near
	; 1999
	; (Rx_Dos_Div32) 32 bit divide procedure 
	; by Erdogan Tan
	; Input ->
	;	DX_AX = 32 bit dividend
	;          CX = 16 bit divisor
	; output ->
	;	DX_AX = 32 bit quotient
	;          BX = 16 bit remainder
	;
	mov	bx, dx
	xchg	ax, bx
	xor	dx, dx
	div	cx	; at first, divide DX
	xchg	ax, bx	; remainder is in DX
			; now, BX has quotient
			; save remainder
	div	cx	; so, DX_AX divided and
			; AX has quotient
			; DX has remainder
	xchg	dx, bx	; finally, BX has remainder

;sync_0: ; 19/09/2019
	retn

div32 endp

sync 	proc near
	; 05/01/2020
	; 02/01/2020
	; 01/01/2020
	; 30/12/2019
	; 26/12/2019
	; 28/09/2019
	; 18/09/2019 - Retro UNIX 386 v2 (RUNIX v2 file system)
	;
	; 14/07/2015
	; 07/07/2015
	; 18/11/2012 unix boot file configuration version
	; of "sync" procedure of retro unix v1.0 by Erdogan Tan
	; 12/8/2012
	; updates super block and the last i-node on disk 
	; if modified
	; e.g. smod = 1, imod = 1, buffer_m = 1
	;
	; RETRO UNIX v1 FS

	;xor	ax, ax ; mov ax, 0

	; 26/12/2019
	xor	bx, bx ; mov bx,0

	call	iget ; (write modified i-node)
	;;;jc	short sync_3
	;;jc	short sync_0 ; 19/09/2019
	;jc	short sync_18 ; 01/01/2020
	jnc	short @f
	jmp	sync_18
@@:
	; 01/01/2020
	cmp	byte ptr [buff_m],0
	jna	short sync_0

	mov	ax,word ptr [buff_s]
	mov	dx,word ptr [buff_s+2]
	mov	bx,offset Buffer
	call	dskwr
	;jc	short sync_18
	jnc	short @f
	jmp	sync_18
@@:
	mov	byte ptr [buff_m],0
sync_0:
	cmp	byte ptr [smod],0
	;jna	sync_17
	; 01/01/2020
	ja	short @f
	jmp	sync_17	
@@:
	; 29/09/2019
	; set super block modification time
	call	epoch
	mov	word ptr [systm.sb_ModifTime],ax
	mov	word ptr [systm.sb_ModifTime+2],dx

	test 	byte ptr [smod],2 ; inode map modified 
	jz	short sync_7

	xor	dx,dx
	mov	ax,word ptr [im_sector]
	add	ax,word ptr [systm.sb_InodeMapAddr]
	adc	dx,word ptr [systm.sb_InodeMapAddr+2]
	mov	bx,offset im_buffer

	call	dskwr
	;jc	short sync_18
	; 01/01/2020
	jnc	short @f
	jmp	sync_18	
@@:
	and	byte ptr [smod],0FDh ; reset bit 1

	; 01/01/2020
	; set free inodes number
	; if it is invalid (in sb)

	cmp	word ptr [systm.sb_FreeInodes],0FFFFh ; invalid
	jb	short sync_7

	mov	cx,word ptr [systm.sb_InodeCount] ; <= 65534
	mov	word ptr [count],cx
	mov	ax,word ptr [systm.sb_InodeMapAddr]
	mov	dx,word ptr [systm.sb_InodeMapAddr+2]
	xor	bp,bp ; 0
	mov	word ptr [im_sector],bp ; 0
sync_1:
	mov	bx,offset im_buffer
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx
	call	dskrd
	jc	short sync_7
sync_2:
	mov	cx,8
	mov	al,byte ptr [bx] ; 02/01/2020
sync_3:
	shr	al,1	; 02/01/2020
	jc	short sync_4 ; allocated inode
	inc	bp	; free inode
sync_4:
	dec	word ptr [count]
	jz	short sync_6
	loop	sync_3
	cmp	bx,offset im_buffer+511
	jnb	short sync_5
	inc	bx
	jmp	short sync_2
sync_18:
	mov	ax,word ptr [Error]
	retn
sync_5: 
	inc	word ptr [im_sector]
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	add	ax,1
	adc	dx,0
	jmp	short sync_1
sync_6:
	mov	word ptr [systm.sb_FreeInodes],bp
	; ***
	jmp	short sync_19 ; 01/01/2020
sync_7:
	; 01/01/2020
	; 29/09/2019
	; set first free inode number
	; if it is invalid (in sb)

	mov	dx,0FFFFh
	cmp	word ptr [systm.sb_FirstFreeIno],dx ; 0FFFFh
	jne	short @f
	cmp	word ptr [systm.sb_FirstFreeIno+2],dx ; 0FFFFh	
	jne	short @f
sync_19:
	call	set_firstfreeinode
@@:
	test 	byte ptr [smod],1 ; free map modified
	;jz	sync_15
	; 27/03/2022
	jnz	short sync_22
	jmp	sync_15
sync_22:
	mov	ax,word ptr [fm_sector]
	mov	dx,word ptr [fm_sector+2]

	add	ax,word ptr [systm.sb_FreeMapAddr]
	adc	dx,word ptr [systm.sb_FreeMapAddr+2]

	mov	bx,offset fbm_buffer 	
	call	dskwr
	jc	short sync_18

	and	byte ptr [smod],0FEh ; reset bit 0

	; 01/01/2020
	; set free blocks number
	; if it is invalid (in sb)

	mov	ax,word ptr [systm.sb_FreeBlocks] 	
	and	ax,word ptr [systm.sb_FreeBlocks+2]
	cmp	ax,0FFFFh 
	;jb	sync_15
	; 01/01/2020
	jnb	short @f
	jmp	sync_15
@@:
	mov	ax,word ptr [systm.sb_VolumeSize]
	mov	dx,word ptr [systm.sb_VolumeSize+2]	
	mov	word ptr [count],ax
	mov	word ptr [count+2],dx

	mov	ax,word ptr [systm.sb_FreeMapAddr]
	mov	dx,word ptr [systm.sb_FreeMapAddr+2]
	xor	si,si
	xor	di,di
	mov	word ptr [fm_sector],si ; 0
	mov	word ptr [fm_sector+2],di
sync_8:
	mov	bx,offset fbm_buffer
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx
	call	dskrd
	jc	short sync_15
sync_9:
	mov	cx,8
	mov	al,byte ptr [bx] ; 02/01/2020
sync_10:	 	
	shr	al,1	; 02/01/2020
	jnc	short sync_11 ; allocated block/sector
	add	si,1	; free block/sector
	jnc	short sync_11
	;adc	di,0
	inc	di
sync_11:
	sub	word ptr [count],1
	ja	short sync_12

	; 02/01/2020
	sbb	word ptr [count+2],0
	jmp	short sync_12
sync_20:	
	; 01/01/2020
	cmp	word ptr [count+2],0
	jna	short sync_14
sync_12:
	loop	sync_10
	cmp	bx,offset fbm_buffer+511
	jnb	short sync_13
	inc	bx
	jmp	short sync_9
sync_13: 
	add	word ptr [fm_sector],1
	adc	word ptr [fm_sector+2],0	
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	add	ax,1
	adc	dx,0
	jmp	short sync_8
sync_14:
	mov	word ptr [systm.sb_FreeBlocks],si
	mov	word ptr [systm.sb_FreeBlocks+2],di
	; ***
	jmp	short sync_16 ; 01/01/2020
sync_15:	
	; 01/01/2020
	; set first free block number
	; set free blocks number
	; if it is invalid (in sb)

	mov	dx,0FFFFh
	cmp	word ptr [systm.sb_FirstFreeBlk],dx ; 0FFFFh
	jne	short @f
	cmp	word ptr [systm.sb_FirstFreeBlk+2],dx ; 0FFFFh	
	jne	short @f
sync_16:
	call	set_firstfreeblock
@@:
	xor	dx,dx
	mov	ax,1 ; Super block address (in 71h/RUNIX partition)
	mov	bx,offset systm 
	call	dskwr
	;jc	sync_18
	jnc	short @f
	jmp	sync_18
@@:
	mov	byte ptr [smod],0 ; reset
sync_17:
	; 05/01/2020
	xor	ax,ax
	test 	byte ptr [bmod], 0FFh ; boot sector modified ?
	jz	short sync_21
	xor	dx,dx
	;xor	ax,ax ; Boot sector/block address (in 71h/RUNIX partition)
	mov	bx,offset BSBUFFER
	call	dskwr
	jc	short sync_21
	mov	byte ptr [bmod],al ; 0 ; reset
sync_21:
	;xor	ax,ax
	retn

sync	endp

set_firstfreeinode proc near
	; 17/01/2020
	; 01/01/2020 - (set first free inode for 71h hard disk partition)
	; 29/09/2019 - Retro UNIX 386 v2
	; set first free inode number in sb
	; Output:
	;   cf = 0 -> calculated
	;   cf = 1 = can not be calculated 

	xor	bp,bp
	mov	word ptr [im_sector],bp ; 0

	mov	cx,word ptr [systm.sb_InodeCount] ; <= 65534
	mov	word ptr [count],cx
	mov	ax,word ptr [systm.sb_InodeMapAddr]
	mov	dx,word ptr [systm.sb_InodeMapAddr+2]
sffi_1:
	mov	bx,offset im_buffer
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx
	call	dskrd
	jc	short sffi_4
sffi_2:
	mov	cx,8
	mov	al,byte ptr [bx] ; 02/01/2020
sffi_3:
	inc	bp
	shr	al,1	; 02/01/2020
	jc	short sffi_5 ; allocated inode
	mov	word ptr [systm.sb_FirstFreeIno],bp ; first free inode
	; 17/01/2020
	mov	word ptr [systm.sb_FirstFreeIno+2],0 ; 0FFFFh -> 0
sffi_4:
	retn
sffi_5:
	dec	word ptr [count]
	jz	short sffi_4
	loop	sffi_3
	cmp	bx,offset im_buffer+511
	jnb	short sffi_6
	inc	bx
	jmp	short sffi_2
sffi_6: 
	inc	word ptr [im_sector]
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	add	ax,1
	adc	dx,0
	jmp	short sffi_1

set_firstfreeinode endp

set_firstfreeblock proc near
	; 02/01/2020
	; 01/01/2020 - (set first free block for 71h hard disk partition)
	; 29/09/2019 - Retro UNIX 386 v2
	; set first free block number in sb
	; Output:
	;   cf = 0 -> calculated
	;   cf = 1 = can not be calculated 

	mov	ax,word ptr [systm.sb_VolumeSize]
	mov	dx,word ptr [systm.sb_VolumeSize+2]	
	mov	word ptr [count],ax
	mov	word ptr [count+2],dx

	mov	ax,word ptr [systm.sb_FreeMapAddr]
	mov	dx,word ptr [systm.sb_FreeMapAddr+2]
	xor	si,si
	xor	di,di
	mov	word ptr [fm_sector],si ; 0
	mov	word ptr [fm_sector+2],di

	dec	si ; 0FFFFh
	dec	di ; 0FFFFh
sffb_1:
	mov	bx,offset fbm_buffer
	mov	word ptr [save_ax],ax
	mov	word ptr [save_dx],dx
	call	dskrd
	jc	short sffb_5
sffb_2:
	mov	cx,8
	mov	al,byte ptr [bx]
sffb_3:	 	
	add	si,1
	jnc	short sffb_4
	adc	di,0
sffb_4:	
	shr	al,1
	jnc	short sffb_6 ; allocated block/sector
	; free block/sector
	mov	word ptr [systm.sb_FirstFreeBlk],si
	mov	word ptr [systm.sb_FirstFreeBlk+2],di	
sffb_5:
	retn
sffb_6:
	sub	word ptr [count],1
	ja	short sffb_8
	jz	short sffb_7

	sbb	word ptr [count+2],0
	jmp	short sffb_8
sffb_7:	
	cmp	word ptr [count+2],0
	ja	short sffb_8
	stc
	retn
sffb_8:
	loop	sffb_3
	cmp	bx,offset fbm_buffer+511
	jnb	short sffb_9
	inc	bx
	jmp	short sffb_2
sffb_9: 
	add	word ptr [fm_sector],1
	adc	word ptr [fm_sector+2],0	
	mov	ax,word ptr [save_ax]
	mov	dx,word ptr [save_dx]
	add	ax,1
	adc	dx,0
	jmp	short sffb_1

set_firstfreeblock endp

find_bfn proc near
	; 15/01/2020
	; 29/12/2019
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

	;mov	ax, ROOT_DIR_INODE_NUMBER ; 41 (unix v1), 1 (runix v2)
	; 26/12/2019
	mov	bx, ROOT_DIR_INODE_NUMBER
	call	iget
	jc	short loc_find_bfn_retn

	;test	word ptr [inode_flgs], 4000h ; directory i-node ?
	;jnz	short @f

	;mov	ah, 0FFh ; error number
	;stc
	;jmp	short loc_find_bfn_retn
;;@@:
	xor	ax, ax 
	mov	word ptr [u_off], ax ; u_off is file offset used by user
	; 15/01/2020
	mov	word ptr [u_off+2], ax ; 29/12/2019 

loc_find_bfn_1:
	mov	word ptr [u_base], offset u_dirbuf
		  ; u.dirbuff holds a file name copied from
		  ; a directory	
	mov	word ptr [u_count], 16 ; 04/12/2015 (10 -> 16) 	
 	
	;mov	ax, ROOT_DIR_INODE_NUMBER ; = 1 ; 18/09/2019
	; 26/12/2019
	mov	bx, ROOT_DIR_INODE_NUMBER 

	call	readi ; read 16 bytes of file with i-number
		; i.e. read a directory entry
	jc	short loc_find_bfn_retn

	mov	ax, word ptr [u_nread]

	or	ax, ax
	jz	short loc_find_bfn_2 ; gives error return 

	; 29/12/2019
	;mov	ax, word ptr [u_dirbuf]
	mov	bx, word ptr [u_dirbuf]

	;cmp	ax, word ptr [uf_i_number] ; Check i-number of directory entry
	cmp	bx, word ptr [uf_i_number]
	jne	short loc_find_bfn_1	; if same with specified uf_i_number
					; it is the boot file 
loc_find_bfn_3:
	; 29/12/2019
	; 26/12/2019
	;mov	bx, ax
	call	iget
loc_find_bfn_retn:
	pop	si
	retn

loc_find_bfn_2:
	stc
	jmp	short loc_find_bfn_retn
	
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
	mov	ax, word ptr [SI]+bs_BF_I_number

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
	;call	PRINT_MSG
	;retn
	; 22/01/2020
	jmp	PRINT_MSG	  

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
	; 03/01/2020
	; 02/01/2020 - Retro UNIX 386 v2 Hard Disk FS
	;	       (71h, Hard Disk Partitions)
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

	; get the byte that the allocation bit 
	; for the i-number contained in R1

	; INPUT: 
	;	ax = inode number
	;	(im_buffer)
	; OUTPUT:
	;	bx = address of the byte contains allocation bit
	;	dx has 1 at calculated bit position

	; 03/01/2020
	; Modified registers: bx, cx, dx, (si, di)
	
	; 02/01/2020
	push	ax ; *

	; 18/09/2019
	cmp	ax,word ptr [systm.sb_InodeCount]
	jna	short @f
	; Temporary (set inode number to the last inode)
	mov	ax,word ptr [systm.sb_InodeCount]
@@:
	; 02/01/2020
	dec	ax ; zero based inode number

	push	ax ; **

	; 11/11/2012 (maknod_imap location -> imap procedure)
      	;sub	ax,41  ; AX has i-41
	; 02/09/2019 - Retro UNIX 386 v2
	;dec	ax ; zero based inode number

	; 02/01/2020
	shr	ax,1
	shr	ax,1
	shr	ax,1 ; AX has byte offset of zero based inode number
	     ; from the start of the (inode) map

	mov	bl,ah ; <= 31
	shr	bl,1 ; inode map sector (index/offset) number (<= 15)	
	xor	bh,bh

	and	ax,511 ; now AX points to byte offset in im_buffer

	; 02/01/2020
	cmp	bx,word ptr [im_sector]
	je	short imap_3 ; current inode map sector is same sector

	push	bx ; *** new inode map sector
	push	ax ; **** byte offset in inode map sector buffer
	push	bx ; ***** new inode map sector

	mov	bx,offset im_buffer	

	test	byte ptr [smod],2 ; inode map modified 
	jz	short imap_2 ; read inode map sector

	mov	ax,word ptr [systm.sb_InodeMapAddr]
	mov	dx,word ptr [systm.sb_InodeMapAddr+2]
	add	ax,word ptr [im_sector]
	adc	dx,0
	call	dskwr
	jnc	short imap_1
	pop	bx ; *****
	pop	dx ; ****
	pop	bx ; ***
imap_0:
	pop	ax ; **
	pop	ax ; *
	retn
imap_1:
	and	byte ptr [smod],0FDh ; reset bit 1
imap_2:
	; 02/01/2020
	pop	ax ; *****
	sub	dx,dx
	add	ax,word ptr [systm.sb_InodeMapAddr]
	adc	dx,word ptr [systm.sb_InodeMapAddr+2]
	call	dskrd
	pop	ax ; **** byte offset
	pop	bx ; *** sector
	jc	short imap_0
	mov	word ptr [im_sector],bx ; ***
imap_3:
	; ax = byte offset in inode map sector buffer
	; bx = inode map sector (index/offset)
	
	pop	cx ; ** ; inode number - 1 ; R3
	   
	mov	dx,1
	and	cl,7    ; CX has (i-41) mod 8 to get the bit position 	 	
	jz	short imap_4 ; 21/8/2012
        ;shl	dx,cl   ; DX has 1 in the calculated bit position
	; 22/01/2020
	shl	dl,cl
imap_4:
	;; 5/8/2012	
	;add	bx,word ptr [systm] ; superblock free map size + 4
	;; 21/8/2012
	;add	bx,offset systm+4 ; is inode map offset in superblock

	; 02/01/2020 - Retro UNIX 386 v2
	mov	bx,offset im_buffer
	add	bx,ax ; ****

	; CX (R3) used internally 	
	; DX (MQ) has a 1 in the calculated bit position
	; BX (R2) has byte address of the byte with allocation bit
imap_5:
	pop	ax ; *
	retn

imap	endp

; UNIX v7, x86 - iget.c (Robert Nordier, 1999)

itrunc proc near
	; 17/01/2020
	; 11/01/2020 - Retro UNIX 386 v2 hard disk FS
	; 26/12/2019
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

	; 26/12/2019
	mov	bx,ax

	; 30/09/2019
	call	iget ; jsr r0,iget
	jc	short itrunc_9 ; 30/09/2019

	test	byte ptr [inode_flgs+1],80h ; regular file or directory	
	jnz	short itrunc_1
itrunc_0:
	mov	ax,0FFFFh
itrunc_9:
	retn
itrunc_1:
	test	byte ptr [inode_flgs+1],10h ; large file (indirect blocks)
	jnz	short itrunc_4
	mov	si,offset inode_dskp
itrunc_2:
	lodsw
	;and	ax,ax
	;jz	short itrunc_3
	; 11/01/2020
	mov	dx,ax
	lodsw	
	and	ax,ax
	jnz	short @f
	or	dx,dx
	jz	short itrunc_3
@@:
	xchg	ax,dx ; 17/01/2020	
	call	free
	; 30/09/2019
	xor	ax,ax
	xor	dx,dx ; 11/01/2020
	mov	word ptr [si-2],ax ; 0
	; 11/01/2020
	mov	word ptr [si-4],dx ; 0
	; 11/01/2020
	;inc	si
	;inc	si
	cmp	si,offset inode_dskp+40
	jb	short itrunc_2
itrunc_3:
	;xor	ax,ax
	;xor	dx,dx
	mov	word ptr [inode_size],ax ; 0
	; 11/01/2020
	mov	word ptr [inode_size+2],dx ; 0
	
	; 30/09/2019
	; Clear inode block addresses
	;mov	cx,20
	;mov	di,offset inode_dskp
	;rep	stosw

	; 30/09/2019
	; clear large file flag
	and	byte ptr [inode_flgs+1],0EFh ; 11101111b ;  not 10h
	
	;call	setimod
	;retn
	jmp	setimod

itrunc_4:
	; free disk blocks by using triple indirect blocks at first
	;mov	ax,word ptr [inode_dskp+36]
	; 30/09/2019
	mov	si,offset inode_dskp+36
	mov	ax,word ptr [si]
	; 11/01/2020
	mov	dx,word ptr [si+2]
	;
	;and	ax,ax
	;jz	short itrunc_5 ; pass to double indirect blocks

	; 11/01/2020
	or	ax,ax
	jnz	short @f	
	and	dx,dx
	jz	short itrunc_5
@@:	
	mov	bx,2 ; Triple indirect sign
	push	bx
	push	dx ; 11/01/2020
	push	ax
	call	tloop
	jc	short itrunc_0
	; 30/09/2019
	mov	word ptr [si],0
	; 11/01/2020
	mov	word ptr [si+2],0 
itrunc_5:
	; free disk blocks by using double indirect blocks at second
	;mov	ax,word ptr [inode_dskp+32]
	; 11/01/2020
	;mov	dx,word ptr [inode_dskp+34]
	; 30/09/2019
	mov	si,offset inode_dskp+32
	mov	ax,word ptr [si]	; 11/01/2020
	mov	dx,word ptr [si+2]
	; 17/01/2020
	;and	ax,ax
	;jz	short itrunc_6 ; pass to single indirect blocks
	; 11/01/2020
	or	ax,ax 
	jnz	short @f
	and	dx,dx
	jz	short itrunc_6
@@:	
	mov	bx,1 ; Double indirect sign ; 17/01/2020
	push	bx
	push	dx ; 11/01/2020
	push	ax
	call	tloop
	jc	short itrunc_0
	; 30/09/2019
	mov	word ptr [si],0
	; 11/01/2020
	mov	word ptr [si+2],0 
itrunc_6:
	; free disk blocks by using single indirect blocks at third
	;mov	si,offset inode_dskp+28
	; 30/09/2019
	; si = offset inode_dskp+32
	;mov	si,offset inode_dskp+32
itrunc_7:
	sub	si,4
	mov	ax,word ptr [si]
	; 11/01/2020
	mov	dx,word ptr [si+2]
	and	ax,ax
	;jz	short itrunc_8 ; skip
	jnz	short @f
	or	dx,dx
	jz	short itrunc_8
@@:
	sub	bx,bx ; 0  ; Single indirect sign
	push	bx
	push	dx ; 11/01/2020
	push	ax
	call	tloop
	;jc	short itrunc_0
	; 11/01/2020
	jnc	short @f
	jmp	itrunc_0
@@:
	; 30/09/2019
	mov	word ptr [si],0
	; 11/01/2020
	mov	word ptr [si+2],0
itrunc_8:
	;sub	si,4	
	cmp	si,offset inode_dskp
	;jnb	short itrunc_7
	ja	short itrunc_7
	;xor	ax,ax
	; 11/01/2020
	sub	ax,ax ; 0
	sub	dx,dx ; 0
	jmp	itrunc_3
	
itrunc endp

tloop proc near
	; 23/03/2022
	; 17/01/2020
	; 11/01/2020 - Retro UNIX 386 v2 hard disk FS
	; 24/12/2019
	; 30/09/2019
	; 18/09/2019 - Retro UNIX v2
	; * Free all the disk blocks associated
	; * with the specified inode structure.
	;
	; (Simplified for Retro UNIX v2 bootable floppy disk...)

	; INPUT: 
	;	 sp+2 = ax, indirect block number (sector address)
	;	 sp+4 = dx, high word of indirect block number 
	;	 sp+6 = bx, levels 
	;	 (0 = indirect, 1 = double indr. 2 = triple indr.) 	
	
	; OUTPUT: indirect blocks will be released

	pop	cx ; return address

	pop	ax ; +++ indirect block number, lw 
	pop	dx ; ++ indirect block number, hw ; 11/01/2020	
	pop	bx ; + levels  ; 0 = single, 1 = double, 2 = triple 

	push	cx ; near call	return address

	push	di ; 30/09/2019
	push	si ; 30/09/2019

	; 11/01/2020
	push	dx ; **
	push	ax ; *

	;call	bread
	push	bx ; 30/09/2019
	mov	bx,offset WriteBuffer ; 24/12/2019
	call	dskrd
	pop	bx ; 30/09/2019
	jc	short tloop_7
	; 11/01/2020
	; dx:ax = block/sector number/address
	mov	si,offset WriteBuffer ; 24/09/2019
	cmp	bx,1
	jb	short tloop_4
	je	short tloop_1
	mov	di,offset trpi_buffer
	jmp	short tloop_2
tloop_1:
	mov	di,offset dbli_buffer
tloop_2:	
 	mov	cx,256
	; 23/03/2022
	;push	di
	rep	movsw
	mov	cl,128
	;pop	si
	dec	bx ; 2 -> 1, 1 -> 0
	; 17/01/2020
tloop_3:
	sub	di,4
	;push	di
	;push	si
	;push	bx
	;push	cx	

	mov	ax,word ptr [di]
	mov	dx,word ptr [di+2] ; 11/01/2020

	; 17/01/2020
	or	ax,ax
	;jz	short tloop_10
	jnz	short @f
	and	dx,dx
	jz	short tloop_10
@@:
	; 17/01/2020
	push	bx
	push	cx

	push	bx ;+
	push	dx ;++ ; 11/01/2020
	push	ax ;+++
	call	tloop
	pop	cx
	pop	bx
	;pop	si
	;pop	di
	jc	short tloop_7
tloop_10:
	loop	tloop_3

	; 17/01/2020
	jmp	short tloop_9

	; 17/01/2020
tloop_7:
	; 11/01/2020
	pop	ax ; *
	pop	dx ; **
tloop_8:
	pop	si ; 30/09/2019
	pop	di ; 30/09/2019
	retn
	
	; free blocks in current indirect block
tloop_4:
	mov	cx,128
	add	si,508 ; 30/09/2019
tloop_5:
	mov	ax,word ptr [si]
	; 11/01/2020
	mov	dx,word ptr [si+2]  
	or	ax,ax
	;jz	short tloop_6
	jnz	short @f
	and	dx,dx
	jz	short tloop_6
@@:
	push	cx ; 30/09/2019
	call	free
	pop	cx ; 30/09/2019
tloop_6:
	sub	si,4
	loop	tloop_5

	; 17/01/2020
tloop_9:
	; 11/01/2020
	pop	ax ; * ; free indirect block's itself
	pop	dx ; **
	call	free
	jmp	short tloop_8 ; 11/01/2020

;tloop_7:
;	; 11/01/2020
;	pop	ax ; *
;	pop	dx ; **
;tloop_8:
;	pop	si ; 30/09/2019
;	pop	di ; 30/09/2019
;	retn

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
	; 	 -> cf=0 -> word ptr [arg] > 0 -> mode (string, 2 chars)
	;	    word ptr [arg] = 0 -> ignored (none is done)	
        
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

	; 29/12/2019
	; bx = inode number

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
	; 	 -> cf=0 -> 
	; BX > 0 -> offset arg == group (decimal string, 3 chars)
	;	    BX = 0 -> ignored (none is done)

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
	; 	 -> cf=0 -> 
	; BX > 0 -> offset arg == owner (decimal string, 3 chars)
	;	    BX = 0 -> ignored (none is done)	

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

	; 29/12/2019
	; bx = inode number

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
	or	bl, bl
	jnz	short chowner_retn

	mov	bh, '0'
	mov	byte ptr [arg], bh 

chowner_retn:	
	retn
		
chowner endp

print_file_size proc near
	; 15/01/2020
	mov	ax, word ptr [inode_size]
	mov	dx, word ptr [inode_size+2]
	;mov	cx, 11
	mov	cl, 11
	jmp	short pdn0

print_file_size endp

print_decimal_number proc near
	; 15/01/2020
	; 10/01/2020
	; 29/12/2019
	; 03/02/2013
	; 21/01/2013 
	; print decimal number
	;
	; INPUT -> AX = Integer
	; 32/02/2013 CX = Number of decimal digits
	; OUTPUT -> decimal number as string

	xor	dx, dx  ; High word of binary number is zero
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

	; 15/01/2020
	mov	si, offset dec_num
	;
	mov	bx, si
	dec	si ; 29/12/2019 
	add	si, cx ; 03/02/2013
	mov	di, si
	;mov	cx, 11
	mov	cl, 11
	mov	ch, '0'
@@: 
	mov	byte ptr [bx], ch
	inc	bx
	dec	cl
	jnz	short @b
	
	;xor	ch, ch
	;mov	byte ptr [bx], ch

	; 15/01/2020
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
	
	; 15/01/2020
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

pdn_14: ; *
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

pddn_putc:
	; 29/12/2019
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

print_big_decimal_number proc near
	; 29/12/2019
	; print big decimal number
	;
	; INPUT -> DX:AX = Integer
	; 	   CX = Number of decimal digits
	; OUTPUT -> decimal number as string
pddn0:
	mov	si, offset dec_num
	;
	mov	bx, si
	dec	si ; 29/12/2019 
	add	si, cx ; 03/02/2013
	mov	di, si
	;mov	cx, 10
	mov	cl, 10
	mov	ch, '0'
@@: 
	mov	byte ptr [bx], ch
	inc	bx
	dec	cl
	jnz	short @b
	
	;xor	ch, ch
	;mov	byte ptr [bx], ch

	mov	cx, 10
pddn_itoa:
	call	div32
	add	byte ptr [si], bl
	and	bl, bl
	jnz	short @f
	and	ax, ax
	jnz	short @f
	and	dx, dx
	jz	short pddn_14 ; *
@@:	
	dec	si
	jmp	short pddn_itoa

pddn_14: ; *
	mov	si, offset dec_num
@@:	; leading zeros will not be printed
        mov	al, byte ptr [si]
	cmp	al, '0'
	;ja	short @f
	ja	short pddn_putc
	cmp	si, di
	;jnb	short @f
	jnb	short pddn_putc
	mov	al, 20h
	mov	byte ptr [si], al 
	inc	si
	jmp	short @b
;@@:
;pddn_putc:
;	mov	ah, 0Eh
;	mov	bx, 07h
;@@:
;	lodsb
;
;	int	10h
;
;	cmp	si, di
;	jna	short @b
;
;	;mov	al, 20h
;	;int	10h
;	
;	retn 

print_big_decimal_number endp

print_volume_info proc near
	; 29/12/2019
	; 24/09/2019 - Retro UNIX 386 v2
	;	       (modified unix v7 inode format)
	; 16/02/2013

	;mov	bx, offset BSBuffer
	;add	bx, bsVolumeSerial+2
	; 29/12/2019
	mov	bx, offset BSBuffer.bsVolumeSerial+2

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
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_VolumeSize] ; total sectors
	; 29/12/2019
	mov	dx, word ptr [systm.sb_VolumeSize+2] ; total sectors, hw
	
	;mov	cl, 4 ; mov	cx, 4
	;call	print_decimal_number

	mov	cl, 10 ; cx = 10  (<=4294967295)
	call	print_big_decimal_number

	mov	si, offset msgVolume_Size
	call	PRINT_MSG

	mov	si, offset msgVol_freeblocks_Hdr
	call	PRINT_MSG

	mov	ax, word ptr [systm.sb_FreeBlocks] ; free sectors 
	; 29/12/2019
	mov	dx, word ptr [systm.sb_FreeBlocks+2] ; free sectors, hw

	;mov	cx, 4	
	;call	print_decimal_number

	mov	cl, 10 ; cx = 10  (<=4294967295)
	call	print_big_decimal_number

	mov	si, offset msgVolume_freeblocks
	call	PRINT_MSG
@@:
	mov	si, offset msgVol_icount_Hdr
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_InodeCount] ; number of inodes
	;mov	cl, 4 ; mov	cx, 4
	mov	cl, 5 ; 10/01/2020
	call	print_decimal_number
	mov	si, offset msgVolume_icount
	call	PRINT_MSG

	mov	si, offset msgVol_free_icount_Hdr
	call	PRINT_MSG
	mov	ax, word ptr [systm.sb_FreeInodes] ; num of free inodes
	;;mov	cx, 4	
	;mov	cl, 4
	mov	cl, 5 ; 10/01/2020
	call	print_decimal_number
	mov	si, offset msgVolume_free_icount
	; 16/01/2020
	;call	PRINT_MSG
	;retn
	jmp	PRINT_MSG	
	
print_volume_info endp

proc_hex_double proc near
	; 24/12/2019 (phexdbl)	
	; 16/02/2013 (AX:DX)
	; 28/01/2002 (DX:AX)
	; From binary (word) to hexadecimal (character) converter
	;
	; input -> AX = word (binary number) to be converted
	; output -> AX = First 2 characters of hexadecimal number
	; output -> DX = Last 2 characters of hexadecimal number

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
        jna	short phexdbl_1
        add	dl,7
phexdbl_1:
        cmp	dh,'9'
        jna	short phexdbl_2
        add	dh,7
phexdbl_2:
        or	ax, '00'

        cmp	al,'9'
        jna	short phexdbl_3
        add	al,7
phexdbl_3:
        cmp	ah,'9'
        jna	short phexdbl_4
        add	ah,7
phexdbl_4:
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
	;	   AX = 0 -> use inode number input
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
	;mov	ax, dx
	;mov	word ptr [arg], ax
	; ax = i-number
	mov	bx, dx
	mov	word ptr [arg], bx
	call	iget
	jc	short show_inode_retn
show_inode_7:
	;mov	ax, word ptr [ii]
	; 26/12/2019
	mov	ax, bx 
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
	loop	@b

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
	test 	byte ptr [inode_flgs+1],80h
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

; 01/01/2020

align 2
	db 0
;PhysicalDriveNumber: db 0 ; 01/01/2020
FileHandle: dw 0

; 24/12/2019
file_error_msg:
	db 0Dh, 0Ah
	db 'File Error !'
CRLF:
	db 0Dh, 0Ah, 0

; 04/12/2015 (Retro UNIX 8086 v1 -> Retro UNIX 386 v1)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

UNIX_Welcome:	; 17/12/2019
	db 0Dh,0Ah
	db 'Retro UNIX 386 v2 Hard Disk FS File Import/Export Utility'
	db 0Dh,0Ah
	db 'UNIXHDCP/UNIXCOPY by Erdogan TAN 2012 - [20/07/2022]'
	db 0Dh,0Ah,0
type_27h: ; 25/09/2019
	db '(Type ', 27h,'?',27h,' to see valid commands)'
	db 0Dh,0Ah
	db 0Dh,0Ah,0
usage:
	db 0Dh,0Ah
	db 'Usage: unixhdcp [Drive] ', 0Dh,0Ah
	db '       unixhdcp -i [hard disk image file name] '
	db 0Dh,0Ah
	db 0Dh,0Ah
	db "Drive names:"
	db 0Dh,0Ah
	db 0Dh,0Ah
	db "hd0    (Hard Disk 1)", 0Dh,0Ah
	db "hd1    (Hard Disk 2)", 0Dh,0Ah
	db "...", 0Dh, 0Ah
	db "C:     (Hard Disk 1)", 0Dh,0Ah
	db "D:     (Hard Disk 2)", 0Dh,0Ah
	db 0Dh,0Ah
	db 0Dh,0Ah
	db 'Example 1: unixhdcp C: ', 0Dh,0Ah
	db 'Example 2: unixhdcp -i hd0.img '
	db 0Dh,0Ah
	db 0
unix_cdrv:
UNIX_HD_Name:	; 17/12/2019
	db 'hd'
UNIX_HD_Number:
	db '0:', 0
unix_img_cdir:	db '!'  ; 07/07/2015
unix_cdir:	db '/'
		db 37 dup(0)

CDirOffset:	dw 0

unix_prompt_char: db '>'

CursorColumn:	dw 0

CommandBuffer:	db 74 dup(0)

program_exit:	db 0

pdir: 	dw 0 

msg_arg: db 0Dh, 0Ah ; 13/01/2012 (chmod)
arg: 	dw 0
	dw 0 ; 13/01/2012  ; 01/10/2019

msg_yes_no:
	db '(Yes/No)? ', 0

msg_drive_not_ready: ; 24/12/2019
msg_unix_drv_read_error:
	db 0Dh, 0Ah
	db "Drive not ready or read error!"
	db 0Dh, 0Ah, 0

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

msg_file_not_found: ; 07/07/2015
	db 0Dh, 0Ah
	db "File not found !", 0Dh, 0Ah
	db "(File must be in current directory) !"
	db 0Dh, 0Ah, 0

	; 17/12/2019
msg_inv_image_file: ; 03/10/2019
	db 0Dh, 0Ah
	db "Invalid hard disk image file !", 0Dh, 0Ah
	db "(File size does not fit to possible CHS parameters) !"
	db 0Dh, 0Ah, 0

msg_inv_partition: ; 03/10/2019
	db 0Dh, 0Ah
	db "Retro UNIX (71h) disk partition not found !"
	db 0Dh, 0Ah, 0

msg_not_runix_fs:
	db 0Dh, 0Ah
	db "Drive has not got a (valid) Retro UNIX v2 FS !"
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

Msg_3dot_OK:    db "..."
Msg_OK:
	db ' OK.', 0Dh, 0Ah, 0

msg_YES:        db ' YES'
	db 0
msg_NO:         db ' NO'
	db 0 
; 24/12/2019
; 12/08/2012
msg_disk_rw_error:
	db 0Dh, 0Ah
	db 'Disk r/w error!'
	db 0

error_msg:
	db 0Dh, 0Ah
	db 'Error !'
UNIX_CRLF:
	db 0Dh, 0Ah, 0

msg_error_Number:
	db 0Dh, 0Ah
	db 'Error No: '
str_err:        dw 3030h
	db 'h'
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

RetryCount:     dw 0

; 07/07/2015
img_file_name:  db 13 dup(0)
	        db 0
img_file_handle: dw 0
;img_file_pos:	dd 0 ; file (position) pointer
;
DirFileName:
	db 20h ; 06/01/2013
BOOT_FILE_NAME: db 14 dup(0) ; 04/12/2015 (9 -> 14+'?')
	db '?' ; Here will be ZERO if name length is 14
	       ; (ci_move_bfn_3:)

uf_make_datetime: dd 0 ; 25/11/2012

uf_i_number: dw 0 ; 25/11/2012

align 4 ; 24/12/2019

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

;rw: db 0

; 20/09/2019
; NOTE: word ptr [imod] is used to check inode modification in iget 
; 24/12/2019
;imod: db 0
;imodx: db  0 ; 19/09/2019 - Retro UNIX 386 v2 
;		(extended flag for inode modification)	

;U:
u_uid: db 0
u_gid: db 0 ; 19/09/2019 - Retro UNIX 386 v2
u_cdir: dw ROOT_DIR_INODE_NUMBER
u_namep: dw 0
u_dirp: dw 0
	dw 0 ; 01/01/2020 (32 bit value for directory size)
; 24/12/2019	
;u_base: dw 0
;u_off: dw 0
;u_count: dw 0
;u_nread: dw 0
u_dirbuf: db 16 dup(0) ; 04/12/2015 (10 -> 16)

;ii: dw 0
;buff_s: dw 0 ; 24/12/2019

year: dw 1970
month: dw 1
day: dw 1
hour: dw 0
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

; 30/11/2012
;imin: dd 0
;ihrs: dd 0
;iday: dw 0
;lday: dw 0
;qday: dw 0
;iyrs: dw 0
;jday: dw 0
;mday: dw 0


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
Str_cday:	db '00'
	db '/'
Str_cmonth:	db '00'
	db '/'
Str_cyear:	db '0000'
	db 20h, 20h
Str_chour: 	db '00'
	db ':'
Str_cminute:  	db '00'
	db ':'
Str_csecond:  	db '00'
	db 0Dh, 0Ah
	db 'Last Modif. Date & Time : '
Str_mday:	db '00'
	db '/'
Str_mmonth:	db '00'
	db '/'
Str_myear:	db '0000'
	db 20h, 20h
Str_mhour: 	db '00'
	db ':'
Str_mminute:  	db '00'
	db ':'
Str_msecond:  	db '00'
	db 0Dh, 0Ah
	db 'Mode Change Date & Time : '
Str_aday:	db '00'
	db '/'
Str_amonth:	db '00'
	db '/'
Str_ayear:	db '0000'
	db 20h, 20h
Str_ahour: 	db '00'
	db ':'
Str_aminute:  	db '00'
	db ':'
Str_asecond:  	db '00'
	db 0Dh, 0Ah, 0

;23/02/2013
list_count: db 0FFh
; 20/01/2013
ls_option: db 0
; 21/01/2013
;dec_num: db 10 dup(20h) ; 03/02/2013, 3 bytes -> 10 bytes
; 15/01/2020
dec_num: db 11 dup(20h)
dw 0 ; 29/12/2019

;30/12/2012
DotDot:
db '.'
Dot:
db '.'
db 0

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
msgVol_Size_Hdr:db "Volume Size : ", 0
msgVolume_Size:	; db "0000" 
	db " blocks", 0Dh, 0Ah, 0
msgVol_freeblocks_Hdr:db "Free Count  : ", 0
msgVolume_freeblocks: ;db "0000" 
	db " blocks", 0Dh, 0Ah, 0	 
msgVol_icount_Hdr:
	db "# of Inodes : ", 0
msgVolume_icount:  ;db "0000" 
	db 0Dh, 0Ah, 0
msgVol_free_icount_Hdr:db 'Free Inodes : ', 0
msgVolume_free_icount : ;db "0000" 
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
txt_major: db "00h", 0Dh,0Ah
	db "Minor Number : "
txt_minor: db "00h", 0Dh,0Ah, 0

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

; 17/12/2019

align 2

; 01/12/2019
	db 0
;PhysicalDriveNumber: ; 01/01/2020
Drive:	; 03/12/2019
buff_d: db 0
buff_s: dw 0FFFFh ; Buffer sector
	dw 0FFFFh ; 19/10/2019 (32 bit sector address)
buff_m:	db 0 ; buffer changed/modified (dirty) flag
buff_w: db 0 ; read/write flag (write=1, read=0)
;buff_c: dw 0 ; count ; 05/09/2019

; 31/10/2012
u_off:	dw 0
	dw 0 ; 08/12/2019  	
; 12/08/2012
u_count: dw 0
u_base:	 dw 0
;u_fofp: dw 0
u_nread: dw 0
	;dw 0 ; 01/01/2020

; 01/09/2019
;BLOCKDEV equ 61FFh ; 0110000111111111b
;CHARDEV equ 21FFh  ; 0010000111111111b
; 02/09/2019
;REGULARDEF equ 81FFh ; 1000000111111111b

; 15/09/2019	
; 01/09/2019 - Retro UNIX v2 inode model (64 bytes)
;	       (Modified UNIX v7 inode model)

; 08/12/2019
;bitpos: db 0

Error: db 0 ; Hardware error
       db 0 ; Software error

; 05/01/2020
bmod: db 0	  

smod: db 0
imod: db 0
imodx: db 0 ; 24/12/2019

rw:   db 0 ; 24/12/2019

      db 0 ; 19/01/2020	 

ii: dw 0

; 07/11/2019
;drive: db 0 ; 03/12/2019
;rw: db 0

dotodot:
dw 3030h
db "h"
db 0Dh, 0Ah, 0

; 04/12/2019
align 4
level: dd 0  ; level, level+1, level+2, level+3 

; 30/11/2019
I_rw: db 0     ; inode table read (0) or write (1)	
I_valid: db 0  ; inode table buffer is valid (1) or not (0)
I_sector: dw 0 ; inode table sector index/offset

align 4

; 24/12/2019
;inode:
;inode_flgs: dw 0 ; file flag
;i_nlks: dw 0 ; Number of links
;i_uid:  dw 0 ; owner's user id 
;i_gid:	db 0 ; owner's group id
;inode_size_h: db 0 ; high byte of 5 bytes file size
;inode_size: dd 0 ; file size
;inode_dskp: dd 10 dup(0)  ; direct or indirect blocks
;i_ltim: dd 0 ; last access time (or last inode modif. time)
;i_mtim: dd 0 ; last (file) modification time 
;i_ctim: dd 0 ; (file) creation time

; 17/12/2019

save_ax:	dw 0FFFFh
save_dx:	dw 0FFFFh
; 19/01/2020
save_cx:	dw 0
save_bx:	dw 0

; 01/01/2020
d_off:	dw 0
	dw 0

;data_start:	dd 0FFFFFFFFh
count:	dd 0	
fm_sector:	dd 0
im_sector:	dw 0

; 03/10/2019 - Retro UNIX 386 v2 - Hard Disk FS

	dw 0 ; reserved (for alignment)

heads:	dw 0
sectors:	dw 0 ; spt
cylinders:	dw 0
hidden_sectors:	dd 0
total_sectors:	dd 0
file_size:	dd 0
pp_sectors:	dd 0
min_sectors:	dw 0
lba_rw:	db 0
	db 0 ; reserved (for alignment)

CHS_limit:	dd 0

align 16

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  buffers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

boot_sector: ; 17/12/2019
BSBUFFER:	db 512 dup(0)
; superblock
super_block: ; 17/12/2019
systm: 	db 512 dup(0)
	     ; 17/12/2019
Buffer:
sector_buffer:	db 512 dup(0) ; Masterboot sector & common buffer
	
	     ; 17/12/2019	
I_buffer:	db 512 dup(0) ; Inode table sector buffer

ReadBuffer:     db 512 dup(0)
WriteBuffer:    db 512 dup(0)

; 24/09/2019
im_buffer:	db 512 dup(0) ; Inode map sector buffer
fbm_buffer:	db 512 dup(0) ; Free blocks map sector buffer
dbli_buffer:	db 512 dup(0)
trpi_buffer:	db 512 dup(0)

SizeOfFile      equ $-100

UNIXCOPY  ends

          end  START_CODE