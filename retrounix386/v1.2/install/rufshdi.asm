; RUFSHDI.ASM
; --------------------------------------------------------------
; RETRO UNIX v0.3 - Modified UNIX v7 inode model - 01/09/2019
;
; RETRO UNIX v0.2 - 14 byte file name modifications (04/12/2015)
; RETRO UNIX v0.1 'fd0' formatting procedures
;
; Derived from UINSTALL.ASM (for 1.44MB floppies) - 30/09/2019
;
; Last Update: 20/07/2022 (Retro UNIX 386 v2) ;*****************
;
; 04/12/2015 (new /dev directory format 
; 	      according to Retro UNIX 8086 v1 kernel)
; 21/04/2014 (tty8, tty9)
; 05/03/2013 (ALIGN) 
; 31/10/2012, 16/12/2012 (unixproc.asm -> sioreg) 
; ERDOGAN TAN [ 14-15-16-21-27/7/2012, 4-5-12-13-14-15-21/8/2012 ]
; These procedures will be located in UNIXFDFS.ASM file 
; when they are completed.
; (NOTE: only for (R)UFS initialization of FD0 1.44MB floppy disk

;SIZE_FREE_MAP equ 360
;SIZE_INODE_MAP equ 32

DISK_SIZE equ 2880 ; in blocks
INODE_COUNT equ SIZE_INODE_MAP * 8
;INODE_LIST_BLOCKS equ (INODE_COUNT / 16)
INODE_LIST_BLOCKS equ (INODE_COUNT / 8) ; 01/09/2019

;ROOT_DIR_INODE equ 41
;ROOT_DIR_INODE equ 1; 07/09/2019 - Retro UNIX 386 v2

;SIZE_Reserved1 equ 512 - (2+SIZE_FREE_MAP+2+SIZE_INODE_MAP) 

;SuperBlock struc
;
;sb_FreeMapSize dw ?
;sb_FreeMap	db SIZE_FREE_MAP dup(?)
;sb_InodeMapSize dw ?
;sb_InodeMap	db SIZE_INODE_MAP dup(?)
;sb_Reserved1	db SIZE_Reserved1 dup(?)
;sb_Reserved2	db 512 dup(?)
;
;SuperBlock ends

; 14/01/2020 - Super Block modification :
;	     - Extended sections/divisions (consequental sectors)
;	     - (for swapping, configuration, boot space etc.)	
; 21/12/2019
; 19/12/2019 (UNIXHDFS.COM, RUFSHDI.ASM)
; 01/09/2019 - Retro UNIX 386 v2 SuperBlock

;SuperBlock struc
;
;sb_Header	dd ?
;sb_BootSectAddr dd ?  ; Hidden Sectors
;sb_VolumeSize	dd ?  ; Entire Volume/Partition Size (includes ext. volume)	
;sb_Version	dd ?
;sb_BlockSize	dd ?
;sb_InodeCount	dd ? 	
;sb_FreeMapAddr	dd ?
;sb_FreeMapSize  dd ?
;sb_InodeMapAddr dd ?
;sb_InodeMapSize dd ?
;sb_InodeTblAddr dd ?
;sb_InodeTblSize dd ?
;sb_FreeInodes	dd ?
;sb_FirstFreeIno dd ?
;sb_FreeBlocks	dd ?
;sb_FirstFreeBlk dd ?
;sb_BootSecParms db 19 dup(?) ; v1 ; 19/12/2019
;sb_BSExtension	db 5 dup(?) ; v2 HDFS ; 19/12/2019
;sb_Status	dd ? ; 19/12/2019
;sb_ModifTime	dd ?
;sb_ExtdVolTbl	dd 0 ; 14/01/2020 ; Extended Volume Start/Table Address
;sb_ExtdVolSize	dd 0 ; 14/01/2020 ; Extended Volume (swap section etc.) Size	
;sb_LBA_rw	db 0 ; 03/10/2019
;sb_ClusterSize	db 0 ; 03/10/2019
;sb_ReadOnly	db 0 ; 03/10/2019
;sb_Mounted	db 0 ; 03/10/2019
;sb_MountInode	dd 0 ; 03/10/2019
;sb_DevMajor	db 0 ; 03/10/2019
;sb_DevMinor	db 0 ; 03/10/2019
;sb_LongName	db 0 ; 03/10/2019
;sb_Direntry32	db 0 ; 03/10/2019
;sb_Reserved	db 508-116 dup(?) ; Must be 0 for current RUFS version
;sb_Footer	dd ?
;
;SuperBlock ends

; 14/01/2020
;sb_HiddenSects equ sb_BootSecAddr
;sb_TotalSects equ sb_VolumeSize

; 19/12/2019 - Retro UNIX 386 v2 HD (071h) partition boot sector
; 04/12/2019
; 03/10/2019 - Retro UNIX 386 v2 
; Hard Disk Partition (71h) Boot Sector Parameters

;RUFS struc
;bsJumpCode     dw ? ; 0EB, 13h ; jmp short @f  	
;bsFSystemID    db 4 dup(?)  ;'RUFS'
;bsVolumeSerial dd ?
;bsDriveID      dw ? ; 'hd'
;bsDriveNumber  db ? ; 80h
;bsReserved     db ? ; 0 = 512 bytes/sector CHS, 1 = LBA
;bsSecPerTrack  db ? ; 63,17
;bsHeads        db ? ; 8 to 255 (may be 2 to 255) 
;bsCylinders    dw ? ; 1 to 1024 (bsTracks)
;bs_BF_I_number dw 0 ; startup (boot) file inode number
;bsMagic	db 0 ; '@' ; magic byte !
;bsPartitionID: db 71h ; Retro UNIX 386 partition/volume ; 19/12/2019
;bsHiddenSects: dd 0 ; Hidden sectors (Boot Sector LBA)	; 19/12/2019
;;@@:
;RUFS ends

RUFS_BSP_SIZE equ 19 ; 21-2

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
; Flag bit 12 - 0 small file addressing
; -------------------------------------
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

; Flag bit 12 - 1 large file addresdsing
; -------------------------------------
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
; Use extents (contigous blocks)
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
;  .di_size:  resd 1	; /* number of bytes in file */
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
;  .di_size:  resd 1	; /* number of bytes in file */
;  .di_size_h: resw 1	; /* number of bytes in file */
;  .di_cssc: resb 1	; Cluster/Block size shift count - 1 ; 04/09/2019	
;  .di_bssc: resb 1	; Block/Sector size shift count - 1 ; 04/09/2019		
;  .di_addr:  resd 8 ; resb 32 ; /* disk block addresses */
;  .di_atime: resd 1	; /* time last accessed */
;  .di_mtime: resd 1	; /* time last modified */
;  .di_ctime: resd 1	; /* time created */
;  .di_reserved2: resd 1 ; reserved (zero)
;endstruc

; 18/12/2019
; 15/09/2019 (second draft)
; Retro UNIX 386 v2 I-node (on disk) : 
;
;struc dinode
;  .di_mode:  resw 1	; /* mode and type of file */
;  .di_nlink: resw 1	; /* number of links to file */
;  .di_uid:   resw 1	; /* owner's user id */  - 0 to 655535 -
;  .di_gid:   resb 1	; /* owner's group id */ - o to 255 -
;  .di_size_h: resb 1	; /* number of bytes in file */ ; - byte 5 -
;  .di_size:  resd 1	; /* number of bytes in file */
;  .di_addr:  resd 10 ; resb 40 ; /* disk block addresses */
;  .di_atime: resd 1	; /* time last accessed */
;  .di_mtime: resd 1	; /* time last modified */
;  .di_ctime: resd 1	; /* time created */
;endstruc

; 04/09/2019 - Retro UNIX 386 v2

;BLOCK SIZE SHIFT COUNTS or STORAGE/DISK (SECTOR SIZE) TYPE
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
 
;DISK ADDR AND FILE SIZE LIMITS FOR EXTENTS !!! LIMITS !!!
;----------------------------------------------------------
; (depending on free contiguous blocks/sectors on disk fs)
; direct - 5 extents - 2 tera bytes (avr. 400GB per extent)  
; indirect - 4*64 extents - 2TB (average 8GB per extent)
; double ind. - 4*64*64 extents - 2TB (avr. 128MB per ext.)
; triple ind. - 1048576 extents - 2TB (avr. 2MB per extent)

; EXTENT SIZING FOR 8GB FILE !!! SAMPLE !!!
;----------------------------------------------------------
; direct - 4 extents - 8GB (average 2GB per extent)  
; indirect - 256 extents - 8GB (average 32MB per extent)
; double ind. - 16384 extents - 8GB (avr. 512KB per extent)
; triple ind. - 1048576 extents - 8GB (avr. 8KB per extent)

unix_fs_install proc near
	; 20/07/2022
	; 05/04/2022
	; 02/01/2020
	; 30/12/2019
	; 19/12/2019 - LBA/CHS type RUNIX HD partition boot sectors
	; 12/12/2019
	; 10/12/2019
	; 05/12/2019
	; 04/12/2019
	; 01/12/2019
	; 03/10/2019 - Retro UNIX 386 v2 (Hard Disk FS, 71h) 
	; 04/09/2019
	; 01/09/2019 - Retro UNIX 386 v2
	; 8086 code by Erdogan Tan
	; 31/10/2012
	; 21/08/2012
	; 15/08/2012
	; 14/08/2012
	; 13/08/2012
	; 05/08/2012
	; 04/08/2012
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	; RETRO UNIX v1 FS
	; initialization/format version
	; NOTE: 
	; The "cold" unix (u0, PDP-11) code is modified for fd0 
	;  -> 1.44 MB floppy disk (Retro UNIX v1, 8086) fs

	; 19/12/2019
	; If [lba_rw]>0, change boot sector to LBA read type boot sector
	cmp	byte ptr [lba_rw], 0
	jna	short set_boot_sector_parms

	mov	si, offset lba_boot_sector
	mov	di, offset boot_sector
	mov	cx, 256
	rep	movsw

set_boot_sector_parms:
	; 20/07/2022
	; 19/12/2019
	; 03/10/2019
	; Set Retro UNIX volume/partition Boot Sector Parameters

	;mov	word ptr [boot_sector.bsFSystemID], 'UR'
	;mov	word ptr [boot_sector.bsFSystemID+2], 'SF'
	;mov	word ptr [boot_sector.bsDriveID], 'dh'
	;mov	byte ptr [boot_sector.bsDriveNumber], 80h
	
	; 19/12/2019 (heads and SPT values are single byte values)
	mov	al, byte ptr [heads]
	mov	byte ptr [boot_sector.bsHeads], al
	mov	al, byte ptr [sectors]
	mov	byte ptr [boot_sector.bsSecPerTrack], al
	
	mov	ax, word ptr [cylinders]
	mov	word ptr [boot_sector.bsCylinders], ax

	;xor 	ax, ax
	;mov	word ptr [boot_sector.bs_BF_I_number], ax
	;mov	word ptr [boot_sector.bsVolumeSerial], ax	
	;mov	word ptr [boot_sector.bsVolumeSerial+2], ax
	;mov	byte ptr [boot_sector.bsMagic], '@'

	; 29/12/2019
	mov	ax, word ptr [VolumeSerial]
	mov	dx, word ptr [VolumeSerial+2]
	mov	word ptr [boot_sector.bsVolumeSerial], ax	
	mov	word ptr [boot_sector.bsVolumeSerial+2], dx	

	mov	al, byte ptr [lba_rw]
	mov	byte ptr [boot_sector.bsReserved], al ; 0 or 1
	; 20/07/2022
	mov	byte ptr [systm.sb_LBA_rw], al ; 0 or 1

	; 19/12/2019
	mov	ax, word ptr [hidden_sectors]
	mov	dx, word ptr [hidden_sectors+2]
	mov	word ptr [boot_sector.bsHiddenSects], ax
	mov	word ptr [boot_sector.bsHiddenSects+2], dx	

	mov	si, offset boot_sector.bsFSystemID
	mov	di, offset systm.sb_BootSecParms
	;mov	cx, 19
	;rep	movsb
	; 19/12/2019
	mov	cx, 12 
	rep	movsw

	; 01/09/2019
	mov word ptr [systm.sb_Header], 0171h
	mov byte ptr [systm.sb_Header+2], 0A1h

	; 10/10/2019
	;mov ax,word ptr [hidden_sectors]
	;mov dx,word ptr [hidden_sectors+2]
	mov word ptr [systm.sb_BootSectAddr], ax
	mov word ptr [systm.sb_BootSectAddr+2], dx
	; 14/01/2020
	; 04/12/2019
	;mov word ptr [systm.sb_HiddenSects], ax
	;mov word ptr [systm.sb_HiddenSects+2], dx
	; 10/10/2019
	mov ax,word ptr [total_sectors]
	mov dx,word ptr [total_sectors+2]
	mov word ptr [systm.sb_VolumeSize], ax
	mov word ptr [systm.sb_VolumeSize+2], dx
	; 14/01/2020
	;mov word ptr [systm.sb_TotalSects], ax
	;mov word ptr [systm.sb_TotalSects+2], dx

	mov byte ptr [systm.sb_Version], 1
	;mov dword ptr [systm.sb_BlockSize], 0

	; 10/10/2019 - Retro UNIX 386 v2 - Default inode count calculation
	; inode count =
	;	256 up to 2MB file system
	;	512 up to 4MB file system
	;      1024 up to 8MB file system
	;      2048 up to 16MB file system
	;      4096 up to 32MB file system
	;      8192 up to 64MB file system
	;     16384 up to 128MB file system
	;     32768 up to 256MB file system
	;     65534 for 512MB to 2TB file system
	;
	; Formula:
	;	if dx >= 16, inode count = 65534
	;       if dx > 0, inode count 4096*dx
	;	if dx = 0, inode count ax/16
	;	(minimum file system size is 512KB) 				 		   

	or dx,dx
	jnz short large_fs
	mov cx,ax
	shr cx,1
	shr cx,1
	shr cx,1
	shr cx,1
	jmp short big_fs
large_fs:
	mov cx,65534	; 0FFFFh is reserved for long file names 
	;		; (or another extension)
	cmp dx,16
	jnb short big_fs
	mov ch,dl
	shl ch,1
	shl ch,1
	shl ch,1
	shl ch,1
	     ; cx <= 61440	
	xor cl,cl	
big_fs:	
	mov word ptr [systm.sb_InodeCount], cx
	mov byte ptr [systm.sb_InodeMapAddr], 2
	mov bx, cx
	shr cx,1
	shr cx,1
	shr cx,1
	and bx,7
	jz short ufsi_1
	inc cx
ufsi_1:	
	mov word ptr [systm.sb_InodeMapSize],cx ; SIZE_INODE_MAP ; bytes
	;add cx,255 ; 05/12/2019
	add cx,511 ; 05/04/2022 (sector count round up)
	mov cl,ch
	xor ch,ch
	shr cl,1
	add cl,2 ; inode map address + inode map sectors
	;mov word ptr [systm.sb_FreeMapAddr],cx
	mov byte ptr [systm.sb_FreeMapAddr],cl ; 10/12/2019
	add ax,7 ; 05/12/2019
	adc dx,0
	;mov cl,8
	;call div32
	; 16/10/2019
	mov cl,3 ; /8
	call shr32
	mov word ptr [systm.sb_FreemapSize],ax ; SIZE_FREE_MAP lw ; bytes
	mov word ptr [systm.sb_FreemapSize+2],dx ; SIZE_FREE_MAP hw ; bytes

	; 05/12/2019
	add ax,511
	adc dx,0
	
	; 16/10/2019
	; mov cx,9
	mov cl,9
	call shr32	

	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]		
	mov word ptr [systm.sb_InodeTblAddr],ax
	mov word ptr [systm.sb_InodeTblAddr+2],dx

	mov cx,word ptr [systm.sb_InodeCount]

	; 05/12/2019
	add cx,7 ; round up

	shr cx,1
	shr cx,1
	shr cx,1 ; 8 inodes per sector
	mov word ptr [systm.sb_InodeTblSize],cx ; sectors

	add ax,cx
	adc dx,0
	mov word ptr [data_start],ax
	mov word ptr [data_start+2],dx

	; 05/12/2019
	mov ax,0FFFFh
				
	;mov word ptr [systm.sb_FreeInodes],0
	;mov word ptr [systm.sb_FreeInodes+2],0
	
	;mov word ptr [systm.sb_FirstFreeIno],ax ; 0FFFFh
	;mov word ptr [systm.sb_FirstFreeIno+2],ax ; 0FFFFh

	mov word ptr [systm.sb_FreeBlocks],ax ; 0FFFFh
	mov word ptr [systm.sb_FreeBlocks+2],ax ; 0FFFFh
	
	;mov word ptr [systm.sb_FirstFreeBlk],ax ; 0FFFFh
	;mov word ptr [systm.sb_FirstFreeBlk+2],ax ;0FFFFh

	;mov byte ptr [systm.sb_Status],0
	
	;mov word ptr [systm.sb_ModifTime],ax ; 0FFFFh
	;mov word ptr [systm.sb_ModifTime+2],ax ; 0FFFFh

	; 08/09/2019 (UNIXFDFS) - 16/10/2019 (UNIXHDFS)
	mov word ptr [systm.sb_Footer],0A100h
	mov word ptr [systm.sb_Footer+2],7101h

	; 05/04/2022
	;mov ax,word ptr [systm.sb_FreeMapAddr]
	;mov dx,word ptr [systm.sb_FreeMapAddr+2]
	
	mov bx,offset fbm_buffer  ; fbm buffer is clear here

; 05/04/2022
;
;	; 10/12/2019
;	; (Clear free map sectors on disk)
;ufsi_15:
;	or byte ptr [smod],1 ; free map modified flag
;	call dskwr
;	;jc short ufsi_8 ; error, exit
;	jc short ufsi_18 ; 10/12/2019
;	and byte ptr [smod],0FEh ; reset free map modified flag
;
;	cmp dx,word ptr [systm.sb_InodeTblAddr+2]
;	jb short ufsi_16
;	cmp ax,word ptr [systm.sb_InodeTblAddr]
;	jnb short ufsi_17	
;ufsi_16:
;	add ax,1
;	adc dx,0
;	add word ptr [fm_sector],1
;	adc word ptr [fm_sector+2],0
;	jmp short ufsi_15
;ufsi_18:
;	; 10/12/2019
;	retn

	; 05/04/2022
	; (clear inode map, free blocks map and inode table sectors)

	mov ax,2 ; inode map address
	xor dx,dx
ufsi_15:
	;or byte ptr [smod],3 
			; set inode map & free map modified flags
	call dskwr
	jc short ufsi_18
	;and byte ptr [smod],0FCh 
			; clear inode map, free map modified flags		
	
	inc ax	; add ax,1
	jnz short ufsi_16
	cmp dx,word ptr [data_start+2]
	je short ufsi_17
	inc dx	; adc dx,0		
ufsi_16:
	cmp ax,word ptr [data_start]
	jb short ufsi_15
	;if ax >= [data_start], dx should be < [data_start+2]
	cmp dx,word ptr [data_start+2]
	jb short ufsi_15	

	; clearing finished...
ufsi_17:
	; 16/10/2019
	mov ax,word ptr [total_sectors]
	mov dx,word ptr [total_sectors+2]

	; 05/04/2022
	jmp short @f
ufsi_18:
	retn
@@:
	; 05/04/2022
	; here..
	; dword ptr [fm_sector] = 0
	; byte ptr [smod] = 0
	; fm_buffer is clear (full zero)
ufsi_2:
;set bit DX:AX in free storage map in core/memory
	sub ax,1
	sbb dx,0

	call free
	;jc short ufsi_8 ; error, exit ; 07/11/2019
	jc short ufsi_18 ; 30/12/2019

	; dx:ax = current sector has been freed/released

	cmp dx,word ptr [data_start+2]
        ja short ufsi_2
	;jb short @f ; 05/12/2019 	

	cmp ax,word ptr [data_start]
        ja short ufsi_2 ; 05/12/2019
@@:
	; 04/12/2019
	;mov word ptr [buff_c], 1 ; count
ufsi_3:
; zero i-list	
	; set superblock (m) and inode (a/m/c) times

	call epoch
	mov word ptr [systm.sb_ModifTime],ax
	mov word ptr [systm.sb_ModifTime+2],dx
	
	or byte ptr [smod],0F0h ; super block modified flag 

	mov di,offset inodes+52 ; 15/09/2019	
	mov cx,26
ufsi_4:
	stosw ; mov [di],ax   ; access (inode modification) time
	xchg ax,dx
	stosw ; mov [di+2],dx
	xchg ax,dx
	stosw ; mov [di+4],ax ; (file) modification time
	xchg ax,dx
	stosw ; mov [di+6],dx
	xchg ax,dx
	stosw ; mov [di+8],ax ; (file & inode) creation time
	xchg ax,dx
	stosw ; mov [di+10],dx		
	xchg ax,dx	
	add di,64-12 ; next inode
	loop ufsi_4

	; 19/10/2019
	mov ax,word ptr [systm.sb_InodeTblAddr]
	mov dx,word ptr [systm.sb_InodeTblAddr+2]

	; write default/initial inodes - Retro UNIX 386 v2
	mov bx,offset inodes ; 07/11/2019
	; 04/12/2019
	mov byte ptr [buff_c],32/8 ; 4 sectors for 32 inodes
ufsi_5:	
	; 01/12/2019
	mov word ptr [buff_s],ax ; Start sector of inode table
	mov word ptr [buff_s+2],dx

	mov byte ptr [buff_m],1 ; modified
	call dskwr
	jc short ufsi_8 ; rw_error
	mov byte ptr [buff_m],0 ; reset ; 07/11/2019
	; 04/12/2019
	;dec word ptr [buff_c]
	dec byte ptr [buff_c]
	jz short ufsi_6
	; 07/11/2019
	;mov ax,word ptr [buff_s]
	;mov dx,word ptr [buff_s+2]
	add ax,1	; next sector
	adc dx,0
	;;mov bx,word ptr [buff_o]
	;add bx,512	; next 8 inodes
	add bh,2 ; 04/12/2019
	jmp short ufsi_5 ; loop
ufsi_6:
	; 23/10/2019
	mov ax,27  ; First free inode
	mov word ptr [systm.sb_FirstFreeIno],ax
	;mov word ptr [systm.sb_FirstFreeIno+2],0
	mov bx,word ptr [systm.sb_InodeCount]

; 05/04/2022
;	; 04/12/2019
;	mov dx,bx
;	mov cl,12 ; inodes/4096
;	shr dx,cl ; 4096 inode allocation bits per sector
;
;	test bx,4095 ; +1 inode map sector ?
;	jz short ufsi_12
;	inc dx
;ufsi_12:
;	; 05/04/2022 
;	;mov word ptr [im_scount],dx ; inode map sector count

	sub bx,ax
	inc bx
	mov word ptr [systm.sb_FreeInodes],bx	

	; 01/12/2019
	;mov bx,1  ; start from root directory inode number
	; 07/11/2019
	mov dx,1

	;; 05/04/2022
	;; clear inode map buffer (im_buffer = fbm_buffer)
	;xor ax,ax
	;mov di,offset im_buffer ; fbm_buffer
	;mov cx,256
	;rep stosw

	; 05/04/2022
	; here..
	; dword ptr [im_sector] = 0
	; byte ptr [smod] = 0F0h
	; im_buffer is clear (full zero)

ufsi_7:
	call imap
	jc short ufsi_8 ; 02/01/2020
	
	;xchg bx,dx
	or byte ptr [bx],al ; set the bit to indicate the i-node
			    ; is not available/free

	or byte ptr [smod],2 ; 02/01/2020

	;xchg bx,dx
	;cmp bx,26
	cmp dx,26
	jnb short ufsi_9
	;inc bx
	inc dx
	jmp short ufsi_7
	; 30/12/2019
ufsi_8: 
	; 06/09/2019
	retn
ufsi_9:
; 05/04/2022
;	; 04/12/2019
;	;mov ax,word ptr [systm.sb_InodeMapAddr]
;	;mov dx,word ptr [systm.sb_InodeMapAddr+2]
;	mov ax,2
;	xor dx,dx ; 0
;	;add ax,word ptr [im_sector] ; Inode map sector (index)
;	;adc dx,0
;
;	; 04/12/2019
;	mov bx,offset im_buffer
;	;or byte ptr [smod],2 ; inode map modified
;	or byte ptr [smod],al ; 2 ; 12/12/2019
;	call dskwr
;	jc short ufsi_8 ; rw_error
;
;	and byte ptr [smod],0FDh ; not 2 ; reset
;
;	cmp word ptr [im_scount],1 ; inode map sector count
;	jna short ufsi_14
;
;	; clear inode map buffer
;	xor ax,ax
;	mov di,offset im_buffer
;	mov cx,256
;	rep stosw
;
;	;mov ax,word ptr [im_sector]
;	inc ax
;	;mov ax,1
;ufsi_13:
;	mov word ptr [im_sector],ax
;
;	;mov dx,word ptr [systm.sb_InodeMapAddr+2]
;	;add ax,word ptr [systm.sb_InodeMapAddr]
;	;adc dx,0
;	
;	;xor dx,dx ; 0
;	add ax,2 ; inode map start address (offset from boot sector)
;	;adc dx,0
;
;	;mov bx,offset im_buffer
;	or byte ptr [smod],2 ; inode map modified
;	call dskwr
;	jc short ufsi_8 ; rw_error
;
;	and byte ptr [smod],0FDh ; not 2 ; reset
;
;	mov ax,word ptr [im_sector]
;	inc ax
;	cmp ax,word ptr [im_scount] ; inode map sector count
;	jb short ufsi_13
ufsi_14:
	; 14/12/2019
	call sync

	; 02/09/2019 - Retro UNIX 386 v2
	; make & write directories
	mov di,offset dirs
	mov bx,1 ; start from inode number of root directory
	;mov si,offset inodes  ; root_inode
	;add si,8 ; si points to directory size in root inode
	; 09/09/2019
	mov si,offset i_dir_sizes
ufsi_10:
	;mov ax,[si]
	lodsw ; 09/09/2019
	mov word ptr [u_count],ax
	
        mov word ptr [u_base],di
	add di,ax

	mov word ptr [u_fofp],offset u_off ; 31/10/2012
        mov word ptr [u_off],0
        ;mov word ptr [u_off+2],0

	call writei
	jc short ufsi_8 ; rw_error ; 30/12/2019 (short jump)

	; 14/12/2019
	;push bx
	;call sync
	;pop bx

	cmp bx,7 ; inode number of mnt directory
	jnb short ufsi_11

	;add si,64 ; now, si points to dir size word of next inode

	inc bx
	jmp short ufsi_10

ufsi_11:	
	; 19/10/2019
	;jmp short sync

unix_fs_install endp

sync 	proc near
	; 05/04/2022
	; 02/01/2020
	; 01/12/2019
	; 05/11/2019
	; 21/10/2019 - Retro UNIX 386 v2 - Hard Disk File System
	; 05/09/2019
	; 02/09/2019 - Retro UNIX 386 v2
	; 12/8/2012
	; updates super block and the last i-node on disk 
	; if modified
	; e.g. smod = 1, imod = 1, buffer_m = 1
	;
	; RETRO UNIX v1 FS
	; initialization/format version

	xor bx, bx ; mov bx,0
	call iget ; (write modified i-node)
	;jc short sync_18 ; 02/01/2020
	jnc short @f
	jmp sync_18
@@:
	; 02/01/2020
	cmp byte ptr [buff_m],0
	jna short sync_0

	mov ax,word ptr [buff_s]
	mov dx,word ptr [buff_s+2]
	mov bx,offset Buffer
	call dskwr
	;jc short sync_18
	jnc short @f
	jmp sync_18
@@:
	mov byte ptr [buff_m],0
sync_0:
	cmp byte ptr [smod],0
	;jna sync_17
	; 02/01/2020
	ja short @f
	jmp sync_17	
@@:
	; 02/01/2020
	; set super block modification time
	call epoch
	mov word ptr [systm.sb_ModifTime],ax
	mov word ptr [systm.sb_ModifTime+2],dx

	test byte ptr [smod],2 ; inode map modified 
	jz short sync_7

	xor dx,dx
	mov ax,word ptr [im_sector]
	add ax,word ptr [systm.sb_InodeMapAddr]
	adc dx,word ptr [systm.sb_InodeMapAddr+2]
	mov bx,offset im_buffer

	call dskwr
	;jc short sync_18
	; 02/01/2020
	jnc short @f
	jmp sync_18	
@@:
	and byte ptr [smod],0FDh ; reset bit 1

	; 02/01/2020
	; set free inodes number
	; if it is invalid (in sb)

	cmp word ptr [systm.sb_FreeInodes],0FFFFh ; invalid
	jb short sync_7

	mov cx,word ptr [systm.sb_InodeCount] ; <= 65534
	mov word ptr [count],cx
	mov ax,word ptr [systm.sb_InodeMapAddr]
	mov dx,word ptr [systm.sb_InodeMapAddr+2]
	xor bp,bp ; 0
	mov word ptr [im_sector],bp ; 0
sync_1:
	mov bx,offset im_buffer
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx
	call dskrd
	jc short sync_7
sync_2:
	mov cx,8
	mov al,byte ptr [bx] ; 02/01/2020
sync_3:
	shr al,1 ; 02/01/2020
	jc short sync_4 ; allocated inode
	inc bp		; free inode
sync_4:
	dec word ptr [count]
	jz short sync_6
	loop sync_3
	cmp bx,offset im_buffer+511
	jnb short sync_5
	inc bx
	jmp short sync_2
sync_18:
	mov ax,word ptr [Error]
	retn
sync_5: 
	inc word ptr [im_sector]
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	add ax,1
	adc dx,0
	jmp short sync_1
sync_6:
	mov word ptr [systm.sb_FreeInodes],bp
	; ***
	jmp short sync_19 ; 02/01/2020
sync_7:
	; 02/01/2020
	; set first free inode number
	; if it is invalid (in sb)

	mov dx,0FFFFh
	cmp word ptr [systm.sb_FirstFreeIno],dx ; 0FFFFh
	jne short @f
	cmp word ptr [systm.sb_FirstFreeIno+2],dx ; 0FFFFh		
	jne short @f
sync_19:
	call set_firstfreeinode
@@:
	test byte ptr [smod],1 ; free map modified
	;jz sync_15
	; 05/04/2022
	jnz short @f
	jmp sync_15	
@@:	
	mov ax,word ptr [fm_sector]
	mov dx,word ptr [fm_sector+2]

	add ax,word ptr [systm.sb_FreeMapAddr]
	adc dx,word ptr [systm.sb_FreeMapAddr+2]

	mov bx,offset fbm_buffer 	
	call dskwr
	jc short sync_18

	and byte ptr [smod],0FEh ; reset bit 0

	; 02/01/2020
	; set free blocks number
	; if it is invalid (in sb)

	mov ax,word ptr [systm.sb_FreeBlocks] 	
	and ax,word ptr [systm.sb_FreeBlocks+2]
	cmp ax,0FFFFh 
	;jb sync_15
	; 02/01/2020
	jnb short @f
	jmp sync_15
@@:
	mov ax,word ptr [systm.sb_VolumeSize]
	mov dx,word ptr [systm.sb_VolumeSize+2]	
	mov word ptr [count],ax
	mov word ptr [count+2],dx

	mov ax,word ptr [systm.sb_FreeMapAddr]
	mov dx,word ptr [systm.sb_FreeMapAddr+2]
	xor si,si
	xor di,di
	mov word ptr [fm_sector],si ; 0
	mov word ptr [fm_sector+2],di
sync_8:
	mov bx,offset fbm_buffer
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx
	call dskrd
	jc short sync_15
sync_9:
	mov cx,8
	mov al,byte ptr [bx] ; 02/01/2020
sync_10:	 	
	shr al,1	; 02/01/2020
	jnc short sync_11 ; allocated block/sector
	add si,1	  ; free block/sector
	jnc short sync_11
	;adc di,0
	inc di
sync_11:
	sub word ptr [count],1
	ja short sync_12
	jz short sync_20 ; 02/01/2020

	; 02/01/2020
	sbb word ptr [count+2],0
	jmp short sync_12
sync_20:
	; 02/01/2020
	cmp word ptr [count+2],0
	jna short sync_14
sync_12:
	loop sync_10
	cmp bx,offset fbm_buffer+511
	jnb short sync_13
	inc bx
	jmp short sync_9
sync_13: 
	add word ptr [fm_sector],1
	adc word ptr [fm_sector+2],0	
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	add ax,1
	adc dx,0
	jmp short sync_8
sync_14:
	mov word ptr [systm.sb_FreeBlocks],si
	mov word ptr [systm.sb_FreeBlocks+2],di
	; ***
	jmp short sync_16 ; 02/01/2020
sync_15:	
	; 02/01/2020
	; set first free block number
	; set free blocks number
	; if it is invalid (in sb)

	mov dx,0FFFFh
	cmp word ptr [systm.sb_FirstFreeBlk], dx ; 0FFFFh
	jne short @f
	cmp word ptr [systm.sb_FirstFreeBlk+2], dx ; 0FFFFh		
	jne short @f
sync_16:
	call set_firstfreeblock
@@:		
 	xor dx,dx
	mov ax,1 ; Super block address (in 71h/RUNIX partition)
	mov bx,offset systm 
	call dskwr
	;jc sync_18
	jnc short @f
	jmp sync_18
@@:
	mov byte ptr [smod],0 ; reset
sync_17:
	xor ax,ax
	retn

sync	endp

set_firstfreeinode proc near
	; 02/01/2020 Retro UNIX 386 v2
	; 	     (Hard Disk FS, 71h partition)
	; set first free inode number in sb
	; Output:
	;   cf = 0 -> calculated
	;   cf = 1 = can not be calculated 

	xor bp,bp
	mov word ptr [im_sector],bp ; 0

	mov cx,word ptr [systm.sb_InodeCount] ; <= 65534
	mov word ptr [count],cx
	mov ax,word ptr [systm.sb_InodeMapAddr]
	mov dx,word ptr [systm.sb_InodeMapAddr+2]
sffi_1:
	mov bx,offset im_buffer
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx
	call dskrd
	jc short sffi_4
sffi_2:
	mov cx,8
	mov al,byte ptr [bx]
sffi_3:
	inc bp
	shr al,1
	jc short sffi_5 ; allocated inode
	mov word ptr [systm.sb_FirstFreeIno],bp	; first free inode
sffi_4:
	retn
sffi_5:
	dec word ptr [count]
	jz short sffi_4
	loop sffi_3
	cmp bx,offset im_buffer+511
	jnb short sffi_6
	inc bx
	jmp short sffi_2
sffi_6: 
	inc word ptr [im_sector]
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	add ax,1
	adc dx,0
	jmp short sffi_1

set_firstfreeinode endp

set_firstfreeblock proc near
	; 02/01/2020 Retro UNIX 386 v2
	; 	     (Hard Disk FS, 71h partition)
	; set first free block number in sb
	; Output:
	;   cf = 0 -> calculated
	;   cf = 1 = can not be calculated 

	mov ax,word ptr [systm.sb_VolumeSize]
	mov dx,word ptr [systm.sb_VolumeSize+2]	
	mov word ptr [count],ax
	mov word ptr [count+2],dx

	mov ax,word ptr [systm.sb_FreeMapAddr]
	mov dx,word ptr [systm.sb_FreeMapAddr+2]
	xor si,si
	xor di,di
	mov word ptr [fm_sector],si ; 0
	mov word ptr [fm_sector+2],di

	dec si ; 0FFFFh
	dec di ; 0FFFFh
sffb_1:
	mov bx,offset fbm_buffer
	mov word ptr [save_ax],ax
	mov word ptr [save_dx],dx
	call dskrd
	jc short sffb_5
sffb_2:
	mov cx,8
	mov al,byte ptr [bx]
sffb_3:	 	
	add si,1
	jnc short sffb_4
	adc di,0
sffb_4:	
	shr al,1
	jnc short sffb_6 ; allocated block/sector
	; free block/sector
	mov word ptr [systm.sb_FirstFreeBlk],si
	mov word ptr [systm.sb_FirstFreeBlk+2],di	
sffb_5:
	retn
sffb_6:
	sub word ptr [count],1
	ja short sffb_8
	jz short sffb_7

	sbb word ptr [count+2],0
	jmp short sffb_8
sffb_7:	
	cmp word ptr [count+2],0
	ja short sffb_8
	stc
	retn
sffb_8:
	loop sffb_3
	cmp bx,offset fbm_buffer+511
	jnb short sffb_9
	inc bx
	jmp short sffb_2
sffb_9: 
	add word ptr [fm_sector],1
	adc word ptr [fm_sector+2],0	
	mov ax,word ptr [save_ax]
	mov dx,word ptr [save_dx]
	add ax,1
	adc dx,0
	jmp short sffb_1

set_firstfreeblock endp

align 2
; 01/12/2019
	db 0
drive:	; 03/12/2019
buff_d: db 0
buff_s: dw 0FFFFh ; Buffer sector
	dw 0FFFFh ; 19/10/2019 (32 bit sector address)
buff_m:	db 0 ; buffer changed/modified (dirty) flag
buff_w: db 0 ; read/write flag (write=1, read=0)
buff_c: dw 0 ; count ; 05/09/2019

align 16

systm: ; superblock
	db 512 dup(0)

; 01/09/2019 - Retro UNIX 386 v2
dirs:
root_dir: ; root directory
		dw 1
		db "."
		db 13 dup(0)
		dw 1
		db ".."
		db 12 dup(0)
		dw 2
		db "dev"
		db 11 dup(0)
		dw 3
		db "bin"
		db 11 dup(0)
		dw 4
		db "etc"
		db 11 dup(0)
		dw 5
		db "usr"
		db 11 dup(0)
		dw 6
		db "tmp"
		db 11 dup(0)
		dw 7
		db "mnt"
		db 11 dup(0)

; 04/12/2015 (14 byte file name modifications)
; 5/8/2012
; 14/7/2012
;dirs:
;root_dir: ; root directory
;		dw 41
;		db ".."
;		db 12 dup(0)
;		dw 41
;		db "."
;		db 13 dup(0)
;		dw 42
;		db "dev"
;		db 11 dup(0)
;		dw 43
;		db "bin"
;		db 11 dup(0)
;		dw 44
;		db "etc"
;		db 11 dup(0)
;		dw 45
;		db "usr"
;		db 11 dup(0)
;		dw 46
;		db "tmp"
;		db 11 dup(0)

size_root_dir equ $ - offset root_dir

dev_dir: ; device directory
		dw 2
		db "."
		db 13 dup(0)		
		dw 1
		db ".."
		db 12 dup(0)		
		dw 8
		db "tty"
		db 11 dup(0)		
		dw 9
		db "mem"
		db 11 dup(0)		
		dw 10
		db "fd0"
		db 11 dup(0)		
		dw 11
		db "fd1"
		db 11 dup(0)		
		dw 12
		db "hd0"
		db 11 dup(0)		
		dw 13
		db "hd1"
		db 11 dup(0)		
		dw 14
		db "hd2"
		db 11 dup(0)		
		dw 15
		db "hd3"
		db 11 dup(0)		
		dw 16
		db "lpr"
		db 11 dup(0)		
		dw 17
                db "tty0"
		db 10 dup(0)		
		dw 18
                db "tty1"
		db 10 dup(0)	
		dw 19
                db "tty2"
		db 10 dup(0)	
		dw 20
                db "tty3"
		db 10 dup(0)		
		dw 21
                db "tty4"
		db 10 dup(0)		
		dw 22
                db "tty5"
 		db 10 dup(0)		
		dw 23
                db "tty6"
		db 10 dup(0)		
		dw 24
                db "tty7"
		db 10 dup(0)		
		dw 25
                db "COM1" ; 09/07/2013
		db 10 dup(0)		
		dw 26
                db "COM2" ; 09/07/2013
		db 10 dup(0)		
		dw 25
		db "tty8" ; 21/04/2014
		db 10 dup(0)		
		dw 26
                db "tty9" ; 21/04/2014  
		db 10 dup(0)

;dev_dir: ; device directory
;		dw 41
;		db ".."
;		db 12 dup(0)		
;		dw 42
;		db "."
;		db 13 dup(0)		
;		dw 1
;		db "tty"
;		db 11 dup(0)		
;		dw 2
;		db "mem"
;		db 11 dup(0)		
;		dw 3
;		db "fd0"
;		db 11 dup(0)		
;		dw 4
;		db "fd1"
;		db 11 dup(0)		
;		dw 5
;		db "hd0"
;		db 11 dup(0)		
;		dw 6
;		db "hd1"
;		db 11 dup(0)		
;		dw 7
;		db "hd2"
;		db 11 dup(0)		
;		dw 8
;		db "hd3"
;		db 11 dup(0)		
;		dw 9
;		db "lpr"
;		db 11 dup(0)		
;		dw 10
;               db "tty0"
;		db 10 dup(0)		
;		dw 11
;               db "tty1"
;		db 10 dup(0)	
;		dw 12
;               db "tty2"
;		db 10 dup(0)	
;		dw 13
;               db "tty3"
;		db 10 dup(0)		
;		dw 14
;               db "tty4"
;		db 10 dup(0)		
;		dw 15
;               db "tty5"
; 		db 10 dup(0)		
;		dw 16
;               db "tty6"
;		db 10 dup(0)		
;		dw 17
;               db "tty7"
;		db 10 dup(0)		
;		dw 18
;               db "COM1" ; 09/07/2013
;		db 10 dup(0)		
;		dw 19
;               db "COM2" ; 09/07/2013
;		db 10 dup(0)		
;		dw 18
;		db "tty8" ; 21/04/2014
;		db 10 dup(0)		
;		dw 19
;               db "tty9" ; 21/04/2014  
;		db 10 dup(0)

size_dev_dir equ $ - offset dev_dir

bin_dir:  ; binary directory
		dw 3
		db "."
		db 13 dup(0)
		dw 1
		db ".."
		db 12 dup(0)

;bin_dir:  ; binary directory
;		dw 41
;		db ".."
;		db 12 dup(0)
;		dw 43
;		db "."
;		db 13 dup(0)
		
size_bin_dir equ $ - offset bin_dir

etc_dir:  ; etcetra directory
		dw 4
		db "."
		db 13 dup(0)	
		dw 1
		db ".."
		db 12 dup(0)

;etc_dir:  ; etcetra directory
;		dw 41
;		db ".."
;		db 12 dup(0)	
;		dw 44
;		db "."
;		db 13 dup(0)
			
size_etc_dir equ $ - offset etc_dir

usr_dir:  ; user directory
		dw 5
		db "."
		db 13 dup(0)	
		dw 1
		db ".."
		db 12 dup(0)

;usr_dir:  ; user directory
;		dw 41
;		db ".."
;		db 12 dup(0)	
;		dw 45
;		db "."
;		db 13 dup(0)
		
size_usr_dir equ $ - offset usr_dir

tmp_dir:  ; temporary directory
		dw 6
		db "."
		db 13 dup(0)	
		dw 1
		db ".."
		db 12 dup(0)

;tmp_dir:  ; temporary directory
;		dw 41
;		db ".."
;		db 12 dup(0)	
;		dw 46
;		db "."
;		db 13 dup(0)
		
size_tmp_dir equ $ - offset tmp_dir

mnt_dir:  ; temporary directory
		dw 7
		db "."
		db 13 dup(0)	
		dw 1
		db ".."
		db 12 dup(0)

size_mnt_dir equ $ - offset mnt_dir

align 2

;dw 0

; 31/10/2012
u_off: dw 0
       dw 0 ; 08/12/2019  		
; 12/08/2012
u_count: dw 0
u_base: dw 0
u_fofp: dw 0
u_nread: dw 0

; 01/09/2019
BLOCKDEV equ 61FFh ; 0110000111111111b
CHARDEV equ 21FFh  ; 0010000111111111b
; 02/09/2019
REGULARDEF equ 81FFh ; 1000000111111111b

; 15/09/2019	
; 01/09/2019 - Retro UNIX v2 inode model (64 bytes)
;	       (Modified UNIX v7 inode model)

inode:
i_flgs: dw 81FFh ; default regular file flag ; 02/09/2019
i_nlks: dw 1 ; Number of links
i_uid:  dw 0 ; owner's user id 
i_gid:	db 0 ; owner's group id
i_size_h: db 0 ; high byte of 5 bytes file size
i_size: dd 0 ; file size
i_dskp: dd 10 dup(0)  ; direct or indirect blocks
i_ltim: dd 0 ; last access time (or last inode modif. time)
i_mtim: dd 0 ; last (file) modification time 
i_ctim: dd 0 ; (file) creation time

; 17/08/2012
; 05/08/2012
; 14/07/2012
;inode:
;i_flgs: dw 800Fh ; special (device) files flags
;i_nlks: db 1 ; Number of links
;i_uid: db 0  ; user id 
;i_size: dw 0 ; file size
;i_dskp: dw 8 dup(0)  ; direct or indirect blocks
;i_ctim: dd 0 ; creation time
;i_mtim: dd 0 ; last modification time 
;i_reserved: dw 0 ; reserved (not in use)

; 09/09/2019
; size table for directory inodes (1 to 7)
i_dir_sizes:
	dw size_root_dir ; 128	; 1
	dw size_dev_dir	; 368	; 2
	dw size_bin_dir	; 32	; 3
	dw size_etc_dir	; 32	; 4
	dw size_usr_dir	; 32	; 5
	dw size_tmp_dir	; 32	; 6
	dw size_mnt_dir	; 32	; 7

; 05/08/2012
; 14/07/2012
idata:
inodes:

; 13/04/2022
; 25/09/2019
; 15/09/2019
; 01/09/2019 - Retro UNIX 386 v2 (UNIX v7 modified) inodes

root_inode: ; 1
		dw 0C1FFh ; Flags (1100000111111111b)
		dw 8	; number of links ; 13/04/2022
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_root_dir ; initial size = 128 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
dev_inode: ; 2
		dw 0C1FFh ; Flags (1100000111111111b)
		dw 2	; number of links
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_dev_dir ; initial size = 128 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
bin_inode: ; 3
		dw 0C1EDh ; Flags  (1100000111101101b)
		dw 2	; number of links
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_bin_dir ; 32 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
etc_inode: ; 4
		dw 0C1EDh ; Flags  (1100000111101101b)
		dw 2	; number of links
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_etc_dir ; 32 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
usr_inode: ; 5
		dw 0C1EDh ; Flags  (1100000111101101b)
		dw 2	; number of links ; 13/04/2022
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_usr_dir ; 32 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tmp_inode: ; 6
		dw 0C1FFh ; Flags (1100000111111111b)
		dw 2	; number of links
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_tmp_dir ; 32 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
mnt_inode: ; 7
		dw 0C1FFh ; Flags (1100000111111111b)
		dw 2	; number of links
		dw 0	; user ID (0 = root)
		db 0	; group ID (0 = root)
		db 0	; size_h (5th byte of file size)
		;dd size_mnt_dir ; 32 bytes
		dd 0
		dd 10 dup (0) ; indirect or contents blocks
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty_inode: ; 8
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0200h ; major = 2, minor = 0 
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
mem_inode: ; 9
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0100h ; major = 1, minor = 0 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
fd0_inode: ; 10
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0100h ; major = 1, minor = 0 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
fd1_inode: ; 11
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0101h ; major = 1, minor = 1 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
hd0_inode: ; 12
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0000h ; major = 0, minor = 0 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
hd1_inode: ; 13
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0001h ; major = 0, minor = 1 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
hd2_inode: ; 14
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0002h ; major = 0, minor = 2 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
hd3_inode: ; 15
		dw 61FFh ; Flags (0110000111111111b) ; BLOCKDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0003h ; major = 0, minor = 3 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
lpr_inode: ; 16
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0400h ; major = 4, minor = 0 	
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time	
tty0_inode: ; 17
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0201h ; major = 2, minor = 1
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty1_inode: ; 18
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0202h ; major = 2, minor = 2
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty2_inode: ; 19
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0203h ; major = 2, minor = 3
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty3_inode: ; 20
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0204h ; major = 2, minor = 4
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty4_inode: ; 21
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0205h ; major = 2, minor = 5
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty5_inode: ; 22
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0206h ; major = 2, minor = 6
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty6_inode: ; 23
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0207h ; major = 2, minor = 7
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
tty7_inode: ; 24
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		dw 1	; number of links
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size = 0
		dd 0	; size_h = 0
		dw 0208h ; major = 2, minor = 8
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
com1_inode: ; 25
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		;dw 1	; number of links
		dw 2	; 30/09/2019
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0300h ; major = 3, minor = 0
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time
com2_inode: ; 26
		dw 21FFh ; Flags (0010000111111111b) ; CHARDEV
		;dw 1	; number of links
		dw 2	; 30/09/2019
		dw 3	; user ID (3 = bin)
		db 3	; group ID (3 = bin)
		db 0	; size_h = 0
		dd 0	; size = 0
		dw 0301h ; major = 3, minor = 1
		dw 19 dup (0) ; not used, must be 0
		dd 0	; last access date & time
		dd 0	; modification date & time
		dd 0	; creation date & time

		; 18/12/2019
		; 02/09/2019
unused_inodes: ; 27 to 32
		db (32-26)*64 dup(0)	
	
; Retro UNIX v1 (UNIX v1) inodes
;
;root_inode: ; 41
;		dw 0C00Eh ; Flags (1100000000001110b)
;		db 7	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_root_dir ; initial size = 70 bytes 
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused
;dev_inode: ; 42
;		dw 0C00Eh ; Flags (1100000000001110b)
;		db 2	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_dev_dir ; 200
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused	
;bin_inode: ; 43
;		dw 0C00Eh ; Flags (1100000000001110b)
;		db 2	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_bin_dir ; 20
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused
;etc_inode: ; 44
;		dw 0C00Eh ; Flags (1100000000001110b)
;		db 2	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_etc_dir ; 20
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused				
;usr_inode: ; 45
;		dw 0C00Eh ; Flags (1100000000001110b)
;		db 2	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_usr_dir ; 20
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused
;tmp_inode: ; 46
;		dw 0C00Fh ; Flags (1100000000001111b)
;		db 2	; number of links 
;		db 0	; user ID (0 = root)
;		dw size_tmp_dir ; 20
;		dw 8 dup (0) ; indirect or contents blocks
;		dd 0	; creation date & time
;		dd 0	; modification date & time
;		dw 0	; unused

; 04/12/2019
; 03/12/2019
align 16

data_start: dd 0FFFFFFFFh
count: dd 0	
fm_sector: dd 0
im_sector: dw 0
im_scount: dw 0 ; 04/12/2019

;align 16

; 05/04/2022
im_buffer: ; 02/09/2019 - Retro UNIX386 v2 (inode map buffer)
	db 512 dup (0) ; 07/11/2019

Buffer:
sector_buffer:
	db 512 dup (0)

; 05/04/2022
;im_buffer:
; 01/09/2019
fbm_buffer:  ; Free Blocks Map buffer
	db 512 dup (0)