; UINSTALL.ASM
; --------------------------------------------------------------
; RETRO UNIX v0.3 - Modified UNIX v7 inode model - 01/09/2019
;
; RETRO UNIX v0.2 - 14 byte file name modifications (04/12/2015)
; RETRO UNIX v0.1 'fd0' formatting procedures
;
; Last Update: 13/04/2022 (Retro UNIX 386 v2) ;*****************
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

; 14/03/2022 (first free block bugfix)
; 23/01/2020 - Retro UNIX 386 v2

SIZE_FREE_MAP equ 360
SIZE_INODE_MAP equ 32

DISK_SIZE equ 2880 ; in blocks

INODE_COUNT equ SIZE_INODE_MAP * 8
;;INODE_LIST_BLOCKS equ (INODE_COUNT / 16)
;INODE_LIST_BLOCKS equ (INODE_COUNT / 8) ; 01/09/2019
INODE_LIST_BLOCKS equ ((INODE_COUNT + 7) / 8) ; 05/04/2022

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

; 14/01/2020 - Extended sections/divisions (consequental sectors)
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

; 21/12/2019
; 19/12/2019 - Retro UNIX 386 v2 HD (071h) partition boot sector 
;		(RUFSHDI.ASM)
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
;bsPartitionID: db 0 ; 71h ; Retro UNIX 386 partition/volume ; 19/12/2019
;bsHiddenSects: dd 0 ; Hidden sectors (Boot Sector LBA)	; 19/12/2019
;;@@:
;RUFS ends

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
; 18/12/2019 - Mounted flag - IFMNT (2000h) ; RUFSHDI.ASM

; Retro UNIX 386 v2 I-node Flags: (di_mode) for files
; 1000000000000000b 	IFREG - 1 = regular file (8000h)
; 0100000000000000b	IFDIR - 1 = directory (4000h)
; 0010000000000000b	IRSVD - 0 = reserved bit (2000h)  ; Mounted flag
; 0001000000000000b	ILARG - Large file addressing bit (1000h)
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

; Retro UNIX 386 v2 I-node Flags: (di_mode) for devices
; 1000000000000000b 	IFREG - 0 = device file (8000h)
; 0100000000000000b	IFBLK - 1 = block device (4000h)
; 0010000000000000b	IFCHR - character special (2000h) -always 1-
; 0001000000000000b	IFIFO - fifo special (1000h)
; 0000100000000000b	ISUID - set user id on exec (800h)		 	
; 0000010000000000b	ISGID - set group id on exec (400h)
; 0000001000000000b	IEXTR - 1 = external device driver (200h)
; 0000000100000000b	IREAD - read, owner (100h)
; 0000000010000000b	IWRITE - write,owner (80h)
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

; 18/12/2019 ; RUFSHDI.ASM
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
	; 05/04/2022
	; 21/12/2019	
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

	; 21/12/2019
	; 19/12/2019 ; RUFSHDI.ASM
	mov	si, offset boot_sector.bsFSystemID
	mov	di, offset systm.sb_BootSecParms
	;mov	cx, 19
	;rep	movsb
	; 19/12/2019
	mov	cx, 12 
	rep	movsw

	; 06/09/2019
	;mov byte ptr [buff_d], dl ; 14/8/2012, drive number

	;mov word ptr [systm.sb_FreemapSize], SIZE_FREE_MAP ; 360
	;mov word ptr [systm.sb_InodeMapSize], SIZE_INODE_MAP ; 32

	; 01/09/2019
	mov word ptr [systm.sb_Header], 0171h
	mov byte ptr [systm.sb_Header+2], 0A1h
	;mov dword ptr [systm.sb_BootSectAddr], 0
	mov word ptr [systm.sb_VolumeSize], DISK_SIZE ; 2880
	mov byte ptr [systm.sb_Version], 1
	;mov dword ptr [systm.sb_BlockSize], 0
	mov word ptr [systm.sb_InodeCount], 256
	mov byte ptr [systm.sb_FreeMapAddr], 3	
	mov word ptr [systm.sb_FreemapSize], SIZE_FREE_MAP ; 360
	mov byte ptr [systm.sb_InodeMapAddr], 2
	mov byte ptr [systm.sb_InodeMapSize], SIZE_INODE_MAP ; 32
	mov byte ptr [systm.sb_InodeTblAddr], 4
	mov byte ptr [systm.sb_InodeTblSize], 32				
	;mov dword ptr [systm.sb_FreeInodes], 0
	;mov dword ptr [systm.sb_FirstFreeIno], 0FFFFFFFFh
	;mov dword ptr [systm.sb_FreeBlocks], 0FFFFFFFFh
	;mov dword ptr [systm.sb_FirstFreeBlock], 0FFFFFFFFh
	;mov byte ptr [systm.sb_Status], 0
	;mov dword ptr [systm.sb_ModifTime], 0FFFFFFFFh
	; 08/09/2019
	mov word ptr [systm.sb_Footer], 0A100h
	mov word ptr [systm.sb_Footer+2], 7101h

	mov ax, DISK_SIZE ; 2880 blocks/sectors
uinstall_1:
;set bit AX/R1 in free storage map in core/memory
	dec ax ; R1
	;call free

	call free_3
	or word ptr [BX], dx ; set bit for this block (available)
			     ; bis r3, (r2)	

	;cmp ax, 32+4 ; 07/09/2019
	cmp ax, INODE_LIST_BLOCKS + 4 ; 15/8/2012
        ja short uinstall_1

	; 02/09/2019
	;mov word ptr [buff_c], 1 ; flag for clear procedure
	mov byte ptr [buff_c], 1 ; 23/01/2020

; zero i-list	
	dec ax

; 05/04/2022
;	; 01/09/2019
;	push ax
;uninstall_11:
;	call free_3
;
;	not dx ; masking bit is '0' and others are '1'
;	and word ptr [BX], dx	
;	; 0 -> allocated
;
;	dec ax
;	jnz short uninstall_11
;
;	pop ax 	

	; 07/09/2019 - Retro UNIX 386 v2
uinstall_9:
	; AX (R1) = Block number

	call clear 	
        jc short uinstall_2 ; rw_error

	and ax, ax
	jz short uinstall_3

	dec ax
	jmp short uinstall_9

uinstall_2:
	retn

uinstall_3:
	; 15/09/2019
	; 02/09/2019 - Retro UNIX 386 v2
	; set superblock (m) and inode (a/m/c) times

	call epoch
	mov word ptr [systm.sb_ModifTime], ax
	mov word ptr [systm.sb_ModifTime+2], dx
	mov byte ptr [smod], 1 ; super block modified flag 

	mov di, offset inodes+52 ; 15/09/2019	
	mov cx, 26
uinstall_12:
	stosw ; mov [di], ax   ; access (inode modification) time
	xchg ax, dx
	stosw ; mov [di+2], dx
	xchg ax, dx
	stosw ; mov [di+4], ax ; (file) modification time
	xchg ax, dx
	stosw ; mov [di+6], dx
	xchg ax, dx
	stosw ; mov [di+8], ax ; (file & inode) creation time
	xchg ax, dx
	stosw ; mov [di+10], dx		
	xchg ax, dx	
	add di, 64-12 ; next inode
	loop uinstall_12

	; 02/09/2019
	; write default/initial inodes - Retro UNIX 386 v2
	mov word ptr [buff_o], offset inodes
	;mov word ptr [buff_c], 32/8 ; 4 sectors for 32 inodes
	mov byte ptr [buff_c], 32/8 ; 23/01/2020	
	mov word ptr [buff_s], 4 ; Start sector of inode table
uinstall_13:	
	mov byte ptr [buff_m], 1 ; modified
	call dskwr
	jc short uinstall_10 ; rw_error
	;dec word ptr [buff_c]	
	dec byte ptr [buff_c] ; 23/01/2020
	jz short uinstall_5
	inc word ptr [buff_s] ; next sector
	add word ptr [buff_o], 512 ; next 8 inodes
	jmp short uinstall_13 ; loop
	
	;; initialize inodes for special files (1 to 40)
	;mov bx, 40 ; BX = R1,  41 = root directory i-number
;uinstall_4:
	;call iget
        ;jc short uinstall_10 ; rw_error

	;mov word ptr [i_flgs], 800Fh ;  1000000000001111b
	;mov byte ptr [i_nlks], 1
        ;call setimod
	;dec bx
	;jnz short uinstall_4

uinstall_5:
	; 02/09/2019 - Retro UNIX 386 v2
	mov word ptr [buff_o], offset Buffer ; reset buffer offset pointer

	mov bx, 1  ; start from root directory inode number
uinstall_14:
	call imap
	
	xchg bx,dx
	or byte ptr [BX], al ; set the bit to indicate the i-node
			     ; is not available/free
	xchg bx, dx
	cmp bx, 26
	jnb short uinstall_6
	inc bx
	jmp short uinstall_14

uinstall_10: ; 06/09/2019
	retn

uinstall_6:
	; 02/09/2019 - Retro UNIX 386 v2
	; im_buffer = Buffer
	;mov word ptr [buff_o], offset im_buffer ; inode map buffer

	mov word ptr [buff_s], 2 ; Inode map sector
	mov byte ptr [buff_m], 1 ; modified
	call dskwr
	jc short uinstall_10 ; rw_error

	;mov word ptr [buff_o], offset Buffer ; reset buffer offset

	; clear buffer
	xor ax, ax
	mov di, word ptr [buff_o]
	mov cx, 256
	rep stosw

	; 02/09/2019 - Retro UNIX 386 v2
	; make & write directories

	mov di, offset dirs
	mov bx, 1 ; start from inode number of root directory
	;mov si, offset inodes  ; root_inode
	;add si, 8 ; si points to directory size in root inode
	; 09/09/2019
	mov si, offset i_dir_sizes
uinstall_7:
	;mov ax, [si]
	lodsw ; 09/09/2019
	mov word ptr [u_count], ax
	
        mov word ptr [u_base], di
	add di, ax
	
	mov word ptr [u_fofp], offset u_off ; 31/10/2012
	
        mov word ptr [u_off], 0
	
	call writei
	jc short uinstall_10 ; rw_error

	cmp bx, 7 ; inode number of mnt directory
	jnb short uinstall_8

	;add si, 64 ; now, si points to dir size word of next inode

	inc bx
	jmp short uinstall_7

	;;push di
	;;push si
	;mov si, offset idata ; base address of assembled dirs
	;mov di, offset dirs  ; directory data for assembled dirs
	;mov bx, 41
;uinstall_6:
	;call imap
	;xchg bx,dx ; 13/8/2012
	;; 21/8/2012 (AX -> AL, word ptr [BX] -> byte ptr [BX])
	;or byte ptr [BX], al ; BX/DX = R2, ax = mq
	;		     ; set the bit to indicate the i-node
	;		     ; is not available/free
	;xchg bx, dx ; 13/8/2012
	;call iget
	;;jnc short uinstall_7
        ;jc short uinstall_10  ; rw_error
;@@:
	;;pop si
	;;pop di
	;;jmp short uinstall_10 ; rw_error

;uinstall_7:
	;; SI, DI registers are not modifed 
	;; in imap, iget, setimod and writei procedures
	;lodsw
        ;mov word ptr [i_flgs], ax
	;lodsb
        ;mov byte ptr [i_nlks], al
	;lodsb
        ;mov byte ptr [i_uid], al
	;call setimod
	;lodsw
        ;mov word ptr [u_count], ax
	;
	;add si, 26 ; now, si points 1st word of next inode
	;
        ;mov word ptr [u_base], di
	;add di, ax
	;
	;mov word ptr [u_fofp], offset u_off ; 31/10/2012
	;
        ;mov word ptr [u_off], 0
	;
	;call writei
	;;jc short @b
        ;jc short uinstall_10 ; rw_error
	;
	;cmp bx, 46
	;jnb short uinstall_8
	;
	;inc bx
	;jmp short uinstall_6

uinstall_8:	
	;pop si
	;pop di

	; 06/09/2019
	mov word ptr [buff_o], offset fbm_buffer ; free blocks map buffer
	mov word ptr [buff_s], 3 ; Free blocks map sector
	mov byte ptr [buff_m], 1 ; modified
	;mov word ptr [buff_c], 1
	mov byte ptr [buff_c], 1 ; sector count = 1 
	call dskwr
	jc short uinstall_10 ; rw_error

;uinstall_9:
	;call sync ; write modified super block and buffer to disk
	;;jc short rw_error

;uinstall_10:
	;retn

unix_fs_install endp

sync 	proc near
	; 14/03/2022
	; 05/09/2019
	; 02/09/2019 - Retro UNIX 386 v2
	; 12/8/2012
	; updates super block and the last i-node on disk 
	; if modified
	; e.g. smod = 1, imod = 1, buffer_m = 1
	;
	; RETRO UNIX v1 FS
	; initialization/format version

	; 02/09/2019 - Retro UNIX 386 v2
	mov word ptr [buff_o], offset Buffer ; reset buffer offset	

	xor bx, bx ; mov bx, 0
	call iget
	jc short sync_2

	xor ax, ax
	cmp byte ptr [smod], al ; 0
	jna short sync_3

	; 05/09/2019
calc_free_blocks:
	mov byte ptr [systm.sb_FreeInodes], 256-26
	mov byte ptr [systm.sb_FirstFreeIno], 27
	mov si, offset fbm_buffer
	mov cx, SIZE_FREE_MAP ; 360
	xor dx, dx
	xor bx, bx
sync_4:
	push cx
	lodsb
	;mov cl, 8
	mov cx, 8 ; 14/03/2022 (BugFix)
sync_5:
	shr al, 1
	jnc short sync_7
	; 07/09/2019
	and dx, dx
	jnz short sync_6
	mov word ptr [systm.sb_FirstFreeBlk], bx
sync_6:
	inc dx
sync_7:
	inc bx
	loop sync_5
	pop cx
	loop sync_4
sync_8:
	mov word ptr [systm.sb_FreeBlocks], dx

	; 02/09/2019
	call epoch
	;mov dword ptr [systm.sb_ModifTime], 0FFFFFFFFh
	; 04/09/2019
	mov word ptr [systm.sb_ModifTime], ax
	mov word ptr [systm.sb_ModifTime+2], dx
sync_1:
	sub ax, ax ; 07/09/2019 
	mov byte ptr [smod], al ; 0

	;mov cx, 256
	;mov si, offset Systm
	;mov di, offset Buffer
	;rep movsw

	; 02/09/2019 - Retro UNIX 386 v2
	mov word ptr [buff_o], offset Systm	

	inc al

        mov word ptr [buff_s], ax ; 1 ; superblock sector number
	;mov byte ptr [buff_w], al
	;call poke
	; 07/09/2019
	call dskwr
sync_2:
	mov ax, word ptr [Error]
sync_3:
	retn	

sync	endp

align 2
	db 0
buff_d: db 0
buff_o: dw offset Buffer ; 02/09/2019 - Retro UNIX 386 v2
buff_s: dw 0FFFFh ; Buffer sector
buff_m:	db 0 ; buffer changed/modified (dirty) flag
buff_w: db 0 ; read/write flag (write=1, read=0)
;buff_c: dw 0 ; count ; 05/09/2019
buff_c:	db 0 ; count ; 23/01/2020

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
; size table fopr directory inodes (1 to 7)
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

		; 21/12/2019
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

align 16

im_buffer: ; 02/09/2019 - Retro UNIX386 v2 (inode map buffer)
Buffer:
sector_buffer:
db 512 dup (0)

; 01/09/2019
fbm_buffer:  ; Free Blocks Map buffer
db 512 dup (0)