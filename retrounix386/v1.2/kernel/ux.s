; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1.2 Kernel (v0.2.2.3) - ux.s 
; Last Modification: 15/07/2022
;
; ///////// RETRO UNIX 386 V1 SYSTEM DEFINITIONS ///////////////
; (Modified from 
;	Retro UNIX 8086 v1 system definitions in 'UNIX.ASM', 01/09/2014)
; ((UNIX.ASM (RETRO UNIX 8086 V1 Kernel), 11/03/2013 - 01/09/2014))
; ----------------------------------------------------------------------------
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
; (Section E10 (17/3/1972) - ux.s)
; ****************************************************************************

; 15/07/2022
; 08/06/2022
; 15/05/2022
; 21/03/2022
; 27/02/2022
; 12/01/2022
; 22/11/2021 (Modification in Runix v1.1 'ux.s' for runix v2 fs compatibility)
; 18/07/2021
; 15/07/2021
; 12/05/2021
; 09/05/2021
; 06/05/2021
; 02/05/2021
; 26/01/2020 (NASM version of SuperBlock structure in UNIXHDCP.ASM)

; 21/12/2019 (UNIXCOPY.COM, UNIXCOPY.ASM, UNIXHDCP.COM, UNIXHDCP.ASM)
; 19/12/2019 (UNIXHDFS.COM, RUFSHDI.ASM)
; 01/09/2019 - Retro UNIX 386 v2 SuperBlock

; 14/01/2020 - Super Block modification:
;	     - Extended sections/divisions (consequental sectors)
;	     - (for swapping, configuration, boot space etc.)	

; 26/01/2020 (unix386.s, ux.s)
; 14/01/2020
SB.HiddenSects	equ SB.BootSectAddr
SB.TotalSects	equ SB.VolumeSize

; 11/05/2021 (Retro UNIX 386 v2)
; Super block status byte (byte 0 of SB.status dword)
; 	bit 0 - superblock modified flag
; 	bit 1 - inode map modified flag
; 	bit 2 - free blocks map modified flag
; 	bit 3 - inode table modified flag
;; 	bit 4 - boot sector modified flag ; 17/08/2021
; 17/08/2021
;	bit 4 - file data write error (for SB.LastInode)
;	bit 5 - inode table write error (for SB.LastInode)
;	bit 6 - inode map write error (for SB.LastInode)
;	bit 7 - free blocks map write error (for SB.LastInode)
	
; 21/12/2019
; 19/12/2019 - Retro UNIX 386 v2 HD (071h) partition boot sector 
;	       (UNIXHDFS.ASM)
; 04/12/2015 (14 byte file names - Retro UNIX 386 v1.1)
; 14/07/2015 (8 byte file names - Retro UNIX 8086 v1 & Retro UNIX 386 v1.0)

bsFSystemID 	equ 2  ; db 'RUFS'	
bsVolumeSerial 	equ 6  ; dd 0 ; (4 bytes)
bsFDSign	equ 10 ; db 'fd'
bsDriveNumber 	equ 12 ; db 0 ; fd0 or fd1 (0 or 1)
bsReserved 	equ 13 ; db 0 ; (512 bytes per sector)	
bsSecPerTrack	equ 14 ; db 18 ; (9 or 15)	
bsHeads		equ 15 ; db  ; 2
bsTracks	equ 16 ; dw 80 ; bsCylinders
bs_bf_inode_number equ 18 ; dw 0 ; 0 or Boot/Startup File I-Number
bsInfoEndsign	equ 20 ; db '@'
; 21/12/2019
bsMagic		equ 20 ; db '@'
bsPartitionID	equ 21 ; db 0 ; db 71h
bsHiddenSects	equ 22 ; dd 0 ; Hidden sectors (Boot Sector LBA)

; 22/11/2021 - Retro UNIX v2 I-node Flags
; ------------------------------------------------------------------
; UINSTALL.ASM (included in UNIXFDFS.ASM) - 23/01/2020
; ------------------------------------------------------------------

; Retro UNIX 386 v2 I-node Flags: (di_mode) for files
; 1000000000000000b 	IFREG - 1 = regular file (8000h)
; 0100000000000000b	IFDIR - 1 = directory (4000h)
; 0010000000000000b	IRSVD - 0 = reserved bit (2000h) ; Mounted flag
; 0001000000000000b	ILARG - large file addressing bit (1000h)
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
; 0000100000000000b	IPIPE - pipe special (800h) ; 07/02/2020
; 0000010000000000b	IREDIR - redirected (400h)  ; 07/02/2020
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

; 10/01/2022 - Retro UNIX 386 v1.2
; (SB structure has been moved from here to 'sysdefs.s'
;	to overcome NASM's bss addressing bug !!!)
; (('ux.s' bss section addresses are being overlapped
;    when SB structure is defined in 'ux.s'))

%if 0

struc SB ; SuperBlock

.Header:	resd 1
;.HiddenSects:
.BootSectAddr:	resd 1	; Hidden Sectors
;.TotalSects:
.VolumeSize:	resd 1	; Entire Volume/Partition Size (includes ext. volume)
.Version:	resd 1	
.BlockSize:	resd 1	
.InodeCount:	resd 1	
.FreeMapAddr:	resd 1	
.FreeMapSize:	resd 1	
.InodeMapAddr:	resd 1	
.InodeMapSize:	resd 1	
.InodeTblAddr:	resd 1	
.InodeTblSize:	resd 1	
.FreeInodes:	resd 1	
.FirstFreeIno:	resd 1	
.FreeBlocks:	resd 1	
.FirstFreeBlk:	resd 1	
.BootSecParms:	resb 19	; v1
.BSExtension:	resb 5	; v2 HDFS
.Status:	resb 1	; 12/05/2021 (system modification status) (*)
.Pdrv:		resb 1  ; Physical disk number (index) ; 12/05/2021 (*) 
.Uno:		resw 1	; user/process number ; 12/05/2021 (*)
.ModifTime:	resd 1	; (last) modification time (*)
.ExtdVolTbl:	resd 1	; Extended Volume Start/Table Address
.ExtdVolSize:	resd 1	; Extended Volume (swap section etc.) Size	
.LBA_rw:	resb 1
.ClusterSize:	resb 1
.ReadOnly:	resb 1	; (SB will not be written to disk if bit 0 is 1)
.Mounted:	resb 1
.MountInode:	resd 1  ; double word
.DevMajor:	resb 1
.DevMinor:	resb 1
.LongName:	resb 1
.Direntry32:	resb 1
; 18/07/2021
.FileBuffer:	resd 1
.ItabBuffer:	resd 1
.ImapBuffer:	resd 1
.FmapBuffer:	resd 1
 ; 15/07/2021
.LastInode:	resd 1
; 02/05/2021
.FmapIndex:	resd 1
.ImapIndex:	resd 1
.ItableIndex:	resd 1
.Reserved:	resb 508-148 ; 18/07/2021
.Footer:	resd 1

endstruc

%endif

alignb 2

inode:
	;; 11/03/2013. 
	;;Derived from UNIX v1 source code 'inode' structure (ux).
	;;i.
	;
	;i.flgs: resw 1
	;i.nlks: resb 1
	;i.uid:	 resb 1
        ;i.size: resw 1 ; size
	;i.dskp: resw 8 ; 16 bytes
	;i.ctim: resd 1
	;i.mtim: resd 1
	;i.rsvd: resw 1 ; Reserved (ZERO/Undefined word for UNIX v1)

	; 26/01/2020
	; Retro UNIX 386 v2.0 - Modified UNIX v7 inode model
	;	(15/09/2029 .. 18/12/2019)

	i.flgs:   resw 1	; /* mode and type of file */
	i.nlks:	  resw 1	; /* number of links to file */
	i.uid:	  resw 1	; /* owner's user id */  - 0 to 65535 -
	i.gid:	  resb 1	; /* owner's group id */ - o to 255 -
	i.size_h: resb 1	; /* number of bytes in file */ ; byte 5
	i.size:	  resd 1 ; size	; /* number of bytes in file */
	i.dskp:	  resd 10 ; 40 bytes ; /* disk block addresses */
	i.atim:	  resd 1	; /* time last accessed */
	i.mtim:	  resd 1	; /* time last modified */
	i.ctim:	  resd 1	; /* time created */

I_SIZE	equ $ - inode

process:
	; 27/02/2022
	; 12/01/2022 (Retro UNIX 386 v1.2) 
	; 06/05/2015
	; 11/03/2013 - 05/02/2014
	;Derived from UNIX v1 source code 'proc' structure (ux).
	;p.
	
        p.pid:   resw nproc
        p.ppid:  resw nproc
	;p.break: resw nproc ; 12/01/2022 (p.break is not used)
        p.ttyc:  resb nproc ; console tty in Retro UNIX 8086 v1.
	; 27/02/2022 (p.waitc is not used)
	;p.waitc: resb nproc ; waiting channel in Retro UNIX 8086 v1.
	p.link:	 resb nproc
	p.stat:	 resb nproc

	; 06/05/2015 (Retro UNIX 386 v1 feature only !) 
	p.upage: resd nproc ; Physical address of the process's
			    ; 'user' structure	

P_SIZE	equ $ - process

; fsp table (original UNIX v1)
;
;Entry
;          15                                      0
;  1     |---|---------------------------------------|
;        |r/w|       i-number of open file           |
;        |---|---------------------------------------| 
;        |               device number               |
;        |-------------------------------------------|
;    (*) | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------| 
;        |  flag that says    | number of processes  |
;        |   file deleted     | that have file open  |
;        |-------------------------------------------| 
;  2     |                                           |
;        |-------------------------------------------| 
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------| 
;  3     |                                           | 
;        |                                           |  
;
; (*) Retro UNIX 386 v1 modification: 32 bit offset pointer 

; 27/03/2020 - Retro UNIX 386 v2 - FSP (OPEN FILES) TABLE 

;Entry
;         15                    7                   0
;  1     |-------------------------------------------|
;        |   	     i-number of open file           |
;        |-------------------------------------------| 
;        |        high word of 32 bit i-number       |
;        |-------------------------------------------|
;        | open mode & status  |   device number     |
;        |-------------------------------------------|
;        |    reserved byte    |     open count      |
;        |-------------------------------------------| 
;        | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 16-31)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 32-47)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 48-63)  | 
;        |-------------------------------------------|
;  2     |                                           |
;        |-------------------------------------------| 
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------| 
;        |                                           | 

; 10/01/2022 - Retro UNIX 386 v1.2
; (file structure has been moved from here to 'sysdefs.s'
;	to overcome NASM's bss addressing bug !!!)
; (('ux.s' bss section addresses are being overlapped
;    when file file structure is defined in 'ux.s'))

%if 0

; 22/11/2021
; 21/07/2021 - Retro UNIX 386 v2 open file structure revision

struc file	; open files (fsp) structure
  .inode:  resw 1  ; inode number of open file (32 bit)
  .i32:	   resw 1  ; higher word of inode number (reserved)
  .drive:  resb 1  ; logical drive (disk) number
  .flags:  resb 1  ; open mode and status
  .count:  resb 1  ; number of processes that have file open
  ;.rsvd:  resb 1  ; reserved byte (for next versions)
  .mnt:    resb 1  ; mnttab index+1 (0 = not mounted)
  .offset: resd 1  ; file offset/pointer (64 bit) 
  .o64:	   resd 1  ; higher 32 bit of file offset
 .size:  ; = 16		
endstruc

%endif

; 01/01/2022
; 22/11/2021
;fp.size equ file.size

; 02/01/2022
fsp:	resb nfiles*16 ; (NFILES*fp.size)
; 01/01/2022
; 22/11/2021 (16/05/2021)
;fsp:	resb NFILES*16 ; (NFILES*fp.size)
; 15/04/2015
;fsp:	resb nfiles*10 ; 11/05/2015 (8 -> 10)

bufp:	resd (nbuf+2) ; will be initialized 
ii:	resd 1 ; 22/11/2021 ; 32 bit inode number (high word is 0)
; 22/11/2021
idev:	resb 1 ; logical drive number of current inode, [ii]
cdev:	resb 1 ; current logical drive number for current user

; 18/05/2015
; 26/04/2013 device/drive parameters (Retro UNIX 8086 v1 feature only!)
; 'UNIX' device numbers (as in 'cdev' and 'u.cdrv')
;	0 -> root device (which has Retro UNIX 8086 v1 file system)
; 	1 -> mounted device (which has Retro UNIX 8086 v1 file system)
; 'Retro UNIX 8086 v1' device numbers: (for disk I/O procedures)
;	0 -> fd0 (physical drive, floppy disk 1), physical drive number = 0
;	1 -> fd1 (physical drive, floppy disk 2), physical drive number = 1
;	2 -> hd0 (physical drive, hard disk 1), physical drive number = 80h
;	3 -> hd1 (physical drive, hard disk 2), physical drive number = 81h
;	4 -> hd2 (physical drive, hard disk 3), physical drive number = 82h
;	5 -> hd3 (physical drive, hard disk 4), physical drive number = 83h
rdev:	 resb 1 ; root device number ; Retro UNIX 8086 v1 feature only!
	        ; as above, for physical drives numbers in following table
mdev:	 resb 1 ; mounted device number ; Retro UNIX 8086 v1 feature only!
; 15/04/2015
;active: resb 1 ; 15/07/2022
;	 resb 1 ; 09/06/2015
mpid:	 resw 1
; 22/11/2021 (32 bit inode numbers)
rootdir: resd 1
mnti:	 resd 1
; 15/05/2022 ; (parent dir inumber of [mnti])
mntp:	 resd 1 

; 14/02/2014
; Major Modification: Retro UNIX 8086 v1 feature only!
;		      Single level run queue
;		      (in order to solve sleep/wakeup lock)
runq:	resw 1
imod:	resb 1
imodx:	resb 1 ; 09/01/2022 - Retro UNIX 386 v1.2
smod:	resb 1
mmod:	resb 1
;	resb 1 ; 09/01/2022 
sysflg:	resb 1

alignb 4

user:
	; 01/01/2022
	; 04/12/2021 - Retro UNIX 386 v1.2
	; 24/10/2021
	; 18/10/2021
	; 10/06/2021
	; 30/05/2021
	; 29/05/2021
	; 21/05/2021
	; 20/05/2021
	; 16/05/2021
	; 01/05/2021
	; 27/03/2021
	; 17/04/2020, 28/04/2020
	; 25/03/2020, 28/03/2020
	; 20/03/2020, 22/03/2020, 23/03/2020
	; 05/03/2020, 08/03/2020, 14/03/2020
	; 07/02/2020 - Retro UNIX 386 v2
	;
	; 27/02/2017 - TRDOS 386
	; 13/01/2017 - TRDOS 386
	; 10/01/2017 - TRDOS 386
	; 19/12/2016 - TRDOS 386	
	; 21/05/2016 - TRDOS 386 (TRDOS v2.0) 
	; 	       [u.pri] usage method modification
	;
	; 04/12/2015 - Retro UNIX 386 v1.1 (14 byte file/directory names)
	; 18/10/2015
	; 12/10/2015
	; 21/09/2015
	; 24/07/2015
	; 16/06/2015
	; 09/06/2015
	; 11/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - 32 bit modifications)
	; 10/10/2013
	; 11/03/2013. 
	;Derived from UNIX v1 source code 'user' structure (ux).
	;u.

	u.sp:	  resd 1 ; esp (kernel stack at the beginning of 'sysent')
	u.usp:	  resd 1 ; esp (kernel stack points to user's registers)
	u.r0:	  resd 1 ; eax
	u.cdir:	  resw 1
		  resw 1 ; 28/03/2020 - reserved for 32 bit inode number
	;u.cdrv:  resw 1 ; 17/04/2020 (dword alignment) 
	u.cdrv:   resb 1 ; 01/05/2021
		  resb 1 ; 01/05/2021 (dword alignment)
	u.fp:	  resb 10 ; Retro UNIX 386 v1
	;u.fp:	  resb OPENFILES ; Retro UNIX 386 v2 ; 28/03/2020 
	u.fsp:	  ; 16/05/2021
	u.fofp:	  resd 1 ; 16/05/2021 (pointer to fsp entry)
	u.dirp:	  resd 1
	u.namep:  resd 1
	u.off:	  resd 1
	;	  resd 1 ; 08/03/2020 - Retro UNIX 386 v2 - 64 bit fptr
	u.base:	  resd 1
	u.count:  resd 1
	u.nread:  resd 1
	u.break:  resd 1 ; break
	; 16/05/2021 (Retro UNIX 386 v2)
	u.mode:   resb 1 ; 16/05/2021 (sysread, syswrite, 'rdwr' file mode)
	; 10/01/2017 (TRDOS 386, relocation and dword alignment)
	; tty number (rtty, rcvt, wtty)
	u.ttyn:	  resb 1 ; 28/07/2013 - Retro Unix 8086 v1 feature only !
	u.ttyp:	  resw 1 
	u.dirbuf: resb 16 ; 04/12/2015 (10 -> 16) 
	;u.pri:	  resw 1 ; 14/02/2014
	u.quant:  resb 1 ; Retro UNIX 8086 v1 Feature only ! (uquant)
		  resb 1 ; 17/04/2020
	u.pri:	  resb 1 ; Modification: 21/05/2016 (priority levels: 0, 1, 2)
		  resb 1 ; 17/04/2020
	; 10/06/2021
	;u.signal: resw 1 ; 21/05/2021 - Retro UNIX 386 v2
	;	  resw 1 ; reserved ; 21/05/2021	
	u.intr:	  resw 1
	u.quit:	  resw 1
	;u.emt:	  resw 1 ; 10/10/2013
	;u.ilgins: resw 1 ; 10/01/2017
	;u.cdrv:  resw 1 ; cdev ; 17/04/2020 (moved to up)
	;u.uid:	  resb 1 ; uid
	;u.ruid:  resb 1
	u.bsys:	  resb 1
	u.uno:	  resb 1
        u.uid:	  resw 1 ; uid	; 27/03/2021 - Retro UNIX 386 v2
	u.ruid:	  resw 1	; 16 bit uid
	u.gid:	  resb 1 ; gid 	; 27/03/2021 - Retro UNIX 386 v2
	u.rgid:	  resb 1
	; 20/05/2021 - Retro UNIX 386 v2
	u.procp:  resd 1 ; /* pointer to proc structure */		
	u.upage:  resd 1 ; 16/04/2015 - Retro Unix 386 v1 feature only !
	u.pgdir:  resd 1 ; 09/03/2015 (page dir addr of process)
	u.ppgdir: resd 1 ; 06/05/2015 (page dir addr of the parent process)
	u.pbase:  resd 1 ; 20/05/2015 (physical base/transfer address)
		; 24/10/2021 (32 bit value for Retro UNIX 386 v2)
	u.pcount: resd 1 ; 20/05/2015 (byte -transfer- count for page)
	;u.pncount: resw 1 
		; 16/06/2015 (byte -transfer- count for page, 'namei', 'mkdir')
	;u.pnbase:  resd 1 
		; 16/06/2015 (physical base/transfer address, 'namei', 'mkdir')
	u.rsvd:	  resw 1 ; 04/12/2021 (dword alignment)
			 ; 09/06/2015
	u.kcall:  resb 1 ; The caller is 'namei' (dskr) or 'mkdir' (dskw) sign
		; 08/03/2020 (block device read/write flag for 'sioreg')		
	u.brwdev: resb 1 ; Block device number for direct I/O (bread & bwrite)
			 ; 24/07/2015 - 24/06/2015
	;u.args:  resd 1 ; arguments list (line) offset from start of [u.upage]
			 ; (arg list/line is from offset [u.args] to 4096 in [u.upage])
			 ; ([u.args] points to argument count -argc- address offset)
 			 ; 24/06/2015	  	
	;u.core:  resd 1 ; physical start address of user's memory space (for sys exec)
	;u.ecore: resd 1 ; physical end address of user's memory space (for sys exec)
	; last error number
	u.error:  resd 1 ; 28/07/2013 - 09/03/2015 
			; Retro UNIX 8086/386 v1 feature only!
			; 21/09/2015 (debugging - page fault analyze)
	u.pfcount: resd 1 ; page fault count for (this) process (for sys geterr)
		; 29/05/2021 - Retro UNIX 386 v2, 2021 (sleep, psig)
;	u.signal: resd 1 ; unix signal recognition (enabling, accepting) bits	
;	u.srb:	  resb 1 ; signal handling/responding method
;	u.snum:	  resb 1 ; signal number (last signal number)
;	u.exit:	  resb 1 ; exit code ; 30/05/2021 - Retro UNIX 386 v2
;	u.s_lock: resb 1 ; signal handling (phase) lock	
;	u.s_addr: resd 1 ; Signal Response Byte or signal handler (callback) address
;	u.s_time: resd 1 ; signal time in unix epoch format		
		; 19/12/2016 (TRDOS 386)	
;	u.tcb:	  resd 1 ; Timer callback address/flag which will be used by timer int
		; 13/01/2017 (TRDOS 386)
;	u.t_lock: resb 1 ; Timer interrupt (callback) lock (unlocked by 'sysrele')
;	u.t_mode: resb 1 ; running mode during timer interrupt (0= system, 0FFh= user)
		; 26/02/2017 (TRDOS 386)
;	u.irqc:	  resb 1  ; Count of IRQ callback services (IRQs in use)
		; 28/02/2017 (TRDOS 386) 
;	u.irqwait: resb 1 ; IRQ waiting for callback service flag (IRQ number, If > 0)
;	u.r_lock:  resb 1 ; 'IRQ callback service is in progress' flag (IRQ lock)
;	u.r_mode:  resb 1 ; running mode during hardware interrupt
		; 07/02/2020 - Retro UNIX 386 v2
;	u.redir:  resb 1 ; device func redirection permitted by user (if u.redir > 0)
		; 26/02/2020 - Retro UNIX 386 v2
;	u.rwm:	  resb 1  ; read & write mode for devices/drives, 0 = direct, 1 = fs r/w 
		; 24/02/2020 - Retro UNIX 386 v2
		; 24/10/2021 (32 bit value)
;	u.scount: resd 1 ; sub byte count ; 22/03/2020 
		; 07/03/2020 - Retro UNIX 386 v2
;	u.bps:	  resw 1 ; u.bps = bytes per sector, for readi and writei
;		  resw 1 ; 24/10/2021 (32 bit value for using with 32 bit registers)	
		; 23/03/2020 - Retro UNIX 386 v2
;	u.timeout: resw 1 ; timeout setting (seconds) -for 'sleep'-
		; 28/04/2020 - Retro UNIX 386 v2
;	u.lock:	  resb 1 ; Device lock flag (bit 0 is for device/disk read/write)
		; 24/04/2020
		; 17/04/2020 - Retro UNIX 386 v2
;	u.ifs:	  resb 1 ; (current) installable file system driver index number
		; 25/03/2020
		; 20/03/2020
	;u.lcount: resd 1 ; last byte count for 'writei' (for restoring write count)	

		; 08/03/2020 - Retro UNIX 386 v2 ('sioreg, 'readi')
	;u.limit: resd 1  ; disk/file size limit for block device read/write ('sioreg')			
	;	  resd 1  ; 08/03/2020 - 64 bit limit (disk size in bytes)
	;	; 26/02/2020 - Retro UNIX 386 v2
	;u.sector: resd 1  ; current sector (for readi and writei, device r/w)  		
	;u.buffer: resd 1  ; current buffer (for readi and writei, device r/w) 
		; 14/03/2020 - Retro UNIX 386 v2 (sleep, wakeup)
	;u.devmm: resw 1 ; device major (hb) and minor (lb) numbers
	;u.zero:  resw 1 ; 14/03/2020 - Must be ZERO for current version
		; 22/03/2020 - Retro UNIX 386 v2 (sleep, wakeup)
;	u.device: resd 1 ; (blk, chr) device description table (entry) address 	
		; 14/03/2020 - Retro UNIX 386 v2 (sleep, wakeup)
;	u.function: resd 1 ; device function (read, write)
		; 22/03/2020 - Retro UNIX 386 v2, 2020 (readi, writei)
;	u.buffer: resd 1  ; for saving buffer header address for 'poke'
;	u.inode:  resw 1  ; for saving inode number of current (device) inode
;		  resw 1  ; 18/10/2021 Retro UNIX 386 v2 - 32 bit inode number			
;	u.idev:	  resb 1  ; for saving logical drv number of current (dev) inode
		; 04/12/2021
;	u.rsvd2:  resw 1  ; (for dword alignment)	
		; 27/02/2017 (TRDOS 386) 
;; 31/12/2021 - temporary !
;	u.fpsave: resb 1  ; TRDOS 386, 'save/restore FPU registers' flag
alignb 4
	; !! wrong sizing in TRDOS 386 v2.0.4 (in 'ubss.s', 28/02/2017) !! 
	;u.fpregs: resb 94 ; 94 byte area for saving and restoring FPU registers
	; 30/05/2021 - Retro UNIX 386 v2
;	u.fpregs: resb 108 ; 108 byte area for saving and restoring FPU registers	
alignb 4

U_SIZE	equ $ - user

; 18/10/2015 - Retro UNIX 386 v1 (local variables for 'namei' and 'sysexec')
pcore:  resd 1 ; physical start address of user's memory space (for sys exec)
ecore:  resd 1 ; physical start address of user's memory space (for sys exec)
nbase:	resd 1	; physical base address for 'namei' & 'sysexec'
;ncount: resw 1 ; remain byte count in page for 'namei' & 'sysexec'
; 11/12/2021 - Retro UNIX 386 v1.2 (32 bit 'ncount')
ncount: resd 1	; remain byte count in page for 'namei' & 'sysexec'
;argc:	resw 1	; argument count for 'sysexec'
; 11/12/2021 - Retro UNIX 386 v1.2 (32 bit 'argc')
argc:	resd 1	; argument count for 'sysexec'
argv:	resd 1	; argument list (recent) address for 'sysexec'

; 03/06/2015 - Retro UNIX 386 v1 Beginning
; 07/04/2013 - 31/07/2013 - Retro UNIX 8086 v1
rw: 	 resb 1 ;; Read/Write sign (iget)
rwdsk:	 resb 1 ;; Read/Write function number (diskio) - 16/06/2015
mget_rw: resb 1 ; 22/11/2021 
retry_count: resb 1 ; Disk I/O retry count - 11/06/2015

; 08/06/2022 - (BugFix) ! (level: resd 0) !
; 22/11/2021 - Retro UNIX 386 v2 compatibility
level:	resd 1  ; level, level+1, level+2, level+3  ; for 'mget' procedure

; (02/01/2022)
; 27/11/2021
; these are will be used for disk block allocation ('alloc' proc)
free_map_offset: 
		resd 1
free_map_index:	resd 1
free_map_sector:
		resd 1
;free_map_buffer:
;		resd 1

; 12/01/2022
; 27/11/2021 - temporary !
;s.time:	resd 1

;alignb 4

; (02/01/2022)
; 22/08/2015
;buffer: resb nbuf * 520

sb0:	resd 2
;s:
; (root disk) super block buffer
systm:
	; 27/11/2021 - Retro UNIX 386 v2 compatible super block
	; 13/11/2015 (Retro UNIX 386 v1)	
	; 11/03/2013. 
	;Derived from UNIX v1 source code 'systm' structure (ux).
	;s.

	;resw 1
	;resb 360 ; 2880 sectors ; original UNIX v1 value: 128
	;resw 1
	;resb 32 ; 256+40 inodes ; original UNIX v1 value: 64
	;s.time: resd 1
	;s.syst: resd 1
        ;s.wait_: resd 1 ; wait
	;s.idlet: resd 1
	;s.chrgt: resd 1
	;s.drerr: resw 1

	; 27/11/2021
	resb 512	

;S_SIZE	equ $ - systm

	;resb 512-S_SIZE ; 03/06/2015

sb1:	resd 2
; (mounted disk) super block buffer
mount:	
	resb 512  ; 03/06/2015

; 21/03/2022
; hard disk masterboot sector buffer
mbrbuf: resd 2
	resb 512

; 10/01/2022
dbli_buf: resb 512  ; double indir ptr buff for 'itrunc', 'tloop'
trpi_buf: resb 512  ; triple indir ptr buff for 'itrunc', 'tloop'

;/ ux -- unix
;
;systm:
;
;	.=.+2
;	.=.+128.
;	.=.+2
;	.=.+64.
;	s.time: .=.+4
;	s.syst: .=.+4
;	s.wait: .=.+4
;	s.idlet:.=.+4
;	s.chrgt:.=.+4
;	s.drerr:.=.+2
;inode:
;	i.flgs: .=.+2
;	i.nlks: .=.+1
;	i.uid:  .=.+1
;	i.size: .=.+2
;	i.dskp: .=.+16.
;	i.ctim: .=.+4
;	i.mtim: .=.+4
;	. = inode+32.
;mount:	.=.+1024.
;proc:
;	p.pid:  .=.+[2*nproc]
;	p.dska: .=.+[2*nproc]
;	p.ppid: .=.+[2*nproc]
;	p.break:.=.+[2*nproc]
;	p.link: .=.+nproc
;	p.stat: .=.+nproc
;tty:
;	. = .+[ntty*8.]
;fsp:	.=.+[nfiles*8.]
;bufp:	.=.+[nbuf*2]+6
;sb0:	.=.+8
;sb1:	.=.+8
;swp:	.=.+8
;ii:	.=.+2
;idev:	.=.+2
;cdev:	.=.+2
;deverr: .=.+12.
;active: .=.+2
;rfap:	.=.+2
;rkap:	.=.+2
;tcap:	.=.+2
;tcstate:.=.+2
;tcerrc: .=.+2
;mnti:	.=.+2
;mntd:	.=.+2
;mpid:	.=.+2
;clockp: .=.+2
;rootdir:.=.+2
;toutt:	.=.+16.
;touts: .=.+32.
;runq:	.=.+6
;
;wlist:	.=.+40.
;cc:	.=.+30.
;cf:	.=.+31.
;cl:	.=.+31.
;clist:	.=.+510.
;imod:	.=.+1
;smod:	.=.+1
;mmod:	.=.+1
;uquant: .=.+1
;sysflg: .=.+1
;pptiflg:.=.+1
;ttyoch: .=.+1
; .even
; .=.+100.; sstack:
;buffer: .=.+[ntty*140.]
;	.=.+[nbuf*520.]
;
; . = core-64.
;user:
;	u.sp:    .=.+2
;	u.usp:   .=.+2
;	u.r0:    .=.+2
;	u.cdir:  .=.+2
;	u.fp:    .=.+10.
;	u.fofp:  .=.+2
;	u.dirp:  .=.+2
;	u.namep: .=.+2
;	u.off:   .=.+2
;	u.base:  .=.+2
;	u.count: .=.+2
;	u.nread: .=.+2
;	u.break: .=.+2
;	u.ttyp:  .=.+2
;	u.dirbuf:.=.+10.
;	u.pri:   .=.+2
;	u.intr:  .=.+2
;	u.quit:  .=.+2
;	u.emt:   .=.+2
;	u.ilgins:.=.+2
;	u.cdev:  .=.+2
;	u.uid:   .=.+1
;	u.ruid:  .=.+1
;	u.bsys:  .=.+1
;	u.uno:   .=.+1
;. = core