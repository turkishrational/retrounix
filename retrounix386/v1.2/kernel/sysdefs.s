; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2.2.4) - SYSDEFS.INC
; Last Modification: 27/12/2022
;
; ///////// RETRO UNIX 386 V1 SYSTEM DEFINITIONS ///////////////
; (Modified from 
;	Retro UNIX 8086 v1 system definitions in 'UNIX.ASM', 01/09/2014)
; ((UNIX.ASM (RETRO UNIX 8086 V1 Kernel), 11/03/2013 - 01/09/2014))
; 	UNIX.ASM (MASM 6.11) --> SYSDEFS.INC (NASM 2.11)
; ----------------------------------------------------------------------------
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ****************************************************************************

; 10/01/2022 - Retro UNIX 386 v1.2
; (SB structure has been moved here from 'ux.s' 
;	to overcome NASM's bss addressing bug !!!)
; (('ux.s' bss section addresses are being overlapped
;    when SB structure is defined in 'ux.s'))

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

; 10/01/2022 - Retro UNIX 386 v1.2
; (file structure has been moved here from 'ux.s' 
;	to overcome NASM's bss addressing bug !!!)
; (('ux.s' bss section addresses are being overlapped
;    when file structure is defined in 'ux.s'))

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

nproc 	equ	16  ; number of processes
ntty	equ     8   ; 8+1 -> 8 (10/05/2013)
;nbuf	equ	6   ; number of buffers (04/02/2016)
nbuf	equ	16  ; number of buffers ; 11/06/2022		
nfiles  equ	50  ; NFILES equ 50 ; 02/01/2022

; 27/12/2021
;NFILES	  equ   32  ; temporary ! ( 27/12/2021)
; 02/01/2022
;OPENFILES equ	10  ; open files (for user) ; 28/03/2020

;csgmnt	equ	2000h	; 26/05/2013 (segment of process 1)
;core	equ 	0  	    ; 19/04/2013	
;ecore	equ	32768 - 64  ; 04/06/2013 (24/05/2013)
	; (if total size of argument list and arguments is 128 bytes)
	; maximum executable file size = 32768-(64+40+128-6) = 32530 bytes
	; maximum stack size = 40 bytes (+6 bytes for 'IRET' at 32570)	
	; initial value of user's stack pointer = 32768-64-128-2 = 32574
	; 	(sp=32768-args_space-2 at the beginning of execution)
	; argument list offset = 32768-64-128 = 32576 (if it is 128 bytes)
	; 'u' structure offset (for the '/core' dump file) = 32704
	; '/core' dump file size = 32768 bytes
 
; 08/03/2014 
;sdsegmnt equ	6C0h  ; 256*16 bytes (swap data segment size for 16 processes)
; 19/04/2013 Retro UNIX 8086 v1 feaure only !
;;sdsegmnt equ 	740h  ; swap data segment (for user structures and registers)

; 30/08/2013
time_count equ 4 ; 10 --> 4 01/02/2014

; 05/02/2014
; process status
;SFREE 	equ 0
;SRUN	equ 1
;SWAIT	equ 2
;SZOMB	equ 3
;SSLEEP	equ 4 ; Retro UNIX 8086 V1 extension (for sleep and wakeup)

; 09/03/2015
userdata equ 80000h ; user structure data address for current user ; temporary
swap_queue equ 90000h - 2000h ; swap queue address ; temporary
swap_alloc_table equ 0D0000h  ; swap allocation table address ; temporary

; 17/09/2015
ESPACE equ 48 ; [u.usp] (at 'sysent') - [u.sp] value for error return

; 27/12/2022 (40)
; 12/01/2022 (37,38,39)
; 21/09/2015 (36) 
; 01/07/2015 (35)
; 14/07/2013 (0-34)
; UNIX v1 system calls
_rele 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_stty 	equ 31
_gtty	equ 32
_ilgins	equ 33
_sleep	equ 34 ; Retro UNIX 8086 v1 feature only !
_msg	equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_ver	equ 39 ; (get) Retro Unix 386 version
; 27/12/2022 - Retro UNIX 386 v1.2
_mem	equ 40 ; get available memory

%macro sys 1-4
    ; 03/09/2015
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3
            mov ecx, %3
            %if %0 = 4
               mov edx, %4
            %endif
        %endif
    %endif
    mov eax, %1
    int 30h
%endmacro

; 13/05/2015 - ERROR CODES
ERR_FILE_NOT_OPEN  equ 10 ; 'file not open !' error
ERR_FILE_ACCESS    equ 11 ; 'permission denied !' error
; 14/05/2015
ERR_DIR_ACCESS     equ 11 ; 'permission denied !' error
ERR_FILE_NOT_FOUND equ 12 ; 'file not found !' error
ERR_TOO_MANY_FILES equ 13 ; 'too many open files !' error
ERR_DIR_EXISTS     equ 14 ; 'directory already exists !' error
; 16/05/2015		
ERR_DRV_NOT_RDY    equ 15 ; 'drive not ready !' error
; 18/05/2015
ERR_DEV_NOT_RDY    equ 15 ; 'device not ready !' error
ERR_DEV_ACCESS     equ 11 ; 'permission denied !' error 
ERR_DEV_NOT_OPEN   equ 10 ; 'device not open !' error	
; 07/06/2015
ERR_FILE_EOF	   equ 16 ; 'end of file !' error
ERR_DEV_VOL_SIZE   equ 16 ; 'out of volume' error
; 09/06/2015
ERR_DRV_READ	   equ 17 ; 'disk read error !'
ERR_DRV_WRITE	   equ 18 ; 'disk write error !'
; 16/06/2015
ERR_NOT_DIR	   equ 19 ; 'not a (valid) directory !' error
ERR_FILE_SIZE	   equ 20 ; 'file size error !'	
; 22/06/2015
ERR_NOT_SUPERUSER  equ 11 ; 'permission denied !' error
ERR_NOT_OWNER      equ 11 ; 'permission denied !' error
ERR_NOT_FILE       equ 11 ; 'permission denied !' error	
; 23/06/2015
ERR_FILE_EXISTS    equ 14 ; 'file already exists !' error
ERR_DRV_NOT_SAME   equ 21 ; 'not same drive !' error
ERR_DIR_NOT_FOUND  equ 12 ; 'directory not found !' error
ERR_NOT_EXECUTABLE equ 22 ; 'not executable file !' error
; 27/06/2015
ERR_INV_PARAMETER  equ 23 ; 'invalid parameter !' error
ERR_INV_DEV_NAME   equ 24 ; 'invalid device name !' error
; 29/06/2015
ERR_TIME_OUT	   equ 25 ; 'time out !' error
ERR_DEV_NOT_RESP   equ 25 ; 'device not responding !' error
; 08/02/2022 
; (error numbers from TRDOS 386 v2.0 'sysdefs.s')
; 10/10/2016
ERR_INV_FILE_NAME  equ 26 ; 'invalid file name !' error
; 18/05/2016
ERR_MISC	   equ 27 ; miscellaneous/other errors
; 15/10/2016
ERR_INV_FORMAT	   equ 28 ; 'invalid format !' error
ERR_INV_DATA	   equ 29 ; 'invalid data !' error
; 16/10/2016
ERR_DISK_WRITE	   equ 30 ; 'disk write protected !'
; 08/02/2022
ERR_INV_FS	   equ 28 ;'invalid fs/superblock !' error

; 12/06/2022
; printer errors
ERR_PRN_NOT_RDY	   equ 15 ; 'device not ready !' error
ERR_PRN_TIMEOUT	   equ 25 ; 'time out !' error
ERR_PRN_PAPER	   equ 31 ; 'out of paper !' error
ERR_PRN_IO	   equ 32 ; 'io error !' error
ERR_PRN_BUSY	   equ 34 ; 'busy !' error

; 26/08/2015
; 24/07/2015
; 24/06/2015
MAX_ARG_LEN	   equ 256 ; max. length of sys exec arguments
; 01/07/2015
MAX_MSG_LEN	   equ 255 ; max. msg length for 'sysmsg'
;
; 26/11/2021 ('no free blocks on disk !' error)
ERR_ALLOC	   equ 33  ; 'disk allocation error !'	
; 22/11/2021
ERR_READ_ONLY_FS   equ 30  ; 'read only file system !' error
; 28/11/2021
ERR_INV_FILE	   equ 255 ; 'invalid file (inode) !' error
; 04/12/2021
ERR_PERM_DENIED	   equ 11  ; 'permission denied !' error
; 11/12/2021
ERR_INV_FUNC	   equ 1   ; 'invalid system call !' error
; 10/01/2022
ERR_INO_ALLOC	   equ 33  ; 'inode allocation error !'
ERR_NOT_REGULAR    equ 255 ; 'not regular file directory !' error

; 12/01/2022 - Retro UNIX 386 v1.2
%define s.time systm+504 ; boot/sysinit time (or current time)
		