; Retro UNIX 386 v1 Kernel (v0.2.0.22) - SYSDEFS.INC
; Last Modification: 15/07/2022
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

nproc 	equ	16  ; number of processes
nfiles 	equ	50
ntty	equ     8   ; 8+1 -> 8 (10/05/2013)
;nbuf	equ	6   ; number of buffers (04/02/2016)
nbuf	equ	8   ; 15/07/2022	  	

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
; 19/04/2013 Retro UNIX 8086 v1 feature only !
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
swap_alloc_table equ 0D0000h  ;  swap allocation table address ; temporary

; 17/09/2015
ESPACE equ 48 ; [u.usp] (at 'sysent') - [u.sp] value for error return

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

%macro sys 1-4
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.		
    mov eax, %1
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
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
; 12/02/2022 
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
ERR_INV_FS	   equ 28  ;'invalid fs/superblock !' error

; 13/06/2022
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