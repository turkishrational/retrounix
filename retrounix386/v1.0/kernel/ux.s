; Retro UNIX 386 v1 Kernel (v0.2.0.22) - SYSX.INC (ux.s)
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
; 13/11/2015

alignb 2

inode:
	; 11/03/2013. 
	;Derived from UNIX v1 source code 'inode' structure (ux).
	;i.

	i.flgs:	 resw 1
	i.nlks:	 resb 1
	i.uid:	 resb 1
        i.size:  resw 1 ; size
	i.dskp:	 resw 8 ; 16 bytes
	i.ctim:	 resd 1
	i.mtim:	 resd 1
	i.rsvd:  resw 1 ; Reserved (ZERO/Undefined word for UNIX v1.)

I_SIZE	equ $ - inode 

process:
	; 26/02/2022
	; 04/02/2022
	; 06/05/2015
	; 11/03/2013 - 05/02/2014
	;Derived from UNIX v1 source code 'proc' structure (ux).
	;p.
	
        p.pid:   resw nproc
        p.ppid:  resw nproc
        ;p.break: resw nproc ; 04/02/2022 (p.break is not used)
        p.ttyc:  resb nproc ; console tty in Retro UNIX 8086 v1.
	; 26/02/2022 (p.waitc is not used)
	;p.waitc: resb nproc ; waiting channel in Retro UNIX 8086 v1.
	p.link:	 resb nproc
	p.stat:	 resb nproc

	; 06/05/2015 (Retro UNIX 386 v1 fetaure only !) 
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


; 15/04/2015
fsp:	 resb nfiles * 10 ; 11/05/2015 (8 -> 10)
bufp:	 resd (nbuf+2) ; will be initialized 
ii:	 resw 1
idev:	 resw 1 ; device number is 1 byte in Retro UNIX 8086 v1 !
cdev:    resw 1 ; device number is 1 byte in Retro UNIX 8086 v1 !
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
mnti:	 resw 1
mntp:	 resw 1 ; 14/05/2022 ; (parent dir inumber of [mnti])
mpid:	 resw 1
rootdir: resw 1
; 14/02/2014
; Major Modification: Retro UNIX 8086 v1 feature only!
;		      Single level run queue
;		      (in order to solve sleep/wakeup lock)
runq:	 resw 1
imod:	 resb 1
smod:	 resb 1
mmod:	 resb 1
sysflg:	 resb 1

alignb 4

user:
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
	u.fp:	  resb 10
	u.fofp:	  resd 1
	u.dirp:	  resd 1
	u.namep:  resd 1
	u.off:	  resd 1
	u.base:	  resd 1
	u.count:  resd 1
	u.nread:  resd 1
	u.break:  resd 1 ; break
	u.ttyp:	  resw 1 
	u.dirbuf: resb 10
	;u.pri:	  resw 1 ; 14/02/2014
	u.quant:  resb 1 ; Retro UNIX 8086 v1 Feature only ! (uquant)
	u.pri:	  resb 1 ; 
	u.intr:	  resw 1
	u.quit:	  resw 1
	;u.emt:	  resw 1 ; 10/10/2013
	u.ilgins: resw 1
	u.cdrv:	  resw 1 ; cdev
	u.uid:	  resb 1 ; uid
	u.ruid:	  resb 1
	u.bsys:	  resb 1
	u.uno:	  resb 1
        u.upage:  resd 1 ; 16/04/2015 - Retro Unix 386 v1 feature only !
	; tty number (rtty, rcvt, wtty)
	u.ttyn:	  resb 1 ; 28/07/2013 - Retro Unix 8086 v1 feature only !
	; last error number
	u.error:  resd 1 ; 28/07/2013 - 09/03/2015 
		        ; Retro UNIX 8086/386 v1 feature only!
	u.pgdir:  resd 1 ; 09/03/2015 (page dir addr of process)
	u.ppgdir: resd 1 ; 06/05/2015 (page dir addr of the parent process)
	u.pbase:  resd 1 ; 20/05/2015 (physical base/transfer address)
	u.pcount: resw 1 ; 20/05/2015 (byte -transfer- count for page)
	;u.pncount: resw 1 
		; 16/06/2015 (byte -transfer- count for page, 'namei', 'mkdir')
	;u.pnbase:  resd 1 
		; 16/06/2015 (physical base/transfer address, 'namei', 'mkdir')
			 ; 09/06/2015
	u.kcall:  resb 1 ; The caller is 'namei' (dskr) or 'mkdir' (dskw) sign		
	u.brwdev: resb 1 ; Block device number for direct I/O (bread & bwrite)
			 ; 24/07/2015 - 24/06/2015
	;u.args:  resd 1 ; arguments list (line) offset from start of [u.upage]
			 ; (arg list/line is from offset [u.args] to 4096 in [u.upage])
			 ; ([u.args] points to argument count -argc- address offset)
 			 ; 24/06/2015	  	
	;u.core:  resd 1 ; physical start address of user's memory space (for sys exec)
	;u.ecore: resd 1 ; physical end address of user's memory space (for sys exec)
			 ; 21/09/2015 (debugging - page fault analyze)
	u.pfcount: resd 1 ; page fault count for (this) process (for sys geterr)

alignb 4

U_SIZE	equ $ - user

; 18/10/2015 - Retro UNIX 386 v1 (local variables for 'namei' and 'sysexec')
pcore:  resd 1 ; physical start address of user's memory space (for sys exec)
ecore:  resd 1 ; physical start address of user's memory space (for sys exec)
nbase:	resd 1	; physical base address for 'namei' & 'sysexec'
ncount: resw 1	; remain byte count in page for 'namei' & 'sysexec'
argc:	resw 1	; argument count for 'sysexec'
argv:	resd 1	; argument list (recent) address for 'sysexec'

; 03/06/2015 - Retro UNIX 386 v1 Beginning
; 07/04/2013 - 31/07/2013 - Retro UNIX 8086 v1
rw: 	 resb 1 ;; Read/Write sign (iget)
rwdsk:	 resb 1 ;; Read/Write function number (diskio) - 16/06/2015
retry_count: resb 1 ; Disk I/O retry count - 11/06/2015
	 resb 1 ;; Reserved (16/06/2015) 

;alignb 4

; 22/08/2015
buffer: resb nbuf * 520

sb0:	resd 2
;s:
; (root disk) super block buffer
systm:
	; 13/11/2015 (Retro UNIX 386 v1)	
	; 11/03/2013. 
	;Derived from UNIX v1 source code 'systm' structure (ux).
	;s.

	resw 1
	resb 360 ; 2880 sectors ; original UNIX v1 value: 128
	resw 1
	resb 32	 ; 256+40 inodes ; original UNIX v1 value: 64
eofitab equ $ - systm ; 12/02/2022 (end of inode table)
	s.time:	 resd 1
	s.syst:	 resd 1
        s.wait_: resd 1 ; wait
	s.idlet: resd 1
	s.chrgt: resd 1
	s.drerr: resw 1

S_SIZE	equ $ - systm

	resb 512-S_SIZE ; 03/06/2015	 

sb1:	resd 2
; (mounted disk) super block buffer
mount:	
	resb 512  ; 03/06/2015

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
