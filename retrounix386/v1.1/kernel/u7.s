; Retro UNIX 386 v1 Kernel (v0.2.1.4) - SYS7.INC
; Last Modification: 13/06/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U7.ASM (13/07/2014) //// UNIX v1 -> u7.s
;
; ****************************************************************************

sysmount: ; / mount file system; args special; name
	; 15/05/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.3)
	; 09/02/2022
	; 08/02/2022
	; 07/02/2022
	; 11/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 14/11/2015
	; 24/10/2015
	; 13/10/2015
	; 10/07/2015
	; 16/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/07/2013 - 04/11/2013 (Retro UNIX 8086 v1)
	;
	; 'sysmount' anounces to the system that a removable 
	; file system has been mounted on a special file.
	; The device number of the special file is obtained via
	; a call to 'getspl'. It is put in the I/O queue entry for
	; dismountable file system (sb1) and the I/O queue entry is
	; set up to read (bit 10 is set). 'ppoke' is then called to
	; to read file system into core, i.e. the first block on the
	; mountable file system is read in. This block is super block
	; for the file system. This call is super user restricted.	
	;
	; Calling sequence:
	;	sysmount; special; name
	; Arguments:
	;	special - pointer to name of special file (device)
	;	name -  pointer to name of the root directory of the
	;		newly mounted file system. 'name' should 
	;		always be a directory.
	; Inputs: - 
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysmount' system call has two arguments; so,
	;	* 1st argument, special is pointed to by BX register
	;	* 2nd argument, name is in CX register
	;
	;	NOTE: Device numbers, names and related procedures are 
	;	       already modified for IBM PC compatibility and 
	;	       Retro UNIX 8086 v1 device configuration.	
	
	;call	arg2
		; jsr r0,arg2 / get arguments special and name
	mov	[u.namep], ebx
	; 09/02/2022
	;push	ecx ; directory name
	cmp	word [mnti], 0
		; tst mnti / is the i-number of the cross device file
			 ; / zero?
	;;ja	error
        	; bne errora / no, error
	;ja	sysmnt_err0
	; 11/01/2022
	jna	short sysmnt_0
	jmp	sysmnt_err0
sysmnt_0:
	; 09/02/2022
	push	ecx ; directory name
	call	getspl
		; jsr r0,getspl / get special files device number in r1
	; 09/02/2022
	pop	dword [u.namep] ; directory name
	; 13/10/2015
	;movzx	ebx, ax ; Retro UNIX 8086 v1 device number (0 to 5)
	; 11/01/2022
	sub	ebx, ebx
	mov	bl, al
	test    byte [ebx+drv.status], 80h ; 24/10/2015 
	jnz	short sysmnt_1
sysmnt_err1:
        mov     dword [u.error], ERR_DRV_NOT_RDY ; drive not ready !
	jmp	error
sysmnt_1:
	; 09/02/2022
	;pop	dword [u.namep]
        	; mov (sp)+,u.namep / put the name of file to be placed
				  ; / on the device
	; 14/11/2015
	push	ebx ; 13/10/2015
		; mov r1,-(sp) / save the device number
        ;
	call	namei
	;or	ax, ax ; Retro UNIX 8086 v1 modification !
		       ; ax = 0 -> file not found 	
	;jz	error
	;jc	error
		; jsr r0,namei / get the i-number of the file
               	; br errora
	jnc	short sysmnt_2
sysmnt_err2:
        mov     dword [u.error], ERR_FILE_NOT_FOUND ; drive not ready !
	jmp	error
sysmnt_2:	
	mov	[mnti], ax
        	; mov r1,mnti / put it in mnti

	; 15/05/2022
	; -Retro UNIX 8086/386 v1 feaure only-
	mov	ax, [ii]
	mov	[mntp], ax ; parent dir inumber of [mnti]

	; 11/01/2022
	mov	ebx, sb1 ; super block buffer header (of mounted disk)
sysmnt_3: ;1:
        ;cmp	byte [ebx+1], 0
		; tstb sb1+1 / is 15th bit of I/O queue entry for
			   ; / dismountable device set?
        ;jna	short sysmnt_4		
		; bne 1b / (inhibit bit) yes, skip writing
	;call	idle 	; (wait for hardware interrupt)
	;jmp	short sysmnt_3
sysmnt_4:   
	pop	eax ; Retro UNIX 8086 v1 device number/ID (0 to 5)     
	mov	[mdev], al
		; mov (sp),mntd / no, put the device number in mntd
	mov	[ebx], al
        	; movb (sp),sb1 / put the device number in the lower byte
			      ; / of the I/O queue entry
	;mov	byte [cdev], 1 ; mounted device/drive
        	; mov (sp)+,cdev / put device number in cdev
        or	word [ebx], 400h ; Bit 10, 'read' flag/bit
		; bis $2000,sb1 / set the read bit
	; Retro UNIX 386 v1 modification : 
	;	32 bit block number at buffer header offset 4
	mov	dword [ebx+4], 1 ; physical block number = 1
	call 	diskio
	jnc	short sysmnt_5
	xor 	eax, eax
	mov	[mnti], ax ; 0
	mov	[mdev], al ; 0
	;mov	[cdev], al ; 0
	; 08/02/2022
	cmp	byte [u.brwdev], 0FFh ; is error code set in [u.error] ?
	jne	short sysmnt_err3
	; yes, clear [u.brwdev] for next check
	; ([u.error] = DRV_NOT_RDY or OUT_OF_VOLUME error) 
	inc	byte [u.brwdev] ; 0, reset
	jmp	short sysmnt_err4
sysmnt_err3:	; 08/02/2022
	; no, set [u.error] to disk read error
	mov	dword [u.error], ERR_DRV_READ ; 'disk read error !'
sysmnt_err4:
	; 08/02/2022
	; 14/11/2015
	dec 	al
	mov	[ebx], eax ; 000000FFh
	inc	al
	dec	eax
	mov	[ebx+4], eax ; 0FFFFFFFFh
	jmp	error
sysmnt_invd:
	; 08/02/2022
	mov	 dword [u.error], ERR_INV_FS ; 28
				 ;'invalid fs/superblock !' error
	xor	al, al
	jmp	short sysmnt_err4	

sysmnt_5:
	; 08/02/2022
	; 11/01/2022 (BugFix)
	; 14/11/2015 (Retro UNIX 386 v1 modification)
	; (Following check is needed to prevent mounting an
	; invalid file system (invalid super block).
	; 
	movzx	eax, byte [ebx] ; device number
	shl	al, 2 ; 4*index
	mov	ecx, [eax+drv.size] ; volume (fs) size
	shr 	ecx, 3 ; 11/01/2021 (8 sectors per 1 fbm byte)
	; ecx = number of free map bytes (required)
	;movzx	edx, word [sb1+8] ; the 1st data word ('mount:')
	movzx	edx, word [ebx+8] ; the 1st data word (of the buffer)	
	; edx = number of free blocks map bytes
	;shl	edx, 3 ; convert free map bytes to free map bits
	; 07/02/2022
	;xor	al, al ; 08/02/2022
	cmp	ecx, edx ; compare free map bits and volume size
			 ; (in sectors), if they are not equal
			 ; the disk to be mounted is an...	
	jne	short sysmnt_invd ; invalid disk !
			 ; (which has not got a valid super block)
	;
	mov	byte [ebx+1], 0
	       	; jsr r0,ppoke / read in entire file system
;sysmnt_6: ;1:
	;;cmp	byte [sb1+1], 0
		; tstb sb1+1 / done reading?
   	;;jna	sysret
	;;call	idle ; (wait for hardware interrupt)
	;;jmp	short sysmnt_6
		; bne 1b / no, wait
        	; br sysreta / yes
	jmp	sysret

sysumount: ; / special dismount file system
	; 15/05/2022
	; 09/05/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.3)
	; 11/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 16/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 09/07/2013 - 04/11/2013 (Retro UNIX 8086 v1)
	;
	; 04/11/2013
	; 09/07/2013
	; 'sysumount' anounces to the system that the special file, 
	; indicated as an argument is no longer contain a removable
	; file system. 'getspl' gets the device number of the special
	; file. If no file system was mounted on that device an error
	; occurs. 'mntd' and 'mnti' are cleared and control is passed
	; to 'sysret'.
	;
	; Calling sequence:
	;	sysmount; special
	; Arguments:
	;	special - special file to dismount (device)
	;
	; Inputs: - 
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysumount' system call has one argument; so,
	;	* Single argument, special is pointed to by BX register
	;
	
	;mov 	ax, 1 ; one/single argument, put argument in BX	
	;call	arg
		; jsr r0,arg; u.namep / point u.namep to special
        mov	[u.namep], ebx
	call	getspl
		; jsr r0,getspl / get the device number in r1

	;;;
	; 09/05/2022 - Erdogan Tan
	; (I have added [mnti] check because
	;  retro unix device number of /dev/fd0 is 0
	;  .. so, 'cmp al, [mdev]' is not enough
	;  for dismounting /dev/fd0. sysumount system call would give
	;  wrong cf=0 result while /dev/fd0 is not mounted.)
	cmp	word [mnti], 0
	jna	short sysmnt_err0 ; there is not a mounted device !
	;;;

	cmp	al, [mdev]
		; cmp r1,mntd / is it equal to the last device mounted?
	jne	short sysmnt_err0 ; 'permission denied !' error
	;jne	error
        	; bne errora / no error
	xor	al, al ; ah = 0
sysumnt_0: ;1:
     	; 11/01/2022
	;cmp 	[sb1+1], al ; 0
	;	; tstb sb1+1 / yes, is the device still doing I/O 
	;		   ; / (inhibit bit set)?
	;jna	short sysumnt_1		
	;	; bne 1b / yes, wait
	;call	idle ; (wait for hardware interrupt)
	;jmp	short sysumnt_0
sysumnt_1:        
	; 15/05/2022
	; change user's current directory to mounting directory
	; if it is on the mounted device (chdir back to root fs)
	cmp	byte [u.cdrv], al ; 0
	jna	short sysumnt_4
	;;;
	; 15/05/2022
	; It is needed to change the parent process's current
	; directory because shell runs (/etc/umount) 
	; as child process.
	xor	ebx, ebx 
	mov	bl, [u.uno]
	shl	bl, 1 ; >= 2 .. <= 32
	add	ebx, p.ppid-2
	mov	dx, [ebx] ; process id of the parent [p.ppid]	
	mov	esi, p.pid
	sub	ecx, ecx
	mov	cl, nproc ; 16  
sysumnt_2:	
	lodsw
	cmp	ax, dx
	je	short sysumnt_3
	loop	sysumnt_2
sysumnt_3:
	xor	eax, eax
	sub	esi, p.pid
	shl	esi, 1
	mov	ebx, [esi+p.upage-4] ; the parent's upage
	; ebx points to user (u) structure in upage
	mov	dx, [mnti]
	;mov	[u.cdir], dx
	;mov	[u.cdrv], al ; 0
	mov	[ebx+u.cdir-user], dx
	mov	[ebx+u.cdrv-user], al ; 0
	;;;
sysumnt_4: 
	mov	[mdev], al ; 0
	     	; clr mntd / no, clear these
   	mov	[mnti], ax ; 0
        	; clr mnti

	;; 15/05/2022
	;mov	[cdev], al ; 0 ; [u.cdrv] = 0
	;mov	ax, dx  ; [u.cdir]
	;call	iget

        jmp	sysret
		; br sysreta / return

getspl: ; / get device number from a special file name
	call	namei
	;or	ax, ax ; Retro UNIX 8086 v1 modification !
		       ; ax = 0 -> file not found 	
	;jc	sysmnt_err2 ; 'file not found !' error
	; 11/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	jnc	short getspl_0
	jmp	sysmnt_err2 ; 'file not found !' error
getspl_0:
	;jz	error
	;jc	error
		; jsr r0,namei / get the i-number of the special file
                ; br errora / no such file
        sub	ax, 3 ; Retro UNIX 8086 v1 modification !
		      ;	i-number-3, 0 = fd0, 5 = hd3 
		; sub $4,r1 / i-number-4 rk=1,tap=2+n
        jc	short sysmnt_err0 ; 'permission denied !' error
	;jc	error
		; ble errora / less than 0?  yes, error
        cmp	ax, 5 ;
		; cmp r1,$9. / greater than 9  tap 7
	;ja	short sysmnt_err0 ; 'permission denied !' error
	;;ja	error
		; bgt errora / yes, error
	; 11/01/2022
	jna	short getspl_retn
	; AX = Retro UNIX 8086 v1 Device Number (0 to 5)
;iopen_retn:
;	retn
		; rts r0 / return with device number in r1
sysmnt_err0:
	mov	dword [u.error], ERR_FILE_ACCESS ; permission denied !
	jmp	error

getspl_retn:
	; AX = Retro UNIX 8086 v1 Device Number (0 to 5)
	; 11/01/2022
iopen_retn:
	retn

iopen:	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
	;		(Printer initialization)
	; 11/01/2022
	; 08/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 19/05/2015
	; 18/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 21/05/2013 - 27/08/2013 (Retro UNIX 8086 v1)
	;
	; open file whose i-number is in r1
	; 
	; INPUTS ->
	;    r1 - inode number
	; OUTPUTS ->
	;    file's inode in core	
	;    r1 - inode number (positive)
	;
	; ((AX = R1))
        ; ((Modified registers: edx, ebx, ecx, esi, edi, ebp)) 
	;        
; / open file whose i-number is in r1
	test	ah, 80h ; Bit 15 of AX
		; tst r1 / write or read access?
        jnz	short iopen_2
		; blt 2f / write, go to 2f
	mov	dl, 2 ; read access
	call	access
        	; jsr r0,access; 2 
	; / get inode into core with read access
	; DL=2
iopen_0:
        cmp	ax, 40
		; cmp r1,$40. / is it a special file
        ja	short iopen_retn
		; bgt  3f / no. 3f
	push	eax ; 08/01/2022
	;push	ax
		; mov r1,-(sp) / yes, figure out
	movzx	ebx, al
	shl	bl, 2 ; * 4 ; 08/01/2022
	;shl	bx, 2
		; asl r1
        add     ebx, iopen_1 - 4
	jmp	dword [ebx]
        	; jmp *1f-2(r1) / which one and transfer to it
iopen_1: ; 1:
	dd	otty ; tty, AX = 1 (runix)
 		 ;otty / tty ; r1=2
        	 ;oppt / ppt ; r1=4
	dd	sret ; mem, AX = 2 (runix)
		 ;sret / mem ; r1=6
		 ;sret / rf0
        	 ;sret / rk0
        	 ;sret / tap0
        	 ;sret / tap1
        	 ;sret / tap2
        	 ;sret / tap3
        	 ;sret / tap4
        	 ;sret / tap5
        	 ;sret / tap6
        	 ;sret / tap7
        dd	sret ; fd0, AX = 3 (runix only)
        dd	sret ; fd1, AX = 4 (runix only)
        dd	sret ; hd0, AX = 5 (runix only)
        dd	sret ; hd1, AX = 6 (runix only) 
        dd	sret ; hd2, AX = 7 (runix only)
        dd	sret ; hd3, AX = 8 (runix only) 
	;;dd	error ; lpr, AX = 9 (error !)
        ;dd	sret ; lpr, AX = 9 (runix)
	; 13/06/2022 - (lpt_init)
        dd	ejec ; lpr, AX = 9 (runix)
	dd	ocvt ; tty0, AX = 10 (runix)	  
		 ;ocvt / tty0
	dd	ocvt ; tty1, AX = 11 (runix)	  
		 ;ocvt / tty1
	dd	ocvt ; tty2, AX = 12 (runix)	  
		 ;ocvt / tty2
	dd	ocvt ; tty3, AX = 13 (runix)	  
		 ;ocvt / tty3
	dd	ocvt ; tty4, AX = 14 (runix)	  
		 ;ocvt / tty4
	dd	ocvt ; tty5, AX = 15 (runix)	  
		 ;ocvt / tty5
	dd	ocvt ; tty6, AX = 16 (runix)	  
		 ;ocvt / tty6
	dd	ocvt ; tty7, AX = 17 (runix)	  
		 ;ocvt / tty7
	dd	ocvt ; COM1, AX = 18 (runix only)	  
		 ;error / crd
	dd	ocvt ; COM2, AX = 19 (runix only)

iopen_2: ; 2: / check open write access
	neg	ax
		;neg r1 / make inode number positive
	mov	dl, 1 ; write access
	call	access
		;jsr r0,access; 1 / get inode in core
	; DL=1
	; 11/01/2022
	test	byte [i.flgs+1], 40h
	;test	word [i.flgs], 4000h ; Bit 14 : Directory flag
 		; bit $40000,i.flgs / is it a directory?
	jz	short iopen_0
	;mov	[u.error], ERR_DIR_ACCESS
	;jmp	error ; permission denied !
	jmp	sysmnt_err0
	;;jnz	error		
       		; bne 2f / yes, transfer (error)
        ;;jmp	short iopen_0
	;cmp	ax, 40
		; cmp r1,$40. / no, is it a special file?
        ;ja	short iopen_2
		; bgt 3f / no, return
	;push	ax
		; mov r1,-(sp) / yes
	;movzx	ebx, al
	;shl	bx, 1
		; asl r1
	;add	ebx, ipen_3 - 2
	;jmp	dword [ebx]
		; jmp *1f-2(r1) / figure out 
			; / which special file it is and transfer
;iopen_3: ; 1:
;	dd 	otty ; tty, AX = 1 (runix)
 		 ;otty / tty ; r1=2
        	 ;leadr / ppt ; r1=4
;	dd	sret ; mem, AX = 2 (runix)
		 ;sret / mem ; r1=6
		 ;sret / rf0
        	 ;sret / rk0
        	 ;sret / tap0
        	 ;sret / tap1
        	 ;sret / tap2
        	 ;sret / tap3
        	 ;sret / tap4
        	 ;sret / tap5
        	 ;sret / tap6
        	 ;sret / tap7
;	dd 	sret ; fd0, AX = 3 (runix only)
;	dd 	sret ; fd1, AX = 4 (runix only)
;	dd 	sret ; hd0, AX = 5 (runix only)
;	dd 	sret ; hd1, AX = 6 (runix only)	
;	dd 	sret ; hd2, AX = 7 (runix only)
;	dd 	sret ; hd3, AX = 8 (runix only)	
;	dd	sret ; lpr, AX = 9  (runix)
	;dd	ejec ; lpr, AX = 9  (runix)
;	dd	sret ; tty0, AX = 10 (runix)	  
		 ;ocvt / tty0
;	dd	sret ; tty1, AX = 11 (runix)	  
		 ;ocvt / tty1
;	dd	sret ; tty2, AX = 12 (runix)	  
		 ;ocvt / tty2
;	dd	sret ; tty3, AX = 13 (runix)	  
		 ;ocvt / tty3
;	dd	sret ; tty4, AX = 14 (runix)	  
		 ;ocvt / tty4
;	dd	sret ; tty5, AX = 15 (runix)	  
		 ;ocvt / tty5
;	dd	sret ; tty6, AX = 16 (runix)	  
		 ;ocvt / tty6
;	dd	sret ; tty7, AX = 17 (runix)	  
		 ;ocvt / tty7
;	dd	ocvt ; COM1, AX = 18 (runix only)	  
		 ;/ ejec / lpr
;	dd	ocvt ; COM2, AX = 19 (runix only)

otty: ;/ open console tty for reading or writing
	; 03/03/2022
	; 02/03/2022
	; 26/02/2022
	; 09/02/2022
	; 06/02/2022
	; 08/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 16/11/2015
	; 12/11/2015
	; 18/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 21/05/2013 - 13/07/2014 (Retro UNIX 8086 v1)
	; 16/07/2013
	; Retro UNIX 8086 v1 modification:
	;  If a tty is open for read or write by
	;     a process (u.uno), only same process can open
	;     same tty to write or read (R->R&W or W->W&R).	
	;
	; (INPUT: DL=2 for Read, DL=1 for Write, DL=0 for sysstty)
	;
	movzx	ebx, byte [u.uno] ; process number
	mov	al, [ebx+p.ttyc-1] ; current/console tty
	; 13/01/2014
	;jmp	short ottyp
	; 26/02/2022
	mov	ah, al
	jmp	short ottypc ; ah = al = console tty number
ocvt:
	sub	al, 10
;ottyp:	; (call from sysstty)
	; 08/01/2022
	xor	ebx, ebx
ottyp:	; (ebx < 256) ; 06/02/2022
	; 26/02/2022
	mov	ah, 0FFh
ottypc:
	; 03/03/2022
	; 26/02/2022
	; 09/02/2022
	; 08/01/2022
	; 16/11/2015
	; 12/11/2015
	; 18/05/2015 (32 bit modifications)
	; 06/12/2013 - 13/07/2014
	mov	dh, al ; tty number
	;movzx 	ebx, al ; AL = tty number (0 to 9), AH = 0
	; 08/01/2022
	mov	bl, al
	shl 	bl, 1  ; aligned to word
	; 26/01/2014	
	add 	ebx, ttyl
	mov 	cx, [ebx]
		   ; CL = lock value (0 or process number)
		   ; CH = open count 
	and 	cl, cl
	; 13/01/2014
	;jz 	short otty_ret
	; 08/01/2022
	jz 	short ottys_0
	;
	; 16/11/2015
	cmp 	cl, [u.uno]
	je	short ottys_3
	;
	;
	; 26/02/2022
	; (is it the console tty of the current process?)
	; ((fast check/permit for console tty open function))
	cmp	al, ah ; cmp dh, ah
	je	short ottys_3 ; bypass parent process check
	;
	; 26/02/2022
	;movzx 	ebx, cl ; the process which has locked the tty
	;shl 	bl, 1
	;mov 	ax, [ebx+p.pid-2]
	;;movzx ebx, byte [u.uno]
	;mov	bl, [u.uno]
	;shl 	bl, 1
	;cmp 	ax, [ebx+p.ppid-2]
	;je 	short ottys_3  ; 16/11/2015
	; 26/02/2022 (BugFix) ; *
	movzx 	esi, cl ; the process which has locked the tty
	shl 	esi, 1
	mov 	ax, [esi+p.pid-2]
	xchg	esi, eax
	mov	al, [u.uno]
	shl 	al, 1
	cmp 	si, [eax+p.ppid-2]
	je 	short ottys_3 ; *
	; 26/02/2022
	; check console tty of the process
	; (open permission must be given if the -requested- tty is
	;  console tty of current process)
	shr	al, 1
	cmp	[eax+p.ttyc-1], dh ; console tty ?
	je 	short ottys_3
	;
	; the tty is locked by another process
	; except the parent process (p.ppid)
        ;
	; 09/02/2022
	;mov	dword [u.error], ERR_DEV_ACCESS
	;		; permission denied ! error
otty_err: ; 13/01/2014
	;or 	dl, dl	; DL = 0 -> called by sysstty
	;;jnz	error
	; 05/12/2021
	;jz	short otty_stc_retn
	;jmp	error
	; 09/02/2022
	cmp	dl, 1 ; dl = 0 ?
	jb	short otty_retn ; yes, cf=1, called by sysstty
	; iopen (dl=1 or dl=2)
	mov     dword [u.error], ERR_DEV_ACCESS
			; permission denied ! error
	jmp	error
;otty_stc_retn:
	;stc
	;retn
ottys_0:
	; 08/01/2022
otty_ret: 
	; 13/01/2014
	cmp 	dh, 7
	jna	short ottys_2
	; 16/11/2015
com_port_check:
	mov	esi, com1p
	cmp	dh, 8	; COM1 (tty8) ?
	jna	short ottys_1 ; yes, it is COM1
	inc	esi	; no, it is COM2 (tty9)
ottys_1:
	; 12/11/2015
	cmp	byte [esi], 0 ; E3h (or 23h)
	ja	short com_port_ready
	;
	; 09/02/2022
	cmp	dl, 1 ; dl = 0 ?
	jb	short otty_retn ; yes, cf=1, called by sysstty
        mov     dword [u.error], ERR_DEV_NOT_RDY
			   ; device not ready ! error
	;jmp	short otty_err
	jmp	error
com_port_ready:
ottys_2:
	; 02/03/2022
	;or	cl, cl  ; cl = lock/owner, ch = open count
	;jnz	short ottys_3
	mov	cl, [u.uno]
ottys_3:
	inc 	ch
	mov 	[ebx], cx ; set tty lock again
	; 06/12/2013
	inc	dh ; tty number + 1
	mov	ebx, u.ttyp
	; 13/01/2014
	test	dl, 2 ; open for read sign
	jnz	short ottys_4
	inc	ebx
ottys_4:
	; Set 'u.ttyp' ('the recent TTY') value
	mov 	[ebx], dh ; tty number + 1
	; 09/02/2022
	or	dl, dl ; sysstty system call check (DL=0)
	jz	short otty_retn ; 03/03/2022
sret:
	;pop 	ax
	pop	eax ; 08/01/2022
otty_retn:	; 09/02/2022
iclose_retn:	
	retn

	;
	; Original UNIX v1 'otty' routine:
	;	
	;mov    $100,*$tks / set interrupt enable bit (zero others) in
        ;                 / reader status reg
        ;mov    $100,*$tps / set interrupt enable bit (zero others) in
        ;                 / punch status reg
        ;mov    tty+[ntty*8]-8+6,r5 / r5 points to the header of the
        ;                          / console tty buffer
        ;incb   (r5) / increment the count of processes that opened the
        ;            / console tty
        ;tst    u.ttyp / is there a process control tty (i.e., has a tty
        ;             / buffer header
        ;bne    sret / address been loaded into u.ttyp yet)?  yes, branch
        ;mov    r5,u.ttyp / no, make the console tty the process control
        ;                 / tty
        ;br     sret / ?
;sret:
		;clr *$ps / set processor priority to zero
;	pop	ax
        	;mov (sp)+,r1 / pop stack to r1
;3:
;	retn
        	;rts r0
	
;ocvt:	; < open tty >
	; 13/01/2014
	; 06/12/2013 (major modification: p.ttyc, u.ttyp)
	; 24/09/2013 consistency check -> ok
	; 16/09/2013
	; 03/09/2013
	; 27/08/2013
	; 16/08/2013
	; 16/07/2013
	; 27/05/2013
	; 21/05/2013
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; In original UNIX v1, 'ocvt' routine 
	;	(exactly different than this one)
	;	was in 'u9.s' file.
	;
	; 16/07/2013
	; Retro UNIX 8086 v1 modification:
	;  If a tty is open for read or write by
	;     a process (u.uno), only same process can open
	;     same tty to write or read (R->R&W or W->W&R).	
	;
	; INPUT: DL=2 for Read DL=1 for Write

	; 16/09/2013
	; sub 	al, 10
	
	; 06/12/2013
	;cmp	al, 7
        ;jna     short ottyp
	; 13/01/2014
	;jmp	short ottyp

;oppt: / open paper tape for reading or writing
;        mov    $100,*$prs / set reader interrupt enable bit
;        tstb   pptiflg / is file already open
;        bne    2f / yes, branch
;1:
;        mov    $240,*$ps / no, set processor priority to 5
;        jsr    r0,getc; 2 / remove all entries in clist
;               br .+4 / for paper tape input and place in free list
;        br     1b
;        movb   $2,pptiflg / set pptiflg to indicate file just open
;        movb   $10.,toutt+1 / place 10 in paper tape input tout entry
;        br     sret
;2:
;        jmp    error / file already open

iclose:
	; 08/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 19/05/2015
	; 18/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 21/05/2013 - 13/01/2014 (Retro UNIX 8086 v1)
	;
	; close file whose i-number is in r1
	; 
	; INPUTS ->
	;    r1 - inode number
	; OUTPUTS ->
	;    file's inode in core	
	;    r1 - inode number (positive)
	;
	; ((AX = R1))
        ;    ((Modified registers: -ebx-, edx)) 
	;        
;/ close file whose i-number is in r1
	mov	dl, 2 ; 12/01/2014
	test	ah, 80h ; Bit 15 of AX
		; tst r1 / test i-number
        ;jnz	short iclose_2
		; blt 2f / if neg., branch
	jz	short iclose_0 ; 30/07/2013
	; 16/07/2013 
	neg	ax ; make it positive
	; 12/01/2014
	dec	dl ; dl = 1 (open for write)
iclose_0:
	cmp	ax, 40
		; cmp r1,$40. / is it a special file
        ja	short iclose_retn  ; 13/01/2014
		; bgt 3b / no, return
	; 12/01/2014
	; DL=2 -> special file was opened for reading
	; DL=1 -> special file was opened for writing
	push	eax ; 08/01/2022
	;push	ax
		; mov r1,-(sp) / yes, save r1 on stack
	movzx	ebx, al
	; 08/01/2022
	shl	bl, 2
	;shl	bx, 2
		; asl r1
	add	ebx, iclose_1 - 4
	jmp	dword [ebx]
		; jmp *1f-2(r1) / compute jump address and transfer
iclose_1 :
	dd	ctty ; tty, AX = 1 (runix)
	dd	cret ; mem, AX = 2 (runix)
	dd	cret ; fd0, AX = 3 (runix only)
	dd	cret ; fd1, AX = 4 (runix only)
	dd	cret ; hd0, AX = 5 (runix only)
	dd	cret ; hd1, AX = 6 (runix only)	
	dd	cret ; hd2, AX = 7 (runix only)
	dd	cret ; hd3, AX = 8 (runix only)	
	dd	cret ; lpr, AX = 9 (runix)
	;dd	error; lpr, AX = 9 (error !)
	;;dd	offset ejec ;;lpr, AX = 9  
	dd	ccvt ; tty0, AX = 10 (runix)	  
	dd	ccvt ; tty1, AX = 11 (runix)	  
	dd	ccvt ; tty2, AX = 12 (runix)	  
	dd	ccvt ; tty3, AX = 13 (runix)	  
	dd	ccvt ; tty4, AX = 14 (runix)	  
	dd	ccvt ; tty5, AX = 15 (runix)	  
	dd	ccvt ; tty6, AX = 16 (runix)	  
	dd	ccvt ; tty7, AX = 17 (runix)	  
	dd	ccvt ; COM1, AX = 18 (runix only)	  
	dd	ccvt ; COM2, AX = 19 (runix only)

	; 1:
	;        ctty   / tty
	;        cppt   / ppt
	;        sret   / mem
	;        sret   / rf0
	;        sret   / rk0
	;        sret   / tap0
	;        sret   / tap1
	;        sret   / tap2
	;        sret   / tap3
	;        sret   / tap4
	;        sret   / tap5
	;        sret   / tap6
	;        sret   / tap7
	;        ccvt   / tty0
	;        ccvt   / tty1
	;        ccvt   / tty2
	;        ccvt   / tty3
	;        ccvt   / tty4
	;        ccvt   / tty5
	;        ccvt   / tty6
	;        ccvt   / tty7
	;        error / crd

;iclose_2: ; 2: / negative i-number
	;neg	ax
		;neg r1 / make it positive
	;cmp	ax, 40
		;cmp r1,$40. / is it a special file?
        ;ja	short @b
		;bgt    3b / no. return
	;push	ax
		;mov r1,-(sp)
	;movzx	ebx, al
	;shl	bx, 1
		;asl r1 / yes. compute jump address and transfer
	;add	ebx, iclose_3 - 2
	;jmp	dword [ebx]
		;jmp *1f-2(r1) / figure out 
;iclose_3:
	;dd	ctty ; tty, AX = 1 (runix)
	;dd	sret ; mem, AX = 2 (runix)
	;dd	sret ; fd0, AX = 3 (runix only)
	;dd	sret ; fd1, AX = 4 (runix only)
	;dd	sret ; hd0, AX = 5 (runix only)
	;dd	sret ; hd1, AX = 6 (runix only)	
	;dd	sret ; hd2, AX = 7 (runix only)
	;dd	sret ; hd3, AX = 8 (runix only)
	 ;dd	sret ; lpr, AX = 9	
	;dd	ejec ; lpr, AX = 9  (runix)
	;dd	ccvt ; tty0, AX = 10 (runix)	  
	;dd	ccvt ; tty1, AX = 11 (runix)	  
	;dd	ccvt ; tty2, AX = 12 (runix)	  
	;dd	ccvt ; tty3, AX = 13 (runix)	  
	;dd	ccvt ; tty4, AX = 14 (runix)	  
	;dd	ccvt ; tty5, AX = 15 (runix)	  
	;dd	ccvt ; tty6, AX = 16 (runix)	  
	;dd	ccvt ; tty7, AX = 17 (runix)	  
	;dd	ccvt ; COM1, AX = 18 (runix only)	  
	;dd	ccvt ; COM2, AX = 19 (runix only) 
	
	;1:
	;      	ctty   / tty
	;       leadr  / ppt
	;       sret   / mem
	;       sret   / rf0
	;       sret   / rk0
	;       sret   / tap0
	;       sret   / tap1
	;       sret   / tap2
	;       sret   / tap3
	;       sret   / tap4
	;       sret   / tap5
	;       sret   / tap6
	;       sret   / tap7
	;       ccvt   / tty0
	;       ccvt   / tty1
	;       ccvt   / tty2
	;       ccvt   / tty3
	;       ccvt   / tty4
	;       ccvt   / tty5
	;       ccvt   / tty6
	;       ccvt   / tty7
	;/      ejec   / lpr

ctty: ; / close console tty
	; 26/02/2022
	; 09/02/2022
	; 06/02/2022
	; 08/01/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.2)
	; 18/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 21/05/2013 - 26/01/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; (DL = 2 -> it is open for reading)
	; (DL = 1 -> it is open for writing)
	; (DL = 0 -> it is open for sysstty system call)
	;
	; 06/12/2013
        movzx   ebx, byte [u.uno] ; process number
        mov     al, [ebx+p.ttyc-1]
	; 13/01/2014
	;jmp	short cttyp
	; 06/02/2022
	jmp	short ctty_0
ccvt:
	sub 	al, 10
cttyp:	; (call from sysstty)
	; 08/01/2022
	xor	ebx, ebx
ctty_0: ; (ebx < 256)  ; 06/02/2022
	; 08/01/2022
	; 18/05/2015 (32 bit modifications)
	; 16/08/2013 - 26/01/2014
	;movzx 	ebx, al ; tty number (0 to 9)
	; 08/01/2022
	mov	bl, al
	shl 	bl, 1  ; aligned to word	
	; 26/01/2014
	add 	ebx, ttyl
	mov 	dh, al ; tty number
	mov 	ax, [ebx]
		   ; AL = lock value (0 or process number)
		   ; AH = open count 
	and 	ah, ah
	jnz	short ctty_ret
  	; 09/02/2022
	cmp	dl, 1	; DL = 0 -> called by sysstty
	jb	short ctty_stc_retn ; cf=1
	; iclose (dl=1 or dl=2)
        mov     dword [u.error], ERR_DEV_NOT_OPEN
			; device not open ! error
	;jmp 	short ctty_err ; open count = 0, it is not open !
	jmp	error
	; 26/01/2014
ctty_ret:
	dec 	ah ; decrease open count
	jnz	short ctty_1
	xor	al, al ; unlock/free tty
ctty_1:
	mov 	[ebx], ax ; close tty instance
	;
	mov	ebx, u.ttyp
	;test	dl, 1 ; open for write sign
	;jz	short ctty_2
	; 26/02/2022
	test	dl, 2 ; open for read sign
	jnz	short ctty_2
	inc	ebx
ctty_2:
	inc	dh ; tty number + 1
	cmp	dh, [ebx]
	;jne	short cret
	jne	short ctty_3 ; 09/02/2022
	; Reset/Clear 'u.ttyp' ('the recent TTY') value
	mov	byte [ebx], 0
ctty_3:
	; 09/02/2022
	or	dl, dl ; sysstty system call check (DL=0)
	jz	short ctty_4
cret:
	;pop	ax
	; 08/01/2022
	pop	eax
ctty_stc_retn:	; 09/02/2022
ctty_4:
	retn

;ctty_err: ; 13/01/2014
;	or 	dl, dl ; DL = 0 -> called by sysstty
;	jnz	error
;	stc
;	retn

	; Original UNIX v1 'ctty' routine:
	;	
        ;mov    tty+[ntty*8]-8+6,r5 
	;		;/ point r5 to the console tty buffer
        ;decb   (r5) / dec number of processes using console tty
        ;br     sret / return via sret

;ccvt:	; < close tty >
	; 21/05/2013 - 13/01/2014 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification !
	; 
	; In original UNIX v1, 'ccvt' routine 
	;		(exactly different than this one)
	;	was in 'u9.s' file.
	;
	; DL = 2 -> it is open for reading
	; DL = 1 -> it is open for writing
	;
	; 17/09/2013
	;sub 	al, 10
	;cmp	al, 7
	;jna	short cttyp
	; 13/01/2014
	;jmp	short cttyp

;cppt: / close paper tape
;        clrb   pptiflg / set pptiflg to indicate file not open
;1:
;        mov    $240,*$ps /set process or priority to 5
;        jsr    r0,getc; 2 / remove all ppt input entries from clist
;                          / and assign to free list
;               br sret
;        br     1b

;ejec:	
;	jmp	error
;/ejec:
;/       mov    $100,*$lps / set line printer interrupt enable bit
;/       mov    $14,r1 / 'form feed' character in r1 (new page).
;/       jsr    r0,lptoc / space the printer to a new page
;/       br     sret / return to caller via 'sret'

ejec:
	; 13/06/2022 - Retro UNIX 386 v1.1 (Kernel v0.2.1.4)
	;	     - Printer Initialization
lpt_init:	
	; Ref: IBM PC-AT BIOS v3 - PRT.ASM - 15/11/1985
	;
	; Default printer port: 378h ; LPT1
	
 	mov	ah, 1  ; INITIALIZE THE PRINTER PORT
	;call	int17h
	call	PRNOP
	jz	short lpt_init_ok
	
	; replace error code with 'device not ready' error
	; (this may be better for 'sysopen')
	mov	eax, ERR_PRN_NOT_RDY
	mov	[u.error], eax
	;jmp	sysret ; (may be) ? ([u.r0] = file descriptor)
	mov	[u.r0], eax
	jmp	error ; (better)	
	
lpt_init_ok:
	;jmp	short cret
	pop	eax    ; inode number
	retn
	