; Temporary Runix kernel v2.0 file for debug - 22/11/2021
; (re-write kernel for test by using previous version without a major defect)
; ****************************************************************************
; Retro UNIX 386 v1 Kernel (v0.2.2.3) - SYS2.INC
; Last Modification: 18/07/2022
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Retro UNIX 8086 v1 - U2.ASM (24/03/2014) //// UNIX v1 -> u2.s
;
; ****************************************************************************

syslink:
	; 12/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/06/2013 (Retro UNIX 8086 v1)
	;
	; 'syslink' is given two arguments, name 1 and name 2.
	; name 1 is a file that already exists. name 2 is the name
	; given to the entry that will go in the current directory.
	; name2 will then be a link to the name 1 file. The i-number
	; in the name 2 entry of current directory is the same
	; i-number for the name 1 file.
	;
	; Calling sequence:
	;	syslink; name 1; name 2
	; Arguments:
	;	name 1 - file name to which link will be created.
	;	name 2 - name of entry in current directory that
	;		 links to name 1.
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'syslink' system call has two arguments; so,
	;	* 1st argument, name 1 is pointed to by BX register
	;	* 2nd argument, name 2 is pointed to by CX register
	;
		; / name1, name2
		;jsr r0,arg2 / u.namep has 1st arg u.off has 2nd
	mov	[u.namep], ebx
	push	ecx
	call	namei
		; jsr r0,namei / find the i-number associated with
			     ; / the 1st path name
     	;;and	ax, ax
	;;jz	error ; File not found
	;jc	error 
		; br error9 / cannot be found
	jnc	short syslink0
	;pop 	ecx
	; 'file not found !' error
	mov	dword [u.error], ERR_FILE_NOT_FOUND ; 12
	jmp	error
syslink0:
	call	iget
		; jsr r0,iget / get the i-node into core
	pop	dword [u.namep] ; ecx
		; mov (sp)+,u.namep / u.namep points to 2nd name
	; 24/12/2021
	push	eax ; *
	;push	ax
		; mov r1,-(sp) / put i-number of name1 on the stack
			    ; / (a link to this file is to be created)
	; 24/12/2021
	mov	cl, [cdev]
	push	ecx ; **
	;push	word [cdev]
		; mov cdev,-(sp) / put i-nodes device on the stack
	call	isdir
		; jsr r0,isdir / is it a directory
	call	namei
		; jsr r0,namei / no, get i-number of name2
	;jnc	error
		; br .+4   / not found 
			 ; / so r1 = i-number of current directory
			 ; / ii = i-number of current directory
		; br error9 / file already exists., error
	jc	short syslink1
	; pop eax ; 24/12/2021
	; pop eax
	; 'file exists !' error
	mov	dword [u.error], ERR_FILE_EXISTS ; 14
	jmp	error
syslink1:
	;pop	cx
	; 24/12/2021
	pop	ecx ; **
	;cmp	cx, [cdev]
	cmp	cl, [cdev]
	;jne	error
		; cmp (sp)+,cdev / u.dirp now points to 
			       ; / end of current directory
	        ; bne error9
	je	short syslink2
	; 'not same drive !' error
	mov	dword [u.error],  ERR_DRV_NOT_SAME ; 21
	jmp	error
syslink2:
	;pop	eax ; 24/12/2021
	;push	eax
	; 24/12/2021
	mov	eax, [esp] ; *
	mov	[u.dirbuf], ax
		; mov (sp),u.dirbuf / i-number of name1 into u.dirbuf
	call	mkdir
		; jsr r0,mkdir / make directory entry for name2 
		 	     ; / in current directory
	; 24/12/2021
	pop	eax ; *
	;pop	ax
		; mov (sp)+,r1 / r1 has i-number of name1
	call	iget
		; jsr r0,iget / get i-node into core
	inc	byte [i.nlks]
		; incb i.nlks / add 1 to its number of links
sysunlink_2:
	; 12/01/2022 - Retro UNIX 386 v1.2
	mov	byte [imodx], 1	; (flag means file data is same
				;  but inode's itself has been modified)
	call	setimod
		; jsr r0,setimod / set the i-node modified flag
	jmp	sysret

sysunlink:
	; 12/01/2022
	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 04/12/2015 (14 byte file names)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/06/2013 (Retro UNIX 8086 v1)
	;
	; 'sysunlink' removes the entry for the file pointed to by
	; name from its directory. If this entry was the last link
	; to the file, the contents of the file are freed and the
	; file is destroyed. If, however, the file was open in any
	; process, the actual destruction is delayed until it is 
	; closed, even though the directory entry has disappeared.
	; 
	; The error bit (e-bit) is set to indicate that the file	
	; does not exist or that its directory can not be written.
	; Write permission is not required on the file itself.
	; It is also illegal to unlink a directory (except for
	; the superuser).
	;
	; Calling sequence:
	;	sysunlink; name
	; Arguments:
	;	name - name of directory entry to be removed 
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification:
	;	 The user/application program puts address of the name
	;        in BX register as 'sysunlink' system call argument.

	; / name - remove link name
	mov	[u.namep], ebx
		;jsr r0,arg; u.namep / u.namep points to name
	call	namei
		; jsr r0,namei / find the i-number associated 
			     ; / with the path name
	;jc	error
		; br error9 / not found
	jnc	short sysunlink1
	; 'file not found !' error
	mov	dword [u.error], ERR_FILE_NOT_FOUND ; 12
	jmp	error
sysunlink1:
	push	eax ; 24/12/2021
	;push	ax
		; mov r1,-(sp) / put its i-number on the stack
	call	isdir
		; jsr r0,isdir / is it a directory
	;xor 	ax, ax
	; 24/12/2021
	xor	eax, eax
	mov	[u.dirbuf], ax ; 0
		; clr u.dirbuf / no, clear the location that will
			   ; / get written into the i-number portion
			 ; / of the entry
	sub	dword [u.off], 16 ; 04/12/2015 (10 -> 16) 
		; sub $10.,u.off / move u.off back 1 directory entry
	call	wdir
		; jsr r0,wdir / free the directory entry
	pop	eax ; 24/12/2021
	;pop	ax
		; mov (sp)+,r1 / get i-number back
	call	iget
		; jsr r0,iget / get i-node
	; 12/01/2022 - Retro UNIX 386 v1.2
	;call	setimod
	;	; jsr r0,setimod / set modified flag
	;	
	dec	byte [i.nlks]
		; decb i.nlks / decrement the number of links
	; 24/12/2021
	jnz	short sysunlink_2
	;jnz	sysret
		; bgt sysret9 / if this was not the last link
			    ; / to file return
	; 12/01/2022 - Retro UNIX 386 v1.2
	call	setimod
		; jsr r0,setimod / set modified flag
	; AX = r1 = i-number
	call	anyi
		; jsr r0,anyi / if it was, see if anyone has it open.
			 ; / Then free contents of file and destroy it.
	jmp	sysret
		; br sysret9
;sysunlink_2:
	;; 12/01/2022 - Retro UNIX 386 v1.2
	;mov	byte [imodx], 1	; (flag means file data is same
	;			;  but inode's itself has been modified)
	;call	setimod
	;	; jsr r0,setimod / set the i-node modified flag
	;jmp	sysret

isdir:
	; 13/03/2022
	; 08/01/2022
	; 01/01/2022 - Retro UNIX 386 v1.2 (runix v2 fs inode)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 04/05/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'isdir' check to see if the i-node whose i-number is in r1
	;  is a directory. If it is, an error occurs, because 'isdir'
	;  called by syslink and sysunlink to make sure directories
	;  are not linked. If the user is the super user (u.uid=0),
	; 'isdir' does not bother checking. The current i-node
	;  is not disturbed.			
	;		
	; INPUTS ->
	;    r1 - contains the i-number whose i-node is being checked.
	;    u.uid - user id
	; OUTPUTS ->
	;    r1 - contains current i-number upon exit
	;    	 (current i-node back in core) 
	;	
	; ((AX = R1))
	;
        ; ((Modified registers: eAX, eDX, eBX, eCX, eSI, eDI, eBP))  
	;

	; / if the i-node whose i-number is in r1 is a directory 
	; / there is an error unless super user made the call
	
	; 01/01/2022
	cmp	word [u.uid], 0 ; 16 bit uid (runix v2 fs inode)
	;cmp	byte [u.uid], 0 
		; tstb u.uid / super user
	jna	short isdir1
		; beq 1f / yes, don't care
	; 08/01/2022
	push	dword [ii]
	;push	word [ii]
		; mov ii,-(sp) / put current i-number on stack
	call	iget
		; jsr r0,iget / get i-node into core (i-number in r1)
	; 01/01/2022 (runix v2 fs inode flags)
	test	byte [i.flgs+1], 80h ; regular file ?
	jz	short isdir2 ; no, error!
	test	byte [i.flgs+1], 40h ; directory flag
	;test 	word [i.flgs], 4000h ; Bit 14 : Directory flag
		; bit $40000,i.flgs / is it a directory
	;jnz	error
		; bne error9 / yes, error
	jz	short isdir0

	; 13/03/2022
	; NOTE:
	; Unix v5-v7 kernels do not allow (ordinary) users
	; (except root/superuser) --if [u.uid] > 0--
	; to link a directory. (ref: sys2.c, 'link')
	;
	; But, Retro UNIX 386 v1.2 will allow the owner of
	; the directory to link it.

	mov	ax, [i.uid] ; owner ID of the parent dir
	cmp	ax, [u.uid] ; user ID of current user/process
	jne	short isdir2
	; 13/03/2022
	; additional checking (may or may not be necessary!?)
	cmp	ax, [u.ruid] 
			; real (login) user ID must be same
	je	short isdir0

isdir2:
	mov 	dword [u.error], ERR_NOT_FILE  ; 11 ; ERR_DIR_ACCESS 
				; 'permission denied !' error
	;pop	ax
	jmp	error	
isdir0:	
	; 08/01/2022
	pop	eax
	;pop	ax
		; mov (sp)+,r1 / no, put current i-number in r1 (ii)
	call	iget
		; jsr r0,iget / get it back in
isdir1: ; 1:
	retn
		; rts r0

mkdir:
	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 04/12/2015 (14 byte directory names)
	; 12/10/2015
	; 17/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 29/04/2013 - 01/08/2013 (Retro UNIX 8086 v1)
	;
	; 'mkdir' makes a directory entry from the name pointed to
	; by u.namep into the current directory.
	;
	; INPUTS ->
	;    u.namep - points to a file name 
	;	           that is about to be a directory entry.
	;    ii - current directory's i-number.	
	; OUTPUTS ->
	;    u.dirbuf+2 - u.dirbuf+10 - contains file name. 
	;    u.off - points to entry to be filled 
	;	     in the current directory		
	;    u.base - points to start of u.dirbuf.
	;    r1 - contains i-number of current directory 
	;	
	; ((AX = R1)) output
	;
	;    (Retro UNIX Prototype : 11/11/2012, UNIXCOPY.ASM)
        ;    ((Modified registers: eAX, eDX, eBX, eCX, eSI, eDI, eBP))  
	;

	; 17/06/2015 - 32 bit modifications (Retro UNIX 386 v1)
	xor 	eax, eax
	mov     edi, u.dirbuf+2
	mov	esi, edi
	stosd
	stosd
	; 04/12/2015 (14 byte directory names)
	stosd
	stosw
		; jsr r0,copyz; u.dirbuf+2; u.dirbuf+10. / clear this
	mov	edi, esi ; offset to u.dirbuf
	; 12/10/2015 ([u.namep] -> ebp)
	;mov 	ebp, [u.namep]
	call	trans_addr_nmbp ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page (1 - 4096)
	; edi = offset to u.dirbuf (edi is not modified in trans_addr_nm)
		; mov u.namep,r2 / r2 points to name of directory entry
		; mov $u.dirbuf+2,r3 / r3 points to u.dirbuf+2
mkdir_1: ; 1: 
	inc	ebp ; 12/10/2015
	;
	; / put characters in the directory name in u.dirbuf+2 - u.dirbuf+10
	; 01/08/2013
	lodsb
		; movb (r2)+,r1 / move character in name to r1
	and 	al, al
	jz 	short mkdir_3 	  
		; beq 1f / if null, done
	cmp	al, '/'
		; cmp r1,$'/ / is it a "/"?
	je	short mkdir_err
	;je	error
		; beq error9 / yes, error
	; 12/10/2015
	;dec	cx
	dec	ecx ; 24/12/2021
	jnz	short mkdir_2
	; 12/10/2015 ([u.namep] -> ebp)
	call	trans_addr_nm ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page
	; edi = offset to u.dirbuf (edi is not modified in trans_addr_nm)
mkdir_2:
	cmp     edi, u.dirbuf+16 ; ; 04/12/2015 (10 -> 16) 
		; cmp r3,$u.dirbuf+10. / have we reached the last slot for
				     ; / a char?
	je	short mkdir_1
		; beq 1b / yes, go back
	stosb
		; movb r1,(r3)+ / no, put the char in the u.dirbuf
	jmp 	short mkdir_1
		; br 1b / get next char
mkdir_err:
	; 17/06/2015
	mov	dword [u.error], ERR_NOT_DIR ; 'not a valid directory !'
	jmp	error

mkdir_3: ; 1:
	mov	eax, [u.dirp]
	mov	[u.off], eax
		; mov u.dirp,u.off / pointer to empty current directory
				 ; / slot to u.off
	; 08/01/2022
wdir:	; 24/12/2021 (Retro UNIX 386 v1.2)
	; 29/04/2013
        mov     dword [u.base], u.dirbuf
		; mov $u.dirbuf,u.base / u.base points to created file name
        mov     dword [u.count], 16 ; 04/12/2015 (10 -> 16) 
		; mov $10.,u.count / u.count = 10
	; 08/01/2022
	mov	eax, [ii]
	;mov	ax, [ii] 
		; mov ii,r1 / r1 has i-number of current directory
	;mov	dl, 1 ; owner flag mask ; RETRO UNIX 8086 v1 modification !
	; 08/01/2022 (Retro UNIX 386 v2 file system inode)
	mov	dx, 80h	; mov dx, IWRITE
	call 	access
		; jsr r0,access; 1 / get i-node and set its file up 
				 ; / for writing
	; AX = i-number of current directory
	; 01/08/2013
	inc     byte [u.kcall] ; the caller is 'mkdir' sign	
	;call	writei
	;	; jsr r0,writei / write into directory
	;retn	
	;	; rts r0
	; 24/12/2021
	jmp	writei


	; 09/05/2022 
	;	(Retro UNIX 386 v1.2, Kernel v0.2.2.1)
	; 06/02/2022
	; 12/01/2022
	; 01/01/2022
	; 24/12/2021
	; 11/12/2021
	; 04/12/2021
	; 30/11/2021
	; 28/11/2021 - Retro UNIX 386 v1.2
	;	(Retro UNIX 386 v2 fs compatibility modification)
sysexec:
	; 02/05/2021
	; 27/03/2021
	; 26/03/2021
	; 25/03/2021 (Retro UNIX 386 v2 - Beginning)
	; 23/10/2015
	; 10/10/2015, 18/10/2015, 19/10/2015
	; 29/07/2015, 05/08/2015, 05/08/2015
	; 21/07/2015, 24/07/2015, 25/07/2015
	; 01/07/2015, 02/07/2015, 20/07,2015
	; 24/06/2015, 25/06/2021
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 03/06/2013 - 06/12/2013 (Retro UNIX 8086 v1)
	;
	; 'sysexec' initiates execution of a file whose path name if
	; pointed to by 'name' in the sysexec call. 
	; 'sysexec' performs the following operations:
	;    1. obtains i-number of file to be executed via 'namei'.
	;    2. obtains i-node of file to be executed via 'iget'.
	;    3. sets trap vectors to system routines.
	;    4. loads arguments to be passed to executing file into
	;	highest locations of user's core
	;    5. puts pointers to arguments in locations immediately
	;	following arguments.
	;    6.	saves number of arguments in next location.
	;    7. initializes user's stack area so that all registers
	;	will be zeroed and the PS is cleared and the PC set
	;	to core when 'sysret' restores registers 
	;	and does an rti.
	;    8. inializes u.r0 and u.sp
	;    9. zeros user's core down to u.r0
	;   10.	reads executable file from storage device into core
	;	starting at location 'core'.
	;   11.	sets u.break to point to end of user's code with
	;	data area appended.
	;   12.	calls 'sysret' which returns control at location
	;	'core' via 'rti' instruction.
	;
	; Calling sequence:
	;	sysexec; namep; argp
	; Arguments:
	;	namep - points to pathname of file to be executed
	;	argp  - address of table of argument pointers
	;	argp1... argpn - table of argument pointers
	;	argp1:<...0> ... argpn:<...0> - argument strings
	; Inputs: (arguments)
	; Outputs: -	
	; ...............................................................
	;
	; Retro UNIX 386 v1 modification: 
	;	User application runs in it's own virtual space 
	;	which is izolated from kernel memory (and other
	;	memory pages) via 80386	paging in ring 3 
	;	privilige mode. Virtual start address is always 0.
	;	User's core memory starts at linear address 400000h
	;	(the end of the 1st 4MB).
	;
	; Retro UNIX 8086 v1 modification: 
	;	user/application segment and system/kernel segment
	;	are different and sysenter/sysret/sysrele routines
	;	are different (user's registers are saved to 
	;	and then restored from system's stack.)
	;
	;	NOTE: Retro UNIX 8086 v1 'arg2' routine gets these
	;	      arguments which were in these registers;
	;	      but, it returns by putting the 1st argument
	;	      in 'u.namep' and the 2nd argument
	;	      on top of stack. (1st argument is offset of the
	;	      file/path name in the user's program segment.)
	
	;call	arg2
	; * name - 'u.namep' points to address of file/path name
	;          in the user's program segment ('u.segmnt')
	;          with offset in BX register (as sysopen argument 1).
	; * argp - sysexec argument 2 is in CX register 
	;          which is on top of stack.
	;
		; jsr r0,arg2 / arg0 in u.namep,arg1 on top of stack

	; 23/06/2015 (32 bit modifications)

	mov	[u.namep], ebx ; argument 1
        ; 18/10/2015
	mov     [argv], ecx  ; * ; argument 2
	call	namei
		; jsr r0,namei / namei returns i-number of file 
			     ; / named in sysexec call in r1
	;jc	error
		; br error9
	jnc	short sysexec_0
	;
	; 'file not found !' error
	mov	dword [u.error], ERR_FILE_NOT_FOUND
	jmp	error

	; 26/03/2021 - Retro UNIX 386 v2
	; (executable file flag is checked in 'access')
	; (following error message is in 'access' subroutine)
;sysexec_not_exf:
	;; 'not executable file !' error
	;mov	dword [u.error], ERR_NOT_EXECUTABLE
	;jmp	error 
sysexec_0:
	; 26/03/2021 ('iget' will be called in 'access')
	;call	iget
	;	; jsr r0,iget / get i-node for file to be executed
	;
	;test	byte [i.flgs], 10h
	;;test	word [i.flgs], 10h
	;	; bit $20,i.flgs / is file executable
	;jz	short sysexec_not_exf
	;;jz	error
	;	; beq error9
	; 26/03/2021
	mov	dx, 40h ; IEXEC - execute, owner
	call	access
	; ! we are here because there is execute permission !
	; ('iopen' is not needed for regular files, from now on)

	; 26/03/2021
	;; 25/03/2021
	;; (ref: Retro UNIX 386 v2, 'ux.s')
	;mov	dx, 40h ; IEXEC - execute, owner
	;;
	;call	iopen
	;	; jsr r0,iopen / gets i-node for file with i-number
	;		     ; / given in r1 (opens file)
	
	; 26/03/2021
	; AX = i-number of the file
	;test	byte [i.flgs], 20h
	;;test	word [i.flgs], 20h
	;	; bit $40,i.flgs / test user id on execution bit
	;jz	short sysexec_1
	;	; beq 1f
	;cmp 	byte [u.uid], 0 ; 02/08/2013
	;	; tstb u.uid / test user id
	;jna	short sysexec_1
	;	; beq 1f / super user

	;mov	cl, [i.uid]
	;mov	[u.uid], cl ; 02/08/2013
	;	; movb i.uid,u.uid / put user id of owner of file
				 ; / as process user id
	; 26/03/2021 - Retro UNIX 386 v2
	;mov	al, [i.flgs+1]
	cmp	word [u.uid], 0	; super user (root) ?
	;jna	short sysexec_19 ; yes, super user
	;; 02/05/2021
	jna	short sysexec_1	; don't set UID or GID

	; 01/01/2022 - Retro UNIX 386 v1.2
	mov	al, [i.flgs+1]

	; test set user id on execution bit 
	test	al, 08h ; ISUID flag (800h)
	;jz	short sysexec_19
	; 02/05/2021
	jz	short sysexec_1 ; do not set group ID
				; (if user ID will not be set)
				; (02/05/2021)	
	; set user id on exec (800h)
	; 26/03/2021
sysexec_setuid:
	;; 27/03/2021
	;cmp	word [u.uid], 0
	;;jna	short sysexec_19 ; super user
	;; 02/05/2021
	;jna	short sysexec_1 ; do not set group ID
	;			; (if user ID will not be set)
	;			; (02/05/2021)
	mov	cx, [i.uid]
	mov	[u.uid], cx
sysexec_19:
	; test set group id on execution bit 
	test	al, 04h ; ISGID flag (400h)
	jz	short sysexec_1
sysexec_setgid:
	; set group id on exec (400h)
	mov	al, [i.gid]
	mov	[u.gid], al
	;
sysexec_1:
	; 06/02/2022
	; 12/01/2022
	; 24/12/2021
	; 11/12/2021
	; 04/12/2021
	; 10/10/2015, 18/10/2015
	; 21/07/2015, 24/07/2015
	; 24/06/2015, 25/06/2015
        ; Moving arguments to the end of [u.upage]
	; (by regarding page borders in user's memory space)
	;
	; 10/10/2015
	; 21/07/2015
	mov	ebp, esp ; (**)
	; 18/10/2015
	mov 	edi, ebp
	mov 	ecx, MAX_ARG_LEN ; 256
	;sub	edi, MAX_ARG_LEN ; 256
	sub	edi, ecx
	mov	esp, edi
	xor	eax, eax
	mov 	[u.nread], eax ; 0
	; 12/01/2022
	; ([argc] must be cleared because previous 'sysexec'
	; may leave it with any value after an error))
	mov	[argc], eax ; 0
	;
	dec	ecx ; 256 - 1
	mov 	[u.count], ecx ; MAX_ARG_LEN - 1 ; 255
	;mov 	dword [u.count], MAX_ARG_LEN - 1 ; 255
	; 24/12/2021
sysexec_2:
	mov	esi, [argv] ; 18/10/2015
	call	get_argp
	;mov	ecx, 4 
	; 06/02/2022
	xor	ecx, ecx
	mov	cl, 4
sysexec_3:
	and	eax, eax
	jz	short sysexec_6
	; 18/10/2015
	add	[argv], ecx ; 4
	;;inc	word [argc]
	; 11/12/2021
	;inc	dword [argc]
	; 12/01/2022
	inc	byte [argc]
	;
	mov	[u.base], eax
 	; 23/10/2015
	mov	word [u.pcount], 0
sysexec_4:
	call	cpass ; get a character from user's core memory
        jnz	short sysexec_5
		; (max. 255 chars + null)
	; 18/10/2015
	sub 	al, al
	stosb
	inc	dword [u.nread]
	jmp	short sysexec_6
sysexec_5:
	stosb
	and 	al, al
	jnz	short sysexec_4
	mov	ecx, 4
	cmp	[ncount], ecx ; 4
	jb	short sysexec_2
	mov	esi, [nbase]
	add	[nbase], ecx ; 4	
	;sub	[ncount], cx 
	; 11/12/2021
	sub	[ncount], ecx
	mov	eax, [esi]
	jmp	short sysexec_3
sysexec_6:
	; 24/12/2021
	; 18/10/2015
	; argument list transfer from user's core memory to
	; kernel stack frame is OK here.
	; [u.nread] = argument list length
	;mov	[argv], esp ; start address of argument list
	;
	; 18/10/2015
	; 24/07/2015
        ; 21/07/2015
	; 02/07/2015
	; 25/06/2015
	; 24/06/2015
	; 23/06/2015
	;
	mov	ebx, [u.ppgdir] ; parent's page directory
	and 	ebx, ebx  ; /etc/init ? (u.ppgdir = 0)	
	jz	short sysexec_7
	mov	eax, [u.pgdir] ; physical address of page directory
	call	deallocate_page_dir
sysexec_7:
	call	make_page_dir
	;jc	short sysexec_14
	;jc	panic  ; allocation error 
	;	       ; after a deallocation would be nonsence !?
	; 26/03/2021
	jc	short sysexec_panic
	
	; 24/07/2015
	; map kernel pages (1st 4MB) to PDE 0
	;     of the user's page directory
	;     (It is needed for interrupts!)
	; 18/10/2015
	mov	edx, [k_page_dir] ; Kernel's page directory
	mov	eax, [edx] ; physical address of
			   ; kernel's first page table (1st 4 MB)
			   ; (PDE 0 of kernel's page directory)
	mov 	edx, [u.pgdir]
	mov	[edx], eax ; PDE 0 (1st 4MB)
	;
	; 20/07/2015
	mov	ebx, CORE ; start address = 0 (virtual) + CORE
	; 18/10/2015
	mov	esi, pcore ; physical start address
sysexec_8:	
	mov	ecx, PDE_A_USER + PDE_A_WRITE + PDE_A_PRESENT
	call	make_page_table
	;jc	panic
	; 26/03/2021
	jc	short sysexec_panic
	;
	;mov	ecx, PTE_A_USER + PTE_A_WRITE + PTE_A_PRESENT
	call	make_page ; make new page, clear and set the pte
	;jc	panic
	; 26/03/2021
	jc	short sysexec_panic
	;
	mov	[esi], eax ; 24/06/2015
	; ebx = virtual address (24/07/2015)
	; 30/11/2021
	;call 	add_to_swap_queue
	; 18/10/2015
	cmp	esi, ecore ; user's stack (last) page ?
	je	short sysexec_9 ; yes
	mov	esi, ecore  ; physical address of the last page
	; 20/07/2015
	mov	ebx, (ECORE - PAGE_SIZE) + CORE
	; ebx = virtual end address + segment base address - 4K
        jmp     short sysexec_8
sysexec_panic:
	; 26/03/2021
	jmp	panic

sysexec_9:
	; 12/01/2022
	; 11/12/2021
	; 18/10/2015
	; 26/08/2015
	; 25/06/2015
	; move arguments from kernel stack to [ecore]
	; (argument list/line will be copied from kernel stack
	; frame to the last (stack) page of user's core memory)
	; 18/10/2015
	mov	edi, [ecore]
	add	edi, PAGE_SIZE
	;movzx	eax, word [argc]
	; 12/01/2022
	;xor	eax, eax
	;mov	al, [argc]
	; [argc] < 32
	; 11/12/2021
	mov	eax, [argc]
	or	eax, eax
	jnz	short sysexec_10
	mov 	ebx, edi
	sub	ebx, 4 
	mov	[ebx], eax ; 0
	jmp 	short sysexec_13
sysexec_10:
	mov	ecx, [u.nread]
	;mov 	esi, [argv]
	mov	esi, esp ; start address of argument list
	sub	edi, ecx ; page end address - argument list length

	;;;;
	; 09/05/2022
	; (move edi -backward- to dword boundary)
	; ((this will prevent 'general protection fault' error
	;  as result of a lodsd or dword move instruction
	;  at the end of argument list))
	sub	edi, 3
	and	edi, ~3 ; (*)
	;;;

	mov	edx, eax
	; 03/02/2022 ; ([argc] < 32)
	inc	dl ; argument count + 1 for argc value
	shl 	dl, 2  ; 4 * (argument count + 1)
	; edx <= 128
	mov	ebx, edi
	; 09/05/2022 (*) - edi is already dword aligned -
	;and	bl, 0FCh ; 32 bit (dword) alignment
	sub 	ebx, edx
	mov	edx, edi
	rep	movsb
	mov 	esi, edx
	mov 	edi, ebx
	mov	edx, ECORE - PAGE_SIZE ; virtual addr. of the last page
	sub 	edx, [ecore] ; difference (virtual - physical) 
	stosd	; eax = argument count
sysexec_11:
	mov	eax, esi
	add	eax, edx
	stosd  ; eax = virtual address
	dec	byte [argc]
	jz	short sysexec_13
sysexec_12:
	lodsb
	and	al, al
	jnz	short sysexec_12
	jmp	short sysexec_11
	;
	; 1:
		; mov (sp)+,r5 / r5 now contains address of list of 
			     ; / pointers to arguments to be passed
		; mov $1,u.quit / u.quit determines handling of quits;
			      ; / u.quit = 1 take quit
		; mov $1,u.intr / u.intr determines handling of 
			     ; / interrupts; u.intr = 1 take interrupt
		; mov $rtssym,30 / emt trap vector set to take 
			       ; / system routine
		; mov $fpsym,*10 / reserved instruction trap vector
			       ; / set to take system routine
		; mov $sstack,sp / stack space used during swapping
		; mov r5,-(sp) / save arguments pointer on stack
		; mov $ecore,r5 / r5 has end of core
		; mov $core,r4 / r4 has start of users core
		; mov r4,u.base / u.base has start of users core
		; mov (sp),r2 / move arguments list pointer into r2
	; 1:
		; tst (r2)+ / argument char = "nul"
		; bne 1b
		; tst -(r2) / decrement r2 by 2; r2 has addr of
			  ; / end of argument pointer list
	; 1:
	     ; / move arguments to bottom of users core
		; mov -(r2),r3 / (r3) last non zero argument ptr
		; cmp r2,(sp) / is r2 = beginning of argument
			    ; / ptr list
		; blo 1f / branch to 1f when all arguments
		       ; / are moved
		; mov -(r2),r3 / (r3) last non zero argument ptr
	; 2:
		; tstb (r3)+
		; bne 2b / scan argument for \0 (nul)

	; 2:
		; movb -(r3),-(r5) / move argument char 
				 ; / by char starting at "ecore"
		; cmp r3,(r2) / moved all characters in 
			    ; / this argument
		; bhi 2b / branch 2b if not
		; mov r5,(r4)+ / move r5 into top of users core;
			     ; / r5 has pointer to nth arg
		; br 1b / string
	; 1:
		; clrb -(r5)
		; bic $1,r5 / make r5 even, r5 points to 
			; / last word of argument strings
		; mov $core,r2
	
	; 1: / move argument pointers into core following 
	      ; / argument strings
		; cmp r2,r4
		; bhis 1f / branch to 1f when all pointers
			; / are moved
		; mov (r2)+,-(r5)
		; br 1b
	; 1:
		; sub $core,r4 / gives number of arguments *2
		; asr r4 / divide r4 by 2 to calculate 
		       ; / the number of args stored
		; mov r4,-(r5) / save number of arguments ahead
			     ; / of the argument pointers
sysexec_13:
	; 30/11/2021
	; 28/11/2021
	; 19/10/2015
	; 18/10/2015
	; 29/07/2015
	; 25/07/2015
	; 24/07/2015
	; 20/07/2015
	; 25/06/2015
	; 24/06/2015
	; 23/06/2015
	;
	; moving arguments to [ecore] is OK here..
	; 18/10/2015
	mov 	esp, ebp ; (**) restore kernel stack pointer
	; ebx = beginning address of argument list pointers
	;	in user's stack
	; 19/10/2015
	sub 	ebx, [ecore]
	add     ebx, (ECORE - PAGE_SIZE)
			; end of core - 4096 (last page)
			; (virtual address)
	mov	[argv], ebx
	mov	[u.break], ebx ; available user memory
	;
	sub	eax, eax
	mov	dword [u.count], 32 ; Executable file header size
		; mov $14,u.count
	mov	dword [u.fofp], u.off
		; mov $u.off,u.fofp
	mov	[u.off], eax ; 0
		; clr u.off / set offset in file to be read to zero
	; 25/07/2015
	mov	[u.base], eax ; 0, start of user's core (virtual)
	; 25/06/2015 
	mov	eax, [ii] ; 28/11/2021 (32 bit inode number)
	; AX = i-number of the executable file
	call	readi
		; jsr r0,readi / read in first six words of 
			; / user's file, starting at $core
		; mov sp,r5 / put users stack address in r5
		; sub $core+40.,r5 / subtract $core +40, 
				; / from r5 (leaves number of words
				; / less 26 available for
			     	; / program in user core
		; mov r5,u.count /
	; 25/06/2015
	mov	ecx, [u.break] ; top of user's stack (physical addr.)
	mov	[u.count], ecx ; save for overrun check
	;
	mov	ecx, [u.nread]
	mov	[u.break], ecx ; virtual address (offset from start)
	cmp	cl, 32
        jne     short sysexec_15
	;:
	; 25/06/2015
	; Retro UNIX 386 v1 (32 bit) executable file header format
	; 18/10/2015
	mov	esi, [pcore] ; start address of user's core memory 
		             ; (phys. start addr. of the exec. file)
	lodsd
	cmp	ax, 1EEBh ; EBh, 1Eh -> jump to +32
	jne	short sysexec_15
		; cmp core,$405 / br .+14 is first instruction 
			      ; / if file is standard a.out format
		; bne 1f / branch, if not standard format
	lodsd
	mov	ecx, eax ; text (code) section size
	lodsd
	add	ecx, eax ; + data section size (initialized data)
		; mov core+2,r5 / put 2nd word of users program in r5;
		              ; / number of bytes in program text
		; sub $14,r5 / subtract 12
	mov	ebx, ecx
	;
	; 25/06/2015
	; NOTE: These are for next versions of Retro UNIX 386
	;	and SINGLIX operating systems (as code template).
	;	Current Retro UNIX 386 v1 files can be max. 64KB
	;	due to RUFS (floppy disk file system) restriction...
	;	Overrun is not possible for current version.
	;
	lodsd	
	add	ebx, eax ; + bss section size (for overrun checking)
	cmp	ebx, [u.count]
	ja	short sysexec_14  ; program overruns stack !
	;
	; 24/07/2015
	; add bss section size to [u.break]
	add 	[u.break], eax
	;
	sub	ecx, 32 ; header size (already loaded)
	;cmp	ecx, [u.count]
	;jnb	short sysexec_16
		; cmp r5,u.count /
		; bgt 1f / branch if r5 greater than u.count
	mov	[u.count], ecx ; required read count
		; mov r5,u.count
	;
	jmp	short sysexec_16
	;
sysexec_14:
	; 23/06/2015
	; insufficient (out of) memory
	mov	dword [u.error], ERR_MINOR_IM ; 1
	jmp	error
	;
sysexec_15:
	; 25/06/2015
        ;movzx   edx, word [i.size] ; file size
	; 30/11/2021
	mov	edx, [i.size] ; file size
	sub	edx, ecx ; file size - loaded bytes
	jna	short sysexec_17 ; no need to next read
	add	ecx, edx ; [i.size]
	cmp	ecx, [u.count] ; overrun check (!)
	ja	short sysexec_14
	mov	[u.count], edx
sysexec_16:
	;mov	ax, [ii] ; i-number
	; 28/11/2021
	mov	eax, [ii] ; 32 bit inode number
	call	readi
		; add core+10,u.nread / add size of user data area
		                    ; / to u.nread
		; br 2f
	; 1:
		; jsr r0,readi / read in rest of file
	; 2:
	mov	ecx, [u.nread]
	add	[u.break], ecx
		; mov u.nread,u.break / set users program break to end of
				    ; / user code
		; add $core+14,u.break / plus data area
	; 20/07/2015
sysexec_17:
	; 26/03/2021 - Retro UNIX 386 v2
	; ('iclose' is not needed for regular files, from now on)
	;;mov	ax, [ii] ; i-number
	;call	iclose
	;	; jsr r0,iclose / does nothing
        xor     eax, eax
	inc	al
	mov	[u.intr], ax ; 1 (interrupt/time-out is enabled)
	mov	[u.quit], ax ; 1 ('crtl+brk' signal is enabled) 
	; 30/11/2021
	dec	al  ; eax = 0
	; 02/07/2015
        ;cmp	dword [u.ppgdir], 0 ; is the caller sys_init (kernel) ?
	cmp	[u.ppgdir], eax ; 0 ; is the caller sys_init (kernel) ?
	ja	short sysexec_18 ; no, the caller is user process
	; If the caller is kernel (sys_init), 'sysexec' will come here
	mov	edx, [k_page_dir] ; kernel's page directory
	mov	[u.ppgdir], edx ; next time 'sysexec' must not come here
sysexec_18:
	; 18/10/2015
	; 05/08/2015
	; 29/07/2015
	mov	ebp, [argv] ; user's stack pointer must points to argument
			    ; list pointers (argument count)
	cli
        mov     esp, [tss.esp0]  ; ring 0 (kernel) stack pointer
	;mov   	esp, [u.sp] ; Restore Kernel stack
			    ; for this process	 
	;add	esp, 20 ; --> EIP, CS, EFLAGS, ESP, SS
	;;xor	eax, eax ; 0
	;dec	al ; eax = 0
	; eax = 0 ; 30/11/2021 
	mov	dx, UDATA
	push	dx  ; user's stack segment
	push	ebp ; user's stack pointer
		    ; (points to number of arguments)
	sti
	pushfd	; EFLAGS
		; Set IF for enabling interrupts in user mode
	;or	dword [esp], 200h 
	;
	;mov	bx, UCODE
	;push	bx ; user's code segment
	push	UCODE
	;push	0
	push	eax ; EIP (=0) - start address -
		; clr -(r5) / popped into ps when rti in 
			  ; / sysrele is executed
		; mov $core,-(r5) / popped into pc when rti 
		                ; / in sysrele is executed
		;mov r5,0f / load second copyz argument
		;tst -(r5) / decrement r5
	mov	[u.sp], esp ; 29/07/2015
	; 05/08/2015
	; Remedy of a General Protection Fault during 'iretd' is here !
	; ('push dx' would cause to general protection fault, 
	; after 'pop ds' etc.)
	;
	;; push dx ; ds (UDATA)
	;; push dx ; es (UDATA)
	;; push dx ; fs (UDATA)
	;; push dx ; gs (UDATA)
	;
	; This is a trick to prevent general protection fault
	; during 'iretd' intruction at the end of 'sysrele' (in u1.s):
	mov 	es, dx ; UDATA
	push 	es ; ds (UDATA)
	push 	es ; es (UDATA)
	push 	es ; fs (UDATA)
	push	es ; gs (UDATA)
	mov	dx, KDATA
	mov	es, dx
	;
	;; pushad simulation
	mov	ebp, esp ; esp before pushad
	push	eax ; eax (0)
	push	eax ; ecx (0)
	push	eax ; edx (0)
	push	eax ; ebx (0)
	push	ebp ; esp before pushad
	push	eax ; ebp (0)
	push	eax ; esi (0)		
	push	eax ; edi (0)	
	;
	mov	[u.r0], eax ; eax = 0
	mov	[u.usp], esp
		; mov r5,u.r0 /
		; sub $16.,r5 / skip 8 words
		; mov r5,u.sp / assign user stack pointer value,
		;             / effectively zeroes all regs
			    ; / when sysrele is executed
		; jsr r0,copyz; core; 0:0 / zero user's core
		; clr u.break
		; mov r5,sp / point sp to user's stack
	;
	jmp	sysret0
	;jmp	sysret
		; br sysret3 / return to core image at $core

get_argp:
	; 11/12/2021 - Retro UNIX 386 v1.2
	; 18/10/2015 (nbase, ncount)
	; 21/07/2015
	; 24/06/2015 (Retro UNIX 386 v1)
	; Get (virtual) address of argument from user's core memory
	;
	; INPUT:
	;	esi = virtual address of argument pointer
	; OUTPUT:
	;	eax = virtual address of argument
	;
	; Modified registers: EAX, EBX, ECX, EDX, ESI 
	;
 	cmp     dword [u.ppgdir], 0 ; /etc/init ?
				    ; (the caller is kernel)
        jna     short get_argpk 
	;
     	mov	ebx, esi
	call	get_physical_addr ; get physical address
        jc	short get_argp_err ; 11/12/2021
	mov 	[nbase], eax ; physical address	
	;mov	[ncount], cx ; remain byte count in page (1-4096)
	; 11/12/2021
	mov	[ncount], ecx
	mov	eax, 4 ; 21/07/2015
	;cmp	cx, ax ; 4
	; 11/12/2021
	cmp	ecx, eax ; 4
	jnb	short get_argp2
	mov	ebx, esi
	add	ebx, ecx
	call	get_physical_addr ; get physical address
	jc	short get_argp_err
	;push	esi
	mov	esi, eax
	;xchg	cx, [ncount]
	; 11/12/2021
	xchg	ecx, [ncount]
	xchg	esi, [nbase]
	mov	ch, 4
	sub	ch, cl
get_argp0:
	lodsb
	;push	ax
	; 11/12/2021
	push	eax
	dec	cl
        jnz     short get_argp0
	mov	esi, [nbase]
	; 21/07/2015
	movzx	eax, ch
	add	[nbase], eax
	;sub	[ncount], ax
	; 11/12/2021
	sub	[ncount], eax
get_argp1:
	lodsb
	dec	ch
        jz      short get_argp3
        ;push	ax
	; 11/12/2021
	push	eax
	jmp     short get_argp1
get_argp_err:
	mov	[u.error], eax
	jmp	error
get_argp2:
	; 21/07/2015
	;mov	eax, 4
	mov 	edx, [nbase] ; 18/10/2015
	add	[nbase], eax
	;sub	[ncount], ax
	; 11/12/2021
	sub	[ncount], eax
	;
	mov	eax, [edx]
	retn
get_argpk:
	; Argument is in kernel's memory space
	mov	word [ncount], PAGE_SIZE ; 4096
	mov	[nbase], esi
	add	dword [nbase], 4
	mov	eax, [esi] ; virtual addr. = physical addr.
	retn
get_argp3:
	mov	cl, 3
get_argp4:
	shl	eax, 8
	;pop	dx
	; 11/12/2021
	pop	edx
	mov 	al, dl
        loop    get_argp4
	;pop	esi
	retn	

sysfstat:
	; 09/05/2022 (Retro UNIX 386 v1.2, Kernel v0.2.2.1)
	;	([idev] return in eax)
	;	0 = root device
	;	1 = mounted device (>0)
	; 06/02/2022 
	; 08/01/2022
	; 26/12/2021 (Retro UNIX 386 v1.2) 
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/06/2013 (Retro UNIX 8086 v1)
	;
	; 'sysfstat' is identical to 'sysstat' except that it operates
	; on open files instead of files given by name. It puts the
	; buffer address on the stack, gets the i-number and
	; checks to see if the file is open for reading or writing.
	; If the file is open for writing (i-number is negative)
	; the i-number is set positive and a branch into 'sysstat'
	; is made.	
	;
	; Calling sequence:
	;	sysfstat; buf
	; Arguments:
	;	buf - buffer address
	;
	; Inputs: *u.r0 - file descriptor
	; Outputs: buffer is loaded with file information
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification:
	;       'sysfstat' system call has two arguments; so,
	;	* 1st argument, file descriptor is in BX register
	;	* 2nd argument, buf is pointed to by CX register

	; / set status of open file
		; jsr r0,arg; u.off / put buffer address in u.off
	push	ecx ; *
		; mov u.off,-(sp) / put buffer address on the stack
		; mov *u.r0,r1 / put file descriptor in r1
		; jsr r0,getf / get the files i-number
	; BX = file descriptor (file number)
	call	getf1
	; 06/02/2022
	and	eax, eax
	;and	ax, ax ; i-number of the file
		; tst	r1 / is it 0?
	;jz	error
		; beq error3 / yes, error
	;jnz	short sysfstat1
	; 26/12/2021 - Retro UNIX 386 v1.2 (runix v2 file system)
	jnz	short sysstat1
	mov	dword [u.error], ERR_FILE_NOT_OPEN  ; 'file not open !'
	jmp	error
	; 26/12/2021
;sysfstat1:
	;cmp	ah, 80h
        ;jb      short sysstat1
	;	; bgt 1f / if i-number is negative (open for writing)
	;neg	ax
	;	; neg r1 / make it positive, then branch
	; 08/01/2022
	;jmp	short sysstat1
		; br 1f / to 1f
sysstat:
	; 09/05/2022 (Retro UNIX 386 v1.1, Kernel v0.2.1.3)
	;	([idev] return in eax)
	;	0 = root device
	;	1 = mounted device (>0)
	; 26/12/2021 (Retro UNIX 386 v1.2) 
	; 18/10/2015
	; 07/10/2015
	; 02/09/2015
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/06/2013 (Retro UNIX 8086 v1)
	;
	; 'sysstat' gets the status of a file. Its arguments are the
	; name of the file and buffer address. The buffer is 34 bytes
	; long and information about the file placed in it.	
	; sysstat calls 'namei' to get the i-number of the file.
	; Then 'iget' is called to get i-node in core. The buffer
	; is then loaded and the results are given in the UNIX
	; Programmers Manual sysstat (II).	
	;
	; Calling sequence:
	;	sysstat; name; buf
	; Arguments:
	;	name - points to the name of the file
	;	buf - address of a 34 bytes buffer
	; Inputs: -
	; Outputs: buffer is loaded with file information
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'sysstat' system call has two arguments; so,
	;	Retro UNIX 8086 v1 argument transfer method 2 is used
	;	to get sysstat system call arguments from the user;
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, buf is pointed to by CX register
	;
	;	NOTE: Retro UNIX 8086 v1 'arg2' routine gets these
	;	      arguments which were in these registers;
	;	      but, it returns by putting the 1st argument
	;	      in 'u.namep' and the 2nd argument
	;	      on top of stack. (1st argument is offset of the
	;	      file/path name in the user's program segment.)		 	
	
	; / ; name of file; buffer - get files status
		; jsr r0,arg2 / get the 2 arguments
	mov	[u.namep], ebx
	push	ecx ; *
	call	namei
		; jsr r0,namei / get the i-number for the file
	;jc	error
		; br error3 / no such file, error
	jnc	short sysstat1
	; pop 	ecx
sysstat_err0:
	; 'file not found !' error
	mov	dword [u.error], ERR_FILE_NOT_FOUND ; 12
	jmp	error

sysstat1: ; 1:
	call	iget
		; jsr r0,iget / get the i-node into core
	; 07/10/2015 (ax = [ii], inode number)
	; 02/09/2015
	pop	dword [u.base] ; *
		; mov (sp)+,r3 / move u.off to r3 (points to buffer)
	call	sysstat_gpa ; get physical address
	jnc 	short sysstat2
sysstat_err1:
	mov	dword [u.error], eax ; error code
	jmp	error
sysstat2:
	mov 	al, [ii] ; 07/10/2015 (result of 'iget' call, above)
	stosb
	inc 	dword [u.base]
	;dec 	cx
	dec	ecx ; 26/12/2021
	jnz	short sysstat3
	call	sysstat_gpa
	;jc	short sysstat_err1
sysstat3:
	mov 	al, [ii+1] ; 07/10/2015 (result of 'iget' call, above)
	stosb
		; mov r1,(r3)+ / put i-number in 1st word of buffer
	inc 	dword [u.base]
	;;dec 	word [u.pcount]
	;dec	cx
	dec	ecx ; 26/12/2021
	jnz	short sysstat4
	call	sysstat_gpa
	;jc	short sysstat_err1	
sysstat4:
	mov	esi, inode
		; mov $inode,r2 / r2 points to i-node
sysstat5: ; 1:
	movsb
		; mov (r2)+,(r3)+ / move rest of i-node to buffer
	inc 	dword [u.base]
	;;dec 	word [u.pcount]
	;dec	cx
	dec	ecx ; 26/12/2021
	jnz	short sysstat6
	call	sysstat_gpa
	;jc	short sysstat_err1
sysstat6:
	; 26/12/2021 - Retro UNIX 386 v1.2
	cmp	esi, inode + 64 ; Retro UNIX v2 inode		
	;cmp	esi, inode + 32
		; cmp r2,$inode+32 / done?
	jne	short sysstat5
		; bne 1b / no, go back

	;;;
	; 09/05/2022
	;*** additional feature *** -retro unix only- 
	;
	; !! return device number -of current inode- in eax !!
	;
	; (modification reason/purpose:
	; to improve 'pwd' command's pathname output/result
	; and to correct 'cp' command's 'can not copy file itself'
	; error due to same inode numbers in root file system
	; and mounted file system.)
	;
	sub	eax, eax
	mov	al, [idev] ; [cdev]
	mov	[u.r0], eax
	;;;  

	jmp	sysret
		; br sysret3 / return through sysret
	;
sysstat_gpa: ; get physical address of file status buffer
	; 02/09/2015
	mov 	ebx, [u.base]
	; 07/10/2015
	call	get_physical_addr ; get physical address
	;jc	short sysstat_gpa1
	jc	short sysstat_err1
	; 18/10/2015
	mov	edi, eax ; physical address
	;mov	[u.pcount], cx ; remain bytes in page
;sysstat_gpa1:
	retn

fclose:
	; 12/03/2022
	; 11/02/2022
	; 08/01/2022
	; 04/12/2021 - Retro UNIX 386 v1.2
	; 08/04/2021
	; 04/04/2021
	; 18/08/2020 - Retro UNIX 386 v2
	; 18/06/2015 (Retro UNIX 386 v1 - Beginning)
	;            (32 bit offset pointer modification)
	; 19/04/2013 - 12/01/2014 (Retro UNIX 8086 v1)
	;
	; Given the file descriptor (index to the u.fp list)
	; 'fclose' first gets the i-number of the file via 'getf'.
	; If i-node is active (i-number > 0) the entry in 
	; u.fp list is cleared. If all the processes that opened
	; that file close it, then fsp etry is freed and the file
	; is closed. If not a return is taken. 
	; If the file has been deleted while open, 'anyi' is called
	; to see anyone else has it open, i.e., see if it is appears
	; in another entry in the fsp table. Upon return from 'anyi'
	; a check is made to see if the file is special.	
	;
	; INPUTS ->
	;    r1 - contains the file descriptor (value=0,1,2...)
	;    u.fp - list of entries in the fsp table
	;    fsp - table of entries (4 words/entry) of open files.	 
	; OUTPUTS ->
	;    r1 - contains the same file descriptor
	;    r2 - contains i-number
	;
	; ((AX = R1))
	; ((Modified registers: edx, ebx, ecx, esi, edi, ebp))
	;
	; Retro UNIX 8086 v1 modification : CF = 1
	;              if i-number of the file is 0. (error)

	; 08/04/2021
	; INPUT:
	;	eax = file descriptor
	; OUTPUT:
	;	cf = 0 -> eax = file descriptor
	;	cf = 1 -> file not open ; 11/02/2022

	; 18/08/2020
	;movzx	edx, ax ; **
	mov	ebx, eax

	; 18/08/2020
_fclose:
	push	ebx ; ***  ; file descriptor (index to u.fp)

	;push	ax ; ***
		; mov r1,-(sp) / put r1 on the stack (it contains 
			     ; / the index to u.fp list)
	; ebx = file descriptor/number
	call	getf
		; jsr r0,getf / r1 contains i-number, 
			    ; / cdev has device =, u.fofp 
			    ; / points to 3rd word of fsp entry
	;cmp	eax, 1 ; 18/08/2020
	cmp	ax, 1 ; r1
		; tst r1 / is i-number 0?
	jb	short fclose_2
		; beq 1f / yes, i-node not active so return
		; tst (r0)+ / no, jump over error return
	
	;mov	ebx, edx ; **
 	; 18/08/2020
	mov	ebx, [esp]
	;mov	edx, eax 
	;;mov 	dx, ax ; *
		; mov r1,r2 / move i-number to r2 ;*
		; mov (sp),r1 / restore value of r1 from the stack
			    ; / which is index to u.fp ; **
	mov	byte [ebx+u.fp], 0
		; clrb u.fp(r1) / clear that entry in the u.fp list
	mov	ebx, [u.fofp]
		; mov u.fofp,r1 / r1 points to 3rd word in fsp entry

	; 18/08/2020 - Retro UNIX 386 v2 (new fsp structure)
	; ebx = 5th word of fsp entry (64 bit file offset)
fclose_0:
	; 08/01/2022
	dec	byte [ebx-2] ; 18/08/2020 - Retro UNIX 386 v2
	;dec	byte [ebx+4] ; 18/06/2015
		; decb 2(r1) / decrement the number of processes 
			   ; / that have opened the file
	jns	short fclose_2 ; jump if not negative (jump if bit 7 is 0)	 
		; bge 1f / if all processes haven't closed the file, return
	;
	;push	dx ; *
		; mov r2,-(sp) / put r2 on the stack (i-number)
	;;xor	ax, ax ; 0
	;xor	eax, eax ; 18/08/2020
		; clear 1st word of fsp entry
	;mov	[ebx-8], ax ; 0 ; 18/08/2020
	; 18/08/2020 - Retro UNIX 386 v2
	mov	word [ebx-8], 0 ; clear 1st word of fsp entry ; i-number
			; Note: 32 bit i-node number field is ready but
			;	it is not used in current runix version.	

	;;mov	[ebx-4], ax ; 0
		; clr -4(r1) / clear 1st word of fsp entry
	;mov	al, [ebx+5] ; 18/06/2015
	;	; tstb	3(r1) / has this file been deleted
	;and	al, al
	;jz	short fclose_1
	;	; beq 2f / no, branch

	;push	edx ; * ; 18/08/2020
	;mov	eax, edx ; * ; 18/08/2020

	;; 18/08/2020 - Retro UNIX 386 v2 (new fsp structure)
	;;test	byte [ebx-3], 80h ; open mode & status flags
	; 04/04/2021 - Retro UNIX 386 v2 (new fsp structure)
	;test	byte [ebx-4], 80h ; open mode & status flags 
	;jz	short fclose_1	; deleted file flag (bit 7)

	; 04/04/2021
	xor	edx, edx
	; 08/01/2022
	mov	dl, [ebx-3] ; open mode & status flags
	test	dl, 80h	    ; deleted file flag (bit 7)
	jz	short fclose_1 ; (not deleted -while open-)
	push	edx
	; 12/03/2022
	push	eax ; inode number 

	; deleted file !

	;mov	ax, dx ; *
		; mov r2,r1 / yes, put i-number back into r1
	; AX = inode number
	call	anyi
		; jsr r0,anyi / free all blocks related to i-number
			    ; / check if file appears in fsp again
	; 12/03/2022
	pop	eax ; inode number
	; 04/04/2021
	pop	edx
	; 11/02/2022
	;(cpu will come here if anyi returns -without error-)
	;; 08/04/2021
	;jc	short fclose_3 ; error code in eax	
	;; 18/08/2020
	;;jmp	short fclose_2

	; ax = inode number
fclose_1: ; 2:
	; 04/04/2021
	and	dl, 1
	inc	dl ; 0 -> 1 (open for read), 1 -> 2 (open for write)
	
	;pop	eax ; * ; 18/08/2020 
	;;pop	ax ; *
		; mov (sp)+,r1 / put i-number back into r1
	call	iclose ; close if it is special file 
		; jsr r0,iclose / check to see if it is a special file
	; 11/02/2022
	;(cpu will come here if iclose returns -without error-)
	;; 08/04/2021
	;jnc	short fclose_2
;fclose_3:
;	pop	ebx ; ***  ; file descriptor
;	; eax = error code
;	retn
fclose_2: ; 1:
	pop	eax ; *** ; 18/08/2020
	;pop	ax ; ***
		; mov (sp)+,r1 / put index to u.fp back into r1
	retn
		; rts r0

	; 08/01/2022
	; 04/12/2021 - Retro UNIX 386 v2 fs compatibility code
getf:	; / get the device number and the i-number of an open file
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/04/2013 - 18/11/2013 (Retro UNIX 8086 v1)

	; 04/12/2021 - Retro UNIX 386 v1.2
	;
	; 16/05/2021
	; 27/03/2020 - Retro UNIX 386 v2 
	;	(new open files -fsp- structure)
	; 13/05/2015
	; 11/05/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/04/2013 - 18/11/2013 (Retro UNIX 8086 v1)
	
	; INPUT: 
	;    ;eax = file descriptor/number
	;     ebx = file descriptor/number ; 18/08/2020 	
	; OUTPUT:
	;     bl = open mode & status flag ; 27/03/2020
	;        (bit 0 open mode flag, 0 = read, 1 = write)  
	;    eax = inode number (in AX)
	;          If eax = 0 -> file not open
	;   byte [cdev] = logical drive number (of inode in eax)
	;	
	; Modified registers: eax, ebx

	; 16/05/2021 ('getf' procedure, unix v7 x86 'fio.c')
	; /*
	;  * Convert a user supplied
	;  * file descriptor into a pointer
	;  * to a file structure.
	;  * Only task is to check range
	;  * of the descriptor.
	;  */
	; struct file *
	; getf(f)
	;
	; 18/08/2020
	; 	ebx = file descriptor/number

	; 16/05/2021
	;NOFILE	equ 10 ; 08/01/2022

	; 18/08/2020
	;mov	ebx, eax
getf1: ;; Calling point from 'rw1' (23/05/2013)

	; ebx = file descriptor/number

	; 18/08/2020
	sub	eax, eax ; eax = 0
 
	cmp	ebx, 10 ; cmp ebx, NOFILE ; 16/05/2021
		; cmp r1,$10. / user limited to 10 open files
        jnb	short getf2 ; 13/05/2015
	;jnb	error
		; bhis error3 / u.fp is table of users open files,
			    ; / index in fsp table
	; 08/01/2022
	mov	al, [ebx+u.fp]
	;mov	bl, [ebx+u.fp]
		; movb u.fp(r1),r1 / r1 contains number of entry 
		                 ; / in fsp table
	or	al, al ; 08/01/2022
	;or	bl, bl
	;jnz	short getf3
	jz	short getf2 ; 18/08/2020
	;;jz	short getf4
		; beq 1f / if its zero return
;getf2:
;	; 'File not open !' error (ax=0)
;	;sub	eax, eax ; 18/08/2020
;	retn
;getf3:	
	; Retro UNIX 386 v1 modification ! (11/05/2015)
	;
	; 'fsp' table (10 bytes/entry)
	; bit 15				   bit 0
	; ---|-------------------------------------------
	; r/w|		i-number of open file
	; ---|-------------------------------------------
	;		   device number
	; -----------------------------------------------
	; offset pointer, r/w pointer to file (bit 0-15)
	; -----------------------------------------------
	; offset pointer, r/w pointer to file (bit 16-31)
	; ----------------------|------------------------
	;  flag that says file 	| number of processes
	;   has been deleted	| that have file open 
	; ----------------------|------------------------

	; Retro UNIX 386 v2 modification ! (27/03/2020)

	; bit 15 				    bit 0
	; ----------------------------------------------- byte 0
	;    	       i-number of open file           
	; ----------------------------------------------- byte 2
	;          high word of 32 bit i-number       
	; ----------------------------------------------- byte 4
	;    open mode & status  |   device number     
	; ----------------------------------------------- byte 6
	;       reserved byte    |    open count  
	; ----------------------------------------------- byte 8
	;   offset pointer, i.e., r/w pointer to file 
	; ----------------------------------------------- byte 10
	;     64 bit file offset pointer (bit 16-31)  
	; ----------------------------------------------- byte 12
	;     64 bit file offset pointer (bit 32-47)   
	; ----------------------------------------------- byte 14
	;     64 bit file offset pointer (bit 48-63)  
	; ----------------------------------------------- byte 16
	
	; 11/05/2015 - Retro UNIX 386 v1
	;mov	eax, 10

	; 27/03/2020 - Retro UNIX 386 v2	
	;mov	eax, opfls.size ; mov eax, 16
	
	;sub	eax, eax ; 18/08/2020 ; eax = 0

	;mov	al, opfls.size  ; mov al, 16
	;
	;mul	bl
	;;mov	ebx, fsp-6 ; the 3rd word in the fsp entry
	;; 27/03/2020
	;mov	ebx, fsp-(opfls.size-opfls.offset) ; fsp-8
	
	;add	ebx, eax
		; asl r1
		; asl r1 / multiply by 8 to get index into 
		       ; / fsp table entry
		; asl r1
		; add $fsp-4,r1 / r1 is pointing at the 3rd word 
			      ; / in the fsp entry
	; 08/01/2022
	mov	bl, al
	; 18/08/2020
	dec	bl ; zero based fsp table entry number
	;shl	bl, 4 ; * 16 ; opfls.size
	; 08/01/2022
	shl	ebx, 4 ; * 16 ; opfls.size
	add	ebx, fsp+8 ; opfls.offset	
	; ebx = address of the file offset pointer 
	;	(not file offset! not fsp entry address !)

	mov	[u.fofp], ebx
		; mov r1,u.fofp / save address of 3rd word 
			     ; / in fsp entry in u.fofp
	; 18/08/2020
	mov	al, [ebx-3]  ; open mode & status flags

	push	eax

	;mov	ax, [ebx]
	;;mov	[cdev], al ; ;;Retro UNIX 8086 v1 ! 
	;mov	[cdev], ax ; ;;in fact (!) 
			     ;;dev number is in 1 byte
		; mov -(r1),cdev / remove the device number cdev

	; 18/08/2020
	mov	al, [ebx-4]  ; logical drive number

	mov	[cdev], al

	;dec	ebx
	;dec	ebx
	; 18/08/2020
	mov	ax, [ebx-8]
		; mov -(r1),r1 / and the i-number r1

	pop	ebx ; ebx = bl = open mode and status flag
		; (bit 0 is open mode flag, 0 = read, 1 = write)
	; eax = inode number (in ax)	

getf2:	; 18/08/2020
getf4:	; 1: ; 08/01/2022
	retn
		; rts r0

	; 15/05/2022
	; 08/01/2022
	; 19/12/2021
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
namei:
	; 18/10/2021
	; 12/06/2021
	; 01/05/2021
	; 26/03/2021
	; 25/03/2021 (Retro UNIX 386 v2 - Beginning)
	; 04/12/2015 (14 byte file names)
	; 18/10/2015 (nbase, ncount)
	; 12/10/2015
	; 21/08/2015
	; 18/07/2015
	; 02/07/2015
	; 17/06/2015
	; 16/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 24/04/2013 - 31/07/2013 (Retro UNIX 8086 v1)
	;
	; 'namei' takes a file path name and returns i-number of
	; the file in the current directory or the root directory
	; (if the first character of the pathname is '/').	
	;
	; INPUTS ->
	;    u.namep - points to a file path name
	;    u.cdir - i-number of users directory
	;    u.cdev - device number on which user directory resides	
	; OUTPUTS ->
	;    r1 - i-number of file
	;    cdev
	;    u.dirbuf - points to directory entry where a match 
	;               occurs in the search for file path name.
	;	        If no match u.dirp points to the end of 
	;               the directory and r1 = i-number of the current
	;	        directory.	
	; ((AX = R1))
	;
	; (Retro UNIX Prototype : 07/10/2012 - 05/01/2013, UNIXCOPY.ASM)
        ; ((Modified registers: eDX, eBX, eCX, eSI, eDI, eBP))  
	;

	;mov	ax, [u.cdir]
		; mov u.cdir,r1 / put the i-number of current directory
			      ; / in r1
	; 01/05/2021
	mov	eax, [u.cdir]	
	mov	dl, [u.cdrv]
	mov	[cdev], dl
	;mov	dx, [u.cdrv]
	;mov	[cdev], dx	; NOTE: Retro UNIX 8086 v1 
				; device/drive number is in 1 byte, 
				; not in 1 word!
		; mov u.cdev,cdev / device number for users directory 
				; / into cdev
	; 12/10/2015
	; 16/06/2015 - 32 bit modifications (Retro UNIX 386 v1)
      	; convert virtual (pathname) addr to physical address
	call    trans_addr_nmbp ; 12/10/2015
		; esi = physical address of [u.namep]
		; ecx = byte count in the page
	cmp	byte [esi], '/'
		; cmpb *u.namep,$'/ / is first char in file name a /
	jne	short namei_1
		; bne 1f
	inc	dword [u.namep]
		; inc u.namep / go to next char
	;dec	cx ; remain byte count in the page
	dec	ecx ; 18/10/2021
	jnz	short namei_0
	; 12/10/2015
	call	trans_addr_nmbp ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page
	dec	esi
namei_0:
	inc 	esi  ; go to next char
	;mov	ax, [rootdir] ; 09/07/2013
	; 18/10/2021
	mov	eax, [rootdir]
		; mov rootdir,r1 / put i-number of rootdirectory in r1
	mov	byte [cdev], 0
		; clr cdev / clear device number
namei_1: ; 1:
	test	byte [esi], 0FFh
	;jz	short getf4
	; 26/03/2021
	;jz	short getf2 ; retn
	;;jz	nig
		; tstb *u.namep / is the character in file name a nul
		; beq nig / yes, end of file name reached; 
			; / branch to "nig"
	; 19/12/2021
namei_9:
	; 12/06/2021
	;jnz	short namei_2
	;retn
	; 08/01/2022
	jz	short getf4
namei_2: ; 1:
	; 18/10/2015
	mov 	[nbase], esi
	;mov 	[ncount], cx
	; 18/10/2021
	mov	[ncount], ecx
	;
	;;mov	dx, 2
	;mov	dl, 2 ; user flag (read, non-owner)
	; 25/03/2021
	;mov	dx, 100h ; IREAD - read, owner 
	; 18/10/2021
	sub	edx, edx
	inc	dh ; dx = 100h ; IREAD - read, owner 
	call	access
		; jsr r0,access; 2 / get i-node with i-number r1
	; 'access' will not return here if user has not "r" permission !

	;test 	word [i.flgs], 4000h
	;	; bit $40000,i.flgs / directory i-node?
        ;jz	short namei_err
		; beq error3 / no, got an error
	; 26/03/2021 
;	mov	al, [i.flgs+1]
;	;test	al, 80h ; IFREG ; 80h
;	;jz	short namei_err ; not a regular file ! (device!?)
;	;and	al, 40h ; IFDIR ; 40h
;	;jz	short namei_err ; not a sub directory !
;	shl	al, 1 ; 80h (IFREG flag) -> cf = 1
;	jnc	short namei_err ; not a regular file ! (device!?)
;	shl	al, 1 ; 80h (IFDIR flag) -> cf = 1
;	jnc	short namei_err ; not a sub directory !

	; 27/03/2021
	call	chk_dir ; check for directory, jump to 'error' if not
	; CPU will be here if the inode is a (valid) directory inode

	; 16/06/2015 - 32 bit modifications (Retro UNIX 386 v1)
	;xor	eax, eax
	;mov	[u.off], eax ; 0
	; 01/05/2021 - Retro UNIX 386 v2 inode structure
	mov	eax, [i.size]
	;mov	ax, [i.size]
	mov	[u.dirp], eax
		; mov i.size,u.dirp / put size of directory in u.dirp
		; clr u.off / u.off is file offset used by user
	; 18/10/2021
	xor	eax, eax
	;mov	[u.off], eax ; 0
	mov	ebx, u.off
	mov	[ebx], eax ; mov dword [u.off], 0
	;
	mov	[u.fofp], ebx ; u.off ; 18/10/2021
	;mov	dword [u.fofp], u.off
			; mov $u.off,u.fofp / u.fofp is a pointer to 
				  ; / the offset portion of fsp entry
namei_3: ; 2:
	mov	dword [u.base], u.dirbuf
		; mov $u.dirbuf,u.base / u.dirbuf holds a file name 
				    ; / copied from a directory
	mov 	dword [u.count], 16 ; 04/12/2015 (10 -> 16) 	
 		; mov $10.,u.count / u.count is byte count 
				 ; / for reads and writes
	;mov 	ax, [ii]
	; 18/10/2021
	mov	eax, [ii] ; (32 bit inode number)
	; 31/07/2013 ('namei_r') - 16/06/2015 ('u.kcall')
 	inc     byte [u.kcall] ; the caller is 'namei' sign	
    	call	readi
		; jsr r0,readi / read 10. bytes of file 
		      ; with i-number (r1); i.e. read a directory entry

	mov 	ecx, [u.nread]
	or 	ecx, ecx
		; tst u.nread
	jz	short nib
		; ble nib / gives error return
	;
	;mov 	bx, [u.dirbuf]
	;and 	bx, bx       
	; 18/10/2021
	test	word [u.dirbuf], 0FFFFh
		; tst u.dirbuf /
	jnz	short namei_4
		; bne 3f / branch when active directory entry 
		       ; / (i-node word in entry non zero)
	mov	eax, [u.off]
	sub	eax, 16 ; 04/12/2015 (10 -> 16) 
	mov	[u.dirp], eax
		; mov u.off,u.dirp
		; sub $10.,u.dirp
	jmp	short namei_3
		; br 2b

	; 18/07/2013
nib: 
	xor	eax, eax  ; xor ax, ax ; ax = 0 -> file not found 
	stc
nig:
	retn

	; 27/03/2021
;namei_err:
	; 16/06/2015
;	mov	dword [u.error], ERR_NOT_DIR ; 'not a directory !' error
;	jmp	error

namei_4: ; 3:
	; 18/10/2015
	; 12/10/2015
	; 21/08/2015
	; 18/07/2015
	mov	ebp, [u.namep]
		; mov u.namep,r2 / u.namep points into a file name string
	mov 	edi, u.dirbuf + 2
		; mov $u.dirbuf+2,r3 / points to file name of directory entry
	; 18/10/2015
	mov	esi, [nbase]
	;mov	cx, [ncount]
	;and	cx, cx
	; 18/10/2021
	mov	ecx, [ncount]
	and	ecx, ecx
	jnz	short namei_5	
	;
	call	trans_addr_nm ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page
namei_5: ; 3:
	inc	ebp ; 18/07/2015
	lodsb   ; mov al, [esi] ; inc esi (al = r4)
		; movb (r2)+,r4 / move a character from u.namep string into r4
	or 	al, al
	jz 	short namei_7
		; beq 3f / if char is nul, then the last char in string
			; / has been moved
	cmp	al, '/'
		; cmp r4,$'/ / is char a </>
	je 	short namei_7
		; beq 3f	
	; 12/10/2015
	;dec	cx ; remain byte count in the page
	dec	ecx ; 18/10/2021
	jnz	short namei_6
	call	trans_addr_nm ; convert virtual address to physical
		; esi = physical address (page start + offset)
		; ecx = byte count in the page
namei_6:
        cmp     edi, u.dirbuf + 16 ; 04/12/2015 (10 -> 16) 
		; cmp r3,$u.dirbuf+10. / have I checked
				     ; / all 8 bytes of file name
	je	short namei_5
		; beq 3b
	scasb	
		; cmpb (r3)+,r4 / compare char in u.namep string to file name 
			      ; / char read from directory
	je 	short namei_5
		; beq 3b / branch if chars match
namei_10:
        jmp	namei_3 ; 2b
		; br 2b / file names do not match go to next directory entry
namei_7: ; 3:
	cmp	edi, u.dirbuf + 16 ; 04/12/2015 (10 -> 16) 
		; cmp r3,$u.dirbuf+10. / if equal all 8 bytes were matched
	je	short namei_8
		; beq 3f
	;mov 	ah, [edi]
	;;inc 	edi 
	;and 	ah, ah
	;	; tstb (r3)+ /
        ; 18/10/2021
	test	byte [edi], 0FFh
	; 08/01/2021
	jnz	short namei_10 
	;jnz	namei_3
		; bne 2b
namei_8: ; 3
	mov	[u.namep], ebp ; 18/07/2015
		; mov r2,u.namep / u.namep points to char 
			       ; / following a / or nul
	;mov	bx, [u.dirbuf]
		; mov u.dirbuf,r1 / move i-node number in directory 
				; / entry to r1

	;;;;
	; 15/05/2022 - Retro UNIX (8086/386) feature only !
	; ! 'pwd' utility modification !
	; ((if directory entry name is a dotdot)))
	;; check if it is mounted device's root directory inode
	; and if so, replace it with parent dir inode number
	;  of mounting directory in [mntp].

	movzx	ebx, word [u.dirbuf]

	cmp	bx, 1 ; root directory inode number
	jne	short namei_11

	cmp	ebx, [ii] ; for root dir, '.' & '..' is 1
	jne	short namei_11 ; not root dir (of mounted dev)

	;cmp	[idev], bh ; 0
	cmp	[cdev], bh ; 0
			; 0 = root fs, dev num in [rdev]
			; 1 = mounted, dev num in [mdev]
	jna	short namei_11

	; dotdot (parent directory link) check
	cmp	word [u.dirbuf+2], '..'
	jne	short namei_11
	cmp	byte [u.dirbuf+4], 0
	jne	short namei_11
	
	; (This may not be necessary because [idev] = 1
	; and [mnti] is expected as a sub dir inode number)
	cmp	[mnti], ebx ; 1
	jna	short namei_11
	
	; change inumber to parent dir inum of mount directory
	mov	ebx, [mntp]
	mov	byte [cdev], 0 ; root fs
namei_11:
	;;;;

	and 	al, al
		; tst r4 / if r4 = 0 the end of file name reached,
		      ;  / if r4 = </> then go to next directory
	; 15/05/2022
	mov	eax, ebx
	;;mov	ax, bx
	;;mov 	ax, [u.dirbuf] ; 17/06/2015
        ;; 18/10/2021 (16 bit inode number in 32 bit register)
	;movzx	eax, word [u.dirbuf] 
	; 19/12/2021
	jmp	namei_9 ; eax = inode number
;	jnz	namei_2 
;		; bne 1b
;	; AX = i-number of the file
;;;nig:
;	retn
		; tst (r0)+ / gives non-error return
;;nib:
;;	xor	ax, ax ; Retro UNIX 8086 v1 modification !
		       ; ax = 0 -> file not found 
;;	stc	; 27/05/2013
;;	retn
		; rts r0

trans_addr_nmbp:
	; 18/10/2021 - Retro UNIX 386 v2
	; 18/10/2015
	; 12/10/2015
	mov 	ebp, [u.namep]
trans_addr_nm: 
	; 18/10/2021 - Retro UNIX 386 v2
	; Convert virtual (pathname) address to physical address
	; (Retro UNIX 386 v1 feature only !)
	; 18/10/2015
	; 12/10/2015 (u.pnbase & u.pncount has been removed from code)
	; 02/07/2015
	; 17/06/2015
	; 16/06/2015
	;
	; INPUTS: 
	;	ebp = pathname address (virtual) ; [u.namep]
	;	[u.pgdir] = user's page directory
	; OUTPUT:
	;       esi = physical address of the pathname
	;	ecx = remain byte count in the page
	;
	; (Modified registers: EBX, ECX, EDX, ESI) ; 18/10/2021
	;

	; 18/10/2021
	sub	ecx, ecx
	;
        ;cmp	dword [u.ppgdir], 0  ; /etc/init ? (sysexec)
	cmp	[u.ppgdir], ecx ; 0 ; 18/10/2021
	jna	short trans_addr_nmk ; the caller is os kernel;
				     ; it is already physical address
   	push	eax	
	mov	ebx, ebp ; [u.namep] ; pathname address (virtual)
       	call	get_physical_addr ; get physical address
	jc	short tr_addr_nm_err
	; 18/10/2015
	; eax = physical address 
	; ecx = remain byte count in page (1-4096) ; 18/10/2021
		; 12/10/2015 (cx = [u.pncount])
	mov	esi, eax ; 12/10/2015 (esi=[u.pnbase])
	pop	eax 
	retn

tr_addr_nm_err:
	mov	[u.error], eax
	;pop 	eax
	jmp	error

trans_addr_nmk:
	; 12/10/2015
	; 02/07/2015
	mov	esi, [u.namep]  ; [u.pnbase]
	;mov	cx, PAGE_SIZE ; 4096 ; [u.pncount]
	; 18/10/2021
	mov	ch, PAGE_SIZE/256
	retn

	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
chk_dir:
	; 12/06/2021
	; 27/03/2021 (Retro UNIX 386 v2)
	; Check for directory inode

	mov	cl, [i.flgs+1]
	;test	cl, 80h ; IFREG ; 80h
	;jz	short chk_dir_err ; not a regular file ! (device!?)
	;and	cl, 40h ; IFDIR ; 40h
	;jz	short chk_dir_err ; not a sub directory !
	shl	cl, 1 ; 80h (IFREG flag) -> cf = 1
	jnc	short chk_dir_err ; not a regular file ! (device!?)
	shl	cl, 1 ; 40h (IFDIR flag) -> cf = 1
	jnc	short chk_dir_err ; not a sub directory !
	; 12/06/2021
	retn
chk_dir_err:
	mov	dword [u.error], ERR_NOT_DIR ; 'not a valid directory !'
	jmp	error

	; 06/02/2022
	; 08/01/2022
	; 01/01/2022 - Retro UNIX 386 v1.2 (runix v2 fs inode)
syschdir:
	; / makes the directory specified in the argument
	; / the current directory
	;
	; 08/01/2022 (Retro UNIX 386 v1.2)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 19/06/2013 (Retro UNIX 8086 v1)
	;
	; 'syschdir' makes the directory specified in its argument
	; the current working directory.
	;
	; Calling sequence:
	;	syschdir; name
	; Arguments:
	;	name - address of the path name of a directory
	;	       terminated by nul byte.	
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification:
	;	 The user/application program puts address of 
	;	 the path name in BX register as 'syschdir' 
	; 	 system call argument.

	mov	[u.namep], ebx
		;jsr r0,arg; u.namep / u.namep points to path name
	call	namei
		; jsr r0,namei / find its i-number
	;jc	error
		; br error3
	jnc	short syschdir0
	; 'directory not found !' error
	mov	dword [u.error], ERR_DIR_NOT_FOUND ; 12
	jmp	error
syschdir0:
	; 08/01/2022
	mov	dx, 100h  ; read access ; IREAD (100h)
	call	access
		; jsr r0,access; 2 / get i-node into core
	
	; 06/02/2022
	;; 01/01/2022 (runix v2 fs inode flag)
	;test	byte [i.flgs+1], 80h ; regular file ?
	;jz	short syschdir2 ; no, error!
	;test	byte [i.flgs+1], 40h ; directory flag ?
	;;test	word [i.flgs], 4000h
	;	; bit $40000,i.flgs / is it a directory?
	;;jz	error 
	;	; beq error3 / no error
	;jnz	short syschdir1
;syschdir2:
	;mov	dword [u.error], ERR_NOT_DIR ; 'not a valid directory !'
	;jmp	error

	; 06/02/2022
	; check for directory, jump to 'error' if not
	call	chk_dir
	; (cpu will not return here
	;  if the inode it is not a valid dir inode)

syschdir1:
	mov	[u.cdir], ax
		; mov r1,u.cdir / move i-number to users 
			      ; / current directory
	;mov	ax, [cdev]
	;mov	[u.cdrv], ax
	; 08/01/2022
	mov	al, [cdev]
	mov	[u.cdrv], al
		; mov cdev,u.cdev / move its device to users 
			        ; / current device
	jmp	sysret
		; br sysret3

syschmod: ; < change mode of file >
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 01/05/2021
	; 25/05/2020 (Retro UNIX 386 v2 - Beginning)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 - 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'syschmod' changes mode of the file whose name is given as
	; null terminated string pointed to by 'name' has it's mode 
	; changed to 'mode'.
	;
	; Calling sequence:
	;	syschmod; name; mode
	; Arguments:
	;	name - address of the file name
	;	       terminated by null byte.
	;	mode - (new) mode/flags < attributes >
	;	
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'syschmod' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, mode is in CX register
	;
	; Mode bits (Flags):
	;	bit 15 - 'i-node is allocated' flag (8000h)
	;	bit 14 - directory flag (4000h)
	;	bit 13 - file has modified flag (always on) (2000h)
	;	bit 12 - large file flag (1000h)
	;	bit 6,7,8,9,10,11 are not used (undefined)
	;	bit 5 - set user ID on execution flag (20h) 
	;	bit 4 - executable flag (10h)
	;	bit 3 - read permission for owner (08h)
	;	bit 2 - write permission for owner (04h)
	;	bit 1 - read permission for non-owner (02h)
	;	bit 0 - write permission for non-owner (01h)

	; 01/05/2021 (07/02/2020)
	; Retro UNIX 386 v2 I-node Flags: (di_mode) for files
	; 	bit 15 - IFREG - regular file (8000h)
	;	bit 14 - IFDIR - directory (4000h)
	;	bit 13 - IRSVD - 0 - reserved/mounted bit (2000h)
	; 	bit 12 - ILARG - large file addressing bit (1000h)
	; 	bit 11 - ISUID - set user id on exec (800h)
	;	bit 10 - ISGID - set group id on exec (400h)
	; 	bit  9 - IEXTT - 0 - use extents (200h)
	;	bit  8 - IREAD - read, owner (100h)
	;	bit  7 - IWRITE - write, owner (80h)
	;	bit  6 - IEXEC - execute, owner (40h)
	; 	bit  5 - read, group (20h)
	; 	bit  4 - write, group (10h)
	; 	bit  3 - execute, group (08h)
	;	bit  2 - read, others (04h)
	; 	bit  1 - write, others (02h)
	; 	bit  0 - execute, others (01h)
	;
	; Retro UNIX 386 v2 I-node Flags: (di_mode) for devices
	;	bit 15 - IFREG - 0 - device file (8000h)
	; 	bit 14 - IFBLK - block device (4000h)
	; 	bit 13 - IFCHR - 1 - character special (2000h)
	;	bit 12 - IFIFO - fifo special (1000h)
	; 	bit 11 - IPIPE - pipe special (800h)
	;	bit 10 - IREDIR - redirected (400h)
	;	bit  9 - IEXTR - external device driver (200h)
	;	bit  8 - IREAD - read, owner (100h)
	;	bit  7 - IWRITE - write, owner (80h)
	;	bit  6 - IEXEC - execute, owner (40h)
	; 	bit  5 - read, group (20h)
	; 	bit  4 - write, group (10h)
	; 	bit  3 - execute, group (08h)
	;	bit  2 - read, others (04h)
	; 	bit  1 - write, others (02h)
	; 	bit  0 - execute, others (01h)

	; / name; mode
	call	isown
		; jsr r0,isown / get the i-node and check user status

	; 25/05/2020
	; AL = new mode (return form 'isown', ecx -> eax)

	;test	word [i.flgs], 4000h
	;	; bit $40000,i.flgs / directory?
	;jz	short syschmod1
	;	; beq 2f / no
	;; AL = (new) mode
	;and	al, 0CFh ; 11001111b (clears bit 4 & 5)
	;	; bic $60,r2 / su & ex / yes, clear set user id and 
	;		   ; / executable modes

	; 01/05/2021
	; AX = new mode (9 bits for devices or 12 bits files)
	mov	dl, [i.flgs+1]
	; 09/01/2022
	test	dl, 80h ; regular file
	jz	short syschmod1 ; device file
	test	dl, 40h ; directory
	jnz	short syschmod1
	; regular file
	and	ax, 0DFFh ; clear bit 9, 12-15 (of new mode)
	; clear bit 8,10,11 of current mode/flags
	and	dl, 0F2h ; and dl, 11110010b
syschmod0:
	or	ah, dl
	mov	[i.flgs], ax	
	jmp	short syschmod2
syschmod1: 
	; device or directory	
	and	ax, 1B6h ; clear bit 9-15 & bit 0,3,6 (EXEC bits)
	; clear bit 8 (IREAD) of current mode/flags
	;	 (dl contains flag bits 8-15)
	and	dl, ~1 ; and dl, 11111110b 
	jmp	short syschmod0

;syschmod1: ; 2:
	;mov	[i.flgs], al	
	;	; movb r2,i.flgs / move remaining mode to i.flgs

	; 25/05/2020 - Retro UNIX 386 v2
	;mov	byte [imodx], 1 ; (flag means file data is same
	;			;  but inode's itself has been modified)
	;call	setimod
	;jmp	sysret

	; 25/05/2020
	;jmp	short syschmod2

syschown: ; < change owner of file >
	; 13/03/2022
	; 11/03/2022
	; 14/02/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 01/05/2021
	;	(change owner and group ID of file)
	; 02/04/2021
	; 25/05/2020 (Retro UNIX 386 v2 - Beginning)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'syschown' changes the owner of the file whose name is given
	; as null terminated string pointed to by 'name' has it's owner
	; changed to 'owner'
	;
	; Calling sequence:
	;	syschown; name; owner
	; Arguments:
	;	name - address of the file name
	;	       terminated by null byte.
	;	owner - (new) owner (number/ID)
	;	
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;				
	; Retro UNIX 8086 v1 modification: 
	;       'syschown' system call has two arguments; so,
	;	* 1st argument, name is pointed to by BX register
	;	* 2nd argument, owner number is in CX register
	;
	; / name; owner
	call	isown
		; jsr r0,isown / get the i-node and check user status
	
	; 25/05/2020
	; AL = owner number (return form 'isown', ecx -> eax)
	; 02/04/2021
	; AX = owner number
	; 01/05/2021
	; byte 2 of EAX is group number

	;cmp 	byte [u.uid], 0 ; 02/08/2013 
	;	; tstb u.uid / super user
	;jz	short syschown1
	;	; beq 2f / yes, 2f
	cmp	word [u.uid], 0
	jz	short syschown1

	;test	byte [i.flgs], 20h ; 32
	;	; bit $40,i.flgs / no, set userid on execution?
	;;jnz	error
	;	; bne 3f / yes error, could create Trojan Horses
	;jz	short syschown1
	; 01/05/2021
	; check set user id on execution flag
	; (protection against Trojan Horses)

	; ((Note: UNIX v7 x86 'chown' source code in 'sys4.c'
	;        does not contain this protection)) 

	; 14/02/2022
	mov	dl, [i.flgs+1]
	test	dl, 80h ; IFREG ; RUNIX v2 inode flags  	
	jz	short syschown_err ; device file !
	; 09/01/2022
	;test	byte [i.flgs+1], 08h ; ISUID ; RUNIX v2 inode flags  	
	; 14/02/2022
	test	dl, 08h	; set user id on execution
	jz	short syschown1 ; 11/03/2022
		; 11/03/2022
		; bit $40,i.flgs / no, set userid on execution?
		; bne 3f / yes error, could create Trojan Horses
syschown_err:
	; 'permission denied !'
	mov	dword [u.error], ERR_FILE_ACCESS  ; 11
	jmp	error
syschown1: ; 2:
	;; AL = owner (number/ID)
	;mov	[i.uid], al ; 23/06/2015
	;	; movb r2,i.uid / no, put the new owners id 
	;		      ; / in the i-node
	; 02/04/2021 (Retro UNIX 386 v2)
	; AX = owner number
	mov	[i.uid], ax ; owner
	; 13/03/2022
	;; 14/02/2022 (Retro UNIX 386 v1.2)
	;test	dl, 04h ; ISGID ; set grup id on execution
	;jz	short syschown2
	; 01/05/2021 (Retro UNIX 386 v2)
	shr	eax, 16
	mov	[i.gid], al ; group
syschown2:
	; 25/05/2020 - Retro UNIX 386 v2
syschmod2:
	; 09/01/2022 (Retro UNIX 386 v1.2)
	mov	byte [imodx], 1	; (flag means file data is same
			      ;  but inode's itself has been modified)
	call	setimod ; 25/05/2020
	
	jmp	sysret

	; 1: 
		; jmp sysret4
	; 3:
		; jmp	error

isown:
	; 11/03/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 06/11/2021
	; 12/06/2021
	; 01/05/2021
	; 25/05/2020 (Retro UNIX 386 v2 - Beginning)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 04/05/2013 - 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'isown' is given a file name (the 1st argument).
	;  It find the i-number of that file via 'namei' 
	;  then gets the i-node into core via 'iget'.
	;  It then tests to see if the user is super user. 
	;  If not, it checks to see if the user is owner of 
	;  the file. If he is not an error occurs.
	;  If user is the owner 'setimod' is called to indicate
	;  the inode has been modified and the 2nd argument of
	;  the call is put in r2.
	;
	; INPUTS ->
	;    arguments of syschmod and syschown calls
	; OUTPUTS ->
	;    u.uid - id of user
	;    imod - set to a 1
	;    r2 - contains second argument of the system call
	;
	;   ((AX=R2) output as 2nd argument)
	;
        ; ((Modified registers: eAX, eDX, eBX, eCX, eSI, eDI, eBP))
	;
		; jsr r0,arg2 / u.namep points to file name
	;; ! 2nd argument on top of stack !
	;; 22/06/2015 - 32 bit modifications
	;; 07/07/2013
	mov	[u.namep], ebx ;; 1st argument
	push 	ecx ;* ; 2nd argument
	;;
	call	namei
		; jsr r0,namei / get its i-number
       ; Retro UNIX 8086 v1 modification !
       ; ax = 0 -> file not found 
	;and	ax, ax
	;jz	error
	;jc	error ; 27/05/2013
		; br error3
	jnc	short isown1  ; 25/05/2020 (isown0 -> isown1)
	; 'file not found !' error
	mov	dword [u.error], ERR_FILE_NOT_FOUND ; 12
	jmp	error
isown1:
	call	iget
		; jsr r0,iget / get i-node into core
	; 09/01/2022 - Retro UNIX 386 v1.2
	;; 06/11/2021
	;jnc	short isown3
	;mov	[u.error], eax
	;jmp	error
;isown3:
	; check if it is super user ID
	; 09/01/2022 (Retro UNIX 386 v2 fs inode structure)
	mov	ax, [u.uid]
	or	ax, ax
	;mov	al, [u.uid] ; 02/08/2013
	;or	al, al
		; tstb u.uid / super user?
	; 12/06/2021
	jz	short isown2 ; 25/05/2020 (isown1 -> isown2)
		; beq 1f / yes, branch
	
	; check user ID	(if same with file's owner)
	cmp	ax, [i.uid] ; 09/01/2022
	;cmp	al, [i.uid]
		; cmpb i.uid,u.uid / no, is this the owner of
				 ; / the file
	;jne	error
		; beq 1f / yes
		; jmp error3 / no, error
	; 11/03/2022
	je	short isown2 ; 25/05/2020
	;; 01/05/2021
	;jne	short isown_err

	; 11/03/2022 (*)
	; Note: It is seen as original unix (v5-v7) kernel
	;	source handles 'u.gid' for group permissions
	;	but kernel uses 'u.uid' as primary and unic
	;	(singular) user id/number; a user group
	;	does/can not contain same user id/number
	;	with anotner group. So, if active/current
	;	user ID is same with file's owner id,
	;	group id check is not needed.
	;	([u.uid] = [i.uid] is enough to confirm)	

	; 01/05/2021
	;mov	ax, [u.uid]
	;or	ax, ax
	;jz	short isown2 
	;cmp	ax, [i.uid]
	;;je	short isown2
	;jne	short isown_err

	; 01/05/2021
	; ((Note: UNIX v7 x86 'chown', 'chmod' procedures
	;    and their sub procedures do not check group ID))
	; (('sys4.c', 'fio.c'))

	; 11/03/2022 (*)
	; 01/05/2021
	; check group ID (if same with group of file's owner)
	;mov	al, [u.gid]
	;cmp	al, [i.gid]
	;je	short isown2
isown_err:
	mov	dword [u.error], ERR_NOT_OWNER ; 11
			; 'permission denied !' error
	jmp	error

	; 25/05/2020
;isown2: ; 1:
;	call	setimod
;		; jsr r0,setimod / indicates 
;		;	       ; / i-node has been modified
isown2: 
	; 25/05/2020
	pop	eax ;* ; 2nd argument
		; mov (sp)+,r2 / mode is put in r2 
		       ; / (u.off put on stack with 2nd arg)
	retn

;;arg:  ; < get system call arguments >
	; 'arg' extracts an argument for a routine whose call is 
	; of form:
	;	sys 'routine' ; arg1
	;		or
	;	sys 'routine' ; arg1 ; arg2
	;		or
	;	sys 'routine' ; arg1;...;arg10 (sys exec) 
	;	
	; INPUTS ->
	;    u.sp+18 - contains a pointer to one of arg1..argn
	;	This pointers's value is actually the value of
	;	update pc at the the trap to sysent (unkni) is
	;	made to process the sys instruction
	;    r0 - contains the return address for the routine
	;	that called arg. The data in the word pointer 
	;	to by the return address is used as address
	;	in which the extracted argument is stored
	;    	
	; OUTPUTS ->
	;    'address' - contains the extracted argument 
	;    u.sp+18 - is incremented by 2 
	;    r1 - contains the extracted argument
	;    r0 - points to the next instruction to be
	;	 executed in the calling routine.
	;
  
	; mov u.sp,r1
	; mov *18.(r1),*(r0)+ / put argument of system call
			; / into argument of arg2
	; add $2,18.(r1) / point pc on stack 
			      ; / to next system argument
	; rts r0

;;arg2: ; < get system calls arguments - with file name pointer>
	; 'arg2' takes first argument in system call
	;  (pointer to name of the file) and puts it in location
	;  u.namep; takes second argument and puts it in u.off
	;  and on top of the stack
	;	
	; INPUTS ->
	;    u.sp, r0
	;    	
	; OUTPUTS ->
	;    u.namep
	;    u.off 
	;    u.off pushed on stack
	;    r1
	;

	; jsr	r0,arg; u.namep / u.namep contains value of
				; / first arg in sys call
	; jsr r0,arg; u.off / u.off contains value of 
				; / second arg in sys call
	; mov r0,r1 / r0 points to calling routine
	; mov (sp),r0 / put operation code back in r0
	; mov u.off,(sp) / put pointer to second argument 
			; / on stack
	; jmp (r1) / return to calling routine

systime: ; / get time of year
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 (Retro UNIX 8086 v1)
	;
	; 20/06/2013
	; 'systime' gets the time of the year.
	; The present time is put on the stack.
	;
	; Calling sequence:
	;	systime
	; Arguments: -
	;	
	; Inputs: -
	; Outputs: sp+2, sp+4 - present time
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'systime' system call will return to the user
	;	with unix time (epoch) in DX:AX register pair
	;
	; 	!! Major modification on original Unix v1 'systime' 
	;	system call for PC compatibility !!		 	

	call 	epoch
	mov 	[u.r0], eax
		; mov s.time,4(sp)
		; mov s.time+2,2(sp) / put the present time 
				   ; / on the stack
		; br sysret4
	jmp	sysret 

sysstime: ; / set time
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysstime' sets the time. Only super user can use this call.
	;
	; Calling sequence:
	;	sysstime
	; Arguments: -
	;	
	; Inputs: sp+2, sp+4 - time system is to be set to.
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;	the user calls 'sysstime' with unix (epoch) time
	;	(to be set) is in CX:BX register pair as two arguments.
	; 
	;	Retro UNIX 8086 v1 argument transfer method 2 is used
	;	to get sysstime system call arguments from the user;
	;	* 1st argument, lowword of unix time is in BX register
	;	* 2nd argument, highword of unix time is in CX register		 	
	;
	; 	!! Major modification on original Unix v1 'sysstime' 
	;	system call for PC compatibility !!	

	; 09/01/2022 (Retro UNIX 386 v2 fs inode structure)
	cmp	word [u.uid], 0 ; 16 bit user ID
	;cmp	byte [u.uid], 0
		; tstb u.uid / is user the super user
	;ja	error
		; bne error4 / no, error
	jna	short systime1
	; 'permission denied !'
	mov	dword [u.error], ERR_NOT_SUPERUSER  ; 11 
	jmp	error
systime1:
	; 23/06/2015 (Retro UNIX 386 v1 - 32 bit version)
	; EBX = unix (epoch) time (from user)
	mov	eax, ebx
	call 	set_date_time
		; mov 4(sp),s.time
		; mov 2(sp),s.time+2 / set the system time
	jmp	sysret
		; br sysret4

sysbreak:
	; 18/10/2015
	; 07/10/2015
	; 23/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 20/06/2013 - 24/03/2014 (Retro UNIX 8086 v1)
	;
	; 'sysbreak' sets the programs break points. 
	; It checks the current break point (u.break) to see if it is
	; between "core" and the stack (sp). If it is, it is made an
	; even address (if it was odd) and the area between u.break
	; and the stack is cleared. The new breakpoint is then put
	; in u.break and control is passed to 'sysret'.
	;
	; Calling sequence:
	;	sysbreak; addr
	; Arguments: -
	;	
	; Inputs: u.break - current breakpoint
	; Outputs: u.break - new breakpoint 
	;	area between old u.break and the stack (sp) is cleared.
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification:
	;	The user/application program puts breakpoint address
	;       in BX register as 'sysbreak' system call argument.
	; 	(argument transfer method 1)
	;
	;  NOTE: Beginning of core is 0 in Retro UNIX 8086 v1 !
	; 	((!'sysbreak' is not needed in Retro UNIX 8086 v1!))
	;  NOTE:
	; 	'sysbreak' clears extended part (beyond of previous
	;	'u.break' address) of user's memory for original unix's
	;	'bss' compatibility with Retro UNIX 8086 v1 (19/11/2013)

		; mov u.break,r1 / move users break point to r1
		; cmp r1,$core / is it the same or lower than core?
		; blos 1f / yes, 1f
	; 23/06/2015
	mov	ebp, [u.break] ; virtual address (offset)
	;and	ebp, ebp
	;jz	short sysbreak_3 
	; Retro UNIX 386 v1 NOTE: u.break points to virtual address !!!
	; (Even break point address is not needed for Retro UNIX 386 v1)
	mov	edx, [u.sp] ; kernel stack at the beginning of sys call
	add	edx, 12 ; EIP -4-> CS -4-> EFLAGS -4-> ESP (user) 
	; 07/10/2015
	mov	[u.break], ebx ; virtual address !!!
	;
	cmp	ebx, [edx] ; compare new break point with 
			   ; with top of user's stack (virtual!)
	jnb	short sysbreak_3
		; cmp r1,sp / is it the same or higher 
			  ; / than the stack?
		; bhis 1f / yes, 1f
	mov	esi, ebx
	sub	esi, ebp ; new break point - old break point
	jna	short sysbreak_3 
	;push	ebx
sysbreak_1:
	mov	ebx, ebp  
	call	get_physical_addr ; get physical address
	jc	tr_addr_nm_err
	; 18/10/2015
	mov	edi, eax 
	sub	eax, eax ; 0
		 ; ECX = remain byte count in page (1-4096)
	cmp	esi, ecx
	jnb	short sysbreak_2
	mov	ecx, esi
sysbreak_2:
	sub	esi, ecx
	add	ebp, ecx
	rep 	stosb
	or	esi, esi
	jnz	short sysbreak_1
	;
		; bit $1,r1 / is it an odd address
		; beq 2f / no, its even
		; clrb (r1)+ / yes, make it even
	; 2: / clear area between the break point and the stack
		; cmp r1,sp / is it higher or same than the stack
		; bhis 1f / yes, quit
		; clr (r1)+ / clear word
		; br 2b / go back
	;pop	ebx
sysbreak_3: ; 1:
	;mov	[u.break], ebx ; virtual address !!!
		; jsr r0,arg; u.break / put the "address" 
			; / in u.break (set new break point)
		; br sysret4 / br sysret
	jmp	sysret

maknod: 
	; 18/07/2022
	; 12/03/2022
	; 11/03/2022
	; 14/02/2022
	; 10/01/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 06/11/2021 (Retro UNIX 386 v2)
	; 15/08/2021
	; 06/05/2021
	; 02/05/2021
	; 01/04/2021
	; 27/03/2021
	; 25/03/2021 (Retro UNIX 386 v2 - Beginning)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 02/05/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'maknod' creates an i-node and makes a directory entry
	; for this i-node in the current directory.
	;
	; INPUTS ->
	;    r1 - contains mode
	;    ii - current directory's i-number	
	;    	
	; OUTPUTS ->
	;    u.dirbuf - contains i-number of free i-node 
	;    i.flgs - flags in new i-node 
	;    i.uid - filled with u.uid
	;    i.nlks - 1 is put in the number of links
	;    i.ctim - creation time
	;    i.ctim+2 - modification time
	;    imod - set via call to setimod
	;	
	; ((AX = R1)) input
	;
	; (Retro UNIX Prototype : 
	;	30/10/2012 - 01/03/2013, UNIXCOPY.ASM)
        ; ((Modified registers: eax, edx, ebx, ecx, esi, edi, ebp))

	; 27/03/2021
	; INPUT:
 	;	AX = mode
	;
	; ('maknod' is called by 'syscreat' and 'sysmkdir')
	; (('syscreat' and 'sysmkdir' will set AX input))

	; 27/03/2021
	; Retro UNIX 386 v2 inode mode flags (ref: 'ux.s')
	; for File Inode: (high byte)
	;   IFREG - 1 = regular file (8000h)
	;   IFDIR - 1 = directory (4000h)
	;   IRSVD - 0 = reserved bit (2000h) ; Mounted flag for dirs
	;   ILARG - large file addressing bit (1000h)
	;   ISUID - set user id on exec (800h)
	;   ISGID - set group id on exec (400h)
	;   IEXTT - 1 = use extents (200h)
	;   IREAD - read, owner (100h)
	; for Device Inode: (high byte)
	;   IFREG - 0 = device file (8000h)
	;   IFBLK - 1 = block device (4000h)
	;   IFCHR - character special (2000h) -always 1-
	;   IFIFO - fifo special (1000h)
	;   IPIPE - pipe special (800h) ; 07/02/2020
	;   IREDIR - redirected (400h)  ; 07/02/2020
	;   IEXTR - 1 = external device driver (200h)
	;   IREAD - read, owner (100h)

	;; / r1 contains the mode
	;or 	ah, 80h	; 10000000b
	;	; bis $100000,r1 / allocate flag set

	; 14/02/2022
	; input ->
	;   [u.namep] points to the file name address
	;	     (in the user's memory space)
	;   [u.dirp] points to empty slot in the directory

	; high 3 bit will be checked here
	; 	(ref: unix v7 x86 source, iget.c)

;	; 27/03/2021
;	test	ah, 0E0h ; 80h+40h+20h
;	jnz	short maknod0	
;
;	; ('syscreat' clears ILARG & IEXTT bits)
;	;and	ah, ~0Ch ; clear ILARG & IEXTT bits
;	;	; user can set ISUID & ISGID bits
;
;	or	ah, 80h ; IFREG
;maknod0:
	push	eax ; ***** ; 27/03/2021 (32 bit push, pop)
		; mov r1,-(sp) / put mode on stack
	; 31/07/2013
	;mov	ax, [ii] ; move current i-number to AX/r1
	;	; mov ii,r1 / move current i-number to r1
	;mov	dl, 1 ; owner flag mask
	; 15/08/2021
	;movzx	eax, word [ii] ; move current i-number to EAX
	; 09/01/2022
	mov	eax, [ii] ; move current i-number to EAX
	; 25/03/2021
	mov	dx, 80h	; IWRITE - write,owner
	call	access	
		; jsr r0,access; 1 / get its i-node into core
	; 15/08/2021
	; NOTE: cpu will not return here if there is a permission error
	;	(it will jump to 'error' address from/in 'access')
	
	push	eax ; **** (parent) ; 27/03/2021 (32 bit push, pop)
		; mov r1,-(sp) / put i-number on stack
	;mov	ax, 40
	;	; mov $40.,r1 / r1 = 40
	; 27/03/2021
	xor	eax, eax
	;;dec	ax ; 0FFFFh
	; 15/08/2021
	;dec	eax ; 0FFFFFFFFh
	; 01/04/2021
;maknod1: ; 1: / scan for a free i-node (next 4 instructions)
	;;inc	ax
	;	; inc r1 / r1 = r1 + 1
	; 15/08/2021
	;inc	eax
	; 27/03/2021 - Retro UNIX 386 v2
	; eax = 0 -> start from first free inode
	; eax > 0 -> locate this inode on the inode map buffer
	; NOTE:
	; Retro UNIX 386 v2 'imap' is mostly different than v1 'imap'
	; ('imap' will not check inode table)
	call	imap
		; jsr r0,imap / get byte address and bit position in 
			    ; /	inode map in r2 & m
	; 09/01/2022
	; (cpu will not return here if there would be an eror in imap)
	; 15/08/2021
	;jc	short maknod_err

	; DX (MQ) has a 1 in the calculated bit position
        ; eBX (R2) has byte address of the byte with allocation bit

	; 06/11/2021
	; ebp = superblock buffer address
	; eax = inode number
	; 10/01/2022
	; [ebp+SB.ImapBuffer] = physical block/sector number
	;		of current IMAP sector	

	; 27/03/2021
	; If cpu is here, there is not an error
	; and EBX points to inode map buffer
	; DL has 1 at bit position which is for free inode
	; (E)AX = inode number
	; 10/01/2022 (Retro UNIX 386 v1.2)
	; ECX = byte offset from the (start of) inode map buffer

	; 15/08/2021
maknod1:
	; 22/06/2015 - NOTE for next Retro UNIX version: 
	;	       Inode count must be checked here
	; (Original UNIX v1 did not check inode count here !?)
	test	[ebx], dl
		; bitb mq,(r2) / is the i-node active
	;jnz	short maknod1
	;	; bne 1b / yes, try the next one
	; 15/08/2021
	jz	short maknod3 ; free inode (inactive inode)
maknod2:
	inc	eax
	call	imap_x ; next call to imap (bypass 1st call code)
	; 10/01/2022
	; (cpu will/would not return here 
	;  if there is/was an error in 'imap_x')
	;jnc	short maknod1
	jmp	short maknod1

	; 14/02/2022
;maknod_err:
;	; 15/08/2021
;	;pop	edx  ; two pops for stack alignment
;	;pop	edx  ; (may not be needed while jumping to 'error')
;	mov	[u.error], eax
;	jmp	error
maknod3:
	; 10/01/2022
	; ebp = superblock buffer address
	; eax = (free) inode number
	; [ebp+SB.ImapBuffer] = physical block/sector number
	;			of current IMAP sector
	; EBX = buffer address/offset for relevant alloc byte
	;  DL has 1 at bit position which is for free inode
	;
	; ECX = byte offset from the (start of) inode map buffer

	;mov	[ebp+SB.LastInode], eax ; (free) inode number

	push	eax ; ***
	push	ecx ; **
	push	edx ; *
	; 18/07/2022
	;push	ebp ; @ ; 11/03/2022

	mov	eax, [ebp+SB.ImapBuffer]

	call	wslot
	; ebx = buffer data address (write operation bit is set)
	; eax = physical sector number
	; Note: ebx contains addr of the 1st buffer in the buf chain
	; (the last allocated buffer becomes the 1st in buffer chain)

	; 18/07/2022
	;pop	ebp ; @ ; 11/03/2022

	;mov	[ebp+SB.ImapBuffer], ebx
			; save inode map buffer address
	pop	edx ; *
	pop	ecx ; **
	add	ebx, ecx ; + byte offset (for allocation bit pos)	

	; set allocation bit (for this new inode) ; 10/01/2022 
	or	[ebx], dl
		; bisb mq,(r2) / no, make it active 
			     ; / (put a 1 in the bit map)
	;push	ebp ; @
	call	dskwr ; writes the 1st buffer to sector
		      ; (at the beginning/head of the buffer chain)
	;pop	ebp ; @

	; if we are here, buffer has been written to disk successfully 
	; (otherwise cpu would jump to 'error' address)

	; set superblock modified flag
	mov	edx, smod
	test	byte [cdev], 1 ; mounted device ?
	jz	short maknod4 ; no, root device
	; yes, mounted device
	;mov	edx, mmod
	inc	edx ; edx = offset mmod
maknod4: 
	inc	byte [edx] ; superblock modified !

	; 09/01/2022 (Retro UNIX 386 v1.2)
	;;
	;; 27/03/2021
	;; (ref: 'UNIXHDCP.ASM', 'mak_nod', 18/01/2020)
	;or	byte [smod], 2 ; inode map modified
	;; 01/04/2021
	;;or	byte [imapbuf_hdr+bufhdr.status], 2
	;or	byte [imapbuf_hdr], 2 ; set modified flag bit
	; 02/05/2021
	;or	byte [esi+ldrv.b_status], 2 ; inode map modified
	; 15/08/2021
	; 06/05/2021
	;or	byte [esi+ldrv.status], 2 ; inode map modified
	;;or	byte [sysbuf_hdr], 2 ; set modified flag bit
	
	;mov	eax, [ebp+SB.LastInode] ; (free) inode number
	;push	eax ; ***

	; 12/03/2022
	; increase first free inode number
	mov	eax, [esp] ; ***
	inc	eax ; next inode number 
		    ; (free inode search start value)
	mov	[ebp+SB.FirstFreeIno], eax

	; 12/03/2022
	; decrease free inode count
	mov	edx, [ebp+SB.FreeInodes]
	;cmp	edx, 0FFFFFFFFh
	;je	short maknod5 ; invalid
	inc	edx ; 0FFFFFFFFh -> 0
	jz	short maknod5 ; invalid
	dec	edx
	dec	edx
	mov	[ebp+SB.FreeInodes], edx ; -1
maknod5:
	call	get_system_time
		; eax = current time (as unix epoch time)
	mov	[ebp+SB.ModifTime], eax

	pop	eax ; *** ; inode number
	;;
	call	iget
		; jsr r0,iget / get i-node into core
	; 09/01/2022
	;  (If cpu is here, there was/is not an error!)
	; 06/11/2021
	;jc	short maknod_err
	
	;test	word [i.flgs], 8000h 
	;	; tst i.flgs / is i-node already allocated
	;jnz	short maknod1	
	;	; blt 1b / yes, look for another one
	; 27/03/2021
	; (Retro UNIX 386 v2 inode flags) 
	test	byte [i.flgs+1], 0E0h ; 80h+40h+20h
	;jnz	short maknod1 ; i-node already allocated
	;		      ; (as it is in inode table)
	; 15/08/2021
	jnz	short maknod2 ; defective inode map !?

	; AX = free inode number
	mov	[u.dirbuf], ax
		; mov r1,u.dirbuf / no, put i-number in u.dirbuf
	pop	eax ; **** (parent) ; 27/03/2021 (32 bit push, pop)
		; mov (sp)+,r1 / get current i-number back
	call	iget
		; jsr r0,iget / get i-node in core
	; 09/01/2022
	; 06/11/2021
	;jc	short maknod_err

	; 14/02/2022
	; here..
	;  [u.namep] points to the file name address
	;	     (in the user's memory space)
	;  [u.dirp] points to empty slot in the directory

	call	mkdir
		; jsr r0,mkdir / make a directory entry 
			     ; / in current directory
	; 09/01/2022
	;  (If cpu is here, there was/is not an error in 'mkdir'!)
	; 06/11/2021
	;jc	short maknod_err
	
	; (eax = 0)
	;movzx	eax, [u.difbuf] ; 06/11/2021
	mov	ax, [u.dirbuf]
		; mov u.dirbuf,r1 / r1 = new inode number
	call	iget
		; jsr r0,iget / get it into core
		; jsr r0,copyz; inode; inode+32. / 0 it out
	; 09/01/2022
	; 06/11/2021
	;jc	short maknod_err

	;mov	ecx, 8
	; 27/03/2021
	xor	ecx, ecx 
	mov	cl, 16 ; 64 bit inodes
	xor	eax, eax ; 0
	mov	edi, inode 
	rep	stosd
	;
	;pop	word [i.flgs]
	;	; mov (sp)+,i.flgs / fill flags
	; 27/03/2021 (32 bit push, pop)
	pop	eax ; *****
	mov	[i.flgs], ax

	;mov 	cl, [u.uid] ; 02/08/2013
	;mov 	[i.uid], cl
	;	; movb u.uid,i.uid / user id
	; 15/08/2021
	; 27/03/2021
	mov	cl, [u.gid] ; 8 bit group ID ; 15/08/2021
	mov	[i.gid], cl 	
	mov	cx, [u.uid] ; 16 bit user ID
	mov	[i.uid], cx	

	mov     byte [i.nlks], 1
		; movb $1,i.nlks / 1 link
	;call	epoch ; Retro UNIX 8086 v1 modification !
	;mov	eax, [s.time]
	;mov 	[i.ctim], eax
	 	; mov s.time,i.ctim / time created
	 	; mov s.time+2,i.ctim+2 / time modified
	; Retro UNIX 8086 v1 modification !
	; i.ctime=0, i.ctime+2=0 and
        ; 'setimod' will set ctime of file via 'epoch'
	;call	setimod
	;	; jsr r0,setimod / set modified flag
	;retn
	;	; rts r0 / return
	; 27/03/2021
	jmp	setimod

sysseek: ; / moves read write pointer in an fsp entry
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; 'sysseek' changes the r/w pointer of (3rd word of in an
	; fsp entry) of an open file whose file descriptor is in u.r0.
	; The file descriptor refers to a file open for reading or
	; writing. The read (or write) pointer is set as follows:
	;	* if 'ptrname' is 0, the pointer is set to offset.
	;	* if 'ptrname' is 1, the pointer is set to its
	;	  current location plus offset.
	;	* if 'ptrname' is 2, the pointer is set to the
	;	  size of file plus offset.
	; The error bit (e-bit) is set for an undefined descriptor.
	;
	; Calling sequence:
	;	sysseek; offset; ptrname
	; Arguments:
	;	offset - number of bytes desired to move 
	;		 the r/w pointer
	;	ptrname - a switch indicated above
	;
	; Inputs: r0 - file descriptor 
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysseek' system call has three arguments; so,
	;	* 1st argument, file descriptor is in BX (BL) register
	;	* 2nd argument, offset is in CX register
	;	* 3rd argument, ptrname/switch is in DX (DL) register	
	;	

	call	seektell
	; AX = u.count
	; BX = *u.fofp
		; jsr r0,seektell / get proper value in u.count
		; add u.base,u.count / add u.base to it
	add	eax, [u.base] ; add offset (u.base) to base
	mov	[ebx], eax
		; mov u.count,*u.fofp / put result into r/w pointer
	jmp	sysret
		; br sysret4

systell: ; / get the r/w pointer
	; 01/03/2022
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; Retro UNIX 8086 v1 modification:
	; ! 'systell' does not work in original UNIX v1,
	; 	    it returns with error !
	; Inputs: r0 - file descriptor 
	; Outputs: r0 - file r/w pointer

	;xor	ecx, ecx ; 0
	;mov	edx, 1 ; 05/08/2013
	; 09/01/2022
	sub	edx, edx
	inc	dl
	; edx = 1
	;call 	seektell
	call 	seektell0 ; 05/08/2013
	;mov	ebx, [u.fofp]
	; 01/03/2022
	;mov	eax, [ebx]
	mov	[u.r0], eax
	jmp	sysret

; Original unix v1 'systell' system call:
		; jsr r0,seektell
		; br error4

seektell:
	; 09/01/2022 (Retro UNIX 386 v1.2)
	; 06/11/2021
	; 12/06/2021
	; 06/05/2021 (Retro UNIX 386 v2)
	; 03/01/2016
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 05/08/2013 (Retro UNIX 8086 v1)
	;
	; 'seektell' puts the arguments from sysseek and systell
	; call in u.base and u.count. It then gets the i-number of
	; the file from the file descriptor in u.r0 and by calling
	; getf. The i-node is brought into core and then u.count
	; is checked to see it is a 0, 1, or 2.
	; If it is 0 - u.count stays the same
	;          1 - u.count = offset (u.fofp)
	;	   2 - u.count = i.size (size of file)
	; 	 		
	; !! Retro UNIX 8086 v1 modification:
	;	Argument 1, file descriptor is in BX;
	;	Argument 2, offset is in CX;
	;	Argument 3, ptrname/switch is in DX register.	
	;
	; mov 	ax, 3 ; Argument transfer method 3 (three arguments)	
	; call 	arg
	;
	; ((Return -> ax = base for offset (position= base+offset))
	;
	mov 	[u.base], ecx ; offset
		; jsr r0,arg; u.base / puts offset in u.base
	; 09/01/2022
	; ebx = file descriptor (0 to 9)
seektell0:
	mov 	[u.count], edx
		; jsr r0,arg; u.count / put ptr name in u.count
	;mov	ax, bx
		; mov *u.r0,r1 / file descriptor in r1 
			     ; / (index in u.fp list)
	; 12/06/2021
	; BX = file descriptor (file number)
	call	getf
		; jsr r0,getf / u.fofp points to 3rd word in fsp entry
	; 09/01/2022
	or	eax, eax
	;or	ax, ax ; i-number of the file
		; mov r1,-(sp) / r1 has i-number of file, 
		             ; / put it on the stack
	;jz	error
		; beq error4 / if i-number is 0, not active so error
	jnz	short seektell1
	mov	dword [u.error], ERR_FILE_NOT_OPEN  ; 'file not open !'
	jmp	error
seektell1:
	; 06/05/2021
	;push	eax
	;cmp	ah, 80h
	;jb	short seektell2
	;	; bgt .+4 / if its positive jump
	;neg	ax
	;	; neg r1 / if not make it positive
seektell2:
	call	iget
		; jsr r0,iget / get its i-node into core
	; 09/01/2022
	; (if there is/was an error in 'iget', cpu will not come here)
	;; 06/11/2021
	;jnc	short seektell6
	;mov	[u.error], eax
	;jmp	error
seektell6:
        mov     ebx, [u.fofp] ; 05/08/2013
	cmp	byte [u.count], 1
		; cmp u.count,$1 / is ptr name =1
	ja	short seektell3
		; blt 2f / no its zero
	je	short seektell4
		; beq 1f / yes its 1
	xor	eax, eax
	;jmp	short seektell5
	retn
seektell3:
	; 03/01/2016
	;;movzx	eax, word [i.size]
	;mov   	ax, [i.size]
	;	; mov i.size,u.count /  put number of bytes 
	;			; / in file in u.count
	;;jmp	short seektell5
	;	; br 2f
	; 06/05/2021 - Retro UNIX 386 v2
	mov	eax, [i.size]
	retn
seektell4: ; 1: / ptrname =1
	;mov	ebx, [u.fofp]
	mov	eax, [ebx]
		; mov *u.fofp,u.count / put offset in u.count
;seektell5: ; 2: / ptrname =0
	;mov	[u.count], eax
	;pop	eax 
		; mov (sp)+,r1 / i-number on stack  r1
	retn
		; rts r0

sysintr: ; / set interrupt handling
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'sysintr' sets the interrupt handling value. It puts
	; argument of its call in u.intr then branches into 'sysquit'
	; routine. u.tty is checked if to see if a control tty exists.
	; If one does the interrupt character in the tty buffer is
	; cleared and 'sysret'is called. If one does not exits
	; 'sysret' is just called.	
	;
	; Calling sequence:
	;	sysintr; arg
	; Argument:
	;	arg - if 0, interrupts (ASCII DELETE) are ignored.
	;	    - if 1, intterupts cause their normal result
	;		 i.e force an exit.
	;	    - if arg is a location within the program,
	;		control is passed to that location when
	;		an interrupt occurs.	
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysintr' system call sets u.intr to value of BX
	;	then branches into sysquit.
	;
	mov	[u.intr], bx
		; jsr r0,arg; u.intr / put the argument in u.intr
		; br 1f / go into quit routine
	jmp	sysret

sysquit:
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'sysquit' turns off the quit signal. it puts the argument of
	; the call in u.quit. u.tty is checked if to see if a control 
	; tty exists. If one does the interrupt character in the tty
	; buffer is cleared and 'sysret'is called. If one does not exits
	; 'sysret' is just called.	
	;
	; Calling sequence:
	;	sysquit; arg
	; Argument:
	;	arg - if 0, this call disables quit signals from the
	;		typewriter (ASCII FS)
	;	    - if 1, quits are re-enabled and cause execution to
	;		cease and a core image to be produced.
	;		 i.e force an exit.
	;	    - if arg is an address in the program,
	;		a quit causes control to sent to that
	;		location.	
	; Inputs: -
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       'sysquit' system call sets u.quit to value of BX
	;	then branches into 'sysret'.
	;
	mov	[u.quit], bx
	jmp	sysret
		; jsr r0,arg; u.quit / put argument in u.quit
	;1:
		; mov u.ttyp,r1 / move pointer to control tty buffer
			      ; / to r1
		; beq sysret4 / return to user
		; clrb 6(r1) / clear the interrupt character 
			   ; / in the tty buffer
		; br sysret4 / return to user

syssetuid: ; / set process id
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 27/03/2021 - Retro UNIX 386 v2
	;		(16 bit uid)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 - 02/08/2013 (Retro UNIX 8086 v1)
	;
	; 'syssetuid' sets the user id (u.uid) of the current process
	; to the process id in (u.r0). Both the effective user u.uid
	; and the real user u.ruid are set to this. 
	; Only the super user can make this call.	
	;
	; Calling sequence:
	;	syssetuid
	; Arguments: -
	;
	; Inputs: (u.r0) - contains the process id.
	; Outputs: -
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       BL contains the (new) user ID of the current process

	; 27/03/2021
	; INPUT:
	;    BX = (new) user ID

		; movb *u.r0,r1 / move process id (number) to r1
	;cmp	bl, [u.ruid] 
	;	; cmpb r1,u.ruid / is it equal to the real user 
			       ; / id number
	; 27/03/2021
	cmp	bx, [u.ruid]
	je	short setuid1
		; beq 1f / yes
	;cmp	byte [u.uid], 0 ; 02/08/2013
	;	; tstb u.uid / no, is current user the super user?
	;;ja	error
	;	; bne error4 / no, error
	; 27/03/2021
	cmp	word [u.uid], 0 
	jna	short setuid0
setuid_err:
setgid_err:
	mov	dword [u.error], ERR_NOT_SUPERUSER  ; 11
				; 'permission denied !' error
	jmp	error
setuid0:
	;mov	[u.ruid], bl
	; 27/03/2021
	mov	[u.ruid], bx
setuid1: ; 1:
	;mov	[u.uid], bl ; 02/08/2013
	;	; movb r1,u.uid / put process id in u.uid
	;	; movb r1,u.ruid / put process id in u.ruid
	; 27/03/2021
	mov	[u.uid], bx
	jmp	sysret
		; br sysret4 / system return

sysgetuid: ; < get user id >
	; 11/03/2022
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 27/03/2021 - Retro UNIX 386 v2
	;		(16 bit uid)
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 07/07/2013 (Retro UNIX 8086 v1)
	;
	; 'sysgetuid' returns the real user ID of the current process.
	; The real user ID identifies the person who is logged in,
	; in contradistinction to the effective user ID, which
	; determines his access permission at each moment. It is thus
	; useful to programs which operate using the 'set user ID'
	; mode, to find out who invoked them.	
	;
	; Calling sequence:
	;	syssetuid
	; Arguments: -
	;
	; Inputs: -
	; Outputs: (u.r0) - contains the real user's id.
	; ...............................................................
	;	
	; Retro UNIX 8086 v1 modification: 
	;       AL contains the real user ID at return.
	;
	; 11/03/2022 - Retro UNIX 386 v1.2
	; Input:
	;	none
	; Output/Return:
	;	AX = real user ID
	;	Hig Word of EAX = effective user ID

	;;movzx eax, byte [u.ruid]
	; 27/03/2021
	;movzx	eax, word [u.ruid]
	; 11/03/2022
	; (return u.uid & u.ruid numbers as it is in unix v5-v7)
	mov	ax, [u.uid]
	shl	eax, 16	; efective user ID in high word of eax
	; 09/01/2022
	mov	ax, [u.ruid] ; real user ID in low word of eax
	mov	[u.r0], eax 
		; movb	u.ruid,*u.r0 / move the real user id to (u.r0)
	jmp	sysret
		; br sysret4 / systerm return, sysret

syssetgid: ; set group id
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 02/04/2021
	; 27/03/2021 - Retro UNIX 386 v2
	;
	; 'syssetgid' sets the user's group id (u.gid) 
	;  of the current process to the process id in (u.r0).
	;  Both the effective user u.gid and the real user
	;  u.rgid are set to this. 
	;  Only the super user can make this call.	
	
	; INPUT:
	;	BL = (new) group ID
	; OUTPUT:
	;	-

	cmp	bl, [u.rgid] 
	je	short setgid1

	;cmp	byte [u.uid], 0
	; 02/04/2021
	cmp	word [u.uid], 0
	ja	short setgid_err
setgid0:
	mov	[u.rgid], bl
setgid1: ; 1:
	mov	[u.gid], bl
	jmp	sysret

sysgetgid: ; < get group id >
	; 11/03/2022
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 27/03/2021 - Retro UNIX 386 v2
	;
	; 'sysgetgid' returns the real group ID of the current process.
	; The real group ID identifies the group of the person
	; who is logged in, in contradistinction to the effective
	; group ID, which determines his access permission at each moment.
	; It is thus useful to programs which operate using
	; the 'set group ID' mode, to find out the group of the user
	; who invoked them.

	; 11/03/2022 - Retro UNIX 386 v1.2
	; Input:
	;	none
	; Output/Return:
	;	AL = real group ID
	;	AH = effective group ID

	;movzx	eax, word [u.ruid]
	;; 09/01/2022
	;mov	ax, [u.ruid]
	; 11/03/2022
	; (return u.gid & u.rgid numbers as it is in unix v5-v7)
	mov	ah, [u.gid] ; efective group ID in byte 1
	mov	al, [u.rgid] ; real group ID in byte 0
	mov	[u.r0], eax
	jmp	sysret

sysver: ; get operating system version
	; 09/01/2022 - Retro UNIX 386 v1.2
	; 18/06/2021 - Retro UNIX 386 v2

	;VMAJOR equ 2 ; Retro UNIX 386 v2
	;VMINOR equ 0 ; Retro UNIX 386 v2.0 (v2.0.0)
	;CURRENT_VERSION equ (VMAJOR*256)+VMINOR

	; 09/01/2022
	VMAJOR equ 1 ; Retro UNIX 386 v1
	VMINOR equ 2 ; Retro UNIX 386 v1.2 (v1.2.0)
	CURRENT_VERSION equ (VMAJOR*256)+VMINOR 		  

	; 29/04/2016 - TRDOS 386 v2.0
	;mov	dword [u.r0], 200h ; AH = major version, AL = minor version 
	; 18/06/2021
	mov	dword [u.r0], CURRENT_VERSION ;	(ah = 2, al = 0)
	jmp	sysret

anyi: 
	; 18/07/2022
	; 26/03/2022
	; 13/03/2022
	; 12/03/2022
	; 09/01/2022
	; 02/01/2022
	; 01/01/2022
	; 27/12/2021 (Retro UNIX 386 v1.2)
	; 22/11/2021 - Retro UNIX 386 v2 compatibility modification
	; 22/06/2015 (Retro UNIX 386 v1 - Beginning)
	; 25/04/2013 (Retro UNIX 8086 v1)
	;
	; 'anyi' is called if a file deleted while open.
	; "anyi" checks to see if someone else has opened this file.
	;
	; INPUTS ->
	;    r1 - contains an i-number
	;    fsp - start of table containing open files
	;
	; OUTPUTS ->
	;    "deleted" flag set in fsp entry of another occurrence of
	;	   this file and r2 points 1st word of this fsp entry.
	;    if file not found - bit in i-node map is cleared
	;    			 (i-node is freed)
	;               all blocks related to i-node are freed
	;	        all flags in i-node are cleared
	; ((AX = R1)) input
	;
	;    (Retro UNIX Prototype: 02/12/2012, UNIXCOPY.ASM)
	;	12/03/2022
        ;    ((Modified registers: eax, edx, ecx, ebx, esi, edi, ebp))  
	;
		; / r1 contains an i-number
	mov	ebx, fsp
		; mov $fsp,r2 / move start of fsp table to r2
anyi_1: ; 1:
	;cmp	eax, [ebx] ; 09/01/2022 (32 bit inode number)
	cmp	ax, [ebx]
		; cmp r1,(r2) / do i-numbers match?
	;je	short anyi_3
		; beq 1f / yes, 1f
	; 12/03/2022
	jne	short anyi_0		

	; 27/12/2021 - Retro UNIX 386 v1.2 (runix v2 fs)
	;neg	ax
	;	; neg r1 / no complement r1
	;cmp	ax, [ebx]
	;	; cmp r1,(r2) / do they match now?
	;;je	short anyi_3
		; beq 1f / yes, transfer
		; / i-numbers do not match
	; 12/03/2022
	;jne	short ayni_0

anyi_3: ; 1: / i-numbers match
	; 13/03/2022 (BugFix)
	;; 09/01/2022
	;test	byte [ebx+5], 1 ; open for writing flag
	;jz	short anyi_0	

	; 27/12/2021 - Retro UNIX 386 v1.2 (runix v2 fs)
	or	byte [ebx+5], 80h ; set deleted file flag (bit 7)
	; 22/06/2015
	;inc 	byte [ebx+9]
		; incb 7(r2) / increment upper byte of the 4th word
		   ; / in that fsp entry (deleted flag of fsp entry)
	retn
		; rts r0
anyi_0:
	; 27/12/2021 - Retro UNIX 386 v1.2 (runix v2 fs)
	;add	ebx, fp.size ; runix v2 fsp table size is 16 bytes
	; 01/01/2022
	add	ebx, 16 ; runix v2 fsp table size is 16 bytes
	;;add	ebx, 10 ; fsp table size is 10 bytes
			; in Retro UNIX 386 v1 (22/06/2015)
		; add $8,r2 / no, bump to next entry in fsp table
	; 22/11/2021
	;cmp	ebx, fsp + (NFILES*fp.size) ; fsp+(NFILES*16)
	; 02/01/2022
	cmp	ebx, fsp + (nfiles*16)
	; 01/01/2022
	;cmp	ebx, fsp + (NFILES*16) ; fsp+(NFILES*fp.size)
	;;cmp	ebx, fsp + (nfiles*10) ; 22/06/2015 
		; cmp r2,$fsp+[nfiles*8] 
			; / are we at last entry in the table
	jb	short anyi_1
		; blt 1b / no, check next entries i-number

	; 27/12/2021
	;;cmp	ax, 32768
	;cmp	ah, 80h ; negative number check
	;	; tst r1 / yes, no match
	;	; bge .+4
	;jb	short anyi_2
	;neg	ax
	;	; neg r1 / make i-number positive
;anyi_2:	
	; 12/03/2022
	;push	eax ; **** ; inode number
	;
	call	imap
		; jsr r0,imap / get address of allocation bit 
			    ; / in the i-map in r2

	;; DL/DX (MQ) has a 1 in the calculated bit position
        ;; EBX (R2) has address of the byte with allocation bit

	; 12/03/2022
	; (at return of imap)
	; ebp = superblock buffer address
	; eax = inode number
	; [ebp+SB.ImapBuffer] = physical block/sector number
	;			of current IMAP sector
	; EBX = buffer address/offset for relevant alloc byte
	;  DL has 1 at bit position which is for free inode
	;
	; ECX = byte offset from the (start of) inode map buffer

 	;;not	dx
	;not 	dl ; 0 at calculated bit position, other bits are 1
        ;;and	[ebx], dx
	;and 	[ebx], dl 
	;	; bicb mq,(r2) / clear bit for i-node in the imap

	; 12/03/2022 ; (*)

	;mov	[ebp+SB.LastInode], eax ; (free) inode number

	push	eax ; ***
	push	ecx ; **
	push	edx ; *

	; 18/07/2022
	;push	ebp ; @ ; 11/03/2022

	mov	eax, [ebp+SB.ImapBuffer]

	call	wslot
	; ebx = buffer data address (write operation bit is set)
	; eax = physical sector number
	; Note: ebx contains addr of the 1st buffer in the buf chain
	; (the last allocated buffer becomes the 1st in buffer chain)

	; 18/07/2022
	;pop	ebp ; @ ; 11/03/2022

	;mov	[ebp+SB.ImapBuffer], ebx
			; save inode map buffer address
	pop	edx ; *
	pop	ecx ; **
	add	ebx, ecx ; + byte offset (for allocation bit pos)	

	;not	dx
	not 	dl ; 0 at calculated bit position, other bits are 1
        ;and	[ebx], dx
	and 	[ebx], dl 
		; bicb mq,(r2) / clear bit for i-node in the imap

	;push	ebp ; @
	call	dskwr ; writes the 1st buffer to sector
		      ; (at the beginning/head of the buffer chain)
	;pop	ebp ; @

	; if we are here, buffer has been written to disk successfully 
	; (otherwise cpu would jump to 'error' address)

	; 12/03/2022 ; (*)
	; set superblock modified flag
	mov	edx, smod
	test	byte [cdev], 1 ; mounted device ?
	jz	short anyi_2 ; no, root device
	; yes, mounted device
	inc	edx ; edx = offset mmod
anyi_2:
	inc	byte [edx] ; superblock modified !

	; 12/03/2022
	; decrease first free inode number (if it is required)
	mov	eax, [esp] ; ***
	; eax = released/freed inode number 
	cmp	[ebp+SB.FirstFreeIno], eax
	jna	short anyi_4
	mov	[ebp+SB.FirstFreeIno], eax
		    ; (free inode search start value)
anyi_4:
	; 12/03/2022
	; increase free inode count
	mov	edx, [ebp+SB.FreeInodes]
	;cmp	edx, 0FFFFFFFFh
	;je	short anyi_5 ; invalid
	inc	edx ; 0FFFFFFFFh -> 0
	jz	short anyi_5 ; invalid
	mov	[ebp+SB.FreeInodes], edx ; +1
anyi_5:
	call	get_system_time
		; eax = current time (as unix epoch time)
	mov	[ebp+SB.ModifTime], eax

	pop	eax ; *** ; inode number

	call	itrunc
		; jsr r0,itrunc / free all blocks related to i-node

	; 26/03/2022
	; eax = 0 ; (itrunc clears eax at return)

	; 12/03/2022
	;pop	eax ; **** ; inode number

	; 26/03/2022
	mov	[i.flgs], ax ; (eax should be 0 here)
	;mov 	word [i.flgs], 0
		; clr i.flgs / clear all flags in the i-node
	retn
		; rts r0 / return

;anyi_3: ; 1: / i-numbers match
;	; 09/01/2022
;	test	byte [ebx+5], 1 ; open for writing flag
;	jz	short anyi_0	
;
;	; 27/12/2021 - Retro UNIX 386 v1.2 (runix v2 fs)
;	or	byte [ebx+5], 80h ; set deleted file flag (bit 7)
;	; 22/06/2015
;	;inc 	byte [ebx+9]
;		; incb 7(r2) / increment upper byte of the 4th word
;		   ; / in that fsp entry (deleted flag of fsp entry)
;	retn
;		; rts r0