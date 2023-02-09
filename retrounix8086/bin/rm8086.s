; ****************************************************************************
; rm8086.s (rm0.asm) - by Erdogan Tan - 25/04/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - remove -- remove (unlink/delete) file(s)
;
; [ Last Modification: 28/04/2022 ]
;
; Derived from (original) UNIX v7 (& v7 x86) 'rm.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; v7.tar.gz
; ****************************************************************************
; [ v7.tar - usr/src/cmd/rm.c (archive date: 10-1-1979) ]
;
; Assembler: NASM v2.15
; ((nasm rm8086.s -l rm8086.txt -o rm8086.bin -Z error.txt))
;
; rm8086.s - 28/04/2022 - Retro UNIX 8086 v1 (16 bit 'rm0.s')
; rm0.s - 27/04/2022 - Retro UNIX 386 v1
; rm1.s - 27/04/2022 - Retro UNIX 386 v1.1
; rm2.s - 27/04/2022 - Retro UNIX 386 v1.2 (modified unix v7 inode)

; 12/01/2022 (Retro UNIX 386 v1.2)
; 13/10/2015

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
_msg    equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !
; 12/01/2022 - Retro UNIX 386 v1.2
; Retro UNIX 386 v2 system calls
_setgid	equ 37
_getgid	equ 38
_sysver	equ 39 ; (get) Retro Unix 386 version

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

;%macro sys 1-4
;   ; 03/09/2015	
;   ; 13/04/2015
;   ; Retro UNIX 386 v1 system call.		
;   %if %0 >= 2   
;       mov ebx, %2
;       %if %0 >= 3    
;           mov ecx, %3
;           ;%if %0 = 4
;           %if	%0 >= 4 ; 11/03/2022
;		mov edx, %4   
;           %endif
;       %endif
;   %endif
;   mov eax, %1
;   int 30h	   
;%endmacro

%macro sys 1-4
    ; Retro UNIX 8086 v1 system call.
    %if %0 >= 2   
        mov bx, %2
        %if %0 >= 3
            mov cx, %3
            %if %0 >= 4
               mov dx, %4
            %endif
        %endif
    %endif
    mov ax, %1
    int 20h
%endmacro

;; Retro UNIX 386 v1 system call format:
;; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

;; 11/03/2022
;; Note: Above 'sys' macro has limitation about register positions;
;;	ebx, ecx, edx registers must not be used after their
;;	positions in sys macro.
;; for example:
;;	'sys _write, 1, msg, ecx' is defective, because
;;	 ecx will be used/assigned before edx in 'sys' macro.
;; correct order may be:
;;	'sys _write, 1, msg, eax ; (eax = byte count)

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

;struc stat
;	; Note: This is for Retro UNIX v1.2 'sysstat' output !!!
;	; (66 bytes)
;	.inode:  resw 1
;	.mode:	 resw 1
;	.nlinks: resw 1
;	.uid:	 resw 1
;	.gid:	 resb 1
;	.size_h: resb 1
;	.size:	 resd 1
;	.dskptr: resd 10
;	.atime:	 resd 1
;	.mtime:	 resd 1
;	.ctime:  resd 1
;	.strucsize:
;endstruc

struc stat
	; Note: This is for Retro UNIX v1 'sysstat' output !!!
	; (34 bytes)
	.inode:  resw 1
	.mode:	 resw 1
	.nlinks: resb 1
	.uid:	 resb 1
	.size:	 resw 1
	.dskptr: resw 8
	.ctime:	 resd 1
	.mtime:	 resd 1
	.rsvd:   resw 1
	.strucsize:
endstruc     

;S_IFMT   equ 0F000h ; /* type of file */
;S_IFDIR  equ 04000h ; /* directory */
;S_IFCHR  equ 02000h ; /* character special */
;S_IFBLK  equ 06000h ; /* block special */
;S_IFREG  equ 08000h ; /* regular */
;S_ISUID  equ 00800h ; /* set user id on execution */
;S_ISGID  equ 00400h ; /* set group id on execution */
;S_IREAD  equ 00100h ; /* read permission, owner */
;S_IWRITE equ 00080h ; /* write permission, owner */
;S_IEXEC  equ 00040h ; /* execute/search permission, owner */

; 24/04/2022
; 21/04/2022 - UNIX v1 inode
; byte 1
S_ALLOC  equ 080h ; Allocated flag
S_IFDIR  equ 040h ; Directory flag
S_IFMDF  equ 020h ; File modified flag (always on)
S_IFLRG  equ 010h ; Large File flag
; byte 0
S_ISUID  equ 020h ; Set User ID On Execution flag
S_IEXEC  equ 010h ; Executable File flag
S_IREAD  equ 008h ; Owner's Read Permission flag
S_IWRITE equ 004h ; Owner's Write Permission flag

;BSIZE equ 512

;26/04/2022
; Directory entry size
DIRSIZ	equ 10	; Retro UNIX 386 v1
;DIRSIZ equ 16	; Retro UNIX 386 v1.1 & v1.2 

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

;[BITS 32] ; 32-bit intructions (for 80386 protected mode)
[BITS 16] ; 16-bit (x86 real mode) intructions

[ORG 0] 

START_CODE:
	; 28/04/2022 (Real Mode, 16 bit register modification)
	;	eax, edx, ecx, ebx -> ax, dx, cx, bx
	;	esi, edi, ebp, esp -> si, di, bp, sp
	;	register+4 -> register+2
	;	dword values on stack -> word values on stack
	;	sys _msg -> call print_msg
	;	 
	; 27/04/2022
	; 26/04/2022
	; 25/04/2022
	; main(argc, argv)

	pop	ax ; [sp] = argument count	
	;mov	[argc], ax
	mov	[argc], al
	;dec	ax
	dec	al
	jnz	short rm_0 ; [argc] = 1

	;sys	_msg, program_msg, 255, 0Fh
	; 28/04/2022
	mov	ax, program_msg
	call	print_msg

rm_usage:
   	; "Usage: rm [–fri] file ..."
	;sys	_msg, usage_msg, 255, 07h
	; 28/04/2022
	mov	ax, usage_msg
	call	print_msg
rm_exit:
	sys	_exit	; sys exit
;hlt:
;	nop
;	nop
;	jmp	short hlt

print_msg:
	; 28/04/2022 (rm8086.s)
	; 22/04/2022 (cp8086.s)
	; Modified registers: ax, bx, cx, dx
strlen:
	; ax = asciiz string address
	mov	bx, ax
	dec	bx
nextchr:
	inc	bx
	cmp	byte [bx], 0
	ja	short nextchr
	;cmp	[bx], 0Dh
	;ja	short nextchr
	sub	bx, ax
	; bx = asciiz string length
	;retn
print_str:
	mov	dx, bx
	sys	_write, 1, ax

	retn

rm_0:
	;if (isatty(0) == 0)
	;	fflg++;

	; set '-f' flag if standard input is not a terminal
	sys	_fstat, 0, stbuf
	;jc	short rm_exit ; 27/04/2022

	; 25/04/2022
	; Retro UNIX v2 inode:
	;	 regular file flag is mode bit 15
	;
	;test	[stbuf+stat.mode+1], S_IFREG
	;jz	short rm_1 ; device file (terminal/console)

	;inc	byte [fflg] 

	; 25/04/2022
	; Retro UNIX v1 inode:
	;	regular file inode numbers are > 41
	;
	cmp	word [stbuf+stat.inode], 41
	jna	short rm_1 ; device file (or root dir?)

	; (if standard input is not a terminal)
	; force removing (without question and 'y' answer)
	inc 	byte [fflg]
rm_1: 
	;if(argc>1 && argv[1][0]=='-') {
	;	arg = *++argv;
	;	argc--;
	;

	mov	bp, sp ; 26/04/2022
	pop	si ; argv[0] ; executable file name (='rm')
	;;mov	[argv], sp
	;mov	esi, [esp] ; 26/04/2022
			; argv[1] ; option (if it is used)
	; 28/04/2022
	pop	si ; == mov si, [sp]
	push	si
	lodsb
	cmp	al, '-'
	jne	short rm_2
	mov	bp, sp ; 26/04/2022
	dec	byte [argc] ; ARGC - 2
rm_28:
	lodsb
	test	al, 0FFh
	jz	short rm_2
	
	;while(*++arg != '\0')
	;	switch(*arg) {
	;	case 'f':
	;		fflg++;
	;		break;
	;	case 'i':
	;		iflg++;
	;		break;
	;	case 'r':
	;		rflg++;
	;		break;
	;	default:
	;		printf("rm: unknown option %s\n", *argv);
	;		exit(1);
	;	}

	cmp	al, 'f'
	je	short rm_1_f
	cmp	al, 'i'
	je	short rm_1_i
	cmp	al, 'r'
	je	short rm_1_r
rm_1_uop:
	; "rm: unknown option '-x'"
	mov	[uop], al
	; 28/04/2022
	mov	ax, unk_op_msg
	call	print_msg
	; (exit code is not used in current Retro UNIX version)
	;sys	_exit, 1 ; (bx = exit code = 1)
	jmp	short rm_exit	
rm_1_r:
	inc	byte [rflg] ; rflg++;
	;jmp	short rm_2
	; 27/04/2022
	jmp	short rm_28 ; get next option char
rm_1_f:
	inc	byte [fflg] ; fflg++;
	;jmp	short rm_2
	; 27/04/2022
	jmp	short rm_28 ; get next option char
rm_1_i:
	inc	byte [iflg] ; iflg++
	; 27/04/2022
	jmp	short rm_28 ; get next option char
rm_2:
	;while(--argc > 0) {
	;	if(!strcmp(*++argv, "..")) {
	;	  fprintf(stderr, "rm: cannot remove `..'\n");
	;	  continue;
	;	}
	;	rm(*argv, fflg, rflg, iflg, 0);
	;}

	dec	byte [argc]
	jz	short rm_4

	;add	word ptr [argv], 4 ; ++argv	
	;mov	si, [argv]
	add	bp, 2 ; ++argv	
	mov	si, [bp]
	cmp	word [si], '..' ; strcmp(*++argv, "..")
	je	short rm_3
	; rm(*argv, fflg, rflg, iflg, 0);
	mov	byte [level], 0
	call	remove_file
	jmp	short rm_2
rm_3:
	; "rm: cannot remove '..'"
	; 28/04/2022
	mov	ax, cnr_dotdot_msg
	call	print_msg
rm_4:
	; (exit code not used in Retro UNIX 386 v1 & v1.1)
	;sys	_exit, [errcode]
	sys	_exit
;hang:
;	jmp	short hang

_return:
	; 27/04/2022
	mov	sp, bp ; add sp, 80 (add sp, 100)
	pop	bp 
	
	retn
	
remove_file:	
	; rm(arg, fflg, rflg, iflg, level)

	; 28/04/2022 (16 bit registers)
	; 27/04/2022
	; 26/04/2022
	; modified registers: ax, bx, cx, dx, di

	; INPUT:
	;	   si = *argv   ; [level] = 0 
	;	or si = namebuf ; [level] > 0 --
	;	byte [fflg], [rflg], [iflg]
	;	byte [level] = sub directory level

	;if(stat(arg, &buf)) {
	;	if (fflg==0) {
	;	   printf("rm: %s nonexistent\n", arg);
	;	   ++errcode;
	;	}
	;	return;
	;}

	; 27/04/2022
	push	bp
	mov	bp, sp
	;sub	sp, 100 ; char name[100];
	sub	sp, 80 ; Retro UNIX 8086 v1 (& 386 v1)	

	sys	_stat, si, stbuf
	jnc	short rm_5

	cmp	byte [fflg], 0
	ja	short _return

	; "rm: <si> non existent"
	; 28/04/2022
	mov	ax, rm_hdr_msg
	call	print_msg
	mov	ax, si
	call	print_msg
	mov 	ax, nonex_msg
	call	print_msg
	
	;inc	word [errcode] ; ++errcode;

	jmp	short _return
rm_5:	 
	;if ((buf.st_mode&S_IFMT) == S_IFDIR) {
	;	if(rflg) {

	; Retro UNIX 386 v2 inode
	;mov	al, [stbuf+stat.mode+1]
	;and	al, S_IFREG | S_IFDIR
	;cmp	al, S_IFREG | S_IFDIR
	;;jne	short rm_17
	; 27/04/2022
	;je	short rm_24
	;jmp	rm_17

	; Retro UNIX 386 v1 (unix v1) inode
	test	byte [stbuf+stat.mode+1], S_IFDIR
	;jz	short rm_17
	; 26/04/2022
	jnz	short rm_24
	jmp	rm_17
rm_24:
	; Directory ! ('-r' option is required)
	
	;if(rflg) { ...
	;	....
	;} 
	;printf("rm: %s directory\n", arg);
	;	++errcode;
	;	return;	

	cmp	byte [rflg], 0
	jna	short rm_9 ; 28/04/2022
	; 26/04/2022
	;ja	short rm_25
	;jmp	rm_9

rm_25:
	;if (access(arg, 02) < 0) {
	;	if (fflg==0)
	;	   printf("%s not changed\n", arg);
	;	errcode++;
	;	return;
	;}

	; dl = permission/mode value
	;	(dl = 2 for Retro UNIX 386 v2 inode)
	;	(dl = 1 for Retro UNIX 386 v1 inode)

	;;mov	dl, 2
	;mov	dl, 1

	call	access
	jnc	short rm_7

	cmp	byte [fflg], 0
	ja	short rm_6

	; si = arg = *[argv]

	; "<si> not changed"
	;sys	_msg, nextline, 255, 07h
	;sys	_msg, si, 255, 07h
	;sys	_msg, nchd_msg, 255, 07h
	; 28/04/2022
	mov	ax, nextline
	call	print_msg
	mov	ax, si
	call	print_msg
	mov	ax, nchd_msg
	call	print_msg
rm_6:
	;inc	word [errcode] ; ++errcode;
	jmp	_return ; return;
rm_7:
	;if(iflg && level!=0) {
	;	printf("directory %s: ", arg);
	;	    if(!yes())
	;	    return;
	;}

	cmp	byte [iflg], 0
	jna	short rm_8
	cmp	byte [level], 0
	jna	short rm_8

	 ;"directory <si> : ? " 
	;sys	_msg, dir_hdr_msg, 255, 07h ; message row header
	;sys	_msg, si, 255, 07h	; directory name 
	;sys	_msg, qu_msg, 255, 07h	; question
	; 28/04/2022
	mov	ax, dir_hdr_msg
	call	print_msg
	mov	ax, si
	call	print_msg
	mov	ax, qu_msg
	call	print_msg

	; get user input (answer)
	; and write it (as answer) via sysmsg system call
	call	ifyes
	jc	short rm_6 ; no
	; yes
rm_8:
	;if((d=open(arg, 0)) < 0) {
	;	printf("rm: %s: cannot read\n", arg);
	;	exit(1);
	;}
	
	sys	_open, si, 0 ; open directory for read
	jnc	short rm_10 ; 28/04/2022
	; 26/04/2022
	;jc	short rm_26
	;jmp	rm_10
rm_26:
	;"rm: <si> : cannot read" 
	;sys	_msg, rm_hdr_msg, 255, 07h ; msg row header
	;sys	_msg, si, 255, 07h	; directory name
	;sys	_msg, cnr_msg, 255, 07h ; 'cannot read'
	; 28/04/2022
	mov	ax, rm_hdr_msg
	call	print_msg
	mov	ax, si
	call	print_msg
	mov	ax, cnr_msg
	call	print_msg

	; (exit code not used in Retro UNIX 386 v1 & v1.1)
	;sys	_exit, 1 ; (bx = exit code = 1)
	sys	_exit
;here:
;	jmp	short here

rm_9:
	;printf("rm: %s directory\n", arg);
	;	++errcode;
	;	return;	

	; "rm: <si> directory"
	mov	ax, rm_hdr_msg
	call	print_msg
	mov	ax, si
	call	print_msg
	mov	ax, dir_msg
	call	print_msg

	;inc	word [errcode] ; ++errcode;

	jmp	_return ; return;

rm_10:
	;while(read(d, (char *)&direct, sizeof(direct)) == sizeof(direct)) {
	;	if(direct.d_ino != 0 && !dotname(direct.d_name)) {
	;	   sprintf(name, "%s/%.14s", arg, direct.d_name);
	;	   rm(name, fflg, rflg, iflg, level+1);
	;	}
	;}
	;
	;close(d);
	;errcode += rmdir(arg, iflg);
	;return;

	; 26/04/2022
	mov	[_d_], ax ; file (directory) descriptor
rm_29:
	sys	_read, [_d_], direct, DIRSIZ
	jc	short rm_15
	cmp	ax, DIRSIZ  ; 16 (runix v1.1&v1.2) or 10 (runix v1)
	jb	short rm_15  ; (jne short rm_14)

	; 27/04/2022
	; if(direct.d_ino != 0 ..
	cmp	word [direct], 0 ; is inode number > 0 ?
	jna	short rm_29  ; no ; read next direntry ; 27/04/2022
	; yes  	
	; && !dotname(direct.d_name))
	mov	bx, direct+2
	call	isdotname
	jz	short rm_29  ; '.' or '..' 
			     ; read next direntry ; 27/04/2022

	; sprintf(name, "%s/%.14s", arg, direct.d_name);
	; 27/04/2022
	mov	di, sp ; name ; 80 byte frame on stack
	push	si ; * ; save si (arg, *argv)
	; 28/04/2022
	push	di ; **
rm_11:
	lodsb
	or	al, al
	jz	short rm_12 ; (end of the path)
	stosb
	jmp	short rm_11
rm_12:
	; add file name to the path
	mov	al, '/'
	stosb
	;mov	cx, 14 ; max. 14 byte file name
	mov	cx, DIRSIZ-2 ; 8 ; 28/04/2022
	mov	si, direct+2 ; direct.d_name
rm_13:
	lodsb
	stosb
	or	al, al
	jz	short rm_14 ; end of the (full) path
	loop	rm_13
	; 28/04/2022
	mov	byte [di], 0
rm_14:
	; rm(name, fflg, rflg, iflg, level+1);
	; 28/04/2022 (Retro UNIX 8086 v1)
	; 27/04/2022
	;lea	esi, [esp+4] ; path name buffer (stack frame)
	; 28/04/2022
	pop	si ; ** ; == lea si, [sp+2]
	;
	push	word [_d_] ; save file (dir) descriptor 
	inc	byte [level]
	call	remove_file ; ! call itself !
	dec	byte [level] ; (this may not be necessary)
	pop	word [_d_] ; restore file (dir) descriptor
	pop	si ; *	; restore si (arg, *argv)
	jmp	short rm_29
rm_15:
	sys	_close, [_d_] ; close(d);
	; errcode += rmdir(arg, iflg);
	; 27/04/2022
	call	rmdir
	;jnc	short rm_16
	;mov	byte [errcode], 1
	;---- 
	;NOTE:
	; sysexit error code ((exit(errcode);)
	; is not used by current Retro UNIX version
	;----
rm_16:	; 27/04/2022
	jmp	_return ; return;

rm_17:
	;if(iflg) {
	;	printf("%s: ", arg);
	;	if(!yes())
	;		return;
	;}
	;else if(!fflg) {
	;	if (access(arg, 02)<0) {
	;	   printf("rm: %s %o mode ", arg, buf.st_mode&0777);
	;	   if(!yes())
	;		return;
	;	}
	;}
	;	
	;if(unlink(arg) && (fflg==0 || iflg)) {
	;	printf("rm: %s not removed\n", arg);
	;	++errcode;
	;}

	; 26/04/2022
	cmp	byte [iflg], 0
	jna	short rm_20 ; 28/04/2022
	;ja	short rm_27
	;jmp	rm_20
rm_27:
	; 28/04/2022
	mov	ax, nextline	; next (new) line
	call	print_msg
	mov	ax, si		; file name
	call	print_msg 
	mov	ax, qu_msg	; question
	call	print_msg
	call	ifyes  ; question 	
	jc	short rm_16 ; answer = no (not 'y')
rm_18:
	sys	_unlink, si
	jnc	short rm_16
	;
	mov	al, [fflg]
	or	al, [iflg]
	jz	short rm_16
	; "rm: <si> not removed"
	; 28/04/2022
	mov	ax, rm_hdr_msg	; header
	call	print_msg
	mov	ax, si		; file name
	call	print_msg 
	mov	ax, not_rmd_msg	; 'not removed'
	call	print_msg
	;----
	;Note:  Current Retro UNIX version
	;	does not use error code return (to parent)
	;----
	;inc	byte [errcode]
	; 27/04/2022
	jmp	_return ; return;
rm_20:
	cmp	byte [fflg], 0
	ja	short rm_18
	;
	;if (access(arg, 02)<0) {
	;   printf("rm: %s %o mode ", arg, buf.st_mode&0777);
	;   if(!yes())
	;	return;
	;}
	;
	;;mov	dl, 2
	;mov	dl, 1
	call	access
	jnc	short rm_18
	
	; "rm: <si> _octal_ mode"
	xor	ax, ax
	xor	bx, bx
	mov	cx, sp
	mov	ax, [stbuf+stat.mode]
	;and	ax, 1FFh ; Retro UNIX 386 v2 inode
	and	ax, 1Fh	 ; Retro UNIX 386 v1 inode
	mov	bl, 8 ; divisor for octal number calculation
	mov	di, octal
rm_21:
	xor	dx, dx
	div 	bx
	push	dx
	or	ax, ax
	jnz	short rm_21
rm_22:
	pop	ax
	add	al, '0'
	stosb
	cmp	cx, sp
	ja	short rm_22
	sub	al, al
	stosb

	;sys	_msg, rm_hdr_msg, 255, 07h ; header
	;sys	_msg, octal, 255, 07h	; octal mode
	;sys	_msg, mode_msg, 255, 07h ; ' mode '
	;sys	_msg, que_msg, 255, 07h	; question
	; 28/04/2022
	mov	ax, rm_hdr_msg
	call	print_msg
	mov	ax, octal
	call	print_msg
	mov	ax, mode_msg
	call	print_msg
	mov	ax, que_msg
	call	print_msg

	call	ifyes  ; question 	
	;jc	short rm_23 ; answer = no (not 'y')
	;jmp	rm_18
	; 28/04/2022
	jnc	short rm_18
rm_23:	; 27/04/2022
	jmp	_return ; return;

access:
	; 28/04/2022 (16 bit registers)
	; 26/04/2022
	; INPUT:
	; 	dl = permission/mode value
	;		(dl = 2 for Retro UNIX 386 v2 inode)
	;		(dl = 1 for Retro UNIX 386 v1 inode)
	;
	;	stbuf = status (sysstat output) buffer
	;
	; OUTPUT:
	;	cf = 0 -> ok
	;	cf = 1 -> error (denied)

	;mov	dl, 2 ; retro unix v2 (write permit flag)
	mov	dl, 1 ; unix v1 (write permit flag)

	mov	cl, [stbuf+stat.mode] ; Retro UNIX 386 v1
	;mov	cx, [stbuf+stat.mode] ; Retro UNIX 386 v2

	; Retro UNIX 386 v1.2 (v2)
	;sys	_getuid	; get user ID of current user
	;mov	[uid], al ; ax = real user ID (<=255)
	;sys	_getgid	; get group ID of current user
	;mov	[gid], al ; al = real group ID (<=255)

	; Retro UNIX 386 v1 (unix v1)
	sys	_getuid	; get user ID of current user
	;mov	[uid], al ; al = user ID 

	; dl = 2 -> write permission flag (retro unix v2)
	; dl = 1 -> write permission flag (retro unix v1)

	;mov	ax, [uid]
	;cmp	ax, [stbuf+stat.uid] ; Retro UNIX v2 inode
	cmp	al, [stbuf+stat.uid] ; unix v1
	jne	short access_1

	;; Retro UNIX 386 v1.2 note:
	;; group permission flags will not be used for now!

	;shr	cx, 6  ; Retro UNIX 386 v2 inode flags
	; AL bit 1 -> write permission owner
	
	shr	cl, 2  ; Retro UNIX 386 v1 inode flags
	; AL bit 0 -> write permission owner
access_1:
	and	cl, dl
	jnz	short access_2
	stc
access_2:
	retn

ifyes:
	; 28/04/2022
	; 26/04/2022
	; check user input as yes or no
	; write answer and return with carry if it is 'no'
	sys	_read, 0, chr, 1 ; read standard input
	mov	al, [chr]
	cmp	al, 'y'
	je	short ifyes_yes
	cmp	al, 'Y'
	je	short ifyes_yes
	; another character means 'no'
ifyes_no:
	; write 'no'
	;sys	_msg, no_msg, 255, 07h	; 'no' answer
	mov	ax, no_msg
	call	print_msg
	stc
	retn	; return
ifyes_yes:
	; write 'yes'
	;sys	_msg, yes_msg, 255, 07h	; 'yes' answer
	mov	ax, yes_msg
	call	print_msg
	;clc
	retn

;isdotname:
	; 27/04/2022
	; 26/04/2022
	; check if file name is dot ('.') or dotdot ('..')
	; (return: zf=1 if it is dot or dotdot)
	;mov	ax, [direct+2]  ; 1st 2 chars of file name
isdotname:
	; bx = file (or directory) name address
	mov	ax, [bx]
	cmp	al, '.'
	jne	short isdot_retn
	or	ah, ah ; 0 ?
	jz	short isdot_retn ; '.'
	cmp	al, ah ; '..' ?
	; 27/04/2022
	jne	short isdot_retn
	cmp	byte [bx+2], 0
	; zf = 1 if it is dotdot
isdot_retn:
	retn

rmdir:
	; 28/04/2022 (16 bit registers)
	; 26/04/2022
	; INPUT:
	;	si = directory name address (*argv)
	;	[iflg] = interactive option
	;
	; OUTPUT:
	;	none

	;rmdir(f, iflg)
	;char *f;
	;{
	;	int status, i;
	;
	;	if(dotname(f))
	;		return(0);
	;	if(iflg) {
	;		printf("%s: ", f);
	;		if(!yes())
	;			return(0);
	;}
	
	mov	bx, si
	call	isdotname
	jz	short rmdir_retn
	
	cmp	byte [iflg], 1
	jb	short rmdir_1

	;sys	_msg, dir_hdr_msg, 255, 07h ; message row header
	;sys	_msg, si, 255, 07h	; directory name 
	;sys	_msg, qu_msg, 255, 07h	; question
	; 28/04/2022
	mov	ax, dir_hdr_msg
	call	print_msg
	mov	ax, si
	call	print_msg
	mov	ax, qu_msg
	call	print_msg
	
	call	ifyes  ; question 	
	jc	short rmdir_retn ; answer = no (not 'y')
rmdir_1:
	;while((i=fork()) == -1)
	;	sleep(3);
	;if(i) {
	;	wait(&status);
	;	return(status);
	;}

	mov 	bx, rmdir_2 ; ! Retro UNIX feature only !
			; 'rmdir' child will continue from   
	sys 	_fork
	jc 	short rmdir_retn

	; parent process will continue to run from here 
	; (from 'jc' after 'sys _fork')

	; ax = child process ID

	; ----
	; Note: Current Retro Unix version does not use
	;	status return -exit code from the child- 
	; ----
	
	sys 	_wait	; wait untill the child exits
	;jc 	short rmdir_retn ; 27/04/2022

	; ax = child process ID -which has been terminated-
	; (bx = status -may be used by retro unix later-)

	; cf = 0 ; return(0)
rmdir_retn:
	; if cf = 0 -> return(0)
	;    else (if cf = 1) -> return(1) 

	retn

rmdir_2:
	; child process will continue to run from here
	
	;execl("/bin/rmdir", "rmdir", f, 0);
	;execl("/usr/bin/rmdir", "rmdir", f, 0);
	;printf("rm: can't find rmdir\n");
	;exit(1);

	; run 'rmdir' utility/program
	; (it will/must not return here)

	; set directory address as /bin/rmdir argument 1
	mov	[binrmdira1], si
	;; set directory addr as /usr/bin/rmdir argument 1
	;mov	[usrbinrmdira1], si ; ((*))

	sys	_exec, binrmdir, binrmdirp
	sys	_exec, usrbinrmdir, usrbinrmdirp

	; ! error !
	; if sysexec fails (would fail) to run 'rmdir'
	; cpu will return here

	; "rm: can't find rmdir"
	;sys	_msg, cnf_rd_msg, 255, 07h
	; 28/04/2022
	mov	ax, cnf_rd_msg
	call	print_msg  

	; bx = exit code (if there is an error, it is > 0)  
	;sys	_exit, 1
	sys	_exit ; Current Retro UNIX version
		      ; does not use exit code from child		
hangemhigh:
	nop
	jmp	short hangemhigh

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

; 28/04/2022 (Retro UNIX 8086 v1)
; 26/04/2022

binrmdirp: 
usrbinrmdirp:
	;dw bindrmdir
	dw _rmdir_
binrmdira1:
usrbinrmdira1: ; ((*))
	dw 0 
binrmdira2:
usrbinrmdira2: ; ((*))
	dw 0
;usrbinrmdirp: 
;	;dw usrbindrmdir
;	dw _rmdir_
;usrbinrmdira1:
;	dw 0 
;usrbinrmdira2:
;	dw 0

_rmdir_:
	db 'rmdir', 0
binrmdir:
	db '/bin/rmdir', 0
usrbinrmdir:
	db '/usr/bin/rmdir', 0

;argc:	dw 0
argc:	db 0 ; argument count
fflg:	db 0 ; -f (forced) option
iflg:	db 0 ; -i (interactive) option
rflg:	db 0 ; -r (recursive) option
;argv:	dw 0 ; argument pointer (for argv[1])
;errcode: dw 0 ; error code (will be returned by sysexit)
;uid:	dw 0 ; user id
;;gid:	db 0 ; group id	
; 26/04/2022
;nameptr: dw namebuf

; ----------------------------------------------------------------

program_msg:
	db 0Dh, 0Ah
	db "Retro UNIX 8086 v1 RM by Erdogan TAN - 28/04/2022"
	db 0Dh, 0Ah, 0
usage_msg:
	db 0Dh, 0Ah
	db "Usage: rm [ -fri ] file ..."
nextline:
	db 0Dh, 0Ah, 0

unk_op_msg:
	db 0Dh, 0Ah
	db "rm: unknown option -"
uop:	db "x"
	db 0Dh, 0Ah, 0

cnr_dotdot_msg:
	db 0Dh, 0Ah
	db "rm: cannot remove '..'"
	db 0Dh, 0Ah, 0

rm_hdr_msg:
	db 0Dh, 0Ah
	db "rm: "
	db 0

nonex_msg:
	db " nonexistent"
	db 0Dh, 0Ah, 0

nchd_msg:
	db " not changed"
	db 0Dh, 0Ah, 0
dir_hdr_msg:
	db 0Dh, 0Ah
	db "directory "
	db 0

qu_msg:
	db ":"
que_msg: db " ? "
	db 0 

yes_msg:
	db "yes"
	db 0Dh, 0Ah, 0
no_msg:
	db "no"
	db 0Dh, 0Ah, 0

cnr_msg:
	db ": cannot read"
	db 0Dh, 0Ah, 0

dir_msg:
	db " directory"
	db 0

not_rmd_msg:
	db " not removed"
	db 0Dh, 0Ah, 0

mode_msg:
	db " mode"
	db 0

cnf_rd_msg:
	db 0Dh, 0Ah
	db "rm: can't find rmdir"
	db 0Dh, 0Ah, 0	

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 2

bss_start:

ABSOLUTE bss_start

; 28/04/2022 (Retro UNIX 89086 v1)
; 26/04/2022
_d_:	resw 1  ; Sub directory's file descriptor
       ;resw 1
;struct direct direct;
direct:	resb DIRSIZ ; Directory entry buffer

; 25/04/2022
;;struct stat buf;
;stbuf: resb 66 ; for Retro UNIX 386 v1.2 (66 byte sysstat data)
stbuf: resb 34 ; for Retro UNIX 386 v1.1 (34 byte sysstat data)

; 26/04/2022
level:	resb 1 ; (sub directory level)
octal:	resb 4 ; (asciiz octal mode number string)
chr:	resb 1 ; (sysread character buffer)	
; 27/04/2022
;char name[100];
;namebuf: resb 100	


; 25/04/2022
;-----------------------------------------------------------------
; Original UNIX v7 - rm (utility) c source code (rm.c)
;-----------------------------------------------------------------
;/* UNIX V7 source code: see www.tuhs.org for details. */;
;
;int	errcode;
;
;#include <stdio.h>
;#include <sys/types.h>
;#include <sys/stat.h>
;#include <sys/dir.h>
;
;char	*sprintf();
;
;main(argc, argv)
;char *argv[];
;{
;	register char *arg;
;	int fflg, iflg, rflg;
;
;	fflg = 0;
;	if (isatty(0) == 0)
;		fflg++;
;	iflg = 0;
;	rflg = 0;
;	if(argc>1 && argv[1][0]=='-') {
;		arg = *++argv;
;		argc--;
;		while(*++arg != '\0')
;			switch(*arg) {
;			case 'f':
;				fflg++;
;				break;
;			case 'i':
;				iflg++;
;				break;
;			case 'r':
;				rflg++;
;				break;
;			default:
;				printf("rm: unknown option %s\n", *argv);
;				exit(1);
;			}
;	}
;	while(--argc > 0) {
;		if(!strcmp(*++argv, "..")) {
;			fprintf(stderr, "rm: cannot remove `..'\n");
;			continue;
;		}
;		rm(*argv, fflg, rflg, iflg, 0);
;	}
;
;	exit(errcode);
;}
;
;rm(arg, fflg, rflg, iflg, level)
;char arg[];
;{
;	struct stat buf;
;	struct direct direct;
;	char name[100];
;	int d;
;
;	if(stat(arg, &buf)) {
;		if (fflg==0) {
;			printf("rm: %s nonexistent\n", arg);
;			++errcode;
;		}
;		return;
;	}
;	if ((buf.st_mode&S_IFMT) == S_IFDIR) {
;		if(rflg) {
;			if (access(arg, 02) < 0) {
;				if (fflg==0)
;					printf("%s not changed\n", arg);
;				errcode++;
;				return;
;			}
;			if(iflg && level!=0) {
;				printf("directory %s: ", arg);
;				if(!yes())
;					return;
;			}
;			if((d=open(arg, 0)) < 0) {
;				printf("rm: %s: cannot read\n", arg);
;				exit(1);
;			}
;			while(read(d, (char *)&direct, sizeof(direct)) == sizeof(direct)) {
;				if(direct.d_ino != 0 && !dotname(direct.d_name)) {
;					sprintf(name, "%s/%.14s", arg, direct.d_name);
;					rm(name, fflg, rflg, iflg, level+1);
;				}
;			}
;			close(d);
;			errcode += rmdir(arg, iflg);
;			return;
;		}
;		printf("rm: %s directory\n", arg);
;		++errcode;
;		return;
;	}
;
;	if(iflg) {
;		printf("%s: ", arg);
;		if(!yes())
;			return;
;	}
;	else if(!fflg) {
;		if (access(arg, 02)<0) {
;			printf("rm: %s %o mode ", arg, buf.st_mode&0777);
;			if(!yes())
;				return;
;		}
;	}
;	if(unlink(arg) && (fflg==0 || iflg)) {
;		printf("rm: %s not removed\n", arg);
;		++errcode;
;	}
;}
;
;dotname(s)
;char *s;
;{
;	if(s[0] == '.')
;		if(s[1] == '.')
;			if(s[2] == '\0')
;				return(1);
;			else
;				return(0);
;		else if(s[1] == '\0')
;			return(1);
;	return(0);
;}
;
;rmdir(f, iflg)
;char *f;
;{
;	int status, i;
;
;	if(dotname(f))
;		return(0);
;	if(iflg) {
;		printf("%s: ", f);
;		if(!yes())
;			return(0);
;	}
;	while((i=fork()) == -1)
;		sleep(3);
;	if(i) {
;		wait(&status);
;		return(status);
;	}
;	execl("/bin/rmdir", "rmdir", f, 0);
;	execl("/usr/bin/rmdir", "rmdir", f, 0);
;	printf("rm: can't find rmdir\n");
;	exit(1);
;}
;
;yes()
;{
;	int i, b;
;
;	i = b = getchar();
;	while(b != '\n' && b != EOF)
;		b = getchar();
;	return(i == 'y');
;}
