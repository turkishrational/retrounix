; ****************************************************************************
; cp386.s (cp1.s) - by Erdogan Tan - 20/04/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - copy -- cp oldfile newfile
;
; [ Last Modification: 24/04/2022 ]
;
; Derived from (original) UNIX v7 (& v7 x86) 'cp.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; v7.tar.gz
; ****************************************************************************
; [ v7.tar - usr/src/cmd/cp.c (archive date: 10-1-1979) ]
;
; Assembler: NASM v2.15
; ((nasm cp1.s -l cp1.txt -o cp1 -Z error.txt))
;
; cp1.s - 21/04/2022 - Retro UNIX 386 v1.2 (modified unix v7 inode)
; cp0.s - 21/04/2022 - Retro UNIX 386 v1 & v1.1
; cp8086.s - 22/04/2022 - Retro UNIX 8086 v1 (16 bit 'cp0.s')

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

%macro sys 1-4
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.		
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            ;%if %0 = 4
            %if	%0 >= 4 ; 11/03/2022
		mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    int 30h	   
%endmacro

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

; 11/03/2022
; Note: Above 'sys' macro has limitation about register positions;
;	ebx, ecx, edx registers must not be used after their
;	positions in sys macro.
; for example:
;	'sys _write, 1, msg, ecx' is defective, because
;	 ecx will be used/assigned before edx in 'sys' macro.
; correct order may be:
;	'sys _write, 1, msg, eax ; (eax = byte count)

struc stat
	; Note: This is for Retro UNIX v1.2 'sysstat' output !!!
	; (66 bytes)
	.inode:  resw 1	
	.mode:	 resw 1
	.nlinks: resw 1 
	.uid:	 resw 1
	.gid:	 resb 1
	.size_h: resb 1
	.size:	 resd 1
	.dskptr: resd 10
	.atime:	 resd 1
	.mtime:	 resd 1
	.ctime:  resd 1
	.strucsize:
endstruc   

;	; 20/04/2022
;struc stat
;	; Note: Retro UNIX v2 'sysstat' output DRAFT !!!
;	; (72 bytes)
;	.idev:	 resb 1
;	.rsvd:	 resb 3
;	.inum:   resd 1	
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

S_IFMT   equ 0F0h ; /* type of file */
S_IFDIR  equ 040h ; /* directory */
S_IFCHR  equ 020h ; /* character special */
S_IFBLK  equ 060h ; /* block special */
S_IFREG  equ 080h ; /* regular */
S_ISUID  equ 008h ; /* set user id on execution */
S_ISGID  equ 004h ; /* set group id on execution */
S_IREAD  equ 001h ; /* read permission, owner */
S_IWRITE equ 080h ; /* write permission, owner */
S_IEXEC  equ 040h ; /* execute/search permission, owner */

BSIZE equ 512

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 20/04/2022
	; main(argc, argv)

	mov	esi, esp		
	mov	edi, esi
	lodsd		; number of arguments
	;mov	edi, esi			
	;mov	[argc], eax
	mov	[argc], al

	;if (argc < 3) 
	;   goto usage;

	;cmp	eax, 3
	cmp	al, 3
	jnb	short cp_0  ; if (argc > 3) {
	dec	al	; 21/04/2022
	jnz	short cp_usage
	sys	_msg, program_msg, 255, 0Fh
cp_usage:
   ; fprintf(stderr, "Usage: cp: f1 f2; or cp f1 ... fn d2\n");
	sys	_msg, usage_msg, 255, 07h
cp_exit:
	sys	_exit	; sys exit
;hlt:
;	nop
;	nop
;	jmp	short hlt

cp_0:
	mov	edx, eax ; [argc]
	; 21/04/2022
	;;dec	edx  ; argc-1
	;dec	dl
	shl	dl, 2 ; * 4 
	add	edi, edx

	; 21/04/2022
	cmp	al,3
	jna	short cp_1

	sys	_stat, [edi], stbuf2
	jc	short cp_usage

	; check retro unix v2 inode flags
	;	(a bit different than unix v7 inode flags)
	; if it is a directory..
	;	regular file flag and dir flag must be 1
	mov	al, [stbuf2+stat.mode+1]
	and	al, S_IFDIR|S_IFREG
	cmp	al, S_IFDIR|S_IFREG ; directory ?
	jne	short cp_usage ; no
	
	; check if it is a device file
	;test	al, S_IFREG ; regular file ?
	;jz	short cp_usage ; no
	;and	al, S_IFDIR ; directory ?
	;jz	short cp_usage ; no
 
cp_1:	
	; esi = esp+4 = argv[0] ; executable file name (cp)
	lodsd	; 21/04/2022
cp_loop: ; for(i=1; i<argc-1;i++)
	;lodsd
	; ((esi = esp+8 = argv[1] ; (old) file 1))
	; esi = argv[i] 
 	; edi = argv[argc-1] ; *
	call	copy
	jnc	short cp_2

	inc	byte [errors]
cp_2:
	lodsd	; 21/04/2022
	cmp	esi, edi
	jb	short cp_loop

	; bypass 'OK.' message if there was an error
	cmp	byte [errors], 0
	ja	short cp_exit
cp_ok:
	sys	_msg, ok_msg, 255, 07h
;cp_exit:
	sys	_exit	; sys exit

;_halt:
;	nop
;	jmp	short _halt

copy:	; copy(from, to)
	;
	; 21/04/2022
	; 20/04/2022
	; INPUT:
	;	esi = pointer to file name to be copied
	;  	edi = ptr to new file name or destination dir
	; OUTPUT:
	;	cf = 0 -> OK
	;	cf = 1 -> Error !
	;
	; Modified registers: eax, ebx, ecx, edx, ebp
	;
	
	; open (old) file for read
	sys	_open, [esi], 0 
	jnc	short cp_3
	
	; esi = file name (from)

	;fprintf(stderr, "cp: cannot open %s\n", from);
	;	return(1);

	sys	_msg, cno_err_msg, 255, 07h
	; file name (from) 
	sys	_msg, [esi], 255, 07h
write_nl:
	; new/next line
	sys	_msg, nextline, 255, 07h
	stc	; return with error (cf=1)
	retn

cp_3:
	mov	[fold], eax ; file (descriptor) number

	; (from)
	sys	_fstat, [fold], stbuf1

	; save mode
	;mov	ax, [stbuf1.mode]
	;mov	[mode], ax
	
	; (to)
	;sys	_stat, [edi], stbuf2  ; stat(to, &stbuf2)
	;jnc	short cp_4
	;jmp	cp_9
	
	; 22/04/2022
	mov	ebp, [edi] ; ! (cp_8) !
	sys	_stat, ebp, stbuf2  ; stat(to, &stbuf2)
	jnc	short cp_4
	jmp	cp_9

cp_4:
	; /* is target a directory? */

	; check retro unix v2 inode flags
	;	(a bit different than unix v7 inode flags)
	; regular file flag and dir flag must be 1 for a dir
	mov	al, [stbuf2+stat.mode+1]
	and	al, S_IFDIR|S_IFREG
	cmp	al, S_IFDIR|S_IFREG ; directory ?
	jne	short cp_8 ; no, overwrite (create file)
			   ; (if the new file is not same file) 	

	; check if it is a device file
	;test	al, S_IFREG ; regular file ?
	;jz	short cp_error ; no
	;and	al, S_IFDIR ; directory ?
	;jz	short cp_error ; no

	; add (old) file name to (destination ) path
	; (directory name +'/'+ file name)  
	
	mov	ebp, esi   ; save esi
	mov	esi, [edi] ; directory name address
	mov	ebx, edi   ; save edi			
	mov	edi, iobuf ; (new) path name buffer addr	

	; p1 = from;
	; p2 = to;
	; bp = iobuf;
cp_5:	; while(*bp++ = *p2++)
	lodsb	
	stosb
	and	al, al
	jnz	short cp_5
	mov	byte [edi-1], '/' ; bp[-1] = '/';
	; p2 = bp
	mov	edx, edi
	mov	esi, [ebp] ; *p1 ; from
cp_6:	; while(*bp = *p1++)
	lodsb
	stosb
	or	al, al
	jz	short cp_7
	cmp	al, '/' ; if (*bp++ == '/')
	jne	short cp_6
	; bp = p2
	; 21/04/2022
	mov	edi, edx ; (discard path before file name)
	jmp	short cp_6
cp_7:	
	mov	esi, ebp ; restore esi
	mov	edi, ebx ; restore edi
	; to = iobuf
	mov	ebp, iobuf
		
	sys	_stat, ebp, stbuf2  ; stat(to, &stbuf2) >= 0
	jc	short cp_10 ; create new file

cp_8:
	;if (stbuf1.st_dev == stbuf2.st_dev &&
	;   stbuf1.st_ino == stbuf2.st_ino) {
	;	fprintf(stderr, "cp: cannot copy file to itself.\n");
	; 	return(1);

	mov	ax, [stbuf1+stat.inode]
	cmp	ax, [stbuf2+stat.inode]
	jne	short cp_10

	; same file ! error...
	mov	ebp, cncis_err_msg ; error message
	; esi = file name (from)
write_err_msg:
	sys	_msg, ebp, 255, 07h
	; close (old) file
	sys	_close, [fold]
	stc	; return with error (cf=1)
	retn

cp_9:
	; 22/04/2022
	; ebp = [edi]
	; 21/04/2022
	; new file (asciiz name address)
	;mov	ebp, [edi]

	; create new file (truncate if it exists) 
cp_10:
	; fnew = creat(to, mode)
	movzx 	ecx, word [stbuf1+stat.mode]
	; ecx = mode 
	sys	_creat, ebp 
	jnc	short cp_11

	;if ((fnew = creat(to, mode)) < 0) {
	;	fprintf(stderr, "cp: cannot create %s\n", to);
	;	close(fold);
	;	return(1);

	; 24/04/2022
	sys	_close, [fold]

	; error message
	sys	_msg, ccf_err_msg, 255, 07h

	; 24/04/2022
	; and file name (to) -at the end of error message-
	sys	_msg, ebp, 255, 07h 
	; write next line (move cursor to next line)
	; and return (from this/copy subroutine)
	jmp	write_nl

cp_11:	
	; 21/04/2022
	mov	[fnew], eax
cp_rw_next:
	; while(n = read(fold, iobuf, BSIZE))
	sys	_read, [fold], iobuf, BSIZE
	jnc	short cp_12

	;if (n < 0) {
	;   fprintf(stderr, "cp: read error\n");

	; write read error message
	mov	ebp, crd_err_msg

cp_rw_err:
	; 21/04/2022
	; cf = 1
	sys	_close, [fnew]	
	jmp	write_err_msg

cp_12:
	; eax = read count
	; eax = 0 -> eof
	or	eax, eax
	jz	short cp_14 ; eof

	mov	ebp, eax  ; n	
	; write(fnew, iobuf, n)
	sys	_write, [fnew], iobuf, ebp
	jc	short cp_13

	;if (write(fnew, iobuf, n) != n)
	;   fprintf(stderr, "cp: write error.\n");
	;   close(fold);
	;   close(fnew);
	;   return(1);

	; eax = written bytes
	cmp	eax, ebp
	;je	short cp_11 ; read next (block)
	; 21/04/2022
	je	short cp_rw_next
	; error !
	; eax < ebp --> cf = 1
cp_13:
	; close new file
	; and then write error mesage
	; and then close old file
	; and then write (move cursor to) next line
	; and then return (from subroutine) 
	;sys	_close, [fnew]

	; write error message
	mov	ebp, cwr_err_msg
	;jmp	short write_err_msg
	; 21/04/2022
	jmp	short cp_rw_err

	; eof
cp_14:
	;close(fold);
	;close(fnew);
	;return(0);
	
	sys	_close, [fold]
	sys	_close, [fnew]
	
	;clc
	retn
	
;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;argc:	dd 0
argc:	db 0

; ----------------------------------------------------------------

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 386 v1.2 COPY by Erdogan TAN - 24/04/2022'
	db  0Dh, 0Ah, 0

usage_msg:
	db  0Dh, 0Ah
	db  'Usage: cp: f1 f2; or cp f1 ... fn d2'
nextline:
	db  0Dh, 0Ah, 0

cno_err_msg:
	db 0Dh, 0Ah
	db 'cp: cannot open '
	db 0
cncis_err_msg:
	db 0Dh, 0Ah
	db 'cp: cannot copy file to itself.'
	db 0Dh, 0Ah, 0

ccf_err_msg:
	db 0Dh, 0Ah
	db 'cp: cannot create '
	db 0

crd_err_msg:
	db 0Dh, 0Ah
	db 'cp: read error.'
	db 0Dh, 0Ah, 0

cwr_err_msg:
	db 0Dh, 0Ah
	db 'cp: write error.'
	db 0Dh, 0Ah, 0

ok_msg:
	db  0Dh, 0Ah
	db  'OK.'
	db  0Dh, 0Ah, 0

errors:	db 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 2

bss_start:

ABSOLUTE bss_start

; 20/04/2022
fold:	resd 1
fnew:	resd 1

stbuf2:	resb 66 ; for Retro UNIX 386 v1.2 (66 byte sysstat data)
stbuf1:	resb 66 ; for Retro UNIX 386 v1.2 (66 byte sysstat data)

iobuf:	resb BSIZE ; resb 512 ; path name buffer

; 20/04/2022
;-----------------------------------------------------------------
; Original UNIX v7 - cp (utility) c source code (cp.c)
;-----------------------------------------------------------------
;/* UNIX V7 source code: see www.tuhs.org for details. */;
;
;/*
; * cp oldfile newfile
; */
;
;#define BSIZE	512
;#include <stdio.h>
;#include <sys/types.h>
;#include <sys/stat.h>
;struct	stat stbuf1, stbuf2;
;char iobuf[BSIZE];
;
;main(argc, argv)
;char *argv[];
;{
;	register i, r;
;
;	if (argc < 3) 
;		goto usage;
;	if (argc > 3) {
;		if (stat(argv[argc-1], &stbuf2) < 0)
;			goto usage;
;		if ((stbuf2.st_mode&S_IFMT) != S_IFDIR) 
;			goto usage;
;	}
;	r = 0;
;	for(i=1; i<argc-1;i++)
;		r |= copy(argv[i], argv[argc-1]);
;	exit(r);
;usage:
;	fprintf(stderr, "Usage: cp: f1 f2; or cp f1 ... fn d2\n");
;	exit(1);
;}
;
;copy(from, to)
;char *from, *to;
;{
;	int fold, fnew, n;
;	register char *p1, *p2, *bp;
;	int mode;
;	if ((fold = open(from, 0)) < 0) {
;		fprintf(stderr, "cp: cannot open %s\n", from);
;		return(1);
;	}
;	fstat(fold, &stbuf1);
;	mode = stbuf1.st_mode;
;	/* is target a directory? */
;	if (stat(to, &stbuf2) >=0 &&
;	   (stbuf2.st_mode&S_IFMT) == S_IFDIR) {
;		p1 = from;
;		p2 = to;
;		bp = iobuf;
;		while(*bp++ = *p2++)
;			;
;		bp[-1] = '/';
;		p2 = bp;
;		while(*bp = *p1++)
;			if (*bp++ == '/')
;				bp = p2;
;		to = iobuf;
;	}
;	if (stat(to, &stbuf2) >= 0) {
;		if (stbuf1.st_dev == stbuf2.st_dev &&
;		   stbuf1.st_ino == stbuf2.st_ino) {
;			fprintf(stderr, "cp: cannot copy file to itself.\n");
;			return(1);
;		}
;	}
;	if ((fnew = creat(to, mode)) < 0) {
;		fprintf(stderr, "cp: cannot create %s\n", to);
;		close(fold);
;		return(1);
;	}
;	while(n = read(fold,  iobuf,  BSIZE)) {
;		if (n < 0) {
;			fprintf(stderr, "cp: read error\n");
;			close(fold);
;			close(fnew);
;			return(1);
;		} else
;			if (write(fnew, iobuf, n) != n) {
;				fprintf(stderr, "cp: write error.\n");
;				close(fold);
;				close(fnew);
;				return(1);
;			}
;	}
;	close(fold);
;	close(fnew);
;	return(0);
;}