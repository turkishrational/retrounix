; ****************************************************************************
; ln8086.s (ln0.s) - by Erdogan Tan - 18/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - ln -- link file (to file)
;
; [ Last Modification: 21/05/2022 ]
;
; Derived from (original) UNIX v2 (ln.s) and v5 'ln.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; ****************************************************************************
; [ v5root.tar - usr/source/s1/ln.c (archive date: 27-11-1974) ]
;
; Assembler: NASM v2.15
; ((nasm ln0.s -l ln0.txt -o ln0 -Z error.txt))

; ln0.s - 21/05/2022 - Retro UNIX 8086 v1 (16 bit 'ln1.s')
; ln1.s - 18/05/2022 - Retro UNIX 386 v1 & v1.1 & v1.2

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

;;struc stat
;;	; Note: Retro UNIX v2 'sysstat' output DRAFT !!!
;;	; (72 bytes)
;;	.idev:	 resb 1
;;	.rsvd:	 resb 3
;;	.inum:   resd 1	
;;	.mode:	 resw 1
;;	.nlinks: resw 1 
;;	.uid:	 resw 1
;;	.gid:	 resb 1
;;	.size_h: resb 1
;;	.size:	 resd 1
;;	.dskptr: resd 10
;;	.atime:	 resd 1
;;	.mtime:	 resd 1
;;	.ctime:  resd 1
;;	.strucsize:
;;endstruc

;;S_IFMT   equ 0F000h ; /* type of file */
;;S_IFDIR  equ 04000h ; /* directory */
;;S_IFCHR  equ 02000h ; /* character special */
;;S_IFBLK  equ 06000h ; /* block special */
;;S_IFREG  equ 08000h ; /* regular */
;;S_ISUID  equ 00800h ; /* set user id on execution */
;;S_ISGID  equ 00400h ; /* set group id on execution */
;;S_IREAD  equ 00100h ; /* read permission, owner */
;;S_IWRITE equ 00080h ; /* write permission, owner */
;;S_IEXEC  equ 00040h ; /* execute/search permission, owner */
;
;S_IFMT   equ 0F0h ; /* type of file */
;S_IFDIR  equ 040h ; /* directory */
;S_IFCHR  equ 020h ; /* character special */
;S_IFBLK  equ 060h ; /* block special */
;S_IFREG  equ 080h ; /* regular */
;S_ISUID  equ 008h ; /* set user id on execution */
;S_ISGID  equ 004h ; /* set group id on execution */
;S_IREAD  equ 001h ; /* read permission, owner */
;S_IWRITE equ 080h ; /* write permission, owner */
;S_IEXEC  equ 040h ; /* execute/search permission, owner */

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

; UNIX v1 inode
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

; ----------------------------------------------------------------------------

[BITS 16] ; 16-bit intructions (for 8086/x86 real mode)

[ORG 0] 

START_CODE:
	; 21/05/2022
	; 18/05/2022
	; 17/05/2022
	mov	si, sp		
	pop	ax ; number of arguments
	pop	dx  ; argv[0]	
	;;mov	[argc], ax
	;mov	[argc], al

	;cmp	ax, 2
	cmp	al, 2
	jnb	short ln_0
	dec	al
	jnz	short ln_usage
	; 21/05/2022
	mov	ax, program_msg
	call	print_msg
ln_usage:
	mov	ax, usage_msg
	;call	print_msg
	jmp	short ln_5	
;ln_exit:
;	sys	_exit	; sys exit
;;hlt:
;;	nop
;;	nop
;;	jmp	short hlt

ln_0:
	; 21/05/2022
	pop	di ; argv[1]
	ja	short ln_3 ; al > 2

	; same file name (different directories)
	; (new file is in current directory)
	mov	si, di
	mov	dx, si
ln_1:
	lodsb
	or	al, al
	jz	short ln_2
	cmp	al, '/'
	jne	short ln_1
	mov	dx, si ; last '/' + 1
	jmp	short ln_1
ln_2:
	dec	dx  ; last '/'
	cmp	dx, di
	jna	short err
	inc	dx  ; last '/' + 1 ; = argv[2]
	jmp	short ln_4
ln_3:
	; different file name
	pop	dx ; argv[2]
ln_4:
	sys	_stat, di, stbuf
	jc	short ln_not_exists

	; check if it is a directory..
	mov	al, [stbuf+stat.mode+1]
	; 21/05/2022
	;and	al, S_IFDIR|S_ALLOC ; (S_ALLOC = S_IFREG)
	;cmp	al, S_IFDIR|S_ALLOC ; directory ?
	;je	short ln_is_dir ; yes
	and	al, S_IFDIR
	jnz	short ln_is_dir ; yes		

	sys	_link, di, dx
	jnc	short ln_6
	
	; 21/05/2022
	mov	ax, cant_link_msg
	call	print_msg
	mov	ax, di
	jmp	short ln_5

err:
	mov	ax, err_msg
	jmp	short ln_5

ln_p_error:
	mov	ax, ln_header
	call	print_msg
	mov	ax, di
	call	print_msg
	mov	ax, bp
ln_5:	 
	call	print_msg
ln_exit:
	sys	_exit	; sys exit
;hlt:
;	nop
;	nop
;	jmp	short hlt

ln_6:
	mov	ax, ok_msg
	jmp	short ln_5

ln_not_exists:
	mov	bp, not_exists_msg
	jmp	short ln_p_error

ln_is_dir:
	mov	bp, is_dir_msg
	jmp	short ln_p_error

;-----------------------------------------------------------------

print_msg:
	; ax = asciiz string address
	mov	si, ax
	dec	si
nextchr:
	inc	si
	cmp	byte [si], 0
	ja	short nextchr
	;cmp	[si], 0Dh
	;ja	short nextchr
	sub	si, ax
	; si = asciiz string length
	;
	sys	_write, 1, ax, si
	;
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;;argc:	dd 0
;argc:	db 0

; ----------------------------------------------------------------

program_msg:
	db  0Dh, 0Ah
	db  'Retro UNIX 386 v1 LINK by Erdogan TAN - 21/05/2022'
	db  0Dh, 0Ah, 0

usage_msg:
	db  0Dh, 0Ah
	db  'Usage: ln target [ newname ]'
nextline:
	db  0Dh, 0Ah, 0
ln_header:
	db 0Dh, 0Ah
	db 'ln: '
	db 0
not_exists_msg:
	;db 0Dh, 0Ah
	db ' does not exist '
	db 0Dh, 0Ah, 0

is_dir_msg:
	db ' is a directory '
	db 0Dh, 0Ah, 0

cant_link_msg:
	db 0Dh, 0Ah
	db 'Can not link '
	;db 0Dh, 0Ah, 0
	db 0 ; 21/05/2022

err_msg:
	db 0Dh, 0Ah
	db 'Error! '
	db 0Dh, 0Ah, 0

ok_msg:
	db  0Dh, 0Ah
	db  'OK.'
	db  0Dh, 0Ah, 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 2

bss_start:

ABSOLUTE bss_start

stbuf:	resb 34 ; for Retro UNIX 386 v1.1 (34 byte sysstat data)
;stbuf:	resb 66 ; for Retro UNIX 386 v1.2 (66 byte sysstat data)

; 17/05/2022
;-----------------------------------------------------------------
; Original UNIX v2 - ln (utility) pdp-11 asm source code (ln.s)
;-----------------------------------------------------------------
;/* UNIX V2 source code: see www.tuhs.org for details. */;
;
;/*
; * ln target [ new name ]
; */
; ----------------------------------------------------------------
;/  link command
;
;ln:
;	mov	sp,r5
;	cmp	(r5)+,$2
;	bhis	1f
;	sys	exit
;1:
;	beq	1f
;	tst	(r5)+
;	mov	(r5)+,0f
;	mov	(r5),0f+2
;	br	2f
;1:
;	tst	(r5)+
;	mov	(r5),0f
;	mov	(r5),r4
;1:
;	tstb	(r4)+
;	bne	1b
;1:
;	cmpb	-(r4),$'/
;	beq	1f
;	cmp	(r5),r4
;	bne	1b
;	br	err
;1:
;	inc	r4
;	mov	r4,0f+2
;2:
;	mov	0f,2f
;	sys	stat; 2:..; stbuf
;	bes	err
;	bit	$40000,stbuf+2
;	bne	err
;	sys	link; 0:..; ..
;	bes	err
;	sys	exit
;
;err:
;	mov	$1,r0
;	sys write; quest; 2
;	sys	exit
;
;quest:
;	<?\n>
;
;.bss
;stbuf:	.=.+40.

;-----------------------------------------------------------------
; Original UNIX v5 - ln (utility) c source code (ln.c)
;-----------------------------------------------------------------
;/* UNIX V5 source code: see www.tuhs.org for details. */;
;
;/*
; * ln target [ new name ]
; */
;
;struct ibuf {
;	int	inum;
;	int	iflags;
;	char	inl;
;	char	iuid;
;	int	isize;
;	int	iaddr[8];
;	char	*ictime[2];
;	char	*imtime[2];
;	int	fill;
;};
;
;#define DIR	040000
;#define FMT	060000
;
;main(argc, argv)
;char **argv;
;{
;	static struct ibuf statb;
;	register char *np;
;
;	if (argc<2) {
;		write(1, "Usage: ln target [ newname ]\n", 29);
;		exit(1);
;	}
;	if (argc==2) {
;		np = argv[1];
;		while(*np++);
;		while (*--np!='/' && np>argv[1]);
;		np++;
;		argv[2] = np;
;	}
;	stat(argv[1], &statb);
;	if ((statb.iflags&FMT) == DIR) {
;		write(1, "No directory link\n", 18);
;		exit(1);
;	}
;	if (link(argv[1], argv[2])<0) {
;		write(1, "Can't link\n", 11);
;		exit(1);
;	}
;	exit(0);
;}

;-----------------------------------------------------------------
; Original UNIX v7 - ln (utility) c source code (ln.c)
;-----------------------------------------------------------------
;/* UNIX V7 source code: see www.tuhs.org for details. */;
;
;/*
; * ln [ -f ] target [ new name ]
; */
;
;#include <sys/types.h>
;#include <sys/stat.h>
;#include "stdio.h"
;char	*rindex();
;
;main(argc, argv)
;char **argv;
;{
;	struct stat statb;
;	register char *np;
;	int fflag = 0;
;	char nb[100], *name=nb, *arg2;
;	int statres;
;
;	if (argc >1 && strcmp(argv[1], "-f")==0) {
;		argc--;
;		argv++;
;		fflag++;
;	}
;	if (argc<2 || argc>3) {
;		printf("Usage: ln target [ newname ]\n");
;		exit(1);
;	}
;	np = rindex(argv[1], '/');
;	if (np==0)
;		np = argv[1];
;	else
;		np++;
;	if (argc==2)
;		arg2 = np;
;	else
;		arg2 = argv[2];
;	statres = stat(argv[1], &statb);
;	if (statres<0) {
;		printf ("ln: %s does not exist\n", argv[1]);
;		exit(1);
;	}
;	if (fflag==0 && (statb.st_mode&S_IFMT) == S_IFDIR) {
;		printf("ln: %s is a directory\n", argv[1]);
;		exit(1);
;	}
;	statres = stat(arg2, &statb);
;	if (statres>=0 && (statb.st_mode&S_IFMT) == S_IFDIR)
;		sprintf(name, "%s/%s", arg2, np);
;	else
;		name = arg2;
;	if (link(argv[1], name)<0) {
;		perror("ln");
;		exit(1);
;	}
;	exit(0);
;}
