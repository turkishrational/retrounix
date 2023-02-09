; ****************************************************************************
; pwd8086.s (pwd0.s) - by Erdogan Tan - 05/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - pwd - print working directory pathname
;
; [ Last Modification: 14/05/2022 ]
;
; Derived from (original) UNIX v5 'pwd.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; v5root.tar.gz
; ****************************************************************************
; [ usr/source/s2/pwd.c (archive date: 27-11-1974) ]

; pwd0.s - Retro UNIX 8086 v1 (16 bit version of 'pwd1.s')
; pwd1.s - Retro UNIX 386 v1 (unix v1 inode structure)
; pwd2.s - Retro UNIX 386 v1.1 (16 byte directory entries)
; pwd3.s - Retro UNIX 386 v1.2 (& v2) (modified unix v7 inode)

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

; ----------------------------------------------------------------------------

[BITS 16] ; 16-bit intructions (for 8086/x86 real mode)

[ORG 0] 

START_CODE:
	; 14/05/2022
	; 11/05/2022
	; 10/05/2022
	; 09/05/2022
	; 06/05/2022 (16 bit version) -ax,bx,cx,dx-
	; 06/05/2022 (32 bit version) -eax,ebx,ecx,edx-
	; 05/05/2022

;main() {
;	int n;

	;  clear (reset) stack (not necessary)
	pop	cx ; cx = number of arguments
pwd_0:
	pop	ax ; ax = argument 0 = executable file name
	loop	pwd_0

	;mov	byte [y.zero], 0 ; (not necessary)
			; (because Retro UNIX kernel
			; clears bss -memory page- while
			; assigning it to program)
	;mov	word [off], 0

	; 06/05/2022
	dec	word [off] ; -1

;loop0:
;	stat(dot, &x);
;	if((file = open(dotdot,0)) < 0) prname();

loop0:
	;sys	_stat, dot, _x

	; 11/05/2022
	;stat(dot, &d);
	;if (d.st_ino==rino && d.st_dev==rdev)
	;	prname();
	
	sys	_stat, dot, _d
	jc	short error

	cmp	word [d.ino], 41 ; d.st_ino == rino
	jne	short pwd_1
	or	ax, ax ; [d.dev] ; && d.st_dev==rdev
	;jz	short prname ; root device, root dir
	jz	short pwd_p ; 11/05/2022
pwd_1:
	mov	[d.dev], ax

	sys	_open, dotdot, 0  ; open for read
	;jc	short prname
	; 11/05/2022
	jnc	short pwd_2

	; 11/05/2022
pwd_err:
	mov	ax, err_1
pmsg_exit:
	call	print_msg
exit:
	sys	_exit ; exit(1);
;hang:
;	nop
;	jmp	short hang

	
pwd_p:	; 11/05/2022
	jmp	prname
	
error:	; 11/05/2022
	mov	ax, err_0
	jmp	short pwd_err

pwd_2:	

;loop1:
;	if((n = read(file,&y,16)) < 16) prname();
;	if(y.jnum != x.inum)goto loop1;
;	close(file);
;	if(y.jnum == 1) ckroot();
;	cat();
;	chdir(dotdot);
;	goto loop0;
;}

	mov	[file], ax

	; 11/05/2022
	;fstat(file, &dd);
	;chdir(dotdot);

	sys	_fstat, ax, _dd
	jc	short error

	;mov	[dd.dev], ax ; 14/05/2022
	mov	cx, ax
	
	sys	_chdir, dotdot
	jc	short error

	cmp	cx, [d.dev]
	jne	short pwd_4

	mov	ax, [d.ino]
	cmp	ax, [dd.ino]
	je	short prname
	
	; do .. while
loop1:
	sys	_read, [file], _y, 10 ; Retro UNIX 386 v1
	;;sys	_read, [file], _y, 16 ; Retro UNIX 386 v1.1 & v1.2
	;jc	short prname
	; 11/05/2022
	jc	short pwd_5
	;cmp	ax, 10	 ; cmp eax, 16
	;;jb	short prname	
	;jb	short pwd_5 ; 11/05/2022
	or	ax, ax
	;jz	short prname
	jz	short pwd_5 ; 11/05/2022
	
	; 11/05/2022
	; while (dir.d_ino != d.st_ino);
	mov	ax, [jnum]	   
	;cmp	ax, [inum]
	cmp	ax, [d.ino]
	jne	short loop1

pwd_3:
	sys	_close, [file]
	
	;;cmp	word [jnum], 1	; Retro UNIX 386 v1.2
	;cmp	word [jnum], 41 ; root dir inode number
	;je	short ckroot
	;call	cat
	;sys	_chdir, dotdot
	;jmp	short loop0

	; 11/05/2022
	call	cat
	jmp	loop0  ; for (;;)

pwd_4:
loop_2:
	; 11/05/2022
	; else do .. while
	sys	_read, [file], _y, 10 ; Retro UNIX 386 v1
	;sys	_read, [file], _y, 16 ; Retro UNIX 386 v1.1 & v1.2
	jc	short pwd_5
	and	ax, ax
	jnz	short pwd_6
pwd_5:
	mov	ax, err_2
	jmp	pmsg_exit
pwd_6:
	; stat(dir.d_name, &dd);
	sys	_stat, yname, _dd
	jnc	short pwd_7
	jmp	error
pwd_7:
	; while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
	mov	cx, [d.ino]
	cmp	cx, [dd.ino]
	jne	short loop_2
	cmp	ax, [d.dev]
	jne	short loop_2
	jmp	short pwd_3 	

;prname() {
;	name[off] = '\n';
;	write(1,name,off+1);
;	exit();

prname:
	sys	_write, 1, nextline, 3	; 06/05/2022

	mov	si, [off]
	
	;if (off<0) off = 0;	
	inc	si ; -1 -> 0
	jz	short prname_crlf
	dec	si
prname_crlf:
	;mov	byte [si+name], 0Dh ; CR
	mov	word [si+name], 0A0Dh  ; CRLF 
	;inc	dword [off]
	inc	si
	inc	si
	;mov	[off], si
	;sys	_write, 1, name, [off]
	sys	_write, 1, name, si
_exit_:
	sys	_exit  ; exit(0);
;hang:
;	nop
;	jmp	short hang

;ckroot() {
;	int i, n;
;
;	if((n = stat(y.name,&x)) < 0) prname();
;	i = x.devn;
;	if((n = chdir(root)) < 0) prname();
;	if((file = open(root,0)) < 0) prname();
;loop:
;	if((n = read(file,&y,16)) < 16) prname();
;	if(y.jnum == 0) goto loop;
;	if((n = stat(y.name,&x)) < 0) prname();
;	if(x.devn != i) goto loop;
;	x.i[0] =& 060000;
;	if(x.i[0] != 040000) goto loop;
;	cat();
;	prname();

;	; 10/05/2022
;	; 09/05/2022 (new -retro unix 8286 v1- kernel)
;	; (sysstat returns with device number in ax)
;ckroot:
;	; 09/05/2022	
;	sys	_stat, yname, _x
;	jc	short ckroot_err
;	mov	[idev], ax ; device number
;	sys	_chdir, root
;	jc	short ckroot_err
;	sys	_open, root, 0 ; open root dir for read
;	jc	short ckroot_err
;	; 10/05/2022
;	mov	[file], ax
;ckroot_loop:
;	sys	_read, [file], _y, 10
;			; read one dir entry	
;	jc	short ckroot_err
;	; 10/05/2022
;	or	ax, ax
;	jz	short ckroot_err	
;	cmp	word [jnum], 0
;	jna	short ckroot_loop
;	sys	_stat, yname, _x
;	jc	short ckroot_err
;	cmp	ax, [idev] ; same device number ?
;	jne	short ckroot_loop ; no
;	; 10/05/2022
;	test	byte [ximode+1], 40h ; directory ?
;	jz	short ckroot_loop
;	;
;	call	cat	
;	;
;ckroot_err:
;	jmp	prname
	
;	; 06/05/2022
;	; Note: For Retro UNIX 386 v1 & v1.1 & v1.2,
;	;	mounted device is handled via 'mnti'
;	;	field which keeps (device 0) mounting
;	;	directory inode number for mounted
;	;	device (1). Device number (0 or 1) check
;	;	(via 'sysstat') is not possible
;	;	for current Retro UNIX kernel versions
;	;	because 'sysstat' output does not contain
;	;	device number.
;	;	***
;	;	As a temporary solution (before a next
;	;	kernel version with new 'sysstat'),
;	;	root dir entries are checked for a subdir 
;	;	('usr' & 'mnt' dirs for now) with root dir
;	;	inode number (41 or 1), if there is..
;	;	root directory is a real (device 0) root
;	;	directory; if not, device 1 root directory 
;	;	is mounted to a sub directory of device 0.
;	; 	(Also it must be verified after 'chdir /')
;	;
;	;	assumptions...
;	;	a sub dir with root inode number	
;	;	dotdot / - chdir /
;	;	   no    -  no ----> root
;	;	   yes 	 - (yes) --> root, not checked
;	;	   no    - yes ----> mounted
;	;	   yes   - (no) ---> root, not checked				  
;
;ckroot:
;	; 06/05/2022
;	; check 'usr' directory
;	mov	dx, usr
;	call	statm 
;	jz	short ckroot_2	; root dir
;	; check 'mnt' directory
;	mov	dx, mnt
;	call	statm 
;	jz	short ckroot_2	; root dir
;
;	sys	_chdir, root
;	jc	short ckroot_2	; jmp short prname
;
;	; check 'usr' directory
;	mov	dx, usr
;	call	statm 
;	jz	short ckroot_1	; mounted
;	; check 'mnt' directory
;	mov	dx, mnt
;	call	statm 
;	jnz	short ckroot_2	; not mounted
;
;	; mounted
;ckroot_1:
;	; move mounting directory name
;	mov	si, dx
;	mov	di, yname
;	movsw
;	movsw
;	; concatenate (add to head of the path)
;	call	cat
;ckroot_2:
;	jmp	prname
;
;statm:
;	; 06/05/2022
;	sys	_stat, dx, _x
;	jc	short statm_1
;	;cmp	word [inum], 1	; Retro UNIX 386 v1.2
;	cmp	word [inum], 41 ; root dir inode number
;statm_1:
;	retn

;cat() {
;	int i, j;
;
;	i = -1;
;	while(y.name[++i] != 0);
;	if((off+i+2) > 511) prname();
;	for(j=off+1; j>=0; --j) name[j+i+1] = name[j];
;	off=i+off+1;
;	name[i] = root[0];
;	for(--i; i>=0; --i) name[i] = y.name[i];
;}

cat:
	xor	bx, bx
	dec	bx ; i = -1
cat_0:
	inc	bx ; ++i
	cmp	byte [yname+bx], 0
	ja	short cat_0
	mov	dx, bx ; i
	mov	cx, [off]
	inc	cx ; j = [off]+1
	add	dx, cx  ; dx = [off]+i+1
	cmp	dx, 511-2 ; (+ CRLF + 0) 
	ja	short prname
	;;ja	short ckroot_2 ; jmp prname
	;ja	short ckroot_err ; 09/05/2022
	mov	si, name
	add	si, cx ; name[j]
	mov	di, si
	add	di, bx ; name[j+i]
	inc	di ; name[j+i+1]	
cat_1:
	;std
	;rep	stosb
	;cld
	dec	cx
	js	short cat_2
	dec	si
	mov	al, [si]
	dec	di
	mov	[di], al
	jmp	short cat_1
cat_2:
	mov	[off], dx ; [off] = i+[off]+1
	mov	byte [name+bx], '/' ; name[i] = '/';
cat_3:
	dec	bx ; --i
	js	short cat_4 ; 0 -> -1
		 ; name[i] = yname[i]
	mov	al, [yname+bx]
	mov	[name+bx], al
	jmp	short cat_3 ; i >= 0
cat_4:
	retn

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

; 05/05/2022

dotdot:	db '.'
dot:	db '.'
	db 0

; 06/05/2022
nextline:
	db 0Dh, 0Ah
root:	db '/', 0

; 14/05/2022
; default mounting directories (for current retro unix version)
;usr:	db 'usr', 0
;mnt:	db 'mnt', 0

; 11/05/2022
err_0:	db 0Dh, 0Ah
	db "Error!"
	db 0dh, 0Ah, 0

err_1:	db 0Dh, 0Ah
	db "pwd: can not open .."
	db 0Dh, 0Ah, 0
err_2:	db 0Dh, 0Ah
	db "read error in .."
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

; 06/05/2022

name:	resb 512

file:	resw 1
off:	resw 1

; 09/05/2022
idev:
; 11/05/2022
d.dev:	resw 1 ; device number (0 = root, 1 = mounted)
;dd.dev: resw 1 ; 14/05/2022

; 06/05/2022
_x:	; stat(us) buffer
_d:	; 11/05/2022
inum:
d.ino:	; 11/05/2022
	resw 1
ximode:
d.mode: ; 11/05/22022
	;resb 64 ; Retro UNIX 386 v1.2
	resb 32 ; Retro UNIX 386 v1 & v1.1

; 11/05/2022
_dd:	; stat(us) buffer
dd.ino:	resw 1
dd.mode:
	resb 32

; 06/05/2022
_y:	; directory entry buffer
jnum:	resw 1
yname:	;resb 14 ; Retro UNIX 386 v1.1 & v1.2
	resb 8 ; Retro UNIX 386 v1 (unix v1)

yzero:	resw 1

; 05/05/2022

;-----------------------------------------------------------------
; Original UNIX v7 - /bin/pwd file - c source code (pwd.c)
;-----------------------------------------------------------------
;
;/*
; * Print working (current) directory
; */
;#include <stdio.h>
;#include <sys/param.h>
;#include <sys/stat.h>
;#include <sys/dir.h>
;
;char	dot[]	= ".";
;char	dotdot[] = "..";
;char	name[512];
;int	file;
;int	off	= -1;
;struct	stat	d, dd;
;struct	direct	dir;
;
;main()
;{
;	int rdev, rino;
;
;	stat("/", &d);
;	rdev = d.st_dev;
;	rino = d.st_ino;
;	for (;;) {
;		stat(dot, &d);
;		if (d.st_ino==rino && d.st_dev==rdev)
;			prname();
;		if ((file = open(dotdot,0)) < 0) {
;			fprintf(stderr,"pwd: cannot open ..\n");
;			exit(1);
;		}
;		fstat(file, &dd);
;		chdir(dotdot);
;		if(d.st_dev == dd.st_dev) {
;			if(d.st_ino == dd.st_ino)
;				prname();
;			do
;				if (read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
;					fprintf(stderr,"read error in ..\n");
;					exit(1);
;				}
;			while (dir.d_ino != d.st_ino);
;		}
;		else do {
;				if(read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
;					fprintf(stderr,"read error in ..\n");
;					exit(1);
;				}
;				stat(dir.d_name, &dd);
;			} while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
;		close(file);
;		cat();
;	}
;}
;
;prname()
;{
;	write(1, "/", 1);
;	if (off<0)
;		off = 0;
;	name[off] = '\n';
;	write(1, name, off+1);
;	exit(0);
;}
;
;cat()
;{
;	register i, j;
;
;	i = -1;
;	while (dir.d_name[++i] != 0);
;	if ((off+i+2) > 511)
;		prname();
;	for(j=off+1; j>=0; --j)
;		name[j+i+1] = name[j];
;	off=i+off+1;
;	name[i] = '/';
;	for(--i; i>=0; --i)
;		name[i] = dir.d_name[i];
;}

; 05/05/2022

;-----------------------------------------------------------------
; Original UNIX v5 - /usr/bin/pwd file - c source code (pwd.c)
;-----------------------------------------------------------------
;
;char dot[] ".";
;char dotdot[] "..";
;char root[] "/";
;char name[512];
;int file, off -1;
;struct statb {int devn, inum, i[18];}x;
;struct entry { int jnum; char name[16];}y;
;
;main() {
;	int n;
;
;loop0:
;	stat(dot, &x);
;	if((file = open(dotdot,0)) < 0) prname();
;loop1:
;	if((n = read(file,&y,16)) < 16) prname();
;	if(y.jnum != x.inum)goto loop1;
;	close(file);
;	if(y.jnum == 1) ckroot();
;	cat();
;	chdir(dotdot);
;	goto loop0;
;}
;ckroot() {
;	int i, n;
;
;	if((n = stat(y.name,&x)) < 0) prname();
;	i = x.devn;
;	if((n = chdir(root)) < 0) prname();
;	if((file = open(root,0)) < 0) prname();
;loop:
;	if((n = read(file,&y,16)) < 16) prname();
;	if(y.jnum == 0) goto loop;
;	if((n = stat(y.name,&x)) < 0) prname();
;	if(x.devn != i) goto loop;
;	x.i[0] =& 060000;
;	if(x.i[0] != 040000) goto loop;
;	cat();
;	prname();
;}
;prname() {
;	name[off] = '\n';
;	write(1,name,off+1);
;	exit();
;}
;cat() {
;	int i, j;
;
;	i = -1;
;	while(y.name[++i] != 0);
;	if((off+i+2) > 511) prname();
;	for(j=off+1; j>=0; --j) name[j+i+1] = name[j];
;	off=i+off+1;
;	name[i] = root[0];
;	for(--i; i>=0; --i) name[i] = y.name[i];
;}