; ****************************************************************************
; pwd386.s (pwd2.s) - by Erdogan Tan - 05/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1.1 - pwd - print working directory pathname
;
; [ Last Modification: 15/05/2022 ]
;
; Derived from (original) UNIX v5 (&v7) 'pwd.c' source Code
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

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 06/05/2022
	; 05/05/2022

;main() {
;	int n;

	;  clear (reset) stack (not necessary)
	pop	ecx ; ecx = number of arguments
pwd_0:
	pop	eax ; eax = argument 0 = executable file name
	loop	pwd_0

	;mov	byte [y.zero], 0 ; (not necessary)
			; (because Retro UNIX kernel
			; clears bss -memory page- while
			; assigning it to program)
	;mov	 dword [off], 0

	; 06/05/2022
	dec	dword [off] ; -1

;loop0:
;	stat(dot, &x);
;	if((file = open(dotdot,0)) < 0) prname();

loop0:
	;sys	_stat, dot, _x

	; 14/05/2022
	;stat(dot, &d);
	;if (d.st_ino==rino && d.st_dev==rdev)
	;	prname();

	sys	_stat, dot, _d
	jc	short error

	cmp	word [d.ino], 41 ; d.st_ino == rino
	jne	short pwd_1
	or	eax, eax
	;or	ax, ax ; [d.dev] ; && d.st_dev==rdev
	;jz	short prname ; root device, root dir
	jz	short pwd_p ; 14/05/2022
pwd_1:
	mov	[d.dev], ax

	; 15/05/2022
	sys	_open, dotdot, 0  ; open for read
	;jc	short prname
	; 14/05/2022
	jnc	short pwd_2

	; 14/05/2022
pwd_err:
	mov	eax, err_1
pmsg_exit:
	call	print_msg
exit:
	sys	_exit ; exit(1);
;hang:
;	nop
;	jmp	short hang


pwd_p:	; 14/05/2022
	jmp	prname
	
error:	; 14/05/2022
	mov	eax, err_0
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

	mov	[file], eax

	; 14/05/2022
	;fstat(file, &dd);
	;chdir(dotdot);

	sys	_fstat, eax, _dd
	jc	short error

	;mov	[dd.dev], ax
	mov	ecx, eax
	
	sys	_chdir, dotdot
	jc	short error

	cmp	cx, [d.dev]
	jne	short pwd_4

	mov	ax, [d.ino]
	cmp	ax, [dd.ino]
	;je	short prname
	; 14/05/2022
	jne	short loop1
	jmp	prname
	
	; do .. while
loop1:
	;;sys	_read, [file], _y, 10 ; Retro UNIX 386 v1
	sys	_read, [file], _y, 16 ; Retro UNIX 386 v1.1 & v1.2
	;jc	short prname
	; 14/05/2022
	jc	short pwd_5
	;cmp	eax, 16	 ; cmp eax, 10
	;;jb	short prname	
	;jb	short pwd_5 ; 14/05/2022
	or	eax, eax
	;jz	short prname
	jz	short pwd_5 ; 14/05/2022
	
	; 14/05/2022
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
	; 14/05/2022
	; else do .. while
	;sys	_read, [file], _y, 10 ; Retro UNIX 386 v1
	sys	_read, [file], _y, 16 ; Retro UNIX 386 v1.1 & v1.2
	jc	short pwd_5
	and	eax, eax
	jnz	short pwd_6
pwd_5:
	mov	eax, err_2
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

	mov	esi, [off]

	; 14/05/2022
	;if (off<0) off = 0;	
	inc	esi ; -1 -> 0
	jz	short prname_crlf
	dec	esi
prname_crlf:
	;mov	byte [esi+name], 0Dh ; CR
	mov	word [esi+name], 0A0Dh  ; CRLF 
	;inc	dword [off]
	inc	esi
	inc	esi
	;mov	[off], esi
	;sys	_write, 1, name, [off]
	sys	_write, 1, name, esi
_exit_:
	sys	_exit
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

; 14/05/2022
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
;	mov	edx, usr
;	call	statm 
;	jz	short ckroot_2	; root dir
;	; check 'mnt' directory
;	mov	edx, mnt
;	call	statm 
;	jz	short ckroot_2	; root dir
;
;	sys	_chdir, root
;	jc	short ckroot_2	; jmp short prname
;
;	; check 'usr' directory
;	mov	edx, usr
;	call	statm 
;	jz	short ckroot_1	; mounted
;	; check 'mnt' directory
;	mov	edx, mnt
;	call	statm 
;	jnz	short ckroot_2	; not mounted
;
;	; mounted
;ckroot_1:
;	; move mounting directory name
;	mov	esi, edx
;	mov	edi, yname
;	movsd
;	; concatenate (add to head of the path)
;	call	cat
;ckroot_2:
;	jmp	prname
;
;statm:
;	; 06/05/2022
;	sys	_stat, edx, _x
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
	xor	ebx, ebx
	dec	ebx ; i = -1
cat_0:
	inc	ebx ; ++i
	cmp	byte [yname+ebx], 0
	ja	short cat_0
	mov	edx, ebx ; i
	mov	ecx, [off]
	inc	ecx ; j = [off]+1
	add	edx, ecx  ; edx = [off]+i+1
	cmp	edx, 511-2 ; (+ CRLF + 0) 
	ja	short prname  ; 14/05/2022
	;;ja	short ckroot_2 ; jmp prname
	;ja	short ckroot_err
	mov	esi, name
	add	esi, ecx ; name[j]
	mov	edi, esi
	add	edi, ebx ; name[j+i]
	inc	edi ; name[j+i+1]	
cat_1:
	;std
	;rep	stosb
	;cld
	dec	ecx
	js	short cat_2
	dec	esi
	mov	al, [esi]
	dec	edi
	mov	[edi], al
	jmp	short cat_1
cat_2:
	mov	[off], edx ; [off] = i+[off]+1
	mov	byte [name+ebx], '/' ; name[i] = root[0]
cat_3:
	dec	ebx ; --i
	js	short cat_4 ; 0 -> -1
		 ; name[i] = yname[i]
	mov	al, [yname+ebx]
	mov	[name+ebx], al
	jmp	short cat_3 ; i >= 0
cat_4:
	retn

;-----------------------------------------------------------------

print_msg:
	; 08/05/2022
	; eax = asciiz string address
	mov	edx, eax
	dec	edx
nextchr:
	inc	edx
	cmp	byte [edx], 0
	ja	short nextchr
	;cmp	[edx], 0Dh
	;ja	short nextchr
	sub	edx, eax
	; edx = asciiz string length
	;
	sys	_write, 1, eax
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

; 14/05/2022
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

; 05/05/2022

name:	resb 512

file:	resd 1
off:	resd 1

; 14/05/2022
idev:
d.dev:	resw 1 ; device number (0 = root, 1 = mounted)
;dd.dev: resw 1

_x:	; stat(us) buffer
_d:	; 14/05/2022
inum:
d.ino:	; 14/05/2022
	resw 1
ximode:
d.mode: ; 14/05/22022
	;resb 64 ; Retro UNIX 386 v1.2
	resb 32 ; Retro UNIX 386 v1 & v1.1

; 14/05/2022
_dd:	; stat(us) buffer
dd.ino:	resw 1
dd.mode:
	resb 32

_y:	; directory entry buffer
jnum:	resw 1
yname:	resb 14 ; Retro UNIX 386 v1.1 & v1.2
	;resb 8 ; Retro UNIX 386 v1 (unix v1)
yzero:	resw 1

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