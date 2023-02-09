; ****************************************************************************
; glob8086.s (glob0.s) - by Erdogan Tan - 28/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - global command (/etc/glob)
;
; [ Last Modification: 30/05/2022 ]
;
; Derived from (original) UNIX v5 'glob.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; ****************************************************************************
; [ v5root.tar.gz - usr/source/s1/glob.c (archive date: 27-11-1974) ]
;
; Assembler: NASM v2.15
; ((nasm glob1.s -l glob1.txt -o glob1 -Z error.txt))

; glob2.s - 30/05/2022 - Retro UNIX 386 v1.1 & v1.2 & v2
; glob1.s - 30/05/2022 - Retro UNIX 386 v1
; glob0.s - 30/05/2022 - Retro UNIX 8086 v1 (16 bit 'glob1.s')

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

;; UNIX v1 inode
;; byte 1
;S_ALLOC  equ 080h ; Allocated flag
;S_IFDIR  equ 040h ; Directory flag
;S_IFMDF  equ 020h ; File modified flag (always on)
;S_IFLRG  equ 010h ; Large File flag
;; byte 0
;S_ISUID  equ 020h ; Set User ID On Execution flag
;S_IEXEC  equ 010h ; Executable File flag
;S_IREAD  equ 008h ; Owner's Read Permission flag
;S_IWRITE equ 004h ; Owner's Write Permission flag

; 28/05/2022
STRSIZ equ 522
;DIRSIZ equ 16  ; Retro UNIX 386 v1.1 & v1.2
DIRSIZ equ 10 ; Retro UNIX 8086 v1 & 386 v1 	

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 16] ; 16-bit intructions (for 8086-80386 real mode)

[ORG 0] 

START_CODE:
	; 28/05/2022
;-----------------------------------------------------------------

main:
	; main(argc, argv)
	
	pop	ax ; number of arguments

	;mov	[argc], ax
	mov	[argc], al

	;if (argc < 3) {
	;	write(2, "Arg count\n", 10);
	;	return;
	;}

	;cmp	ax, 3
	cmp	al, 3
	jnb	short glb_0
	
	mov	ax, msg_arg_count
glb_print_exit:
	call	print_msg
glb_exit:
	sys	_exit	; sys exit
;hlt:
;	nop
;	nop
;	jmp	short hlt

glb_0:
	;argv++;
	;*av++ = *argv;
	pop	dx ; argv[0]
	mov	bx, ava
	mov	[bx], dx
	add	bx, 2
	pop	dx ; argv[1]
	mov	[bx], dx
	add	bx, 2
	mov	[av], bx

	;while (--argc >= 2)
	;	expand(*++argv);
glb_1:
	dec	byte [argc]
	cmp	byte [argc], 2
	jb	short glb_2

	pop	bp  ; argument pointer

	call	expand
	jmp	short glb_1

glb_2:	
	;if (ncoll==0) {
	;	write(2, "No match\n", 9);
	;	return;
	;}

	cmp	byte [ncoll], 0
	ja	short glb_3

	mov	ax, msg_no_match
	jmp	short glb_print_exit

glb_3:
	;execute(ava[1], &ava[1]);
	;cp = cat("/usr/bin/", ava[1]);
	;execute(cp+4, &ava[1]);
	;execute(cp, &ava[1]);

	sys	_exec, [ava+2], ava+2
	mov	bp, usr_bin
	;mov	bx, [ava+2]
	call	cat
	; cp = binary file name pointer
	; dx = cp
	mov	di, dx
	add	di, 4
	;execute(cp+4, &ava[1])
	sys	_exec, di, ava+2 ; '/bin...'
	;execute(cp, &ava[1]);
	sys	_exec, dx, ava+2 ; '/usr/bin...'

	mov	ax, msg_not_found
	jmp	glb_print_exit

;-----------------------------------------------------------------

expand: ;expand(as)

	;register char *s, *cs;
	;register int dirf;
	;char **oav;

	;static struct {
	;	int  ino;
	;	char name[16];
	;} entry;	

	;s = cs = as	

	; INPUT:
	;   bp = argument pointer 

	; Modified regs: ax, bx, cx, dx, si, di, bp
	
	mov	si, bp
	xor	dx, dx ; dx = 0 

expd_0:	
	;while (*cs!='*' && *cs!='?' && *cs!='[') {

	mov	al, [si]
	cmp	al, '*'
	je	short expd_1
	cmp	al, '?'
	je	short expd_1
	cmp	al, '['
	je	short expd_1

	;if (*cs++ == 0) {
	;	*av++ = cat(s, "");
	;	return;
	;}

	inc	si
	or	al, al
	jnz	short expd_0

	sub	bx, bx ; 0

	; bp = pointer to (current) argument
	; bx = 0

cat_get_av:
	call	cat
	; dx = concatenated string address (ab[])

	mov	bx, [av]  ; offset address of ava[] pointer
	mov	[bx], dx ; [bx] = ava[] pointer's itself
	add	bx, 2
	mov	[av], bx  ; next ava[] address

	retn

expd_1:	
	sub	di, di ; 0
expd_2:
	;for (;;) {

	;if (cs==s) {
	;	dirf = open(".", 0);
	;	s = "";
	;	break;

	cmp	si, bp
	;jne	short expd_4
	ja	short expd_4

	;sub	di, di ; 0
	sys	_open, DOT, di ; open '.' for read
	jc	short expd_3	
	mov	di, ax ; file descriptor (>=0)
	inc	di ; file descriptor + 1
expd_3:
	; dx = 1
	sub	bp, bp ; 0  ; s = ""
	jmp	short expd_break
expd_4:
	;if (*--cs == '/') {
	;	*cs = 0;
	;	dirf = open(s==cs? "/": s, 0);
	;	*cs++ = 0200;
	;	break;
	dec	si
	cmp	byte [si], '/'
	jne	short expd_2
	mov	byte [si], 0
	mov	dx, bp
	cmp	si, bp
	jne	short expd_5
	;ja	short expd_5
	mov	dx, ROOTDIR
expd_5:	
	;dirf = open(s==cs? "/": s, 0);
	;sub	di, di ; 0
	sys	_open, dx, di ; ; open for read
	jc	short expd_6
	mov	di, ax ; file descriptor (>=0)
	inc	di ; file descriptor + 1
expd_6:
	mov	byte [si], 80h ; *cs++ = 0200;
	inc	si
expd_7:
expd_break:
	;if (dirf<0) {
	;	write(2, "No directory\n", 13);
	;	exit();
	;}
	dec	di ; 1 -> 0 or 0 -> -1
	jns	short expd_9

	mov	ax, msg_no_dir
expd_8:
	call	print_msg
_exit_:
	sys	_exit
;hang:
	;nop
	;jmp	short hang

expd_9:
	;oav = av;
	push	word [av]
expd_10:
	;while (read(dirf, &entry, 16) == 16) {

	sys	_read, di, entry, DIRSIZ
	jc	short expd_11
	;cmp	ax, 10
	;jne	short expd_11
	or	ax, ax  ; read count
	jz	short expd_11
	
	;if (entry.ino==0)
	;	continue;
	cmp	word [entry.ino], 0
	jna	short expd_10 ; continue
	
	;if (match(entry.name, cs)
	mov	bx, entry.name
	; si = cs 
	call	match
	jnz	short expd_10

	; bp = pointer to (current) argument
	; bx = entry.name

	;; *av++ = cat(s, entry.name);
	;call	cat
	;; dx = concatenated string address (ab[])
	
	;mov	bx, [av]
	;mov	[bx], dx
	;add	bx, 2
	;mov	[av], bx

	; *av++ = cat(s, entry.name);
	call	cat_get_av

	; ncoll++;
	inc	byte [ncoll]

	; di = file descriptor = dirf
	; si = cs
	; bp = s

	jmp	short expd_10

expd_11:
	;close(dirf);
	sys	_close, di
	pop	si ; oav	
	;sort(oav);
	;call	sort
	;retn
	;;jmp	sort

;-----------------------------------------------------------------

sort:	;sort(oav)

	; INPUT:
	;    si = first ptr to
	;	   file/dir names to be sorted

	;p1 = oav;
	mov	ax, [av]
	sub	ax, 2
	mov	dx, [av]  ; dx = [av]
srt_1:
	; ax = [av] - 2	
	;while (p1 < av-1) {
	cmp	si, ax ; si = p1
	jnb	short srt_4
	mov	di, si ; p2 = p1;
srt_2:
	;while(++p2 < av) {
	add	di, 2
	cmp	di, dx ; di = p2
	jnb	short srt_3
	
	;if (compar(*p1, *p2) > 0)
	call	compar
	jna	short srt_2

	;c = *p1;
	;*p1 = *p2;
	;*p2 = c;
	mov	cx, [di]
	xchg	[si], cx
	mov	[di], cx

	jmp	short srt_2	

srt_3:
	;p1++;
	add	si, 2
	jmp	short srt_1
srt_4:
	retn

;-----------------------------------------------------------------

compar: ;compar(as1, as2)
	; INPUT:
	;	si = p1 ; asciiz string (compared)
	;	di = p2 ; asciiz string (with this)
	;
	; OUTPUT:
	;	*p1 - *p2 (first non-equal chars)
	;	0 = equal strings (with p2 length)

	; Modified registers: cx, bx, bp
	;

	;s1 = as1;
	;s2 = as2;

	mov	bx, [si] ; *p1
	mov	bp, [di] ; *p2
comp_1:
	;while (*s1++ == *s2)
	;	if (*s2++ == 0)
	mov	cl, [bx] ; *s1++
	inc	bx
	mov	ch, [bp] ; *s2
	inc	bp
	;return (*--s1 - *s2);
	sub	cl, ch
	jz	short comp_2
	retn
comp_2:
	;if (*s2++ == 0)
	;  return(0);
	or	ch, ch
	jnz	short comp_1
	; zf = 1
mtch_0:
	retn

;-----------------------------------------------------------------	

match:	; match(s, p)

	; char *s, *p;
	; if (*s=='.' && *p!='.')
	;	return(0);
	; return(amatch(s, p));

	; INPUT:
	;	bx = directory entry
	;	si = user input
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: ax, cx, dx

	mov	al, [bx] ; *s
	cmp	al, '.'
	jne	short mtch_1
	cmp	al, [si] ; *p
	;je	short mtch_1
	;retn 	; zf = 0
	jne	short mtch_0 ; retn  ; zf = 0
mtch_1:
	push	si ; *
	push	di ; **
	push	bx ; ***
	push	bp ; ****

	call	amatch

	pop	bp ; ****
	pop	bx ; ***
	pop	di ; **
	pop	si ; *
	
	retn

;-----------------------------------------------------------------

amatch:	;amatch(as, ap)

	;char *as, *ap;
	;
	;register char *s, *p;
	;register scc;
	;int c, cc, ok, lc;

	; INPUT:
	;	bx = directory entry
	;	si = user input
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: ax, cx, dx, bx, si, di, bp

	xor	ax, ax

	;s = as;
	;p = ap;

amch_next:
	;if (scc = *s++)
	;	if ((scc =& 0177) == 0)
	;		scc = 0200;
	
	; si = p	
	; bx = s

	mov	al, [bx] ; *s++
	inc	bx
	;mov	bp, ax  ; scc
	and	al, al
	jz	short amch_0
	and	al, 7Fh ; 127
	jnz	short amch_0
	or	al, 80h ;  scc = 80h
amch_0:
	mov	bp, ax ; scc

switch:	;switch (c = *p++) {

	lodsb
	cmp	al, '['
	jne	short amch_10
amch_1: 
	;case '[':
	;ok = 0;
	;lc = 077777;
	;while (cc = *p++) {
	;	if (cc==']') {
	;	   if (ok)
	;	      return(amatch(s, p));
	;	   else
	;	      return(0);
	;	} else if (cc=='-') {
	;	    if (lc<=scc && scc<=(c = *p++))
	;		ok++;
	;	    } else
	;		if (scc == (lc=cc))
	;	     	   ok++;
	;	}
	;	return(0);

	xor	cx, cx ; ok = 0
	mov	dx, 7FFFh ; lc = 077777
amch_2:
	;while (cc = *p++) {
	lodsb	; cc = *p++
	and	al, al
	jz	short amch_x
	cmp	al, ']' ; if (cc=']')
	jne	short amch_3
	and	cx, cx ; 0 ; if (ok)
	jz	short amch_xx
	
	;return(amatch(s, p));
	; bx = s
	; si = p
	call	amatch
	dec	al ; 1 -> 0, 0 -> 0FFh
	jmp	short amch_x
amch_3:
	cmp	al, '-' ; else if (cc='-')
	jne	short amch_5
	cmp	dx, bp ; if (lc<=scc
	ja	short amch_2 ; while
			; && scc<=(c = *p++))
	lodsb
	cmp	bp, ax
	ja	short amch_2 ; while
amch_4:
	inc	cx ; ok++;
	jmp	short amch_2 ; while
amch_5: 
	mov	dx, ax ; (lc=cc)
	cmp	bp, ax ; if (scc == (lc=cc))
  	jne	short amch_2 ; while
	jmp	short amch_4 ; ok++; ; while 
amch_10:
	cmp	al, '?'
	jne	short amch_20
amch_11: 
	;case '?':
	;	if (scc)
	;	   return(amatch(s, p));
	;	return(0);

	or	bp, bp
	jz	short amch_xx ; not match

	;return(amatch(s, p));
	; bx = s
	; si = p
	call	amatch
	dec	al ; 1 -> 0, 0 -> 0FFh
	jmp	short amch_x
amch_20:
	cmp	al, '*'
	jne	short amch_30
amch_21:
	;case '*':
	;	return(umatch(--s, p));

	dec	bx ; --s
	; bx = s
	; si = p
	call	umatch
	dec	al ; 1 -> 0, 0 -> 0FFh
	jmp	short amch_x
amch_30:
	and	al, al
	jnz	short amch_40 ; default	
amch_31:
	;case '\0':
	;	return(!scc);

	; al = 0
	or	bp, bp
	jnz	short amch_x ; not match
	dec	al ; 0FFh
	jmp	short amch_x ; match
amch_40:
	;default:
	;	if (c!=scc)
	;	   return(0);

	cmp	ax, bp	
	jne	short amch_xx ; not match
	;mov	al, 0FFh
	;jmp	short amch_x ; match
	jmp	amch_next

amch_xx:
	xor	al, al ; 0
amch_x:
	; al = 0 -> not match
	; al = 0FFh -> match
	
	inc	al ; 0 -> 1, 0FFh -> 0

	; al = 0 -> zf = 1
	; al > 0 -> zf = 0
umch_x:
	retn

;-----------------------------------------------------------------	

umatch: ; umatch(s, p)
	; char *s, *p;

	; INPUT:
	;	bx = s
	;	si = p
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: ax, bx, cx, dx

	;if(*p==0)
	;	return(1);
	;while(*s)
	;	if (amatch(s++,p))
	;		return(1);
	;return(0);

	mov	al, [si]
	or	al, al ; if (*p==0)
	jz	short umch_x ; return(1) ; match
umch_0:
	mov	al, [bx] ; while(*s)	
	and	al, al
	jnz	short umch_2
	inc	al ; 1 ; not match
umch_1:
	retn
umch_2:
	; bx = s
	; si = p
	push	si
	push	bx
	call	amatch
	pop	bx
	pop	si
	jz	short umch_1 ; al = 0 ; match
	
	; al = 1 ; not match
	inc	bx ; s++
	jmp	short umch_0

;-----------------------------------------------------------------
	
cat:	;cat(as1, as2)
	
	;char *as1, *as2;

	;register char *s1, *s2;
	;register int c;

	; INPUT:
	;    bp = base name (path)
	;    bx = name to be added
	; OUTPUT:
	;    dx = concatenated name (path)
	;
	; Modified regs: ax, cx, dx	

	;s2 = string;
	;s1 = as1;
	
	; bp = as1
	; bx = as2
	
	push	si ; *
	push	di ; **

	mov	di, [string] ; s2 = string 	 
	or	bp, bp ; 0 ?
	jz	short cat_3
	mov	si, bp ; s1 = as1
cat_0:
	;while (c = *s1++) {
	
	lodsb
	or	al, al
	jz	short cat_2

	;if (s2 > &ab[STRSIZ])
	;		toolong();

	cmp	di, ab+STRSIZ
	jnb	short cat_too_long
	
	and	al, 7Fh ; c =& 0177;
	jnz	short cat_1

	;if (c==0) {
	;	*s2++ = '/';
	;	break;

	;or	bx, bx ; bx = 0 ?
	;jz	short cat_5 ; al = 0
	
	mov	al, '/'
	stosb
	;jmp	short cat_3
	jmp	short cat_2

cat_1:
	; *s2++ = c;
	stosb
	jmp	short cat_0 ; while
cat_2:
	; al = 0
	and	bx, bx ; bx = 0 ?
	jnz	short cat_3
	cmp	di, ab+STRSIZ
	jb	short cat_5
	jmp	short cat_too_long
cat_3:
	;s1 = as2;
	mov	si, bx	
cat_4:
	;do {
	;	if (s2 > &ab[STRSIZ])
	;		toolong();
	;	*s2++ = c = *s1++;
	;} while (c);

	cmp	di, ab+STRSIZ
	jnb	short cat_too_long
	
	lodsb	; c = *s1++;
cat_5:
	stosb	; *s2++ = c
	and	al, al  ; while (c);	
	jnz	short cat_4

	;s1 = string;
	;string = s2;
	
	mov	dx, [string]
	mov	[string], di

	;return(s1);

	pop	di ; **
	pop	si ; *
	retn

;-----------------------------------------------------------------

cat_too_long:
	pop	di ; **
	pop	si ; *
too_long:
	mov	ax, msg_too_long
	jmp	glb_print_exit

;	call	print_msg
;
;	sys	_exit	; sys exit
;hangemhigh:
;	nop
;	nop
;	jmp	short hangemhigh

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

program_msg:
	db  0
	db  'Retro UNIX 8086 v1 /etc/glob by Erdogan TAN'
	db  ' - 30/05/2022'
	db  0

ROOTDIR:
	db '/', 0

DOT:	db '.', 0
	;db 0, 0

usr_bin:	
	db '/usr/bin/', 0

msg_arg_count:
	db 0Dh, 0Ah
	db "Arg count"
nextline:
	db  0Dh, 0Ah, 0

msg_no_match:
	db 0Dh, 0Ah
	db "No match"
	db  0Dh, 0Ah, 0

msg_not_found:
	db 0Dh, 0Ah
	db "Command not found."
	db  0Dh, 0Ah, 0

msg_no_dir:
	db 0Dh, 0Ah
	db "No directory"
	db  0Dh, 0Ah, 0

msg_too_long:
	db 0Dh, 0Ah
	db 'Arg list too long'
	db  0Dh, 0Ah, 0

av:	dw ava	 ;char **av &ava[1];
string:	dw ab	 ;char *string ab;

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 4

bss_start:

ABSOLUTE bss_start

;argc:	resd 1
argc:	resb 1
errno:	resb 1	;int errno;
ncoll:	resw 1	;int ncoll;
entry:
entry.ino:
	resw 1
entry.name:
	;resb 14
	resb 10
	resb 2

ava:	resb 200
ab:	resb STRSIZ ; 522

; 28/05/2022
;-----------------------------------------------------------------
; Original UNIX v5 - /etc/glob (utility) c source code (glob.c)
;-----------------------------------------------------------------
;/* UNIX V5 source code: see www.tuhs.org for details. */;
; * v5root.tar.gz (/usr/source/s1) *
;
;#
;/* global command --
;
;  glob params
;
;  "*" in params matches r.e ".*"
;  "?" in params matches r.e. "."
;  "[...]" in params matches character class
;  "[...a-z...]" in params matches a through z.
;
;  perform command with argument list
;  constructed as follows:
;    if param does not contain "*", "[", or "?", use it as is
;    if it does, find all files in current directory
;    which match the param, sort them, and use them
;
;  prepend the command name with "/bin" or "/usr/bin"
;  as required.
;*/
;
;#define E2BIG	 7
;#define ENOEXEC 8
;#define ENOENT	 2
;
;#define STRSIZ	522
;char	ab[STRSIZ];		/* generated characters */
;char	*ava[200];		/* generated arguments */
;char	**av &ava[1];
;char	*string ab;
;int	errno;
;int	ncoll;
;
;main(argc, argv)
;char *argv[];
;{
;	register char *cp;
;
;	if (argc < 3) {
;		write(2, "Arg count\n", 10);
;		return;
;	}
;	argv++;
;	*av++ = *argv;
;	while (--argc >= 2)
;		expand(*++argv);
;	if (ncoll==0) {
;		write(2, "No match\n", 9);
;		return;
;	}
;	execute(ava[1], &ava[1]);
;	cp = cat("/usr/bin/", ava[1]);
;	execute(cp+4, &ava[1]);
;	execute(cp, &ava[1]);
;	write(2, "Command not found.\n", 19);
;}
;
;expand(as)
;char *as;
;{
;	register char *s, *cs;
;	register int dirf;
;	char **oav;
;	static struct {
;		int	ino;
;		char	name[16];
;	} entry;
;
;	s = cs = as;
;	while (*cs!='*' && *cs!='?' && *cs!='[') {
;		if (*cs++ == 0) {
;			*av++ = cat(s, "");
;			return;
;		}
;	}
;	for (;;) {
;		if (cs==s) {
;			dirf = open(".", 0);
;			s = "";
;			break;
;		}
;		if (*--cs == '/') {
;			*cs = 0;
;			dirf = open(s==cs? "/": s, 0);
;			*cs++ = 0200;
;			break;
;		}
;	}
;	if (dirf<0) {
;		write(2, "No directory\n", 13);
;		exit();
;	}
;	oav = av;
;	while (read(dirf, &entry, 16) == 16) {
;		if (entry.ino==0)
;			continue;
;		if (match(entry.name, cs)) {
;			*av++ = cat(s, entry.name);
;			ncoll++;
;		}
;	}
;	close(dirf);
;	sort(oav);
;}
;
;sort(oav)
;char **oav;
;{
;	register char **p1, **p2, **c;
;
;	p1 = oav;
;	while (p1 < av-1) {
;		p2 = p1;
;		while(++p2 < av) {
;			if (compar(*p1, *p2) > 0) {
;				c = *p1;
;				*p1 = *p2;
;				*p2 = c;
;			}
;		}
;		p1++;
;	}
;}
;
;execute(afile, aarg)
;char *afile;
;char **aarg;
;{
;	register char *file, **arg;
;
;	arg = aarg;
;	file = afile;
;	execv(file, arg);
;	if (errno==ENOEXEC) {
;		arg[0] = file;
;		*--arg = "/bin/sh";
;		execv(*arg, arg);
;	}
;	if (errno==E2BIG)
;		toolong();
;}
;
;toolong()
;{
;	write(2, "Arg list too long\n", 18);
;	exit();
;}
;
;match(s, p)
;char *s, *p;
;{
;	if (*s=='.' && *p!='.')
;		return(0);
;	return(amatch(s, p));
;}
;
;amatch(as, ap)
;char *as, *ap;
;{
;	register char *s, *p;
;	register scc;
;	int c, cc, ok, lc;
;
;	s = as;
;	p = ap;
;	if (scc = *s++)
;		if ((scc =& 0177) == 0)
;			scc = 0200;
;	switch (c = *p++) {
;
;	case '[':
;		ok = 0;
;		lc = 077777;
;		while (cc = *p++) {
;			if (cc==']') {
;				if (ok)
;					return(amatch(s, p));
;				else
;					return(0);
;			} else if (cc=='-') {
;				if (lc<=scc && scc<=(c = *p++))
;					ok++;
;			} else
;				if (scc == (lc=cc))
;					ok++;
;		}
;		return(0);
;
;	default:
;		if (c!=scc)
;			return(0);
;
;	case '?':
;		if (scc)
;			return(amatch(s, p));
;		return(0);
;
;	case '*':
;		return(umatch(--s, p));
;
;	case '\0':
;		return(!scc);
;	}
;}
;
;umatch(s, p)
;char *s, *p;
;{
;	if(*p==0)
;		return(1);
;	while(*s)
;		if (amatch(s++,p))
;			return(1);
;	return(0);
;}
;
;compar(as1, as2)
;char *as1, *as2;
;{
;	register char *s1, *s2;
;
;	s1 = as1;
;	s2 = as2;
;	while (*s1++ ==  *s2)
;		if (*s2++ == 0)
;			return(0);
;	return (*--s1 - *s2);
;}
;
;cat(as1, as2)
;char *as1, *as2;
;{
;	register char *s1, *s2;
;	register int c;
;
;	s2 = string;
;	s1 = as1;
;	while (c = *s1++) {
;		if (s2 > &ab[STRSIZ])
;			toolong();
;		c =& 0177;
;		if (c==0) {
;			*s2++ = '/';
;			break;
;		}
;		*s2++ = c;
;	}
;	s1 = as2;
;	do {
;		if (s2 > &ab[STRSIZ])
;			toolong();
;		*s2++ = c = *s1++;
;	} while (c);
;	s1 = string;
;	string = s2;
;	return(s1);
;}

; 28/05/2022
;-----------------------------------------------------------------
; Original UNIX v2 - /etc/glob (utility) c source code (glob.c)
;-----------------------------------------------------------------
;/* UNIX V2 source code: see www.tuhs.org for details. */;
; * svntree-20081216.tar.gz (unix72) *
;
;/* global command --
;
;  glob params
;
;  "*" in params matches r.e ".*"
;  "?" in params matches r.e. "."
;  "[...]" in params matches character class
;  "[...a-z...]" in params matches a through z.
;
;  perform command with argument list
;  constructed as follows:
;    if param does not contain "*", "[", or "?", use it as is
;    if it does, find all files in current directory
;    which match the param, sort them, and use them
;
;  prepend the command name with "/bin" or "/usr/bin"
;  as required.
;*/
;
;char	ab[2000];		/* generated characters */
;char	*ava[200];		/* generated arguments */
;char	**av ava;
;char	*string ab;
;
;main(argc, argv)
;char *argv[];
;{
;	int i, j, c;
;	int inode, dirf, ap;
;	int fb[5], sb[17];
;	char *cp, *cpo;
;
;	if (argc < 3) {
;		write(1, "Arg count\n", 10);
;		return;
;	}
;	ap = 0;
;	av++;
;	fb[4] = 0;
;loop:
;	cpo = cp = *++argv;
;	while(c = *cp++) if (c=='*' | c=='?' | c=='[') goto compl;
;	av[ap++] = copy(cpo);
;	if (--argc>=2) goto loop;
;	goto donow;
;
;compl:
;	if(*--cp == '/') {
;		*cp = '\0';
;		if((dirf=open(cp==cpo? "/" : cpo, 0))<0)
;			goto oper;
;		*cp++ = '/';
;		goto compl1;
;	}
;	if(cp != cpo) goto compl;
;	if((dirf=open(".",0)) >= 0) goto compl1;
;oper:
;	write(1, "No directory\n", 13);
;	return;
;compl1:
;	j = ap;
;l2:
;	while (read(dirf, &inode, 2)>0) {
;		read(dirf, fb, 8);
;		if (inode==0) goto l2;
;		if (match(fb, cp)) {
;			c = *cp;
;			*cp = '\0';
;			av[ap++] = cat(cpo, fb);
;			*cp = c;
;		}
;	}
;	close(dirf);
;	i = j;
;	while(i<ap-1) {
;		j = i;
;		while(++j<ap) {
;			if (compar(av[i],av[j])) {
;				c = av[i];
;				av[i] = av[j];
;				av[j] = c;
;			}
;		}
;		i++;
;	}
;	if (--argc>=2) goto loop;
;donow:
;	if (ap<=1) {
;		write(1, "No match\n", 9);
;		return;
;	}
;	av[ap] = 0;
;	execv(av[0], av);
;	i = cat("/bin/", av[0]);
;	execv(i, av);
;	i = cat("/usr", i);
;	execv(i, av);
;	if (stat(i, sb) == 0) {
;		*av = i;
;		*--av = "/bin/sh";
;		execv(av[0], av);
;	}
;	write(1, "No command\n", 11);
;}
;
;match(s, p)
;char *s, *p; {
;	if (*s=='.' & *p!='.') return(0);
;	return(amatch(s, p));
;}
;
;amatch(s, p)
;char *s, *p;
;{
;	int c, cc, ok, lc, scc;
;
;	scc = *s;
;	lc = 077777;
;	switch (c = *p) {
;
;	case '[':
;		ok = 0;
;		while (cc = *++p) {
;			switch (cc) {
;
;			case ']':
;				if (ok)
;					return(amatch(++s, ++p));
;				else
;					return(0);
;
;			case '-':
;				ok =| lc <= scc & scc <= (cc=p[1]);
;			}
;			if (scc==(lc=cc)) ok++;
;		}
;		return(0);
;
;	case '?':
;	caseq:
;		if(scc) return(amatch(++s, ++p));
;		return(0);
;	case '*':
;		return(umatch(s, ++p));
;	case 0:
;		return(!scc);
;	}
;	if (c==scc) goto caseq;
;	return(0);
;}
;
;umatch(s, p)
;char *s, *p;
;{
;	if(*p==0) return(1);
;	while(*s)
;		if (amatch(s++,p)) return(1);
;	return(0);
;}
;
;compar(s1,s2)
;char *s1, *s2;
;{
;	int c1,c2;
;
;loop:
;	if ((c1 = *s1++)==0) return(0);
;	if ((c2 = *s2++)==0) return(1);
;	if (c1==c2) goto loop;
;	return(c1>c2);
;}
;
;copy(s1)
;char *s1;
;{
;	char *ts;
;
;	ts = string;
;	while(*string++ = *s1++);
;	return(ts);
;}
;
;cat(s1, s2)
;char *s1, *s2;
;{
;	char *ts;
;
;	ts = string;
;	while(*string++ = *s1++);
;	string--;
;	while(*string++ = *s2++);
;	return(ts);
;}