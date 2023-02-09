; ****************************************************************************
; glob386.s (glob2.s) - by Erdogan Tan - 28/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - global command (/etc/glob)
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
; ((nasm glob2.s -l glob2.txt -o glob2 -Z error.txt))

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
DIRSIZ equ 16  ; Retro UNIX 386 v1.1 & v1.2
;DIRSIZ equ 10 ; Retro UNIX 8086 v1 & 386 v1 	

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 28/05/2022
;-----------------------------------------------------------------

main:
	; main(argc, argv)
	
	pop	eax ; number of arguments

	;mov	[argc], eax
	mov	[argc], al

	;if (argc < 3) {
	;	write(2, "Arg count\n", 10);
	;	return;
	;}

	;cmp	eax, 3
	cmp	al, 3
	jnb	short glb_0
	
	mov	eax, msg_arg_count
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
	pop	edx ; argv[0]
	mov	ebx, ava
	mov	[ebx], edx
	add	ebx, 4
	pop	edx ; argv[1]
	mov	[ebx], edx
	add	ebx, 4
	mov	[av], ebx

	;while (--argc >= 2)
	;	expand(*++argv);
glb_1:
	dec	byte [argc]
	cmp	byte [argc], 2
	jb	short glb_2

	pop	ebp  ; argument pointer

	call	expand
	jmp	short glb_1

glb_2:	
	;if (ncoll==0) {
	;	write(2, "No match\n", 9);
	;	return;
	;}

	cmp	byte [ncoll], 0
	ja	short glb_3

	mov	eax, msg_no_match
	jmp	short glb_print_exit

glb_3:
	;execute(ava[1], &ava[1]);
	;cp = cat("/usr/bin/", ava[1]);
	;execute(cp+4, &ava[1]);
	;execute(cp, &ava[1]);

	sys	_exec, [ava+4], ava+4
	mov	ebp, usr_bin
	;mov	ebx, [ava+4]
	call	cat
	; cp = binary file name pointer
	; edx = cp
	mov	edi, edx
	add	edi, 4
	;execute(cp+4, &ava[1])
	sys	_exec, edi, ava+4 ; '/bin...'
	;execute(cp, &ava[1]);
	sys	_exec, edx, ava+4 ; '/usr/bin...'

	mov	eax, msg_not_found
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
	;   ebp = argument pointer 

	; Modified regs: eax, ebx, ecx, edx, esi, edi, ebp
	
	mov	esi, ebp
	xor	edx, edx ; edx = 0 

expd_0:	
	;while (*cs!='*' && *cs!='?' && *cs!='[') {

	mov	al, [esi]
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

	inc	esi
	or	al, al
	jnz	short expd_0

	sub	ebx, ebx ; 0

	; ebp = pointer to (current) argument
	; ebx = 0

cat_get_av:
	call	cat
	; edx = concatenated string address (ab[])

	mov	ebx, [av]  ; offset address of ava[] pointer
	mov	[ebx], edx ; [ebx] = ava[] pointer's itself
	add	ebx, 4
	mov	[av], ebx  ; next ava[] address

	retn

expd_1:	
	sub	edi, edi ; 0
expd_2:
	;for (;;) {

	;if (cs==s) {
	;	dirf = open(".", 0);
	;	s = "";
	;	break;

	cmp	esi, ebp
	;jne	short expd_4
	ja	short expd_4

	;sub	edi, edi ; 0
	sys	_open, DOT, edi ; open '.' for read
	jc	short expd_3	
	mov	edi, eax ; file descriptor (>=0)
	inc	edi ; file descriptor + 1
expd_3:
	; edx = 1
	sub	ebp, ebp ; 0  ; s = ""
	jmp	short expd_break
expd_4:
	;if (*--cs == '/') {
	;	*cs = 0;
	;	dirf = open(s==cs? "/": s, 0);
	;	*cs++ = 0200;
	;	break;
	dec	esi
	cmp	byte [esi], '/'
	jne	short expd_2
	mov	byte [esi], 0
	mov	edx, ebp
	cmp	esi, ebp
	jne	short expd_5
	;ja	short expd_5
	mov	edx, ROOTDIR
expd_5:	
	;dirf = open(s==cs? "/": s, 0);
	;sub	edi, edi ; 0
	sys	_open, edx, edi ; ; open for read
	jc	short expd_6
	mov	edi, eax ; file descriptor (>=0)
	inc	edi ; file descriptor + 1
expd_6:
	mov	byte [esi], 80h ; *cs++ = 0200;
	inc	esi
expd_7:
expd_break:
	;if (dirf<0) {
	;	write(2, "No directory\n", 13);
	;	exit();
	;}
	dec	edi ; 1 -> 0 or 0 -> -1
	jns	short expd_9

	mov	eax, msg_no_dir
expd_8:
	call	print_msg
_exit_:
	sys	_exit
;hang:
	;nop
	;jmp	short hang

expd_9:
	;oav = av;
	push	dword [av]
expd_10:
	;while (read(dirf, &entry, 16) == 16) {

	sys	_read, edi, entry, DIRSIZ
	jc	short expd_11
	;cmp	eax, 16
	;jne	short expd_11
	or	eax, eax  ; read count
	jz	short expd_11
	
	;if (entry.ino==0)
	;	continue;
	cmp	word [entry.ino], 0
	jna	short expd_10 ; continue
	
	;if (match(entry.name, cs)
	mov	ebx, entry.name
	; esi = cs 
	call	match
	jnz	short expd_10

	; ebp = pointer to (current) argument
	; ebx = entry.name

	;; *av++ = cat(s, entry.name);
	;call	cat
	;; edx = concatenated string address (ab[])
	
	;mov	ebx, [av]
	;mov	[ebx], edx
	;add	ebx, 4
	;mov	[av], ebx

	; *av++ = cat(s, entry.name);
	call	cat_get_av

	; ncoll++;
	inc	byte [ncoll]

	; edi = file descriptor = dirf
	; esi = cs
	; ebp = s

	jmp	short expd_10

expd_11:
	;close(dirf);
	sys	_close, edi
	;pop	ebx ; oav
	pop	esi ; oav	
	;sort(oav);
	;call	sort
	;retn
	;;jmp	sort

;-----------------------------------------------------------------

sort:	;sort(oav)

	; INPUT:
	;    esi = first ptr to
	;	   file/dir names to be sorted

	;p1 = oav;
	;mov	esi, ebx
	mov	ebx, [av]
	sub	ebx, 4
	mov	ebp, [av]  ; ebp = [av]
srt_1:
	; ebx = [av] - 4	
	;while (p1 < av-1) {
	cmp	esi, ebx ; esi = p1
	jnb	short srt_4
	mov	edi, esi ; p2 = p1;
srt_2:
	;while(++p2 < av) {
	add	edi, 4
	cmp	edi, ebp ; edi = p2
	jnb	short srt_3
	
	;if (compar(*p1, *p2) > 0)
	call	compar
	jna	short srt_2

	;c = *p1;
	;*p1 = *p2;
	;*p2 = c;
	mov	eax, [edi]
	xchg	[esi], eax
	mov	[edi], eax

	jmp	short srt_2	

srt_3:
	;p1++;
	add	esi, 4
	jmp	short srt_1
srt_4:
	retn

;-----------------------------------------------------------------

compar: ;compar(as1, as2)
	; INPUT:
	;	esi = p1 ; asciiz string (compared)
	;	edi = p2 ; asciiz string (with this)
	;
	; OUTPUT:
	;	*p1 - *p2 (first non-equal chars)
	;	0 = equal strings (with p2 length)

	; Modified registers: eax, ecx, edx
	;

	;s1 = as1;
	;s2 = as2;
	mov	eax, [esi] ; *p1
	mov	edx, [edi] ; *p2
comp_1:
	;while (*s1++ == *s2)
	;	if (*s2++ == 0)
	mov	cl, [eax] ; *s1++
	inc	eax
	mov	ch, [edx] ; *s2
	inc	edx
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
	;	ebx = directory entry
	;	esi = user input
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: eax, ecx, edx

	mov	al, [ebx] ; *s
	cmp	al, '.'
	jne	short mtch_1
	cmp	al, [esi] ; *p
	;je	short mtch_1
	;retn 	; zf = 0
	jne	short mtch_0 ; retn  ; zf = 0
mtch_1:
	push	esi ; *
	push	edi ; **
	push	ebx ; ***
	push	ebp ; ****

	call	amatch

	pop	ebp ; ****
	pop	ebx ; ***
	pop	edi ; **
	pop	esi ; *
	
	retn

;-----------------------------------------------------------------

amatch:	;amatch(as, ap)

	;char *as, *ap;
	;
	;register char *s, *p;
	;register scc;
	;int c, cc, ok, lc;

	; INPUT:
	;	ebx = directory entry
	;	esi = user input
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: eax, ecx, edx, ebx, esi, edi, ebp

	xor	eax, eax

	;s = as;
	;p = ap;

amch_next:
	;if (scc = *s++)
	;	if ((scc =& 0177) == 0)
	;		scc = 0200;
	
	; esi = p	
	; ebx = s

	mov	al, [ebx] ; *s++
	inc	ebx
	;mov	ebp, eax  ; scc
	and	al, al
	jz	short amch_0
	and	al, 7Fh ; 127
	jnz	short amch_0
	or	al, 80h ;  scc = 80h
amch_0:
	mov	ebp, eax ; scc

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

	xor	ecx, ecx ; ok = 0
	mov	edx, 7FFFh ; lc = 077777
amch_2:
	;while (cc = *p++) {
	lodsb	; cc = *p++
	and	al, al
	jz	short amch_x
	cmp	al, ']' ; if (cc=']')
	jne	short amch_3
	and	ecx, ecx ; 0 ; if (ok)
	jz	short amch_xx
	
	;return(amatch(s, p));
	; ebx = s
	; esi = p
	call	amatch
	dec	al ; 1 -> 0, 0 -> 0FFh
	jmp	short amch_x
amch_3:
	cmp	al, '-' ; else if (cc='-')
	jne	short amch_5
	cmp	edx, ebp ; if (lc<=scc
	ja	short amch_2 ; while
			; && scc<=(c = *p++))
	lodsb
	cmp	ebp, eax
	ja	short amch_2 ; while
amch_4:
	inc	ecx ; ok++;
	jmp	short amch_2 ; while
amch_5: 
	mov	edx, eax ; (lc=cc)
	cmp	ebp, eax ; if (scc == (lc=cc))
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

	or	ebp, ebp
	jz	short amch_xx ; not match

	;return(amatch(s, p));
	; ebx = s
	; esi = p
	call	amatch
	dec	al ; 1 -> 0, 0 -> 0FFh
	jmp	short amch_x
amch_20:
	cmp	al, '*'
	jne	short amch_30
amch_21:
	;case '*':
	;	return(umatch(--s, p));

	dec	ebx ; --s
	; ebx = s
	; esi = p
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
	or	ebp, ebp
	jnz	short amch_x ; not match
	dec	al ; 0FFh
	jmp	short amch_x ; match
amch_40:
	;default:
	;	if (c!=scc)
	;	   return(0);

	cmp	eax, ebp	
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
	;	ebx = s
	;	esi = p
	; OUTPUT:
	;	zf = 1 ->
	;	     dir entry matches with input
	;	zf = 0 ->
	;	     dir entry does not match with input
	;
	; Modified registers: eax, ebx, ecx, edx

	;if(*p==0)
	;	return(1);
	;while(*s)
	;	if (amatch(s++,p))
	;		return(1);
	;return(0);

	mov	al, [esi]
	or	al, al ; if (*p==0)
	jz	short umch_x ; return(1) ; match
umch_0:
	mov	al, [ebx] ; while(*s)	
	and	al, al
	jnz	short umch_2
	inc	al ; 1 ; not match
umch_1:
	retn
umch_2:
	; ebx = s
	; esi = p
	push	esi
	push	ebx
	call	amatch
	pop	ebx
	pop	esi
	jz	short umch_1 ; al = 0 ; match
	
	; al = 1 ; not match
	inc	ebx ; s++
	jmp	short umch_0

;-----------------------------------------------------------------
	
cat:	;cat(as1, as2)
	
	;char *as1, *as2;

	;register char *s1, *s2;
	;register int c;

	; INPUT:
	;    ebp = base name (path)
	;    ebx = name to be added
	; OUTPUT:
	;    edx = concatenated name (path)
	;
	; Modified regs: eax, ecx, edx	

	;s2 = string;
	;s1 = as1;
	
	; ebp = as1
	; ebx = as2
	
	push	esi ; *
	push	edi ; **

	mov	edi, [string] ; s2 = string 	 
	or	ebp, ebp ; 0 ?
	jz	short cat_3
	mov	esi, ebp ; s1 = as1
cat_0:
	;while (c = *s1++) {
	
	lodsb
	or	al, al
	jz	short cat_2

	;if (s2 > &ab[STRSIZ])
	;		toolong();

	cmp	edi, ab+STRSIZ
	jnb	short cat_too_long
	
	and	al, 7Fh ; c =& 0177;
	jnz	short cat_1

	;if (c==0) {
	;	*s2++ = '/';
	;	break;

	;or	ebx, ebx ; ebx = 0 ?
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
	and	ebx, ebx ; ebx = 0 ?
	jnz	short cat_3
	cmp	edi, ab+STRSIZ
	jb	short cat_5
	jmp	short cat_too_long
cat_3:
	;s1 = as2;
	mov	esi, ebx	
cat_4:
	;do {
	;	if (s2 > &ab[STRSIZ])
	;		toolong();
	;	*s2++ = c = *s1++;
	;} while (c);

	cmp	edi, ab+STRSIZ
	jnb	short cat_too_long
	
	lodsb	; c = *s1++;
cat_5:
	stosb	; *s2++ = c
	and	al, al  ; while (c);	
	jnz	short cat_4

	;s1 = string;
	;string = s2;
	
	mov	edx, [string]
	mov	[string], edi

	;return(s1);

	pop	edi ; **
	pop	esi ; *
	retn

;-----------------------------------------------------------------

cat_too_long:
	pop	edi ; **
	pop	esi ; *
too_long:
	mov	eax, msg_too_long
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
	; eax = asciiz string address
	mov	esi, eax
	dec	esi
nextchr:
	inc	esi
	cmp	byte [esi], 0
	ja	short nextchr
	;cmp	[esi], 0Dh
	;ja	short nextchr
	sub	esi, eax
	; esi = asciiz string length
	;
	sys	_write, 1, eax, esi
	;
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

program_msg:
	db  0
	db  'Retro UNIX 386 v1.1 /etc/glob by Erdogan TAN'
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

av:	dd ava	 ;char **av &ava[1];
string:	dd ab	 ;char *string ab;

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
	resb 14
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