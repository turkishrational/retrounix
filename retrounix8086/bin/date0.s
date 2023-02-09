; ****************************************************************************
; date8086.s (date0.s) - by Erdogan Tan - 31/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - date - print and set the date
;
; [ Last Modification: 02/06/2022 ]
;
; Derived from (original) UNIX v5 'date.c' source Code
; Ref:
; www.tuhs.org (https://minnie.tuhs.org)
; ****************************************************************************
; [ v5root.tar.gz - usr/source/s1/date.c (archive date: 27-11-1974) ]
;		  - usr/source/s3/ctime.c (archive date: 27-11-1974)

; Assembler: NASM v2.15
; ((nasm date0.s -l date0.txt -o date0 -Z error.txt))

; date1.s - 02/06/2022 - Retro UNIX 386 v1 & v1.1 & V1.2
; date0.s - 02/06/2022 - Retro UNIX 8086 v1 (16 bit 'date1.s')

;; (unix) v5man.pdf (page 45)
;; --------------------------
;;
;;NAME
;;	date - print and set the date
;;SYNOPSIS
;;	date [ mmddhhmm[yy] ]
;;DESCRIPTION
;;	If no argument is given, the current date is printed to the second.
;;	If an argument is given, the current date is set.
;;	The first num is the month number; dd is the day number in the month;
;;	hh is the hour number (24 hour sys), the 2'nd mm is the minute number;
;;	yy is the last 2 digits of the year number and is optional. 
;;
;;	For example:
;;		date 10080045
;;
;;	sets the date to Oct 8, 12:45 AM. 
;;
;;	The current year is the default if no year is mentioned. 
;;

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

;-----------------------------------------------------------------
;  text - code
;-----------------------------------------------------------------

[BITS 16] ; 16-bit intructions (for 8086 & 80386 real mode)

[ORG 0] 

START_CODE:
	; 02/06/2022
	; (32 bit to 16 bit conversions for Retro UNIX 8086 v1)
	; 02/06/2022
	; 31/05/2022
	pop	ax ; number of arguments
	pop	dx ; argv[0]	
	;;mov	[argc], ax
	;mov	[argc], al

	;cmp	ax, 2
	cmp	al, 1
	jna	short dt_2 ; print current date time
dt_0:
	; set date & time
	; ((date [ mmddhhmm[yy] ]))	
	pop	bp ; word [cbp]  ; argv[1]

	;if(gtime()) {
	;	write(1, "bad conversion\n", 15);
	;	exit();

	call	gtime
	jnc	short dt_1

	mov	ax, msg_bad
dt_p_x:
	call	print_msg
	sys	_exit	
dt_1:
	;if(stime(timbuf) < 0)
	;	write(1, "no permission\n", 14);

	sys	_stime, [timbuf], [timbuf+2]
	jnc	short dt_2
dt_err:
	mov	ax, msg_no_perm
	jmp	short dt_p_x
dt_2:
	;time(timbuf);
	;cbp = cbuf;
	;ctime(timbuf);
	;write(1, cbuf, 20);
	;tzn = tzname[localtime(timbuf)[8]];
	;if (tzn)
	;	write(1, tzn, 3);
	;write(1, cbuf+19, 6);

	mov	ax, nextline
	call	print_msg

	sys	_time
	;mov	[timbuf], ax
	;mov	[timbuf+2], dx
	
	; dx:ax = unix epoch time
	call	ctime

	sys	_write, 1, cbuf, 25

	mov	ax, nextline
	jmp	short dt_p_x

;-----------------------------------------------------------------

gtime:
	; INPUT:
	;    bp = input string to be converted
	;		 (to unix epoch time)
	; OUTPUT:
  	;     cf = 0 -> OK
	;     cf = 1 -> bad input 	

	;t = gpair();
	;if(t<1 || t>12)
	;	goto bad;
	;d = gpair();
	;if(d<1 || d>31)
	;	goto bad;
	;h = gpair();
	;if(h == 24) {
	;	h = 0;
	;	d++;

	call	gpair
	jc	short _bad
	or	dl, dl
	jz	short bad
	cmp	dl, 12
	ja	short bad
	mov	[_t_], dx
	call	gpair
	jc	short _bad
	and	dl, dl
	jz	short bad
	cmp	dl, 31
	ja	short bad
	mov	[_d_], dx
	call	gpair
	jc	short _bad
	cmp	dl, 24
	ja	short bad
	jb	short gtime_1
	sub	dl, dl ; 0
gtime_1:
	mov	[_h_], dx
	call	gpair
	jc	short _bad
	cmp	dl, 59
	ja	short bad
	mov	[_m_], dx

	;m = gpair();
	;if(m<0 || m>59)
	;	goto bad;
	;y = gpair();
	;if (y<0) {
	;	time(nt);
	;	y = localtime(nt)[5];

	;call	time
	sys	_time
	; dx:ax = time (in unix epoch format)	

	call	gmtime

	call	gpair
	jnc	short gtime_2
	mov	dx, [year]
	jmp	short gtime_4
bad:
	;return(1);
	stc
_bad:
	retn
gtime_2:
	mov	ax, 2000
	cmp	ax, [year] ; 2000
	jna	short gtime_3
	mov	ax, 1900
gtime_3:
	add	dx, ax
gtime_4:
	mov	[_y_], dx

	;timbuf[0] = 0;
	;timbuf[1] = 0;
	;y =+ 1900;
	;for(i=1970; i<y; i++)
	;	gdadd(dysize(i));
	;while(--t)
	;	gdadd(dmsize[t-1]);
	
	xor	ax, ax
	;mov	[timbuf], ax ; 0
	;mov	[timbuf+2], ax ; 0

	sub	cx, cx ; 0
	mov	bx, 1970
gtime_5:
	cmp	bx, [_y_]
	jnb	short gtime_6
	call	dysize
	;call	gdadd
	add	[timbuf], ax
	;adc	word [timbuf], 0
	adc	[timbuf], cx ; 0 	
	inc	bx
	jmp	short gtime_5
	
gtime_6:
	;while(--t)
	;	gdadd(dmsize[t-1]);
	
	call	dmsize
	;xor	cx, cx
	add	[timbuf], ax
	;adc	word [timbuf+2], 0
	adc	[timbuf+2], cx ; 0

	; gdadd(d-1);
	mov	ax, [_d_]
	dec 	al
	add	[timbuf], ax
	;adc	word [timbuf+2], 0
	adc	[timbuf+2], cx ; 0

	; here, [timbuf] contains days since 1/1/1970

	;gmdadd(24, h);
	;gmdadd(60, m);
	;gmdadd(60, 0);

	mov	bx, [_h_]
	mov	cl, 24
	call	gmdadd
	mov	bx, [_m_]
	mov	cl, 60
	call	gmdadd

	sub	bx, bx ; 0
	;mov	cl, 60
	
	;call	gmdadd
	;retn

;-----------------------------------------------------------------

gmdadd: ; gmdadd(m, n)

	;timbuf[0] =* m;
	;t1 = timbuf[1];
	;while(--m)
	;	gdadd(t1);
	;gdadd(n);
	
	; cx = m
	; bx = n

	; 32 bit multiplication
	; sample:
	;	(0023h:4A5Bh)*6Ch
	;
	; result: 0EE3h:5E64h

	mov	ax, [timbuf] ; 4A5Bh
	mul	cx  ; * 006Ch
	push	ax  ; 5E64h
	push	dx  ; 001Fh	
	mov	ax, [timbuf+2] ; 0023h 
	mul	cx  ; * 006Ch
	; ax = 0EC4h
	; dx = 0
	pop	dx  ; 001Fh
	add	dx, ax ; 001Fh + 0EC4h
	; dx = 0EE3h
	pop	ax  ; 5E64h
	; dx:ax = 0EE3h:5E64h
	add	ax, bx ; [timbuf]*m + n
	adc	dx, 0
	mov	[timbuf], ax
	mov	[timbuf+2], dx
	retn

;-----------------------------------------------------------------

;gdadd:	; gdadd(n)
;
;	;t = timbuf[1]+n;
;	;if(t < timbuf[1])
;	;	timbuf[0]++;
;	;timbuf[1] = t;
;
;	; ax = n
;
;	add	[timbuf], ax
;	retn

;-----------------------------------------------------------------
; dysize (ctime.c)

dysize: ; dysize(y)
	
	;if((y%4) == 0)
	;	return(366);
	;return(365);

	; bl = low byte of year (to be checked)

	mov	ax, 365

	test	bl, 3
	jnz	short dysize_r

	inc	ax ; 366
dysize_r:
	retn

;-----------------------------------------------------------------
; dmsize (ctime.c)

dmsize: 
	
	;int	dmsize[12]
	;{
	;	31,
	;	28,
	;	31,
	;	30,
	;	31,
	;	30,
	;	31,
	;	31,
	;	30,
	;	31,
	;	30,
	;	31
	;};

	mov	bx, [_t_] ; 1 to 12
	dec	bl ; 0 to 11
	shl	bl, 1
	mov	ax, [bx+DMonth] ; (ctime.inc)	
	retn		

;-----------------------------------------------------------------

gpair:
	; INPUT:
	;    bp = number text (digit pair) address
	; OUTPUT:
  	;     dl (dx) = value (number) 		
 
	;cp = cbp;
	;if(*cp == 0)
	;	return(-1);
	;c = (*cp++ - '0') * 10;
	;if (c<0 || c>100)
	;	return(-1);
	;if(*cp == 0)
	;	return(-1);
	;if ((d = *cp++ - '0') < 0 || d > 9)
	;	return(-1);
	;cbp = cp;
	;return (c+d);

	xor	dx, dx

	mov	si, bp ; word [cbp] ; argv[1]
	lodsb
	and	al, al
	jz	short gp_c
	sub	al, '0'
	jb 	short gp_x
	;cmp	al, 10 ; ':'-'0'
	;ja	short gp_c
	cmp	al, 9
	ja	short gp_c	
	mov	ah, 10
	mul	ah
	;; (ax >= 0 and ax <= 100)
	; (ax >= 0 and ax <= 90)
	mov	dl, al
	lodsb
	or	al, al
	jz	short gp_c
	sub	al, '0'
	jb	short gp_x
	cmp	al, 9
	ja	short gp_c

	mov	bp, si

	add	dl, al

	; (dx >= 0 and dx <= 99)		
gp_x:
	retn
gp_c:
	stc
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
; ctime, localtime, asctime, gmtime functions

%include 'ctime.s' ; 02/06/2022
; 	(ctime.s file last modification date: 02/06/2022) 

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

;;argc:	dw 0
;argc:	db 0

; ----------------------------------------------------------------

;program_msg:
	;db  0Dh, 0Ah
	db  'Retro UNIX 8086 v1 DATE by Erdogan TAN - 02/06/2022'
	;db  0Dh, 0Ah, 0
	db 0

;usage_msg:
;	db  0Dh, 0Ah
;	db  'usage: date [ mmddhhmm[yy] ]' ; (unix v5)

;nextline:
;	db  0Dh, 0Ah, 0

msg_bad:
	db 0Dh, 0Ah
	db 'bad conversion'
nextline:
	db 0Dh, 0Ah, 0

msg_no_perm:
	db 0Dh, 0Ah
	db 'no permission'
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------

align 4

bss_start:

ABSOLUTE bss_start

timbuf:	resd 1 ; (unix epoch time, 32 bit)

_t_:	resw 1 ; month
_d_:	resw 1 ; day
_h_:	resw 1 ; hour
_m_:	resw 1 ; minute
_y_:	resw 1 ; year
	resw 1

; 31/05/2022
;-----------------------------------------------------------------
; Original UNIX v5 - date - c source code (date.s)
;-----------------------------------------------------------------
; UNIX V5 source code: see www.tuhs.org for details.
;-----------------------------------------------------------------
; v5root.tar.gz - usr/source/s1/date.c (archive date: 27-11-1974)

;int	timbuf[2];
;char	*cbp;
;
;char *tzname[2];
;int	dmsize[];
;char	cbuf[];
;char	*cbp;
;
;struct {
;	char	name[8];
;	char	tty;
;	char	fill1;
;	int	wtime[2];
;	int	fill2;
;} wtmp[2];
;
;main(argc, argv)
;int argc, **argv;
;{
;	register char *tzn;
;	extern int timezone, *localtime();
;	int wf;
;
;	if(argc > 1) {
;		cbp = argv[1];
;		if(gtime()) {
;			write(1, "bad conversion\n", 15);
;			exit();
;		}
;		if (*cbp != 's') {
;	/* convert to Greenwich time, on assumption of Standard time. */
;			dpadd(timbuf, timezone);
;	/* Now fix up to local daylight time. */
;			if (localtime(timbuf)[8])
;				dpadd(timbuf, -1*60*60);
;		}
;		time(wtmp[0].wtime);
;		wtmp[0].tty =  '|';
;		if(stime(timbuf) < 0)
;			write(1, "no permission\n", 14);
;		if ((wf = open("/usr/adm/wtmp", 1)) >= 0) {
;			time(wtmp[1].wtime);
;			wtmp[1].tty = '}';
;			seek(wf, 0, 2);
;			write(wf, wtmp, 32);
;		}
;	}
;	time(timbuf);
;	cbp = cbuf;
;	ctime(timbuf);
;	write(1, cbuf, 20);
;	tzn = tzname[localtime(timbuf)[8]];
;	if (tzn)
;		write(1, tzn, 3);
;	write(1, cbuf+19, 6);
;}
;
;gtime()
;{
;	register int i;
;	register int y, t;
;	int d, h, m;
;	extern int *localtime();
;	int nt[2];
;
;	if (*cbp == 's')
;		return(spidertime());
;	t = gpair();
;	if(t<1 || t>12)
;		goto bad;
;	d = gpair();
;	if(d<1 || d>31)
;		goto bad;
;	h = gpair();
;	if(h == 24) {
;		h = 0;
;		d++;
;	}
;	m = gpair();
;	if(m<0 || m>59)
;		goto bad;
;	y = gpair();
;	if (y<0) {
;		time(nt);
;		y = localtime(nt)[5];
;	}
;	if (*cbp == 'p')
;		h =+ 12;
;	if (h<0 || h>23)
;		goto bad;
;	timbuf[0] = 0;
;	timbuf[1] = 0;
;	y =+ 1900;
;	for(i=1970; i<y; i++)
;		gdadd(dysize(i));
;	while(--t)
;		gdadd(dmsize[t-1]);
;	gdadd(d-1);
;	gmdadd(24, h);
;	gmdadd(60, m);
;	gmdadd(60, 0);
;	return(0);
;
;bad:
;	return(1);
;}
;
;gdadd(n)
;{
;	register char *t;
;
;	t = timbuf[1]+n;
;	if(t < timbuf[1])
;		timbuf[0]++;
;	timbuf[1] = t;
;}
;
;gmdadd(m, n)
;{
;	register int t1;
;
;	timbuf[0] =* m;
;	t1 = timbuf[1];
;	while(--m)
;		gdadd(t1);
;	gdadd(n);
;}
;
;gpair()
;{
;	register int c, d;
;	register char *cp;
;
;	cp = cbp;
;	if(*cp == 0)
;		return(-1);
;	c = (*cp++ - '0') * 10;
;	if (c<0 || c>100)
;		return(-1);
;	if(*cp == 0)
;		return(-1);
;	if ((d = *cp++ - '0') < 0 || d > 9)
;		return(-1);
;	cbp = cp;
;	return (c+d);
;}
;
;/*
; * get time from spider network.
; */
;char	asktime[] {0226, 0207, 0205};
;
;spidertime()
;{
;	register tiuf, n;
;	static char buf[10];
;	struct { char ch[4]; };
;	int c;
;
;	if ((tiuf = open("/dev/tiu/d0", 2)) < 0)
;		return(1);
;	/* get trouble */
;	snstat(tiuf, &c, 3);
;	/* set signal */
;	c = 3;
;	snstat(tiuf, &c, 0);
;	write(tiuf, asktime, 3);
;	snstat(tiuf, &c, 3);
;	n = read(tiuf, buf, 10);
;	/* get signal byte */
;	snstat(tiuf, &c, 1);
;	if (c!=3 || buf[0]!=012 || buf[5]!=0177600)
;		return(1);
;	timbuf[0].ch[0] = buf[2];
;	timbuf[0].ch[1] = buf[1];
;	timbuf[0].ch[2] = buf[4];
;	timbuf[0].ch[3] = buf[3];
;	return(0);
;}
