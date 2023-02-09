; ****************************************************************************
; sh386.s (sh1.s) - Retro Unix 386 v1 Shell - /bin/sh
; ----------------------------------------------------------------------------
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 03/01/2016 ]
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (Bell Laboratories, 1971-1972)
; ****************************************************************************
;
; <Preliminary Release of UNIX Implementation Document>
; <Isuse: D, Date: 17/3/1972, ID: IMO.1-1, Section: E.11> 
; <sh - command interpreter>
;
; ****************************************************************************

; sh1.s (27/12/2015, Retro UNIX 386 v1.1)
; sh0.s (24/08/2015, Retro UNIX 386 v1, NASM 2.11, 32 bit version of 'sh.asm')
; SHELL02.ASM, 13/11/2013 - 08/04/2014 (sh.asm, Retro UNIX 8086 v1, MASM 6.11) 

; 27/08/2015
; 24/08/2015
; 08/04/2014
; 13/11/2013

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
;/ sh -- command interpreter
	
	;;27/12/2015
	;;clear BSS
	;mov	ecx, ((bss_end - bss_start) + 3) / 4
	;sub	eax, eax
	;mov	edi, bss_start
	;rep	stosd
s0:
	mov	ebp, esp
		; mov sp,r5
	mov	[shellarg], ebp
		; mov r5,shellarg / save orig sp in shellarg
	mov	ebx, [ebp+4]
	cmp	byte [ebx], '-'
		 ; cmpb	*2(r5),$'- / was this sh called by init or loginx~
	jne	short s1
		; bne 2f / no
	sys	_intr, 0
		; sys intr; 0 / yes, turn off interrupts
	sys	_quit, 0
		; sys quit; 0
	sys	_write, 1, msg_unix_sh, msgsh_size
s1: ;2:
	sys	_getuid
		; sys	getuid / who is user
	;and	eax, eax
	and	al, al
		; tst r0 / is it superuser
	jnz	short s2
		; bne 2f / no
	mov	byte [_at], '#'
		; movb $'#,at / yes, set new prompt symbol
s2: ;2:
	cmp	dword [ebp], 1
		; cmp (r5),$1 / tty input?
	jna	short newline
		; ble newline / yes, call with '-' (or with no command
		      ; / file name)
	xor	ebx, ebx
		; clr r0 / no, set tty
	sys	_close
		; sys close / close it
	mov	ebx, [ebp+8] ; arg 1
		; mov 4(r5),0f / get new file name
	xor	ecx, ecx ; arg 2
	sys	_open
		; sys open; 0:..; 0 / open it
	jnc	short s3
		; bec 1f / branch if no error
	mov	esi, msgNotFound
	call	error
		; jsr r5,error / error in file name
		; <Input not found\n\0>; .even
	sys	_exit
		; sys exit
s3: ;1:
	mov	byte [_at], 0
		; clr at / clear prompt character, if reading non-tty
		   ; / input file
	jmp	short newcom
newline:
	cmp	byte [_at], 0
		; tst at / is there a prompt symbol
	jna	short newcom
		; beq newcom / no
		; mov $1,r0 / yes
nl:
	sys	_write, 1, prompt, p_size
	;sys	_write, 1, _at, 2
		; sys write; at; 2. / print prompt
newcom:
	mov	esp, [shellarg]
		; mov shellarg,sp /
	mov	esi, parbuf 
		; mov $parbuf,r3 / initialize command list area
	mov	edi, parp
		; mov $parp,r4 / initialize command list pointers
	xor	eax, eax
	mov	[infile], eax ; 0	
		; clr infile / initialize alternate input
	mov	[outfile], eax ; 0
		; clr outfile / initialize alternate output
	mov	[glflag], al ; 0
	;mov	[glflag], ax ; 0
		; clr glflag / initialize global flag
	; 27/12/2015
	mov	[FCAT], al ; 0
newarg:
	call	blank
		; jsr pc,blank / squeeze out leading blanks
	call	delim
	je	short nch4 ; '\n', ';', '&'
		; jsr r5,delim / is new character a ; \n or &
		;     br 2f / yes
	push	esi
		; mov r3,-(sp) / no, push arg pointer onto stack
	cmp	al, '<'
		; cmp r0,$'< / new input file?
	jne	short na1
		; bne 1f / no
	mov	[infile], esi
		; mov (sp),infile / yes, save arg pointer
	;jmp 	short na4
	jmp	short na3
	;mov	dword [esp], 0
		; clr (sp) / clear pointer
	;jmp	short nch1
		; br 3f
na1: ;1:
	cmp	al, '>'
		; cmp r0,$'> / new output file?
	jne	short nch0
	;jne	short newchar
		; bne newchar / no
	mov	[outfile], esi
		; mov (sp),outfile / yes, save arg pointer
	; 27/12/2015
	call	getc
	cmp	al, '>'
	jne	short na2
	;
	mov	[FCAT], al ; '>>'
	jmp	short na3
na2:
	cmp	al, 20h ; ' '
	jne	short na4
na3:
	call	blank
na4:	
	mov	dword [esp], 0
		; clr (sp) / clear pointer
	;jmp	short nch1
		; br 3f
	jmp	short nch7
newchar:
	cmp	al, 20h
		; cmp $' ,r0 / is character a blank
	je	short nch2
		; beq 1f / branch if it is (blank as arg separator)
        cmp     al, 8Dh ; 128 + 13
	je	short nch2
		; cmp $'\n+200,r0 / treat \n preceded by \
		; beq 1f / as blank
nch0:
	call	putc
		; jsr pc,putc / put this character in parbuf list
nch1: ;3:
	call	getc
		; jsr pc,getc / get next character
nch7:
	call	delim
	jne	short newchar
	;jz	short nch2 ; '\n', ';', '&'
		; jsr	r5,delim / is char a ; \n or &,
		;	br 1f / yes
	;jmp	short newchar
		; br newchar / no, start new character tests
nch2: ;1:
	mov	byte [esi], 0
	inc	esi
		; clrb (r3)+ / end name with \0 when read blank, 
			  ; or delim
	pop	ebx
	mov	[edi], ebx
		; mov (sp)+,(r4)+ / move arg ptr to parp location
	or	ebx, ebx
	jz	short nch3
	;jnz	short nch3
		; bne 1f / if (sp)=0, in file or out file points
				; to arg
	add	edi, 4
		; tst -(r4) / so ignore dummy (0), in pointer list
nch3: ;1:
	call	delim
	jne	short newarg
	;jz	short nch4 ; '\n', ';', '&'
		; jsr r5,delim / is char a ; \n or &.
		;     br 2f / yes
	;jmp	short newarg
		; br newarg / no, start newarg processing
nch4: ;2:
	mov	dword [edi], 0
		; clr (r4) / \n, &, or ; takes to here 
			 ; / (end of arg list) after 'delim' call
	push	eax
		; mov r0,-(sp) / save delimiter in stack
	call	docom
		; jsr pc,docom / go to exec command in parbuf
	cmp	byte [esp], '&'
		; cmpb (sp),$'& / get a new command without wait?
        je      newcom
		; beq newcom / yes
	and	edx, edx
		; tst r1 / was chdir just executed or line ended
			; / with ampersand?
	jz	short nch6
		; beq 2f / yes
nch5: ;1:
	sys	_wait
		; sys wait / no, wait for new process to terminate
                        ; / command executed)
	jc	short nch6
		; bcs 2f / no, children not previously waited for
	cmp	eax, edx
		; cmp r0,r1 / is this my child
	jne	short nch5
		; bne 1b
nch6: ;2:
	cmp	byte [esp], 0Dh
	;cmp	byte [esp], 0Ah
		; cmp (sp),$'\n / was delimiter a new line
        je      newline
		; beq newline / yes
        jmp     newcom
		; br newcom / no, pick up next command
docom:
	sub	edi, parp
		; sub $parp,r4 / out arg count in r4
	jne	short dcom1
		; bne 1f / any arguments?
dcom0:
	sub 	edx, edx ; 0
		; clr r1 / no, line ended with ampersand
	retn
		; rts pc / return from call
dcom1: ;1:
	mov	ebx, edi
	; 06/12/2013
	mov	esi, qecho 
	call	chcom
	jnz	short dcom7
	; 28/12/2015
	mov	ebp, ebx
	mov	edi, parp
dcom9:  ; CRLF
	sys	_write, 1, nextline, 2
	;
	sub	ebp, 4 ; remain arg count x 4
	jna	short dcom0
	add	edi, 4
	mov	esi, [edi]
	;or 	esi, esi
	;jz	short dcom0
	mov	edx, esi ; string address
dcom10:
	lodsb
	or 	al, al
	jnz	short dcom10
	xchg 	edx, esi
	sub	edx, esi  ; byte count
	dec	edx
	;jz	short dcom0
	; write string
	sys	_write, 1, esi ; edx = byte count
	jmp	short dcom9
dcom7:
	mov	esi, qchdir
	call	chcom
	jnz	short dcom4
		; jsr r5,chcom; qchdir / is command chdir?
		;     br 2f / command not chdir
dcom12:
	cmp	bl, 8 ; 8 = arg count x 4 (24/08/2015)
	;cmp	bx, 8
		; cmp r4,$4 / prepare to exec chdir, 
			  ; 4 = arg count x 2
	je	short dcom2
		; beq 3f
dcom8:
	mov	esi, msgArgCount
	call	error
		; jsr r5,error / go to print error
		;  	<Arg count\n\0>; .even
	;jmp	short dcom3
		; br 4f
	jmp	short dcom0
dcom2: ;3:
	mov	ebx, [parp+4]
		;mov parp+2,0f / move directory name to sys call
	sys	_chdir
		; sys chdir; 0:0 / exec chdir
	jnc	short dcom3
		; bec 4f / no error exit
	mov	esi, msgBadDir
	call	error
		; jsr r5,error / go to print error
		; 	<Bad directory\n\0>; .even
				; / this diagnostic
dcom3: ;4:
	xor	edx, edx ; 0
		; clr r1 / set r1 to zero to skip wait
	retn
		; rts pc / and return
dcom4: ;2:
	; 06/12/2013
	mov	esi, qcd
	call	chcom
	jz	short dcom12
dcom11:
	mov	esi, glogin
	call	chcom
	jnz	short dcom5
		; jsr r5,chcom; glogin / is command login?
		;     br 2f / not loqin, go to fork
	sys	_exec, parbuf, parp
		; sys exec; parbuf; parp / exec login
	sys	_exec, binpb, parp
		; sys exec; binpb; parp / or /bin/login
dcom5: ;2: / no error return??
	mov	ebx, newproc
	; child process will return to 'newproc' address
	sys	_fork
		; sys fork / generate sh child process
			 ; for command
                ;     br newproc / exec command with 
			 ; new process
	; parent process will return here
	jnc	short dcom6
		; bec 1f / no error exit, old process
	mov	esi, msgTryAgain
	call	error
		; jsr r5,error / go to print error
		;     <Try again\n\0>; .even / this diagnostic
        jmp     newline
		; jmp newline / and return for next try
dcom6: ;1:
	mov	edx, eax ; child process ID
		; mov r0,r1 / save id of child sh
	retn
		; rts pc / return to "jsr pc, docom" call
			; in parent sh
error:
	sys	_write, 1, nextline, 2
s4:
	lodsb
	mov	[och], al
                ; movb (r5)+,och / pick up diagnostic character
	and	al, al
	jz	short s5
		; beq 1f / 0 is end of line
	sys	_write, 1, och, 1
		; mov $1,r0 / set for tty output
		; sys write; och; 1 / print it
	jmp	short s4
	;jmp	short error
		; br error / continue to get characters
s5: ;1:
	;inc	esi
		; inc r5 / inc r5 to point to return
	;;and	si, 0FFFEh
	;shr	esi, 1
	;shl	esi, 1
		; bic $1,r5 / make it even
	sys	_seek, 0, 0, 2	
		; clr r0 / set for input
		; sys seek; 0; 2 / exit from runcom. skip to
			       ; / end of input file
	retn
		;; ((/ rts r5)) 
		;; (not in original unix v1 'sh.s')

chcom: ; / has no effect if tty input
		; mov (r5)+,r1 / glogin gchdir r1, bump r5
	mov	edi, parbuf
		; mov $parbuf,r2 / command address  r2 'login'
s6: ;1:
	lodsb
		; movb (r1)+,r0 / is this command 'chdir'
	scasb
                ; cmpb (r2)+,r0 / compare command name byte
                              ; / with 'login' or 'chdir'
	jne	short s7
		; bne 1f / doesn't compare
	or	al, al
		; tst r0 / is this
	jnz	short s6
		; bne 1b / end of names
		; tst (r5)+ / yes, bump r5 again to execute 
			; / login or chdir
s7: ;1:
	retn
		; rts r5 / no, return to exec command

putc:
	cmp	al, 27h ; '
		; cmp r0,$'' / single quote?
	je	short pch1	
		; beq 1f / yes
	cmp	al, 22h ; "
		; cmp r0,$'" / double quote
	je	short pch1
		; beq 1f / yes
	and	al, 7Fh
		; bic $!177,r0 / no, remove 200, if present
	mov	[esi], al
	inc	esi
		; movb r0,(r3)+ / store character in parbuf
	retn
		; rts pc
pch1: ;1:
	push	eax
		; mov r0,-(sp) / push quote mark onto stack
pch2: ;1:
	call	getc
		; jsr pc,getc / get a quoted character
	cmp	al, 0Dh
	;cmp	al, 0Ah ; \n
		; cmp r0,$'\n / is it end or line
	jne	short pch3
		; bne 2f / no
	mov	esi, msgImbalance
	call	error
		; jsr r5,error / yes, indicate missing
			      ; quote mark
		;     <"' imbalance\n\0>; .even
        jmp     newline
		; jmp newline / ask for new line
pch3: ;2:
	cmp	[esp], al
		; cmp r0,(sp) / is this closing quote mark
	je	short pch4
		; beq 1f / yes
	and	al, 7Fh
		; bic $!177,r0 / no, strip off 200
			      ; if present
	mov	[esi], al
	inc	esi
		; movb r0,(r3)+ / store quoted character
			      ; in parbuf
	jmp	short pch2
		; br 1b / continue
pch4: ;1:
	pop	eax
		; tst (sp)+ / pop quote mark off stack
	retn
		; rts pc / return

; / thp`e new process

newproc:
	mov	esi, [infile]
	or	esi, esi
	jz	short np2
		; mov infile,0f / move pointer to new file name
		; beq 1f / branch if no alternate read file given
	cmp 	byte [esi], 0
		; tstb *0f
	jna	short np1
		; beq 3f / branch if no file name given
	sys	_close, 0
		; clr r0 / set tty input file name
		; sys close / close it
	sys	_open, esi, 0  
		; sys open; 0:..; 0 / open new input file 
				  ; for reading
	jnc	short np2
		; bcc 1f / branch if input file ok
np1: ;3:
	mov	esi, msgInputFile
	call	error
		; jsr r5,error / file not ok, print error
		;     <Input file\n\0>; .even / this diagnostic
	sys	_exit
		; sys exit / terminate this process 
			  ; and make parent sh
np2: ;1:
	mov	esi, [outfile]
		; mov outfile,r2 / more pointer to new file name
	and	esi, esi
	jz	short np6
		; beq 1f / branch if no alternate write file
	; 27/12/2015
	cmp	byte [esi], 0
	jna	short np4
	;
	;cmp	byte [esi], '>'
		; cmpb (r2),$'> / is > at beqinning of file name?
	;jne	short np3
		; bne 4f / branch if it isn't
	;inc	esi
		; inc r2 / yes, increment pointer
	; 27/12/2015
	cmp	byte [FCAT], '>' ; '>>'
	jne	short np3
	;
	sys	_open, esi, 1
		; mov r2,0f
		; sys open; 0:..; 1 / open file for writing
	jnc	short np5
		; bec 3f / if no error
np3: ;4:
	sys	_creat, esi, 15 ; Decimal 15 = Octal 17
		; mov r2,0f
		; sys creat; 0:..; 17 / create new file 
				    ; with this name
	jnc	short np5
		; bec 3f / branch if no error
np4: ;2:
	mov	esi, msgOutputFile
	call	error
		; jsr r5,error
		;     <Output file\n\0>; .even
	sys	_exit
		; sys exit
np5: ;3:
	sys	_close, eax
		; sys close / close the new write file
		; mov	r2,0f / move new name to open
np10:
	sys	_close, 1
		; mov $1,r0 / set tty file name
		; sys close / close it
	sys	_open, esi, 1
		; sys open; 0:..; 1 / open new output file,
			      ; /it now has file descriptor 1
	; 27/12/2015
	cmp	byte [FCAT], '>' ; '>>'
	jne	short np6
	;
	sys	_seek, eax, 0, 2
		; sys seek; 0; 2 / set pointer to 
				; current end of file
np6: ;1:
	cmp	byte [glflag], 0
		; tst glflag / was *, ? or [ encountered?
        ja      short np9
		; bne 1f / yes
	sys	_exec, parbuf, parp
		; sys exec; parbuf; parp / no, execute
					;  this command
	sys	_exec, binpb, parp
		; sys exec; binpb; parp / or /bin/this command
np7: ;2:
	sys	_stat, binpb, inbuf
		; sys stat; binpb; inbuf / if can't execute
					; / does it exist?
	jc	short np8
		; bes 2f / branch if it doesn't
	mov	esi, parp-4
	mov	dword [esi], shell
		; mov $shell,parp-2 / does exist,
				   ;  not executable
	mov	eax, binpb
	mov	[parp], eax
		; mov $binpb,parp / so it must be
	sys	_exec, shell, esi 
		; sys exec; shell; parp-2 / a command file,
		; / get it with sh /bin/x (if x name of file)
np8: ;2:
	mov	esi, msgNoCmd
	call	error
		; jsr r5,error / a return for exec 
			       ; is the diagnostic
		;     <No command\n\0>; .even
	mov	esp, [shellarg]
	sys 	_exit
		; sys exit
np9: ;1:
	mov	esi, parp-4
	mov	dword [esi], glob
		; mov $glob,parp-2 / prepare to process *,?
	sys	_exec, glob, esi
		; sys exec; glob; parp-2
			    ; / execute modified command
	jmp	short np8
		; br 2b

delim:
        cmp     al, 0Dh ; carriage return
	je	short dlim2
	;cmp	al, 0Ah
		; cmp r0,$'\n / is character a newline
	;je	short dlim2
		; beq 1f
	cmp	al, '&'
		;cmp r0,$'& / is it &
	je	short dlim2
		; beq 1f / yes
	cmp	al, ';'
		; cmp r0,$'; / is it ;
	je	short dlim2
		; beq 1f / yes
	cmp	al, '?'
		; cmp r0,$'? / is it ?
	je	short dlim1
		; beq 3f
	cmp	al, '['
		; cmp r0,$'[ / is it beginning of character string
		      ; / (for glob)
	jne	short dlim2
		; bne 2f
dlim1: ;3:
	inc	byte [glflag]
		; inc glflag / ? or * or [ set flag
;2:
	;tst	(r5)+ / bump to process all except \n,;,&
dlim2: ;1:
	; zf = 1 if the char is '\n' or ';' or '&'
	retn
		; rts r5
blank:
	call	getc
		; jsr pc,getc / get next character
	cmp	al, 20h
		; cmp $' ,r0 / leading blanks
	je	short blank
		; beq blank / yes, 'squeeze out'
	cmp	al, 8Dh ; 80h + 0Dh
        ;cmp     al, 8Ah ; 80h + 0Ah
	je	short blank
	        ; cmp r0,$200+'\n / new-line preceded by \
				 ;  is translated
		; beq blank / into blank
	; 28/12/2015
	cmp	al, 0Ah
	je	short blank
	;
	retn
		; rts pc
getc:
	cmp	dword [param], 0
		; tst param / are we substituting for $n
	ja	short gch3
		; bne 2f/ yes
gch0:
        mov     ebx, [inbufp]
		; mov inbufp,r1 / no, move normal input pointer to r1
s8:
	cmp	ebx, [einbuf]
		; cmp r1,einbuf / end of input line?
	jb	short gch1
		; bne 1f / no
	call	getbuf	
		; jsr pc,getbuf / yes, put next console line
				;  in buffer
	jmp	short gch0
		; br getc
gch1: ;1:
	mov	al, [ebx]
	inc	ebx
		; movb (r1)+,r0 / move byte from input buffer to r0
	mov	[inbufp], ebx
		; mov r1,inbufp / increment routine
	or	al, [escap]
	;or	ax, escap
		; bis escap,r0 / if last character was \ this adds
		        	; / 200 to current character
	;mov	byte [escap], 0
	;mov	word [escap], 0
		; clr escap / clear, so escap normally zero
	cmp	al, '\'
		; cmp r0,$'\\ / note that \\ is equal \ in as
	je	short gch2
		; beq 1f
	mov	byte [escap], 0
	cmp	al, '$'
		; cmp r0,$'$ / is it $
	je	short gch5
		; beq 3f / yes
	retn
		; rts pc / no
gch2: ;1:
        mov     byte [escap], 80h
	;mov	word [escap], 128
		; mov $200,escap / mark presence of \ in command line
	jmp	short s8
	;jmp	short gch0
		; br getc / get next character
gch3: ;2:
	mov	ebx, [param]
	mov	al, [ebx]
		; movb *param,r0 / pick up substitution character
				; / put in r0
	or	al, al
	jz	short gch4
		; beq	1f / if end of substitution arg, branch
	inc	dword [param]
		; inc param / if not end, set for next character
	retn
		; rts pc / return as though character in ro is normal
		       ; / input
gch4: ;1:
	mov	dword [param], 0
		; clr param / unset substitution pointer
	jmp	short gch0
		; br getc / get next char in normal input
gch5: ;3:
	call	gch0
	;call	getc
		; jsr pc,getc / get digit after $
	sub	al, '0'
		; sub $'0,r0 / strip off zone bits
	cmp	al, 9
		; cmp r0,$9. / compare with digit 9
	jna	short gch6 
		; blos 1f / less than or equal 9
	mov	al, 9
		; mov $9.,r0 / if larger than 9, force 9
gch6: ;1:
	mov	ebx, [shellarg]
		; mov shellarg,r1 / get pointer to stack for
		           ; / this call of shell
	movzx 	eax, al	; al->eax
	inc	al
	;inc	eax
		; inc r0 / digit +1
	cmp	eax, [ebx]
		; cmp r0,(r1) / is it less than # of args 
			      ;	in this call
	jnb	short gch0
		; bge getc / no, ignore it. so this $n is not replaced
	shl	al, 2 ; multiply by 4 (24/08/2015)
	;shl	al, 1
	;;shl	ax, 1
		; asl r0 / yes, multiply by 2 (to skip words)
	add	ebx, eax
		; add r1,r0 / form pointer to arg pointer (-2)
	mov	eax, [ebx+4]
	mov	[param], eax
		; mov 2(r0),param / move arg pointer to param
        jmp     getc
		; br getc / go to get substitution arg for $n
getbuf:
	mov	ecx, inbuf
		; mov $inbuf,r0 / move input buffer address
        mov     [inbufp], ecx
		; mov r0,inbufp / to input buffer pointer
	mov	[einbuf], ecx
		; mov r0,einbuf / and initialize pointer to end of
		         ; / character string
	dec	ecx
		; dec r0 / decrement pointer so can utilize normal
		       ; / 100p starting at 1f
		; mov r0,0f / initialize address for reading 1st char
	mov	edx, 1
gbuf0: ;1:
	inc	ecx
		; inc 0f / this routine filles inbuf with line from
		       ; / console - if there is cnc
	push	ecx
	; edx = 1
	sys	_read, 0, och
	pop	ecx
	;xor	ebx, ebx ; 0
	;sys	_read ; sys _read, ebx, ecx, edx ; ebx = 0, edx = 1
		; sys read; 0:0; 1 / read next char into inbuf
        jc      xit1
		; bcs xit1 / error exit
	and	eax, eax
		; tst r0 / a zero input is end of file
        jz      xit1
		; beq xit1 / exit
	inc	dword [einbuf]  ; 08/04/2014 (24/08/2015, 32 bit)
	mov	al, [och]
	cmp	byte [_at], 0
	jna	short gbuf1
	cmp	al, 8 ; backspace
	je	short gbuf3
	cmp	al, 127 ; delete
	je	short gbuf6 ; 06/12/2013
gbuf1:
	;mov	ebx, ecx
	;inc	dword [einbuf]
		; inc einbuf / eventually einbuf points to \n
		       ; / (+1) of this line
	cmp	ecx, inbuf + 256
		; cmp 0b,$inbuf+256. / have we exceeded 
				   ; input buffer size
        jnb     xit1
		; bhis xit1 / if so, exit assume some sort of binary
	; 08/04/2014
	cmp	al, 0Dh
	jne	short gbuf8
	mov	ebx, [einbuf]
	dec	ebx
	mov	[ebx], al
	retn
gbuf8:
	mov	ebx, ecx
	mov	[ebx], al
	;cmp	al, 0Ah ; \n
		; cmpb	*0b,$'\n / end of line?
	;je	short  gbuf5
	;jne	short gbuf1
		; bne 1b / no, go to get next char
	;cmp	al, 0Dh ; ENTER
	;je	short gbuf5
	cmp	byte [_at], 0 ; at > 0 --> tty input
	jna	short gbuf0
	cmp	al, 1Bh	; ESC
	jne	short gbuf2
	mov	eax, inbuf
	mov	[inbufp], eax
	mov	[einbuf], eax
        jmp     nl  ; cancel current command, new line
gbuf2:
	 ; 28/12/2015
	cmp	byte [_at], 0
	jna	gbuf0
gbuf7:
	push	ecx
	;mov	[och], al
	; edx = 1
	sys	_write, 1, och
	;sys	_write, 1, och, 1  ; echo (write char on tty)
	pop	ecx
        jmp     gbuf0
gbuf6: ; DELETE key -> BACKSPACE key
	; mov 	al, 8
	mov	byte [och], 8 ; 06/12/2013
gbuf3:
	; 08/04/2014
	dec	dword [einbuf] ; (24/08/2015, 32 bit code)
	; 12/12/2013
	dec	ecx
	cmp	ecx, inbuf
	jb	short gbuf4
	dec 	ecx
	; 08/04/2014
	;jmp	short gbuf2
	jmp 	short gbuf7
gbuf4:
	;mov 	al, 7
	mov	byte [och], 07h ; beep
	; 08/04/2014
	;jmp	short gbuf2
	jmp 	short gbuf7
;gbuf5:
;	retn
		; rts pc / yes, return

xit1:
	sys	_exit
		; sys exit

;-----------------------------------------------------------------
;  DATA
;-----------------------------------------------------------------

;  /// Messages

msg_unix_sh:	db 0Dh, 0Ah
		db 'Retro Unix 386 v1.1 - shell'
		;db 0Dh, 0Ah
msgsh_size equ  $ - msg_unix_sh
		;db 0
		db '03/01/2016'
nextline:	db 0Dh, 0Ah, 0
;Error messages:
msgNotFound: 	db 'Input not found', 0
msgArgCount: 	db 'Arg count',  0
msgBadDir: 	db 'Bad directory', 0
msgTryAgain: 	db 'Try again', 0
msgImbalance:   db 22h, 27h, 20h, 'imbalance',  0
msgInputFile: 	db 'Input file', 0
msgOutputFile: 	db 'Output file', 0
msgNoCmd: 	db 'No command',  0

; /// Commands, files, parameters

; 27/12/2015
FCAT:	; '>>' 
	db 0

;quest:
	;db '?', 0Dh, 0Ah
	;<?\n>

prompt:
	db 0Dh, 0Ah
_at:
	db '@ '
	;<@ >
p_size  equ $ - prompt

qecho: 	db 'echo', 0
;
qcd:	db 'cd', 0
;
qchdir:
	db 'chdir', 0
	;<chdir\0>
glogin:
	db 'login', 0
	;<login\0>
shell:
	db '/bin/sh', 0
	;</bin/sh\0>
glob:
	db '/etc/glob', 0
	;</etc/glob\0>
binpb:
	db '/bin/'
	;</bin/>

; /// BSS data

bss_start:

ABSOLUTE bss_start

parbuf:
        resb 1000
 	; .=.+1000.
alignb 2
	;.even
param:
	resd 1
	;.=.+2
glflag:
	resb 1
	resb 1
	;.=.+2
infile:
	resd 1
	; .=.+2 
outfile:
	resd 1
	;.=.+2
; parp-4
	resd 1
	;.=.+2 / room for glob
parp:
        resb 200
	;.=.+200.
inbuf:
        resb 256
	;.=.+256.
;escap:
	;resw 1
	;.=.+2
inbufp:
	resd 1
	;.=.+2
einbuf:
	resd 1
	;.=.+2
och:
	resb 1
	resb 1
	;.=.+2
shellarg:
	resd 1
	;.=.+2
escap:
	resb 1
	;
	resb 1

bss_end:
