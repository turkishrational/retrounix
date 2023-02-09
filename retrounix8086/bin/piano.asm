; PIANO3.ASM  [ For true DOS ]
; One-Octave 'Piano' Program
; Free from Annie (2006)
;
; Retro UNIX 8086 v1 & MASM 
; modification by Erdogan Tan (08/12/2013)

; [ Last Modification: 18/03/2022 ]
;
; Use keyboard number keys 1 through 8 to play the notes.
; Space bar toggles the 'sustain' function.  ESC exits.
;
;

.8086

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

sys macro syscallnumber, arg1, arg2, arg3

    ; Retro UNIX 8086 v1 system call.

    ifnb <arg1> 	
      mov bx, arg1
    endif
    
    ifnb <arg2> 	
      mov cx, arg2
    endif

    ifnb <arg3> 	
      mov dx, arg3
    endif
     			
    mov ax, syscallnumber	
    int 20h	
   
    endm

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>


code   	SEGMENT PUBLIC 'CODE'
        assume cs:code,ds:code,es:code,ss:code

start_code:
        jmp     start           ;go start the program
;
tog     db      0               ;our 'sustain' flag
;
start:
        mov     si,offset msg   ;point DX to sign-on message
        call    prt_str         ;print it
;
; Get user keypress.
;
get_key:
	mov	dx, 1
	xor	bx, bx
	; 18/03/2022
	;mov	cx, sp
	;dec	cx
	;dec	cx
@@:
	push	bx
	mov	cx, sp ; 18/03/2022
	sys	_read
      	pop	ax
        cmp     al, 27          ;was ESC pressed?
        jz      short quit      ;yes, so go exit
        cmp     al, 32          ;was SPACE pressed?
        jz      short toggle    ;yes, so go toggle 'sustain' mode
;
; Filter out all keys except '1' through '8' by checking the scan code.
;
        cmp     al, '1'         ;less than '1'?
        ;jl	short get_key	;yes, so ignore it
        jl	short @b 
	cmp     al, '8'         ;greater than '8'?
        ;jg	short get_key	;yes, so ignore it
	jg	short @b
;
; Set up the tone parameters.
;
        sub     al, '1' 	;change ascii code to number	
        shl     al, 1           ;* by 2 (2 bytes/word)
        cbw                     ;byte --> word in AX
        mov     bx,ax           ;put in BX (for table)
        mov     ax,0            ;numerator (low word)
        mov     dx,12h          ;(high word)
        div     word ptr [table] + bx  ;divisor from table
        mov     bx,ax           ;save quotient in BX
;
; Set 1/pitch into timer, then turn on tone.
;
        mov     al,10110110b    ;the magic number...
        mov     dx,43h          ;
        out     dx,al           ;
        mov     ax,bx           ;1/pitch into AX
        mov     dx,42h          ;
        out     dx,al           ;
        mov     al,ah           ;MSB to AL, then...
        out     dx,al           ;
        mov     dx,61h          ;
        in      al,dx           ;
        or      al,3            ;turn on bits 0 and 1...
        out     dx,al           ;
        call    clr_buf         ;yes, so go clear keyboard buffer
        cmp     tog,1           ;is 'sustain' on?
        je      short get_key   ;go get another keypress
;
; Delay for 2/18ths of a second.
;
delay:
        mov     ah,00h          ;function 0 - get system timer tick
        int     01Ah            ;call ROM BIOS time-of-day services
        add     dx,2            ;add our delay value to DX
        mov     bx,dx           ;store result in BX
pozz:
        int     01Ah            ;call ROM BIOS time-of-day services
        cmp     dx,bx           ;has the delay duration passed?
        jl      short pozz      ;no, so go check again
        call    stopnote        ;go turn off the note
        jmp     short get_key   ;go get another keypress
;
quit:
        jmp     exit            ;'lilypad' to exit
;
; Toggle 'sustain' mode.
;
toggle:
        cmp     tog,0           ;'sustain' mode currently off?
        je      short turn_on   ;yes, so go turn it on
        mov     tog,0           ;no, so set 'sustain' flag to 'off'
        call    stopnote        ;turn off note
        call    clr_buf         ;go clear keyboard buffer
        ;mov    dx,offset off   ;point DX to appropriate screen msg
        mov     si,offset off ; 18/03/2022
	call    prt_str         ;print it
        jmp     get_key         ;go get another keypress
turn_on:
        mov     tog,1           ;set 'sustain' flag to 'on'
        call    clr_buf         ;go clear keyboard buffer
        ;mov    dx,offset onn   ;point DX to appropriate screen msg
        mov     si,offset onn ; 18/03/2022
	call    prt_str         ;print it
        jmp     get_key         ;go get another keypress
;
; Clear the keyboard buffer.
;
clr_buf:
	xor	cx, cx	; 18/03/2022
        xor	bx, bx
	sys	_gtty
	and	bx, bx
	;jz	short stopnote	
	jz	short @f ; 18/03/2022
	xor	bx, bx
	push	bx
	mov	cx, sp
	mov	dx, 1
	sys	_read
	pop	ax
	jmp	short clr_buf
@@:
	retn
;
; Turn off speaker.
;
stopnote:
        push    dx              ;preserve DX
        mov     dx,61h          ;
        in      al,dx           ;
        and     al,11111100b    ;mask lower 2 bits
        out     dx,al           ;
        pop     dx              ;restore DX
        retn                    ;return to caller
;
; ESC pressed, so exit.
;
exit:
        call    stopnote        ;go silence the speaker
	mov	ax, 0D0Ah	; New line (0Dh, 0Ah)
	push	ax
	mov	cx, sp
	mov	bx, 1
        mov     dx, 2
	sys	_write
	pop	ax
	sys	_exit		; exit (terminate process)
@@:
        hlt
        jmp     short @b 

;
; Our 'print string' routine.
;
prt_str:
	xor	dx, dx
	push	si
@@:
	lodsb
	cmp	al, '$'
	je	short @f
	inc	dx
	jmp	short @b
@@:
	pop	cx
	mov	bx, 1
	sys	_write 
	retn
;
msg     db      13,10,'PIANO3'
        db      13,10,'Keyboard number keys 1 through 8 play the notes.'
        db      13,10,'ESC exits.  SPACE BAR toggles sustain -- now OFF$'
onn     db      08,08,'N ',08,'$'
off     db      08,'FF$'
;
;frequencies of notes
;
table   dw      262	;middle C
        dw      294	;D
        dw      330	;E
        dw      347	;F
        dw      392	;G
        dw      440	;A
        dw      494	;B
        dw      524	;C

code	ends

	end  start_code