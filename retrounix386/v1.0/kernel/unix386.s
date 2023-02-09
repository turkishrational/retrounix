; ****************************************************************************
; UNIX386.ASM (RETRO UNIX 386 Kernel) - v0.2.0.22
; ----------------------------------------------------------------------------
; NASM version 2.15 (unix386.s)
;
; RETRO UNIX 386 (Retro Unix == Turkish Rational Unix)
; Operating System Project (v0.2) by ERDOGAN TAN (Beginning: 24/12/2013)
;
; Derived from 'Retro UNIX 8086 v1' source code by Erdogan Tan
; (v0.1 - Beginning: 11/07/2012)
;
; [ Last Modification: 24/07/2022 ] ; 2022 modification (previous: 12/7/2022)
;
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; Derived from 'UNIX v7/x86' source code by Robert Nordier (1999)
; UNIX V7/x86 source code: see www.nordier.com/v7x86 for details.
;
; ****************************************************************************
; 14/07/2022 - 24/07/2022 (v0.2.0.22)
; 12/07/2022 (v0.2.0.21)
; 15/05/2022 - 14/06/2022 (v0.2.0.20) 
; 04/02/2016 (v0.2.0.17) - 29/04/2022 (v0.2.0.18) - 09/05/2022 (v0.2.0.19)

; Assembler: NASM 2.15
;	nasm unix386.s -l unix386.lst -o unix386 -Z error.txt

; 24/12/2013

; Entering protected mode:
; Derived from 'simple_asm.txt' source code file and 
; 'The world of Protected mode' tutorial/article by Gregor Brunmar (2003)
; (gregor.brunmar@home.se)
; http://www.osdever.net/tutorials/view/the-world-of-protected-mode
;

; "The Real, Protected, Long mode assembly tutorial for PCs" 
; by Michael Chourdakis (2009) 
; http://www.codeproject.com/Articles/45788/
; http://www.michaelchourdakis.com
;

; Global Descriptor Table:
; Derived from 'head.s" source code of Linux v1.0 kernel
; by Linus Torvalds (1991-1992)
;

KLOAD	equ 10000h ; Kernel loading address
	; NOTE: Retro UNIX 8086 v1 /boot code loads kernel at 1000h:0000h 
KCODE	equ 08h	; Code segment descriptor (ring 0)
KDATA	equ 10h	; Data segment descriptor (ring 0)
; 19/03/2015
UCODE	equ 1Bh ; 18h + 3h  (ring 3)
UDATA	equ 23h ; 20h + 3h  (ring 3)
; 24/03/2015
TSS	equ 28h	; Task state segment descriptor (ring 0)
; 19/03/2015
CORE	equ 400000h  ; Start of USER's virtual/linear address space 
		     ; (at the end of the 1st 4MB)
ECORE	equ 0FFC00000h ; End of USER's virtual address space (4GB - 4MB)
		     ; ULIMIT = (ECORE/4096) - 1 = 0FFBFFh (in GDT)

; 27/12/2013
KEND    equ KLOAD + 65536 ; (28/12/2013) (end of kernel space)

; IBM PC/AT BIOS ----- 10/06/85 (postequ.inc)
;--------- CMOS TABLE LOCATION ADDRESS'S -------------------------------------
CMOS_SECONDS	EQU	00H		; SECONDS (BCD)
CMOS_MINUTES	EQU	02H		; MINUTES (BCD)	
CMOS_HOURS	EQU	04H		; HOURS (BCD)
CMOS_DAY_WEEK	EQU	06H		; DAY OF THE WEEK  (BCD)
CMOS_DAY_MONTH	EQU	07H		; DAY OF THE MONTH (BCD) 
CMOS_MONTH	EQU	08H		; MONTH (BCD)
CMOS_YEAR	EQU	09H		; YEAR (TWO DIGITS) (BCD)
CMOS_CENTURY	EQU	32H		; DATE CENTURY BYTE (BCD)
CMOS_REG_A	EQU	0AH		; STATUS REGISTER A
CMOS_REG_B	EQU	00BH		; STATUS REGISTER B  ALARM
CMOS_REG_C	EQU	00CH		; STATUS REGISTER C  FLAGS
CMOS_REG_D	EQU	0DH		; STATUS REGISTER D  BATTERY
CMOS_SHUT_DOWN	EQU	0FH		; SHUTDOWN STATUS COMMAND BYTE
;----------------------------------------
;	CMOS EQUATES FOR THIS SYSTEM	;
;-----------------------------------------------------------------------------
CMOS_PORT	EQU	070H		; I/O ADDRESS OF CMOS ADDRESS PORT
CMOS_DATA	EQU	071H		; I/O ADDRESS OF CMOS DATA PORT
NMI		EQU	10000000B	; DISABLE NMI INTERRUPTS MASK -
					; HIGH BIT OF CMOS LOCATION ADDRESS

; Memory Allocation Table Address
; 05/11/2014
; 31/10/2014
MEM_ALLOC_TBL	equ	100000h		; Memory Allocation Table at the end of
					; the 1st 1 MB memory space.
					; (This address must be aligned
					;  on 128 KB boundary, if it will be
					;  changed later.)
					; ((lower 17 bits of 32 bit M.A.T.
					;   address must be ZERO)).
					; ((((Reason: 32 bit allocation 
					;     instructions, dword steps)))
					; (((byte >> 12 --> page >> 5)))  
;04/11/2014	
PDE_A_PRESENT	equ	1		; Present flag for PDE
PDE_A_WRITE	equ 	2		; Writable (write permission) flag
PDE_A_USER	equ	4		; User (non-system/kernel) page flag
;
PTE_A_PRESENT	equ	1		; Present flag for PTE (bit 0)
PTE_A_WRITE	equ 	2		; Writable (write permission) flag (bit 1)
PTE_A_USER	equ	4		; User (non-system/kernel) page flag (bit 2)
PTE_A_ACCESS    equ	32		; Accessed flag (bit 5) ; 09/03/2015

; 17/02/2015 (unix386.s)
; 10/12/2014 - 30/12/2014 (0B000h -> 9000h) (dsectrm2.s)
DPT_SEGM equ 09000h  ; FDPT segment (EDD v1.1, EDD v3)
;
HD0_DPT	 equ 0	    ; Disk parameter table address for hd0
HD1_DPT	 equ 32	    ; Disk parameter table address for hd1
HD2_DPT	 equ 64	    ; Disk parameter table address for hd2
HD3_DPT	 equ 96	    ; Disk parameter table address for hd3


; FDPT (Phoenix, Enhanced Disk Drive Specification v1.1, v3.0)
;      (HDPT: Programmer's Guide to the AMIBIOS, 1993)
;
FDPT_CYLS	equ 0 ; 1 word, number of cylinders
FDPT_HDS	equ 2 ; 1 byte, number of heads
FDPT_TT		equ 3 ; 1 byte, A0h = translated FDPT with logical values
		      ; otherwise it is standard FDPT with physical values 	
FDPT_PCMP	equ 5 ; 1 word, starting write precompensation cylinder
		      ; (obsolete for IDE/ATA drives)
FDPT_CB		equ 8 ; 1 byte, drive control byte
			; Bits 7-6 : Enable or disable retries (00h = enable)
			; Bit 5	: 1 = Defect map is located at last cyl. + 1
			; Bit 4 : Reserved. Always 0
			; Bit 3 : Set to 1 if more than 8 heads
			; Bit 2-0 : Reserved. Alsways 0
FDPT_LZ		equ 12 ; 1 word, landing zone (obsolete for IDE/ATA drives)
FDPT_SPT	equ 14 ; 1 byte, sectors per track

; Floppy Drive Parameters Table (Programmer's Guide to the AMIBIOS, 1993)
; (11 bytes long) will be used by diskette handler/bios
; which is derived from IBM PC-AT BIOS (DISKETTE.ASM, 21/04/1986).						 	  		 	  

[BITS 16]       ; We need 16-bit intructions for Real mode

[ORG 0] 
	; 12/11/2014
	; Save boot drive number (that is default root drive)
	mov	[boot_drv], dl ; physical drv number

	; Determine installed memory
	; 31/10/2014
	;
	mov	ax, 0E801h ; Get memory size 
	int	15h	   ; for large configurations
	jnc	short chk_ms
	mov	ah, 88h    ; Get extended memory size 
	int	15h
	;	   
	;mov	al, 17h	; Extended memory (1K blocks) low byte
	;out	70h, al ; select CMOS register
	;in	al, 71h ; read data (1 byte)
	;mov	cl, al
	;mov	al, 18h ; Extended memory (1K blocks) high byte
	;out	70h, al ; select CMOS register
	;in	al, 71h ; read data (1 byte)
	;mov	ch, al
 	;      
	mov	cx, ax
	xor	dx, dx
chk_ms:
	mov	[mem_1m_1k], cx
	mov	[mem_16m_64k], dx
	; 05/11/2014
	;and	dx, dx
	;jz	short L2
        cmp     cx, 1024
	jnb	short L0
		 ; insufficient memory_error	
		 ; Minimum 2 MB memory is needed... 
	; 05/11/2014
	; (real mode error printing)
	sti
	mov	si, msg_out_of_memory
	mov	bx, 7
	mov	ah, 0Eh	; write tty
oom_1:
	lodsb
	or	al, al
	jz	short oom_2
	int	10h
	jmp	short oom_1
oom_2:
        hlt
	jmp	short oom_2

; 04/02/2022
; 05/11/2014
msg_out_of_memory:
	db 	07h, 0Dh, 0Ah
        db      'Insufficient memory !'
	db	0Dh, 0Ah
_int13h_48h_buffer: 
	; 04/02/2022 (Runix Kernel v0.2.0.18, 'diskinit.inc')
	db	'(Minimum 2MB memory is needed.)'
 	db	0Dh, 0Ah, 0

L0:
%include 'diskinit.inc' ; 07/03/2015

	; 10/11/2014
     	cli	; Disable interrupts (clear interrupt flag)
		; Reset Interrupt MASK Registers (Master&Slave)
	;mov	al, 0FFh	; mask off all interrupts
	;out	21h, al		; on master PIC (8259)
	;jmp 	$+2  ; (delay)
	;out	0A1h, al	; on slave PIC (8259)
	;
	; Disable NMI 
	mov   	al, 80h 
	out   	70h, al		; set bit 7 to 1 for disabling NMI
	; 23/02/2015
	nop			;
	;in	al, 71h		; read in 71h just after writing out to 70h
				; for preventing unknown state (!?)
	;
 	; 20/08/2014
	; Moving the kernel 64 KB back (to physical address 0)
	; DS = CS = 1000h
	; 05/11/2014
	xor	ax, ax
	mov	es, ax ; ES = 0
	;
	mov	cx, (KEND - KLOAD)/4
	xor	si, si
	xor	di, di
	rep	movsd
	;
	push	es ; 0
	push	L17
	retf
	;
L17:
	; Turn off the floppy drive motor
        mov     dx, 3F2h
        out     dx, al ; 0 ; 31/12/2013

	; Enable access to memory above one megabyte
L18:
	in	al, 64h
	test	al, 2
        jnz     short L18
	mov	al, 0D1h	; Write output port
	out	64h, al
L19:
	in	al, 64h
	test	al, 2
        jnz     short L19
	mov	al, 0DFh	; Enable A20 line
	out	60h, al
;L20:
	;
	; Load global descriptor table register

        ;mov     ax, cs
        ;mov     ds, ax

        lgdt    [cs:gdtd]

        mov     eax, cr0
	; or 	eax, 1
	inc     ax
	mov     cr0, eax

	; Jump to 32 bit code
	
	db 66h 			; Prefix for 32-bit
	db 0EAh 		; Opcode for far jump
	dd StartPM 		; Offset to start, 32-bit
				; (1000h:StartPM = StartPM + 10000h)
	dw KCODE		; This is the selector for CODE32_DESCRIPTOR,
				; assuming that StartPM resides in code32

[BITS 32] 

StartPM:
	; Kernel Base Address = 0 ; 30/12/2013
	mov ax, KDATA           ; Save data segment identifier
        mov ds, ax              ; Move a valid data segment into DS register
       	mov es, ax              ; Move data segment into ES register
       	mov fs, ax              ; Move data segment into FS register
      	mov gs, ax              ; Move data segment into GS register
        mov ss, ax              ; Move data segment into SS register
        mov esp, 90000h         ; Move the stack pointer to 090000h

clear_bss: ; Clear uninitialized data area
	; 11/03/2015
	xor	eax, eax ; 0
	;mov	ecx, (bss_end - bss_start)/4
	;;shr	ecx, 2 ; bss section is already aligned for double words
	; 27/02/2022
	mov	ecx, BSS_SIZE/4
	mov	edi, bss_start
	rep	stosd  	

memory_init:
	; Initialize memory allocation table and page tables
	; 16/11/2014
	; 15/11/2014
	; 07/11/2014
	; 06/11/2014
	; 05/11/2014
	; 04/11/2014
	; 31/10/2014 (Retro UNIX 386 v1 - Beginning) 
	;
;	xor	eax, eax
;	xor 	ecx, ecx
	mov	cl, 8
	mov	edi, MEM_ALLOC_TBL	
	rep	stosd		   ; clear Memory Allocation Table
				   ; for the first 1 MB memory
	;
	mov	cx, [mem_1m_1k]	   ; Number of contiguous KB between
				   ; 1 and 16 MB, max. 3C00h = 15 MB.
	shr	cx, 2		   ; convert 1 KB count to 4 KB count
	mov	[free_pages], ecx
	mov	dx, [mem_16m_64k]  ; Number of contiguous 64 KB blocks
				   ; between 16 MB and 4 GB.	
	or	dx, dx
	jz	short mi_0
	;
	mov	ax, dx
	shl	eax, 4		   ; 64 KB -> 4 KB (page count)
	add	[free_pages], eax
	add	eax, 4096	   ; 16 MB = 4096 pages
	jmp	short mi_1
mi_0:
	mov	ax, cx
	add	ax, 256		   ; add 256 pages for the first 1 MB		 
mi_1:
	mov	[memory_size], eax ; Total available memory in pages
				   ; 1 alloc. tbl. bit = 1 memory page
				   ; 32 allocation bits = 32 mem. pages   
	;
	add	eax, 32767	   ; 32768 memory pages per 1 M.A.T. page 	
	shr	eax, 15		   ; ((32768 * x) + y) pages (y < 32768)
				   ;  --> x + 1 M.A.T. pages, if y > 0
				   ;  --> x M.A.T. pages, if y = 0
	mov	[mat_size], ax	   ; Memory Alloc. Table Size in pages		
	shl	eax, 12		   ; 1 M.A.T. page = 4096 bytes
	;			   ; Max. 32 M.A.T. pages (4 GB memory)
	mov	ebx, eax	   ; M.A.T. size in bytes
	; Set/Calculate Kernel's Page Directory Address
	add	ebx, MEM_ALLOC_TBL
	mov	[k_page_dir], ebx  ; Kernel's Page Directory address
				   ; just after the last M.A.T. page
	;
	sub	eax, 4		   ; convert M.A.T. size to offset value
	mov	[last_page], eax   ; last page ofset in the M.A.T.
	;			   ; (allocation status search must be 
				   ; stopped after here)	
	xor	eax, eax
	dec	eax		   ; FFFFFFFFh (set all bits to 1)	
	push	cx
	shr	ecx, 5		   ; convert 1 - 16 MB page count to 
				   ; count of 32 allocation bits
	rep	stosd
	pop	cx
	inc	eax		   ; 0	
	and	cl, 31		   ; remain bits
	jz	short mi_4
	mov	[edi], eax	   ; reset	
mi_2:
	bts	[edi], eax	   ; 06/11/2014		
	dec	cl
	jz	short mi_3
	inc	al
	jmp	short mi_2
mi_3:
	sub	al, al	   	   ; 0
	add	edi, 4		   ; 15/11/2014
mi_4:
	or	dx, dx		  ; check 16M to 4G memory space	
	jz	short mi_6	  ; max. 16 MB memory, no more...
	;	
	mov	ecx, MEM_ALLOC_TBL + 512 ; End of first 16 MB memory
	;	
	sub	ecx, edi	  ; displacement (to end of 16 MB)
	jz	short mi_5	  ; jump if EDI points to 
				  ;         end of first 16 MB	
	shr	ecx, 1		  ; convert to dword count
	shr	ecx, 1		  ; (shift 2 bits right) 
	rep 	stosd		  ; reset all bits for reserved pages
				  ; (memory hole under 16 MB)
mi_5:
	mov	cx, dx		  ; count of 64 KB memory blocks
	shr	ecx, 1		  ; 1 alloc. dword per 128 KB memory
	pushf			  ; 16/11/2014		
	dec	eax		  ; FFFFFFFFh (set all bits to 1)
	rep	stosd
	inc	eax		  ; 0
	popf			  ; 16/11/2014
	jnc	short mi_6
	dec	ax		  ; eax = 0000FFFFh
	stosd
	inc	ax		  ; 0		
mi_6:
	cmp	edi, ebx	  ; check if EDI points to 	
	jnb	short mi_7	  ; end of memory allocation table
	;			  ; (>= MEM_ALLOC_TBL + 4906) 
	mov	ecx, ebx	  ; end of memory allocation table
	sub	ecx, edi	  ; convert displacement/offset
	shr	ecx, 1		  ; to dword count 	 		
	shr	ecx, 1		  ; (shift 2 bits right) 
	rep 	stosd		  ; reset all remain M.A.T. bits
mi_7:
	; Reset M.A.T. bits in M.A.T. (allocate M.A.T. pages)
	mov	edx, MEM_ALLOC_TBL
	;sub	ebx, edx	  ; Mem. Alloc. Tbl. size in bytes
	;shr	ebx, 12		  ; Mem. Alloc. Tbl. size in pages	
	mov	cx, [mat_size]	  ; Mem. Alloc. Tbl. size in pages
	mov	edi, edx
	shr	edi, 15		  ; convert M.A.T. address to
				  ; byte offset in M.A.T.
				  ; (1 M.A.T. byte points to 
				  ;	      32768 bytes)
				  ; Note: MEM_ALLOC_TBL address 
				  ; must be aligned on 128 KB 
				  ; boundary!
	add	edi, edx	  ; points to M.A.T.'s itself	
	; eax = 0
	sub	[free_pages], ecx ; 07/11/2014
mi_8:
	btr	[edi], eax	  ; clear bit 0 to bit x (1 to 31)
	;dec	bl
	dec	cl
	jz	short mi_9
	inc	al
	jmp	short mi_8
mi_9:
	;
	; Reset Kernel's Page Dir. and Page Table bits in M.A.T.
	;		(allocate pages for system page tables)

	; edx = MEM_ALLOC_TBL
	mov	ecx, [memory_size] ; memory size in pages (PTEs)
	add	ecx, 1023	 ; round up (1024 PTEs per table)	 	
	shr	ecx, 10		 ; convert memory page count to 
				 ; page table count (PDE count)
	;
	push	ecx		 ; (**) PDE count (<= 1024)
	;
	inc	ecx		 ; +1 for kernel page directory	
	;
	sub	[free_pages], ecx ; 07/11/2014
	;
	mov	esi, [k_page_dir] ; Kernel's Page Directory address
	shr	esi, 12		 ; convert to page number
mi_10:
	mov	eax, esi	 ; allocation bit offset		 
	mov	ebx, eax
	shr	ebx, 3		 ; convert to alloc. byte offset
	and	bl,  0FCh	 ; clear bit 0 and bit 1
				 ;   to align on dword boundary
	and	eax, 31		 ; set allocation bit position 
				 ;  (bit 0 to bit 31)
	;
	add	ebx, edx	 ; offset in M.A.T. + M.A.T. address 
	;
	btr 	[ebx], eax	 ; reset relevant bit (0 to 31)
	;
	inc	esi		 ; next page table
	loop	mi_10		 ; allocate next kernel page table 
				 ; (ecx = page table count + 1)		
	;
	pop	ecx		 ; (**) PDE count (= pg. tbl. count)
	;
	; Initialize Kernel Page Directory and Kernel Page Tables
	;
	; Initialize Kernel's Page Directory
	mov	edi, [k_page_dir]
	mov	eax, edi
	or	al, PDE_A_PRESENT + PDE_A_WRITE
		     	      ; supervisor + read&write + present
	mov	edx, ecx 	; (**) PDE count (= pg. tbl. count)	
mi_11:
	add	eax, 4096	; Add page size (PGSZ)
			        ; EAX points to next page table
	stosd
	loop	mi_11
	sub	eax, eax	; Empty PDE
	mov	cx, 1024	; Entry count (PGSZ/4)
	sub	ecx, edx
	jz	short mi_12
	rep	stosd 		; clear remain (empty) PDEs
	;
	; Initialization of Kernel's Page Directory is OK, here.
mi_12:
	; Initialize Kernel's Page Tables
	;
	; (EDI points to address of page table 0)
	; eax = 0
	mov	ecx, [memory_size] ; memory size in pages
	mov	edx, ecx	; (***)
	mov	al, PTE_A_PRESENT + PTE_A_WRITE
			     ; supervisor + read&write + present 	
mi_13:
	stosd
	add	eax, 4096	
	loop	mi_13	
	and	dx, 1023	; (***)
	jz	short mi_14
	mov	cx, 1024	
	sub	cx, dx		; from dx (<= 1023) to 1024
	xor	eax, eax
	rep	stosd		; clear remain (empty) PTEs 
				; of the last page table
mi_14:
	;  Initialization of Kernel's Page Tables is OK, here.
	;
	mov	eax, edi	; end of the last page table page
			        ; (beginging of user space pages)
	shr	eax, 15		; convert to M.A.T. byte offset
	and	al, 0FCh	; clear bit 0 and bit 1 for
				; aligning on dword boundary	
	 
	mov	[first_page], eax
	mov	[next_page], eax ; The first free page pointer
				 ; for user programs
				 ; (Offset in Mem. Alloc. Tbl.)	
	;
	; Linear/FLAT (1 to 1) memory paging for the kernel is OK, here.
	;
	
	; Enable paging
	;
        mov     eax, [k_page_dir]
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h	; set paging bit (bit 31)
	mov	cr0, eax
        ;jmp    KCODE:StartPMP

	db 0EAh 		; Opcode for far jump
        dd StartPMP		; 32 bit offset
	dw KCODE		; kernel code segment descriptor


StartPMP:
	; 06/11//2014
	; Clear video page 0
	;
	; Temporary Code
	;
	mov	ecx, 80*25/2
	mov	edi, 0B8000h
	xor	eax, eax	; black background, black fore color
	rep	stosd
	
	; 19/08/2014
	; Kernel Base Address = 0
	; It is mapped to (physically) 0 in the page table.
	; So, here is exactly 'StartPMP' address.
	;
	;;mov	ah, 4Eh	; Red background, yellow forecolor
	;;mov	esi, msgPM
	;; 14/08/2015 (kernel version message will appear
	;;	       when protected mode and paging is enabled)
	mov	ah, 0Bh ; Black background, light cyan forecolor
	mov	esi, msgKVER
	mov	edi, 0B8000h ; 27/08/2014
	; 20/08/2014
	call	printk

	; 'UNIX v7/x86' source code by Robert Nordier (1999)
	; // Set IRQ offsets
	;
	;  Linux (v0.12) source code by Linus Torvalds (1991)
	;
					;; ICW1
	mov	al, 11h			; Initialization sequence
	out	20h, al			; 	8259A-1
	; jmp 	$+2
	out	0A0h, al		; 	8259A-2
					;; ICW2
	mov	al, 20h			; Start of hardware ints (20h)
	out	21h, al			;	for 8259A-1
	; jmp 	$+2
	mov	al, 28h			; Start of hardware ints (28h)
	out	0A1h, al		; 	for 8259A-2
					;
	mov	al, 04h			;; ICW3
	out	21h, al			; 	IRQ2 of 8259A-1 (master)
	; jmp 	$+2
	mov	al, 02h			; 	is 8259A-2 (slave)
	out	0A1h, al		;
					;; ICW4
	mov	al, 01h	 		;
	out	21h, al			; 	8086 mode, normal EOI	
	; jmp 	$+2
	out	0A1h, al		;	for both chips.

	;mov	al, 0FFh	; mask off all interrupts for now
	;out	21h, al
	;; jmp 	$+2
	;out	0A1h, al

	; 02/04/2015
	; 26/03/2015 System call (INT 30h) modification
	;  DPL = 3 (Interrupt service routine can be called from user mode)			
	;
	;; Linux (v0.12) source code by Linus Torvalds (1991)
	;  setup_idt:
	;
        ;; 16/02/2015
	;;mov	dword [DISKETTE_INT], fdc_int ; IRQ 6 handler
	; 21/08/2014 (timer_int)
	mov	esi, ilist
	lea	edi, [idt]
	; 26/03/2015
	mov	ecx, 48		; 48 hardware interrupts (INT 0 to INT 2Fh)
	; 02/04/2015
	mov	ebx, 80000h
rp_sidt1:
	lodsd
	mov	edx, eax
	mov	dx, 8E00h
	mov	bx, ax
	mov	eax, ebx	; /* selector = 0x0008 = cs */
       			        ; /* interrupt gate - dpl=0, present */
	stosd	; selector & offset bits 0-15 	
	mov	eax, edx
	stosd	; attributes & offset bits 16-23
	loop	rp_sidt1
	mov	cl, 16        ; 16 software interrupts (INT 30h to INT 3Fh)
rp_sidt2:
	lodsd
	and	eax, eax
	jz	short rp_sidt3
	mov	edx, eax
	mov	dx, 0EE00h	; P=1b/DPL=11b/01110b
	mov	bx, ax
	mov	eax, ebx	; selector & offset bits 0-15 	
	stosd
	mov	eax, edx
	stosd
	loop	rp_sidt2
	jmp	short sidt_OK
rp_sidt3:
	mov	eax, ignore_int
	mov	edx, eax
	mov	dx, 0EE00h	; P=1b/DPL=11b/01110b
	mov	bx, ax
	mov	eax, ebx	; selector & offset bits 0-15 	
rp_sidt4:
	stosd
	xchg	eax, edx
	stosd
	xchg	edx, eax
	loop	rp_sidt4
sidt_OK: 
	lidt 	[idtd]
	;
	; TSS descriptor setup ; 24/03/2015
	mov	eax, task_state_segment
	mov	[gdt_tss0], ax
	rol	eax, 16
	mov	[gdt_tss1], al
	mov	[gdt_tss2], ah
	mov	word [tss.IOPB], tss_end - task_state_segment
		; 
		; IO Map Base address (When this address points
		; to end of the TSS, CPU does not use IO port 
		; permission bit map for RING 3 IO permissions, 
		; access to any IO ports in ring 3 will be forbidden.)
 		;
	;mov	[tss.esp0], esp ; TSS offset 4
	;mov	word [tss.ss0], KDATA ; TSS offset 8 (SS)
   	mov	ax, TSS  ; It is needed when an interrupt 
			 ; occurs (or a system call -software INT- is requested)
			 ; while cpu running in ring 3 (in user mode).				
			 ; (Kernel stack pointer and segment will be loaded
			 ; from offset 4 and 8 of the TSS, by the CPU.)	 
	ltr	ax  ; Load task register
	;
esp0_set0:
	; 30/07/2015
	mov 	ecx, [memory_size] ; memory size in pages
	shl 	ecx, 12 ; convert page count to byte count
	cmp	ecx, CORE ; beginning of user's memory space (400000h)
			  ; (kernel mode virtual address)
	jna	short esp0_set1
	;
	; If available memory > CORE (end of the 1st 4 MB)
	; set stack pointer to CORE
	;(Because, PDE 0 is reserved for kernel space in user's page directory)
	;(PDE 0 points to page table of the 1st 4 MB virtual address space)
	mov	ecx, CORE
esp0_set1:
	mov	esp, ecx ; top of kernel stack (**tss.esp0**)
esp0_set_ok:
	; 30/07/2015 (**tss.esp0**) 
	mov	[tss.esp0], esp
        mov     word [tss.ss0], KDATA
	; 14/08/2015
	; 10/11/2014 (Retro UNIX 386 v1 - Erdogan Tan)
	;
	;cli	; Disable interrupts (for CPU)
	;    (CPU will not handle hardware interrupts, except NMI!)
	;
	xor	al, al		; Enable all hardware interrupts!
	out	21h, al		; (IBM PC-AT compatibility)
	jmp 	$+2		; (All conventional PC-AT hardware
	out	0A1h, al	;  interrupts will be in use.)	
				; (Even if related hardware component
				;  does not exist!)
	; Enable NMI 
	mov	al, 7Fh		; Clear bit 7 to enable NMI (again)
	out  	70h, al
	; 23/02/2015
	nop
	in	al, 71h		; read in 71h just after writing out to 70h
				; for preventing unknown state (!?)
	;
	; Only a NMI can occur here... (Before a 'STI' instruction)
	;
	; 02/09/2014
	;xor	bx, bx
	;mov	dx, 0200h	; Row 2, column 0  ; 07/03/2015
	; 27/02/2022	
	xor	ebx, ebx
	xor	edx, edx
	mov	dh, 2
	call	set_cpos
	;
	; 06/11/2014
	; Temporary Code
	;
	call	memory_info
	; 14/08/2015
	;call getch ; 28/02/2015
drv_init:
	sti	; Enable Interrupts 
	; 06/02/2015
	mov	edx, [hd0_type] ; hd0, hd1, hd2, hd3
	mov	bx, [fd0_type] ; fd0, fd1
	; 22/02/2015
	and	bx, bx
	jnz	short di1
	;
	or 	edx, edx
	jnz	short di2
	;
setup_error:
	mov 	esi, setup_error_msg
psem:	
	lodsb
	or	al, al
	;jz	short haltx ; 22/02/2015
	jz	short di3
	push	esi
	xor	ebx, ebx ; 0
			; Video page 0 (bl=0)
	mov	ah, 07h ; Black background, 
			; light gray forecolor
	call	write_tty
	pop	esi
	jmp	short psem

di1:
	; supress 'jmp short T6'
	;  (activate fdc motor control code)
	mov	word [T5], 9090h ; nop
	;
	;mov	ax, int_0Eh	; IRQ 6 handler
	;mov	di, 0Eh*4	; IRQ 6 vector
	;stosw
	;mov 	ax, cs
	;stosw
	;; 16/02/2015
        ;;mov	dword [DISKETTE_INT], fdc_int ; IRQ 6 handler
	;
	CALL	DSKETTE_SETUP	; Initialize Floppy Disks
	;
	or	edx, edx
        jz      short di3
di2:
	call   	DISK_SETUP	; Initialize Fixed Disks
        jc      short setup_error
di3:
	call	setup_rtc_int	; 22/05/2015 (dsectrpm.s)
	;
	call	display_disks ; 07/03/2015  (Temporary)
;haltx:
	; 14/08/2015
	;call	getch ; 22/02/2015
	sti	; Enable interrupts (for CPU)
	; 14/08/2015
	;mov 	ecx, 0FFFFFFFh
	; 16/02/2022
	mov 	ecx, 04FFFFFh
md_info_msg_wait:
	push 	ecx
	mov	al, 1
	mov 	ah, [ptty] ; active (current) video page
	call	getc_n
	pop	ecx
	jnz	short md_info_msg_ok
	loop	md_info_msg_wait
md_info_msg_ok:
	; 30/06/2015
	call	sys_init
	;
	;jmp 	cpu_reset ; 22/02/2015
hang:
	; 27/02/2022
	sub	eax, eax
_hang: 
	; 23/02/2015
	;sti			; Enable interrupts
	hlt
	;
	;nop
	;; 03/12/2014
	;; 28/08/2014
	;mov	ah, 11h
	;call	getc
	;jz      _c8
	;
	; 23/02/2015
	; 06/02/2015
	; 07/09/2014
	xor	ebx, ebx
	mov	bl, [ptty]	; active_page
	mov	esi, ebx
	;shl 	si, 1
	; 17/07/2022
	shl	esi, 1
	add	esi, ttychr
	mov	ax, [esi]
	;and	ax, ax
	;;jz	short _c8
	;jz	short hang
	; 27/02/2022
	and	eax, eax
	jz	short _hang
	mov	word [esi], 0
	cmp	bl, 3		; Video page 3
	;jb	short _c8
	jb	short hang
	;	
	; 02/09/2014
	mov	ah, 0Eh		; Yellow character 
				; on black background
	; 27/02/2022 (32 bit reg push-pop)
	; 07/09/2014
nxtl:
	;push	bx
	push	ebx
	;
	;xor	ebx, ebx	; bl = 0 (video page 0)
				; bh = 0 (video mode)
				; Retro UNIX 386 v1 - Video Mode 0
				; (PC/AT Video Mode 3 - 80x25 Alpha.)
	push	eax
	call 	write_tty
	pop	eax
	;pop	bx
	pop	ebx
	cmp	al, 0Dh		; carriage return (enter)
	;jne	short _c8
	jne	short hang
	mov	al, 0Ah		; next line
	jmp	short nxtl
	
;_c8:
;	; 25/08/2014
;	cli				; Disable interrupts
;	mov	al, [scounter + 1]
;	and	al, al
;	jnz	hang
;	call	rtc_p
;	jmp     hang


	; 27/08/2014
	; 20/08/2014
printk:
        ;mov    edi, [scr_row]
pkl:
	lodsb
	or 	al, al
	jz	short pkr
	stosw
	jmp	short pkl
pkr:
	retn

; 25/07/2015
; 14/05/2015 (multi tasking -time sharing- 'clock', x_timer)
; 17/02/2015
; 06/02/2015 (unix386.s)
; 11/12/2014 - 22/12/2014 (dsectrm2.s) 
;
; IBM PC-XT Model 286 Source Code - BIOS2.ASM (06/10/85)
;
;-- HARDWARE INT  08 H - ( IRQ LEVEL 0 ) ---------------------------------------
;	THIS ROUTINE HANDLES THE TIMER INTERRUPT FROM FROM CHANNEL 0 OF        :
;	THE 8254 TIMER.  INPUT FREQUENCY IS 1.19318 MHZ AND THE DIVISOR        :
;	IS 65536, RESULTING IN APPROXIMATELY 18.2 INTERRUPTS EVERY SECOND.     :
;									       :
;	THE INTERRUPT HANDLER MAINTAINS A COUNT (40:6C) OF INTERRUPTS SINCE    :
;	POWER ON TIME, WHICH MAY BE USED TO ESTABLISH TIME OF DAY.	       :
;	THE INTERRUPT HANDLER ALSO DECREMENTS THE MOTOR CONTROL COUNT (40:40)  :
;	OF THE DISKETTE, AND WHEN IT EXPIRES, WILL TURN OFF THE 	       :
;	DISKETTE MOTOR(s), AND RESET THE MOTOR RUNNING FLAGS.		       :
;	THE INTERRUPT HANDLER WILL ALSO INVOKE A USER ROUTINE THROUGH	       :
;	INTERRUPT 1CH AT EVERY TIME TICK.  THE USER MUST CODE A 	       :
;	ROUTINE AND PLACE THE CORRECT ADDRESS IN THE VECTOR TABLE.	       :
;-------------------------------------------------------------------------------
;

timer_int:	; IRQ 0
;int_08h:	; Timer
	; 14/10/2015
	; Here, we are simulating system call entry (for task switch)
	; (If multitasking is enabled, 
	; 'clock' procedure may jump to 'sysrelease')
	push	ds
	push	es
	push	fs
	push	gs
	pushad  ; eax, ecx, edx, ebx, esp -before pushad-, ebp, esi, edi
	mov     cx, KDATA
        mov     ds, cx
        mov     es, cx
        mov     fs, cx
        mov     gs, cx
	;
	mov	ecx, cr3
	mov	[cr3reg], ecx ; save current cr3 register value/content
	;
	cmp 	ecx, [k_page_dir]
	je	short T3
	;
	; timer interrupt has been occurred while OS is in user mode
	mov 	[u.r0], eax
	mov	ecx, esp
	add	ecx, ESPACE ; 4 * 12 (stack frame)	
	mov	[u.sp], ecx ; kernel stack pointer at the start of interrupt
	mov	[u.usp], esp ; kernel stack points to user's registers   
	;
	mov	ecx, [k_page_dir]
	mov	cr3, ecx
T3:
	sti				; INTERRUPTS BACK ON
	INC	word [TIMER_LOW]	; INCREMENT TIME
	JNZ	short T4		; GO TO TEST_DAY
	INC	word [TIMER_HIGH]	; INCREMENT HIGH WORD OF TIME
T4:					; TEST_DAY
	CMP	word [TIMER_HIGH],018H	; TEST FOR COUNT EQUALING 24 HOURS
	JNZ	short T5		; GO TO DISKETTE_CTL
	CMP	word [TIMER_LOW],0B0H
	JNZ	short T5		; GO TO DISKETTE_CTL

;-----	TIMER HAS GONE 24 HOURS
	;;SUB	AX,AX
	;MOV	[TIMER_HIGH],AX
	;MOV	[TIMER_LOW],AX
	sub	eax, eax
	mov	[TIMER_LH], eax
	;	
	MOV	byte [TIMER_OFL],1

;-----	TEST FOR DISKETTE TIME OUT

T5:
	; 23/12/2014
	jmp	short T6		; will be replaced with nop, nop
					; (9090h) if a floppy disk
					; is detected.
	;mov	al,[CS:MOTOR_COUNT]
	mov	al, [MOTOR_COUNT]
	dec	al
	;mov	[CS:MOTOR_COUNT], al	; DECREMENT DISKETTE MOTOR CONTROL
	mov	[MOTOR_COUNT], al
	;mov	[ORG_MOTOR_COUNT], al
	JNZ	short T6		; RETURN IF COUNT NOT OUT
	mov 	al,0F0h
	;AND	[CS:MOTOR_STATUS],al 	; TURN OFF MOTOR RUNNING BITS
	and	[MOTOR_STATUS], al
	;and	[ORG_MOTOR_STATUS], al
	MOV	AL,0CH			; bit 3 = enable IRQ & DMA, 
					; bit 2 = enable controller
					;	1 = normal operation
					;	0 = reset	
					; bit 0, 1 = drive select
					; bit 4-7 = motor running bits 
	MOV	DX,03F2H		; FDC CTL PORT
	OUT	DX,AL			; TURN OFF THE MOTOR
T6:	
	;inc	word [CS:wait_count]	; 22/12/2014 (byte -> word)
					; TIMER TICK INTERRUPT
	;;inc	word [wait_count] ;;27/02/2015
	;INT	1CH			; TRANSFER CONTROL TO A USER ROUTINE
	;;;;cli
	;call 	u_timer			; TRANSFER CONTROL TO A USER ROUTINE
	call	[x_timer] ; 14/05/2015
T7:
	; 14/10/2015
	MOV	AL,EOI			; GET END OF INTERRUPT MASK
	CLI				; DISABLE INTERRUPTS TILL STACK CLEARED
	OUT	INTA00,AL		; END OF INTERRUPT TO 8259 - 1	
	;
	mov 	eax, [cr3reg] 		; previous value/content of cr3 register
 	mov	cr3, eax  ; restore cr3 register content
	;
	popad ; edi, esi, ebp, temp (icrement esp by 4), ebx, edx, ecx, eax
	;
	pop	gs
	pop	fs
	pop	es
	pop	ds
	iretd	; return from interrupt


; ////////////////

; 14/05/2015 - Multi tasking 'clock' procedure (sys emt)
x_timer:
	dd 	u_timer	; (temporary demo code)	; 14/05/2015
	;dd	clock

; 23/02/2022 - Real time clock (digital) output demo (sys emt)
x_rtci:
	dd	rtc_p	; (temporary demo code)	; 23/02/2022

; 14/10/2015
cr3reg: dd 0

	; 06/02/2015
	; 07/09/2014
	; 21/08/2014
u_timer:
	; 27/02/2022
	; 12/02/2022
;timer_int:	; IRQ 0
	; 06/02/2015
	;push	eax
	;push	edx
	;push	ecx
	;push	ebx
	;push	ds
	;push	es
	;mov	eax, KDATA
	;mov	ds, ax
	;mov	es, ax
	inc	dword [tcount]
	mov	ebx, tcountstr + 4
	;mov	ax, [tcount]
	; 27/02/2022
	mov	eax, [tcount]
	;mov	ecx, 10
	xor	ecx, ecx
	mov	cl, 10
rp_divtcnt:
	xor	edx, edx
	div	ecx
	add	dl, 30h
	mov	[ebx], dl
	;or	ax, ax
	; 27/02/2022
	or	eax, eax
	jz	short print_lzero
	dec	ebx
	jmp	short rp_divtcnt
print_lzero:
	cmp	ebx, tcountstr
	jna	short print_tcount
	dec	ebx
 	mov	byte [ebx], 30h
	jmp	short print_lzero
print_tcount:
	push	esi
	push	edi
	mov	esi, timer_msg ; Timer interrupt message
	;; 07/09/2014
	;mov	bx, 1		; Video page 1
	; 12/02/2022
	;mov	bx, 6		; Video page 6
	; 27/02/2022
	sub	ebx, ebx
	mov	bl, 6	; Video page 6
ptmsg:
	lodsb
	or	al, al
	jz	short ptmsg_ok
	push	esi
	;push	bx
	; 27/02/2022
        push	ebx
        mov     ah,  2Fh ; Green background, white forecolor
	call 	write_tty
	;pop	bx
	; 27/02/2022
	pop	ebx
	pop	esi
	jmp	short ptmsg
	;; 27/08/2014
	;mov	edi, 0B8000h + 0A0h ; Row 1
	;call	printk
	;
ptmsg_ok:
	; 07/09/2014
	;xor	dx, dx		; column 0, row 0
	; 27/02/2022
	xor	edx, edx
	call	set_cpos	; set cursor position to 0,0 
	; 23/02/2015
	; 25/08/2014
	;mov	ebx, scounter		; (seconds counter)
	;dec	byte [ebx+1]		; (for reading real time clock)
;	dec	byte [scounter+1]
;;	jns	short timer_eoi		; 0 -> 0FFh ?
;	jns	short u_timer_retn
	; 26/02/2015
;	call	rtc_p
;	mov	ebx, scounter		; (seconds counter)
;	mov	byte [ebx+1], 18	; (18.2 timer ticks per second)
;	dec 	byte [ebx]		; 19+18+18+18+18 (5)	
;	jnz	short timer_eoi		; (109 timer ticks in 5 seconds)
;	jnz	short u_timer_retn ; 06/02/2015
;	mov	byte [ebx], 5
;	inc	byte [ebx+1] ; 19
;;timer_eoi:
;;	mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
;;	out	20h, al	; 8259 PORT
	;
;u_timer_retn:  ; 06/02/2015
	pop	edi
	pop	esi
	;pop	es
	;pop	ds
	;pop	ebx
	;pop	ecx
	;pop	edx
	;pop	eax
	;iret
	retn	; 06/02/2015

	; 28/08/2014
irq0:
        push 	dword 0
	jmp	short which_irq
irq1:
        push 	dword 1
	jmp	short which_irq
irq2:
        push 	dword 2
	jmp	short which_irq
irq3:
	; 20/11/2015
	; 24/10/2015
	call	dword [cs:com2_irq3]
	push 	dword 3
	jmp	short which_irq
irq4:
	; 20/11/2015
	; 24/10/2015
	call	dword [cs:com1_irq4]
        push 	dword 4
	jmp	short which_irq
irq5:
        push 	dword 5
	jmp	short which_irq
irq6:
        push 	dword 6
	jmp	short which_irq
irq7:
        push 	dword 7
	jmp	short which_irq
irq8:
        push 	dword 8
	jmp	short which_irq
irq9:
        push 	dword 9
	jmp	short which_irq
irq10:
        push 	dword 10
	jmp	short which_irq
irq11:
        push 	dword 11
	jmp	short which_irq
irq12:
        push 	dword 12
	jmp	short which_irq
irq13:
        push 	dword 13
	jmp	short which_irq
irq14:
        push 	dword 14
	jmp	short which_irq
irq15:
        push 	dword 15
	;jmp	short which_irq

	; 27/02/2022
	; 19/10/2015
	; 29/08/2014
	; 21/08/2014
which_irq:
	xchg	eax, [esp]  ; 28/08/2014
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	;
	mov	bl, al
	;
	mov	eax, KDATA
	mov	ds, ax
	mov	es, ax
	; 19/10/2015
	cld
        ; 27/08/2014
        add     dword [scr_row], 0A0h
	;
	mov	ah, 17h	; blue (1) background, 
			; light gray (7) forecolor
        mov     edi, [scr_row]
	mov	al, 'I'
	stosw
	mov	al, 'R'
	stosw
	mov	al, 'Q'
	stosw
	mov	al, ' '
	stosw
	mov	al, bl
	cmp	al, 10
	jb	short iix
	mov	al, '1'
	stosw
	mov	al, bl
	sub	al, 10
iix:
	add	al, '0'
	stosw
	mov	al, ' '
	stosw
	mov	al, '!'
	stosw
	mov	al, ' '
	stosw
	; 23/02/2015
	cmp	bl, 7 ; check for IRQ 8 to IRQ 15 
	;jna	iiret
	; 27/02/2022
	jna	short iiz
iiy:
	mov	al, 20h  ; END OF INTERRUPT COMMAND TO
	out	0A0h, al ; the 2nd 8259
iiz:
	jmp     iiret
	;
	; 22/08/2014
	;mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
	;out	20h, al	; 8259 PORT
	;
	;pop	es
	;pop	ds
	;pop	edi
	;pop	esi
	;pop	ebx
	;pop 	eax
	;iret

	; 02/04/2015
	; 25/08/2014
exc0:
        push 	dword 0
        jmp     cpu_except
exc1:
        push 	dword 1
        jmp     cpu_except
exc2:
        push 	dword 2
        jmp     cpu_except
exc3:
        push 	dword 3
        jmp     cpu_except
exc4:
        push 	dword 4
        jmp     cpu_except
exc5:
        push 	dword 5
        jmp     cpu_except
exc6:
        push 	dword 6
        jmp     cpu_except
exc7:
        push 	dword 7
        jmp     cpu_except
exc8:
	; [esp] = Error code
        push 	dword 8
        jmp     cpu_except_en
exc9:
        push 	dword 9
        jmp     cpu_except
exc10:
	; [esp] = Error code
        push 	dword 10
        jmp     cpu_except_en
exc11:
	; [esp] = Error code
        push 	dword 11
        jmp     cpu_except_en
exc12:
	; [esp] = Error code
        push 	dword 12
        jmp     cpu_except_en
exc13:
	; [esp] = Error code
        push 	dword 13
        jmp     cpu_except_en
exc14:
	; [esp] = Error code
        push 	dword 14
	jmp	short cpu_except_en
exc15:
        push 	dword 15
        jmp     cpu_except
exc16:
        push 	dword 16
        jmp     cpu_except
exc17:
	; [esp] = Error code
        push 	dword 17
	jmp	short cpu_except_en
exc18:
        push 	dword 18
	jmp	short cpu_except
exc19:
        push 	dword 19
	jmp	short cpu_except
exc20:
        push 	dword 20
	jmp	short cpu_except
exc21:
        push 	dword 21
	jmp	short cpu_except
exc22:
        push 	dword 22
	jmp	short cpu_except
exc23:
        push 	dword 23
	jmp	short cpu_except
exc24:
        push 	dword 24
	jmp	short cpu_except
exc25:
        push 	dword 25
	jmp	short cpu_except
exc26:
        push 	dword 26
	jmp	short cpu_except
exc27:
        push 	dword 27
	jmp	short cpu_except
exc28:
        push 	dword 28
	jmp	short cpu_except
exc29:
        push 	dword 29
	jmp	short cpu_except
exc30:
        push 	dword 30
	jmp	short cpu_except_en
exc31:
        push 	dword 31
        jmp     short cpu_except

	; 27/02/2022
	; 19/10/2015
	; 19/09/2015
	; 01/09/2015
	; 28/08/2015
	; 28/08/2014
cpu_except_en:
	xchg	eax, [esp+4] ; Error code
	mov	[ss:error_code], eax
	pop	eax  ; Exception number
	xchg	eax, [esp]
		; eax = eax before exception
		; [esp] -> exception number
		; [esp+4] -> EIP to return
	; 19/10/2015
	; 19/09/2015
	; 01/09/2015
	; 28/08/2015
	; 29/08/2014
	; 28/08/2014
	; 25/08/2014
	; 21/08/2014
cpu_except:	; CPU Exceptions
	cld
	xchg	eax, [esp] 
		; eax = Exception number
		; [esp] = eax (before exception)	
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	; 28/08/2015
	mov	bx, KDATA
	mov	ds, bx
	mov	es, bx
	mov	ebx, cr3
	push	ebx ; (*) page directory
	; 19/10/2015
	cld
	; 25/03/2015
	mov	ebx, [k_page_dir]
	mov	cr3, ebx
	; 28/08/2015
	cmp	eax, 0Eh ; 14, PAGE FAULT	
	jne	short cpu_except_nfp
	call	page_fault_handler
	and 	eax, eax
        ;jz	iiretp ; 01/09/2015
	; 27/02/2022
	jnz	short cpu_except_pf
	jmp	iiretp
cpu_except_pf:
	mov	eax, 0Eh ; 14
cpu_except_nfp:
	; 02/04/2015
	mov	ebx, hang
	xchg	ebx, [esp+28]
		; EIP (points to instruction which faults)
	  	; New EIP (hang)
	mov	[FaultOffset], ebx
	mov	dword [esp+32], KCODE ; kernel's code segment
	or	dword [esp+36], 200h ; enable interrupts (set IF)
	;
	mov	ah, al
	and	al, 0Fh
	cmp	al, 9
	jna	short h1ok
	add	al, 'A'-':'
h1ok:
	shr	ah, 1
	shr	ah, 1
	shr	ah, 1
	shr	ah, 1
	cmp	ah, 9
	jna	short h2ok
	add	ah, 'A'-':'
h2ok:	
	xchg 	ah, al	
	add	ax, '00'
	mov	[excnstr], ax
	;
	; 29/08/2014
	mov	eax, [FaultOffset]
	push	ecx
	push	edx
	mov	ebx, esp
	; 28/08/2015
	mov	ecx, 16	  ; divisor value to convert binary number
			  ; to hexadecimal string
	;mov	ecx, 10	    ; divisor to convert	
			    ; binary number to decimal string
b2d1:
	xor	edx, edx
	div	ecx
	;push	dx
	; 27/02/2022
	push	edx
	cmp	eax, ecx
	jnb	short b2d1
	mov	edi, EIPstr ; EIP value
			    ; points to instruction which faults	
	; 28/08/2015
	mov	edx, eax
b2d2:
	;add	al, '0'
	mov	al, [edx+hexchrs]
	stosb		    ; write hexadecimal digit to its place	
	cmp	ebx, esp
	jna	short b2d3
	; 27/02/2022
	pop	eax
	;pop	ax
	mov	dl, al
	jmp	short b2d2
b2d3:
	mov 	al, 'h' ; 28/08/2015
	stosb
	mov	al, 20h	    ; space
	stosb
	xor	al, al	    ; to do it an ASCIIZ string	
	stosb
	;
	pop	edx
	pop	ecx
	;
	mov	ah, 4Fh	; red (4) background, 
			; white (F) forecolor
	mov	esi, exc_msg ; message offset
	;
	jmp	short piemsg
	;
        ;add    dword [scr_row], 0A0h
        ;mov    edi, [scr_row]
        ;
	;call 	printk
	;
	;mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
	;out	20h, al	; 8259 PORT
	;
	;pop	es
	;pop	ds
	;pop	edi
	;pop	esi
	;pop 	eax
	;iret
	
	; 28/08/2015
	; 23/02/2015
	; 20/08/2014
ignore_int:
	push	eax
	push	ebx ; 23/02/2015
	push	esi
	push	edi
	push 	ds
	push 	es
	; 28/08/2015
	mov	eax, cr3
	push	eax ; (*) page directory
	;
	mov	ah, 67h	; brown (6) background, 
			; light gray (7) forecolor
	mov	esi, int_msg ; message offset
piemsg:
        ; 27/08/2014
        add     dword [scr_row], 0A0h
        mov     edi, [scr_row]
        ;
	call 	printk
	;
	; 23/02/2015
	mov	al, 20h  ; END OF INTERRUPT COMMAND TO
	out	0A0h, al ; the 2nd 8259
iiretp: ; 01/09/2015
	; 28/08/2015
	pop	eax ; (*) page directory
	mov	cr3, eax
	;
iiret:
	; 22/08/2014
	mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
	out	20h, al	; 8259 PORT
	;
	pop	es
	pop	ds
	pop	edi
	pop	esi
	pop	ebx ; 29/08/2014
	pop 	eax
	iretd

	; 23/02/2022
	; 26/02/2015
	; 07/09/2014
	; 25/08/2014
rtc_int:       ; Real Time Clock Interrupt (IRQ 8)
	; 22/08/2014
	push	eax
	push	ebx ; 29/08/2014
	push	esi
	push	edi
	push 	ds
	push 	es
	;
	mov	eax, KDATA
	mov	ds, ax
	mov	es, ax
	;
	; 25/08/2014
	;call	rtc_p
	; 23/02/2022
	call	[x_rtci]
	;
	; 22/02/2015 - dsectpm.s
	; [ source: http://wiki.osdev.org/RTC ]
	; read status register C to complete procedure
	;(it is needed to get a next IRQ 8) 
	mov	al, 0Ch ; 
	out	70h, al ; select register C
	nop
	in	al, 71h ; just throw away contents
	; 22/02/2015
	MOV	AL,EOI		; END OF INTERRUPT
	OUT	INTB00,AL	; FOR CONTROLLER #2
	;
	jmp	short iiret	

	; 22/08/2014
	; IBM PC/AT BIOS source code ----- 10/06/85 (bios.asm)
	; (INT 1Ah)
	;; Linux (v0.12) source code (main.c) by Linus Torvalds (1991)
time_of_day:
	call	UPD_IPR			; WAIT TILL UPDATE NOT IN PROGRESS
        jc      short rtc_retn 
	mov	al, CMOS_SECONDS
	call	CMOS_READ
	mov	[time_seconds], al 
	mov	al, CMOS_MINUTES
	call	CMOS_READ
	mov	[time_minutes], al 
	mov	al, CMOS_HOURS
	call	CMOS_READ
        mov     [time_hours], al
	mov	al, CMOS_DAY_WEEK 
	call	CMOS_READ
	mov	[date_wday], al
 	mov	al, CMOS_DAY_MONTH
	call	CMOS_READ
	mov	[date_day], al
	mov	al, CMOS_MONTH
	call	CMOS_READ
	mov	[date_month], al
	mov	al, CMOS_YEAR
	call	CMOS_READ
	mov	[date_year], al
	mov	al, CMOS_CENTURY
	call	CMOS_READ
	mov	[date_century], al
	;
	mov	al, CMOS_SECONDS
	call 	CMOS_READ
	cmp	al, [time_seconds]
	jne	short time_of_day

rtc_retn:
	retn

rtci_default:
	; 23/02/2022 (Temporary!)
	; (default real time clock handler in multitasking mode)
	; ((2 rtc ticks per second after 'setup_rtc_int'))
	inc	dword [rtc_ticks] ; real time clock counter
			; (not used in anywhere of kernel for now!)
	retn

rtc_p:	
	; 27/02/2022
	; 12/02/2022
	; 07/09/2014
	; 29/08/2014
	; 27/08/2014
	; 25/08/2014
 	; Print Real Time Clock content
	;
	;
	call	time_of_day
	jc	short rtc_retn
	;
	cmp	al, [ptime_seconds]
        je      short rtc_retn ; 29/08/2014
	;
	mov	[ptime_seconds], al
	;
	mov	al, [date_century]
	call	bcd_to_ascii
	mov	[datestr+6], ax
	mov	al, [date_year]
	call	bcd_to_ascii
	mov	[datestr+8], ax
	mov	al, [date_month]
	call	bcd_to_ascii
	mov	[datestr+3], ax
	mov	al, [date_day]
	call	bcd_to_ascii
	mov	[datestr], ax
	;
	movzx	ebx, byte [date_wday]
	shl 	bl, 2
	add	ebx, daytmp
	mov	eax, [ebx]
	mov	[daystr], eax
	;
	mov	al, [time_hours]
	call	bcd_to_ascii
	mov	[timestr], ax
	mov	al, [time_minutes]
	call	bcd_to_ascii
	mov	[timestr+3], ax
	mov	al, [time_seconds]
	call	bcd_to_ascii
	mov	[timestr+6], ax
	;		
	mov	esi, rtc_msg ; message offset
	; 23/02/2015
	push	edx
	push	ecx
	; 07/09/2014
	;mov	bx, 2	; Video page 2
	; 27/02/2022
	sub	ebx, ebx
	mov	bl, 7	; Video page 7
prtmsg:
	lodsb
	or	al, al
	jz	short prtmsg_ok
	push	esi
	; 27/02/2022
	push	ebx
	;push	bx
        mov	ah, 3Fh	; cyan (6) background, 
			; white (F) forecolor
	call 	write_tty
	;pop	bx
	; 27/02/2022
	pop	ebx
	pop	esi
	jmp	short prtmsg
	;
	;mov	edi, 0B8000h+0A0h+0A0h ; Row 2
	;call	printk
prtmsg_ok:
	; 07/09/2014
	;xor	dx, dx		; column 0, row 0
	; 27/02/2022
	xor	edx, edx
	call	set_cpos	; set cursor position to 0,0 
	; 23/02/2015
	pop	ecx
	pop	edx
	retn

; Default IRQ 7 handler against spurious IRQs (from master PIC)
; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
default_irq7:
	; 27/02/2022
	;push	ax
	push	eax
	mov	al, 0Bh  ; In-Service register
	out	20h, al
        jmp short $+2
	jmp short $+2
	in	al, 20h
	and 	al, 80h ; bit 7 (is it real IRQ 7 or fake?)
        jz      short irq7_iret ; Fake (spurious) IRQ, do not send EOI 
        mov     al, 20h ; EOI
	out	20h, al 
irq7_iret:
	;pop	ax
	; 27/02/2022
	pop	eax
	iretd
	
	; 22/08/2014
	; IBM PC/AT BIOS source code ----- 10/06/85 (test4.asm)
CMOS_READ:
	pushf		; SAVE INTERRUPT ENABLE STATUS AND FLAGS
	rol	al, 1	; MOVE NMI BIT TO LOW POSITION
	stc		; FORCE NMI BIT ON IN CARRY FLAG
	rcr	al, 1	; HIGH BIT ON TO DISABLE NMI - OLD IN CY
	cli		; DISABLE INTERRUPTS
	out	CMOS_PORT, al	; ADDRESS LOCATION AND DISABLE NMI
	nop		; I/O DELAY
	in	al, CMOS_DATA	; READ THE REQUESTED CMOS LOCATION
	push	ax	; SAVE (AH) REGISTER VALUE AND CMOS BYTE
	; 15/03/2015 ; IBM PC/XT Model 286 BIOS source code 
		     ; ----- 10/06/85 (test4.asm)
	mov	al, CMOS_SHUT_DOWN*2 ; GET ADDRESS OF DEFAULT LOCATION
	;mov	al, CMOS_REG_D*2 ; GET ADDRESS OF DEFAULT LOCATION
	rcr	al, 1	; PUT ORIGINAL NMI MASK BIT INTO ADDRESS
	out	CMOS_PORT, al	; SET DEFAULT TO READ ONLY REGISTER
	pop	ax	; RESTORE (AH) AND (AL), CMOS BYTE
	popf	
	retn		; RETURN WITH FLAGS RESTORED

	; 22/08/2014
	; IBM PC/AT BIOS source code ----- 10/06/85 (bios2.asm)
UPD_IPR:				; WAIT TILL UPDATE NOT IN PROGRESS
	push	ecx
	mov	ecx, 65535		; SET TIMEOUT LOOP COUNT (= 800)
		; mov cx, 800	
UPD_10:
	mov	al, CMOS_REG_A		; ADDRESS STATUS REGISTER A
	cli				; NO TIMER INTERRUPTS DURING UPDATES
	call	CMOS_READ		; READ UPDATE IN PROCESS FLAG
	test	al, 80h			; IF UIP BIT IS ON ( CANNOT READ TIME )
	jz	short UPD_90		; EXIT WITH CY= 0 IF CAN READ CLOCK NOW
	sti				; ALLOW INTERRUPTS WHILE WAITING
	loop	UPD_10			; LOOP TILL READY OR TIMEOUT
	xor	eax, eax		; CLEAR RESULTS IF ERROR
		; xor ax, ax
	stc				; SET CARRY FOR ERROR
UPD_90:
	pop	ecx			; RESTORE CALLERS REGISTER
	cli				; INTERRUPTS OFF DURING SET
	retn				; RETURN WITH CY FLAG SET

bcd_to_ascii:
	; 25/08/2014
	; INPUT ->
	;	al = Packed BCD number
	; OUTPUT ->
	;	ax  = ASCII word/number
	;
	; Erdogan Tan - 1998 (proc_hex) - TRDOS.ASM (2004-2011)
	;
	db 0D4h,10h                     ; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
	or ax,'00'                      ; Make it ASCII based

        xchg ah, al 
	
	retn	
	
%include 'keyboard.inc' ; 07/03/2015

%include 'video.inc' ; 07/03/2015

setup_rtc_int:
; source: http://wiki.osdev.org/RTC
	cli		; disable interrupts
	; default int frequency is 1024 Hz (Lower 4 bits of register A is 0110b or 6)
	; in order to change this ...
	; frequency  = 32768 >> (rate-1) --> 32768 >> 5 = 1024
	; (rate must be above 2 and not over 15)
	; new rate = 15 --> 32768 >> (15-1) = 2 Hz
	mov	al, 8Ah 
	out	70h, al ; set index to register A, disable NMI
	nop
	in	al, 71h ; get initial value of register A
	mov 	ah, al
	and	ah, 0F0h
	mov	al, 8Ah 
	out	70h, al ; reset index to register A
	mov	al, ah
	or	al, 0Fh	; new rate (0Fh -> 15)
	out	71h, al ; write only our rate to A. Note, rate is the bottom 4 bits. 
	; enable RTC interrupt
	mov	al, 8Bh ;
	out	70h, al ; select register B and disable NMI
	nop
	in	al, 71h ; read the current value of register B
	mov	ah, al  ;
	mov 	al, 8Bh ;
	out	70h, al ; set the index again (a read will reset the index to register B)	
	mov	al, ah  ;
	or	al, 40h ;
	out	71h, al ; write the previous value ORed with 0x40. This turns on bit 6 of register B
	sti
	retn

; Write memory information
; Temporary Code
; 06/11/2014
; 14/08/2015 
memory_info:	
	mov	eax, [memory_size] ; in pages
	push	eax
	shl	eax, 12		   ; in bytes
	mov	ebx, 10
	mov	ecx, ebx	   ; 10
	mov	esi, mem_total_b_str	
	call	bintdstr
	pop	eax
	mov	cl, 7
	mov	esi, mem_total_p_str
	call	bintdstr	
	; 14/08/2015
	call	calc_free_mem
	; edx = calculated free pages
	; ecx = 0
	mov 	eax, [free_pages]
	cmp	eax, edx ; calculated free mem value 
		; and initial free mem value are same or not?
	jne 	short pmim ; print mem info with '?' if not
	push 	edx ; free memory in pages	
	;mov 	eax, edx
	shl	eax, 12 ; convert page count
			; to byte count
	mov	cl, 10
	mov	esi, free_mem_b_str
	call	bintdstr
	pop	eax
	mov	cl, 7
	mov	esi, free_mem_p_str
	call	bintdstr
pmim:
	mov	esi, msg_memory_info
pmim_nb:	
	lodsb
	or	al, al
	jz	short pmim_ok
	push	esi
	xor	ebx, ebx ; 0
			; Video page 0 (bl=0)
	mov	ah, 07h ; Black background, 
			; light gray forecolor
	call	write_tty
	pop	esi
	jmp	short pmim_nb
pmim_ok:
	retn

; Convert binary number to hexadecimal string
; 10/05/2015  
; dsectpm.s (28/02/2015)
; Retro UNIX 386 v1 - Kernel v0.2.0.6  
; 01/12/2014
; 25/11/2014
;
bytetohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	ebx
	xor	ebx, ebx
	mov	bl, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs] 
	pop	ebx	
	retn

wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
	;
	push	ebx
	xor	ebx, ebx
	xchg	ah, al
	push	ax
	mov	bl, ah
	shr	bl, 4
	mov	al, [ebx+hexchrs] 	 	
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	shl	eax, 16
	pop	ax
	pop	ebx
	jmp	short bytetohex
	;mov	bl, al
	;shr	bl, 4
	;mov	bl, [ebx+hexchrs] 	 	
	;xchg	bl, al	 	
	;and	bl, 0Fh
	;mov	ah, [ebx+hexchrs] 
	;pop	ebx	
	;retn

dwordtohex:
	; INPUT ->
	; 	EAX = dword (binary number)
	; OUTPUT ->
	;	EDX:EAX = hexadecimal string
	;
	push	eax
	shr	eax, 16
	call	wordtohex
	mov	edx, eax
	pop	eax
	call	wordtohex
	retn

; 10/05/2015
hex_digits:
hexchrs:
	db '0123456789ABCDEF'

; Convert binary number to decimal/numeric string
; 06/11/2014
; Temporary Code
;

bintdstr:
	; EAX = binary number
	; ESI = decimal/numeric string address
	; EBX = divisor (10)
	; ECX = string length (<=10)
	add	esi, ecx
btdstr0:
	dec	esi
	xor	edx, edx
	div	ebx
	add	dl, 30h
	mov	[esi], dl
	dec	cl
	jz	btdstr2
	or	eax, eax
	jnz	short btdstr0
btdstr1:
	dec	esi
        mov     byte [esi], 20h ; blank space
	dec	cl
	jnz	short btdstr1
btdstr2:
	retn

; Calculate free memory pages on M.A.T.
; 06/11/2014
; Temporary Code
;

calc_free_mem:
	xor	edx, edx
	;xor	ecx, ecx
	mov	cx, [mat_size] ; in pages
	shl	ecx, 10	; 1024 dwords per page
	mov	esi, MEM_ALLOC_TBL
cfm0:
	lodsd
	push	ecx
	mov	ecx, 32
cfm1:
	shr	eax, 1
	jnc	short cfm2
	inc	edx
cfm2:
	loop	cfm1
	pop	ecx
	loop	cfm0
	retn

%include 'diskio.inc'  ; 07/03/2015
%include 'memory.inc'  ; 09/03/2015
%include 'sysdefs.inc' ; 09/03/2015
%include 'u0.s'        ; 15/03/2015
%include 'u1.s'        ; 10/05/2015
%include 'u2.s'        ; 11/05/2015
%include 'u3.s'        ; 10/05/2015
%include 'u4.s'        ; 15/04/2015
%include 'u5.s'        ; 03/06/2015
%include 'u6.s'        ; 31/05/2015
%include 'u7.s'        ; 18/04/2015
%include 'u8.s'        ; 11/06/2015
%include 'u9.s'        ; 29/06/2015

; 07/03/2015
; Temporary Code
display_disks:
	cmp 	byte [fd0_type], 0
	jna 	short ddsks1
	call	pdskm
ddsks1:
	cmp	byte [fd1_type], 0
	jna	short ddsks2
	mov	byte [dskx], '1'
	call	pdskm
ddsks2:
	cmp	byte [hd0_type], 0
	jna	short ddsk6
	mov	word [dsktype], 'hd'
	mov	byte [dskx], '0'
	call	pdskm
ddsks3:
	cmp	byte [hd1_type], 0
	jna	short ddsk6
	mov	byte [dskx], '1'
	call	pdskm
ddsks4:
	cmp	byte [hd2_type], 0
	jna	short ddsk6
	mov	byte [dskx], '2'
	call	pdskm
ddsks5:
	cmp	byte [hd3_type], 0
	jna	short ddsk6
	mov	byte [dskx], '3'
	call	pdskm
ddsk6:
	mov	esi, nextline
	call	pdskml
pdskm_ok:
	retn
pdskm:
	mov	esi, dsk_ready_msg
pdskml:	
	lodsb
	or	al, al
	jz	short pdskm_ok
	push	esi
	xor	ebx, ebx ; 0
			; Video page 0 (bl=0)
	mov	ah, 07h ; Black background, 
			; light gray forecolor
	call	write_tty
	pop	esi
	jmp	short pdskml

align 16

gdt:	; Global Descriptor Table
	; (30/07/2015, conforming cs)
	; (26/03/2015)
	; (24/03/2015, tss)
	; (19/03/2015)
	; (29/12/2013)
	;
	dw 0, 0, 0, 0		; NULL descriptor
	; 18/08/2014
			; 8h kernel code segment, base = 00000000h		
	dw 0FFFFh, 0, 9A00h, 00CFh	; KCODE
			; 10h kernel data segment, base = 00000000h	
	dw 0FFFFh, 0, 9200h, 00CFh	; KDATA
			; 1Bh user code segment, base address = 400000h ; CORE
	dw 0FBFFh, 0, 0FA40h, 00CFh	; UCODE 
			; 23h user data segment, base address = 400000h ; CORE
	dw 0FBFFh, 0, 0F240h, 00CFh	; UDATA
			; Task State Segment
	dw 0067h ; Limit = 103 ; (104-1, tss size = 104 byte, 
			       ;  no IO permission in ring 3)
gdt_tss0:
	dw 0  ; TSS base address, bits 0-15 
gdt_tss1:
	db 0  ; TSS base address, bits 16-23 
	      		; 49h	
	db 11101001b ; E9h => P=1/DPL=11/0/1/0/B/1 --> B = Task is busy (1)
	db 0 ; G/0/0/AVL/LIMIT=0000 ; (Limit bits 16-19 = 0000) (G=0, 1 byte)
gdt_tss2:
	db 0  ; TSS base address, bits 24-31 

gdt_end:
	;; 9Ah = 1001 1010b (GDT byte 5) P=1/DPL=00/1/TYPE=1010, 
					;; Type= 1 (code)/C=0/R=1/A=0
		; P= Present, DPL=0=ring 0,  1= user (0= system)
		; 1= Code C= non-Conforming, R= Readable, A = Accessed

	;; 92h = 1001 0010b (GDT byte 5) P=1/DPL=00/1/TYPE=1010, 
					;; Type= 0 (data)/E=0/W=1/A=0
		; P= Present, DPL=0=ring 0,  1= user (0= system)
		; 0= Data E= Expansion direction (1= down, 0= up)
		; W= Writeable, A= Accessed
	
	;; FAh = 1111 1010b (GDT byte 5) P=1/DPL=11/1/TYPE=1010, 
					;; Type= 1 (code)/C=0/R=1/A=0
		; P= Present, DPL=3=ring 3,  1= user (0= system)
		; 1= Code C= non-Conforming, R= Readable, A = Accessed

	;; F2h = 1111 0010b (GDT byte 5) P=1/DPL=11/1/TYPE=0010, 
					;; Type= 0 (data)/E=0/W=1/A=0
		; P= Present, DPL=3=ring 3,  1= user (0= system)
		; 0= Data E= Expansion direction (1= down, 0= up)
	
	;; CFh = 1100 1111b (GDT byte 6) G=1/B=1/0/AVL=0, Limit=1111b (3)

		;; Limit = FFFFFh (=> FFFFFh+1= 100000h) // bits 0-15, 48-51 //
		;	 = 100000h * 1000h (G=1) = 4GB
		;; Limit = FFBFFh (=> FFBFFh+1= FFC00h) // bits 0-15, 48-51 //
		;	 = FFC00h * 1000h (G=1) = 4GB - 4MB
		; G= Granularity (1= 4KB), B= Big (32 bit), 
		; AVL= Available to programmers	

gdtd:
        dw gdt_end - gdt - 1    ; Limit (size)
        dd gdt			; Address of the GDT

	; 20/08/2014
idtd:
        dw idt_end - idt - 1    ; Limit (size)
        dd idt			; Address of the IDT

Align 4

	; 21/08/2014
ilist:
	;times 	32 dd cpu_except ; INT 0 to INT 1Fh
	;
	; Exception list
	; 25/08/2014	
	dd	exc0	; 0h,  Divide-by-zero Error
	dd	exc1	
	dd 	exc2	
	dd	exc3	
	dd	exc4	
	dd	exc5	
	dd 	exc6	; 06h,  Invalid Opcode
	dd	exc7	
	dd	exc8	
	dd	exc9	
	dd 	exc10	
	dd	exc11
	dd	exc12
	dd	exc13	; 0Dh, General Protection Fault
	dd 	exc14	; 0Eh, Page Fault
	dd	exc15
	dd	exc16
	dd	exc17
	dd 	exc18
	dd	exc19
	dd 	exc20
	dd	exc21
	dd	exc22
	dd	exc23
	dd 	exc24
	dd	exc25
	dd	exc26
	dd	exc27
	dd 	exc28
	dd	exc29
	dd 	exc30
	dd	exc31
	; Interrupt list
	dd	timer_int	; INT 20h
		;dd	irq0	
	dd	keyb_int	; 27/08/2014
		;dd	irq1
	dd	irq2
		; COM2 int
	dd	irq3
		; COM1 int
	dd	irq4
	dd	irq5
;DISKETTE_INT: ;06/02/2015
	dd	fdc_int		; 16/02/2015, IRQ 6 handler	
		;dd	irq6
; Default IRQ 7 handler against spurious IRQs (from master PIC)
; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
	dd	default_irq7	; 25/02/2015
		;dd	irq7
; Real Time Clock Interrupt
	dd	rtc_int		; 23/02/2015, IRQ 8 handler
		;dd	irq8	; INT 28h
	dd	irq9
	dd	irq10
	dd	irq11
	dd	irq12
	dd	irq13
;HDISK_INT1:  ;06/02/2015 	
	dd	hdc1_int 	; 21/02/2015, IRQ 14 handler		
		;dd	irq14
;HDISK_INT2:  ;06/02/2015
	dd	hdc2_int 	; 21/02/2015, IRQ 15 handler		
		;dd	irq15	; INT 2Fh
		; 14/08/2015
	dd	sysent		; INT 30h (system calls)
	
	;dd	ignore_int
	dd	0

;;;
;;; 11/03/2015
%include 'kybdata.inc'	; KEYBOARD (BIOS) DATA
%include 'vidata.inc'	; VIDEO (BIOS) DATA
%include 'diskdata.inc'	; DISK (BIOS) DATA (initialized)
;;;

Align 2

; 12/11/2014 (Retro UNIX 386 v1)
boot_drv:    db 0 ; boot drive number (physical)
; 24/11/2014
drv:	     db 0 
last_drv:    db 0 ; last hdd
hdc:         db 0  ; number of hard disk drives
		     ; (present/detected)
;
; 24/11/2014 (Retro UNIX 386 v1)
; Physical drive type & flags
fd0_type:    db 0  ; floppy drive type
fd1_type:    db 0  ; 4 = 1.44 Mb, 80 track, 3.5" (18 spt)
		     ; 6 = 2.88 Mb, 80 track, 3.5" (36 spt)
		     ; 3 = 720 Kb, 80 track, 3.5" (9 spt)
		     ; 2 = 1.2 Mb, 80 track, 5.25" (15 spt)
		     ; 1 = 360 Kb, 40 track, 5.25" (9 spt)		
hd0_type:    db 0  ; EDD status for hd0 (bit 7 = present flag)
hd1_type:    db 0  ; EDD status for hd1 (bit 7 = present flag)
hd2_type:    db 0  ; EDD status for hd2 (bit 7 = present flag)
hd3_type:    db 0  ; EDD status for hd3 (bit 7 = present flag)
		     ; bit 0 - Fixed disk access subset supported
		     ; bit 1 - Drive locking and ejecting
		     ; bit 2 - Enhanced disk drive support
		     ; bit 3 = Reserved (64 bit EDD support)
		     ; (If bit 0 is '1' Retro UNIX 386 v1
		     ; will interpret it as 'LBA ready'!)		

; 12/07/2022
; (drv.cylinders, drv.spt, drv.spt will not be used now on)
; ('diskio.inc')
; ((spt and heads and cylinder counts will be taken from DPT))

; 11/03/2015 - 10/07/2015
;drv.cylinders: dw 0,0,0,0,0,0,0
;drv.heads:     dw 0,0,0,0,0,0,0
;drv.spt:       dw 0,0,0,0,0,0,0
; 12/07/2022 - 11/03/2015
drv.size:      dd 0,0,0,0,0,0,0
drv.status:    db 0,0,0,0,0,0,0
drv.error:     db 0,0,0,0,0,0,0
;

; 27/08/2014
scr_row:
	dd 0B8000h + 0A0h + 0A0h + 0A0h ; Row 3
scr_col:
	dd 0

;; 14/08/2015
;;msgPM:
;;      db "Protected mode and paging are ENABLED ... ", 0
msgKVER:
	;;;;;db "Retro UNIX 386 v1 - Kernel v0.2.0.17 [04/02/2016]", 0
	;;;;db "Retro UNIX 386 v1 - Kernel v0.2.0.18 [29/04/2022]", 0	
	;;;db "Retro UNIX 386 v1 - Kernel v0.2.0.19 [02/06/2022]", 0
	;;db "Retro UNIX 386 v1 - Kernel v0.2.0.20 [14/06/2022]", 0
	;db "Retro UNIX 386 v1 - Kernel v0.2.0.21 [12/07/2022]", 0
	db "Retro UNIX 386 v1 - Kernel v0.2.0.22 [24/07/2022]", 0

Align 2

; 20/08/2014
  ; /* This is the default interrupt "handler" :-) */ 
  ; Linux v0.12 (head.s)
int_msg:
	db "Unknown interrupt ! ", 0

Align 2  

; 21/08/2014
timer_msg:
	db "IRQ 0 (INT 20h) ! Timer Interrupt : "
tcountstr:
	db "00000 "
	db 0

Align 2
	; 21/08/2014
exc_msg:
	db "CPU exception ! "
excnstr: 		; 25/08/2014
	db "??h", "  EIP : "
EIPstr: ; 29/08/2014
	times 12 db 0
rtc_msg:
	db "Real Time Clock - "
datestr:
	db "00/00/0000"
	db " "
daystr:
	db "DAY "
timestr:	
        db "00:00:00"
	db " "
	db 0 

daytmp:
	; 28/02/2015
	db "??? SUN MON TUE WED THU FRI SAT "

ptime_seconds: db 0FFh

	; 23/02/2015
	; 25/08/2014
;scounter:
;	db 5
;	db 19

;; 05/11/2014
;msg_out_of_memory:
;	db 	07h, 0Dh, 0Ah
;	db      'Insufficient memory ! '
;	db	'(Minimum 2 MB memory is needed.)'
; 	db	0Dh, 0Ah, 0
	;
setup_error_msg:
	db 0Dh, 0Ah
	db 'Disk Setup Error!' 
	db 0Dh, 0Ah,0

; 02/09/2014 (Retro UNIX 386 v1)
;crt_ulc : db 0 ; upper left column (for scroll) 
;	  db 0 ; upper left row (for scroll)	

;crt_lrc : db 79 ; lower right column (for scroll) 
;	  db 24 ; lower right row (for scroll)


; 06/11/2014 (Temporary Data)
; Memory Information message
; 14/08/2015
msg_memory_info:
	db	07h
	db	0Dh, 0Ah
	;db 	"MEMORY ALLOCATION INFO", 0Dh, 0Ah, 0Dh, 0Ah
	db	"Total memory : "
mem_total_b_str: ; 10 digits
	db	"0000000000 bytes", 0Dh, 0Ah
	db	"               ", 20h, 20h, 20h
mem_total_p_str: ; 7 digits
	db	"0000000 pages", 0Dh, 0Ah
	db 	0Dh, 0Ah
	db	"Free memory  : "
free_mem_b_str:  ; 10 digits
	db	"?????????? bytes", 0Dh, 0Ah
	db	"               ", 20h, 20h, 20h
free_mem_p_str:  ; 7 digits
	db	"??????? pages", 0Dh, 0Ah
	db	0Dh, 0Ah, 0

dsk_ready_msg:
	db 	0Dh, 0Ah
dsktype:
	db	'fd'
dskx:
	db	'0'
	db	20h
	db 	'is READY ...'
	db 	0
nextline:
	db 	0Dh, 0Ah, 0

; KERNEL - SYSINIT Messages
; 24/08/2015
; 13/04/2015 - (Retro UNIX 386 v1 Beginning)
; 14/07/2013
;kernel_init_err_msg:
;	db 0Dh, 0Ah
;	db 07h
;	db 'Kernel initialization ERROR !'
;	db 0Dh, 0Ah, 0 
; 24/08/2015
;;; (temporary kernel init message has been removed
;;;  from 'sys_init' code)
;kernel_init_ok_msg: 
;	db 0Dh, 0Ah
;	db 07h
;	db 'Welcome to Retro UNIX 386 v1 Operating System !'
;	db 0Dh, 0Ah
;       db 'by Erdogan Tan - 04/02/2016 (v0.2.0.17)'
;	db 0Dh, 0Ah, 0
panic_msg:
	db 0Dh, 0Ah, 07h
	db 'ERROR: Kernel Panic !'
	db 0Dh, 0Ah, 0
etc_init_err_msg:
	db 0Dh, 0Ah
	db 07h
	db 'ERROR: /etc/init !?'
	db 0Dh, 0Ah, 0

; 10/05/2015
badsys_msg:
	db 0Dh, 0Ah
	db 07h
	db 'Invalid System Call !'
	db 0Dh, 0Ah
	db 'EAX: '
bsys_msg_eax:
	db '00000000h'
	db 0Dh, 0Ah
	db 'EIP: '
bsys_msg_eip:
	db '00000000h' 
	db 0Dh, 0Ah, 0

BSYS_M_SIZE equ $ - badsys_msg


align 2

; EPOCH Variables
; 13/04/2015 - Retro UNIX 386 v1 Beginning
; 09/04/2013 epoch variables
; Retro UNIX 8086 v1 Prototype: UNIXCOPY.ASM, 10/03/2013
;
year: 	dw 1970
;month: dw 1
;day: 	dw 1
;hour: 	dw 0
;minute: dw 0
;second: dw 0
; 02/06/2022
month:	db 1
day:	db 1
hour:	db 1
minute: db 1
second:	db 1
	db 1

DMonth:
	dw 0
	dw 31
	dw 59
	dw 90
	dw 120
	dw 151
	dw 181
	dw 212
	dw 243
	dw 273
	dw 304
	dw 334

; 04/11/2014 (Retro UNIX 386 v1)
mem_1m_1k:   dw 0  ; Number of contiguous KB between
                     ; 1 and 16 MB, max. 3C00h = 15 MB.
mem_16m_64k: dw 0  ; Number of contiguous 64 KB blocks
		   ;   between 16 MB and 4 GB.

align 16

bss_start:

ABSOLUTE bss_start

	; 11/03/2015
	; Interrupt Descriptor Table (20/08/2014)
idt:
	resb	64*8 ; INT 0 to INT 3Fh
idt_end:

;alignb 4

task_state_segment:
	; 24/03/2015
tss.link:   resw 1
	    resw 1
; tss offset 4	
tss.esp0:   resd 1
tss.ss0:    resw 1
	    resw 1	
tss.esp1:   resd 1
tss.ss1:    resw 1
	    resw 1 	
tss.esp2:   resd 1
tss.ss2:    resw 1
	    resw 1
; tss offset 28
tss.CR3:    resd 1
tss.eip:    resd 1
tss.eflags: resd 1
; tss offset 40
tss.eax:    resd 1		 		
tss.ecx:    resd 1
tss.edx:    resd 1
tss.ebx:    resd 1
tss.esp:    resd 1
tss.ebp:    resd 1
tss.esi:    resd 1
tss.edi:    resd 1
; tss offset 72
tss.ES:     resw 1
	    resw 1	
tss.CS:	    resw 1
	    resw 1
tss.SS:	    resw 1
	    resw 1
tss.DS:	    resw 1
	    resw 1
tss.FS:	    resw 1
	    resw 1
tss.GS:	    resw 1
	    resw 1		
tss.LDTR:   resw 1
	    resw 1
; tss offset 100		
	    resw 1		
tss.IOPB:   resw 1
; tss offset 104 
tss_end:

k_page_dir:  resd 1 ; Kernel's (System) Page Directory address
		    ; (Physical address = Virtual address)	 	
memory_size: resd 1 ; memory size in pages
free_pages:  resd 1 ; number of free pages		
next_page:   resd 1 ; offset value in M.A.T. for
		    ; first free page search
last_page:   resd 1 ; offset value in M.A.T. which
		    ; next free page search will be
		    ; stopped after it. (end of M.A.T.)
first_page:  resd 1 ; offset value in M.A.T. which
		    ; first free page search
		    ; will be started on it. (for user)
mat_size:    resd 1 ; Memory Allocation Table size in pages		

;;;
; 02/09/2014 (Retro UNIX 386 v1)
; 04/12/2013 (Retro UNIX 8086 v1)
CRT_START:   resw 1 ; starting address in regen buffer
		    ; NOTE: active page only
cursor_posn: resw 8 ; cursor positions for video pages
active_page: 
ptty: 	     resb 1 ; current tty
; 01/07/2015
ccolor:	     resb 1 ; current color attributes ('sysmsg')	
; 26/10/2015
; 07/09/2014
ttychr:      resw ntty+2 ; Character buffer (multiscreen)

; 21/08/2014
tcount:	     resd 1

; 18/05/2015 (03/06/2013 - Retro UNIX 8086 v1 feature only!)
p_time:      resd 1 ; present time (for systime & sysmdate)

; 18/05/2015 (16/08/2013 - Retro UNIX 8086 v1 feature only !)
; (open mode locks for pseudo TTYs)
; [ major tty locks (return error in any conflicts) ]
ttyl:        resw ntty+2 ; opening locks for TTYs.

; 15/04/2015 (Retro UNIX 386 v1)
; 22/09/2013 (Retro UNIX 8086 v1)
wlist:       resb ntty+2 ; wait channel list (0 to 9 for TTYs)
; 15/04/2015 (Retro UNIX 386 v1)
;; 12/07/2014 -> sp_init set comm. parameters as 0E3h
;; 0 means serial port is not available 
;;comprm: ; 25/06/2014
com1p:       resb 1 ;;0E3h
com2p:       resb 1 ;;0E3h

; 17/11/2015
; request for response (from the terminal)	
req_resp:     resw 1 			
; 07/11/2015
ccomport:    resb 1 ; current COM (serial) port
		    ; (0= COM1, 1= COM2)
; 09/11/2015
comqr:	     resb 1 ; 'query or response' sign (u9.s, 'sndc')
; 07/11/2015
rchar:	     resw 1 ; last received char for COM 1 and COM 2		
schar:	     resw 1 ; last sent char for COM 1 and COM 2

; 23/10/2015
; SERIAL PORTS - COMMUNICATION MODES
; (Retro UNIX 386 v1 feature only!)
; 0 - command mode (default/initial mode)
; 1 - terminal mode (Retro UNIX 386 v1 terminal, ascii chars)
;;; communication modes for future versions:  
; // 2 - keyboard mode (ascii+scancode input)
; // 3 - mouse mode
; // 4 - device control (output) mode
; VALID COMMANDS for current version:
; 	'LOGIN'
;  Login request: db 0FFh, 'LOGIN', 0 
;	 ("Retro UNIX 386 v1 terminal requests login")
;  Login response: db 0FFh, 'login', 0
;	 ("login request accepted, wait for login prompt") 
; When a login requests is received and acknowledged (by
; serial port interrupt handler (communication procedure),
; Retro UNIX 386 v1 operating system will start terminal mode
; (login procedure) by changing comm. mode to 1 (terminal mode)
; and then running 'etc/getty' for tty8 (COM1) or tty9 (COM2)
; 
; 'sys connect' system call is used to change communication mode
; except 'LOGIN' command which is used to start terminal mode
; by using (COM port) terminal.

;com1own:     resb 1 ; COM1 owner (u.uno)
;com2own:     resb 1 ; COM2 owner (u.uno)
;com1mode:    resb 1 ; communication mode for COM1
;com1com:     resb 1 ; communication command for COM1
;com2mode:    resb 1 ; communication mode for COM1
;com2com      resb 1 ; communication command for COM1
;com1cbufp:   resb 8 ; COM1 command buffer char pointer	
;com2cbufp:   resb 8 ; COM2 command buffer char pointer	
;com1cbuf:    resb 8 ; COM2 command buffer
;com2cbuf:    resb 8 ; COM2 command buffer

; 22/08/2014 (RTC)
; (Packed BCD)
time_seconds: resb 1
time_minutes: resb 1
time_hours:   resb 1
date_wday:    resb 1
date_day:     resb 1
date_month:   resb 1			
date_year:    resb 1
date_century: resb 1

%include 'diskbss.inc'	; UNINITIALIZED DISK (BIOS) DATA

;;; Real Mode Data (10/07/2015 - BSS)

;alignb 2

; 27/02/2022
;%include 'ux.s' ; 12/04/2015 (unix system/user/process data)

; 23/02/2022
;; Memory (swap) Data (11/03/2015)
; 09/03/2015
;swpq_count: resw 1 ; count of pages on the swap que
;swp_drv:    resd 1 ; logical drive description table address of the swap drive/disk
;swpd_size:  resd 1 ; size of swap drive/disk (volume) in sectors (512 bytes). 		  				
;swpd_free:  resd 1 ; free page blocks (4096 bytes) on swap disk/drive (logical)
;swpd_next:  resd 1 ; next free page block
;swpd_last:  resd 1 ; last swap page block	

alignb 4

; 10/07/2015
; 28/08/2014
error_code:	resd 1
; 29/08/2014
FaultOffset: 	resd 1
; 21/09/2015
PF_Count:	resd 1	; total page fault count
		       	; (for debugging - page fault analyze)
		 	; 'page _fault_handler' (memory.inc)
			; 'sysgeterr' (u9.s)
; 23/02/2022
rtc_ticks:	resd 1  ; (temporary! this rtc counter value may be used 
			;  for a system call in next retro unix 386 version)
			; -2 ticks per second-
;; 21/08/2015
;;buffer: resb (nbuf*520) ;; sysdefs.inc, ux.s

; 27/02/2022
%include 'ux.s' ; 12/04/2015 (unix system/user/process data)

bss_end:

; 27/02/2022
BSS_SIZE equ bss_end - bss_start

; 27/12/2013
_end:  ; end of kernel code (and read only data, just before bss)
