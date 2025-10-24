;
; bootlace.nasm: bitwisde identical reproduction of the Minix 1.5 bootlace a.out executable in NASM
; NASM port by pts@fazekas.hu at Fri Oct 24 19:14:53 CEST 2025
;
; Compile with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -o bootlace bootlace.nasm
; Compile identically with: nasm-0.98.39 -O999999999 -w+orphan-labels -f bin -o bootlace bootlace.nasm
;
; Based on the following Minix 1.5 source and binary files:
;
; * /usr/oz/shoelace/bootlace.x (1990-04-24)
; * /usr/oz/shoelace/shoeasm.x (1990-04-24)
; * /usr/oz/shoelace/shoebfs.c (1990-04-24)
; * /usr/oz/shoelace/shoeboot.c (1990-04-24)
; * /etc/bootlace (2003-01-04 and 2004-04-20).
;

; In /usr/oz/shoelace/bootlace.doc
;
; NAME
;      BootLace - bootstrap the ShoeLace
;
; DESCRIPTION
;      /etc/bootlace contains the code that will be written into
;      the boot block of a Minix file system to make it bootable.
;      Some of the information in the file is incomplete and must
;      be patched by LaceUp in order to make a volume bootable.
;
;      BootLace is not directly executable from Minix.
;
; OPERATION
;      BootLace contains two pieces of code. The first piece is
;      that contained within the boot sector. This contains suffi-
;      cient code to retrieve the rest of the code in the boot
;      block and to set up enough of the environment to run the
;      basic file system scanner.
;
;      The main task undertaken by BootLace is to locate the file
;      /shoelace on the boot volume and to read it into memory.
;
;      BootLace is loaded at address 0x7c00 by the BIOS bootstrap
;      loader.  BootLace will load ShoeLace at address 0x10000 in
;      order to avoid problems caused by 64kb dma wraparound.  Once
;      ShoeLace is loaded, BootLace relinquishes control by making
;      a far call to address 0x10000.  ShoeLace is able to locate
;      the start of the boot block image examining the return
;      address that is pushed onto the stack. Two data objects can
;      be obtained from BootLace corpse:
;
;           1  the disk code needed to access the boot volume via
;              the BIOS
;
;           2  the boot parameter block.
;
;      BootLace also points the floppy parameter table vector to an
;      internal version of the table if the boot device is a floppy
;      disk. It is important that this vector be changed if other
;      code is subsequently loaded on the BootLace corpse.
;
bits 16
cpu 8086

%macro att 1
  times -($-section_text)+(%1) times 0 nop
  times +($-section_text)-(%1) times 0 nop
%endm
%macro att 2+
  times -($-section_text)+(%1) times 0 nop
  times +($-section_text)-(%1) times 0 nop
  %2
%endm

org -0x20  ; Negative of sizeof(struct exec). Negative of the size of the a.out header.
struct_exec:  ; a.out executable header in /usr/include/a.out.h
.a_magic:	dw 0x301
.a_flags:	db 0x10
.a_cpu:		db 4  ; 8086.
.a_hdrlen:	db 0x20
.a_unused:	db 0
.a_version:	dw 0
.a_text:	dd section_data-section_text
.a_data:	dd section_data_end-section_data
.a_bss:		dd section_bss_end-section_data_end
.a_entry:	dd _start
.a_total:	dd 0x10000
.a_syms:	dd section_syms_end-section_syms

section_text:

; In /usr/oz/shoelace/bootlace.x

;               MINIX TurboC Mini-Bootstrap Code
;
; This code will load into the first sector on disk. When
; read in it will read in the second sector just beyond the
; end of this sector. The whole lot will be moved into
; high memory and a suitable stack set up.
;
; Edit History:
;
; 02-May-1989   Moved hclicksize
; 24-Apr-1989   Make a BIOS compatible parameter block. Throw
;               out dynamic floppy sizing. Convert to .s format.
; 22-Apr-1989   Remove intersegment read capability
; 20-Apr-1989   Added hclicksize
; 17-Apr-1989   Moved movsw back in, take out loadseg
; 16-Apr-1989   Moved shoelace code movsw out of boot block
; 15-Apr-1989   Squeeze code
; 14-Apr-1989   Add loadseg to indicate top of shoelace
; 30-Mar-1989   Some cosmetic surgery to include VECTORSEG
; 29-Oct-1988   Moved shoelace higher to 256kb boundary
; 29-Sep-1988   Added cret and csv for portability
; 18-Sep-1988   Added jump to start
; 16-Apr-1988   Modelled on minix boot code
;

global _main
global _copyviabufptr
global _readblock
global _print
global _bootentry
global _loadlace
global _endbootsector
global _n_sectors
global _n_heads
global _hidden
global _diskcode
global _filename
global _hclicksize
global _noshoename
global _lacename
global _endshoename
global _startload
global _bufptr  ; In .bss .
global dsret
global sret
global cret
global _zoneblock
global _checkfilesize
global _dirscan
global _scanzone
global _shoehorn
global _readshoe

VECTORSEG      equ 0           ; vector segment
LOADSEG        equ 0x7c0       ; here the boot block itself is loaded
BOOTSEG        equ 0x1000      ; here it will copy itself (64k-127k)
SECTORSIZE     equ 512         ; size of disk sector
BOOTSIZE       equ SECTORSIZE  ; size of boot code
FDTABLEVECTOR  equ 0x1e        ; vector to floppy parameters
SIGNATURESIZE  equ 2           ; signature size in bytes
BYTESPERCLICK  equ 16          ; bytes per click

; Program entry point
;
; When the PC is powered on, it reads the first block from the
; disk into address 0x7C00 and jumps to it.  This is the code that
; is contained in that first block.
;
; On entry the following register contents are assumed:
;
;       es:si   partition table entry pointer if hard disk
;       dl      BIOS disk code (80h hard disk, 00h floppy disk, etc)
_start:
_main:
		att 0, jmp short _bootentry  ; 0x2b.

; Store the code for the disk here. This offset must match the
; one listed in shoe.h
_diskcode:	att 2, db 0x99  ; The BIOS drive number in DL will be copied here.

; This provides a BIOS parameter block. This is an attempt to make
; the boot sector DOS compatible. Unfortunately not all the fields
; make too much sense.
bpb:
		att 3, db '12345678'  ; 8-character OEM ID in a BPB.
		att 0xb, dw _endbootsector  ; bytes per sector
		att 0xd, db 0               ; sectors per block
		att 0xe, dw 0               ; boot sectors
		att 0x10, db 0               ; number of FATS
		att 0x11, dw 0               ; number of root directory entries
		att 0x13, dw 0               ; total sectors on volume
		att 0x15, db 0               ; media descriptor byte
		att 0x16, dw 0               ; sectors per FAT

_n_sectors:	att 0x18, dw 0  ; .sectors_per_track value. Matters for floppy only. For HDD, this is 0, and it will be autodected with int 13h AH==8.
_n_heads:	att 0x1a, dw 0  ; .head_count value. Matters for floppy only. For HDD, this is 0, and it will be autodected with int 13h AH==8.
_hidden:	att 0x1c, dd 0  ; LBA sector offset of the start of the partition. Also called the number of hidden sectors.

dskbase:
		att 0x20, times 11 db  0 ; BIOS Disk Parameter Table (DPT), 11 bytes. Used for floppy only. For HDD, all this is 0. https://fd.lod.bz/rbil/interrup/bios/1e.html

; This entry point will be used when the boot code is executed from
; the boot block. This is first bit of code that will actually be
; executed at boot time. The first thing to do is to reset the
; code segment.
_bootentry:
		att 0x2b, jmp LOADSEG:_loadlace  ; 0x7c0:0x30. Make sure we run with 0-based IP from now on.

; The real boot code begins here. None of the addressing registers,
; except cs, is correct at this stage.
_loadlace:

; The drive number is still in dl. es:si points at
; partition table entry if hard disk boot.
		att 0x30, mov ax, cs  ; current code segment
		att 0x32, mov ds, ax  ; data addressable
		att 0x34, mov ss, ax  ; stack addressable
		att 0x36, mov sp, -LOADSEG*BYTESPERCLICK
; Find out the parameters of the hard disk if this is a hard disk
; boot. The only way to find out the number of heads and the number
; of sectors per track is to ask.
		att 0x39, mov [_diskcode], dl  ; code for boot disk
		att 0x3d, test dl, 0x80  ; check for winchester
		att 0x40, jz short floppy
		att 0x42, push word [es:si+0x2]  ; sector and cylinder of start
		att 0x46, mov dh, [es:si+0x1]  ; head of start of partition
		att 0x4a, push dx
		att 0x4b, les ax, [es:si+0x8]; find start of partition
		att 0x4f, mov [_hidden], ax
		att 0x52, mov [_hidden+2], es
		att 0x56, mov ah, 0x8  ; get drive parameters
		att 0x58, int 0x13  ; dl already loaded

winistaterror:
		att 0x5a, jc short winistaterror  ; loop on error
		att 0x5c, and cl, 0x3f  ; isolate number of sectors
		att 0x5f, mov [_n_sectors], cl
		att 0x63, inc dh
		att 0x65, mov [_n_heads], dh  ; number of heads
		att 0x69, jmp short readshoehorn

; Initialize the floppy disk parameters. Initially assume that we
; have high capacity disks available. The disk parameter pointer
; is set to point at the parameters for this disk.
floppy:
		att 0x6b, mov ax, VECTORSEG  ; vector segment
		att 0x6e, mov es, ax  ; locate vector segment
		att 0x70, mov word [es:FDTABLEVECTOR*4+0], dskbase
		att 0x77, mov [es:FDTABLEVECTOR*4+2], ds
		att 0x7c, mov ax, 0x1  ; say initialise
		att 0x7f, push ax  ; save as cylinder 0 sector 1
		att 0x80, mov dh, 0x0  ; say head 0
		att 0x82, push dx
		att 0x83, int 0x13; ah and dl already loaded

; The upper half kb of boot code is read in from the boot device. This
; is loaded immediately following the end of this half kb. It is assumed
; that values for cx (ch = cylinder number, cl = sector number) and
; dx (dh = head number, dl = disk code) have been pushed
; onto the stack (cx pushed then dx). These values should yield
; the boot sector. It is further assumed that by simply incrementing
; the sector number will yield the second half of the boot block.
;
; The boot code requires that the boot sector contains bytes aa and 55
; at the end of the boot sector. Assume that the boot block writer
; has shuffled space in the code to effect this. To recover the executable
; it is necessary to read over this signature.
readshoehorn:
		att 0x85, push cs  ; read next block into this segment
		att 0x86, pop es
		att 0x87, mov bx, BOOTSIZE-SIGNATURESIZE  ; above boot sector
		att 0x8a, pop dx  ; head and disk
		att 0x8b, pop cx  ; recover sector and cylinder
		att 0x8c, inc cx  ; next sector

shoehornerror:  
		att 0x8d, mov ah, 0x0  ; initialise
		att 0x8f, int 0x13
		att 0x91, mov ax, 0x201  ; read one sector
		att 0x94, int 0x13
		att 0x96, jc short shoehornerror  ; loop on error
		att 0x98, mov ax, BOOTSEG  ; work out 32 bit address
		att 0x9b, mul word [_hclicksize]
		att 0x9f, mov [_bufptr+0], ax
		att 0xa2, mov [_bufptr+2], dx
		att 0xa6, call _shoehorn  ; shoe horn the code in
		att 0xa9, call BOOTSEG:0x0

_endbootsector:

; This marks the end of the boot code that must reside in the boot
; sector. The code following this point must reside in the boot block.

; The boot code will use the tiny model in which code and data occupy
; the same 64kb segment. Because of the space constraints, the data
; used for the boot block cannot be placed in the DATA segment
; since this might force it beyond the 1kb boot sector limit.
;
; BSS stuff is allowed since we don't have to ensure it's initialised.
;
; For this reason, all data required by the INITIAL boot block is placed
; here (within the code segment), and the code relies on the fact that
; both code and data are addressable within the same segment (cs == ds).

_hclicksize:	att 0xae, dw BYTESPERCLICK  ; 0x10.
_filename:	att 0xb0, dw _lacename
_noshoename:	att 0xb2, db 'Lost /'
_lacename:	att 0xb8, db 'shoelace'
_endshoename:	att 0xc0, db 0, 0xa, 0
_startload:	att 0xc3, db 1

; Copy local buffer
;
; This function copies a local buffer to an arbitrary location
; via bufseg:bufptr. On completion bufptr is updated. The
; calling sequence is:
;
;       void copyviabufptr(char *buf, unsigned int bytes)
_copyviabufptr:
		att 0xc4, push bp  ; make stack frame
		att 0xc5, mov bp, sp
		att 0xc7, push si
		att 0xc8, push di
		att 0xc9, les ax, [_bufptr+0]  ; load 32 bit pointer
		att 0xcd, mov dx, es
		att 0xcf, div word [_hclicksize]  ; convert to segment and offset
		att 0xd3, mov es, ax
		att 0xd5, mov di, dx
		att 0xd7, mov cx, [bp+0x6]  ; bytes to copy
		att 0xda, add [_bufptr+0], cx
		att 0xde, adc word [_bufptr+2], byte +0x0
		att 0xe3, mov si, [bp+0x4]  ; pointer to buffer
		att 0xe6, cld  ; direction is up
		att 0xe7, repne movsb
		att 0xe9, jmp strict near dsret

; Read a minix block from the disk
;
; The calling convention is:
;
;       void readblock(unsigned long lbn, buffer buf)
;
; It is assumed that there are 2 physical sectors per minix block. The
; frame buffer contortions yield the following structure:
;
;       0       saved frame pointer     <- sp and bp
;       2       return address
;       4       ls block number
;       6       ms block number
;       8       buffer pointer
_readblock:
		att 0xec, push bp  ; make stack frame
		att 0xed, mov bp, sp
		att 0xef, push si
		att 0xf0, push di
		; lbn *= BLOCK_SIZE/SECTOR_SIZE;
		att 0xf1, mov ax, [bp+0x4]  ; get block number
		att 0xf4, mov dx, [bp+0x6]
		att 0xf7, shl ax, 1  ; left shift to make sector
		att 0xf9, rcl dx, 1
		att 0xfb, add ax, [_hidden]  ; combine hidden sectors
		att 0xff, adc dx, [_hidden+2]
		att 0x103, xor si, si  ; no offset yet
		att 0x105, mov bh, [_n_sectors]  ; possible overlap
		att 0x109, jmp short readdosector

readnextsector:
		att 0x10b, xor bh, bh  ; force overlap
		att 0x10d, mov ax, [bp+0x4]  ; recover sector number
		att 0x110, mov dx, [bp+0x6]
		att 0x113, inc ax  ; next sector
		att 0x114, jnz short readdosector
		att 0x116, inc dx  ; propagate inc

readdosector:
		att 0x117, mov [bp+0x4], ax  ; save sector number
		att 0x11a, mov [bp+0x6], dx
		att 0x11d, div word [_n_sectors]  ; extract sector number
		att 0x121, mov cl, dl  ; save sector number
		att 0x123, inc cl  ; sectors count from 1
		att 0x125, mov di, SECTORSIZE  ; read size
		att 0x128, mov bl, 0x1  ; read one sector
		att 0x12a, cmp cl, bh  ; check for track overlap
		att 0x12c, jnc short readoverlap  ; overlaps are read slowly
		att 0x12e, inc bx  ; can read an extra sector
		att 0x12f, add di, di  ; double read size

readoverlap:
; head = lbn / n_sectors % n_heads;
; cyl  = lbn / n_sectors / n_heads;
;
; This computation assumes that (n_cylinders-1)*n_heads < 65536 ie the
; 10 bit cylinder number multiplied by the number of heads results in
; a quantity that will fit in 16 bits.
		att 0x131, xor dx, dx  ; cast unsigned int to long
		att 0x133, div word [_n_heads]  ; extract head and cylinder number
		att 0x137, mov dh, dl  ; locate head number
		att 0x139, mov ch, al  ; ls part of cylinder number
		att 0x13b, shr ax, 1  ; isolate ms part of cylinder number
		att 0x13d, shr ax, 1
		att 0x13f, and al, 0xc0  ; only two bits wanted
		att 0x141, or cl, al  ; combine with sector number
		att 0x143, mov dl, [_diskcode]  ; disk drive specifier

readblockerror:
		att 0x147, mov ah, 0x2  ; read from disk
		att 0x149, mov al, bl  ; sectors to read
		att 0x14b, push ds  ; this segment
		att 0x14c, pop es
		att 0x14d, mov bx, [bp+0x8]  ; buffer pointer
		att 0x150, add bx, si  ; add in offset
		att 0x152, int 0x13  ; perform request
		att 0x154, jnc short readok  ; no error
		att 0x156, mov ah, 0x0  ; initialise
		att 0x158, int 0x13
		att 0x15a, jmp short readblockerror

readok:
		att 0x15c, add si, di  ; increase offset
		att 0x15e, cmp si, 2*SECTORSIZE  ; 2 sectors per block
		att 0x162, jc short readnextsector
		att 0x164, jmp short dsret  ; return


; Print a string
;
; The string to be printed is passed on the stack, C style. It is
; left to the caller to pop the result off. The calling convention is
;
;       void print(char *s)
;
; No stack frame is used. The argument is popped off into a register
; and manipulated from there.
_print:
		att 0x166, pop dx  ; return address off the stack
		att 0x167, pop bx  ; pointer string to print
		att 0x168, push bx  ; replace it for later

nextchar:
		att 0x169, mov al, [bx]  ; al contains char to be printed 
		att 0x16b, test al, al  ; check for null char
		att 0x16d, jz short printdone  ; no
		att 0x16f, mov ah, 14  ; 14 = print char
		att 0x171, inc bx  ; increment string pointer
		att 0x172, push bx  ; save bx
		att 0x173, mov bx, 0x1  ; foreground color and page 0
		att 0x176, int 0x10  ; call BIOS VIDEO_IO
		att 0x178, pop bx  ; restore bx
		att 0x179, jmp short nextchar  ; next character
printdone:
		att 0x17b, jmp dx  ; return

; Code in /usr/oz/shoelace/bootlace.x ends here.

; In /usr/oz/shoelace/shoeasm.x

;               Bootblock and ShoeLace Low Level Code
;
; This code is useful for both the bootblock and ShoeLace code.
;
; Edit History:
;
; 03-May-1989   Hacked from shoeboot.s
;

; C Function Exit
;
; The Minix ACK compiler generates calls to .dsret and
; friends when exiting from functions.
dsret:
		att 0x17d, pop di  ; recover registers
sret:
		att 0x17e, pop si
cret:
		att 0x17f, mov sp, bp  ; recover old frame
		att 0x181, pop bp
		att 0x182, ret

; Convert zone to block
;
; A 16 bit zone number is converted to a 32 bit block number
; using the specified shift.
;
;       long zoneblock(unsigned int zone)
;
; No frame is set up, instead the arguments are popped off the stack
; into the registers and used from there.
;
; The function assumes that the scaling factor is to be found in
; the global _zsize.
_zoneblock:
		att 0x183, pop bx  ; return address
		att 0x184, pop ax  ; zone number
		att 0x185, push ax  ; push zone
		att 0x186, mul word [_zsize]  ; convert to blocks
		att 0x18a, jmp bx  ; return

; Check filesize
;
; The filesize is checked to see if it is less than, equal to
; or greater than zero. The function returns an int which is
; less than, equal to or greater than zero reflecting the value
; of filesize.
;
;       int checkfilesize(void)
_checkfilesize:
		att 0x18c, mov ax, [_filesize]  ; low order filesize
		att 0x18f, or al, ah  ; combine bytewise
		att 0x191, xor ah, ah  ; knock out high byte
		att 0x193, or ax, [_filesize+2]  ; combine ms word
		att 0x197, ret

; Code in /usr/oz/shoelace/shoeasm.x ends here.

; In /usr/oz/shoelace/shoebfs.c
; The intention of this code is to provide a root directory scanner
; and file reader for the boot code in as small a space as possible.
; This forms a basic file system scanner.

; /*
;  * Scan a zone
;  *
;  * Scan the specified zones taking note of the level of indirection.
;  * Indirection is taken care of by recursion. Processing of zero
;  * level zones is done by calling the service routine. If the
;  * routine returns 0, processing continues. Any other value causes
;  * processing to be aborted.
;  *
;  * Assume that n > 0. The outer loop will execute a silly number
;  * of times if this is not true.
;  */
; inode_nr scanzone F4(zone_nr *, zp, int, level, int, n, INODEFN, fn) {
;   long b;				/* current block */
;   int i;				/* index */
;   inode_nr v;				/* return value from function */
;   buffer tb;				/* zone reading buffer */
;   do {
;     b = zoneblock(*zp++);
;     i = zsize;
;     do {
;       readblock(b++, (buffer *) &tb[0]);
;       if ((v = level ? scanzone((zone_nr *) tb, level-1,
; 				sizeof(tb)/sizeof(zone_nr), fn)
; 		     : (*fn)(tb)) != 0)
; 	goto DoneScan;
;     } while (--i);
;   } while (--n);
; DoneScan:
;   return v;
; }
_scanzone:
		att 0x198, push bp
		att 0x199, mov bp, sp
		att 0x19b, sub sp, 0x408
		att 0x19f, push si
		att 0x1a0, push di
.1:
		att 0x1a1, mov bx, [bp+0x4]
		att 0x1a4, add word [bp+0x4], byte +0x2
		att 0x1a8, push word [bx]
		att 0x1aa, call _zoneblock
		att 0x1ad, pop bx
		att 0x1ae, mov [bp-0x4], ax
		att 0x1b1, mov [bp-0x2], dx
		att 0x1b4, mov cx, [_zsize]
		att 0x1b8, mov [bp-0x6], cx
.2:
		att 0x1bb, lea ax, [bp-0x408] ; Large (>1 KiB) stack usage.
		att 0x1bf, push ax
		att 0x1c0, push word [bp-0x2]
		att 0x1c3, push word [bp-0x4]
		att 0x1c6, add word [bp-0x4], byte +0x1
		att 0x1ca, adc word [bp-0x2], byte +0x0
		att 0x1ce, call _readblock
		att 0x1d1, add sp, byte +0x6
		att 0x1d4, cmp word [bp+0x6], byte +0x0
		att 0x1d8, jz short .3
		att 0x1da, mov ax, [bp+0x6]
		att 0x1dd, dec ax
		att 0x1de, push word [bp+0xa]
		att 0x1e1, mov cx, 0x200  ; sizeof(tb) / sizeof(zone_nr).
		att 0x1e4, push cx
		att 0x1e5, push ax
		att 0x1e6, lea ax, [bp-0x408]
		att 0x1ea, push ax
		att 0x1eb, call _scanzone
		att 0x1ee, add sp, byte +0x8
		att 0x1f1, push ax
		att 0x1f2, jmp short .4
.3:
		att 0x1f4, mov ax, [bp+0xa]
		att 0x1f7, lea cx, [bp-0x408]
		att 0x1fb, push cx
		att 0x1fc, call ax
		att 0x1fe, pop bx
		att 0x1ff, push ax
.4:
		att 0x200, pop word [bp-0x8]
		att 0x203, cmp word [bp-0x8], byte +0x0
		att 0x207, jz short .5
		att 0x209, jmp short .6
.5:
		att 0x20b, dec word [bp-0x6]
		att 0x20e, jnz short .2
		att 0x210, dec word [bp+0x8]
		att 0x213, jnz short .1
.6:
		att 0x215, mov ax, [bp-0x8]
		att 0x218, jmp strict near dsret

; /*
;  * Search the root directory for an entry
;  *
;  * On entry to this routine, we will have block full of directory
;  * entries. Scan all the directory entries for the one we want. If
;  * the entry is found, its inode number will be returned. If the
;  * entry is not found, but the directory has not been searched in
;  * full, then return 0 - otherwise return ROOT_INODE. Note that
;  * there is potential for ambiguity since a return value
;  * of ROOT_INODE could mean that a the root directory has been
;  * found or that the required entry is not present. The caller
;  * should interrogate filesize to distinguish between the two
;  * cases (filesize <= 0 always indicates the latter).
;  */
; inode_nr dirscan F1(buffer *, dp) {
;   char *ip;				/* name comparison pointer */
;   char *ep;				/* directory entry name */
;   dir_struct *edp;			/* end of buffer */
;   inode_nr thisnode;			/* current inode */
;   for (edp = (dir_struct *) dp + sizeof(*dp)/sizeof(dir_struct);
;        (dir_struct *) dp < edp;
;        ((dir_struct *) dp)++) {
;     if ((thisnode = ((dir_struct *) dp)->d_inum) != 0) {
;       for (ip = filename, ep = ((dir_struct *) dp)->d_name; *ip++ == *ep; ) {
; 	if (*ep++ == 0)
; 	  return thisnode;
;       }
;     }
;     filesize -= sizeof(dir_struct);
;     if (checkfilesize() <= 0)
;       return ROOT_INODE;
;   }
;   return 0;
; }
_dirscan:
		att 0x21b, push bp
		att 0x21c, mov bp, sp
		att 0x21e, sub sp, byte +0x8
		att 0x221, push si
		att 0x222, mov si, [bp+0x4]
		att 0x225, push di
		att 0x226, lea ax, [si+0x400]
		att 0x22a, mov [bp-0x6], ax
.1:
		att 0x22d, cmp [bp-0x6], si
		att 0x230, jna short .5
		att 0x232, mov ax, [si]
		att 0x234, mov [bp-0x8], ax
		att 0x237, cmp word [bp-0x8], byte +0x0
		att 0x23b, jz short .3
		att 0x23d, mov cx, [_filename]
		att 0x241, mov [bp-0x2], cx
		att 0x244, lea ax, [si+0x2]
		att 0x247, mov di, ax
.2:
		att 0x249, mov bx, [bp-0x2]
		att 0x24c, inc word [bp-0x2]
		att 0x24f, mov al, [bx]
		att 0x251, cbw
		att 0x252, push ax
		att 0x253, mov al, [di]
		att 0x255, cbw
		att 0x256, pop cx
		att 0x257, cmp cx, ax
		att 0x259, jnz short .3
		att 0x25b, mov bx, di
		att 0x25d, inc di
		att 0x25e, cmp byte [bx], 0x0
		att 0x261, jnz short .2
		att 0x263, push word [bp-0x8]
		att 0x266, jmp short .6
.3:
		att 0x268, mov cx, [_filesize]
		att 0x26c, mov dx, [_filesize+2]
		att 0x270, sub cx, byte +0x10
		att 0x273, sbb dx, byte +0x0
		att 0x276, mov [_filesize], cx
		att 0x27a, mov [_filesize+2], dx
		att 0x27e, call _checkfilesize
		att 0x281, or ax, ax
		att 0x283, jg short .4
		att 0x285, mov ax, 0x1
		att 0x288, push ax
		att 0x289, jmp short .6
.4:
		att 0x28b, add si, byte +0x10
		att 0x28e, jmp short .1
.5:
		att 0x290, xor ax, ax
		att 0x292, push ax
.6:
		att 0x293, pop ax
		att 0x294, jmp strict near dsret

; Code in /usr/oz/shoelace/shoebfs.c ends here.

; In /usr/oz/shoelace/shoeboot.c
;
; The intention of this code is to provide a root directory scanner
; and file reader for the boot code in as small a space as possible.
; The code will read into memory the rest of the shoe which will
; then perform the boot.

; void shoehorn F0() {
;   inode_nr minixnode;                   /* inode of image */
;   block_nr inodeblock;                  /* block where inodes starts */
;   int minixinx;                         /* index to minix node */
;   INODEFN fn;                           /* data disposition */
;   int j;                                /* index */
;   zone_nr *zp;                          /* zone number pointer */
;   int len;                              /* number of zones in list */
;   int ind;                              /* indirection */
; /* Get parameters from the disk superblock */
;   readblock((long) SUPER_BLOCK, (buffer *) &blockbuf[0]);
;   zsize = 1 << ((struct super_block *) blockbuf)->s_log_zone_size;
;   inodeblock = SUPER_BLOCK + 1 +
;                ((struct super_block *) blockbuf)->s_imap_blocks +
;                ((struct super_block *) blockbuf)->s_zmap_blocks;
; /* Locate ShoeLace then load it */
;   for (minixnode = ROOT_INODE, fn = (INODEFN) dirscan, j = 3;
;        --j;
;        fn = (INODEFN) readshoe) {
;     readblock((unsigned long) (inodeblock +
;                                (--minixnode)/(BLOCK_SIZE/sizeof(d_inode))),
;               (buffer *) &blockbuf[0]);
;     minixinx = minixnode % (BLOCK_SIZE/sizeof(d_inode));
;     filesize = ((d_inode *) blockbuf)[minixinx].i_size;
; /* Inline form of dozones() in order to save precious bytes */
;     for (zp = &((d_inode *) blockbuf)[minixinx].i_zone[0],
;          ind = 0, len = NR_DZONE_NUM;
;          
;          ind < 3 && (minixnode = scanzone(zp, ind, len, fn)) == 0;
;          
;          zp += len, ind++, len = 1)
;       ;
;     if (minixnode <= ROOT_INODE)
;       for (endshoename = '\r', print(noshoename); ; )
;         ;
;   }
; }
_shoehorn:
		att 0x297, push bp
		att 0x298, mov bp, sp
		att 0x29a, sub sp, byte +0x10
		att 0x29d, push si
		att 0x29e, push di
		att 0x29f, mov ax, _blockbuf
		att 0x2a2, push ax
		att 0x2a3, xor ax, ax
		att 0x2a5, push ax
		att 0x2a6, mov ax, 0x1
		att 0x2a9, push ax
		att 0x2aa, call _readblock
		att 0x2ad, add sp, byte +0x6
		att 0x2b0, mov cx, [_blockbuf+0xa]
		att 0x2b4, mov ax, 0x1
		att 0x2b7, shl ax, cl
		att 0x2b9, mov [_zsize], ax
		att 0x2bc, mov cx, [_blockbuf+4]
		att 0x2c0, add cx, byte +0x2
		att 0x2c3, add cx, [_blockbuf+6]
		att 0x2c7, mov [bp-0x4], cx
		att 0x2ca, mov word [bp-0x2], 0x1
		att 0x2cf, mov word [bp-0x8], _dirscan
		att 0x2d4, mov word [bp-0xa], 0x3
.1:
		att 0x2d9, dec word [bp-0xa]
		att 0x2dc, jnz short .2
		att 0x2de, jmp strict near .7
.2:
		att 0x2e1, mov ax, [bp-0x2]
		att 0x2e4, sub ax, strict word 1
		att 0x2e7, mov [bp-0x2], ax
		att 0x2ea, mov cx, 0x20
		att 0x2ed, mov ax, [bp-0x2]
		att 0x2f0, xor dx, dx
		att 0x2f2, div cx
		att 0x2f4, add ax, [bp-0x4]
		att 0x2f7, xor cx, cx
		att 0x2f9, mov dx, _blockbuf
		att 0x2fc, push dx
		att 0x2fd, push cx
		att 0x2fe, push ax
		att 0x2ff, call _readblock
		att 0x302, add sp, byte +0x6
		att 0x305, mov cx, 0x20
		att 0x308, mov ax, [bp-0x2]
		att 0x30b, xor dx, dx
		att 0x30d, div cx
		att 0x30f, mov [bp-0x6], dx
		att 0x312, mov bx, [bp-0x6]
		att 0x315, mov cx, 0x5
		att 0x318, shl bx, cl
		att 0x31a, mov cx, [bx+_blockbuf+4]
		att 0x31e, mov dx, [bx+_blockbuf+6]
		att 0x322, mov [_filesize], cx
		att 0x326, mov [_filesize+2], dx
		att 0x32a, mov bx, [bp-0x6]
		att 0x32d, mov cx, 0x5
		att 0x330, shl bx, cl
		att 0x332, add bx, _blockbuf+0xe
		att 0x336, mov [bp-0xc], bx
		att 0x339, mov word [bp-0x10], 0x0
		att 0x33e, mov word [bp-0xe], 0x7
.3:
		att 0x343, cmp word [bp-0x10], byte +0x3
		att 0x347, jnl short .4
		att 0x349, push word [bp-0x8]
		att 0x34c, push word [bp-0xe]
		att 0x34f, push word [bp-0x10]
		att 0x352, push word [bp-0xc]
		att 0x355, call _scanzone
		att 0x358, add sp, byte +0x8
		att 0x35b, mov [bp-0x2], ax
		att 0x35e, cmp word [bp-0x2], byte +0x0
		att 0x362, jnz short .4
		att 0x364, mov bx, [bp-0xe]
		att 0x367, shl bx, 1
		att 0x369, add bx, [bp-0xc]
		att 0x36c, mov [bp-0xc], bx
		att 0x36f, inc word [bp-0x10]
		att 0x372, mov word [bp-0xe], 0x1
		att 0x377, jmp short .3
.4:
		att 0x379, cmp word [bp-0x2], byte +0x1
		att 0x37d, ja short .6
		att 0x37f, mov byte [_endshoename], 0xd
		att 0x384, mov ax, _noshoename
		att 0x387, push ax
		att 0x388, call _print
		att 0x38b, pop bx
.5:
		att 0x38c, jmp short .5  ; Infinite busy loop. Halt the system.
.6:
		att 0x38e, mov word [byte bp-0x8], _readshoe
		att 0x393, jmp strict near .1
.7:
		att 0x396, jmp strict near dsret

; /*
;  * Read in shoe
;  *
;  * This function reads in the rest of the shoe. This is loaded
;  * into memory immediately following the end of the boot block.
;  * It will contain the rest of the code needed to boot minix.
;  */
; static inode_nr readshoe F1(buffer *, bp) {
;   unsigned int hsize;                   /* size of a.out header */
;   hsize = 0;
;   if (startload != 0)
;     hsize = ((struct exec *) bp)->a_hdrlen;
;   startload = 0;
;   copyviabufptr((char *) bp + hsize, sizeof(*bp)-hsize);
;   filesize -= sizeof(*bp);
;   hsize = 0;
;   if (checkfilesize() <= 0)
;     hsize = 2*ROOT_INODE;
;   return hsize;
; }
_readshoe:
		att 0x399, push bp
		att 0x39a, mov bp, sp
		att 0x39c, push ax
		att 0x39d, push si
		att 0x39e, push di
		att 0x39f, mov di, [bp+0x4]
		att 0x3a2, xor si, si
		att 0x3a4, cmp byte [_startload], 0x0
		att 0x3a9, jz short .1
		att 0x3ab, mov al, [di+0x4]
		att 0x3ae, xor ah, ah
		att 0x3b0, mov si, ax
.1:
		att 0x3b2, mov byte [_startload], 0x0
		att 0x3b7, mov ax, 0x400
		att 0x3ba, sub ax, si
		att 0x3bc, mov bx, si
		att 0x3be, add bx, di
		att 0x3c0, push ax
		att 0x3c1, push bx
		att 0x3c2, call _copyviabufptr
		att 0x3c5, pop bx
		att 0x3c6, pop bx
		att 0x3c7, mov cx, [_filesize]
		att 0x3cb, mov dx, [_filesize+2]
		att 0x3cf, sub cx, 0x400  ; sizeof(buffer).
		att 0x3d3, sbb dx, byte +0x0
		att 0x3d6, mov [_filesize], cx
		att 0x3da, mov [_filesize+2], dx
		att 0x3de, xor si, si
		att 0x3e0, call _checkfilesize
		att 0x3e3, or ax, ax
		att 0x3e5, jg short .2
		att 0x3e7, mov si, 2  ; 2*ROOT_INODE.
.2:
		att 0x3ea, mov ax, si
		att 0x3ec, jmp strict near dsret

endtext:
		att 0x3ef, times (section_text-$)&1 db 0  ; Aligment padding.
		att 0x3f0

; Code in /usr/oz/shoelace/shoeboot.c ends here.

section_data:  ; Empty.
section_data_end:

section_syms:

%macro symbol 3
  %%name: db %1  ; n_name.
  times 8-($-%%name) db 0  ; NUL-padded to 8 bytes.
  dd %2  ; n_value.
  db %3  ; n_sclass.
  db 0x13  ; n_numaux. Unused by Minix.
  dw 0x4e7a  ; n_type. Unused by Minix.
%endm

		att 0x3f0
		symbol '_diskcod', _diskcode, 0x12
		symbol '_filenam', _filename, 0x12
		symbol '_lacenam', _lacename, 0x12
		symbol 'begbss', section_bss, 0x14
		symbol '.', section_bss_end, 0x14
		symbol '_print', _print, 0x12
		symbol '_readblo', _readblock, 0x12
		symbol 'enddata', section_data_end, 0x13
		symbol 'begdata', section_data, 0x13
		symbol '_hidden', _hidden, 0x12
		symbol '_scanzon', _scanzone, 0x12
		symbol '_start', _start, 0x12
		symbol '_n_heads', _n_heads, 0x12
		symbol 'endbss', section_bss_end, 0x14
		symbol '_endshoe', _endshoename, 0x12
		symbol '_main', _main, 0x12
		symbol '_checkfi', _checkfilesize, 0x12
		symbol '_bootent', _bootentry, 0x12
		symbol '_zoneblo', _zoneblock, 0x12
		symbol '_filesiz', _filesize, 0x14
		symbol '_hclicks', _hclicksize, 0x12
		symbol '_zsize', _zsize, 0x14
		symbol '_endboot', _endbootsector, 0x12
		symbol 'endtext', endtext, 0x12
		symbol 'begtext', section_text, 0x12
		symbol '_copyvia', _copyviabufptr, 0x12
		symbol '_loadlac', _loadlace, 0x12
		symbol '_n_secto', _n_sectors, 0x12
		symbol '_bufptr', _bufptr, 0x14
		symbol '_startlo', _startload, 0x12
		symbol '_dirscan', _dirscan, 0x12
		symbol '.sret', sret, 0x12
		symbol '.cret', cret, 0x12
		symbol '.dsret', dsret, 0x12
		symbol '_shoehor', _shoehorn, 0x12
		symbol '_noshoen', _noshoename, 0x12
		symbol '_blockbu', _blockbuf, 0x14
		symbol '_readsho', _readshoe, 0x2
		symbol 'BOOTSIZE', BOOTSIZE, 0x1
		symbol 'floppy', floppy, 0x2
		symbol 'winistat', winistaterror, 0x2
		symbol 'dskbase', dskbase, 0x2
		symbol 'FDTABLEV', FDTABLEVECTOR, 0x1
		symbol 'readok', readok, 0x2
		symbol 'BOOTSEG', BOOTSEG, 0x1
		symbol 'SECTORSI', SECTORSIZE, 0x1
		symbol 'VECTORSE', VECTORSEG, 0x1
		symbol 'printdon', printdone, 0x2
		symbol 'readshoe', readshoehorn, 0x2
		symbol 'readbloc', readblockerror, 0x2
		symbol 'nextchar', nextchar, 0x2
		symbol 'SIGNATUR', SIGNATURESIZE, 0x1
		symbol 'BYTESPER', BYTESPERCLICK, 0x1
		symbol 'shoehorn', shoehornerror, 0x2
		symbol 'readover', readoverlap, 0x2
		symbol 'readnext', readnextsector, 0x2
		symbol 'readdose', readdosector, 0x2

section_syms_end:

absolute section_data_end  ; BSS.
section_bss:

_blockbuf:	resb 0x400  ; static buffer blockbuf  ; block buffer. 0x400 bytes at @0x3f0.
_bufptr:	resw 2  ; ADDRESS bufptr;  code load point 4 bytes at @0x7f0.
_filesize:	resd 1  ; long filesize;  size of file. 4 bytes at @0x7f4.
_zsize:		resw 1  ; unsigned int zsize;  zone size  ; 2 bytes at @0x7f8.
; No more global variables, we have them all. Total size: a_bss == 0x40a bytes.

section_bss_end:

; __END__
