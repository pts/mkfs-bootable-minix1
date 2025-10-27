;
; mbr_bootlace.nasm: Minix 1.5 bootlace boot code that can be put to a HDD MBR
; MBR-specific code and size optimizations by pts@fazekas.hu at Fri Oct 24 19:14:53 CEST 2025
;
; Compile for /shoelace  with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -DSHOELACE -o mbr_bootlace.bin  mbr_bootlace.nasm
; Compile for /minix 1.5 with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -DMINIX    -o mbr_bootminix.bin mbr_bootlace.nasm
; Compile for /boot      with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -DBOOT     -o mbr_bootboot.bin  mbr_bootlace.nasm
;
; Based on the following Minix 1.5 source files:
;
; * /usr/oz/shoelace/bootlace.x (1990-04-24)
; * /usr/oz/shoelace/shoeasm.x (1990-04-24)
; * /usr/oz/shoelace/shoebfs.c (1990-04-24)
; * /usr/oz/shoelace/shoeboot.c (1990-04-24)
;
; This boot code can boot Minix >=1.6 (both i86 and i386) from a HDD which
; doesn't have partitions, but the minix1 filesystem (with s_magic is created to span over
; the entire HDD (i.e. /dev/hd0). It can't boot from a minix2 filesystem
; (introduced in Minix 1.6, boot floppies already use it).
;
; Here is how you should prepare such a bootable filesystem image for
; mbr_bootminix.bin:
;
; 1. Create the Minix filesystem.
; 2. Copy your minix kernel image file (typically larger than 128 KiB) as
;    /minix, to the root directory of the fileystem.
; 3. Unmount the filesystem.
; 4. Copy the binary version of this code (mbr_bootminix.bin) to the first two
;    sectors of the HDD. On a Linux host, if the Minix filesystem image
;    filename is *hd.img*, you can do this by running `dd if=mbr_bootlace.bin
;    of=hd.img count=2 conv=notrunc`.
; 5. There is not need to create a partition table. *mbr_bootminix.bin*
;    already has one. The Minix root partition will be /dev/hd4 (or dev/hd4,
;    /if booting from the 2nd HDD). (It also works as /dev/hd0.)
;    There is no need to configure this, it will be autodetected on boot.
;
; Here is how you should prepare such a bootable filesystem image for
; mbe_bootlace.bin:
;
; 1. Create the minix1 filesystem.
; 2. Copy the file named shoelace of the ShoeLace bootloader to the root
;    directory of the fileystem.
; 3. If you have a Minix kernel image, then extract the 4 component files
;    kernel, mm, fs, and init using
;    [split_minix_kernel.pl](https://github.com/pts/mkfs-bootable-minix1/blob/master/split_minix_kernel.pl).
; 4. Create the directory /etc/system on the filesystem, and copy the kernel
;    component files kernel, mm, fs, and init there.
; 5. Create a the file /etc/config on the filesystem with the following
;    contents:
;    ```
;    kernel /etc/system/kernel
;    mm /etc/system/mm
;    fs /etc/system/fs
;    init /etc/system/init
;    ```
; 6. Unmount the filesystem.
; 7. Copy the binary version of this code (mbr_bootlace.bin) to the first two
;    sectors of the HDD. On a Linux host, if the Minix filesystem image
;    filename is *hd.img*, you can do this by running `dd if=mbr_bootlace.bin
;    of=hd.img count=2 conv=notrunc`.
; 8. There is not need to create a partition table. *mbr_bootlace.bin*
;    already has one. The Minix root partition will be /dev/hd4. There is no
;    need to configure this, it will be autodetected on boot.
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

; --- Compile-time configuration (`nasm -D...').

%ifdef MINIX
  %define MINIX 1  ; This can boot a Minix 1.5 (either i86 or i386) kernel image in /minix. It can't boot Minix >=1.6.
%else
  %define MINIX 0
%endif
%ifdef BOOT
  %define BOOT 1  ; This can boot a Minix 1.7.0 boot monitor in /boot. !! Maybe it works with 1.6 and 2.0 as well.
%else
  %define BOOT 0
%endif
%ifdef SHOELACE
  %define SHOELACE 1  ; This can boot a ShoeLace bootloader (used to boot Minix 1.5) in /shoelace.
%endif
%ifdef BOOTLACE
  %define SHOELACE 1
  %undef BOOTLACE
%endif
%ifndef SHOELACE
  %define SHOELACE 0
%endif
%if MINIX+SHOELACE+BOOT==0
  %define SHOELACE 1  ; Default.
%endif
%if MINIX+SHOELACE+BOOT>1
  %error ERRROR_MULTIPLE_BOOT_METHODS_SELECTED
  times -1 nop
%endif

; ---

bits 16
cpu 8086

; Minix 1.5 device numbers. The DEV_HD* ones have changed (exceptfor DEV_HD0) in Minix 1.6.
DEV_FD0 equ 0x200  ; GRUB (fd0): First floppy.
DEV_HD0 equ 0x300  ; GRUB (hd0): Start of first hard disk.
DEV_HD1 equ 0x301  ; GRUB (hd0,1): First hard disk, partition 1.
DEV_HD2 equ 0x302  ; GRUB (hd0,2): First hard disk, partition 2.
DEV_HD3 equ 0x303  ; GRUB (hd0,3): First hard disk, partition 3.
DEV_HD4 equ 0x304  ; GRUB (hd0,4): First hard disk, partition 4.
DEV_HD5 equ 0x305  ; GRUB (hd1): Start of second hard disk.
DEV_HD6 equ 0x306  ; GRUB (hd1,1): Second hard disk, partition 1.
DEV_HD7 equ 0x307  ; GRUB (hd1,2): Second hard disk, partition 2.
DEV_HD8 equ 0x308  ; GRUB (hd1,3): Second hard disk, partition 3.
DEV_HD9 equ 0x309  ; GRUB (hd1,4): Second hard disk, partition 4.
DEV_RAM equ 0x100  ; Ramdisk.

SECTOR_SIZE equ 0x200  ; Byte size of a disk sector.

BOOT_SIGNATURE equ 0xaa55  ; dw.

SCANCODE equ 13  ; Scancode of '=' on the IBM PC keyboard.

CR  equ 13  ; ASCII CR '\r'.
LF  equ 10  ; ASCII LF '\n'.
NUL equ 0   ; ASCII NUL '\0'.

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

; Program entry point
;
; When the PC is powered on, it reads the first 0x200 bytes from the
; HDD into address 0x7c00 and jumps to it.  This is the code that
; is contained in that first sector.
;
; On entry the following register contents are assumed:
;
; * DL: BIOS drive number (80h hard disk, 00h floppy disk, etc.)
_start:
_main:
; BIOS has put the boot drive number to DL.
%if SHOELACE
  IMAGE_SEG equ 0x1000  ; We will load the /shoelace image here (at IMAGE_SEG:0).
  MBR_LOAD_SEG equ 0x7c0  ; The BIOS has loaded our code (from _start) to MBR_LOAD_SEG:0.
		mov ax, MBR_LOAD_SEG  ; current code segment
		mov ds, ax  ; data addressable
		cli  ; Some broken early 8086 CPUs can process an interrupts after `mov ss, ax'.
		mov ss, ax  ; stack addressable
		mov sp, -MBR_LOAD_SEG<<4
		sti
		xor di, di  ; Workaround for buggy BIOS doing int 13h AH==8. Also the 0 value will be used later.
		mov [byte di+_diskcode], dl  ; code for boot disk
		test dl, dl
		jns short fatal_lost  ; On floppy.
		mov es, di  ; Workaround for buggy BIOS.
		push dx  ; The AH==8 call below overwrites DL.
		; The BIOS may jump to 0:0x7c00, 0x7c0:0 or any combination. We don't care for CS, because we use only relative jumps. We've already set DS := MBR_LOAD_SEG == 0x7c0.
		mov ah, 8  ; Read drive parameters. Modifies CF, AH, DL, DH, CX, BL, ES, DI.
		jmp short cont_main
		times 0x1c-($-_start) hlt  ; shoemain(...) in /shoelace expects _hidden to be at _start+0x1c.
  _n_sectors: equ _start+0x18  ; dw .sectors_per_track value. It will be autodected with int 13h AH==8. shoemain(...) in /shoelace expects _n_sectors to be at _start+0x18.
  _n_heads: equ _start+0x1a  ; dw .head_count value. It will be autodected with int 13h AH==8. shoemain(...) in /shoelace expects _n_heads to be at _start+0x1a.
  _hidden: dd 0  ; dd .hidden_sector_count value. LBA sector offset of the start of the partition. Must be initialized to 0. shoemain(...) in /shoelace expects _hidden to be at _start+0x1c.
  %if _hidden!=_start+0x1c
    %error ERROR_BAD_LOCATION_FOR_HIDDEN
    times -1 nop
  %endif
%elif MINIX
  IMAGE_SEG equ 0x60  ; We will load the /minix image here (at IMAGE_SEG:0).
  ; Leave ((0x9000-0x60)<<4)/1024 == 574.5 KiB for the Minix kernel. Typical
  ; Minix 1.5 i86 kernels images are <154 KiB. Typical Minix 1.5 i386
  ; kernel images are <497 KiB (most of these are BSS NUL bytes).
  BOOT_COPY_SEG equ 0x9000
		xor di, di
		mov ds, di
		mov si, 0x7c00  ; The BIOS has loaded a sector starting at _start from the MBR here.
		cli  ; Some broken early 8086 CPUs can process an interrupts after `mov ss, ax'.
		mov sp, [byte si+.boot_copy_seg-_start] ; BOOT_COPY_SEG is also good for SP: 0x9000 bytes of total size (code + data + BSS + stack) is enough for this bootloader.
		mov ss, sp
		sti
		mov es, sp
		cld
		mov cx, SECTOR_SIZE>>1
		rep movsw  ; Copy the MBR code from 0:0x7c00 to BOOT_COPY_SEG:0. After the copy, set CS := DS := SS := BOOT_COPY_SEG. ES floats (i.e. it's arbitrary).
		mov ds, sp
		xor di, di  ; Workaround for buggy BIOS doing int 13h AH==8. Also the 0 value will be used later.
		mov [byte di+_diskcode], dl  ; code for boot disk
		test dl, dl
		jns short fatal_lost  ; On floppy.
		mov es, di  ; Workaround for buggy BIOS.
		push dx  ; The AH==8 call below overwrites DL.
		mov ah, 8  ; Read drive parameters. Modifies CF, AH, DL, DH, CX, BL, ES, DI.
		jmp BOOT_COPY_SEG:cont_main-_start
  .boot_copy_seg: equ $-2
  _n_sectors: equ _start+0  ; dw .sectors_per_track value. It will be autodected with int 13h AH==8.
  _n_heads: equ _start+8  ; dw .head_count value. It will be autodected with int 13h AH==8.
%elif BOOT
  IMAGE_SEG equ 0x1000  ; We will load the /boot image here (at IMAGE_SEG:0).
  MBR_LOAD_SEG equ 0x7c0  ; The BIOS has loaded our code (from _start) to MBR_LOAD_SEG:0.
		mov ax, MBR_LOAD_SEG  ; current code segment
		mov ds, ax  ; data addressable
		cli  ; Some broken early 8086 CPUs can process an interrupts after `mov ss, ax'.
		mov ss, ax  ; stack addressable
		mov sp, -MBR_LOAD_SEG<<4
		sti
		xor di, di  ; Workaround for buggy BIOS doing int 13h AH==8. Also the 0 value will be used later.
		mov [byte di+_diskcode], dl  ; code for boot disk
		test dl, dl
		jns short fatal_lost  ; On floppy.
		mov es, di  ; Workaround for buggy BIOS.
		push dx  ; The AH==8 call below overwrites DL.
		; The BIOS may jump to 0:0x7c00, 0x7c0:0 or any combination. We don't care for CS, because we use only relative jumps. We've already set DS := MBR_LOAD_SEG == 0x7c0.
		mov ah, 8  ; Read drive parameters. Modifies CF, AH, DL, DH, CX, BL, ES, DI.
		jmp short cont_main
  _n_sectors: equ _start+0  ; dw .sectors_per_track value. It will be autodected with int 13h AH==8.
  _n_heads: equ _start+8  ; dw .head_count value. It will be autodected with int 13h AH==8.
%else
  %error ERROR_UNSUPPORTED_MODE_FOR_MAIN
  times -1 nop
%endif
_diskcode: equ _start+2  ; db .bios_boot_drive_number. shoemain(...) in /shoelace expects _diskcode to be at _start+2.
_filesize: equ _start+4  ; dd. long filesize; size of image file or directory. 4 bytes. Alternatively, this could also be in the BSS, but we have it here for fun.

fatal_lost:
		mov byte [_noimgname+4], 0  ; Just display 'Lost'.
fatal_lost_file:
		mov si, _noimgname
fatal_si:
.nextchar:
		lodsb
		test al, al  ; check for null char
		jz short .halt
		mov ah, 14  ; 14 = print char
		xor bx, bx
		int 0x10  ; call BIOS VIDEO_IO
		jmp short .nextchar
		cli
		hlt
.halt:
		jmp short .halt

; !! Can we fix floppy support in QEMU 2.11.1 running Minix 1.5 by populating dskbase (11 bytes at _start+0x20) with the correct DPT (https://fd.lod.bz/rbil/interrup/bios/1e.html#2483)?
;    shoemain(...) in /shoelace does memcpy(dskbase, &bpb[B_FPT_P], sizeof(dskbase));
cont_main:
; Find out the parameters of the hard disk if this is a hard disk
; boot. The only way to find out the number of heads and the number
; of sectors per track is to ask.
%if SHOELACE  ; Others have done it already.
		cld  ; direction is up
%endif
		int 0x13  ; BIOS disk syscall. This call changes ES and DI only if DL is a floppy drive.
		jc short fatal_lost

		and cx, byte 0x3f  ; isolate number of sectors
		mov [_n_sectors], cx
		mov cl, dh
		inc cx
		mov [_n_heads], cx  ; number of heads
		mov ah, 1  ; Get status of last drive operation. Needed after the AH == 8 call.
		pop dx  ; Restore DL (drive number).
		int 0x13  ; BIOS disk syscall.

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
		push ds  ; read next block into this segment
		pop es
.retry_sector_1_read:
		mov bx, SECTOR_SIZE  ; above boot sector
		mov dh, 0  ; head == 0.
		mov cx, 2  ; cylinder == 0, sector == 2 (i.e. the sector following the MBR).
		mov ax, 0x201  ; read one sector
		int 0x13
		jc short fatal_lost  ; This read is not retried. To retry, we could jump back to .retry_sector_1_read.
%if SHOELACE
		push ds  ; Segment used by _main(...) in /shoelace for getting e.g. _diskcode (see above).
		;push ax  ; Dummy value, popped by _main(...) in /shoelace. We don't push it, because the `call' below pushes a value.
		call _loadandrunimage
%elif MINIX+BOOT
		call _loadandrunimage
		; Fall through to jump_to_minix or jump_to_boot.
%else
  %error ERROR_UNSUPPORTED_MODE_FOR_JUMP_TO_LOAD
  times -1 nop
%endif

%if MINIX
  jump_to_minix:
		mov ax, 0xffff
		push ax  ; Minix boot_parameters.bp_processor. 88 would force 386 to 88. Default processor type for no restriction is 0xffff.
		mov bx, SCANCODE  ; Minix boot_parameters.bp_scancode. Scancode of '=' on the IBM PC keyboard. The Minix kernel expects it both boot_parameters and BX.
		push bx
		inc ax
		push ax  ; Minix boot_parameters.bp_ramsize. Ramdisk size. The value doesn't matter, because we are not booting from DEV_RAM (ramdisk).
		push ax  ; Minix boot_parameters.bp_ramimagedev. The value doesn't matter, because we are not booting from DEV_RAM (ramdisk).
		mov dx, DEV_HD8
		cmp byte [_diskcode], 0x81  ; Are we booting from the 2nd HDD?
		je short .hdd_done
		mov dl, DEV_HD4&0xff
  .hdd_done:
		push dx  ; Minix Minix boot_parameters.bp_rootdev. Root filesystem device. DX is ignored by the Minix kernel entry point.
		; xor ax, ax  ; Indicates that DS:SI points to boot_parameters. AX is already 0.
		mov cx, 0xa  ; Byte size of boot_parameters.
		mov di, ss  ; Segment of boot_parameters.
		mov si, sp  ; Offset  of boot_parameters.
		jmp IMAGE_SEG:0  ; Jump to the entry point of the kernel component of the Minix kernel image: MINIX in /usr/src/kernel/start.x.
%endif

%if BOOT
  jump_to_boot:
  ; Load protocol for /boot when booting from HDD:
  ; * Set ES:SI to point to partition entry in the partition table (0x10 bytes).
  ; * Set DL to the BIOS drive number (_diskcod, typically 0x80).
  ; * Jump to IMAGE_SEG:0x20 (for short a.out header, typical) or IMAGE_SEG:0x30 (for long a.out header, safer).
		mov dl, [_diskcode]
		push ds
		pop es
		mov si, bootable_partition
		jmp IMAGE_SEG:0x30  ; Jump to the entry point of the Minux >=1.6 boot monitor: boot in /usr/src/boot/boothead.S  . Offset 0x30 would also work.
%endif

; The boot code will use the tiny model in which code and data occupy
; the same 64kb segment. Because of the space constraints, the data
; used for the boot block cannot be placed in the DATA segment
; since this might force it beyond the 1kb boot sector limit.
;
; BSS stuff is allowed since we don't have to ensure it's initialised.
;
; For this reason, all data required by the INITIAL boot block is placed
; here (within the code segment).

_noimgname:	db 'Lost '
%if SHOELACE
  _imgname1:	db '/shoelace'
%elif MINIX
  _imgname1:	db '/minix'
%elif BOOT
  _imgname1:	db '/boot'
%else
  %error ERROR_UNSUPPORTED_MODE_FOR_IMGNAME1
  times -1 nop
%endif
_endimgname:	db NUL, LF, NUL

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
		push bp  ; make stack frame
		mov bp, sp
		push si
		push di
		; lbn *= BLOCK_SIZE/SECTOR_SIZE;
		mov ax, [bp+0x4]  ; get block number
		mov dx, [bp+0x6]
		shl ax, 1  ; left shift to make sector
		rcl dx, 1
%if 0  ; We don't need to add this, because the MBR always starts at LBA sector offset 0, thus _hidden == 0.
		add ax, [_hidden]  ; combine hidden sectors
		adc dx, [_hidden+2]
%endif
		xor si, si  ; no offset yet
		mov bh, [_n_sectors]  ; possible overlap
		jmp short readdosector

readnextsector:
		xor bh, bh  ; force overlap
		mov ax, [bp+0x4]  ; recover sector number
		mov dx, [bp+0x6]
		inc ax  ; next sector
		jnz short readdosector
		inc dx  ; propagate inc

readdosector:
		mov [bp+0x4], ax  ; save sector number
		mov [bp+0x6], dx
		div word [_n_sectors]  ; extract sector number;  TODO(pts): Add shorter code for the gemotry calculation.
		mov cl, dl  ; save sector number
		inc cl  ; sectors count from 1
		mov di, SECTOR_SIZE  ; read size
		mov bl, 0x1  ; read one sector
		cmp cl, bh  ; check for track overlap
		jnc short readoverlap  ; overlaps are read slowly
		inc bx  ; can read an extra sector
		add di, di  ; double read size

readoverlap:
; head = lbn / n_sectors % n_heads;
; cyl  = lbn / n_sectors / n_heads;
;
; This computation assumes that (n_cylinders-1)*n_heads < 65536 ie the
; 10 bit cylinder number multiplied by the number of heads results in
; a quantity that will fit in 16 bits.
		xor dx, dx  ; cast unsigned int to long
		div word [_n_heads]  ; extract head and cylinder number
		mov dh, dl  ; locate head number
		mov ch, al  ; ls part of cylinder number
		shr ax, 1  ; isolate ms part of cylinder number
		shr ax, 1
		and al, 0xc0  ; only two bits wanted
		or cl, al  ; combine with sector number
		mov dl, [_diskcode]  ; disk drive specifier

readblockerror:
		mov ah, 0x2  ; read from disk
		mov al, bl  ; sectors to read
		push ds  ; this segment
		pop es
		mov bx, [bp+0x8]  ; buffer pointer
		add bx, si  ; add in offset
		int 0x13  ; perform request
		jnc short readok  ; no error
		mov ah, 0x0  ; initialise
		int 0x13
		jmp short readblockerror

readok:
		add si, di  ; increase offset
		cmp si, 2*SECTOR_SIZE  ; 2 sectors per block
		jc short readnextsector
		; jmp short dsret  ; Fall through to dsret.

; C Function Exit
;
; The Minix ACK compiler generates calls to .dsret and
; friends when exiting from functions.
dsret:
		pop di  ; recover registers
sret:
		pop si
cret:
		mov sp, bp  ; recover old frame
		pop bp
		ret

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

; Check filesize
;
; The filesize is checked to see if it is less than, equal to
; or greater than zero. The function returns an int which is
; less than, equal to or greater than zero reflecting the value
; of filesize.
;
;       int checkfilesize(void)
;
; This function also sets the FLAGS based on `or ax, ax', where
; AX is is the result.
;
; This function doesn't modify any other register than AX or FLAGS.
_checkfilesize:
		mov ax, [_filesize]  ; low order filesize
		or al, ah  ; combine bytewise
		xor ah, ah  ; knock out high byte
		or ax, [_filesize+2]  ; combine ms word
		ret

; Code in /usr/oz/shoelace/shoeasm.x ends here.

; The original bootlace source is a mixture of 8086 assembly and C. The C
; code snippets below are based on that code, but both the C code snippets
; and the assembly counterparts have been modified for both clarity (with extra comments). and
; functionality. They still correspond to each other.

; typedef struct buffer_t { unsigned char buf[0x400]; } buffer_t;
; typedef unsigned short inode_nr;
; typedef unsigned short zone_nr;
; typedef inode_nr (*INODEFN)(buffer_t *bufp);
; #define NR_ZONE_NUMS 9  /* # zone numbers in an inode */
; #define SUPER_BLOCK 1
; #define ROOT_INODE 1
; #define SUPER_MAGIC 0x137f  /* Value for s_magic in minix1 */
; #define UNSUPPORTED_SUPER_MAGIC_V2 0x2468  /* Value for s_magic in minix2. */
; struct super_block {  /* At start of block SUPER_BLOCK. */
;   ino_t s_ninodes;              /* # usable inodes on the minor device */
;   zone_nr s_nzones;             /* total device size, including bit maps etc */
;   unshort s_imap_blocks;        /* # of blocks used by inode bit map */
;   unshort s_zmap_blocks;        /* # of blocks used by zone bit map */
;   zone_nr s_firstdatazone;      /* number of first data zone */
;   short int s_log_zone_size;    /* log2 of blocks/zone */
;   off_t s_max_size;             /* maximum file size on this device */
;   short s_magic;                /* magic number to recognize super-blocks */
; };
; typedef struct {  /* inode, as stored on disk. 0x20 bytes. */
;   unsigned short i_mode;                /* file type, protection, etc. */
;   unsigned short i_uid;                 /* user id of the file's owner */
;   unsigned long  i_size;                /* current file size in bytes */
;   unsigned long  i_mtime;               /* when was file data last changed */
;   unsigned char  i_gid;                 /* group number */
;   unsigned char  i_nlinks;              /* how many links to this file */
;   zone_nr        i_zone[NR_ZONE_NUMS];  /* block nums for direct, ind, and dbl ind */
; } d_inode;
; typedef struct {  /* Directory entry, as stored on disk. 0x10 bytes. */
;   ino_t d_inum;                 /* inode number */
;   char d_name[NAME_MAX];        /* character string */
; } dir_struct;
; extern const char imgname[];
; extern long filesize;

; In /usr/oz/shoelace/shoebfs.c
;
; The intention of this code is to provide a root directory scanner
; and file reader for the boot code in as small a space as possible.
; This forms a basic file system scanner.
;
; /* Scans the specified zones taking note of the level of indirection,
;  * calling the function `fn' for each block (1 KiB) in the zone.
;  *
;  * Indirection is taken care of by recursion. Processing of bottom-level
;  * zones is done by calling the service routine by function pointer `fn'.
;  * Stops at the first nonzero return value of `fn' (and propagates it), or
;  * returns 0 at the end of the zone.
;  *
;  * Assumes that n > 0. The outer loop will execute a silly number
;  * of times if this is not true.
;  */
; inode_nr scanzone(zone_nr *zp, int level, int n, INODEFN fn) {
;   long b;                             /* current block */
;   int i;                              /* index */
;   inode_nr v;                         /* return value from function */
;   buffer_t tb;                        /* zone reading buffer */
;   do {
;     b = zoneblock(*zp++);
;     i = zsize;
;     do {
;       readblock(b++, (buffer_t*)&tb[0]);
;       if ((v = level ? scanzone((zone_nr*)tb, level - 1,
;                                 sizeof(tb)/sizeof(zone_nr), fn)
;                      : (*fn)(tb)) != 0) {
;         return v;
;       }
;     } while (--i);
;   } while (--n);
;   return 0;
; }
;
; This _scanzone implementation receives its argument zp in register BX.
;
; This _scancode implementation doesn't receive or pass on its argument fn.
; It is passed via self-modifying code in the body of
; _procdir_or_procimagefile.
;
; This _scanzone implementation preserves the value of BX.
_scanzone:
		push bp
		mov bp, sp
		sub sp, 0x400  ; Large (>1 KiB) stack usage because of the tp buffer of size 0x400 bytes. Also, the function is recursive.
		push si  ; Save.
		push di  ; Save.
		push bx  ; Save.
.1:
		mov ax, [bx]  ; AX := zone number.
		times 2 inc bx  ; BX (zp) += sizeof(zone_nr) (== 2).
		push bx  ; Save BX (zp).

		;push word [bx]
		; call _zoneblock  ; We inline it.
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
;_zoneblock:
		mov cx, 0  ; Self-modifying code: the constant here will be modified.
_zsize: equ $-2  ; unsigned int zsize;  zone size  ; 2 bytes.
		mul cx  ; DX:AX := zoneblock(zp).

		mov si, ax
		mov di, dx
.2:
		push cx  ; Save CX (i).
		;mov ax, sp  ; This would work only without `push si' and `push di' above. With them, reorganizing it doesn't bring any savings.
		lea ax, [bp-0x400]  ; tb.
		push ax  ; Save tb pointer value.
		push ax
		push di
		push si
		add si, byte 1
		adc di, byte 0
		call _readblock
		add sp, byte +0x6
		pop bx  ; Restore BX := tb pointer value.
		mov ax, [bp+0x6-2]
		sub ax, strict word 1
		jc short .3
		;push bx  ; No need to save BX (tb), the `call _scanzone' below is special, and doesn't ruin it.
		mov cx, 0x200  ; sizeof(tb) / sizeof(zone_nr).
		;push fn_arg  ; Not received, not passed on.
		push cx
		push ax
		;push bx  ; Pass argument zp of _scanzone below in BX.
		call _scanzone
		times 2 pop cx  ; Clean up 2 arguments of _scanzone above from the stack.
		;pop bx  ; No need to restore BX (tb).
.3:
		;mov bx, bx  ; The argument `tb' of _procdir_or_procimagefile is passed in register BX.
		call _procdir_or_procimagefile  ; (*fn)(tb). The argument `fn' of _procdir_or_procimagefile is passed implicitly via self-modifying code in the body of _procdir_or_procimagefile.
		pop cx  ; Restore CX (i).
		test ax, ax
		jnz short .6
		loop .2  ; CX -= 1. Jump iff the new CX is 0.
		pop bx  ; Restore BX (zp).
		dec word [bp+0x8-2]
		jnz short .1
.6:
		pop bx  ; Restore for the caller.
		jmp strict short dsret

%macro emit_full_partition 0
  %00:
  bootable_partition: equ $
  .status:	db 0x80  ; Bootable.
  .head:	db 0
  .sector:	db 1
  .cylinder:	db 0
  .type:	db 0x81  ; Minix.
  .last_head:	db 16
  .last_sector:	db -1
  .last_cylinder: db -1
  .lba_first:	dd 0
  .lba_count:	dd 0x7fffffff ; Minix can't use the partition if this value is negative.
%endm

%macro emit_empty_partition 0
  times 4 dd 0  ; Hide it from Linux fdisk(1).
%endm

%macro emit_partition_table 0
  times 0x1b8-($-_start) hlt
  %if SHOELACE
    disk_id_signature: db 'BotL'
  %elif BOOT
    disk_id_signature: db 'BotM'
  %else
    disk_id_signature: db 'MinX'
  %endif
  ;assert_at .header+0x1bc
  reserved_word_0: dw 0
  ;assert_at .header+0x1be
  %if SHOELACE+MINIX
    ; Minix 1.5 and ShoeLace sort partitions by start LBA (signed), but they explicitly put the ==0 value last.
    ; So our partition will end up being /dev/hd4. So we put it to the correct spot.
    partition_1: emit_empty_partition
    partition_2: emit_empty_partition
    partition_3: emit_empty_partition
    partition_4: emit_full_partition  ; Minix 1.5 and ShoeLace will find it.
  %elif BOOT
    partition_1: emit_full_partition  ; initialize(...) in /usr/src/boot/boot.c will find the first one.
    partition_2: emit_empty_partition
    partition_3: emit_empty_partition
    partition_4: emit_empty_partition
  %else
    %error ERROR_UNSUPPORTED_MODE_FOR_EMIT_PARTITION_TABLE
    times -1 nop
  %endif
  ;assert_at .header+0x1fe
  boot_signature: dw BOOT_SIGNATURE
  ;assert_at .header+0x200
%endm

%if MINIX
  emit_partition_table  ; Emit it before _procimagefile, because we otherwise _procimagefile wouldn't fit to the MBR sector.
%endif

; /* Processes the next (1 KiB) read from the image file. Copies the first
;  * block to to _bufptr, and copies later blocks consecutively. (There are
;  * no bound checks.)
;  *
;  * Returns 2 * ROOT_INODE (== 2) if the end-of-file has been reached (i.e.
;  * filesize has decreased to 0); otherwise 0. 0 means that the caller
;  * should continue reading the next block, and call again.
;  */
; inode_nr procimagefile(buffer_t *bufp) {
;   unsigned int hsize;  /* Size of a.out header, for SHOELACE. */
;   hsize = (startload == 0) ? 0 : ((struct exec*)bufp)->a_hdrlen;  /* This is the behavior for SHOELACE. See the assembly code for MINIX. */
;   startload = 0;
;   copyviabufptr((char*)bufp + hsize, sizeof(*bufp) - hsize);
;   filesize -= sizeof(*bufp);
;   return checkfilesize() <= 0 ? 2 * ROOT_INODE : 0;
; }
;
; The argument `bufp' is passed in register BX.
;
; The starting code of the function is shared with _procdir_or_procimagefile, so we don't include it here.
;_procimagefile:
;		push si
;		push di
;		xor ax, ax
;		cmp al, 1  ; Self-modifying code: the constant 1 will be modified to 0 below.
;_startload: equ $-1  ; unsigned char startload = 1;
_procimagefile.in:
		xor ax, ax
		xor cx, cx
		cmp al, 1  ; Self-modifying code: the constant 1 will be modified to 0 below.
_startload: equ $-1  ; unsigned char startload = 1;
		je short .hsize_and_filesize_ok
		dec byte [_startload]  ; _startload := 0.
; Here we compute hsize (in register AX) and _filesize. The default values
; (hsize == 0, _filesize is the image file size read from the inode, i.e.
; struct_stat.st_size). If the default is good, we don't have to change anything.
;
; Using hsize and filesize we can control which part of the file will be loaded
; to IMAGE_SEG:0 : _filesize-hsize bytes starting at file offset hsize will
; be loaded. The defaut is to load the entire image file.
;
; To compute hsize and _filesize, we can use the header fields within the
; first 0x400 bytes of the image files, already read at DS:BX. We must not
; increase filesize here. We can ruin registers CX, DX, SI, DI and FLAGS. We
; must keep BP and BX (bufp) intact, and we must set AX to our desired
; hsize.
%define CL_IS_ZERO 1
%if SHOELACE
  ; For SHOELACE, we limit _filesize to min(_filesize, a_hdrlen + a_text +
  ; a_data), and skip the a.out executable header (of size a_hdrlen,
  ; typically 0x20) at the beginning. The first limitation skips about 12
  ; KiB of symbols at the end of /shoelace.
  %define CL_IS_ZERO 1  ; Enable optimization below.
		mov al, [bx+4]  ; AX := hsize := ((struct exec*)bufp)->a_hdrlen. Typically 0x20. AH is already 0.
		cmp [bx+8+2], cx  ; High word of a_text.
		jnz short .hsize_and_filesize_ok  ; Don't limit if a_text is >=0x10000 bytes. Typically a_text is 0x6e40 bytes.
		; We don't have space in the MBR sector for the following check.
		;cmp [bx+0xc+2], cx  ; High word of a_data.
		;jnz short .hsize_and_filesize_ok  ; Don't limit if a_data is >=0x10000 bytes. Typically a_data is 0x1cd4 bytes.
		mov dx, ax
		add dx, [bx+8]  ; DX := ((struct exec*)bufp)->a_text. It usually fits to a 16-bit word.
		jc short .hsize_and_filesize_ok  ; If it overflows, don't limit _filesize. Unfortunately we have no space for this check in the MBR sector.
		add dx, [bx+0xc]  ; DX ++= ((struct exec*)bufp)->a_data. It usually fits to a 16-bit word.
		jc short .hsize_and_filesize_ok  ; If it overflows, don't limit _filesize. Unfortunately we have no space for this check in the MBR sector.
		mov si, _filesize
		cmp word [si+2], cx  ; CX is already 0.
		jnz short .large_enough  ; High word is nonzero, file size is >0x10000 bytes. This is very unusual, typical /shoelae file size is 47 KiB.
		cmp word [si], dx
		jbe short .hsize_and_filesize_ok  ; _filesize is already too small, don't limit.
.large_enough:
		mov [si+2], cx  ; Set high word of _filesize to 0.
		mov [si], dx  ; Decrease (set) low word of _filesize to DX.
%endif
%if MINIX
  ; For MINIX, we set hsize to 0x200 to skip the bootblok at the beginning
  ; of the Minix kernel image.
  ;
  ; We also limit _filesize to min(_filesize, (menu_cs - 0x40) << 4) to skip
  ; the menu code and data at the end of the image (~14.5 KiB). We get
  ; menu_cs from the first sector. The structure is one of these:
  ;
  ; * DS:BP+0x1f6: dw final, menu_ds, menu_pc, menu_cs, bootsig
  ; * DS:BP+0x1f6: dw ?      final,   menu_ds, menu_pc, menu_cs
  ;
  ; Here bootsig is 0xaa55, menu_cs > 0x60, menu_ds >= menu_cs, menu_pc ==
  ; 0. (Some Minix kernels have menu_ds == menu_ds, some have menu_ds >
  ; menu_cs.)
  %define CL_IS_ZERO 1  ; Enable optimization below.
		lea di, [bx+0x200-2]
		cmp word [di], BOOT_SIGNATURE
		jne short .no_bootsig
		cmp word [di-4], cx  ; Is menu_pc == 0?
		mov ax, [di-2]  ; DX := menu_cs.
		mov di, [di-6]  ; DI := menu_ds.
		jmp short .have_menu_cs_and_ds
.no_bootsig:
		cmp word [di-2], cx  ; Is menu_pc == 0?
		jne short .filesize_ok  ; Bad values at the end of the boot sector, don't limit _filesize.
		mov ax, [di]  ; DX := menu_cs.
		mov di, [di-4]  ; DI := menu_ds.
.have_menu_cs_and_ds:
		cmp di, ax
		jb short .filesize_ok  ; menu_ds < menu_cs, probably all values are bad, don't limit _filesize.
		cmp ax, strict word 0x60
		jbe short .filesize_ok  ; menu_cs <= 0x60, probably all values are bad, don't limit _filesize.
.calc_filesize_based_on_menu_cs:
		sub ax, strict word 0x40
		mov di, 0x10
		mul di  ; DX:AX (filesize_limit) := AX * DI == (menu_cs - 0x40) << 4.
		mov si, _filesize
		cmp dx, [si+2]
		ja short .filesize_ok  ; High word of filesize_limit larger than _filesize, so we can't decrease it.
		jb short .large_enough
		cmp ax, [si]
		jae short .filesize_ok  ; High word of filesize_limit is equal, low word is larger or equal than _filesize, so we can't decrease it.
.large_enough:
		mov [si+2], dx  ; Set high word of _filesize, decreasing it.
		mov [si], ax    ; Set low  word of _filesize, decreasing it.
.filesize_ok:
		mov ax, 0x200  ; Set hsize to 0x200 to skip the bootblok at the beginning of the Minix kernel image.
%endif
%if BOOT
  %define CL_IS_ZERO 1  ; Enable optimization below.
  ; TODO(pts): Load only a_hdrlen + a_text + a_data bytes (no a_syms etc.). This optimization is not needed, because /boot typically doesn't contain symbols.
%endif
.hsize_and_filesize_ok:
%if CL_IS_ZERO
		mov ch, 0x400>>8  ; sizeof(buffer_t) == 0x400.
%else
		mov cx, 0x400  ; sizeof(buffer_t).
%endif
%undef CL_IS_ZERO
		sub cx, ax
		xchg si, ax  ; SI := AX; AX := junk.
		add si, bx  ; SI := (char*)bufp + hsize.
		; call _copyviabufptr  ; We inline this call.
		mov dx, IMAGE_SEG  ; Self-modifying code: the constant here will be modified.
_bufptr_seg: equ $-2
		mov es, dx
		mov di, 0  ; Self-modifying code: the constant here will be modified.
_bufptr_ofs: equ $-2
		add di, di
		jnc short .di_is_small_enough
		add dh, 0x8000>>12  ; Move offset overflow to segment. Also sets CF := 0.
		mov es, dx
		mov word [_bufptr_seg], dx
.di_is_small_enough:
		shr di, 1
		rep movsb  ; Copy sizeof(*bufp)-hsize bytes from bufp to the image output buffer (_bufptr).
		mov [_bufptr_ofs], di
		sub byte [_filesize+1], 0x400>>8  ; sizeof(buffer_t) == 0x400.
		sbb word [_filesize+2], cx  ; CX is 0 now.
		xor si, si
		call _checkfilesize
		;or ax, ax  ; Not needed, _checkfilesize has set the FLAGS.
		jg short .2
		times 2 inc si  ; SI := 2*ROOT_INODE.
.2:
		xchg ax, si  ; AX := SI; SI := junk.
		pop di  ; Restore.
		pop si  ; Restore.
		ret

%if SHOELACE+BOOT
  emit_partition_table
%endif
%if BOOT
  boot_parameter_sector:  ; PARAMSEC == 1, i.e. this sector follows the boot sector. For us, it follows the MBR sector. /boot reads it in get_parameters() called from boot() in /usr/src/boot/boot.c.
  db 'rootdev=bootdev', LF
  %ifndef BOOTINET  ; Use `nasm -DBOOTINET' to enable networking.
    db 'inet=none', LF
  %endif
  db 'boot', LF  ; Make it autoboot, withint entering the boot monitor `menu'.
  times 0x40-(boot_parameter_sector-$) db LF  ; Leave room for manual edits. Unfortunately, the `save' command would destroy the rest of our code, overwriting it with LFs.
  db NUL  ; onetoken(...) in /usr/src/boot stops at the first non-LF byte in the 0x00..0x1f.
%endif

; /* Processes the next block of a directory by searching for an entry named
;  * in the NUL-terminated string imgname. It will be called with the root
;  * directory (/) only.
;  *
;  * Returns the inode number of the imgname as soon is at has been found;
;  * otherwise ROOT_INODE (== 1) if the end-of-directory has been reached
;  * without finding imgname; otherwise 0. Please note that 0 is an invalid
;  * inode number. 0 means that the caller should continue reading the next
;  * block, and call again.
;  *
;  * There is potential for ambiguity since a return value of ROOT_INODE
;  * could mean that a the root directory has been found or that the
;  * required entry is not present. The caller should check filesize <= 0,
;  * which indicates the latter if true.
;  */
; inode_nr procdir(buffer_t *bufp) {
;   char *ip;                           /* name comparison pointer */
;   char *ep;                           /* directory entry name */
;   dir_struct *edp;                    /* end of buffer */
;   inode_nr thisnode;                  /* current inode */
;   for (edp = (dir_struct*)bufp + sizeof(*bufp) / sizeof(dir_struct);
;        (dir_struct*)bufp < edp;
;        ((dir_struct*)bufp)++) {
;     if ((thisnode = ((dir_struct*)bufp)->d_inum) != 0) {
;       for (ip = imgname, ep = ((dir_struct*)bufp)->d_name; *ip++ == *ep; ) {
;       if (*ep++ == 0)
;         return thisnode;
;       }
;     }
;     filesize -= sizeof(dir_struct);
;     if (checkfilesize() <= 0) return ROOT_INODE;
;   }
;   return 0;
; }
;
; The argument `bufp' is passed in register BX.
_procdir_or_procimagefile:
		push si
		push di
.or_opcode_byte:
		test ax, strict word _procimagefile.in-($+3)  ; Self-modifying code: to change the function pointer fn to _procimagefile, this opcode byte will be modified from this harmless `test ax, ...' (0xa9) to `jmp strict near' (0xe9).
_procdir.in:
		lea cx, [bx+0x400]  ; CX will keep holding edp.
		push ds
		pop es  ; For the `cmpsb' below.
.1:
		xor ax, ax  ; Function return value.
		cmp cx, bx
		jna short .6
		mov ax, [bx]  ; thisnode. This will also be the function return value.
		test ax, ax
		jz short .3
		mov si, _imgname1+1  ; +1 to skip over the leading `/'.
		lea di, [bx+2]  ; Skip over d_inum.
.2:
		cmpsb
		jne short .3
		cmp byte [di-1], 0x0
		jne short .2
		jmp short .6  ; Return the value of [bx].
.3:
		sub word [_filesize], byte 0x10
		sbb word [_filesize+2], byte 0
		call _checkfilesize  ; It doesn't modify ES, CX or DX.
		;or ax, ax  ; Not needed, _checkfilesize has set the FLAGS.
		jng short .4
		add bx, byte 0x10  ; sizeof(dir_struct).
		jmp short .1
.4:
		mov ax, 1  ; ROOT_INODE.
.6:
		pop di  ; Restore.
		pop si  ; Restore.
		ret

; Code in /usr/oz/shoelace/shoebfs.c ends here.

; In /usr/oz/shoelace/shoeboot.c
;
; The intention of this code is to provide a root directory scanner
; and file reader for the boot code in as small a space as possible.
; The code will read into memory the image file, which will
; then perform the boot.

; void loadandrunimage(void) {
;   inode_nr node;        /* inode of /, then inode of image */
;   block_nr inodeblock;  /* block where inodes starts */
;   int nodeidx;          /* index to node */
;   INODEFN fn;           /* data disposition */
;   int j;                /* index */
;   zone_nr *zp;          /* zone number pointer */
;   int len;              /* number of zones in list */
;   int indirection;
;   /* Get parameters from the disk superblock */
;   readblock((long) SUPER_BLOCK, (buffer_t*)&blockbuf[0]);
;   zsize = 1 << ((struct super_block *) blockbuf)->s_log_zone_size;
;   inodeblock = SUPER_BLOCK + 1 +
;                ((struct super_block *) blockbuf)->s_imap_blocks +
;                ((struct super_block *) blockbuf)->s_zmap_blocks;
;   /* Locate the image, then load it. */
;   node = ROOT_INODE;
;   fn = procdir;  /* For the first loop iteration (/). */
;   /* Do the loop body twice: once for / and once for the file. */
;   for (j = 2; j >= 1; --j) {
;     readblock((unsigned long) (inodeblock +
;                                (--node)/(BLOCK_SIZE/sizeof(d_inode))),
;               (buffer_t*)&blockbuf[0]);
;     nodeidx = node % (BLOCK_SIZE / sizeof(d_inode));
;     filesize = ((d_inode*)blockbuf)[nodeidx].i_size;
;     zp = &((d_inode*)blockbuf)[nodeidx].i_zone[0];
;     indirection = 0; len = (NR_ZONE_NUMS - 2);  /* Direct zones. */
;     while (indirection < 3 &&
;            (node = scanzone(zp, indirection, len, fn)) == 0) {
;       zp += len; ++indirection; len = 1;
;     }
;     if (node <= ROOT_INODE) fatal("Lost /", imgname, NULL);
;     fn = procimagefile;  /* For the second loop iteration (the file). */
;   }
; }
;
; Helper function which uses the stack frame (BP) of _loadandrunimage. See
; the commentn of _loadandrunimage for description and register usage.
_loadandrunimage.process_inode:
		; Now AX is node-1; BP is inodeblock.
		push ax  ; Save node.
		mov cl, 5
		shr ax, cl
		add ax, bp  ; AX += BP (inodeblock).
		xor cx, cx
		mov si, _blockbuf
		push si
		push cx  ; 0. High word of the lbn argument of readblock is 0 when reading the nodes.
		push ax
		call _readblock
		add sp, byte +0x6
		pop bx  ; Restore BX := node.
		and bx, byte 0x1f
		mov cl, 5
		shl bx, cl
		mov cx, [si+bx+4]
		mov [_filesize], cx
		mov cx, [si+bx+6]
		mov [_filesize+2], cx
		lea bx, [si+bx+0xe]  ; Set BX (zp).
		xor si, si  ; indirection.
		mov di, 7  ; len.
.3:
		;push bx  ; No need to save BX (zp), the `call _scanzone' below is special, and doesn't ruin it.
		;push fn_arg  ; fn. Not passed.
		push di  ; len.
		push si  ; indirection.
		;push bx  ; Pass argument zp of _scanzone below in BX (zp).
		call _scanzone
		times 2 pop cx  ; Clean up 2 arguments of _scanzone above from the stack.
		;pop bx  ; No need to restore BX (zp).
		; Now: AX (node) has been updated by the return value of _scanzone.
		test ax, ax
		jnz short .4
		times 2 add bx, di  ; BX (zip) += 2 * DI.
		mov di, 1
		inc si  ; indirection.
		cmp si, byte 3  ; indirection.
		jne short .3
.4:
		ret
_loadandrunimage:
		;push bp  ; No need to save BP, this function doesn't return.  ; In this function, BP is not used as a frame pointer, but it will be used for local variable inodeblock below.
		;push si  ; No need to save SI, this function doesn't return.
		;push di  ; No need to save DI, this function doesn't return.
		xor ax, ax
		mov si, _blockbuf
		push si  ; _blockbuf. Argument 2 of _readblock below.
		push ax  ; 0. High word of SUPER_BLOCK. Argument 1 of _readblock below.
		inc ax  ; BP := 1.
		push ax  ; 1. Low word of SUPER_BLOCK. Argument 1 of _readblock below.
		call _readblock
		;  !! Add support for minix2 filesystem.
		add sp, byte +0x6
		mov cx, [si+0xa]  ; s_log_zone_size. Typically it's 0. 0 means: 1 zone == 1 block == 1 KiB. 5 means: 1 zone == (1<<5) blocks == 32 KiB.
		mov ax, 1
		shl ax, cl
		mov [_zsize], ax
		mov bp, [si+4]
		times 2 inc bp
		add bp, [si+6]  ; Set BP (inodeblock) to its final value.
		xor ax, ax  ; AX (node) := ROOT_INODE - 1 == 0.
		call .process_inode  ; ROOT_INODE (/): AX == ROOT_INODE - 1.
		sub ax, strict word 1  ; Sets AX -= 1, and also updates the files with respect to `cmp ax, strict word 1'.
		ja short .found_image_file  ; Jump iff node > ROOT_INODE.
		mov byte [_endimgname], CR  ; The message would be equally comprehensible without the CRLF at the end.
		jmp strict near fatal_lost_file  ; Doesn't return.
.found_image_file:
		mov byte [_procdir_or_procimagefile.or_opcode_byte], 0xe9  ; Self-modifying code: to change the function pointer fn to _procimagefile, we modify the opcode byte at _procdir_or_procimagefile.or_opcode_byte from `test ax, ...' (0xa9) to `jmp strict near' (0xe9).
		call .process_inode  ; inode of the image file. AX == inode number of the file - 1.
		; Fall through to jump_to_image.

jump_to_image:
%if SHOELACE
		jmp IMAGE_SEG:0  ; Jump to _main(...) in /shoelace. The BPB segment (DS, with _nsectors) and the dummy offset value has already been pushed by the `call _loadandrunimage' above.
%elif MINIX+BOOT
		ret  ; jmp near jump_to_minix  ; The actual code is in the MBR sector, because there is enough space there.
%else
  %error ERROR_UNSUPPORTED_MODE_FOR_JUMP_TO_IMAGE
  times -1 nop
%endif

%if $-_start>0x400
  %error ERROR_BOOTLACE_TOO_LONG
  times -1 nop
%endif

absolute $  ; BSS.
		resb (_start-$)&1  ; Aligment.
_blockbuf:	resb 0x400  ; buffer_t blockbuf;  ; block buffer. 0x400 bytes.

; No more global variables, we have them all. Total size: a_bss == 0x40a bytes.

; __END__
