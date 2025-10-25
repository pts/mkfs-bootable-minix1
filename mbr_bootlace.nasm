;
; mbr_bootlace.nasm: Minix 1.5 bootlace boot code that can be put to a HDD MBR
; MBR-specific code and size optimizations by pts@fazekas.hu at Fri Oct 24 19:14:53 CEST 2025
;
; Compile with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -o mbr_bootlace.bin mbr_bootlace.nasm
;
; Based on the following Minix 1.5 source files:
;
; * /usr/oz/shoelace/bootlace.x (1990-04-24)
; * /usr/oz/shoelace/shoeasm.x (1990-04-24)
; * /usr/oz/shoelace/shoebfs.c (1990-04-24)
; * /usr/oz/shoelace/shoeboot.c (1990-04-24)
;
; This boot code can boot Minix 1.5 (both 8086 and i386) from a HDD which
; doesn't have partitions, but the minix1 filesystem is created to span over
; the entire HDD (i.e. /dev/hd0). Here is how you should prepare such a
; bootable filesystem image:
;
; 1. Create the Minix filesystem.
; 2. Copy the file named shoelace of the Shoelace boot manager to the root
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

bits 16
cpu 8086

VECTORSEG      equ 0           ; vector segment
LOADSEG        equ 0x7c0       ; here the boot block itself is loaded
BOOTSEG        equ 0x1000      ; here it will copy itself (64k-127k)
SECTORSIZE     equ 512         ; size of disk sector
BOOTSIZE       equ SECTORSIZE  ; size of boot code
FDTABLEVECTOR  equ 0x1e        ; vector to floppy parameters

BOOT_SIGNATURE equ 0xaa55  ; dw.

; Program entry point
;
; When the PC is powered on, it reads the first 0x200 bytes from the
; HDD into address 0x7c00 and jumps to it.  This is the code that
; is contained in that first sector.
;
; On entry the following register contents are assumed:
;
;       dl      BIOS drive number (80h hard disk, 00h floppy disk, etc.)
_start:
_main:
; The drive number is still in dl. es:si points at
; partition table entry if hard disk boot.
		cld  ; direction is up
		mov ax, LOADSEG  ; current code segment
		mov ds, ax  ; data addressable
		cli  ; Some broken early 8086 CPUs can process an interrupts after `mov ss, ax'.
		mov ss, ax  ; stack addressable
		mov sp, -LOADSEG<<4
		sti
; Find out the parameters of the hard disk if this is a hard disk
; boot. The only way to find out the number of heads and the number
; of sectors per track is to ask.
		mov [_diskcode], dl  ; code for boot disk
		test dl, dl
		jns short fatal_lost  ; On floppy.
		xor di, di  ; Workaround for buggy BIOS. Also the 0 value will be used later.
		mov es, di  ; Workaround for buggy BIOS.
		push dx  ; The AH==8 call below overwrites DL.
		; The BIOS may jump to 0:0x7c00, 0x7c0:0 or any combination. We don't care for CS, because we use only relative jumps. We've already set DS := LOADSEG == 0x7c0.
		jmp short cont_main
		times 0x1c-($-_start) hlt  ; shoemain(...) in /shoelace expects _hidden to be at _start+0x1c.
_diskcode: equ _start+2  ; db .bios_boot_drive_number. shoemain(...) in /shoelace expects _diskcode to be at _start+2.
_n_sectors: equ _start+0x18  ; dw .sectors_per_track value. It will be autodected with int 13h AH==8. shoemain(...) in /shoelace expects _n_sectors to be at _start+0x18.
_n_heads: equ _start+0x1a  ; dw .head_count value. It will be autodected with int 13h AH==8. shoemain(...) in /shoelace expects _n_heads to be at _start+0x1a.
_hidden:	dd 0  ; dd .hidden_sector_count value. LBA sector offset of the start of the partition. Also called the number of hidden sectors. shoemain(...) in /shoelace expects _hidden to be at _start+0x1c.
%if _hidden!=_start+0x1c
  %error ERROR_BAD_LOCATION_FOR_HIDDEN
  times -1 nop
%endif
fatal_lost:
		mov si, _noshoename
		mov byte [si+4], 0  ; Just display 'Lost'.
_fatal:
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
		mov ah, 8  ; Read drive parameters. Modifies CF, AH, DL, DH, CX, BL, ES, DI.
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
readshoehorn:
		push ds  ; read next block into this segment
		pop es
		mov bx, BOOTSIZE  ; above boot sector
		mov dh, 0  ; head == 0.
		mov cx, 2  ; cylinder == 0, sector == 2 (i.e. the sector following the MBR).
		mov ax, 0x201  ; read one sector
		int 0x13
		jc short fatal_lost
		jmp near _shoehorn  ; shoe horn the code in

; The boot code will use the tiny model in which code and data occupy
; the same 64kb segment. Because of the space constraints, the data
; used for the boot block cannot be placed in the DATA segment
; since this might force it beyond the 1kb boot sector limit.
;
; BSS stuff is allowed since we don't have to ensure it's initialised.
;
; For this reason, all data required by the INITIAL boot block is placed
; here (within the code segment).

_noshoename:	db 'Lost /'
_lacename:	db 'shoelace'
_endshoename:	db 0, 0xa, 0

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
		add ax, [_hidden]  ; combine hidden sectors
		adc dx, [_hidden+2]
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
		mov di, SECTORSIZE  ; read size
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
		cmp si, 2*SECTORSIZE  ; 2 sectors per block
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
		push bp
		mov bp, sp
		sub sp, 0x408  ; word [bp-0x8] is also unused.
		push si
		push di
.1:
		mov bx, [bp+0x4]
		add word [bp+0x4], byte +0x2
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
		mov ax, [bx]  ; AX := zone number.
		mov cx, 0  ; Self-modifying code: the constant here will be modified.
_zsize: equ $-2  ; unsigned int zsize;  zone size  ; 2 bytes.
		mul cx  ; convert to blocks

		mov [bp-0x4], ax
		mov [bp-0x2], dx
		mov [bp-0x6], cx
.2:
		lea ax, [bp-0x408] ; Large (>1 KiB) stack usage.
		push ax
		push word [bp-0x2]
		push word [bp-0x4]
		add word [bp-0x4], byte +0x1
		adc word [bp-0x2], byte +0x0
		call _readblock
		add sp, byte +0x6
		cmp word [bp+0x6], byte +0x0
		jz short .3
		mov ax, [bp+0x6]
		dec ax
		push word [bp+0xa]
		mov cx, 0x200  ; sizeof(tb) / sizeof(zone_nr).
		push cx
		push ax
		lea ax, [bp-0x408]
		push ax
		call _scanzone
		add sp, byte +0x8
.3:
		lea bx, [bp-0x408]  ; The argument `tb' of _dirscan_or_readshoe is passed in register BX.
		cmp [bp+0xa], byte 0  ; _dirscan_or_readshoe will use the resulting FLAGS.
		call _dirscan_or_readshoe  ; (*fn)(tb). Instead of a function pointer, we use 0 for _dirscan and nonzero for _readshoe.
		test ax, ax
		jnz short .6
		dec word [bp-0x6]
		jnz short .2
		dec word [bp+0x8]
		jnz short .1
.6:
.j_dsret:
		jmp strict near dsret

		;times 0x1b8-35-($-_start) hlt  ; Move di_is_small_enough as late as possible, for the short jumps below.

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
;
; The argument `bp' is passed in register BX.
;
; The starting code of the function is shared with _dirscan_or_readshoe, so we don't include it here.
;_readshoe:
;		push si
;		push di
;.in:
;		xor ax, ax
;		cmp al, 1  ; Self-modifying code: the constant 1 will be modified to 0 below.
;_startload: equ $-1  ; unsigned char startload = 1;
_readshoe.in2:
		je short .1
		dec byte [_startload]  ; _startload := 0.
		mov al, [bx+0x4]
.1:
		mov cx, 0x400  ; sizeof(buffer).
		sub cx, ax
		add bx, ax
		; call _copyviabufptr  ; We inline this call.
; Copy local buffer
;
; This function copies a local buffer to an arbitrary location
; via bufseg:bufptr. On completion bufptr is updated. The
; calling sequence is:
;
;       void copyviabufptr(char *buf, unsigned int bytes)
;_copyviabufptr:
		; mov cx, cx  ; CX := bytes to copy; AX := junk.
		mov dx, BOOTSEG  ; Self-modifying code: the constant here will be modified.
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
		mov si, bx  ; pointer to buffer
		rep movsb
		mov [_bufptr_ofs], di
		sub word [_filesize], 0x400  ; sizeof(buffer).
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

		times 0x1b8-($-_start) hlt
disk_id_signature: db 'BotL'
;assert_at .header+0x1bc
reserved_word_0: dw 0
; Minix and Shoelace sort partitions by start LBA (signed), but they explicitly put the ==0 value last.
; So our partition will end up being /dev/hd4. So we put it to the correct spot.
partition_1:
		times 4 dd 0  ; Hide it from Linux fdisk(1).
partition_2:  ; Ugly enough so that Shoelace and Minix don't recognize it. Linux fdisk(1) prints it though.
		times 4 dd 0  ; Hide it from Linux fdisk(1).
partition_3:  ; Ugly enough so that Shoelace and Minix don't recognize it. Linux fdisk(1) prints it though.
		times 4 dd 0  ; Hide it from Linux fdisk(1).
partition_4:  ; Partition 1. This is a fake partition which spans the entire HDD.
;assert_at .header+0x1be
.status:	db 0x80  ; Bootable.
.head:		db 0
.sector:	db 1
.cylinder:	db 0
.type:		db 0x81  ; Minix.
.last_head:	db 16
.last_sector:	db -1
.last_cylinder:	db -1
.lba_first:	dd 0
.lba_count:	dd 0x7fffffff ; Minix can't use the partition if this value is negative.
;assert_at .header+0x1fe
boot_signature: dw BOOT_SIGNATURE
;assert_at .header+0x200

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
;
; The argument `bp' is passed in register BX.
_dirscan_or_readshoe:
		push si
		push di
		jz short _dirscan.in  ; We use this inestead of a function pointer.
_readshoe.in: equ $
		xor ax, ax
		cmp al, 1  ; Self-modifying code: the constant 1 will be modified to 0 below.
_startload: equ $-1  ; unsigned char startload = 1;
		jmp near _readshoe.in2
_dirscan.in:
		lea cx, [bx+0x400]  ; CX will keep holding edp.
.1:
		xor ax, ax  ; Function return value.
		cmp cx, bx
		jna short .6
		mov dx, [bx]
		test dx, dx
		jz short .3
		mov si, _lacename
		lea di, [bx+0x2]
.2:
		lodsb
		cmp al, [di]
		jne short .3
		inc di
		cmp byte [di-1], 0x0
		jne short .2
		xchg ax, dx  ; AX := DX; DX := junk.
		jmp short .6
.3:
		sub word [_filesize], byte 0x10
		sbb word [_filesize+2], byte 0
		call _checkfilesize  ; It doesn't modify CX or DX.
		;or ax, ax  ; Not needed, _checkfilesize has set the FLAGS.
		jng short .4
		add bx, byte +0x10
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
		;push bp  ; No need to save BP, this function doesn't return.
		mov bp, sp
		sub sp, byte +0x10  ; Local variables word [bp-0x6], word [bp-0x10], word [bp-0xe], word [bp-0x2] are unused.
		;push si  ; No need to save SI, this function doesn't return.
		;push di  ; No need to save SI, this function doesn't return.
		mov si, _blockbuf
		push si
		xor ax, ax
		mov word [bp-0x8], ax  ; Set to 0, indicate _dirscan as the ake function pointer.
		push ax
		mov ax, 1  ; SUPER_BLOCK.
		push ax
		call _readblock
		add sp, byte +0x6
		mov cx, [si+0xa]
		mov ax, 1
		shl ax, cl
		mov [_zsize], ax
		mov cx, [si+4]
		times 2 inc cx
		add cx, [si+6]
		mov [bp-0x4], cx
		mov byte [bp-0xa], 3  ; j.
		xor ax, ax  ; AX (minixnode) := 1 - 1.
.1:
		; Now AX is minixnode.
		push ax  ; Save minixnode.
		mov cl, 5
		shr ax, cl
		add ax, [bp-0x4]
		xor cx, cx
		mov si, _blockbuf
		push si
		push cx
		push ax
		call _readblock
		add sp, byte +0x6
		pop bx  ; Restore BX := minixnode.
		and bx, byte 0x1f
		mov cl, 5
		shl bx, cl
		mov cx, [si+bx+4]
		mov [_filesize], cx
		mov cx, [si+bx+6]
		mov [_filesize+2], cx
		lea bx, [si+bx+0xe]
		mov [bp-0xc], bx
		xor si, si  ; ind.
		mov di, 7  ; len.
.3:
		push word [bp-0x8]
		push di  ; len.
		push si  ; indx.
		push word [bp-0xc]
		call _scanzone
		add sp, byte +0x8
		; Now: AX (minixnode) has been updated by the return value of _scanzone.
		test ax, ax
		jnz short .4
		times 2 add [bp-0xc], di
		mov di, 1
		inc si  ; ind.
		cmp si, byte 3  ; ind.
		jne short .3
.4:
		cmp ax, strict word 1  ; ROOT_INODE.
		ja short .6
		mov si, _noshoename
		mov byte [byte si-_noshoename+_endshoename], 0xd  ; The message would be comprehensible without the CRLF at the end.
		jmp strict near _fatal  ; Doesn't return.
.6:
		inc word [byte bp-0x8]  ; Set to nonzero, indicate _readshoe as the fake function pointer.
		dec ax  ; AX (minixnode) -= 1.
		dec byte [bp-0xa]  ; j.
		jnz short .1
		; Fall through to jump_to_shoelace.

jump_to_shoelace:
		push ds  ; Segment used by _main(...) in /shoelace for getting e.g. _diskcode (see above).
		push ax  ; Dummy value, popped by _main(...) in /shoelace.
		jmp BOOTSEG:0  ; Jump to _main(...) in /shoelace.

%if $-_start>0x400
  %error ERROR_BOOTLACE_TOO_LONG
  times -1 nop
%endif

absolute $  ; BSS.
		resb (_start-$)&1  ; Aligment.
_blockbuf:	resb 0x400  ; static buffer blockbuf  ; block buffer. 0x400 bytes.
_filesize:	resd 1  ; long filesize;  size of file. 4 bytes.

; No more global variables, we have them all. Total size: a_bss == 0x40a bytes.

; __END__
