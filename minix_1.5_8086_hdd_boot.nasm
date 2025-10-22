;
; minix_1.5_8085_hd_boot.nasm: boot sector code for Minux 1.5 8086 booting from HDD
; by pts@fazekas.hu at Tue Oct 21 20:31:00 CEST 2025
;
; Compile with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -o minix_1.5_8086_hdd_boot.bin minix_1.5_8086_hdd_boot.nasm
;
; Please note that this boot code can't boot Minix from a floppy, because
; the sectors/track probing isn't implemented.
;
; TODO(pts): In about 5%, Minix reports some I/O errors during boot. Does this also happen at `-kernel' boot? Does this happen at 1-sector LBA boot? Does this happen at multisector LBA boot? Or is it a Linux mount(8) error?
; TODO(pts): Add code to boot from floppy by guessing the sectors/track.
;

bits 16
cpu 8086

BOOT_COPY_SEG equ 0x9000
BOOT_SIGNATURE equ 0xaa55

SCANCODE equ 13  ; Scancode of '=' (0xd on the PC keyboard). Also put to BX.

; Minix 1.5 device numbers.
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

mbr:  ; The BIOS has loaded this to 0:0x7c00, and has jumped to it in some combination.
		xor cx, cx
		cld
		cli
		mov ss, cx
		mov sp, 0x600
		sti
		xor di, di
		mov si, 0x7c00  ; The BIOS has loaded `mbr' here.
		mov ds, cx
		mov es, [byte si+.boot_copy_seg]  ; ES := BOOT_COPY_SEG.
		mov cx, 0x100
		rep movsw
		jmp BOOT_COPY_SEG:.cont-mbr
.boot_copy_seg: equ $-2
.cont:
		mov di, [byte si-0x200+.kernel_sector_count-mbr]
		times 2 push cx  ; High dword of LBA64 (0) in the Disk Address Packet (DAP) for int 13h AH==42h.
		push cx  ; High word of low dword of LBA64 (0).
		push word [byte si-0x200+.kernel_start_sector-mbr]  ; Low word of high dword of LBA64 (kernel_start_sector).
		mov ax, 0x60
		push ax  ; Segment of read destination.
		push cx  ; Offset  of read destination (0).
		push cx  ; Placeholder for the number of sectors to read.
		mov al, 0x10  ; AX := 0x10. AH is already 0.
		push ax  ; Size of DAP.
		mov bp, sp  ; SS:BP := address of DAP.
		push cx  ; Placeholder for sectors_per_track.
		push cx  ; Placeholder for head_count.
		dec cx
		push cx  ; Minix boot parameter .bp_processor == 0xffff. 88 would force 386 to 88. Default processor type for no restriction is 0xffff.
		inc cx  ; CX := 0.
		mov bx, SCANCODE
		push bx  ; Minix boot parameter .bp_scancode. Also put to BX before jumping to the Minix kernel.
		push cx  ; Minix boot parameter .bp_ramsize. We're not booting from a ramdisk, so it's arbitrary.
		push cx  ; Minix boot parameter .bp_ramimagedev. We're not booting from a ramdisk, so it's arbitrary.
		mov ax, DEV_HD5
		cmp dl, 0x81  ; DL has been set by the BIOS with the boot drive number before jumping to `mbr'.
		je short .hdd_done
		mov dl, 0x80  ; (hd0). Boot drive number for reading via int13h AH==42.
		mov al, DEV_HD0&0xff
.hdd_done:
		push ax  ; Minix boot parameter .bp_rootdev.
		mov ds, cx  ; DS := 0.

		mov ah, 0x41  ; Check extensions (EBIOS). DL already contains the drive number.
		mov bx, 0x55aa
		int 0x13  ; BIOS disk syscall.
		jc short .done_ebios  ; No EBIOS.
		cmp bx, 0xaa55
		jne short .done_ebios  ; No EBIOS.
		ror cl, 1
		jnc short .done_ebios  ; No EBIOS.
		add word [cs:.call_read_lba-mbr+1], byte .read_lba-.first_lba_read_error  ; Self-modifying code: replace `call .first_lba_read_error' at .call_read_lba with `call .read_lba'.
.done_ebios:
.read_more:
		mov ax, 0x7f  ; Some BIOSes are limited to reading 0x7f sectors at a time.
		cmp di, ax
		ja .read_sector_count_ok ; Jump iff more than 0x7f sectors are needed, and then read 0x7f sectors. We'll read the rest later.
		mov ax, di
.read_sector_count_ok:
.call_read_lba:
		call .first_lba_read_error  ; Self-modifying code: .first_lba_read_error will modify this to `call .read_chs'. Also modified above.
		jc short .fatal_disk_error
		sub di, ax
		jnz short .read_continue
.jump_to_minix_kernel:
		mov bx, SCANCODE
		xor di, di  ; Segment of the Minix boot parameters.
		mov si, sp  ; Offset  of the Minix boot parameters.
		mov cl, 0xa  ; CX := byte size of the Minix boot parameters. (CH is still 0.)
		xor ax, ax  ; Indicate to the Minix kernel that DI:SI points to the Minix boot parameters.
		jmp 0x60:0  ; Minix kernel entry point.
.read_continue:
		add [bp+8], ax  ; Add number of sectors read (in AX) to the LBA64 in the DAP.
		adc byte [bp+0xa], 0
		mov cl, 5
		shl ax, cl  ; AX := number of paragraphs read.
		add [bp+6], ax  ; Add number of paragraphs read (in AX) to the segment of read destination in the DAP.
%if 0  ; Show progress by printing a dot.a
		mov ax, 0xe00|'.'
		xor bx, bx
		int 0x10
%endif
		jmp short .read_more

.fatal_disk_error:
		mov si, .msg_disk_error-mbr
		; Fall through to .fatal.	

; Prints NUL-terminated message starting at SI, and halts. Ruins many registers.
.fatal:
		mov ah, 0xe
		xor bx, bx  ; mov bx, 7  ; BL == 7 (foreground color) is used in graphics modes only.
.next_msg_byte:	cs lodsb
		test al, al  ; Found terminating NUL?
		jz .halt
		int 0x10
		jmp short .next_msg_byte
.halt:		cli
.hang:		hlt
		jmp short .hang
		; Not reached.

; Input: AX: number of sectors to read. Will be ruined becaue of the output.
; Input: DH: arbitrary. Must be preserved.
; Input: DS, SS: 0. Must be preserved.
; Input: DL: BIOS drive number to read from. Must be preserved.
; Input: BX, CX, SI, ES: arbitrary. Can be ruined.
; Input: DI: arbitrary. Must be preserved.
; Input: SS:BP: points to the Data Address Packet (DAP) for int 13h AH==42h, except that word [bp+2] (number of sectors to read) is incorrect there. Must be preserved.
; Output: AX: number of sectors actually read.
; Output: CF: 0 on sucess, 1 on error.
.read_lba:
		mov word [bp+2], ax  ; Actual number of sectors to read.
		mov si, bp  ; DS:SI := address of DAP.
		push ax  ; Save number of sectors to read.
		mov ah, 0x42  ; INT 13h AH==42h: Extended Read Sectors From Drive.
		int 0x13  ; BIOS disk syscall. Will fail if tried on a floppy.
		pop ax  ; Restore AX := number of sectors to read, same as the number of sectors actually read.
		;jmp short .first_lba_read_error
		jnc short .first_lba_read_error.ret

.first_lba_read_error:  ; Fall back to .read_chs.
		mov word [cs:.call_read_lba-mbr+1], .read_chs-.call_read_lba-3  ; Self-modifying code: replace `times 2 nop' at .read_lba with `jmp short .read_chs'.
		; Now get the drive parameters to .sectors_per_track and .head_cont.
		push di  ; Save.
		xor di, di  ; Workaround for buggy BIOS. Also the 0 value will be used later.
		mov es, di  ; Workaround for buggy BIOS.
		mov ah, 8  ; Read drive parameters.
		push dx
		int 0x13  ; BIOS disk syscall. This call changes ES and DI only if DL is a floppy drive.
		jc short .fatal_disk_error
		and cx, byte 0x3f
		mov [bp-2], cx  ; .sectors_per_track.
%if 0
		mov al, cl
		out 0xe9, al
		cli
		hlt
%endif
		mov dl, dh
		mov dh, 0
		inc dx
		mov [bp-4], dx  ; .head_count.
		mov ah, 1  ; Get status of last drive operation. Needed after the AH == 8 call.
		pop dx  ; Restore drive number.
		int 0x13  ; BIOS disk syscall.
		pop di  ; Restore.
		; Fall through to .read_chs.

.read_chs:
%if 1
		mov bl, dl  ; BL := drive number.
		; Converts sector offset (LBA) value in dword [bp+8] to
		; BIOS-style CHS value in CX and DH. Ruins DL, AX and FLAGS.
		; This is heavily optimized for code size.
		mov ax, [bp+0xa]  ; High word of LBA.
		xor dx, dx
		div word [bp-2]  ; We assume that .sectors_per_track is between 1 and 63.
		xchg cx, ax  ; CX := AX (quotient); AX := junk.
		mov ax, [bp+8]  ; Low word of LBA.
		div word [bp-2]  ; .sectors_per_track.
		inc dx  ; Like `inc dl`, but 1 byte shorter. Sector numbers start with 1.
		xchg cx, dx  ; CX := sec value.
		div word [bp-4]  ; .head_count. We assume that .head_count is between 1 and 255.
		; Now AX is the cyl value (BIOS allows between 0 and 1023),
		; DX is the head value (between 0 and 254), thus the DL is
		; also the head value, CX is the sec value (BIOS allows
		; between 1 and 63), thus CL is also the sec value. Also the
		; high 6 bits of AH (and AX) are 0, because BIOS allows cyl
		; value less than 1024. (Thus `ror ah, 1` below works.)
		;
		; BIOS int 13h AH == 2 wants the head value in DH, the low 8
		; bits of the cyl value in CH, and it wants CL ==
		; (cyl>>8<<6)|head. Thus we copy DL to DH (cyl value), AL to
		; CH (low 8 bits of the cyl value), AH to CL (sec value),
		; and or the 2 bits of AH (high 8 bits of the cyl value)
		; shifted to CL.
		mov dh, dl
		mov ch, al
		ror ah, 1
		ror ah, 1
		or cl, ah
		mov ax, 0x201  ; AL == 1 means: read 1 sector.
		mov dl, bl  ; Restore DL := drive number.
		les bx, [bp+4]  ; ES:BX := address of read destination buffer.
%else  ; Fake.
		mov word [bp+2], 1  ; Actual number of sectors to read.
		mov si, bp  ; DS:SI := address of DAP.
		mov ah, 0x42  ; INT 13h AH==42h: Extended Read Sectors From Drive.
%endif
		int 0x13  ; BIOS disk syscall. Will fail if tried on a floppy.
		mov ax, 1  ; Indicate 1 sector has been read.
.first_lba_read_error.ret:
		ret

.msg_disk_error: db 'Disk error', 0

		times 0x1fa-($-$$) db 0
.kernel_start_sector: dw -2  ; Filled by mkfsbm1.pl.
.kernel_sector_count: dw -1  ; Filled by mkfsbm1.pl.
.boot_signature: dw BOOT_SIGNATURE
%if $-mbr!=0x200
  %error ERROR_ASSERT_BAD_MBR_SIZE
  times -1 nop
%endif

; __END__
