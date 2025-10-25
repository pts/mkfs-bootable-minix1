# mkfs-bootable-minix1: tools to create bootable Minix 1 filesystems

mkfs-bootable-minix1 is a set of command-line tools for Linux to create
bootable Minix 1 filesystems. It contains the MBR boot code (as NASM
assembly source) and a filesystem creator tool which can take a Minix kernel
image file, an MBR image file file, and create a Minix 1 filesystem from
them, which can be booted as a hard disk (HDD) image in emulators such as
QEMU and VirtualBox.

## The simple way to create a bootable Minix 1 filesystem

An end-to-end demo script
[demo_minix_1.5_8086_qemu.sh](demo_minix_1.5_8086_qemu.sh) is also included.
It compiles the MBR boot code
[minix_1.5_8086_hdd_boot.nasm](minix_1.5_8086_hdd_boot.nasm) with NASM, it
downloads the official [Minix 1.5
8086](http://download.minix3.org/previous-versions/Intel-1.5/pc/) (released
on 1990-06-01) kernel image
[disk.03](http://download.minix3.org/previous-versions/Intel-1.5/pc/disk.03)
and root filesystem images
[disk.04](http://download.minix3.org/previous-versions/Intel-1.5/pc/disk.04)
+
[disk.05](http://download.minix3.org/previous-versions/Intel-1.5/pc/disk.05),
it runs the Perl script [mkfsbm1.pl](mkfsbm1.pl) to create a bootable
filesystem from the MBR image and the kernel image, then it mounts the
filesystem (using the *minix* filesystem support built to the Linux kernel),
it copies over the files from the official filesystem images, and finally it
boots Minix 1.5 8086 in QEMU (*qemu-system-i386*).

The boot process works like this:

1. As usual, the PC BIOS loads the Master Boot Record (MBR) from LBA sector
   0, and starts running it in real mode at 0:0x7c00.
2. The MBR contains the boot code from
   [minix_1.5_8086_hdd_boot.nasm](minix_1.5_8086_hdd_boot.nasm), which loads
   the Minix kernel to memory (0x60:0), sets up to boot parameters (such as
   the device number containing the root filesystem), and jumps to the
   kernel code.

Some technical aspects:

* There are no partitions. The disk image contains the entire Minix
  filesystem. The MBR sector (first 0x200 bytes of the disk image) is the
  first sector of the boot block (first 1 KiB) of the Minix filesystem.
* The kernel image is not a file on the filesystem, but it is located in the
  reserved block area as a contiguous sequence of blocks. This way it is
  easier for the MBR boot code to load it: it just has to read subsequent
  sectors.
* Typical Minix filesystem creation tooks (such as mkfs(1) on Minix 1.5 or
  mkfs.minix(1) on Linux) are not able create a Minix filesystem with a
  non-empty reserved block area, so the custom tool [mkfsbm1.pl](mkfsbm1.pl)
  had to be written for this feature.
* The layout of a Minix 1 filesystem is: boot block, superblock, inode
  bitmap blocks (1..8), data zone bitmap blocks (1..8), inode blocks
  (1..2048), reserved blocks (0 or more), data zone blocks (1 or more).
  Each block is 1 KiB, i.e. 2 512-byte sectors. The superblock contains
  the block counts of each item in the layout.
* When Minix or Linux mounts a Minix 1 filesystem, it looks at the block
  counts in the superblock, and it properly ignores the reserved blocks.
* When the fsck.minix(1) tool on Linux checks a Minix filesystem containing
  reserved blocks, it displays: *Warning: Firstzone != Norm_firstzone*.
* When the fsck(1) tool on Minix checks a Minix filesystem containing
  reserved blocks, it displays:
  *warning: expected first data zone to be ... instead of ...*.

To make it easier to use the disk image with VirtualBox, run *mkfsbm1.pl
--vhd* to create a Virtual PC .vhd footer at the end of the disk image,
after the Minix filesystem. QEMU doesn't care, it accepts the default
raw disk images. Having the Virtual PC .vhd footer doesn't break QEMU or the
Linux kernel mounting the filesystem.

## Creating a bootable Minix 1 filesystem using Shoelace

Shoelace is the bootloader which understand minix1 filesystem, load the
kernel from any file on the filesystem (even if the file is fragmented), and
boot it. Shoelace was released in 1989. The version tested here is from
1990-04-24. Here is how booting from HDD typically works with Shoelace:

1. As usual, the PC BIOS loads the Master Boot Record (MBR) from LBA sector
   0, and starts running it in real mode at 0:0x7c00.
2. The MBR contains the boot code which looks at the parititon table,
   decides which partition to boot (maybe taking user input, maybe just
   booting the first active partition), loads the boot sector (very first
   sector) of that tpartition, and starts running it in real mode at
   0:0x7c00.
3. The boot sector contains the first sector of bootlace, one of the
   components of the Shoelace boot manager. It loads the second sector
   (right after the boot sector) of bootlace, and the resulting code loads
   the file named */shoelace* (typical size: 47 KiB, including symbols) on
   the minix1 filesystem on that partition, and starts running it in real
   mode at 0x1000:0. The bootlace code is smart enough to understand and
   read a minix1 filesystem (superblock, root inode, root directory, file
   inode, file data). It can load a file even if it's fragmented.
4. The /shoelace component of Shoelace loads its config file */etc/config* on
   the Minix1 filesystem, and loads and boots the kernel specified in the
   config file. Typically it is used (and it's more convenient to use it) to
   load Minix kernel component files (kernel, mm, fs and init) rather than
   the full kernel image file. The kernel component files are typically in
   the directory */etc/system*.

Shoelace can also boot from floppy.

Shoelace is not able to boot from a minix1 filesystem spanning over an
entire HDD (e.g. */dev/hd0*), it works only with filesystems on a partition.
However, by replacing the bootlace component with something else, the
replacement can convince the /shoelace component to boot that way. In this
case, the boot process will work like this:

1. The PC bios loads the MBR, which contains the first sector of the
   replacement bootlace, and the fake partition table. The fake partition
   table contains partition 4 (/dev/hd4 on Minix), which spans over the HDD
   (i.e. starts at LBA sector 0, CHS sector 0:0:1).
2. The first sector of the replacement bootlace loads the second sector.
3. The replacement bootlace code loads the the file named
   */sheolace*, which is an unmodified /shoelace component of Shoelace.
4. /shoelace thinks that the minix1 filesystem is on parititon 4, finds its
   */etc/config* file there, and boots the Minix kernel with /dev/hd4 as the
   root filesystem.
5. Minix happily mounts the minix1 filesystem as /dev/hd4, and boots from it.

This replacement bootlace is provided in NASM source file
[mbr_bootlace.nasm](mbr_bootlace.nasm). After compiling it (e.g. on Linux),
you can run `./mkfsbm1.pl --boot=mbr_bootlace.bin hd.img` to create a minix1
filesystem with it preinstalled to the MBR.

If you have a Minix kernel image, then extract the 4 component files kernel,
mm, fs, and init using
[split_minix_kernel.pl](https://github.com/pts/mkfs-bootable-minix1/blob/master/split_minix_kernel.pl).

After that, you still have to
copy the remaining files to the filesystem: */shoelace*,
*/etc/system/kernel*, */etc/system/mm*, */etc/system/fs*,
*/etc/system/init*. The contents of */etc/config* should be:

```
kernel /etc/system/kernel
mm /etc/system/mm
fs /etc/system/fs
init /etc/system/init
```

After this setup, it will boot in QEMU. (See a working QEMU command line in
the demo script [demo_minix_1.5_8086_qemu.sh](demo_minix_1.5_8086_qemu.sh).
Tested on QEMU 2.11.1.)

## More about Minix 1.5 kernels

If you want to run Minix 1.5 8086 in 16-bit protected mode instead, using
more than 640 KiB of memory, then download the kernel using
[download_minix_1.5_8086_qemu_pm.bin.sh](download_minix_1.5_8086_qemu_pm.bin.sh)
(command: `./minix_1.5_8086_qemu_pm.bin.sh --rm`), and run the demo script
as `./demo_minix_1.5_8086_qemu.sh minix_1.5_8086_qemu_pm.bin` instead. This
kernel was modified and compiled by freebird or gohigh on 2003-01-04 (see
[oldlinux.org](https://oldlinux.org/)).

A Minix 1.5 kernel image consists of the following components (in this
order): bootblok, kernel, mm, fs, init, menu, db. After booting, only the
kernel, mm, fs and init components remain in memory. The Shoelace boot
manager, in addition to booting a kernel image file, is able to boot a Minix
kernel using the 4 in-memory component files (kernel, mm, fs and init). To
facilitate this, mkfs-bootable-minix1 provides the Perl script
[split_minix_kernel.pl](split_minix_kernel.pl), which splits a Minix 1.5
kernel image to its 4 in-memory component files. It's also possible to save
disk space by splitting, because the NUL bytes of BSS sections are included
in the kernel image files, but not inthe component files.
