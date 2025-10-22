# mkfs-bootable-minix1: tools to create bootable Minix 1 filesystems

mkfs-bootable-minix1 is a set of command-line tools for Linux to create
bootable Minix 1 filesystems. It contains the MBR boot code (as NASM
assembly source) and a filesystem creator tool which can take a Minix kernel
image file, an MBR image file file, and create a Minix 1 filesystem from
them, which can be booted as a hard disk (HDD) image in emulators such as
QEMU and VirtualBox.

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
