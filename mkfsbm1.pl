#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# mkfsbm1.pl: create a bootable Minix 1 filesystem
# by pts@fazekas.hu at Tue Oct 21 16:32:49 CEST 2025
# 

BEGIN { $ENV{LC_ALL} = "C" }  # For deterministic output. Typically not needed. Is it too late for Perl?
BEGIN { $ENV{TZ} = "GMT" }  # For deterministic output. Typically not needed. Perl respects it immediately.
BEGIN { $^W = 1 }  # Enable warnings.
use integer;
use strict;

sub fnargq($) { $_[0] =~ m@^[.\w/]@ ? $_[0] : "./" . $_[0] }
sub fnopenq($) { $_[0] =~ m@^[-+.\w/]@ ? $_[0] : "./" . $_[0] }

my %size_multipliers = (  # On a minix1 filesystem, 1 block == 1 KiB == 1K.
    "b" => 1, "k" => (1 << 10), "m" => (1 << 20), "g" => (1 << 30), "t" => (1 << 30 << 10), "p" => (1 << 30 << 20),
    "s" => 512, "h" => (512 * 16 * 63));  # g is for IBM PC HDD geometry: sector_size * heads * sectors_per_track.

sub parse_size($) {
  my $size = $_[0];
  my $orig_size = $size;
  my $multiplier = ($size =~ s@([bkmgtpsh])\Z(?!\n)@@i) ? $size_multipliers{lc($1)} : 1;
  my $size_number_spec = $size;
  $size = ($size =~ m@^0[0-7]*\Z(?!\n)@) ? oct($size) : ($size =~ m@^0[xX][0-9a-fA-F]*\Z(?!\n)@) ? hex($size) : ($size =~ m@^[1-9]\d*\Z(?!\n)@) ? int($size) : undef;
  die("fatal: bad size number in $orig_size: $size_number_spec\n") if !defined($size) or $size < 0;
  my $size_before_multiplication = $size;
  $size *= $multiplier;
  die("fatal: size too large: $orig_size\n") if !$multiplier or $size < 0 or $size / $multiplier != $size_before_multiplication;
  $size
}

sub parse_uint($) {
  my $uint = $_[0];
  my $v = ($uint =~ m@^0[0-7]*\Z(?!\n)@) ? oct($uint) : ($uint =~ m@^0[xX][0-9a-fA-F]*\Z(?!\n)@) ? hex($uint) : ($uint =~ m@^[1-9]\d*\Z(?!\n)@) ? int($uint) : undef;
  die("fatal: bad nonnegative integer: $uint\n") if !defined($v) or $v < 0;
  $v
}

sub parse_namelength($) {
  my $v = parse_uint($_[0]);
  die("fatal: bad namelength: $v\n") if $v != 14 and $v != 30;
  $v
}

sub parse_int32($) {
  my $int32 = $_[0];
  return -0x7fffffff - 1 if $int32 =~ m@^-(?:0+20000000000|0[xX]0*80000000|2147483648)\Z(?!\n)@;  # Avoid overflows and underflows.
  my $sign = substr($int32, 0, 1) eq "-" ? -1 : 1;
  # TODO(pts): Catch more out-of-range errors in 32-bit Perls. Currently it seems to be saturating.
  my $v = ($int32 =~ m@^-?(0[0-7])*\Z(?!\n)@) ? oct($1) * $sign : ($int32 =~ m@^-?(0[xX][0-9a-fA-F]*)\Z(?!\n)@) ? hex($1) * $sign : ($int32 =~ m@^-?([1-9]\d*)\Z(?!\n)@) ? int($1) * $sign : undef;
  die("fatal: bad integer: $int32\n") if !defined($v);
  die("fatal: 32-bit signed integer out of range: $int32\n") if (($v < 0) ? -$v : $v) & ~0x7fffffff;
  $v
}

sub get_min_blockc($$) {
  my $inodec = defined($_[0]) ? $_[0] : 1;  # 1 is the minimum number of inodes: unused inode 0 and the root inode.
  my $reservedblockc = ($_[1] + 0x3ff) >> 10;
  my $imapblockc = ($inodec + 1 + 0x1fff) >> 13;
  my $iblockc = ($inodec + 1 + 0x1f) >> 5;
  my $zmapblockc = 1;
  my $zblockc = 1;  # Directory entries for the root inode.
  2 + $imapblockc + $zmapblockc + $iblockc + $reservedblockc + $zblockc
}

sub format_blockc_size($) {
  my $blockc = $_[0];
  my $h = $size_multipliers{h} >> 10;
  ($blockc % $h == 0) ? sprintf("%uH", $blockc / $h) : (($blockc & 0xfffff) == 0) ? sprintf("%uG", $blockc >> 20) : (($blockc & 0x3ff) == 0) ? sprintf("%uM", $blockc >> 10) : sprintf("%uK", $blockc)
}

my $device_fn;

sub write_block($$) {
  my($blocki, $data) = @_;
  if (length($data)) {
    my $got;
    die("fatal: error seeking to block $blocki: $device_fn\n") if !defined($got = sysseek(F, $blocki << 10, 0)) or $got != ($blocki << 10);
    my $size = length($data);
    die("fatal: error writing $size bytes to block $blocki: $device_fn\n") if (syswrite(F, $data, length($data)) or 0) != length($data);
  }
}

sub read_file($;$) {
  my($fn, $max_size) = @_;
  die("fatal: error opening for reading: $fn: $!\n") if !open(FR, "< " . fnopenq($fn));
  binmode(FR);
  my($try_size, $got);
  my $s = "";
  while (($try_size = (!defined($max_size) or $max_size >= length($fn) + 0x10000) ? 0x10000 : $max_size - length($fn)) > 0) {
    die("fatal: error reading file: $fn\n") if !defined($got = sysread(FR, $s, $try_size, length($s)));
    last if !$got;  # Error on EOF.
  }
  close(FR);
  $s
}

sub read_file_limited($;$$) {
  my($fn, $min_size, $max_size) = @_;
  my $s = read_file($fn, defined($max_size) ? $max_size + 1 : undef);
  die("fatal: input file shorter than $min_size bytes: $fn\n") if defined($min_size) and length($s) < $min_size;
  die("fatal: input file longer than $max_size bytes: $fn\n") if defined($max_size) and length($s) > $max_size;
  $s
}

# --- main().

#my $device_fn;  # See above.
my $size;
my $uid = 2;  # UID if bin. Same default as in Minix 1.5 pc/disk.13.ex/p/commands/mkfs.c .
my $gid = 2;  # GID of bin. Same default as in Minix 1.5 pc/disk.13.ex/p/commands/mkfs.c .
my $mtime = 0xdd;  # Some small, reproducible default. 1970-01-01 00:03:41 GMT.
my $reserved_size = 0;
my $do_fix_qemu = 0;
my $do_force_size = 0;
my $do_truncate0 = 0;
my $do_add_vhd_footer = 0;
my $boot_fn;
my $super_fn;
my $kernel_fn;
my $inodec;
my $namelength = 14;  # Please note that Minix <=2.0.4 supports only $namelength == 14. Linux 1.0.4 supports $namelength == 30.
die("Usage: $0 [<flag>...] <device> [<byte-size>]\n") if !@ARGV or $ARGV[0] eq "--help";
{ my $i;
  for ($i = 0; $i < @ARGV; ++$i) {
    my $arg = $ARGV[$i];
    if ($arg eq "--") { ++$i; last }
    elsif ($arg eq "-" or $arg !~ m@^-@) { last }
    elsif ($arg =~ s@^--device=@@) { $device_fn = $arg }
    elsif ($arg =~ s@^--size=max@@) { $size = 0xffff << 10 }
    elsif ($arg =~ s@^--size=@@) { $size = parse_size($arg) }
    elsif ($arg =~ s@^--reserved=@@) { $reserved_size = parse_size($arg) }
    elsif ($arg =~ s@^--inodes=max@@) { $inodec = 0xfffe }
    elsif ($arg =~ s@^--inodes=@@) { $inodec = parse_uint($arg) }
    elsif ($arg =~ s@^--namelength=@@) { $namelength = parse_namelength($arg) }  # Flag compatible with mkfs.minix(1).
    elsif (($arg eq "-n" or $arg eq "--namelength") and $i < $#ARGV) { $namelength = parse_namelength($ARGV[++$i]) }  # Flag compatible with mkfs.minix(1).
    elsif ($arg =~ s@^--uid=@@) { $uid = parse_uint($arg) }  # User ID.
    elsif ($arg =~ s@^--gid=@@) { $gid = parse_uint($arg) }  # Group ID.
    elsif ($arg =~ s@^--mtime=@@) { $mtime = parse_int32($arg) }  # Group ID.
    elsif ($arg =~ s@^--boot=@@) { $boot_fn = $arg }  # Copy the contents of this file to the boot block.
    elsif ($arg =~ s@^--super=@@) { $super_fn = $arg }  # Copy the contents of this file to the superblock (but replace the first 0x12 bytes with the appropriate haders).
    elsif ($arg =~ s@^--kernel=@@) { $kernel_fn = $arg }  # Copy the contents of this Minix kernel file to the reserved area.
    elsif ($arg eq    "--vhd") { $do_add_vhd_footer = 1 }
    elsif ($arg eq "--no-vhd") { $do_add_vhd_footer = 0 }
    elsif ($arg eq    "--vpc") { $do_add_vhd_footer = 1 }
    elsif ($arg eq "--no-vpc") { $do_add_vhd_footer = 0 }
    elsif ($arg eq    "--fix-qemu") { $do_fix_qemu = 1 }
    elsif ($arg eq "--fix-qemu=up" or $arg eq "--fix-qemu-up") { $do_fix_qemu = 2 }
    elsif ($arg eq "--no-fix-qemu") { $do_fix_qemu = 0 }
    elsif ($arg eq    "--force-size") { $do_force_size = 1 }
    elsif ($arg eq "--no-force-size") { $do_force_size = 0 }
    elsif ($arg eq    "--truncate0") { $do_truncate0 = 1 }  # Truncate file to 0 bytes first to free previously allocated blocks.
    elsif ($arg eq "--no-truncate0") { $do_truncate0 = 0 }
    else { die("fatal: unknown command-line flag: $arg\n") }
  }
  if (!defined($device_fn)) {
    die("fatal: missing <device> argument\n") if $i >= @ARGV;
    $device_fn = $ARGV[$i++];
  }
  $size = ($ARGV[$i++] eq "max") ? 0xffff << 10 : parse_size($ARGV[$i - 1]) if $i < @ARGV;
  die("fatal: too many command-line arguments\n") if $i < @ARGV;
}
die("fatal: minix1 inode count too small, must be at least 1: $inodec\n") if defined($inodec) and $inodec < 1;  # Minimum is 1: the root inode.
die("fatal: minix1 inode count too large: $inodec\n") if defined($inodec) and $inodec >= 0xffff;
die("fatal: minix1 filesystem too small: $size bytes\n") if defined($size) and ($size >> 10) < get_min_blockc($inodec, $reserved_size);
#die $size;
die("fatal: minix1 user ID (UID) too large: $uid\n") if $uid > 0xffff;
die("fatal: minix1 group GID (GID) too large: $gid\n") if $gid > 0xff;

die("fatal: error opening device: $device_fn\n") if !open(F, "+< " . fnopenq($device_fn))
    and !open(F, "> " . fnopenq($device_fn));
binmode(F);
my $device_size;
die("fatal: error getting device size: $device_fn\n") if !defined($device_size = sysseek(F, 0, 2));

my $boot_data;
if (defined($boot_fn)) {
  $boot_data = read_file_limited($boot_fn, 0, 0x1000);
  my $a_text;
  my $md = "\xf8";  # Media descriptor byte for HDD. We don't support creating a bootable floppy image with bootlace+shoelace.
  if ($boot_data =~ s@^\x01\x03\x10\x04\x20\0\0\0 (..)\0\0 \0\0\0\0 ..\0\0 \0\0\0\0 ..[\0\x01]. ..\0\0 \xeb\x29 \x99 12345678 [\x40-\xff]\0.{30} (\xea\x30\0\xc0\x07)@\xeb\x29\x99ShoeLace\0\x02\x02\x02\0\0\0\0\0\0$md\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0$2@sx and
      ($a_text = unpack("v", $1)) >= 0x1fe and $a_text <= 0xffe) {
    # The bootlace a.out executable file of the ShoeLace bootloader.
    $boot_data = substr($boot_data, 0, $a_text);
    substr($boot_data, 0x1fe, 0) = "\x55\xaa";  # Insert the boot signature word.
    goto FIX_SHOELACE;
  } elsif ($boot_data =~ m@^\x01\x03[\0\x10\x20\x30][\x04\x10]@sx) {
    die("fatal: unrecognized a.out file for --boot=: $boot_fn\n");
  } elsif ($boot_data =~ s@^\xeb\x29 \x99 ShoeLace\0\x02.{30} (\xea\x30\0\xc0\x07)@\xeb\x29\x99ShoeLace\0\x02\x02\x02\0\0\0\0\0\0$md\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0$1@sx) {
    # A bootlace image of the ShoeLace bootloader, extracted from the boot block of an existing Minix filesystem (floppy boot sector or HDD MBR);
    die("fatal: boot signature not found in bootlace image: $boot_fn\n") if length($boot_data) < 0x200 or substr($boot_data, 0x1fe, 2) ne "\x55\xaa";
   FIX_SHOELACE:
    # bootlace tries to find the Minix filesystem on a partition, but we have it on the entire disk. So we binary patch the bootlace code below to use the entire disk.
    die("fatal: partition start code not found in bootlace image: $boot_fn\n") if substr($boot_data, 0x42, 20) ne "\x26\xff\x74\x02\x26\x8a\x74\x01\x52\x26\xc4\x44\x08\xa3\x1c\x00\x8c\x06\x1e\x00";  # push word [es:si+2] ++ mov dh, [es:si+1] ++ push dx ++ les ax, [es:si+8] ++ mov [0x1c], ax ++ mov [0x1e], es.
    substr($boot_data, 0x42, 20) = "\x31\xc0\xa3\x1c\x00\xa3\x1e\x00\x40\x50\xb6\x00\x52\x90\x90\x90\x90\x90\x90\x90";  # xor ax, ax ++ mov [0x1c], ax ++ mov [0x1e], ax ++ inc ax ++ push ax ++ mov dh, 0 ++ push dx ++ nop. Use first sector: CHS 0:0:1, LBA 0.
  }
  die("fatal: boot image too long: $boot_fn\n") if length($boot_data) > 0x400;
  $boot_data .= "\0" x (0x400 - length($boot_data));
}
my $super_data = "";
if (defined($super_fn)) {
  $super_data = read_file_limited($super_fn, 0, 0x400);
  $super_data .= "\0" x (0x400 - length($super_data));
}
my $reserved_data;
if (defined($kernel_fn)) {
  $reserved_data = read_file_limited($kernel_fn, 0x400);
  my $final_cs;
  if ($reserved_data =~ m@^\xb8\xc0\x07 \x8e\xd8 \x31\xf6 \xb8.. \x8e\xc0  \x31\xff \xb9\x00\x01 [\xf2\xf3]\xa5 \xea@sx) {  # Minix 1.5 8086 kernel image with floppy boot sector. Example files: pc/disk.03, demo_dsk.ibm .
    # db is the debugger. It is not used in practice, and values are 0 in production images.
    my($db_ds, $db_pc, $db_cs, $final, $menu_ds, $menu_pc, $menu_cs) = unpack("v7", substr($reserved_data, ((substr($reserved_data, 0x1fe, 2) eq "\x55\xaa") ? 0x1f0 : 0x1f2), 7 << 1));
    $final_cs = (($final - 1) << 5) + 0x60;
    my $menu_fofs = 0x200 + (($menu_cs - 0x60) << 4);
    my $db_fofs = 0x200 + (($db_cs - 0x60) << 4);
    printf(STDERR "info: Minix kernel boot final=0x%x final_cs=0x%x menu_ds=0x%x menu_pc=0x%x menu_cs=0x%x menu_fofs=0x%x db_ds=0x%x db_pc=0x%x db_cs=0x%x db_fofs=0x%x f=%s\n", $final, $final_cs, $menu_ds, $menu_pc, $menu_cs, $menu_fofs, $db_ds, $db_pc, $db_cs, $db_fofs, $kernel_fn);
    die("fatal: bad final: $kernel_fn\n") if $final < 2;
    die("fatal: bad menu_pc: $kernel_fn\n") if $menu_pc;
    die("fatal: bad menu_cs: $kernel_fn\n") if $menu_cs < 0x80;  # This is way too low.
    die("fatal: inconsistent menu_cs and menu_ds: $kernel_fn\n") if $menu_ds < $menu_cs;  # Minix 1.5 8086 has these equal, but in Minix 1.5 i386, $menu_ds is 0x100 larger.
    die("fatal: menu_cs is larger than kernel_cs: $kernel_fn\n") if $menu_cs > $final_cs;
    die("fatal: Minix kernel image too short: $kernel_fn\n") if length($reserved_data) < 0x200 + (($final_cs - 0x60) << 4);
    $reserved_data = substr($reserved_data, 0x200, $menu_fofs - 0x200);
    goto KERNEL_PART;
  } else { KERNEL_PART:
    if ($reserved_data =~ m@^\xeb\x04 .. (..) \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc@sx) {
      my $kernel_cs = 0x60; my $kernel_ds = unpack("v", $1);
      die(sprintf("fatal: bad kernel_ds=0x%x: %s\n", $kernel_ds, $kernel_fn)) if $kernel_ds <= 0x60 or $kernel_ds >= 0x60 + (length($reserved_data) >> 4);
      my($kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256) = unpack("v8", substr($reserved_data, ($kernel_ds - 0x60) << 4, 0x10));
      my $kernel_size_para = length($reserved_data) >> 4;
      my $kernel_sum_size_para = ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256 + $init_text_a256 + $init_data_a256) << 4;
      # Our $kernel_magic has been overwritten with $kernel_text_a256 and $kernel_data_a256.
      my $mm_magic   = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256) << 8, 4));
      my $fs_magic   = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256) << 8, 4));
      my $init_magic = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256 + $init_text_a256) << 8, 4));
      printf(STDERR "info: Minix kernel kernel_cs=0x%x kernel_ds=0x%x size=0x%x0 sum_size=0x%x0 kernel_para=0x%x0+0x%x0 mm_para=0x%x0+0x%x0 fs_para=0x%x0+0x%x0 init_para=0x%x0+0x%x0 mm_magic=0x%x fs_magic=0x%x init_magic=0x%x f=%s\n",
             $kernel_cs, $kernel_ds, $kernel_size_para, $kernel_sum_size_para, $kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256, $mm_magic, $fs_magic, $init_magic, $kernel_fn);
      die("fatal: inconsistent Minix kernel_text_a and kernel_ds: $kernel_fn\n") if $kernel_cs + ($kernel_text_a256 << 4) != $kernel_ds;
      die("fatal: inconsistent Minix kernel size and sum_size: $kernel_fn\n") if $kernel_size_para != $kernel_sum_size_para;
      die("fatal: kernel image is not long enough: $kernel_fn\n") if length($reserved_data) < ($kernel_sum_size_para << 4);
      #die("fatal: bad kernel_magic: $kernel_fn\n") if $kernel_magic != 0x8526f;  # Checked by /shoelace. Our $kernel_magic has been overwritten with $kernel_text_a256 and $kernel_data_a256.
      die("fatal: bad mm_magic: $kernel_fn\n") if $mm_magic != 0x8dada;  # Also checked by /shoelace.
      die("fatal: bad fs_magic: $kernel_fn\n") if $fs_magic != 0x8dada;  # Also checked by /shoelace.
      #die("fatal: bad init_magic: $kernel_fn\n") if $init_magic != 0x8dada;  # Not checked by /shoelace. init_magic is not set for the original 8086 kernel pc/disk.03.
      die("fatal: bad mm start: $kernel_fn\n") if substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256) << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
      my $is_mm_i386 = defined($1);  # Not checking kernel text, because it always starts with 8086 (16-bit) code.
      die("fatal: bad fs start: $kernel_fn\n") if substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256) << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
      my $is_fs_i386 = defined($1);
      die("fatal: bad init start: $kernel_fn\n") if substr($reserved_data, ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256) << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
      my $is_init_i386 = defined($1);
      my $pmswitch = "unknown";  # Switch mechanism used to switch to protected mode.
      if ($reserved_data =~ m@^.{43} \xbe.. \xbb\x30\x28 \xb4\x89 \xcd\x15 \xc3@sx) {  # mov si, ... ++ mov bx, 0x2830 ++ mov ah, 0x89 ++ int 0x15 ++ ret.
        $pmswitch = "int15h"  # This can be 8086 or i386.
      } elsif ($reserved_data =~ m@^.{43} \x0f\x01\x16.. \x0f\x20\xc0@sx or  # lgdt [...] ++ mov eax, cr0 ++ or al, 1 ++ mov cr0, eax.
               $reserved_data =~ m@^.{43} \xbe.. \xbb\x30\x28 \xb4. \xe8.\0 (?:[\xeb\xb0\xe6]. | \x88[\xf8\xd8])+ \x0f\x01\x16.. \x0f\x20\xc0@sx) {  # mov si, ... ++ mov bx, 0x2830 ++ mov ah, ... (0) ++ call ... ++ (lots mov()s and out()s) ++ lgdt [...] ++ mov eax, cr0 ++ or al, 1 ++ mov cr0, eax.
        $pmswitch = "lgdt-cr0-i386"
      }
      printf(STDERR "info: Minix kernel CPU mm=%s fs=%s init=%s pmswitch=%s f=%s\n", ($is_mm_i386 ? "i386" : "8086"), ($is_fs_i386 ? "i386" : "8086"), ($is_init_i386 ? "i386" : "8086"), $pmswitch, $kernel_fn);
      die("fatal: inconsistent CPU among mm, fs, init: $kernel_fn\n") if $is_mm_i386 != $is_fs_i386 or $is_mm_i386 != $is_init_i386;
      die("fatal: inconsistent CPU and pmswitch: $kernel_fn\n") if !$is_mm_i386 and $pmswitch =~ m@i386@;
      substr($reserved_data, $kernel_size_para << 4) = "";
    } elsif ($reserved_data =~ m@^\x9c \x0e \x50 \x51 \x52 \x53 \x55 \x56 \x57 \x0e \x8c\xc8 \xbf@sx) {  # Compressed kernel.
      # Just use the entire file as a compressed kernel image.
    } elsif (defined($final_cs)) {
      die("fatal: unrecognized Minix kernel image before menu: $kernel_fn\n");
    } else {
      die("fatal: unrecognized Minix kernel image: $kernel_fn\n");
    }
  }
  if ($reserved_size) {
    my $size = length($reserved_data);
    die("fatal: --reserved=$reserved_size value conflicts with --kernel=$kernel_fn of size $size bytes\n") if $size > $reserved_size;
  } else {
    $reserved_size = length($reserved_data);
  }
}

# --- From this point we change $device_fn in F.

$device_size = $device_size + 0;  # Convert "0 but true" to 0.
die("fatal: please specify the <size> argument (in bytes)\n") if !defined($size) and !$device_size;
$size = $device_size if !defined($size);
if ($do_fix_qemu == 1) {
  $size -= $size % $size_multipliers{h};
  # The `$size - $size_multipliers{h}' below fixes the QEMU 2.11.1 disk
  # image size detection quirk: QEMU hides the last HDD track (cylinder)
  # from the guest, even excluding this track from the geometry size it
  # returns.
}
my $blockc = (($do_fix_qemu == 1) ? $size - $size_multipliers{h} : $size) >> 10;  # This can make $blockc negative if $do_fix_quemu is true.
if ($do_fix_qemu == 2) { $size = ($blockc << 10) + $size_multipliers{h} - 1; $size -= $size % $size_multipliers{h} }
die("fatal: minix1 filesystem too small: $blockc blocks\n") if $blockc < get_min_blockc($inodec, $reserved_size);
die("fatal: minix1 filesystem too large: $blockc blocks\n") if $blockc > 0xffff;
my $reservedblockc = ($reserved_size + 0x3ff) >> 10;
die("fatal: assert: reserved data too long\n") if defined($reserved_data) and length($reserved_data) > ($reservedblockc << 10);
# TODO(pts): Add flag to round up $inodec, like mkfs.minix(1) on Linux does it.
$inodec = ($blockc < $reservedblockc) ? 0x1f : (($blockc - $reservedblockc) / 3) + 8 if !defined($inodec);  # Minix 1.5 pc/disk.13.ex/p/commands/mkfs.c default is 3 * blocks/file.
die("fatal: bad minix1 inode count: $inodec\n") if $inodec < 1 or $inodec >= 0xffff;
# Now we have the final $blockc and $inodec.
my $imapblockc = ($inodec + 1 + 0x1fff) >> 13;  # 1..8.
my $iblockc = ($inodec + 1 + 0x1f) >> 5;
my $zmapblockc = 1;  # Lower estimate for $zmapblock, because $zmapblock >= 1.
my $firstdatablock = 2 + $imapblockc + $zmapblockc + $iblockc + $reservedblockc;  # In this lower estimate for $firstdatablock, we use 1 as lower estimate for $zmapblockc.
my $zmapblockc2;
while (($zmapblockc2 = ($blockc - $firstdatablock + 1 + 0x1fff) >> 13) > $zmapblockc) {  # Stay in the loop if $zmapblock isn't large enough.
  $firstdatablock = 2 + $imapblockc + ($zmapblockc = $zmapblockc2) + $iblockc + $reservedblockc;  # Increase both $zmapblockc and $firstdatablock.
}
die("fatal: assert: zmapblockc too large: $zmapblockc\n") if $zmapblockc > 8;
my $firstreservedblock = 2 + $imapblockc + $zmapblockc + $iblockc;
printf(STDERR "info: blockc=0x%x=%u fssize=%s inodec=0x%x=%u firstreservedblock=0x%x=%u firstdatablock=0x%x=%u f=%s\n",
       $blockc, $blockc, format_blockc_size($blockc), $inodec, $inodec, $firstreservedblock, $firstreservedblock, $firstdatablock, $firstdatablock, $device_fn);

# Patch .kernel_start_sector and .kernel_sector_count in the boot sector.
if (defined($boot_data) and defined($kernel_fn)) {
  my($kernel_start_sector, $kernel_sector_count, $boot_signature) = unpack("v3", substr($boot_data, 0x1fa, 6));
  if ($boot_signature == 0xaa55 and $kernel_sector_count == 0xffff and $kernel_start_sector == 0xfffe) {
    $kernel_start_sector = $firstreservedblock << 1; $kernel_sector_count = (length($reserved_data) + 0x1ff) >> 9;
    die("fatal: kernel_start_sector too large\n") if $kernel_start_sector > 0xffff;
    die("fatal: kernel way too long\n") if $kernel_sector_count > 0xffff;
    die("fatal: kernel too long\n") if $kernel_sector_count > ((0x90000 - 0x600) >> 9);  # Limit the kernel arbitrarily to 574.5 KiB. Most boot sectors can't handle more anyway.
    substr($boot_data, 0x1fa, 4) = pack("vv", $kernel_start_sector, $kernel_sector_count);
  }
}

if ($do_truncate0 and $device_size) {
  die("fatal: error truncating device: $device_fn\n") if !truncate(F, 0);  # This will fail on block devices, but it will work on disk image files.
  $device_size = 0;
}
if ($device_size < $size or ($do_force_size and $device_size != $size)) {
  die("fatal: error changing device size: $device_fn\n") if !truncate(F, $size);
}
write_block(0, $boot_data) if defined($boot_data);  # Write boot block.
my $magic = ($namelength == 30) ? 0x138f : 0x137f;
substr($super_data, 0, 0x12) = pack("v6Vv", $inodec, $blockc, $imapblockc, $zmapblockc, $firstdatablock, 0, 0x10081c00, $magic);
write_block(1, $super_data);  # Write superblock.
{ my $inodei = $inodec + 1;
  my $inodec18 = $inodei + (-$inodei & 7);
  my $imap_data = "\0" x ($inodec18 << 3);
  substr($imap_data, 0, 1) = "\3";  # Indicate that inode 0 and the root inode as used.
  for (; $inodei < $inodec18; ++$inodei) { vec($imap_data, $inodei, 1) = 1 }
  substr($imap_data, $inodec18 >> 3) = "\xff" x ((($imapblockc << 13) - $inodec18) >> 3);
  write_block(2, $imap_data);  # TODO(pts): Write fewer NULs to sparse file.
}
{ my $blocki = $blockc - $firstdatablock + 1;
  my $blockc18 = $blocki + (-$blocki & 7);
  my $zmap_data = "\0" x ($blockc18 << 3);
  substr($zmap_data, 0, 1) = "\3";  # Indicate that block 0 and the root directory block as used.
  for (; $blocki < $blockc18; ++$blocki) { vec($zmap_data, $blocki, 1) = 1 }
  substr($zmap_data, $blockc18 >> 3) = "\xff" x ((($zmapblockc << 13) - $blockc18) >> 3);
  write_block(2 + $imapblockc, $zmap_data);  # TODO(pts): Write fewer NULs to sparse file.
}
my $dir_data = pack(scalar(("va" . $namelength) x 2), 1, ".", 1, "..");
write_block(2 + $imapblockc + $zmapblockc, pack("vvVVCCv", 040777, $uid, length($dir_data), $mtime, $gid, 2, $firstdatablock));  # Write rootdir (/) inode.
write_block($firstreservedblock, $reserved_data) if defined($reserved_data);  # Write data to the reserved area.
write_block($firstdatablock, $dir_data);  # Write rootdir (/) entries: "." and "..".
# TODO(pts): Add an option to write extra NUL bytes: boot block, end of superblock, end of rootdir inode block, end of rootdir entries block.

if ($do_add_vhd_footer) {  # Add a Virtual PC .vhd footer to make it easy to use the image file in VirtualBox.
  # In QEMU, it will keep working as either `-drive file=hd.img,format=bin'
  # or `-drive file=hd.img,format=vpc'. However, for `-fda hd.img', QEMU
  # will still emit a wearning, because it doesn't check the last sector for
  # the VHD footer during autodetection.
  #
  # VirtualBox respects the disk geometry in the .vhd file up to the max
  # of C*H*S == 1024*16*63 =~ 504 MiB; above 1024 cyls it starts doing
  # transformations.
  my $vhd_size = $blockc << 10;
  $vhd_size += $size_multipliers{h} if $do_fix_qemu;
  $vhd_size += (16 * 63 * 0x200) - 1;
  $vhd_size -= $vhd_size % (16 * 63 * 0x200);
  my $heads = 16;
  my $sectors_per_track = 63;
  my $cylinders = $vhd_size / (16 * 63 * 0x200);
  # It's possible to have VHD disk images larger than ~504 MiB, but there
  # are many compatibility issues, so we don't support creating those here.
  die("fatal: device has too many cylinders for compatible VHD: $cylinders\n") if $cylinders > 1024;
  $vhd_size += 0xfffff;
  $vhd_size &= ~0xfffff;  # Round up to the nearest MiB, as required by Microsoft Azure.
  die("fatal: error changing device size for VHD: $device_fn\n") if !truncate(F, $vhd_size);
  # VirtualBox doesn't allow adding a disk with the same UUID, so we try to
  # make these as different as possible, without breaking determinism. QEMU
  # doesn't have such a limitation.
  my $uuid = pack("a8nnN", "Minix1__", $blockc, $inodec, $reserved_size);
  my $vhd_footer = pack("a8N5a4Na4N4nCCNNa16",
      "conectix", 2, 0x10000, -1, -1, 0, "vpc ", 0x50003, "Wi2k", 0, $vhd_size, 0, $vhd_size, $cylinders, $heads, $sectors_per_track, 2, 0, $uuid);
  my $checksum = -1;
  for (my $i = 0; $i < length($vhd_footer); ++$i) { $checksum -= vec($vhd_footer, $i, 8) }
  substr($vhd_footer, 0x40, 4) = pack("N", $checksum);
  $vhd_footer .= "\0" x (0x200 - length($vhd_footer));
  write_block($vhd_size >> 10, $vhd_footer);
}
close(F);

__END__
