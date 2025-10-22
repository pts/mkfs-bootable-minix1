#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# mkfsbm1: create bootable Minix 1 filesystems
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
my $boot_fn;
my $super_fn;
my $kernel_fn;
my $inodec;
die("Usage: $0 [<flag>...] <device> [<size>]\n") if !@ARGV or $ARGV[0] eq "--help";
{ my $i;
  for ($i = 0; $i < @ARGV; ++$i) {
    my $arg = $ARGV[$i];
    if ($arg eq "--") { ++$i; last }
    elsif ($arg eq "-" or $arg !~ m@^-@) { last }
    elsif ($arg =~ s@^--device=@@) { $device_fn = $arg }
    elsif ($arg =~ s@^--size=@@) { $size = parse_size($arg) }
    elsif ($arg =~ s@^--reserved=@@) { $reserved_size = parse_size($arg) }
    elsif ($arg =~ s@^--inodes=@@) { $inodec = parse_uint($arg) }
    elsif ($arg =~ s@^--uid=@@) { $uid = parse_uint($arg) }  # User ID.
    elsif ($arg =~ s@^--gid=@@) { $gid = parse_uint($arg) }  # Group ID.
    elsif ($arg =~ s@^--mtime=@@) { $mtime = parse_int32($arg) }  # Group ID.
    elsif ($arg =~ s@^--boot=@@) { $boot_fn = $arg }  # Copy the contents of this file to the boot block.
    elsif ($arg =~ s@^--super=@@) { $super_fn = $arg }  # Copy the contents of this file to the superblock (but replace the first 0x12 bytes with the appropriate haders).
    elsif ($arg =~ s@^--kernel=@@) { $kernel_fn = $arg }  # Copy the contents of this Minix kernel file to the reserved area.
    elsif ($arg eq    "--fix-qemu") { $do_fix_qemu = 1 }
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
  $size = parse_size($ARGV[$i++]) if $i < @ARGV;
  die("fatal: too many command-line arguments\n") if $i < @ARGV;
}
die("fatal: minix1 inode count too small, must be at least 1: $inodec\n") if defined($inodec) and $inodec < 1;  # Minimum is 1: the root inode.
die("fatal: minix1 inode count too large: $inodec\n") if defined($inodec) and $inodec >= 0xffff;
die("fatal: minix1 filesystem too small: $size bytes\n") if defined($size) and ($size >> 10) < get_min_blockc($inodec, $reserved_size);
die("fatal: minix1 user ID (UID) too large: $uid\n") if $uid > 0xffff;
die("fatal: minix1 group GID (GID) too large: $gid\n") if $gid > 0xff;

die("fatal: error opening device: $device_fn\n") if !open(F, "+< " . fnopenq($device_fn))
    and !open(F, "> " . fnopenq($device_fn));
binmode(F);
my $device_size;
die("fatal: error getting device size: $device_fn\n") if !defined($device_size = sysseek(F, 0, 2));

my $boot_data;
if (defined($boot_fn)) {
  $boot_data = read_file_limited($boot_fn, 0, 0x400);
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
  if ($reserved_data =~ m@^\xb8\xc0\x07 \x8e\xd8 \x31\xf6 \xb8.. \x8e\xc0  \x31\xff \xb9\x00\x01 \xf2\xa5 \xea@sx) {  # Minix 1.5 8086 kernel image with floppy boot sector. Example files: pc/disk.03,
    my($final, $menu_ds, $menu_pc, $menu_cs) = unpack("v4", substr($reserved_data, 0x1f8, 8));
    $final_cs = (($final - 1) << 5) + 0x60;
    my $menu_fofs = 0x200 + (($menu_cs - 0x60) << 4);
    printf(STDERR "info: Minix kernel boot final=0x%x final_cs=0x%x menu_ds=0x%x menu_pc=0x%x menu_cs=0x%x menu_fofs=0x%x f=%s\n", $final, $final_cs, $menu_ds, $menu_pc, $menu_cs, $menu_fofs, $kernel_fn);
    die("fatal: bad final: $kernel_fn\n") if $final < 2;
    die("fatal: bad menu_pc: $kernel_fn\n") if $menu_pc;
    die("fatal: inconsistent menu_cs and menu_ds: $kernel_fn\n") if $menu_ds != $menu_cs;
    die("fatal: menu_cs is larger than kernel_cs: $kernel_fn\n") if $menu_cs > $final_cs;
    die("fatal: Minix kernel image too short: $kernel_fn\n") if length($reserved_data) < 0x200 + (($final_cs - 0x60) << 4);
    $reserved_data = substr($reserved_data, 0x200, $menu_fofs - 0x200);
    goto KERNEL_PART;
  } else { KERNEL_PART:
    if ($reserved_data =~ m@^\xeb\x04 .. (..) \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc@sx) {
      my $kernel_cs = 0x60; my $kernel_ds = unpack("v", $1);
      die(sprintf("fatal: bad kernel_ds=0x%x: %s\n", $kernel_ds, $kernel_fn)) if $kernel_ds <= 0x60 or $kernel_ds >= 0x60 + (length($reserved_data) >> 4);
      my($kernel_code_a256, $s1_a256, $s2_a256, $s3_a256, $s4_a256, $s5_a256, $s6_a256, $s7_a256) = unpack("v8", substr($reserved_data, ($kernel_ds - 0x60) << 4));
      my $kernel_size_para = length($reserved_data) >> 4;
      my $kernel_sum_size_para = ($kernel_code_a256 + $s1_a256 + $s2_a256 + $s3_a256 + $s4_a256 + $s5_a256 + $s6_a256 + $s7_a256) << 4;
      printf(STDERR "info: Minix kernel kernel_cs=0x%x kernel_ds=0x%x size=0x%x0 sum_size=0x%x0 kernel_code_a=0x%x s1_a=0x%x s2_a=0x%x s3_a=0x%x s4_a=0x%x s5_a=0x%x s6_a=0x%x s7_a=0x%x f=%s\n",
             $kernel_cs, $kernel_ds, $kernel_size_para, $kernel_sum_size_para, $kernel_code_a256, $s1_a256, $s2_a256, $s3_a256, $s4_a256, $s5_a256, $s6_a256, $s7_a256, $kernel_fn);
      die("fatal: inconsistent Minix kernel_code_a and kernel_ds: $kernel_fn\n") if $kernel_cs + ($kernel_code_a256 << 4) != $kernel_ds;
      die("fatal: inconsistent Minix kernel size and sum_size: $kernel_fn\n") if $kernel_size_para != $kernel_sum_size_para;
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
$size = $device_size if !defined($size);  # Round down to block size.
if ($do_fix_qemu) {
  $size -= $size % $size_multipliers{h};
  # The `$size - $size_multipliers{h}' below fixes the QEMU 2.11.1 disk
  # image size detection quirk: QEMU hides the last HDD track (cylinder)
  # from the guest, even excluding this track from the geometry size it
  # returns.
}
my $blockc = ($do_fix_qemu ? $size - $size_multipliers{h} : 0) >> 10;  # This can make $blockc negative if $do_fix_quemu is true.
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
  $firstdatablock = 2 + $imapblockc + ($zmapblockc = $zmapblockc2) + $iblockc + $reservedblockc;  # Increase both.
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
substr($super_data, 0, 0x12) = pack("v6Vv", $inodec, $blockc, $imapblockc, $zmapblockc, $firstdatablock, 0, 0x10081c00, 0x137f);
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
write_block(2 + $imapblockc + $zmapblockc, pack("vvVVCCv", 040777, $uid, 0x20, $mtime, $gid, 2, $firstdatablock));  # Write rootdir (/) inode.
write_block($firstreservedblock, $reserved_data) if defined($reserved_data);  # Write data to the reserved area.
write_block($firstdatablock, pack("va14va14", 1, ".", 1, ".."));  # Write rootdir (/) entries: "." and "..".
# TODO(pts): Add an option to write extra NUL bytes: boot block, end of superbloc, end of rootdir inode block, end of rootdir entries block.

__END__
