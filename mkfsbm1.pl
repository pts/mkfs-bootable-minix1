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
die("fatal: minix1 filesystem size too small: $size\n") if defined($size) and ($size >> 10) < get_min_blockc($inodec, $reserved_size);
die("fatal: minix1 user ID (UID) too large: $uid\n") if $uid > 0xffff;
die("fatal: minix1 group GID (GID) too large: $gid\n") if $gid > 0xff;

die("fatal: error opening device: $device_fn\n") if !open(F, "+< " . fnopenq($device_fn))
    and !open(F, "> " . fnopenq($device_fn));
binmode(F);
my $device_size;
die("fatal: error getting device size: $device_fn\n") if !defined($device_size = sysseek(F, 0, 2));
$device_size = $device_size + 0;  # Convert "0 but true" to 0.
$size = $device_size if !defined($size);  # Round down to block size.
if ($do_fix_qemu) {
  $size -= $size % $size_multipliers{h};
  # This fixes the QEMU 2.11.1 disk image size detection quirk: QEMU hides
  # the last HDD track (cylinder) from the guest, even excluding this track
  # from the geometry size it returns.
  #
  # This can make size negative.
  $size -= $size_multipliers{h};
}
my $blockc = $size >> 10;
my $reservedblockc = ($reserved_size + 0x3ff) >> 10;
# TODO(pts): Add flag to round up $inodec, like mkfs.minix(1) on Linux does it.
$inodec = ($blockc < $reservedblockc) ? 0x1f : (($blockc - $reservedblockc) / 3) + 8 if !defined($inodec);  # Minix 1.5 pc/disk.13.ex/p/commands/mkfs.c default is 3 * blocks/file.
die("fatal: bad minix1 inode count: $inodec\n") if $inodec < 1 or $inodec >= 0xffff;
die("fatal: minix1 filesystem size too small: $size\n") if $blockc < get_min_blockc($inodec, $reserved_size);
die("fatal: minix1 filesystem size too large: $blockc\n") if $blockc > 0xffff;
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
printf(STDERR "info: blockc=0x%x=%u fssize=%s inodec=0x%x=%u firstdatablock=0x%x=%u f=%s\n",
       $blockc, $blockc, format_blockc_size($blockc), $inodec, $inodec, $firstdatablock, $firstdatablock, $device_fn);

if ($do_truncate0 and $device_size) {
  die("fatal: error truncating device: $device_fn\n") if !truncate(F, 0);  # This will fail on block devices, but it will work on disk image files.
  $device_size = 0;
}
if ($device_size < $size or ($do_force_size and $device_size != $size)) {
  die("fatal: error changing device size: $device_fn\n") if !truncate(F, $size);
}
write_block(1, pack("v6Vv", $inodec, $blockc, $imapblockc, $zmapblockc, $firstdatablock, 0, 0x10081c00, 0x137f));  # Write superblock.
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
write_block(2 + $imapblockc + $zmapblockc + $iblockc + $reservedblockc, pack("va14va14", 1, ".", 1, ".."));  # Write rootdir (/) entries: "." and "..".
# TODO(pts): Add an option to write extra NUL bytes: boot block, end of superbloc, end of rootdir inode block, end of rootdir entries block.

__END__
