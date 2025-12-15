#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# compress_minix_kernel_image.pl: compress a Minix 1.5.10 i86 or i386 kernel image using aPACK
# by pts@fazekas.hu at Sun Dec 14 02:07:20 CET 2025
#
# Typical size reduction for Minix 1.5.10 i86  kernel image: 171520 --> 48384 bytes.
# Typical size reduction for Minix 1.5.10 i386 kernel image: 507392 --> 43264 bytes.
# Most of the size reduction is because of the lots of consecutive NUL bytes in .bss section.
#
# !! Also strip symbols before compression. (That's complicated, but doable.)
#

BEGIN { $ENV{LC_ALL} = "C" }  # For deterministic output. Typically not needed. Is it too late for Perl?
BEGIN { $ENV{TZ} = "GMT" }  # For deterministic output. Typically not needed. Perl respects it immediately.
BEGIN { $^W = 1 }  # Enable warnings.
use integer;
use strict;

sub fnargq($) { $_[0] =~ m@^[.\w/]@ ? $_[0] : "./" . $_[0] }
sub fnopenq($) { $_[0] =~ m@^[-+.\w/]@ ? $_[0] : "./" . $_[0] }
# This is Unix-only, it should be ported to Windows (CommandLineToArgvW).
sub shq($) { my $arg = $_[0]; if (!length($arg) or $arg !~ y@-:=+./A-Za-z0-9@@) { $arg =~ s@'@'\\''@g; $arg = "'$arg'" } $arg }

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

sub write_file($$) {
  my($fn, $s) = @_;
  die("fatal: error opening for writing: $fn: $!\n") if !open(FW, "> " . fnopenq($fn));
  binmode(FW);
  die("fatal: error wriiting to file: $fn\n") if length($s) and (syswrite(FW, $s, length($s)) or 0) != length($s);
  close(FW);
  undef
}

# ---

my $apack1p_prog = undef;
if (@ARGV and $ARGV[0] =~ m@^--apack1p=(.*)@s) { shift(@ARGV); $apack1p_prog = $1 }
die("Usage: $0 <minix> [<minix.co>]\n") if @ARGV != 1 and @ARGV != 2;

my $ufn = $ARGV[0];
my $cfn = defined($ARGV[1]) ? $ARGV[1] : $ufn;

$_ = read_file($ufn);
printf(STDERR "info: read Minix kernel image: %s (%u bytes)\n", $ufn, length($_));
die("fatal: kernel image file too short\n") if length($_) < 0x220;
my $bootblok = substr($_, 0, 0x200);
substr($_, 0, 0x200) = "";
die("fatal: bad kernel code start\n") if !m@^\xeb\x04 .. (..) \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc@sx;
my $kernel_ds = unpack("v", $1);
die("fatal: bad kernel_ds\n") if $kernel_ds <= 0x60 or $kernel_ds >= 0x60 + (length($_) >> 4);
my($kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256) = unpack("v8", substr($_, ($kernel_ds - 0x60) << 4, 0x10));
die("fatal: bad mm code start\n") if substr($_, ($kernel_text_a256 + $kernel_data_a256) << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
my $is_mm_i386 = defined($1) ? 1 : 0;  # Not checking kernel text, because it always starts with 8086 (16-bit) code.
my $arch = ($is_mm_i386) ? "i386" : "i86";
my @xflag = ($arch eq "i86") ? ("-x") : ();

# * DS:BP+0x1f6: dw final, menu_ds, menu_pc, menu_cs, bootsig
# * DS:BP+0x1f6: dw ?      final,   menu_ds, menu_pc, menu_cs
my($menu_cs, $boot_signature) = unpack("vv", substr($bootblok, 0x1fc, 4));
$menu_cs = $boot_signature if $boot_signature != 0xaa55;
my $size = ($menu_cs - 0x60) << 4;
die("fatal: kernel image file too short\n") if length($_) < $size;
substr($_, $size) = "";
my $hdrsize = 2; my $xsize = ($hdrsize << 4) + $size; my $lastsize = $xsize & 0x1ff; my $nblocks  = ($xsize + 0x1ff) >> 9;
my $nreloc = 0; my $minalloc = 0xffff; my $maxalloc = 0xffff; my $ss = 0; my $sp = 0x200; my $checksum = 0; my $ip = 0; my $cs = 0; my $relocpos = 0; my $noverlay = 0;
substr($_, 0, 0) = pack("a2v13x4", "MZ", $lastsize, $nblocks, $nreloc, $hdrsize, $minalloc, $maxalloc, $ss, $sp, $checksum, $ip, $cs, $relocpos, $noverlay);  # x4 assumes $hdrsize == 2.

write_file($cfn, $_);
if (!defined($apack1p_prog) or !length($apack1p_prog)) {
  my $mydir = $0;
  $mydir = "." if $mydir !~ s@/+[^/]+\Z(?!\n)@@;
  $apack1p_prog = "$mydir/tools/apack1p-1.00.upx";
}
my @apack1p_cmd = ($apack1p_prog, "-q", "-q", "-h", "-t", "-1", @xflag, $cfn, $cfn);
print(STDERR "info: running apack1p: ", join(" ", map { shq($_) } @apack1p_cmd), "\n");
die("fatal: apack1p ($apack1p_prog) failed\n") if system(@apack1p_cmd);

$_ = read_file($cfn);
die("fatal: missing MZ .exe signature\n") if length($_) < 0x18 or substr($_, 0, 2) ne "MZ";
($lastsize, $nblocks, $nreloc, $hdrsize, $minalloc, $maxalloc, $ss, $sp, $checksum, $ip, $cs) = unpack("x2v11", substr($_, 0, 0x18));
die("fatal: unexpected .exe header values\n") if !$nblocks or $hdrsize != 2 or $maxalloc != 0xffff or $ip or $cs;
substr($_, 0, 0x20) = "";
$size = (($lastsize & 0x1ff) or 0x200) + (($nblocks - 1) << 9) - ($hdrsize << 4);
die("fatal: compressed image size mismatch\n") if $size != length($_);
# push ds ++ push es.
die("fatal: unexpected aPACK decompressor code at start\n") if substr($_, 0, 2) ne "\x1e\x06";
# pop ax ++ pop es ++ pop ds ++ mov ss, ax ++ mov sp, 0x200 ++ jmp 0:0.
my $arch2 = (substr($_, length($_) - 13) eq "\x58\x07\x1f\x8e\xd0\xbc\0\2\xea\0\0\0\0") ? "i386" :
    (substr($_, length($_) - 15) eq "\x58\x07\x1f\xfa\x8e\xd0\xbc\0\2\xfb\xea\0\0\0\0") ? "i86" : undef;
die("fatal: unexpected aPACK decompressor code at end\n") if !defined($arch);
die("fatal: arch mismatch: $arch vs $arch2\n") if $arch ne $arch2;
if ($arch eq "i386") {
  substr($_, 0, 2) = "\x0e\x60";  # push cs ++ pusha.
  substr($_, length($_) - 13) = "\x58\x61\x6a\0\xcb";  # pop ax ++ popa ++ push byte 0 ++ retf.
} else {  # This would also work for $arch eq "i386", but the alternative is shorter.
  substr($_, 0, 2) = "\x56\x57";  # push si ++ push di.
  # substr($_, length($_) - 15, 10) = "\x58\x5f\x5e\xb9\x0a\0\x31\xc0\x90\x90";  # pop ax ++ pop di ++ pop si ++ mov cx, 0xa ++ xor ax, ax ++ nop ++ nop. Followed by jmp 0:overwritten_0. Works, but longer.
  substr($_, length($_) - 15) = "\x58\x5f\x5e\x50\xb9\x0a\0\x31\xc0\x50\xcb";  # pop ax ++ pop di ++ pop si ++ push ax ++ mov cx, 0xa ++ xor ax, ax ++ push ax ++ retf. Followed by the last 5 bytes of jmp 0:overwritten_0.
}
$_ .= "\0" x ((-length($_)) & 0xf); $size = length($_);
die("fatal: compressed image to small\n") if $size < 0x70;
$menu_cs = ($size >> 4) + 0x60;
# PC boot sector code which prints a NUL-terminated message, waits for a keypress and reboots.
my $bootcode = "\xfa\x31\xc0\x8e\xd8\x8e\xd0\xbc\x00\x7c\xfb\xfc\xbe\x21\x7c\x31\xdb\xac\x84\xc0\x74\x06\xb4\x0e\xcd\x10\xeb\xf5\x98\xcd\x16\xcd\x19" .
    "mbr_bootlace.nasm in the MBR can boot this compressed Minix 1.5.10 $arch kernel.\r\n";
substr($_, 0, 0) = pack("Z508vv", $bootcode, $menu_cs, 0xaa55);
printf(STDERR "info: writing compressed Minix kernel image: %s (%u bytes)\n", $cfn, length($_));
write_file($cfn, $_);

__END__
