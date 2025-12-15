#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# compress_minix_kernel_image.pl: strip and/or compress (using aPACK 1.00) a Minix 1.5.10 i86 or i386 kernel image
# by pts@fazekas.hu at Sun Dec 14 02:07:20 CET 2025
#
# Typical size reduction for Minix 1.5.10 i86  kernel image --compress: 171520 --> 42176 bytes.
# Typical size reduction for Minix 1.5.10 i386 kernel image --compress: 507392 --> 43264 bytes.
# Most of the size reduction is because of the lots of consecutive NUL bytes in .bss section.
#
# Example usage: perl -x compress_minix_kernel_image.pl minix minix.co
#
# Compression steps done by this program:
#
# 1. It strips symbols from all 4 Minix 1.5.10 kernel components (kernel, mm, fs and init).
# 2. It concatenates the 4 components, and prepends a DOS MZ .exe header. (As
#    a side effect, this removes components menu and db (debugger)).
# 3. It runs aPACK 1.00 (apack1p) to compress the DOS MZ .exe using LZSS compression.
# 4. It prepares a bootblok with print-and-reboot code the correct .menu_cs field value.
# 5. It removes the DOS MZ .exe header from the beginning, and it prepends
#    the prepared bootblok.
#
# Use mbr_bootlace.nasm (compiled with -DMINIX) to boot the compressed Minix
# kernel image output. ShoeLace is't able boot it. It's not possible to boot
# it by writing it to a floppy and booting from floppy.
#
# The apack1p port of aPACK 1.00 is used, so compression needs a Linux i386
# or amd64 system (or approriate emulation in Docker, WSL etc.).
#
# If you want only the symbols to be stripped (no compression), run this
# program with the `--strip` flag: `perl -x compress_minix_kernel_image.pl
# --strip minix minix.stripped`. This will keep the components menu and db,
# and all of mbr_bootlace.nasm, ShoeLace and direct floppy boot will also
# work.
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

sub get_syms_size($$$$$) {
  my($name, $is_i386, $text_a256, $data_a256, $s) = @_;
  #die("fatal: assert bad size of s\n") if length($s) != 28;
  return (0, 0) if length($s) < 28;
  my($opcode, $delta, $syms_ofs, $sr) = unpack("CCvZ24", $s);
  return (0, 0) if !($opcode == 0xeb and ($name eq "kernel" ? ($delta == 4) : ($delta == 0x1a and !length($sr))) and $syms_ofs > 0);
  $syms_ofs <<= 8 if $is_i386 or $syms_ofs <= 0xff;  # Minix 1.5 kernels other than in http://download.minix3.org/previous-versions/Intel-1.5/
  return (0, 0) if $syms_ofs & 0xf or $syms_ofs > ($data_a256 << 8);
  (($data_a256 << 4) - ($syms_ofs >> 4), 1)
}

# ---

my $ufn;
my $cfn;
my $apack1p_prog;
my $do_strip;
my $do_compress;
die("Usage: $0 [<flag>...] <minix> [<minix.co>]\n") if !@ARGV or $ARGV[0] eq "--help";
{ my $i;
  for ($i = 0; $i < @ARGV; ++$i) {
    my $arg = $ARGV[$i];
    if ($arg eq "--") { ++$i; last }
    elsif ($arg eq "-" or $arg !~ m@^-@) { last }
    elsif ($arg =~ s@^--apack1p=@@) { $apack1p_prog = $arg }
    elsif ($arg eq "--info") { $do_strip = $do_compress = 0 }  # Just print file info, don't change the Minix kernel image file.
    elsif ($arg eq    "--strip") { $do_strip = 1 }  # Strip symbols.
    elsif ($arg eq "--no-strip") { $do_strip = 0 }
    elsif ($arg eq    "--compress") { $do_compress = 1 }  # Strip symbols.
    elsif ($arg eq "--no-compress") { $do_compress = 0 }
    else { die("fatal: unknown command-line flag: $arg\n") }
  }
  if (!defined($ufn)) {
    die("fatal: missing <minix> argument\n") if $i >= @ARGV;
    $ufn = $ARGV[$i++];
  }
  $cfn = $ARGV[$i++] if !defined($cfn) and $i < @ARGV;
  die("fatal: too many command-line arguments\n") if $i < @ARGV;
}
$do_compress = (defined($do_strip) and $do_strip) ? 0 : 1 if !defined($do_compress);  # Default action.
$do_strip = 1 if !defined($do_strip) and $do_compress;
$cfn = $ufn if !defined($cfn);
die("fatal: --compress conflicts with --no-strip\n") if $do_compress and defined($do_strip) and !$do_strip;

$_ = read_file($ufn);
printf(STDERR "info: read Minix kernel image: %s (%u bytes)\n", $ufn, length($_));
die("fatal: kernel image file too short: $ufn\n") if length($_) < 0x220;
my $bootblok = substr($_, 0, 0x200);
substr($_, 0, 0x200) = "";
die("fatal: bad kernel code start: $ufn\n") if !m@^\xeb\x04 .. (..) \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc@sx;
my $kernel_ds = unpack("v", $1);
die("fatal: bad kernel_ds: $ufn\n") if $kernel_ds <= 0x60 or $kernel_ds >= 0x60 + (length($_) >> 4);
my($kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256) = unpack("v8", substr($_, ($kernel_ds - 0x60) << 4, 0x10));
die("fatal: bad mm code start: $ufn\n") if substr($_, ($kernel_text_a256 + $kernel_data_a256) << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
my $is_mm_i386 = defined($1) ? 1 : 0;  # Not checking kernel text, because it always starts with 8086 (16-bit) code.
my $arch = ($is_mm_i386) ? "i386" : "i86";
my @xflag = ($arch eq "i86") ? ("-x") : ();

# * DS:BP+0x1f6: dw final, menu_ds, menu_pc, menu_cs, bootsig
# * DS:BP+0x1f6: dw ?      final,   menu_ds, menu_pc, menu_cs
# $final is (filesize + 0x1ff) >> 9) - 1. The 1 is because of the bootblok.
my($db_ds, $db_pc, $db_cs, $final, $menu_ds, $menu_pc, $menu_cs) = unpack("v7", substr($bootblok, ((substr($bootblok, 0x1fe, 2) eq "\x55\xaa") ? 0x1f0 : 0x1f2), 7 << 1));
my $size = ($menu_cs - 0x60) << 4;
die("fatal: kernel image file too short: $ufn\n") if length($_) < $size;
my $expected_final = (length($_) + 0x1ff) >> 9;
die("fatal: bad final: expected=$expected_final got=$final: $ufn\n") if $final != $expected_final and $final != $expected_final + 1;  # +1 for the original Minix 1.5.10 i86 kernel image.
my $menu_etc = substr($_, $size);  # menu and db (debugger).
substr($_, $size) = "";

my $mm_ofs256 = $kernel_text_a256 + $kernel_data_a256;
my $fs_ofs256 = $mm_ofs256 + $mm_text_a256 + $mm_data_a256;
my $init_ofs256 = $fs_ofs256 + $fs_text_a256 + $fs_data_a256;
my($kernel_syms_para, $ok_kernel) = get_syms_size("kernel", $is_mm_i386, $kernel_text_a256, $kernel_data_a256, substr($_, 0, 28));
my($mm_syms_para,     $ok_mm)     = get_syms_size("mm",     $is_mm_i386, $mm_text_a256,     $mm_data_a256,     substr($_, $mm_ofs256 << 8,   28));
my($fs_syms_para,     $ok_fs)     = get_syms_size("fs",     $is_mm_i386, $fs_text_a256,     $fs_data_a256,     substr($_, $fs_ofs256 << 8,   0x30));
my($init_syms_para,   $ok_init)   = get_syms_size("init",   $is_mm_i386, $init_text_a256,   $init_data_a256,   substr($_, $init_ofs256 << 8, 28));
my $kernel_size_para = (length($_) + 0xf) >> 4;
my $kernel_sum_size_para = ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256 + $init_text_a256 + $init_data_a256) << 4;
my $fs_data_ofs = ($fs_ofs256 + $fs_text_a256) << 8;
sub print_and_check_sizes($) {
  my $msg = $_[0];
  printf(STDERR "info: ${msg}Minix kernel kernel_acs=0x%x kernel_ds=0x%x size_4=0x%x0=%u sum_size_4=0x%x0 menu_etc_size=0x%x kernel_para=0x%x+0x%x+0x%x mm_para=0x%x+0x%x+0x%x fs_para=0x%x+0x%x+0x%x init_para=0x%x+0x%x+0x%x size=%u kernel=minix-1.5.10 arch=%s%s\n",
      0x60, $kernel_ds, $kernel_size_para, $kernel_size_para << 4, $kernel_sum_size_para, length($menu_etc),
      $kernel_text_a256 << 4, ($kernel_data_a256 << 4) - $kernel_syms_para, $kernel_syms_para,
      $mm_text_a256     << 4, ($mm_data_a256     << 4) - $mm_syms_para,     $mm_syms_para,
      $fs_text_a256     << 4, ($fs_data_a256     << 4) - $fs_syms_para,   , $fs_syms_para,
      $init_text_a256   << 4, ($init_data_a256   << 4) - $init_syms_para,   $init_syms_para,
      length($bootblok) + length($_) + length($menu_etc), $arch, length($msg) ? "" : " f=$ufn");
  die("fatal: bad size: $ufn\n") if $size != length($_);
  die("fatal: inconsistent Minix kernel_text_a and kernel_ds: $ufn\n") if 0x60 + ($kernel_text_a256 << 4) != $kernel_ds;
  die("fatal: inconsistent Minix kernel size and sum_size: $ufn\n") if $kernel_size_para != $kernel_sum_size_para;
  die("fatal: bad kernel text start: $ufn\n") if !$ok_kernel;
  die("fatal: bad mm text start: $ufn\n") if !$ok_mm;
  die("fatal: bad fs text start: $ufn\n") if !$ok_fs;
  die("fatal: bad init text start: $ufn\n") if !$ok_init;
  die("fatal: bad init offset in fs: $ufn\n") if unpack("v", substr($_, $fs_data_ofs + 4, 2)) != $init_ofs256 + 6;  # This is patch3() in build.c.
  die("fatal: bad init data size in fs: $ufn\n") if unpack("v", substr($_, $fs_data_ofs + 8, 2)) != $init_data_a256;  # This is patch3() in build.c.
}
print_and_check_sizes("");

if ($do_strip) {
  my $stripped_kernel_size = (($kernel_text_a256 + $kernel_data_a256) << 8) - ($kernel_syms_para << 4);
  my $stripped_mm_size = (($mm_text_a256 + $mm_data_a256) << 8) - ($mm_syms_para << 4);
  my $stripped_fs_size = (($fs_text_a256 + $fs_data_a256) << 8) - ($fs_syms_para << 4);
  my $stripped_init_size = (($init_text_a256 + $init_data_a256) << 8) - ($init_syms_para << 4);
  $_ = join("", substr($_, 0, $stripped_kernel_size), "\0" x (-$stripped_kernel_size & 0xff),
                substr($_, $mm_ofs256 << 8, $stripped_mm_size), "\0" x (-$stripped_mm_size & 0xff),
                substr($_, $fs_ofs256 << 8, $stripped_fs_size), "\0" x (-$stripped_fs_size & 0xff),
                substr($_, $init_ofs256 << 8, $stripped_init_size), "\0" x (-$stripped_init_size & 0xff));
  $mm_ofs256 = $kernel_text_a256 + ($kernel_data_a256 = (($stripped_kernel_size + 0xff) >> 8) - $kernel_text_a256);
  $fs_ofs256 = $mm_ofs256 + $mm_text_a256 + ($mm_data_a256 = (($stripped_mm_size + 0xff) >> 8) - $mm_text_a256);
  $init_ofs256 = $fs_ofs256 + $fs_text_a256 + ($fs_data_a256 = (($stripped_fs_size + 0xff) >> 8) - $fs_text_a256);
  $init_data_a256 = (($stripped_init_size + 0xff) >> 8) - $init_text_a256;
  # die(sprintf("fs \@0x%x; fs.data \@0x%x; init \@0x%x", $fs_ofs256 << 8, ($fs_ofs256 + $fs_text_a256) << 8, $init_ofs256 << 8));
  $menu_cs -= ($size - length($_)) >> 4;
  $menu_ds -= ($size - length($_)) >> 4;
  $db_cs -= ($size - length($_)) >> 4 if $db_cs;
  $db_ds -= ($size - length($_)) >> 4 if $db_ds;
  $final = (length($_) + length($menu_etc) + 0x1ff) >> 9;
  substr($bootblok, ((substr($bootblok, 0x1fe, 2) eq "\x55\xaa") ? 0x1f0 : 0x1f2), 7 << 1) = pack("v7", $db_ds, $db_pc, $db_cs, $final, $menu_ds, $menu_pc, $menu_cs);
  substr($_, ($kernel_ds - 0x60) << 4, 0x10) = pack("v8", $kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256);
  $fs_data_ofs = ($fs_ofs256 + $fs_text_a256) << 8;
  substr($_, $fs_data_ofs + 4, 2) = pack("v", $init_ofs256 + 6);   # This is patch3() in build.c.
  substr($_, $fs_data_ofs + 8, 2) = pack("v", $init_data_a256);   # This is patch3() in build.c.
  $kernel_size_para = (length($_) + 0xf) >> 4;
  $kernel_sum_size_para = ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256 + $init_text_a256 + $init_data_a256) << 4;
  $size = length($_);
  $kernel_syms_para = $mm_syms_para = $fs_syms_para = $init_syms_para = 0;
  print_and_check_sizes("stripped ");
  # write_file("$ufn.strip", $bootblok . $_ . $menu_etc);
}
if (!$do_compress) {
  if ($do_strip or $cfn ne $ufn) {
    substr($_, 0, 0) = $bootblok;
    $_ = $menu_etc;
    my $msg = $do_strip ? "stripped" : "uncompressed";
    printf(STDERR "info: writing $msg Minix kernel image: %s (%u bytes)\n", $cfn, length($_));
    write_file($cfn, $_);
  }
  exit(0);
}

my $hdrsize = 2; my $xsize = ($hdrsize << 4) + $size; my $lastsize = $xsize & 0x1ff; my $nblocks  = ($xsize + 0x1ff) >> 9;
my $nreloc = 0; my $minalloc = 0xffff; my $maxalloc = 0xffff; my $ss = 0; my $sp = 0x200; my $checksum = 0; my $ip = 0; my $cs = 0; my $relocpos = 0; my $noverlay = 0;
substr($_, 0, 0) = pack("a2v13x4", "MZ", $lastsize, $nblocks, $nreloc, $hdrsize, $minalloc, $maxalloc, $ss, $sp, $checksum, $ip, $cs, $relocpos, $noverlay);  # x4 assumes $hdrsize == 2.
write_file($cfn, $_);
if (!defined($apack1p_prog) or !length($apack1p_prog)) {
  my $mydir = $0;
  $mydir = "." if $mydir !~ s@/+[^/]+\Z(?!\n)@@;
  $apack1p_prog = "$mydir/tools/apack1p-1.00.upx";
}
my @apack1p_cmd = ($apack1p_prog, "-q", "-q", "-h", "-t", "-1", @xflag, fnargq($cfn), fnargq($cfn));
print(STDERR "info: running apack1p: ", join(" ", map { shq($_) } @apack1p_cmd), "\n");  # This also prints an extra error to STDERR if @apack1_cmd[0] doesn't exist.
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
