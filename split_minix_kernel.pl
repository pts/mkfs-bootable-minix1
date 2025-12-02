#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# split_minix_kernel.pl: split a Minix kernel image to kernel, mm, fs, init
# by pts@fazekas.hu at Fri Oct 24 14:39:57 CEST 2025
#

BEGIN { $ENV{LC_ALL} = "C" }  # For deterministic output. Typically not needed. Is it too late for Perl?
BEGIN { $ENV{TZ} = "GMT" }  # For deterministic output. Typically not needed. Perl respects it immediately.
BEGIN { $^W = 1 }  # Enable warnings.
use integer;
use strict;

sub fnargq($) { $_[0] =~ m@^[.\w/]@ ? $_[0] : "./" . $_[0] }
sub fnopenq($) { $_[0] =~ m@^[-+.\w/]@ ? $_[0] : "./" . $_[0] }

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

sub write_file($$) {
  my($fn, $data) = @_;
  die("fatal: error opening for reading: $fn: $!\n") if !open(FW, "> " . fnopenq($fn));
  binmode(FW);
  my $i = 0; my $got;
  while ($i < length($data)) {
    die("fatal: error reading file: $fn\n") if !defined($got = syswrite(FW, $data, length($data) - $i, $i));
    $i += $got;
  }
  close(FW);
  undef
}

sub write_aout_file($$$$$$$$) {
  my($fn, $image_data, $ofs, $a_text8, $a_data8, $a_syms4, $a_cpu, $do_strip) = @_;
  my $a_syms = $a_syms4 << 4; my $a_text = $a_text8 << 8; my $a_data = ($a_data8 << 8) - $a_syms;
  my $text = substr($image_data, $ofs, $a_text);
  my $data = substr($image_data, $ofs + $a_text, $a_data);
  my $syms = substr($image_data, $ofs + $a_text + $a_data, $a_syms);
  my $a_bss;
  if ($data !~ y@\0@@c) {  # The entire $data is NUL. Typically it doesn't happen.
    ($a_data, $a_bss) = (0, $a_data);
    $data = "";
  } elsif ($data =~ m@(\0+)\Z(?!\n)@) {
    $a_bss = length($1);  # & ~1;  # Some Minix 1.5.10 component files have $a_data unaligned (or aligned to a multiple of 2), so we don't need for this.
    substr($data, $a_data -= $a_bss) = "";
  } else {
    $a_bss = 0;
  }
  if ($text =~ m@(\0+)\Z(?!\n)@) {
    substr($text, $a_text = ($a_text - length($1) + 0xf) & ~0xf) = "";  # It's safe to remove trailing NUL bytes from (typical) Minix 1.5 i86 or i386 kernel code. The x86 machine code of these NUL bytes doesn't make sense to have.
  }

  $a_syms -= 0x10 while $a_syms > 0 and !vec($syms, $a_syms - 0x10, 8);  # Strip tailing NULs (i.e. when the symbol name starts with a NUL).
  substr($syms, $a_syms) = "";
  if ($do_strip) { $a_syms = 0; $syms = "" }
  my $a_flags = ($a_text) ? 0x20 : 0x10;  # For $a_text == 0 (e.g. for init), the boundary between the text and data segments can't be restored.
  my $aout_header = pack("vC4vV6", 0x301, $a_flags, $a_cpu, 0x20, 0, 0, $a_text, $a_data, $a_bss, 0, 0x10000, $a_syms);
  my $size = length($aout_header) + length($text) + length($data) + length($syms);
  printf(STDERR "info: writing component: a_cpu=0x%x a_text=0x%x a_data=0x%x a_bss=0x%x a_syms=0x%x size=0x%x=%u f=%s\n", $a_cpu, $a_text, $a_data, $a_bss, $a_syms, $size, $size, $fn);
  write_file($fn, join("", $aout_header, $text, $data, $syms));
}

sub get_syms_size($$$$) {
  my($name, $text_a256, $data_a256, $s) = @_;
  #die("fatal: assert bad size of s\n") if length($s) != 28;
  return (0, 0) if length($s) < 28;
  my($opcode, $delta, $syms_ofs, $sr) = unpack("CCvZ24", $s);
  return (0, 0) if !($opcode == 0xeb and ($name eq "kernel" ? ($delta == 4) : ($delta == 0x1a and !length($sr))) and $syms_ofs > 0);
  #printf(STDERR "info: %-6s text start: 0x%02x %02x %04x %s\n", $name, $opcode, $delta, $syms_ofs, unpack("H*", $sr));
  my $is_force_i386 = ($name eq "fs" and $s =~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx and defined($1));
  $syms_ofs <<= 8 if $is_force_i386 or $syms_ofs <= 0xff;  # Minix 1.5 kernels other than in http://download.minix3.org/previous-versions/Intel-1.5/
  return (0, 0) if $syms_ofs & 0xf or $syms_ofs > ($data_a256 << 8);
  (($data_a256 << 4) - ($syms_ofs >> 4), 1)
}

# --- main().

my $do_strip = 0; if (@ARGV and $ARGV[0] eq "--strip") { $do_strip = 1; shift(@ARGV) }
die("Usage: $0 [--strip] -- <image> <kernel> [<mm> <fs> <init>]\n") unless (@ARGV == 3 or @ARGV == 6) and $ARGV[0] eq "--";
my($dummy, $image_fn, $kernel_fn, $mm_fn, $fs_fn, $init_fn) = @ARGV;

my $image_data = read_file($image_fn);
if (length($image_data) >= 0x400 and
    $image_data =~ m@^\xb8\xc0\x07 \x8e\xd8 \x31\xf6 \xb8.. \x8e\xc0  \x31\xff \xb9\x00\x01 [\xf2\xf3]\xa5 \xea@sx) {  # Minix 1.5 8086 kernel image with floppy boot sector (bootblok). Example files: pc/disk.03, demo_dsk.ibm .
  # db is the debugger. It is not used in practice, and values are 0 in production images.
  my($db_ds, $db_pc, $db_cs, $final, $menu_ds, $menu_pc, $menu_cs) = unpack("v7", substr($image_data, ((substr($image_data, 0x1fe, 2) eq "\x55\xaa") ? 0x1f0 : 0x1f2), 7 << 1));
  my $final_cs = (($final - 1) << 5) + 0x60;
  my $menu_fofs = 0x200 + (($menu_cs - 0x60) << 4);
  my $db_fofs = ($db_cs == 0) ? 0 : 0x200 + (($db_cs - 0x60) << 4);
  printf(STDERR "info: Minix kernel boot final=0x%x final_cs=0x%x menu_ds=0x%x menu_pc=0x%x menu_cs=0x%x menu_fofs=0x%x db_ds=0x%x db_pc=0x%x db_cs=0x%x db_fofs=0x%x f=%s\n", $final, $final_cs, $menu_ds, $menu_pc, $menu_cs, $menu_fofs, $db_ds, $db_pc, $db_cs, $db_fofs, $image_fn);
  die("fatal: bad final: $image_fn\n") if $final < 2;
  die("fatal: bad menu_pc: $image_fn\n") if $menu_pc;
  die("fatal: bad menu_cs: $image_fn\n") if $menu_cs < 0x80;  # This is way too low.
  die("fatal: inconsistent menu_cs and menu_ds: $image_fn\n") if $menu_ds < $menu_cs;  # Minix 1.5 8086 has these equal, but in Minix 1.5 i386, $menu_ds is 0x100 larger.
  die("fatal: menu_cs is larger than kernel_cs: $image_fn\n") if $menu_cs > $final_cs;
  die("fatal: Minix kernel image too short: $image_fn\n") if length($image_data) < 0x200 + (($final_cs - 0x60) << 4);
  $image_data = substr($image_data, 0x200, $menu_fofs - 0x200);
} elsif ($image_data =~ s@^\x01\x03\x20([\x04\x10])\x20\0\0\0(....)(?:.{20}) (?=\xeb\x04 \x08\0 \0\0 \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc)@@sx) {  # Just the kernel component.
  my $a_cpu = ord($1); my $a_text = unpack("V", $2);
  my $cpu = ($a_cpu eq 0x10) ? "i386" : "8086";
  printf(STDERR "info: Minix kernel component file cpu=%s a_text=0x%x f=%s\n", $cpu, $a_text, $image_fn);
  die("fatal: a_text is not a multiple of 0x10: $image_fn\n") if $a_text & 0xf;
  my $fake_kernel_ds = 0x60 + ($a_text >> 4);
  die("fatal: fake kernel_ds too large: $image_fn\n") if $fake_kernel_ds > 0xffff;
  substr($image_data, 4, 2) = pack("v", $fake_kernel_ds);
  # It gets a only a little bit further from here.
} else {
  die("fatal: unrecognized Minix kernel image: $image_fn\n");
}
die("fatal: unrecognized Minix kernel image before menu: $image_fn\n") if
    $image_data !~ m@^\xeb\x04 .. (..) \xfa \xfc \x2e\x8b\x16\x04\x00 \x8e\xda \x8e\xc2 \x8e\xd2 \xbc@sx;
my $kernel_cs = 0x60; my $kernel_ds = unpack("v", $1);
die(sprintf("fatal: bad kernel_ds=0x%x: %s\n", $kernel_ds, $image_fn)) if $kernel_ds <= 0x60 or $kernel_ds >= 0x60 + (length($image_data) >> 4);
my($kernel_text_a256, $kernel_data_a256, $mm_text_a256, $mm_data_a256, $fs_text_a256, $fs_data_a256, $init_text_a256, $init_data_a256) = unpack("v8", substr($image_data, ($kernel_ds - 0x60) << 4, 0x10));
my $mm_ofs256 = $kernel_text_a256 + $kernel_data_a256;
my $fs_ofs256 = $mm_ofs256 + $mm_text_a256 + $mm_data_a256;
my $init_ofs256 = $fs_ofs256 + $fs_text_a256 + $fs_data_a256;
my($kernel_syms_para, $ok_kernel) = get_syms_size("kernel", $kernel_text_a256, $kernel_data_a256, substr($image_data, 0, 28));
my($mm_syms_para,     $ok_mm)     = get_syms_size("mm",     $mm_text_a256,     $mm_data_a256,     substr($image_data, $mm_ofs256 << 8,   28));
my($fs_syms_para,     $ok_fs)     = get_syms_size("fs",     $fs_text_a256,     $fs_data_a256,     substr($image_data, $fs_ofs256 << 8,   0x30));
my($init_syms_para,   $ok_init)   = get_syms_size("init",   $init_text_a256,   $init_data_a256,   substr($image_data, $init_ofs256 << 8, 28));
my $kernel_size_para = length($image_data) >> 4;
my $kernel_sum_size_para = ($kernel_text_a256 + $kernel_data_a256 + $mm_text_a256 + $mm_data_a256 + $fs_text_a256 + $fs_data_a256 + $init_text_a256 + $init_data_a256) << 4;
# Our $kernel_magic has been overwritten with $kernel_text_a256 and $kernel_data_a256.
my $mm_magic   = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($image_data, ($mm_ofs256   + $mm_text_a256  ) << 8, 4));
my $fs_magic   = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($image_data, ($fs_ofs256   + $fs_text_a256  ) << 8, 4));
my $init_magic = ($kernel_size_para < $kernel_sum_size_para) ? 0xffffffff : unpack("V", substr($image_data, ($init_ofs256 + $init_text_a256) << 8, 4));
printf(STDERR "info: Minix kernel kernel_cs=0x%x kernel_ds=0x%x size=0x%x0 sum_size=0x%x0 kernel_para=0x%x+0x%x+0x%x mm_para=0x%x+0x%x+0x%x fs_para=0x%x+0x%x+0x%x init_para=0x%x+0x%x+0x%x mm_magic=0x%x fs_magic=0x%x init_magic=0x%x f=%s\n",
       $kernel_cs, $kernel_ds, $kernel_size_para, $kernel_sum_size_para,
       $kernel_text_a256 << 4, ($kernel_data_a256 << 4) - $kernel_syms_para, $kernel_syms_para,
       $mm_text_a256     << 4, ($mm_data_a256     << 4) - $mm_syms_para,     $mm_syms_para,
       $fs_text_a256     << 4, ($fs_data_a256     << 4) - $fs_syms_para,   , $fs_syms_para,
       $init_text_a256   << 4, ($init_data_a256   << 4) - $init_syms_para,   $init_syms_para,
       $mm_magic, $fs_magic, $init_magic, $image_fn);
die("fatal: inconsistent Minix kernel_text_a and kernel_ds: $image_fn\n") if $kernel_cs + ($kernel_text_a256 << 4) != $kernel_ds;
die("fatal: inconsistent Minix kernel size and sum_size: $image_fn\n") if $kernel_size_para != $kernel_sum_size_para;
die("fatal: kernel image is not long enough: $image_fn\n") if length($image_data) < ($kernel_sum_size_para << 4);
#die("fatal: bad kernel_magic: $image_fn\n") if $kernel_magic != 0x8526f;  # Checked by /shoelace. Our $kernel_magic has been overwritten with $kernel_text_a256 and $kernel_data_a256.
die("fatal: bad mm_magic: $image_fn\n") if $mm_magic != 0x8dada;  # Also checked by /shoelace.
die("fatal: bad fs_magic: $image_fn\n") if $fs_magic != 0x8dada;  # Also checked by /shoelace.
#die("fatal: bad init_magic: $image_fn\n") if $init_magic != 0x8dada;  # Not checked by /shoelace. init_magic is not set for the original 8086 kernel pc/disk.03.
die("fatal: bad kernel text start: $image_fn\n") if !$ok_kernel;
die("fatal: bad mm text start: $image_fn\n") if !$ok_mm;
die("fatal: bad fs text start: $image_fn\n") if !$ok_fs;
die("fatal: bad init text start: $image_fn\n") if !$ok_init;
die("fatal: bad mm start: $image_fn\n") if substr($image_data, $mm_ofs256 << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
my $is_mm_i386 = defined($1);  # Not checking kernel text, because it always starts with 8086 (16-bit) code.
die("fatal: bad fs start: $image_fn\n") if substr($image_data, $fs_ofs256 << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
my $is_fs_i386 = defined($1);
die("fatal: bad init start: $image_fn\n") if substr($image_data, $init_ofs256 << 8, 0x30) !~ m@^\xeb\x1a .{26} \x8b(?:(\x25)..\0\0 \xe8..\0\0 | \x26.. \xe8..)@sx;
my $is_init_i386 = defined($1);
my $pmswitch = "unknown";  # Switch mechanism used to switch to protected mode.
if ($image_data =~ m@^.{43} \xbe.. \xbb\x30\x28 \xb4\x89 \xcd\x15 \xc3@sx) {  # mov si, ... ++ mov bx, 0x2830 ++ mov ah, 0x89 ++ int 0x15 ++ ret.
  $pmswitch = "int15h"  # This can be 8086 or i386.
} elsif ($image_data =~ m@^.{43} \x0f\x01\x16.. \x0f\x20\xc0@sx or  # lgdt [...] ++ mov eax, cr0 ++ or al, 1 ++ mov cr0, eax.
         $image_data =~ m@^.{43} \xbe.. \xbb\x30\x28 \xb4. \xe8.\0 (?:[\xeb\xb0\xe6]. | \x88[\xf8\xd8])+ \x0f\x01\x16.. \x0f\x20\xc0@sx) {  # mov si, ... ++ mov bx, 0x2830 ++ mov ah, ... (0) ++ call ... ++ (lots mov()s and out()s) ++ lgdt [...] ++ mov eax, cr0 ++ or al, 1 ++ mov cr0, eax.
  $pmswitch = "lgdt-cr0-i386"
}
printf(STDERR "info: Minix kernel CPU mm=%s fs=%s init=%s pmswitch=%s f=%s\n", ($is_mm_i386 ? "i386" : "8086"), ($is_fs_i386 ? "i386" : "8086"), ($is_init_i386 ? "i386" : "8086"), $pmswitch, $image_fn);
die("fatal: inconsistent CPU among mm, fs, init: $image_fn\n") if $is_mm_i386 != $is_fs_i386 or $is_mm_i386 != $is_init_i386;
die("fatal: inconsistent CPU and pmswitch: $image_fn\n") if !$is_mm_i386 and $pmswitch =~ m@i386@;
substr($image_data, 2, 4) = "\x08\0\0\0";  # Write back click_shift and placeholder for kernel_ds. Also clear the $kernel_syms_para field.
substr($image_data, ($kernel_ds - 0x60) << 4, 0x10) = "\x6f\x52\x08\0" . "\0" x 0xc;  # Set kernel_magic.
substr($image_data, ($mm_ofs256   << 8) + 2, 2) = "\0\0";  # Clear the $mm_syms_para   field set at `text_offset + SYM_OFFSET' by patch2() in build.c.
substr($image_data, ($fs_ofs256   << 8) + 2, 2) = "\0\0";  # Clear the $fs_syms_para   field set at `text_offset + SYM_OFFSET' by patch2() in build.c.
substr($image_data, ($init_ofs256 << 8) + 2, 2) = "\0\0";  # Clear the $init_syms_para field set at `text_offset + SYM_OFFSET' by patch2() in build.c.
substr($image_data, (($fs_ofs256 + $fs_text_a256) << 8) + 4, 6) = "\0\0\0\0\0\0";  # Clear the init_org, init_text_size, init_data_size fields set by patch3() in build.c.
my $a_cpu = $is_mm_i386 ? 0x10 : 4;  # 8086 is 4, i386 is 0x10.

write_aout_file($kernel_fn, $image_data, 0,                 $kernel_text_a256, $kernel_data_a256, $kernel_syms_para, $a_cpu, $do_strip);
write_aout_file($mm_fn,     $image_data, $mm_ofs256 << 8,   $mm_text_a256    , $mm_data_a256,     $mm_syms_para,     $a_cpu, $do_strip) if defined($mm_fn);
write_aout_file($fs_fn,     $image_data, $fs_ofs256 << 8,   $fs_text_a256    , $fs_data_a256,     $fs_syms_para,     $a_cpu, $do_strip) if defined($fs_fn);
write_aout_file($init_fn,   $image_data, $init_ofs256 << 8, $init_text_a256  , $init_data_a256,   $init_syms_para,   $a_cpu, $do_strip) if defined($init_fn);

__END__
