#! /bin/sh --
#
# download_minix_1.5_8086_qemu_pm.bin.sh: downloader for minix_1.5_8086_qemu_pm.bin
# by pts@fazekas.hu at Thu Oct 23 04:27:28 CEST 2025
#
# minix_1.5_8086_qemu_pm.bin. is a Minix 1.5 8086 kernel image which works
# in (16-bit) protected mode in QEMU 2.11.1. This is great news, because
# none of the official kernels on
# http://download.minix3.org/previous-versions/Intel-1.5/ can do it.
# It was found in: https://github.com/oldlinux-web/oldlinux-files/tree/master/bochs/MINIX-1.5-040514.zip
#

set -ex
test "$0" = "${0%/*}" || cd "${0%/*}"

if ! test -f MINIX-1.5-040514.zip; then
  # Alternative download link: https://web.archive.org/web/20251023023735/https://media.githubusercontent.com/media/oldlinux-web/oldlinux-files/af7120d204d37af256fa9bbca1572e91f89b8ef0/bochs/MINIX-1.5-040514.zip
  wget -nv -O MINIX-1.5-040514.zip.tmp https://github.com/oldlinux-web/oldlinux-files/raw/af7120d204d37af256fa9bbca1572e91f89b8ef0/bochs/MINIX-1.5-040514.zip
  test "$(sha256sum <MINIX-1.5-040514.zip.tmp)" = "1bfeaa42120d82a05a51df5d6195a8b8fb5ae99678b81ae8f58a68917a8a8e44  -"
  mv MINIX-1.5-040514.zip.tmp MINIX-1.5-040514.zip
else
  test "$(sha256sum <MINIX-1.5-040514.zip)" = "1bfeaa42120d82a05a51df5d6195a8b8fb5ae99678b81ae8f58a68917a8a8e44  -"
fi
unzip -p MINIX-1.5-040514.zip MINIX-1.5-040514/cccccc.img |
    dd iflag=fullblock bs=1024 skip=53235 count=174 status=none |
    (dd iflag=fullblock bs=1024 count=8 status=none && dd iflag=fullblock bs=1024 skip=1 count=165 status=none) >minix_1.5_8086_qemu_pm.bin.tmp
test "$(sha256sum <minix_1.5_8086_qemu_pm.bin.tmp)" = "83572bc39d312d67ae820aa927f9e0cb86b13ee615915541e433e41025260b20  -"
touch -d "2003-01-04 18:30:02 GMT" minix_1.5_8086_qemu_pm.bin.tmp
mv minix_1.5_8086_qemu_pm.bin.tmp minix_1.5_8086_qemu_pm.bin
if test "$1" = --rm; then
  rm -f MINIX-1.5-040514.zip MINIX-1.5-040514.zip.tmp minix_1.5_8086_qemu_pm.bin.tmp
fi

: "$0" OK.
