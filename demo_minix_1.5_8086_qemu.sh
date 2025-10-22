#! /bin/sh --
#
# demo_minix_1.5_8086_qemu.sh: create bootable disk image for Minix 1.5 8086, and start it in QEMU on Linux
# by pts@fazekas.hu at Wed Oct 22 03:23:28 CEST 2025
#

set -ex
test "$0" = "${0%/*}" || cd "${0%/*}"

if tools/nasmf-0.98.39.upx -v >/dev/null 2>&1; then
  nasm=tools/nasmf-0.98.39.upx
elif nasm-0.98.39 -v >/dev/null 2>&1; then
  nasm=nasm-0.98.39
elif nasm -v >/dev/null 2>&1; then
  nasm=nasm
else
  set +x
  echo "fatal: NASM not found" >&2
  exit 1
fi
if tools/miniperl-5.004.04.upx -e0 >/dev/null 2>&1; then
  perl=tools/miniperl-5.004.04.upx
elif perl -e0 >/dev/null 2>&1; then
  perl=perl
else
  set +x
  echo "fatal: Perl not found" >&2
  exit 1
fi

for f in disk.03 disk.04 disk.05; do
  if test -f "$f"; then
    :
  elif wget -nv -O "$f".tmp http://download.minix3.org/previous-versions/Intel-1.5/pc/"$f"; then
    mv "$f".tmp "$f"
  else
    rm -f "$f".tmp
  fi
done

"$nasm" -O0 -w+orphan-labels -o minix_1.5_8086_hdd_boot.bin minix_1.5_8086_hdd_boot.nasm
rm -f hd.img
"$perl" -x mkfsbm1.pl --size="$(perl -e "print 61*63*16*512")" --fix-qemu --boot=minix_1.5_8086_hdd_boot.bin --kernel=disk.03 hd.img

sudo umount p ||:
sudo umount f ||:
test -d p || mkdir p
test -d f || mkdir f
sudo mount -t minix -o loop,rw hd.img p
sudo mount -t minix -o loop,ro disk.04 f
sudo cp -a f/* p/  # No f/.*
sudo umount f
sudo mount -t minix -o loop,ro disk.05 f
sudo cp -a f/* p/usr/  # No fusr/.*
sudo umount f
sudo cp -a minix_1.5_8086_rc.hd p/etc/rc
sudo chown 0.0 p/etc/rc
sudo chmod 755 p/etc/rc
sudo umount p
rmdir p f

qemu-system-i386 -M isapc -m 4 -drive file=hd.img,format=raw -net none -boot c -debugcon stdio

: "$0" OK.
