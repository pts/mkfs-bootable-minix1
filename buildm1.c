/*
 * buildm1.c: a port of build.c to modern C (C89)
 * modernized by pts@fazekas.hu at Thu Oct 23 05:24:14 CEST 2025
 *
 * Compile with: gcc -m32 -s -O2 -W -Wall -Wno-implicit-int -Wno-implicit-function-declaration buildm1.c -o buildm1
 *
 * Please note that it still requires little-endian, and sizeof(long) == 4.
 */

/* This program takes the previously compiled and linked pieces of the
 * operating system, and puts them together to build a boot diskette.
 * The files are read and put on the boot diskette in this order:
 *
 *      bootblok:       the diskette boot program
 *      kernel:         the operating system kernel
 *      mm:             the memory manager
 *      fs:             the file system
 *      init:           the system initializer
 *      menu:           the menu to change the boot parameters
 *      db:             the debugger
 *
 * The bootblok file goes in sector 0 of the boot diskette.  The operating system
 * begins directly after it.  The kernel, mm, fs, init, and menu are each
 * padded out to a multiple of clicksize bytes, and then concatenated into a
 * single file beginning 512 bytes into the file.  The first byte of sector 1
 * contains executable code for the kernel.  There is no header present.
 * Clicksize is the CLICK_SIZE which the kernel, mm and fs files were
 * compiled with, and is encoded in bytes 2-3 of the kernel code segment.
 *
 * After the boot image has been built, build goes back and makes several
 * patches to the image file or diskette:
 *
 *      1. The last few words of the boot block are set as follows:
 *	   Word at 502:	Number of sectors to load
 *	   Word at 504:	DS value for running menu
 *	   Word at 506:	PC value for starting menu
 *	   Word at 508:	CS value for running menu
 *	   Word at 510:	magic 0xAA55 number for boot loader
 *
 *	2. Build writes a table into the first 8 words of the kernel's
 *	   data space.  It has 4 entries, the code and data sizes for each
 *	   program.  The kernel needs this information to run mm, fs, and
 *	   init.  Build also writes the kernel's DS value into address 4
 *	   of the kernel's TEXT segment, so the kernel can set itself up.
 *
 *      3. The origin and size of the init program are patched into bytes 4-9
 *         of the file system data space. The file system needs this
 *         information, and expects to find it here.
 *
 * When the "-s" flag is given, the symbol table (if any) of each component
 * is loaded at the end of the bss.  Patching has further steps:
 *
 *	2a. The offset of the symbol table in the data segment (in clicks) is
 *	    placed in the spare word at location 2 in the code segment.  The
 *	    sizes array is adjusted so that the symbol table is effectively
 *	    part of the data segment.
 *
 * Build is called by:
 *
 *      buildm1 [-s] [-b] <bootblok> <kernel> <mm> <fs> <init> <menu> [<db>] <image>
 *
 * to get the resulting image onto the file "image".
 * To compile this program on a unix system, no special flags are needed.
 * For a DOS system use
 *	-DMSDOS
 * In either case, if you wish to use your own custom image build utilities
 * then use
 *	-DCUSTOM
 * You will need to supply
 *	create_image(char *f);		-- f is the name of the image
 *	read_block(int blk, char buff[SECTOR_SIZE]);
 *					-- read a sector of the boot image
 *	write_block(int blk, char buff[SECTOR_SIZE]);
 *					-- write a sector of the boot image
 *	IOinit();			-- Initialize IO
 */

/* Modified by Bruce Evans, 21 Nov 88 to load symbol tables for debugger.
 * BDE 2 Mar 89. CLICK_SIZE and CLICK_SHIFT variables clicksize, click_shift.
 * BDE 8 Feb 89. CLICK_SIZE 256 instead of 16.
 * BDE 28 Jul 89. Sym offsets in clicks instead of bytes. Removed bad shorts.
 * BDE Sep 89. Don't load symbol tables by default.
 * 
 * Marty Leisner Aug(?) 89. Magic number for boot loader.
 *
 * Andrew Cagney 11 Nov 89. Supported CUSTOM versions. Fixed expression
 * invoving negated sizeof(). Sizeof may be unsigned. Used fcntl.h.
 * Eliminated special case for PCIX since Minix printf now does %ld.
 *
 * BDE Mar 90. Load debugger.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef char assert_sizeof_long_4[sizeof(long) == 4 ? 1 : -1];

/* Warning: the sizes != BLOCK_SIZE are extremely inefficient.  There are
 * scattered magic 512's and the distinction between the physical sector
 * size and the buffer and i/o sizes is not clear.  A mess.
 */

#define PROG_ORG 1536           /* where does kernel begin in abs mem */
#define DS_OFFSET 4L            /* position of DS written in kernel text seg */
#define SYM_OFFSET 2L		/* position of syms writ in kernel text seg */
#define SECTOR_SIZE 512         /* size of buf */
#define READ_UNIT 512           /* how big a chunk to read in */
#define KERNEL_D_MAGIC 0x526F   /* identifies kernel data space */
#define FS_D_MAGIC 0xDADA	/* identifies fs data space */
#define DATA_ALIGNMENT 16	/* minimum alignment of separate I&D data */
#define HCLICK_SHIFT 4
#define KERN 0
#define MM   1
#define FS   2
#define INIT 3			/* must be the last of the system programs */
#define MENU 4			/* bootstrap programs follow system programs */
#define DB   5
#define PROGRAMS 6              /* kernel + mm + fs + init + menu + db = 6 */

/* Information about the file header. */
#define HEADER1 32              /* short form header size */
#define HEADER2 48              /* long form header size */
#define SEP_POS 1               /* tells where sep I & D bit is */
#define HDR_LEN 2               /* tells where header length is */
#define TEXT_POS 0              /* where is text size in header */
#define DATA_POS 1              /* where is data size in header */
#define BSS_POS 2               /* where is bss size in header */
#define SYM_POS 5               /* where is sym size in header */
#define SEP_ID_BIT 0x20         /* bit that tells if file is separate I & D */

#include <sys/types.h>
#include <fcntl.h>
#ifdef O_BINARY
#  define BREAD		(O_RDONLY | O_BINARY)
#  define BREADWRITE	(O_RDWR | O_BINARY)
#else
#  define BREAD		O_RDONLY
#  define BREADWRITE	O_RDWR
#endif

int image;                      /* file descriptor used for output file */
int cur_sector;                 /* which 512-byte sector to be written next */
int buf_bytes;                  /* # bytes in buf at present */
char buf[SECTOR_SIZE];          /* buffer for output file */
char zero[SECTOR_SIZE];         /* zeros, for writing bss segment */

char sym_flag;			/* nonzero to load symbol tables */
char bss16_flag;		/* nonzero to mask a_bss to 16 bits */

unsigned click_shift;		/* CLICK_SHIFT used to compile kernel/mm/fs */
unsigned clicksize;		/* CLICK_SIZE used to compile kernel/mm/fs */
				/* click_size would be ambiguous */

struct sizes {
  unsigned long base;		/* offset from start of programs */
  unsigned long end;		/* offset to byte after last in this program */
  unsigned short ds;		/* data segment after loading */
  unsigned short ip;		/* instruction pointer after loading */
  unsigned short cs;		/* code segment after loading */
  unsigned text_size;           /* size in bytes */
  unsigned data_size;           /* size in bytes */
  unsigned bss_size;            /* size in bytes */
  unsigned sym_size;            /* size in bytes */
  int sep_id;                   /* 1 if separate, 0 if not */
} sizes[PROGRAMS];

char *name[] = {"\nkernel", "mm    ", "fs    ", "init  ", "menu  ", "db    "};

main(argc, argv)
int argc;
char *argv[];
{
/* Copy the boot block and the programs to the output. */

  int i;
  long programs = DB;  /* DB (db) is not specified. */
  if (*(char*)&programs != 5) pexit("little-endian machine required", "");  /* This fails on big endian and PDP-11 endian systems. */

  /* Take any symbols flag out of args. */
  for (i = 1; i < argc; ++i) {
	if (argv[i][0] == '-' && (argv[i][1] != 's' || argv[i][1] == 'b') && argv[i][2] == '\0') {
		int j;
		char *flagp = (argv[i][1] == 's') ? &sym_flag : &bss16_flag;
		for (j = i + 1; j < argc; ++j)
		argv[j - 1] = argv[j];
		--argc;
		*flagp |= 1;
	}
  }

  if (argc == DB + 2 + 1) {
  } else if (argc == DB + 2 + 2) {
    ++programs;  /* DB (db) is specified. */
  } else {
    pexit("Usage: buildm1 [-s] [-b] <bootblok> <kernel> <mm> <fs> <init> <menu> [<db>] <image>", "");
  }

  IOinit();			/* check for DMAoverrun (DOS) */
  create_image(argv[(int)programs+2]);	/* create the output file */

  /* Go get the boot block and copy it to the output file or diskette. */
  copy1(argv[1]);

  /* Copy the programs to the output file or diskette. */
  for (i = 0; i < DB; i++) copy2(i, argv[i + 2]);
  copy2(DB, ((int)programs > DB) ? argv[DB + 2] : NULL);
  flush();
  printf("                                                ------      -----\n");
  printf("Total size             %31lu      %5lx\n", sizes[PROGRAMS - 1].end,
	 sizes[PROGRAMS - 1].end);
  printf("Operating system size  %31lu      %5lx\n", sizes[INIT].end,
	 sizes[INIT].end);

  /* Make the three patches to the output file or diskette. */
  patch1();
  patch2();
  patch3();
  exit(0);
}



copy1(file_name)
char *file_name;
{
/* Copy the specified file to the output.  The file has no header.  All the
 * bytes are copied, until end-of-file is hit.
 */

  int fd, bytes_read;
  char inbuf[READ_UNIT];

  if ( (fd = open(file_name, BREAD)) < 0) pexit("can't open ",file_name);

  do {
        bytes_read = read(fd, inbuf, READ_UNIT);
        if (bytes_read < 0) pexit("read error on file ", file_name);
        if (bytes_read > 0) wr_out(inbuf, bytes_read);
  } while (bytes_read > 0);
  flush();
  close(fd);
  return 0;
}


copy2(num, file_name)
int num;                        /* which program is this (0 - 4) */
char *file_name;                /* file to open */
{
/* Open and read a file, copying it to output.  First read the header,
 * to get the text, data, bss and symbol sizes.  Also see if it is separate
 * I & D.  Write the text, data, bss and symbols to output.  The sum of these
 * four pieces must be padded upwards to a multiple of clicksize, if need
 * be.  The individual pieces need not be multiples of clicksize bytes,
 * except for the text size when separate I & D is in use.
 */

  int fd, sepid;
  unsigned long text_bytes, data_bytes, bss_bytes, sym_bytes;
  unsigned long file_text_bytes, file_sym_bytes;
  unsigned long tot_bytes;
  unsigned short cs;

  if (0) fprintf(stderr, "num=%d file_name=%s\n", num, file_name);
  if (file_name) {
    if ( (fd = open(file_name, BREAD)) < 0) pexit("can't open ", file_name);

    /* Read the header to see how big the segments are. */
    read_header(fd, &sepid, &text_bytes, &data_bytes, &bss_bytes, &sym_bytes,
                file_name);
  } else {  /* Simulate empty file. Typically used for DB (db). */
    file_name = "(none)"; fd = -1; sepid = 0;
    text_bytes = data_bytes = bss_bytes = sym_bytes = 0;
  }

  /* If the kernel, determine click_shift and clicksize. */
  if (num == 0) {
	long lseek();
	unsigned char click_buf[4];

	if (read(fd, click_buf, sizeof click_buf) != sizeof click_buf)
		pexit("can't read click_shift in ", file_name);
	if (lseek(fd, - (long) sizeof click_buf, 1) < 0)
		pexit("can't seek before click_shift in ", file_name);
	click_shift = click_buf[2] + (click_buf[3] << 8);
	if (click_shift == 0)
		click_shift = HCLICK_SHIFT;	/* old kernel */
	else if (click_shift < HCLICK_SHIFT)
		pexit("kernel click_shift must be >= 4", "");
	clicksize = 1 << click_shift;
  }
  if (0) fprintf(stderr, "!! click_shift=%u clicksize=%u\n", click_shift, clicksize);

  /* Check separate I&D text has enough padding (no longer crucial). */
  if (sepid && ((text_bytes % DATA_ALIGNMENT) != 0) ) {
        pexit("separate I & D but text size not multiple of 16 bytes.  File: ", 
                                                                file_name);
  }

  /* Pad text, data+bss and symbols individually to a multiple of clicksize.
   * Common I&D text is turned into data so does not need (or allow) padding.
   */
  file_text_bytes = text_bytes;
  if (sepid) {
	text_bytes += padding(text_bytes);
	bss_bytes += padding(data_bytes + bss_bytes);
  } else
	bss_bytes += padding(text_bytes + data_bytes + bss_bytes);
  file_sym_bytes = sym_bytes;
  sym_bytes += padding(sym_bytes);

  /* Accumulate totals. */
  tot_bytes = (unsigned long) text_bytes + (data_bytes+bss_bytes) + sym_bytes;

  /* Record the size information in the table. */
  if (num > 0)
	sizes[num].base = sizes[num - 1].end;
  sizes[num].end = sizes[num].base + tot_bytes;
  sizes[num].ip = 0;
  cs = (PROG_ORG + sizes[num].base) >> HCLICK_SHIFT;
  sizes[num].cs = cs;
  sizes[num].ds = sepid ? cs + (text_bytes >> HCLICK_SHIFT) : cs;
  sizes[num].text_size = text_bytes;
  sizes[num].data_size = data_bytes;
  sizes[num].bss_size  = bss_bytes;
  sizes[num].sym_size  = sym_bytes;
  sizes[num].sep_id    = sepid;

  /* Print the name and size of the program. */
  printf("%s  text=%5lu  data=%5lu  bss=%6lu  tot=%6lu  hex=%5lx  %s\n",
	 name[num], text_bytes, data_bytes, bss_bytes, tot_bytes,
	 tot_bytes, (sizes[num].sep_id ? "Sep I&D" : "Com I&D"));

  if (fd >= 0) {
    /* Read in the text and data segments, and copy them to output. */
    copy3(fd, file_text_bytes, file_name);
    wr_zero(text_bytes - file_text_bytes);
    copy3(fd, data_bytes, file_name);

    /* Write the bss to output. */
    wr_zero(bss_bytes);

    /* Copy symbol table to output and pad it. */
    copy3(fd, sym_bytes, file_name);
    wr_zero(sym_bytes - file_sym_bytes);

    close(fd);
  }
  return 0;
}


copy3(fd, left_to_read, file_name)
int fd;
unsigned left_to_read;
char *file_name;
{
  int bytes_read;
  int count;
  char inbuf[READ_UNIT];

  while (left_to_read != 0)
  {
    if ( (unsigned) (count = left_to_read) > READ_UNIT)
      count = READ_UNIT;
    if ( (bytes_read = read(fd, inbuf, count)) <= 0) {
      pexit("read error on file ", file_name);
    }
    wr_out(inbuf, bytes_read);
    left_to_read -= count;
  }
  return 0;
}


#ifdef XENIX_HEADER
# include </usr/include/sys/a.out.h>
#endif

read_header(fd, sepid, text_bytes, data_bytes, bss_bytes, sym_bytes,file_name)
int fd, *sepid;
unsigned long *text_bytes, *data_bytes, *bss_bytes, *sym_bytes;
char *file_name;
{
/* Read the header and check the magic number.  The standard Monix header 
 * consists of 8 longs, as follows:
 *      0: 0x04100301L (combined I & D space) or 0x04200301L (separate I & D)
 *      1: 0x00000020L (stripped file) or 0x00000030L (unstripped file)
 *      2: size of text segments in bytes
 *      3: size of initialized data segment in bytes
 *      4: size of bss in bytes
 *      5: 0x00000000L
 *      6: total memory allocated to program (text, data and stack, combined)
 *      7: 0x00000000L
 * The longs are represented low-order byte first and high-order byte last.
 * The first byte of the header is always 0x01, followed by 0x03.
 * The header is followed directly by the text and data segments, whose sizes
 * are given in the header.
 */

#ifdef XENIX_HEADER
  struct  aexec a_header;
#else
  long head[12];
  unsigned short hd[4];
#endif
  int n, header_len;

#ifdef XENIX_HEADER
  /*
    Do it right, read header *structure* to get header length.
    Fortunately header has no longs so we don't have to worry about
    swapped words, not to mention swapped bytes.
  */
  if ((n = read(fd, &a_header, sizeof a_header)) != sizeof a_header)
  {
    printf("expected %d, got %d\n", sizeof a_header, n);
    pexit("file header too short: ", file_name);
  }
  if (a_header.xa_magic == FMAGIC)
    *sepid = 0;
  else if (a_header.xa_magic == IMAGIC)
    *sepid = 1;
  else
    pexit("not Xenix a.out FMAGIC or IMAGIC. FIle: ", file_name);
  if (a_header.xa_entry != 0)
    pexit("nonzero entry point. FIle: ", file_name);
  *text_bytes = a_header.xa_text;
  *data_bytes = a_header.xa_data;
  *bss_bytes  = a_header.xa_bss;
  *sym_bytes  = a_header.xa_syms;
#else
  /* Read first 8 bytes of header to get header length. */
  if ((n = read(fd, hd, 8)) != 8) pexit("file header too short: ", file_name);
  header_len = hd[HDR_LEN];
  if (header_len != HEADER1 && header_len != HEADER2) 
        pexit("bad header length. File: ", file_name);

  /* Extract separate I & D bit. */
  *sepid = hd[SEP_POS] & SEP_ID_BIT;

  /* Read the rest of the header and extract the sizes. */
  if ((n = read(fd, head, header_len - 8)) != header_len - 8)
        pexit("header too short: ", file_name);

  *text_bytes = head[TEXT_POS];
  *data_bytes = head[DATA_POS];
  *bss_bytes  = head[BSS_POS];
  if (bss16_flag) *bss_bytes &= 0xffffU;
  *sym_bytes  = head[SYM_POS];
  if (0) fprintf(stderr, "!! READHEAD a_text=0x%lx=%lu a_data=0x%lx=%lu a_bss=0x%lx=%lu\n", *text_bytes, *text_bytes, *data_bytes, *data_bytes, *bss_bytes, *bss_bytes);
#endif

  if (!sym_flag)
	*sym_bytes = 0;		/* pretend there are no symbols */
  return 0;
}


wr_out(buffer, bytes)
char buffer[READ_UNIT];
int bytes;
{
/* Write some bytes to the output file.  This procedure must avoid writes
 * that are not entire 512-byte blocks, because when this program runs on
 * MS-DOS, the only way it can write the raw diskette is by using the system
 * calls for raw block I/O.
 */

  int room, count, count1;
  register char *p, *q;

  /* Copy the data to the output buffer. */
  room = SECTOR_SIZE - buf_bytes;
  count = (bytes <= room ? bytes : room);
  count1 = count;
  p = &buf[buf_bytes];
  q = buffer;
  while (count--) *p++ = *q++;
  
  /* See if the buffer is full. */
  buf_bytes += count1;
  if (buf_bytes == SECTOR_SIZE) {
        /* Write the whole block to the disk. */
        write_block(cur_sector, buf);
        clear_buf();
  }

  /* Is there any more data to copy. */
  if (count1 == bytes) return 0;
  bytes -= count1;
  buf_bytes = bytes;
  p = buf;
  while (bytes--) *p++ = *q++;
  return 0;
}


flush()
{
  if (buf_bytes == 0) return 0;
  write_block(cur_sector, buf);
  clear_buf();
  return 0;
}


clear_buf()
{
  register char *p;

  for (p = buf; p < &buf[SECTOR_SIZE]; p++) *p = 0;
  buf_bytes = 0;
  cur_sector++;
  return 0;
}


patch1()
{
/* Fill in the last few words of the boot block. */

  unsigned short ubuf[SECTOR_SIZE/2], sectrs;

  if (sizes[INIT].end % clicksize != 0)
	pexit("MINIX is not multiple of clicksize bytes", "");

  /* calc nr of sectors to load (starting at 0) */
  sectrs = (unsigned) (sizes[PROGRAMS - 1].end / 512L);
  if (sizes[PROGRAMS - 1].end % 512 != 0)
     ++sectrs;

  read_block(0, ubuf);          /* read in boot block */
  ubuf[(SECTOR_SIZE/2) - 8] = sizes[DB].ds;
  ubuf[(SECTOR_SIZE/2) - 7] = sizes[DB].ip;
  ubuf[(SECTOR_SIZE/2) - 6] = sizes[DB].cs;
  ubuf[(SECTOR_SIZE/2) - 5] = sectrs;
  ubuf[(SECTOR_SIZE/2) - 4] = sizes[MENU].ds;
  ubuf[(SECTOR_SIZE/2) - 3] = sizes[MENU].ip;
  ubuf[(SECTOR_SIZE/2) - 2] = sizes[MENU].cs;
  ubuf[(SECTOR_SIZE/2) - 1] = 0xAA55;
  write_block(0, ubuf);
  return 0;
}

patch2()
{
/* This program now has information about the sizes of the kernel, mm, fs, and
 * init.  This information is patched into the kernel as follows. The first 8
 * words of the kernel data space are reserved for a table filled in by build.
 * The first 2 words are for kernel, then 2 words for mm, then 2 for fs, and
 * finally 2 for init.  The first word of each set is the text size in clicks;
 * the second is the data+bss size in clicks.  If separate I & D is NOT in
 * use, the text size is 0, i.e., the whole thing is data.
 *
 * In addition, the DS value the kernel is to use is computed here, and loaded
 * at location 4 in the kernel's text space.  It must go in text space because
 * when the kernel starts up, only CS is correct.  It does not know DS, so it
 * can't load DS from data space, but it can load DS from text space.
 *
 * Write the offset of the symbol table for each progam into location 2 of
 * its code space, for the debugger. No one was expecting this, but is is
 * the only available unused space.
 */

  int i;
  unsigned t, d, b, s;
  unsigned long text_bytes, data_bytes, sym_offset;
  unsigned long text_offset, data_offset;

  /* See if the magic number is where it should be in the kernel. */
  text_offset = 512L;
  data_offset = 512L + (unsigned long) sizes[KERN].text_size; /*start of data*/
  i = get_word(data_offset);
  if (i != KERNEL_D_MAGIC)  {
	pexit("kernel data space: no magic #","");
  }
  
  for (i = 0; i <= INIT; i++) {
        t = sizes[i].text_size;
        d = sizes[i].data_size;
        b = sizes[i].bss_size;
	s = sizes[i].sym_size;
        if (sizes[i].sep_id) {
		text_bytes = t;
		data_bytes = (unsigned long) (d + b) + s;
		sym_offset = d + b;
        } else {
		text_bytes = 0;
		data_bytes = (unsigned long) t + (d + b) + s;
		sym_offset = t + d + b;
        }
	put_click(data_offset + 4*i + 0L, text_bytes);
	put_click(data_offset + 4*i + 2L, data_bytes);
	put_click(text_offset + SYM_OFFSET, sym_offset);
        text_offset += (unsigned long) t + d + b + s;
  }

  /* Now write the DS value into a magic word of the kernel text space. */
  put_word(512L + DS_OFFSET, sizes[KERN].ds);
  return 0;
}


patch3()
{
/* Write the origin and text and data sizes of the init program in FS's data
 * space.  The file system expects to find these 3 words there.
 */

  unsigned long init_text_size, init_data_size;
  unsigned mag;
  unsigned long fs_data, mm_data;	/* offsets to data in file */

  mm_data = sizes[MM].base + sizes[MM].text_size + SECTOR_SIZE;
  fs_data = sizes[FS].base + sizes[FS].text_size + SECTOR_SIZE;
  init_text_size = sizes[INIT].text_size;
  init_data_size = sizes[INIT].data_size + sizes[INIT].bss_size
                 + sizes[INIT].sym_size;
  if (sizes[INIT].sep_id == 0) {
        init_data_size += init_text_size;
        init_text_size = 0;
  }

  /* Check for appropriate magic numbers. */
  if (get_word(mm_data) != FS_D_MAGIC) pexit("mm data space: no magic #","");
  mag = get_word(mm_data + 2);
  if (mag == 0) mag = HCLICK_SHIFT;	/* old mm */
  if (mag != click_shift) pexit("mm click_shift does not match kernel's", "");
  if (get_word(fs_data) != FS_D_MAGIC) pexit("fs data space: no magic #","");
  mag = get_word(fs_data + 2);
  if (mag == 0) mag = HCLICK_SHIFT;	/* old fs */
  if (mag != click_shift) pexit("fs click_shift does not match kernel's", "");

  put_click(fs_data+4L, PROG_ORG + sizes[INIT].base);
  put_click(fs_data+6L, init_text_size);
  put_click(fs_data+8L, init_data_size);
  return 0;
}

int get_byte(offset)
unsigned long offset;
{
/* Fetch one byte from the output file. */

  char buff[SECTOR_SIZE];

  read_block( (unsigned) (offset / SECTOR_SIZE), buff);
  return(buff[(unsigned) (offset % SECTOR_SIZE)] & 0377);
}

int get_word(offset)
unsigned long offset;
{
/* Fetch one word from the output file. */

  return((get_byte(offset + 1) << 8) | get_byte(offset));
}

put_byte(offset, byte_value)
unsigned long offset;
int byte_value;
{
/* Write one byte into the output file. This is not very efficient, but
 * since it is only called to write a few words it is just simpler.
 */

  char buff[SECTOR_SIZE];

  read_block( (unsigned) (offset/SECTOR_SIZE), buff);
  buff[(unsigned) (offset % SECTOR_SIZE)] = byte_value;
  write_block( (unsigned)(offset/SECTOR_SIZE), buff);
  return 0;
}


pexit(s1, s2)
char *s1, *s2;
{
  printf("Build: %s%s\n", s1, s2);
  exit(1);
}


int padding(num)
unsigned num;
{
/* Calculate padding to make number a multiple of clicksize. */

  unsigned fragment;

  fragment = num % clicksize;
  return(fragment == 0 ? 0 : clicksize - fragment);
}

put_click(offset, value)
unsigned long offset;
unsigned long value;
{
/* Convert value to clicks and write it into output file. */

  put_word(offset, (unsigned) (value >> click_shift));
  return 0;
}


put_word(offset, word_value)
unsigned long offset;
unsigned word_value;
{
/* Write one word into the output file. Maybe truncate word_value to 2 bytes.*/

  put_byte(offset, word_value % 256);
  put_byte(offset + 1, word_value / 256);
  return 0;
}


wr_zero(remainder)
unsigned remainder;
{
/* Write zeros into the output file. */

  unsigned count;

  while (remainder != 0) {
	count = (remainder < SECTOR_SIZE ? remainder : SECTOR_SIZE);
	wr_out(zero, count);
	remainder -= count;
  }
  return 0;
}


#ifndef CUSTOM /* the rest of this file is not used in CUSTOM configurations */

/*===========================================================================
 * The following code is only used in the UNIX version of this program.
 *===========================================================================*/
#ifndef MSDOS
create_image(f)
char *f;
{
/* Create the output file. */
  image = creat(f, 0666);
  close(image);
  image = open(f, BREADWRITE);
  return 0;
}

read_block(blk, buff)
int blk;
char buff[SECTOR_SIZE];
{
  lseek(image, (long)SECTOR_SIZE * (long) blk, 0);
  if (read(image, buff, SECTOR_SIZE) != SECTOR_SIZE) pexit("block read error", "");
  return 0;
}

write_block(blk, buff)
int blk;
char buff[SECTOR_SIZE];
{
  lseek(image, (long)SECTOR_SIZE * (long) blk, 0);
  if (write(image, buff, SECTOR_SIZE) != SECTOR_SIZE) pexit("block write error", "");
  return 0;
}

IOinit() { return 0; }	/* dummy */

#else /*MSDOS*/
/*===========================================================================
 *   This is the raw diskette I/O for MSDOS. It uses diskio.asm or biosio.asm
 *==========================================================================*/

#define MAX_RETRIES     5

char *buff;
char buff1[SECTOR_SIZE];
char buff2[SECTOR_SIZE];
int drive;

IOinit()			/* check if no DMAoverrun & assign the buffer */
{
  if (DMAoverrun(buff1))
     buff = buff2;
  else
     buff = buff1;
}


read_block (blocknr,user)
int blocknr;
char user[SECTOR_SIZE];
{
  /* read the requested MINIX-block in core */
  int retries,err,i;
  char *p;

  retries = MAX_RETRIES;
  do
      err = absread (drive, blocknr, buff);
  while (err && --retries);

  if (!retries)
    dexit ("reading",drive,blocknr,err);

  p=buff; i=SECTOR_SIZE;
  while (i--) *(user++) = *(p++);
}



write_block (blocknr,user)
int blocknr;
char user[SECTOR_SIZE];
{
  /* write the requested MINIX-block to disk */
  int retries,err,i;
  char *p;

  p=buff; i=SECTOR_SIZE;
  while (i--) *(p++) = *(user++);

  retries = MAX_RETRIES;
  do
      err = abswrite (drive, blocknr, buff);
  while (err && --retries);

  if (!retries)
    dexit ("writing",drive,blocknr,err);
}



dexit (s,drive,sectnum,err)
int sectnum, err,drive;
char *s;
{ extern char *derrtab[];
  printf ("Error %s drive %c, sector: %d, code: %d, %s\n",
           s, drive+'A',sectnum, err, derrtab[err] );
  exit (2);
}


create_image (s)
char *s;
{
  char kbstr[10];
  if (s[1] != ':') pexit ("wrong drive name (dos): ",s);
  drive = (s[0] & ~32) - 'A';
  if (drive<0 || drive>32) pexit ("no such drive: ",s);
  printf("Put a blank, formatted diskette in drive %s\nHit return when ready",s);
  gets (kbstr,10);
  puts("");
}

char *derrtab[14] = {
        "no error",
        "disk is read-only",
        "unknown unit",
        "device not ready",
        "bad command",
        "data error",
        "internal error: bad request structure length",
        "seek error",
        "unknown media type",
        "sector not found",
        "printer out of paper (??" ")",
        "write fault",
        "read error",
        "general error"
};


#endif /*MSDOS*/

#endif /* CUSTOM */
