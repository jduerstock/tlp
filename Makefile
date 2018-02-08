
all: tlp.bin

tlp.bin: tlp.asm tlp.cfg tlp.o
	ca65 -o tlp.o tlp.asm -l tlp.lst
	ld65 -o tlp.bin -C tlp.cfg tlp.o
	sha1sum -c tlp.sha1

clean:
	rm -f tlp.bin tlp.o
