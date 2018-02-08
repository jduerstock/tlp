
all: tlp.bin

tlp.bin: tlp.cfg tlp.o
	ld65 -o tlp.bin -C tlp.cfg tlp.o
	sha1sum -c tlp.sha1

tlp.o: tlp.asm
	ca65 -o tlp.o tlp.asm -l tlp.lst

clean:
	rm -f tlp.bin tlp.o tlp.lst
