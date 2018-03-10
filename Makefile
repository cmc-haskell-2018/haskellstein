run: tilemaps/genmap1.txt build
	stack exec haskellstein tilemaps/genmap1.txt
build:
	stack build
tilemaps/genmap1.txt: lib/libc/Makefile
	cd lib/libc && make -f Makefile run
genmap: lib/libc/Makefile
	cd lib/libc && make -f Makefile run	
clean: lib/libc/Makefile
	stack clean
	cd lib/libc && make -f Makefile clean
