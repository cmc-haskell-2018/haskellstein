run: build
	build/bin/genmap > ../../tilemaps/genmap1.txt
build: src/map.c
	mkdir -p build/bin
	gcc -Wall -g src/map.c -o build/bin/genmap
clean:
	rm -rf build
