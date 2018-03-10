all:
	stack build
	stack exec haskellstein tilemaps/genmap1.txt
clean:
	stack clean
