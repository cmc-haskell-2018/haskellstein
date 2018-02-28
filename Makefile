all:
	stack build
	stack exec haskellstein tilemaps/testlvl.txt
