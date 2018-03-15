MAPSLOC = tilemaps/
MAPS = $(MAPSLOC)genmap1.txt $(MAPSLOC)genmap2.txt $(MAPSLOC)genmap3.txt
EXEC = stack exec haskellstein
SECONDMAKEPOS = lib/libc/
SECONDMAKE = cd $(SECONDMAKEPOS) && make -f Makefile

run: create_maps build
	$(EXEC) $(MAPS)

build:
	stack build

create_maps: $(SECONDMAKEPOS)Makefile
	$(SECONDMAKE) run

clean: $(SECONDMAKEPOS)Makefile
	stack clean
	rm -rf $(MAPS)
	$(SECONDMAKE) clean
