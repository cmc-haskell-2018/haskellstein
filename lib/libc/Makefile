MAPSLOC = ../../tilemaps/
BUILDDIR = build/
BINDIR = $(BUILDDIR)bin/
EXE = $(BINDIR)genmap
SRC = src/map.c
COMPILER = gcc
FLAGS = -Wall -g

run: build
	$(EXE) > $(MAPSLOC)genmap1.txt
	$(EXE) > $(MAPSLOC)genmap2.txt
	$(EXE) > $(MAPSLOC)genmap3.txt

build: $(SRC)
	mkdir -p $(BINDIR)
	$(COMPILER) $(FLAGS) $(SRC) -o $(EXE)

clean:
	rm -rf $(BUILDDIR)
