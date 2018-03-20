# COMPILER INFO
GHC = ghc
GHC_FLAGS = -Wall -dynamic

# BASIC INFO
TARGET_NAME = game
OUTPUT_NAME = haskellstein

# MODULES INFO
MODULES_NAMES = Main Haskellstein \
 Haskellstein/Raycasting \
 Haskellstein/Data \
 Haskellstein/CheckMap \
 Haskellstein/Initialize \
 Haskellstein/Interactions
MODULES_PATH = src
MODULES = $(MODULES_NAMES:%=$(MODULES_PATH)/%.hs)

# LIBS INFO
SFTOOL_NAME = sftool
SFTOOL_PATH = sftool
LIBS_PATH = lib
LIBS = -l$(SFTOOL_NAME) -lsfml-graphics -lsfml-window \
                        -lsfml-system -lstdc++

#GAME_INPUT_FILES
MAPSLOC = tilemaps/

# MAKEFILE INSTRUCTIONS
all: $(TARGET_NAME)

$(TARGET_NAME):
	$(GHC) $(GHC_FLAGS) $(MODULES) -L$(SFTOOL_PATH) -L$(LIBS_PATH) $(LIBS) \
	 -o $(OUTPUT_NAME)

clean:
	rm -f $(OUTPUT_NAME)
	rm -f $(MODULES_PATH)/*.o $(MODULES_PATH)/*.hi
	rm -f $(MODULES_PATH)/Haskellstein/*.o $(MODULES_PATH)/Haskellstein/*.hi
	rm -f $(MAPSLOC)genmap*
	cd sftool; make clean
	cd genmaps; make clean

force:
	make clean
	cd sftool; make all
	cd genmaps; make run
	make all

run:
	LD_LIBRARY_PATH=lib ./haskellstein
	make all
	./$(OUTPUT_NAME) $(MAPSLOC)genmap*
